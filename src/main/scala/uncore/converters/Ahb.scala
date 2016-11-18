package uncore.converters

import Chisel._
import junctions._
import uncore.tilelink._
import uncore.util._
import uncore.constants._
import config._
import HastiConstants._

/* We need to translate TileLink requests into operations we can actually execute on AHB.
 * The general plan of attack is:
 *   get         => one AHB=>TL read
 *   put         => [multiple AHB write fragments=>nill], one AHB write=>TL
 *   getBlock    => AHB burst reads =>TL
 *   putBlock    => AHB burst writes=>TL
 *   getPrefetch => noop=>TL
 *   putPrefetch => noop=>TL
 *   putAtomic   => one AHB=>TL read, one idle, one AHB atom_write=>nill, one idle
 *
 * This requires that we support a pipeline of optional AHB requests with optional TL responses
 */
class AHBRequestIO(implicit p: Parameters) extends HastiMasterIO
    with HasGrantType
    with HasClientTransactionId
    with HasTileLinkBeatId {
  val executeAHB = Bool()
  val respondTL  = Bool()
  val latchAtom  = Bool()
  val firstBurst = Bool()
  val finalBurst = Bool()
  val cmd        = Bits(width = M_SZ) // atomic op
}

// AHB stage1: translate TileLink Acquires into AHBRequests
class AHBTileLinkIn(supportAtomics: Boolean = false)(implicit val p: Parameters) extends Module
    with HasHastiParameters
    with HasTileLinkParameters
    with HasAddrMapParameters {
  val io = new Bundle {
    val acquire = new DecoupledIO(new Acquire).flip // NOTE: acquire must be either a Queue or a Pipe
    val request = new DecoupledIO(new AHBRequestIO)
  }
  
  // Match the AHB burst with a TileLink {Put,Get}Block
  val burstSize = tlDataBeats match {
    case 1  => HBURST_SINGLE
    // case 2 not supported by AHB
    case 4  => HBURST_WRAP4
    case 8  => HBURST_WRAP8
    case 16 => HBURST_WRAP16
    case _  => throw new java.lang.AssertionError("TileLink beats unsupported by AHB")
  }
  
  // Bursts start at 0 and wrap-around back to 0
  val finalBurst = UInt(tlDataBeats-1, width = log2Up(tlDataBeats)).asUInt
  val firstBurst = UInt(0,             width = log2Up(tlDataBeats))
  val next_wmask = Wire(UInt(width = tlDataBytes)) // calculated below
  
  // State variables for processing more complicated TileLink Acquires
  val s_atom_r :: s_atom_idle1 :: s_atom_w :: s_atom_idle2 :: Nil = Enum(UInt(), 4)
  val atom_state = Reg(init = s_atom_r) // never changes if !supportAtomics
  val done_wmask = Reg(init = UInt(0, width = tlDataBytes))
  val burst      = Reg(init = firstBurst)
  
  // Grab some view of the TileLink acquire
  val acq_wmask    = io.acquire.bits.wmask()
  val isReadBurst  = io.acquire.bits.is(Acquire.getBlockType)
  val isWriteBurst = io.acquire.bits.is(Acquire.putBlockType)
  val isBurst      = isWriteBurst || isReadBurst
  val isAtomic     = io.acquire.bits.is(Acquire.putAtomicType) && Bool(supportAtomics)
  val isPut        = io.acquire.bits.is(Acquire.putType)
  
  // Final states?
  val last_wmask = next_wmask === acq_wmask
  val last_atom  = atom_state === s_atom_idle2
  val last_burst = burst      === finalBurst
  
  // Block the incoming request until we've fully consumed it
  // NOTE: the outgoing grant.valid may happen while acquire.ready is still false;
  // for this reason it is essential to have a Queue or a Pipe infront of acquire
  io.acquire.ready := io.request.ready && MuxLookup(io.acquire.bits.a_type, Bool(true), Array(
    Acquire.getType         -> Bool(true),
    Acquire.getBlockType    -> last_burst, // hold it until the last beat is burst
    Acquire.putType         -> last_wmask, // only accept the put if we can fully consume its wmask
    Acquire.putBlockType    -> Bool(true),
    Acquire.putAtomicType   -> last_atom,  // atomic operation stages complete
    Acquire.getPrefetchType -> Bool(true),
    Acquire.putPrefetchType -> Bool(true)))
  
  // Advance the fragment state
  when (io.request.ready && io.acquire.valid && isPut) {
    when (last_wmask) { // if this was the last fragment, restart FSM
      done_wmask := UInt(0)
    } .otherwise {
      done_wmask := next_wmask 
    }
  }
  
  // Advance the burst state
  // We assume here that TileLink gives us all putBlock beats with nothing between them
  when (io.request.ready && io.acquire.valid && isBurst) {
    when (last_burst) {
      burst := UInt(0)
    } .otherwise {
      burst := burst + UInt(1)
    }
  }
  
  // Advance the atomic state machine
  when (io.request.ready && io.acquire.valid && isAtomic) {
    switch (atom_state) {
      is (s_atom_r)     { atom_state := s_atom_idle1 }
      is (s_atom_idle1) { atom_state := s_atom_w     } // idle1 => AMOALU runs on a different clock than AHB slave read
      is (s_atom_w)     { atom_state := s_atom_idle2 }
      is (s_atom_idle2) { atom_state := s_atom_r     } // idle2 state is required by AHB after hmastlock is lowered
    }
  }
  
  // Returns (range=0, range=-1, aligned_wmask, size)
  def mask_helper(in_0 : Bool, range : UInt): (Bool, Bool, UInt, UInt) = {
    val len = range.getWidth
    if (len == 1) {
      (range === UInt(0), range === UInt(1), in_0.asUInt() & range, UInt(0))
    } else {
      val mid = len / 2
      val lo  = range(mid-1, 0)
      val hi  = range(len-1, mid)
      val (lo_0, lo_1, lo_m, lo_s) = mask_helper(in_0,         lo)
      val (hi_0, hi_1, hi_m, hi_s) = mask_helper(in_0 && lo_0, hi)
      val out_0 = lo_0 && hi_0
      val out_1 = lo_1 && hi_1
      val out_m = Cat(hi_m, lo_m) | Fill(len, (in_0 && out_1).asUInt())
      val out_s = Mux(out_1, UInt(log2Up(len)), Mux(lo_0, hi_s, lo_s))
      (out_0, out_1, out_m, out_s)
    }
  }
  
  val pending_wmask = acq_wmask & ~done_wmask
  val put_addr = PriorityEncoder(pending_wmask)
  val (wmask_0, _, exec_wmask, put_size) = mask_helper(Bool(true), pending_wmask)
  next_wmask := done_wmask | exec_wmask
  
  // Calculate the address, with consideration to put fragments and bursts
  val addr_block = io.acquire.bits.addr_block
  val addr_beatin= io.acquire.bits.addr_beat
  val addr_burst = Mux(isReadBurst, addr_beatin + burst, addr_beatin)
  val addr_byte  = Mux(isPut, put_addr, io.acquire.bits.addr_byte())
  val addr_beat  = Mux(isWriteBurst, UInt(0), addr_burst)
  val ahbAddr    = Cat(addr_block, addr_burst, addr_byte)
  val ahbSize    = Mux(isPut, put_size, Mux(isBurst, UInt(log2Ceil(tlDataBytes)), io.acquire.bits.op_size()))
  
  val ahbBurst = MuxLookup(io.acquire.bits.a_type, HBURST_SINGLE, Array(
    Acquire.getType         -> HBURST_SINGLE,
    Acquire.getBlockType    -> burstSize,
    Acquire.putType         -> HBURST_SINGLE,
    Acquire.putBlockType    -> burstSize,
    Acquire.putAtomicType   -> HBURST_SINGLE,
    Acquire.getPrefetchType -> HBURST_SINGLE,
    Acquire.putPrefetchType -> HBURST_SINGLE))
  
  val ahbWrite = MuxLookup(io.acquire.bits.a_type, Bool(false), Array(
    Acquire.getType         -> Bool(false),
    Acquire.getBlockType    -> Bool(false),
    Acquire.putType         -> Bool(true),
    Acquire.putBlockType    -> Bool(true),
    Acquire.putAtomicType   -> MuxLookup(atom_state, Bool(false), Array(
      s_atom_r              -> Bool(false),
      s_atom_idle1          -> Bool(false),  // don't care
      s_atom_w              -> Bool(true),
      s_atom_idle2          -> Bool(true))), // don't care
    Acquire.getPrefetchType -> Bool(false),  // don't care
    Acquire.putPrefetchType -> Bool(true)))  // don't care
  
  val ahbExecute = MuxLookup(io.acquire.bits.a_type, Bool(false), Array(
    Acquire.getType         -> Bool(true),
    Acquire.getBlockType    -> Bool(true),
    Acquire.putType         -> !wmask_0,  // handle the case of a Put with no bytes!
    Acquire.putBlockType    -> Bool(true),
    Acquire.putAtomicType   -> MuxLookup(atom_state, Bool(false), Array(
      s_atom_r              -> Bool(true),
      s_atom_idle1          -> Bool(false),
      s_atom_w              -> Bool(true),
      s_atom_idle2          -> Bool(false))),
    Acquire.getPrefetchType -> Bool(false),
    Acquire.putPrefetchType -> Bool(false)))
  
  val respondTL = MuxLookup(io.acquire.bits.a_type, Bool(false), Array(
    Acquire.getType         -> Bool(true),
    Acquire.getBlockType    -> Bool(true),
    Acquire.putType         -> last_wmask,
    Acquire.putBlockType    -> last_burst,
    Acquire.putAtomicType   -> MuxLookup(atom_state, Bool(false), Array(
      s_atom_r              -> Bool(true), // they want the old data
      s_atom_idle1          -> Bool(false),
      s_atom_w              -> Bool(false),
      s_atom_idle2          -> Bool(false))),
    Acquire.getPrefetchType -> Bool(true),
    Acquire.putPrefetchType -> Bool(true)))
  
  io.request.valid                := io.acquire.valid
  io.request.bits.htrans          := HTRANS_IDLE // unused/ignored
  io.request.bits.haddr           := ahbAddr
  io.request.bits.hmastlock       := isAtomic && atom_state =/= s_atom_idle2
  io.request.bits.hwrite          := ahbWrite
  io.request.bits.hburst          := ahbBurst
  io.request.bits.hsize           := ahbSize
  io.request.bits.hprot           := HPROT_DATA | HPROT_PRIVILEGED
  io.request.bits.hwdata          := io.acquire.bits.data
  io.request.bits.executeAHB      := ahbExecute
  io.request.bits.respondTL       := respondTL
  io.request.bits.latchAtom       := isAtomic && atom_state === s_atom_r
  io.request.bits.firstBurst      := burst === firstBurst
  io.request.bits.finalBurst      := burst === finalBurst || !isBurst
  io.request.bits.cmd             := io.acquire.bits.op_code()
  io.request.bits.is_builtin_type := Bool(true)
  io.request.bits.g_type          := io.acquire.bits.getBuiltInGrantType()
  io.request.bits.client_xact_id  := io.acquire.bits.client_xact_id
  io.request.bits.addr_beat       := addr_beat

  val debugBurst = Reg(UInt())
  when (io.request.valid) {
    debugBurst := addr_burst - burst
  }
  
  // We only support built-in TileLink requests
  assert(!io.acquire.valid || io.acquire.bits.is_builtin_type, "AHB bridge only supports builtin TileLink types")
  // Ensure alignment of address to size
  assert(!io.acquire.valid || (ahbAddr & ((UInt(1) << ahbSize) - UInt(1))) === UInt(0), "TileLink operation misaligned")
  // If this is a putBlock, make sure it moves properly
  assert(!io.acquire.valid || !isBurst || burst === firstBurst || debugBurst === addr_burst - burst, "TileLink putBlock beats not sequential")
  // We better not get an incomplete TileLink acquire
  assert(!io.acquire.valid || isBurst  || burst === firstBurst, "TileLink never completed a putBlock")
  // If we disabled atomic support, we better not see a request
  assert(!io.acquire.bits.is(Acquire.putAtomicType) || Bool(supportAtomics))
}

// AHB stage2: execute AHBRequests
class AHBBusMaster(supportAtomics: Boolean = false)(implicit val p: Parameters) extends Module
    with HasHastiParameters
    with HasTileLinkParameters
    with HasAddrMapParameters {
  val io = new Bundle {
    val request = new DecoupledIO(new AHBRequestIO).flip
    val grant   = new DecoupledIO(new Grant)
    val ahb     = new HastiMasterIO()
  }
  
  // All AHB outputs are registered (they might be IOs)
  val midBurst  = Reg(init = Bool(false))
  val htrans    = Reg(init = HTRANS_IDLE)
  val haddr     = Reg(UInt())
  val hmastlock = Reg(init = Bool(false))
  val hwrite    = Reg(Bool())
  val hburst    = Reg(UInt())
  val hsize     = Reg(init = UInt(0, width = SZ_HSIZE))
  val hprot     = Reg(UInt())
  val hwdata0   = Reg(Bits())
  val hwdata1   = Reg(Bits())
  val hrdata    = Reg(Bits())
  
  io.ahb.htrans    := htrans
  io.ahb.haddr     := haddr
  io.ahb.hmastlock := hmastlock
  io.ahb.hwrite    := hwrite
  io.ahb.hburst    := hburst
  io.ahb.hsize     := hsize
  io.ahb.hprot     := hprot
  io.ahb.hwdata    := hwdata1 // one cycle after the address phase
  
  // TileLink response data needed in data phase
  val respondTL0      = Reg(init = Bool(false))
  val respondTL1      = Reg(init = Bool(false))
  val latchAtom0      = Reg(init = Bool(false))
  val latchAtom1      = Reg(init = Bool(false))
  val executeAHB0     = Reg(init = Bool(false))
  val executeAHB1     = Reg(init = Bool(false))
  val bubble          = Reg(init = Bool(true)) // nothing useful in address phase
  val cmd             = Reg(Bits())
  val g_type0         = Reg(UInt())
  val g_type1         = Reg(UInt())
  val client_xact_id0 = Reg(Bits())
  val client_xact_id1 = Reg(Bits())
  val addr_beat0      = Reg(UInt())
  val addr_beat1      = Reg(UInt())
  val grant1          = Reg(new Grant)
  
  // It is allowed to progress from Idle/Busy during a wait state
  val addrReady = io.ahb.hready || bubble || (!executeAHB1 && !executeAHB0)
  val dataReady = io.ahb.hready || !executeAHB1
  
  // Only accept a new AHBRequest if we have enough buffer space in the pad
  // to accomodate a persistent drop in TileLink's grant.ready
  io.request.ready := addrReady && io.grant.ready
  
  // htrans must be updated even if no request is valid
  when (addrReady) {
    when (io.request.fire() && io.request.bits.executeAHB) {
      midBurst := !io.request.bits.finalBurst
      when (io.request.bits.firstBurst) {
        htrans := HTRANS_NONSEQ
      } .otherwise {
        htrans := HTRANS_SEQ
      }
    } .otherwise {
      when (midBurst) {
        htrans := HTRANS_BUSY
      } .otherwise {
        htrans := HTRANS_IDLE
      }
    }
  }

  // Address phase, clear repondTL when we have nothing to do
  when (addrReady) {
    when (io.request.fire()) {
      respondTL0 := io.request.bits.respondTL
      latchAtom0 := io.request.bits.latchAtom
      executeAHB0:= io.request.bits.executeAHB
      bubble     := Bool(false)
    } .otherwise {
      respondTL0 := Bool(false)
      latchAtom0 := Bool(false)
      executeAHB0:= Bool(false)
      bubble     := Bool(true) // an atom-injected Idle is not a bubble!
    }
  }

  // Transfer bulk address phase
  when (io.request.fire()) {
    haddr     := io.request.bits.haddr
    hmastlock := io.request.bits.hmastlock
    hwrite    := io.request.bits.hwrite
    hburst    := io.request.bits.hburst
    hsize     := io.request.bits.hsize
    hprot     := io.request.bits.hprot
    hwdata0   := io.request.bits.hwdata
    cmd             := io.request.bits.cmd
    g_type0         := io.request.bits.g_type
    client_xact_id0 := io.request.bits.client_xact_id
    addr_beat0      := io.request.bits.addr_beat
  }
  
  // Execute Atomic ops; unused and optimized away if !supportAtomics
  val amo_p = p.alterPartial({
    case CacheBlockOffsetBits => hastiAddrBits
  })
  val alu = Module(new AMOALU(hastiDataBits, rhsIsAligned = true)(amo_p))
  alu.io.addr := haddr
  alu.io.cmd  := cmd
  alu.io.typ  := hsize
  alu.io.rhs  := hwdata0
  alu.io.lhs  := hrdata
  
  // Transfer bulk data phase
  when (dataReady) {
    when (addrReady) {
      respondTL1    := respondTL0
      latchAtom1    := latchAtom0
      executeAHB1   := executeAHB0
    } .otherwise {
      respondTL1    := Bool(false)
      latchAtom1    := Bool(false)
      executeAHB1   := Bool(false)
    }
    hwdata1         := Mux(Bool(supportAtomics), alu.io.out, hwdata0)
    g_type1         := g_type0
    client_xact_id1 := client_xact_id0
    addr_beat1      := addr_beat0
  }
  
  // Latch the read result for an atomic operation
  when (dataReady && latchAtom1) {
    hrdata := io.ahb.hrdata
  }
  
  // Only issue TL grant when the slave has provided data
  io.grant.valid := dataReady && respondTL1
  io.grant.bits := Grant(
      is_builtin_type = Bool(true),
      g_type          = g_type1,
      client_xact_id  = client_xact_id1,
      manager_xact_id = UInt(0),
      addr_beat       = addr_beat1,
      data            = io.ahb.hrdata)

  // We cannot support errors from AHB to TileLink
  assert(!io.ahb.hresp, "AHB hresp error detected and cannot be reported via TileLink")
}

class AHBBridge(supportAtomics: Boolean = true)(implicit val p: Parameters) extends Module
    with HasHastiParameters
    with HasTileLinkParameters
    with HasAddrMapParameters {
  val io = new Bundle {
    val tl  = new ClientUncachedTileLinkIO().flip
    val ahb = new HastiMasterIO()
  }
  
  // Hasti and TileLink widths must agree at this point in the topology
  require (tlDataBits == hastiDataBits)
  require (p(PAddrBits) == hastiAddrBits)
  
  // AHB does not permit bursts to cross a 1KB boundary
  require (tlDataBits * tlDataBeats <= 1024*8)
  // tlDataBytes must be a power of 2
  require (1 << log2Ceil(tlDataBytes) == tlDataBytes)
  
  // Create the sub-blocks
  val fsm = Module(new AHBTileLinkIn(supportAtomics))
  val bus = Module(new AHBBusMaster(supportAtomics))
  val pad = Module(new Queue(new Grant, 4))
  
  fsm.io.acquire <> Queue(io.tl.acquire, 2) // Pipe is also acceptable
  bus.io.request <> fsm.io.request
  io.ahb         <> bus.io.ahb
  io.tl.grant    <> pad.io.deq
  
  // The pad is needed to absorb AHB progress while !grant.ready
  // We are only 'ready' if the pad has at least 3 cycles of space
  bus.io.grant.ready := pad.io.count <= UInt(1)
  pad.io.enq.bits  := bus.io.grant.bits
  pad.io.enq.valid := bus.io.grant.valid
}
