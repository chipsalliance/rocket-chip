// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.rocket

import Chisel._
import Chisel.ImplicitConversions._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tile.{HasCoreParameters, CoreModule, CoreBundle}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import chisel3.experimental.dontTouch
import TLMessages._

class SPFLookupReq(implicit p: Parameters) extends CoreBundle()(p) {
  val addr = UInt(width = paddrBits)
}

class SPFPrefReq(implicit p: Parameters) extends CoreBundle()(p) {
  val addr = UInt(width = paddrBits)
}

object SPFCRs {
  val ctrl = 0x00
}

trait SPFParamsBase {
  val nStreams:       Int
  val address:        Option[BigInt]
  val size:           Int
  val distance:       Int
  val prefQueueSize:  Int
  val pageCross:      Boolean
  val ageOut:         Boolean
  val ageBits:        Int
  val wordBytes:      Int
  val distBits:       Int
  val distDefault:    Int
}

case class SPFParams (
  nStreams:       Int = 4,                // # of independent steams to track
  address:        Option[BigInt] = None,  // # memory-mapped address of SPF TL slave interface for control registers
  size:           Int = 0x1000,           // Size in bytes of TL slave address space
  distance:       Int = 3,                // # of cache blocks to prefetch ahead
  prefQueueSize:  Int = 8,                // # of entries in prefetch queue
  pageCross:      Boolean = false,        // Allow prefetching across page boundaries?
  wordBytes:      Int = 4,                // # of words in TL slave access
  ageOut:         Boolean = true,         // Support timers to age out (and mark invalid) inactive entries
  ageBits:        Int = 13,               // # of bits to encode age, for aging out
  distBits:       Int = 4,                // # of bits to encode prefetch distance field
  distDefault:    Int = 3)                // Default value for prefetch distance
  extends SPFParamsBase {

  require(distDefault > 0)
}

abstract class SPFBundle(val c: SPFParamsBase) extends Bundle

// Layout for memory-mapped control register
class SPFControl(c: SPFParamsBase) extends SPFBundle(c) {
  val en          = Bool()
  val crossPageEn = Bool()
  val ageOutEn    = Bool()
  val dist        = UInt(width=c.distBits)
}

object SPFControl {
  def init(c: SPFParamsBase): SPFControl = {
    val ctrl = Wire(new SPFControl(c))

    ctrl.en           := Bool(false)
    ctrl.crossPageEn  := Bool(false)
    ctrl.ageOutEn     := Bool(false)
    ctrl.dist         := UInt(c.distDefault)
    ctrl
  }
}

// Create free-listed array of prefetch requests. Requests sent out over TL may be ack'ed out-of-order,
// and we need to hold onto request until it is ACK'ed.
// Note: This won't guarantee that requests will be sent out in the same order generated
class SPFReqArray(nEntries: Int) (implicit p: Parameters) extends Module {
  val io = new Bundle {
    val enq     = Decoupled(new SPFPrefReq).flip
    val deq     = Decoupled(new SPFPrefReq)
    val deqIdx  = UInt(width=log2Up(nEntries)).asOutput

    // When we get HintAck we can invalidate corresponding entry
    val ack     = Valid(UInt(width=log2Up(nEntries))).flip
  }

  // State for each entry
  val valids = Reg(init = UInt(0, nEntries))
  val sent   = Reg(init = UInt(0, nEntries))
  val data   = Reg(Vec(nEntries, new SPFPrefReq))

  // Replace by finding lowest-indexed invalid entry
  val replEntryIdx = PriorityEncoder(~valids)
  val replEntryVec = PriorityEncoderOH(~valids) & Fill(nEntries, io.enq.fire())

  // Request is "ready" if it is valid and has not already sent a request
  // Pick lowest-indexed ready entry for next request
  val readyVec = valids & ~sent
  val pickVec  = PriorityEncoderOH(readyVec) & Fill(nEntries, io.deq.fire())
  val ackVec   = UIntToOH(io.ack.bits) & Fill(nEntries, io.ack.valid)

  io.enq.ready := !(valids.andR)
  when (io.enq.fire()) {
    data(replEntryIdx) := io.enq.bits
  }
  valids := (valids | replEntryVec) & ~ackVec
  sent   := (sent   | pickVec)      & ~ackVec
  
  io.deq.valid     := readyVec.orR
  io.deq.bits.data := PriorityMux(readyVec, data)
  io.deqIdx        := PriorityEncoder(readyVec)
}

trait HasSPFParameters extends HasCoreParameters {
  val blockIdxBits   = paddrBits - lgCacheBlockBytes    // # of bits to address one cache block in PA space
  val pgBlockIdxBits = pgIdxBits - lgCacheBlockBytes    // # of bits to address one cache block in a single page
}

class SPFModule(c: SPFParamsBase, outer: TLSPF) (implicit p: Parameters) extends LazyModuleImp(outer) 
  with HasSPFParameters {
  val io = IO(new Bundle {
    val lookupReq = Valid(new SPFLookupReq).flip
  })

  val (tl_in, edge_in) = outer.node.in(0)
  val (tl_out, edge_out) = outer.masterNode.out(0)
  val ctrl = Reg(init = SPFControl.init(c))

  val lookupValid = tl_in.a.fire() && (tl_in.a.bits.opcode === AcquireBlock) && ctrl.en
  val lookupAddr = (tl_in.a.bits.address >> lgCacheBlockBytes)   // Don't care about block offset bits
  val replStream = Wire(UInt(width = log2Up(c.nStreams)))

  class Entry extends Bundle {
    val prevAddr    = UInt(width = blockIdxBits)          // Last address seen
    val sentAddr    = UInt(width = blockIdxBits)          // Last address prefetched
    val stride      = UInt(width = blockIdxBits)          // Computed stride (can be negative)
    val strideSign  = UInt(width=1)                       // sign of stride (1 = positive, 0 = negative)
    val strideValid = Bool()                              // Is computed stride valid?
    val sentValid   = Bool()                              // Is sentAddr valid?
    val armed       = Bool()                              // Is this entry generating prefetches?
    val genCount    = UInt(width = log2Up(c.distance))    // # of prefetches 
    val paused      = Bool()                              // We crossed a page and will wait for a miss on the next sequential page
    val age         = UInt(width = c.ageBits)             // # of cycles since generating a prefetch
  }

  protected val regmapBase = Seq(
    SPFCRs.ctrl -> RegFieldGroup("ctrl", Some("SPF Control"), Seq(
      RegField(1,          ctrl.en,           RegFieldDesc("en",          "Prefetcher global enable",             reset=Some(0))),
      RegField(1,          ctrl.crossPageEn,  RegFieldDesc("crossPageEn", "Enable prefetching cross-page",        reset=Some(0))),
      RegField(1,          ctrl.ageOutEn,     RegFieldDesc("ageOutEn",    "Enabling aging out inactive entries",  reset=Some(0))),
      RegField(c.distBits, ctrl.dist,         RegFieldDesc("dist",        "Prefetch distance",                    reset=Some(c.distDefault))))) 
  )

  // Mask Page offset and Block offset from address
  def maskPage(addr: UInt) = {
    val mask = Wire(UInt(width=blockIdxBits))
    mask := (UIntToOH(pgBlockIdxBits) - 1)
    addr & ~mask
  }

  // Get Page offset bits while masking block offset bits
  def getPageBlock(addr: UInt) = {
    val mask = Wire(UInt(width=blockIdxBits))
    mask := (UIntToOH(pgBlockIdxBits) - 1)
    addr & mask
  }

  // Generate address of next cache block by: incrementing page offset. Note that addresses stored in prefetch entries
  // exclude block offset bits
  def genNextAddr(addr: UInt, stride: UInt): (UInt, UInt) = {
    val (crossPage, pgOffset) = Split(addr(pgBlockIdxBits-1, 0) +& stride, pgBlockIdxBits)
    val ppnP1 = addr(blockIdxBits-1,pgBlockIdxBits) + 1
    val nextAddr = Cat( Mux(crossPage(0), ppnP1, addr(blockIdxBits-1,pgBlockIdxBits)), pgOffset) 
    (crossPage, nextAddr)
  }

  // Queue for prefetch requests
  val reqQueue = Module(new SPFReqArray(c.prefQueueSize))

  // State Elements
  val valids      = Reg(init = UInt(0, c.nStreams))
  val regEntries  = Reg(Vec(c.nStreams, new Entry))
  val entries     = regEntries.map(_.asTypeOf(new Entry))

  // Detect when prefetcher is being disabled, so we can clear valids and handle cleanup
  val disabling = RegNext(ctrl.en) && !ctrl.en

  // ------------------------------------------------------
  // Incoming request lookup portion
  // ------------------------------------------------------
  
  // To prevent allocating a new entry when a stream crosses a page, look for the (lowest-indexed) paused entry
  // and see if the lookup address matches the paused entry's PPN + 1.  Then we will restart the paused stream
  val pausedVec = entries.map(_.paused).asUInt & valids
  val pausedEntry = PriorityMux(pausedVec, entries)
  val pausedPpnP1 = pausedEntry.prevAddr(blockIdxBits-1,pgBlockIdxBits) + 1
  val pausedPpnMatch = (pausedPpnP1 === lookupAddr(blockIdxBits-1,pgBlockIdxBits)) && (pausedVec.orR)
  
  // When new lookup request comes in, try to find matching entry.  If no match, check for a match on a paused entry
  val lookupMatchVecPre = entries.map(ent => ent.prevAddr(blockIdxBits-1,pgBlockIdxBits) === lookupAddr(blockIdxBits-1,pgBlockIdxBits)).asUInt & valids
  val usePausedEntry = !lookupMatchVecPre.orR && pausedPpnMatch

  val lookupMatchVec = Mux(usePausedEntry, pausedVec, lookupMatchVecPre)
  val lookupMatchIdx = OHToUInt(lookupMatchVec)
  val lookupMiss = !lookupMatchVec.orR
  val matchEntry = Mux1H(lookupMatchVec, entries)

  assert(PopCount(lookupMatchVec) <= UInt(1), "SPF lookupMatchVec not zero/one-hot")

  val curStride = lookupAddr - matchEntry.prevAddr

  // If lookupAddr falls between matchEntry's prevAddr and sentAddr, trigger an incremental prefetch
  val triggerInc = (getPageBlock(lookupAddr) >= getPageBlock(matchEntry.prevAddr)) &&
                   (getPageBlock(lookupAddr) <= getPageBlock(matchEntry.sentAddr)) &&
                   matchEntry.strideValid &&
                   matchEntry.sentValid
      

  // Did computed stride match previously valid stride?  If so, clear strideValid
  val strideMismatch = (curStride =/= matchEntry.stride) && matchEntry.strideValid

  // Incoming lookup request
  when (lookupValid) {
    when (lookupMiss) {
      // Allocate entry
      regEntries(replStream).prevAddr := lookupAddr
      regEntries(replStream).armed := false.B
      regEntries(replStream).strideValid := false.B
      regEntries(replStream).sentValid := false.B
      regEntries(replStream).paused := false.B
    } .otherwise {

      // Update prevAddr and stride if we are ignoring lookup, but don't generate new prefetches
      regEntries(lookupMatchIdx).prevAddr := lookupAddr
      regEntries(lookupMatchIdx).stride := curStride
      regEntries(lookupMatchIdx).strideValid := !strideMismatch
      regEntries(lookupMatchIdx).strideSign := 1 // TODO: fix this

      // Arm entry if current computed stride matches previously computed stride
      when (!matchEntry.armed && matchEntry.strideValid && !strideMismatch && (!matchEntry.paused || usePausedEntry)) {
        regEntries(lookupMatchIdx).armed := true.B
        regEntries(lookupMatchIdx).paused := false.B
        //regEntries(lookupMatchIdx).genCount := Mux(triggerInc, UInt(0), ctrl.dist-UInt(1))
        regEntries(lookupMatchIdx).genCount := ctrl.dist-UInt(1)
      }
    }
  }

  // Age timers (if supported)
  // - When entry is allocated reset age to 0
  // - When entry is generating prefetch reset age to 0
  // - Otherwise if entry is valid, increment age
  val armedVec = Wire(UInt(width=c.nStreams))
  val agingOutVec = Wire(UInt(width=c.nStreams))

  if (c.ageOut) {
    for (i <- 0 until c.nStreams) {
      when (lookupValid && lookupMiss && (replStream === UInt(i))) {
        regEntries(i).age := UInt(0)
      } .elsewhen (armedVec(i)) {
        regEntries(i).age := UInt(0)
      } .elsewhen (valids(i) && ctrl.ageOutEn) {
        regEntries(i).age := regEntries(i).age + UInt(1)
      }
    }

    agingOutVec := entries.map(_.age === (UIntToOH(c.ageBits) - 1)).asUInt & valids & Fill(c.nStreams,ctrl.ageOutEn)
  } else {
    agingOutVec := UInt(0, c.nStreams)
  }

  val validSet = UIntToOH(replStream, c.nStreams) & Fill(c.nStreams, lookupValid && lookupMiss)
  when (disabling) {
    valids := UInt(0, c.nStreams)
  } .otherwise {
    valids := (valids | validSet) & ~agingOutVec
  }

  // Pseudo-LRU replacement for choosing a stream to replace, unless we are configured for only 1 stream
  if (c.nStreams > 1) {
    val plru = new PseudoLRU(c.nStreams)
    replStream := Mux(!valids(c.nStreams-1, 0).andR, PriorityEncoder(~valids(c.nStreams-1, 0)), plru.replace)

    when (lookupValid && !lookupMiss) {
      plru.access(lookupMatchIdx)
    }
  } else {
    replStream := UInt(0, log2Up(c.nStreams))
  }

  // ------------------------------------------------------
  // Prefetch request generation portion
  // ------------------------------------------------------

  // Select armed entry to generate request
  armedVec := entries.map(_.armed).asUInt & valids
  val anyReqValid = armedVec.orR
  val pickedEntrySel = PriorityEncoder(armedVec)
  val pickedEntry = PriorityMux(armedVec, entries)

  // Generate incremented address
  val baseAddr = Mux(pickedEntry.sentValid, pickedEntry.sentAddr, pickedEntry.prevAddr)
  val (crossPage, baseAddrInc) = genNextAddr(baseAddr, pickedEntry.stride)
  val prefAddr = Cat(baseAddrInc, UInt(0, lgCacheBlockBytes))

  val crossPagePause = crossPage(0) && !ctrl.crossPageEn

  // When generating a new prefetch request, update selected stream
  // If a request would cross a page boundary, and we have crossPageEn==1'b0, then don't enqueue request
  // TODO: Handle case where we are generating prefetches from an entry, and we get a lookup at the same time
  //       with a mismatching stride?
  when (anyReqValid && reqQueue.io.enq.ready) {
    regEntries(pickedEntrySel).armed := (pickedEntry.genCount > UInt(0)) && !crossPagePause
    regEntries(pickedEntrySel).paused := crossPagePause
    regEntries(pickedEntrySel).sentValid := !crossPagePause

    when (!crossPagePause) {
      regEntries(pickedEntrySel).genCount := pickedEntry.genCount - UInt(1)
      regEntries(pickedEntrySel).sentAddr := baseAddrInc
    }
  }

  reqQueue.io.enq.valid := anyReqValid && !crossPagePause
  reqQueue.io.enq.bits.addr := prefAddr
  reqQueue.io.deq.ready := tl_out.a.fire()
  reqQueue.io.ack.valid := tl_out.d.fire()
  reqQueue.io.ack.bits := tl_out.d.bits.source
  
  tl_out.a.valid := reqQueue.io.deq.valid
  tl_out.a.bits := edge_out.Hint(
                    fromSource = reqQueue.io.deqIdx,
                    toAddress = reqQueue.io.deq.bits.addr,
                    lgSize = lgCacheBlockBytes,
                    param = TLHints.PREFETCH_READ)._2
  tl_out.b.ready := Bool(true)
  tl_out.c.valid := Bool(false)
  tl_out.d.ready := Bool(true)
  tl_out.e.valid := Bool(false)
}
  
abstract class TLSPFBase (w: Int, c: SPFParamsBase, hartId: Int)(implicit p: Parameters) extends LazyModule {
  require(isPow2(c.size))
  val device = new SimpleDevice("spf", Seq("sifive,spf0"))

  // Node for register map
  val rnode = TLRegisterNode(address = Seq(AddressSet(c.address.getOrElse(0), c.size-1)), device = device, beatBytes = w)

  // Node for adapter (monitoring channel)
  val node = TLAdapterNode()

  // Master node for generating prefetch hints
  val masterNode = TLClientNode(Seq(TLClientPortParameters(
    Seq(TLClientParameters(
      name = s"Core ${hartId} SPF",
      sourceId = IdRange(0, c.prefQueueSize),
      requestFifo = true
    )))))
}

class TLSPF(w: Int, c: SPFParams, hartId: Int)(implicit p: Parameters) extends TLSPFBase(w,c,hartId)(p) {
  lazy val module = new SPFModule(c, this) {
    rnode.regmap(regmapBase:_*)

    // For now, pass-through the channel we are monitoring to the output
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out <> in
    }
  }
}
