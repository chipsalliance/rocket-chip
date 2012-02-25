package Top {

import Chisel._
import Constants._
import hwacha.GenArray

class HubMemReq extends Bundle {
  val rw   = Bool()
  val addr = UFix(width = PADDR_BITS-OFFSET_BITS)
  val tag  = Bits(width = GLOBAL_XACT_ID_BITS)
  // Figure out which data-in port to pull from
  val data_idx = Bits(width = TILE_ID_BITS)
  val is_probe_rep = Bool()
}

class MemData extends Bundle {
  val data = Bits(width = MEM_DATA_BITS)
}

class TransactionInit extends Bundle {
  val t_type = Bits(width = TTYPE_BITS)
  val has_data = Bool()
  val tile_xact_id = Bits(width = TILE_XACT_ID_BITS)
  val address = Bits(width = PADDR_BITS)
}

class TransactionInitData extends MemData

class TransactionAbort extends Bundle {
  val tile_xact_id = Bits(width = TILE_XACT_ID_BITS)
}

class ProbeRequest extends Bundle {
  val p_type = Bits(width = PTYPE_BITS)
  val global_xact_id = Bits(width = GLOBAL_XACT_ID_BITS)
  val address = Bits(width = PADDR_BITS)
}

class ProbeReply extends Bundle {
  val p_type = Bits(width = PTYPE_BITS)
  val has_data = Bool()
  val global_xact_id = Bits(width = GLOBAL_XACT_ID_BITS)
}

class ProbeReplyData extends MemData

class TransactionReply extends Bundle {
  val t_type = Bits(width = TTYPE_BITS)
  val has_data = Bool()
  val tile_xact_id = Bits(width = TILE_XACT_ID_BITS)
  val global_xact_id = Bits(width = GLOBAL_XACT_ID_BITS)
}

class TransactionReplyData extends MemData

class TransactionFinish extends Bundle {
  val global_xact_id = Bits(width = GLOBAL_XACT_ID_BITS)
}

class ioTileLink extends Bundle { 
  val xact_init      = (new ioDecoupled) { new TransactionInit() }.flip
  val xact_init_data = (new ioDecoupled) { new TransactionInitData() }.flip
  val xact_abort     = (new ioDecoupled) { new TransactionAbort() }
  val probe_req      = (new ioDecoupled) { new ProbeRequest() }
  val probe_rep      = (new ioDecoupled) { new ProbeReply() }.flip
  val probe_rep_data = (new ioDecoupled) { new ProbeReplyData() }.flip
  val xact_rep       = (new ioDecoupled) { new TransactionReply() }
  val xact_rep_data  = (new ioDecoupled) { new TransactionReplyData() }
  val xact_finish    = (new ioDecoupled) { new TransactionFinish() }.flip
}

trait CoherencePolicy {
  def cpuCmdToRW( cmd: Bits): (Bool, Bool) = {
    val store   = (cmd === M_XWR)
    val load    = (cmd === M_XRD)
    val amo     = cmd(3).toBool
    val read    = load  || amo || (cmd === M_PFR)
    val write   = store || amo || (cmd === M_PFW)
    (read, write)
  }
}

trait ThreeStateIncoherence extends CoherencePolicy {
  val tileInvalid :: tileClean :: tileDirty :: Nil = Enum(3){ UFix() }

  def isHit ( cmd: Bits, state: UFix): Bool = {
    val (read, write) = cpuCmdToRW(cmd)
    ( state === tileClean || state === tileDirty)
  }

  def isValid (state: UFix): Bool = {
    state != tileInvalid
  }

  def needsWriteback (state: UFix): Bool = {
    state === tileDirty
  }

  def newStateOnWriteback() = tileInvalid
  def newStateOnFlush() = tileInvalid
  def newState(cmd: Bits, state: UFix): UFix = {
    val (read, write) = cpuCmdToRW(cmd)
    Mux(write, tileDirty, Mux(read, Mux(state === tileDirty, tileDirty, tileClean), state))
  }
  def newStateOnHit(cmd: Bits, state: UFix): UFix = newState(cmd, state)
  def newStateOnPrimaryMiss(cmd: Bits): UFix = newState(cmd, tileInvalid)
  def newStateOnSecondaryMiss(cmd: Bits, state: UFix): UFix = {
    val (read, write) = cpuCmdToRW(cmd)
    Mux(write, tileDirty, state)
  }

}

trait FourStateCoherence extends CoherencePolicy {

  val tileInvalid :: tileShared :: tileExclusiveClean :: tileExclusiveDirty :: Nil = Enum(4){ UFix() }
  val globalInvalid :: globalShared :: globalExclusiveClean :: Nil = Enum(3){ UFix() }
  val probeInvalidate :: probeDowngrade :: probeCopy :: Nil = Enum(3){ UFix() }

  def isHit ( cmd: Bits, state: UFix): Bool = {
    val (read, write) = cpuCmdToRW(cmd)
    ((read && ( state === tileShared || state === tileExclusiveClean || state === tileExclusiveDirty)) ||
     (write && (state === tileExclusiveClean || state === tileExclusiveDirty)))
  }

  def isValid (state: UFix): Bool = {
    state != tileInvalid
  }

  def needsWriteback (state: UFix): Bool = {
    state === tileExclusiveDirty
  }

  def newStateOnWriteback() = tileInvalid
  def newStateOnFlush() = tileInvalid

  // TODO: New funcs as compared to incoherent protocol:
  def newState(cmd: Bits, state: UFix): UFix
  def newStateOnHit(cmd: Bits, state: UFix): UFix 
  def newStateOnPrimaryMiss(cmd: Bits): UFix 
  def newStateOnSecondaryMiss(cmd: Bits, state: UFix): UFix 

  def needsSecondaryXact (cmd: Bits, outstanding: TransactionInit): Bool

  def getMetaUpdateOnProbe (incoming: ProbeRequest): Bits = {
    val state = UFix(0)
    switch(incoming.p_type) {
      is(probeInvalidate) { state := tileInvalid }
      is(probeDowngrade) { state := tileShared }
    }
    state.toBits
  }
}

class XactTracker(id: Int) extends Component {
  val io = new Bundle {
    val xact_init   = (new ioDecoupled) { new TransactionInit() }
    val probe_rep   = (new ioDecoupled) { new ProbeReply() }
    val probe_req   = (new ioDecoupled) { new ProbeRequest() }.flip
    val xact_rep    = (new ioDecoupled) { new TransactionReply() }.flip
    val mem_req     = (new ioDecoupled) { new MemReq()  }.flip
    val xact_finish = Bool(INPUT)
    val busy        = Bool(OUTPUT)
    val addr         = Bits(PADDR_BITS, OUTPUT)
    val tile_id      = Bits(TILE_ID_BITS, OUTPUT)
    val tile_xact_id = Bits(TILE_XACT_ID_BITS, OUTPUT)
    val sharer_count = Bits(TILE_ID_BITS, OUTPUT)
    val ttype        = Bits(TTYPE_BITS, OUTPUT)
  }

  val valid = Reg(resetVal = Bool(false))
  val addr  = Reg{ Bits() }
  val ttype = Reg{ Bits() }
  val tile_id = Reg{ Bits() }
  val tile_xact_id = Reg{ Bits() }
  val probe_done = Reg{ Bits() }
  
}

abstract class CoherenceHub extends Component

class CoherenceHubNoDir extends CoherenceHub {

  def coherenceConflict(addr1: Bits, addr2: Bits): Bool = {
    addr1(PADDR_BITS-1, OFFSET_BITS) === addr2(PADDR_BITS-1, OFFSET_BITS)
  }
  def getTransactionReplyType(ttype: UFix, count: UFix): Bits = {
    val ret = Wire() { Bits(width = TTYPE_BITS) }
    switch (ttype) {
      is(X_READ_SHARED) { ret := Mux(count > UFix(0), X_READ_SHARED, X_READ_EXCLUSIVE) }
      is(X_READ_EXCLUSIVE) { ret := X_READ_EXCLUSIVE }
      is(X_READ_UNCACHED)  { ret := X_READ_UNCACHED  }
      is(X_WRITE_UNCACHED) { ret := X_WRITE_UNCACHED }
    }
    ret 
  }

  val io = new Bundle {
    val tiles = Vec(NTILES) { new ioTileLink() }
    val mem = new ioDCache().flip
  }
  
  val trackerList      = (0 until NGLOBAL_XACTS).map(new XactTracker(_))
  val busy_arr         = GenArray(NGLOBAL_XACTS){ Wire(){Bool()} }
  val addr_arr         = GenArray(NGLOBAL_XACTS){ Wire(){Bits(width=PADDR_BITS)} }
  val tile_id_arr      = GenArray(NGLOBAL_XACTS){ Wire(){Bits(width=TILE_ID_BITS)} }
  val tile_xact_id_arr = GenArray(NGLOBAL_XACTS){ Wire(){Bits(width=TILE_XACT_ID_BITS)} }
  val sh_count_arr     = GenArray(NGLOBAL_XACTS){ Wire(){Bits(width=TILE_ID_BITS)} }
  val ttype_arr        = GenArray(NGLOBAL_XACTS){ Wire(){Bits(width=TTYPE_BITS)} }
  val free_arr         = GenArray(NGLOBAL_XACTS){ Wire(){Bool()} }
  for( i <- 0 until NGLOBAL_XACTS) {
    busy_arr.write(        UFix(i), trackerList(i).io.busy)
    addr_arr.write(        UFix(i), trackerList(i).io.addr)
    tile_id_arr.write(     UFix(i), trackerList(i).io.tile_id)
    tile_xact_id_arr.write(UFix(i), trackerList(i).io.tile_xact_id)
    ttype_arr.write(        UFix(i), trackerList(i).io.ttype)
    sh_count_arr.write(        UFix(i), trackerList(i).io.sharer_count)
    trackerList(i).io.xact_finish := free_arr.read(UFix(i))
  }

  // Nack conflicting transaction init attempts
  val aborting   = Wire() { Bits(width = NTILES) } 
  val initiating = Wire() { Bits(width = NTILES) } 
  for( j <- 0 until NTILES ) {
    val init   = io.tiles(j).xact_init
    val abort  = io.tiles(j).xact_abort
    val conflicts = Bits(width = NGLOBAL_XACTS) 
    for( i <- 0 until NGLOBAL_XACTS) {
      val t = trackerList(i).io
      conflicts(i) := t.busy(i) && coherenceConflict(t.addr, init.bits.address)
    }
    aborting(j) := (conflicts.orR || busy_arr.flatten().andR)
    abort.valid := init.valid && aborting
    abort.bits.tileTransactionID := init.bits.tileTransactionID
    init.ready  := aborting(j) || initiating(j)
  }
  
  // Free finished transactions
  for( j <- 0 until NTILES ) {
    val finish = io.tiles(j).xact_finish
    free_arr.write(finish.bits.globalTransactionID, finish.valid)
    finish.ready := Bool(true)
  }

  // Forward memory responses from mem to tile
  val xrep_cnt = Reg(resetVal = UFix(0, log2up(REFILL_CYCLES)))
  val xrep_cnt_next = xrep_cnt + UFix(1)
  when (io.mem.resp_val) { xrep_cnt := xrep_cnt_next }
  val idx = io.mem.resp_tag
  val readys = Bits(width = NTILES) 
  for( j <- 0 until NTILES ) {
    io.tiles(j).xact_rep.bits.ttype := getTransactionReplyType(ttype_arr.read(idx), sh_count_arr.read(idx))
    io.tiles(j).xact_rep.bits.tileTransactionID := tile_xact_id_arr.read(idx)
    io.tiles(j).xact_rep.bits.globalTransactionID := idx
    io.tiles(j).xact_rep_data.bits.data := io.mem.resp_data
    readys := Mux(xrep_cnt === UFix(0), io.tiles(j).xact_rep.ready && io.tiles(j).xact_rep_data.ready, io.tiles(j).xact_rep_data.ready)
    val this_rep_valid = UFix(j) === tile_id_arr.read(idx) && io.mem.resp_val
    io.tiles(j).xact_rep.valid      := this_rep_valid && xrep_cnt === UFix(0) 
    io.tiles(j).xact_rep_data.valid := this_rep_valid
  }
  // If there were a ready signal due to e.g. intervening network:
  //io.mem.resp_rdy := readys(tile_id_arr.read(idx)).xact_rep.ready
  

  // Pick a single request of these types to process
  //val xact_init_arb   = (new Arbiter(NTILES)) { new TransactionInit() }
  //val probe_reply_arb = (new Arbiter(NTILES)) { new ProbeReply() }


}

}
