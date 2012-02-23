package Top {

import Chisel._
import Constants._

class TransactionInit extends Bundle {
  val ttype = Bits(width = TTYPE_BITS)
  val tileTransactionID = Bits(width = TILE_XACT_ID_BITS)
  val address = Bits(width = PADDR_BITS)
  val data = Bits(width = MEM_DATA_BITS)
}

class TransactionAbort extends Bundle {
  val tileTransactionID = Bits(width = TILE_XACT_ID_BITS)
}

class ProbeRequest extends Bundle {
  val ptype = Bits(width = PTYPE_BITS)
  val globalTransactionID = Bits(width = GLOBAL_XACT_ID_BITS)
  val address = Bits(width = PADDR_BITS)
}

class ProbeReply extends Bundle {
  val ptype = Bits(width = PTYPE_BITS)
  val hasData = Bool()
  val globalTransactionID = Bits(width = GLOBAL_XACT_ID_BITS)
}

class ProbeReplyData extends Bundle {
  val data = Bits(width = MEM_DATA_BITS)
}

class TransactionReply extends Bundle {
  val ttype = Bits(width = TTYPE_BITS)
  val tileTransactionID = Bits(width = TILE_XACT_ID_BITS)
  val globalTransactionID = Bits(width = GLOBAL_XACT_ID_BITS)
}

class TransactionReplyData extends Bundle {
  val data = Bits(width = MEM_DATA_BITS)
}

class TransactionFinish extends Bundle {
  val globalTransactionID = Bits(width = GLOBAL_XACT_ID_BITS)
}

class ioTileLink extends Bundle { 
  val xact_init      = (new ioDecoupled) { new TransactionInit() }.flip
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
    switch(incoming.ptype) {
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
    val tile_id_in  = Bits(TILE_ID_BITS, INPUT)  
    val tile_id_out = Bits(TILE_ID_BITS, OUTPUT)  
    val ongoing_addr = Bits(PADDR_BITS, OUTPUT)
    val busy        = Bool(OUTPUT)
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
  val io = new Bundle {
    val tiles = Vec(NTILES) { new ioTileLink() }
    val mem = new ioDCache().flip
  }
  
  val trackerList =  (0 until NGLOBAL_XACTS).map(new XactTracker(_))

  // In parallel, every cycle: nack conflicting transactions, free finished ones
  for( j <- 0 until NTILES ) {
    val init   = io.tiles(j).xact_init
    val abort  = io.tiles(j).xact_abort
    val conflicts = Bits(width = NGLOBAL_XACTS) 
    val busys     = Bits(width = NGLOBAL_XACTS) 
    for( i <- 0 until NGLOBAL_XACTS) {
      val t = trackerList(i).io
      busys(i)     := t.busy
      conflicts(i) := t.busy && init.valid && (t.ongoing_addr === init.bits.address)
    }
    abort.valid := conflicts.orR || busys.andR
    abort.bits.tileTransactionID := init.bits.tileTransactionID
    //if abort.rdy, init.pop()
    
  }
  for( i <- 0 until NGLOBAL_XACTS) {
    val t     = trackerList(i).io
    val freed = Bits(width = NTILES) 
    for( j <- 0 until NTILES ) {
      val finish = io.tiles(j).xact_finish
      freed(j)    := finish.valid && (UFix(i) === finish.bits.globalTransactionID)
    }
    t.xact_finish := freed.orR
    //finish.pop()
  }

  // Forward memory responses from mem to tile
  //for( j <- until NTILES ) {
  //  tiles(j).xact_rep.ttype = 
  //  tiles(j).xact_rep.tileTransactionID = 
  //  tiles(j).xact_rep.globalTransactionID = 
  //  val data = Bits
  //
  // Pick a single request of these types to process
  //val xact_init_arb   = (new Arbiter(NTILES)) { new TransactionInit() }
  //val probe_reply_arb = (new Arbiter(NTILES)) { new ProbeReply() }


}

}
