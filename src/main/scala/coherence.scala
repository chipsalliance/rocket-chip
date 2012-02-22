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
  val data = Bits(width = MEM_DATA_BITS)
}

class TransactionReply extends Bundle {
  val ttype = Bits(width = TTYPE_BITS)
  val tileTransactionID = Bits(width = TILE_XACT_ID_BITS)
  val globalTransactionID = Bits(width = GLOBAL_XACT_ID_BITS)
  val data = Bits(width = MEM_DATA_BITS)
}

class TransactionFinish extends Bundle {
  val globalTransactionID = Bits(width = GLOBAL_XACT_ID_BITS)
}

class ioTileLink extends Bundle { 
  val xact_init   = new TransactionInit().asOutput
  val xact_abort  = new TransactionAbort().asInput
  val probe_req   = new ProbeRequest().asInput
  val probe_rep   = new ProbeReply().asOutput
  val xact_rep    = new TransactionReply().asInput
  val xact_finish = new TransactionFinish().asOutput
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

}
