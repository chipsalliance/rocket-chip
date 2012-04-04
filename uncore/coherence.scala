package rocket

import Chisel._
import Constants._

class TransactionInit extends Bundle {
  val x_type = Bits(width = X_INIT_TYPE_MAX_BITS)
  val tile_xact_id = Bits(width = TILE_XACT_ID_BITS)
  val address = UFix(width = PADDR_BITS - OFFSET_BITS)
}

class TransactionInitData extends MemData

class TransactionAbort extends Bundle {
  val tile_xact_id = Bits(width = TILE_XACT_ID_BITS)
}

class ProbeRequest extends Bundle {
  val p_type = Bits(width = P_REQ_TYPE_MAX_BITS)
  val global_xact_id = Bits(width = GLOBAL_XACT_ID_BITS)
  val address = Bits(width = PADDR_BITS - OFFSET_BITS)
}

class ProbeReply extends Bundle {
  val p_type = Bits(width = P_REP_TYPE_MAX_BITS)
  val global_xact_id = Bits(width = GLOBAL_XACT_ID_BITS)
}

class ProbeReplyData extends MemData

class TransactionReply extends MemData {
  val x_type = Bits(width = X_REP_TYPE_MAX_BITS)
  val tile_xact_id = Bits(width = TILE_XACT_ID_BITS)
  val global_xact_id = Bits(width = GLOBAL_XACT_ID_BITS)
  val require_ack = Bool()
}

class TransactionFinish extends Bundle {
  val global_xact_id = Bits(width = GLOBAL_XACT_ID_BITS)
}

object cpuCmdToRW {
  def apply(cmd: Bits): (Bool, Bool) = {
    val store   = (cmd === M_XWR)
    val load    = (cmd === M_XRD)
    val amo     = cmd(3).toBool
    val read    = load  || amo || (cmd === M_PFR) || (cmd === M_PFW)
    val write   = store || amo
    (read, write)
  }
}

trait CoherencePolicy {
  def isHit (cmd: Bits, state: UFix): Bool
  def isValid (state: UFix): Bool

  def needsTransactionOnSecondaryMiss(cmd: Bits, outstanding: TransactionInit): Bool
  def needsTransactionOnCacheControl(cmd: Bits, state: UFix): Bool
  def needsWriteback (state: UFix): Bool

  def newStateOnHit(cmd: Bits, state: UFix): UFix
  def newStateOnCacheControl(cmd: Bits): UFix
  def newStateOnWriteback(): UFix
  def newStateOnFlush(): UFix
  def newStateOnTransactionReply(incoming: TransactionReply, outstanding: TransactionInit): UFix
  def newStateOnProbeRequest(incoming: ProbeRequest, state: UFix): Bits

  def getTransactionInitTypeOnPrimaryMiss(cmd: Bits, state: UFix): UFix
  def getTransactionInitTypeOnSecondaryMiss(cmd: Bits, state: UFix, outstanding: TransactionInit): UFix
  def getTransactionInitTypeOnCacheControl(cmd: Bits): Bits
  def getTransactionInitTypeOnWriteback(): Bits

  def newProbeReply (incoming: ProbeRequest, state: UFix): ProbeReply

  def hasData (reply: ProbeReply): Bool
  def hasData (init: TransactionInit): Bool
  def hasData (reply: TransactionReply): Bool

  def isCoherenceConflict(addr1: Bits, addr2: Bits): Bool
  def getTransactionReplyType(x_type: UFix, count: UFix): Bits
  def getProbeRequestType(x_type: UFix, global_state: UFix): UFix
  def needsMemRead(x_type: UFix, global_state: UFix): Bool
  def needsMemWrite(x_type: UFix, global_state: UFix): Bool
  def needsAckReply(x_type: UFix, global_state: UFix): Bool
}

trait IncoherentPolicy extends CoherencePolicy {
  // UNIMPLEMENTED
  def newStateOnProbeRequest(incoming: ProbeRequest, state: UFix): Bits = state
  def newProbeReply (incoming: ProbeRequest, state: UFix): ProbeReply = { 
    val reply = Wire() { new ProbeReply() }
    reply.p_type := UFix(0)
    reply.global_xact_id := UFix(0)
    reply
  }
  def hasData (reply: ProbeReply) = Bool(false)
  def isCoherenceConflict(addr1: Bits, addr2: Bits): Bool = Bool(false)
  def getTransactionReplyType(x_type: UFix, count: UFix): Bits = Bits(0)
  def getProbeRequestType(x_type: UFix, global_state: UFix): UFix = UFix(0)
  def needsMemRead(x_type: UFix, global_state: UFix): Bool = Bool(false)
  def needsMemWrite(x_type: UFix, global_state: UFix): Bool = Bool(false)
  def needsAckReply(x_type: UFix, global_state: UFix): Bool = Bool(false)
}

trait ThreeStateIncoherence extends IncoherentPolicy {
  val tileInvalid :: tileClean :: tileDirty :: Nil = Enum(3){ UFix() }
  val xactInitReadClean :: xactInitReadDirty :: xactInitWriteback :: Nil = Enum(3){ UFix() }
  val xactReplyData :: xactReplyAck :: Nil = Enum(2){ UFix() }
  val probeRepInvalidateAck :: Nil = Enum(1){ UFix() }

  def isHit ( cmd: Bits, state: UFix): Bool = (state === tileClean || state === tileDirty)
  def isValid (state: UFix): Bool = state != tileInvalid

  def needsTransactionOnSecondaryMiss(cmd: Bits, outstanding: TransactionInit) = Bool(false)
  def needsTransactionOnCacheControl(cmd: Bits, state: UFix): Bool = state === tileDirty
  def needsWriteback (state: UFix): Bool = state === tileDirty

  def newState(cmd: Bits, state: UFix): UFix = {
    val (read, write) = cpuCmdToRW(cmd)
    Mux(write, tileDirty, Mux(read, Mux(state === tileDirty, tileDirty, tileClean), state))
  }
  def newStateOnHit(cmd: Bits, state: UFix): UFix = newState(cmd, state)
  def newStateOnCacheControl(cmd: Bits) = tileInvalid //TODO
  def newStateOnWriteback() = tileInvalid
  def newStateOnFlush() = tileInvalid
  def newStateOnTransactionReply(incoming: TransactionReply, outstanding: TransactionInit) = {
    MuxLookup(incoming.x_type, tileInvalid, Array(
      xactReplyData -> Mux(outstanding.x_type === xactInitReadDirty, tileDirty, tileClean),
      xactReplyAck  -> tileInvalid
    ))
  }

  def getTransactionInitTypeOnPrimaryMiss(cmd: Bits, state: UFix): UFix = {
    val (read, write) = cpuCmdToRW(cmd)
    Mux(write || cmd === M_PFW, xactInitReadDirty, xactInitReadClean)
  }
  def getTransactionInitTypeOnSecondaryMiss(cmd: Bits, state: UFix, outstanding: TransactionInit): UFix = {
    val (read, write) = cpuCmdToRW(cmd)
    Mux(write, xactInitReadDirty, outstanding.x_type)
  }
  def getTransactionInitTypeOnCacheControl(cmd: Bits): Bits = xactInitWriteback //TODO
  def getTransactionInitTypeOnWriteback(): Bits = xactInitWriteback

  def hasData (init: TransactionInit): Bool = (init.x_type === xactInitWriteback)
  def hasData (reply: TransactionReply) = (reply.x_type === xactReplyData)
}

trait FourStateCoherence extends CoherencePolicy {

  val tileInvalid :: tileShared :: tileExclusiveClean :: tileExclusiveDirty :: Nil = Enum(4){ UFix() }
  val globalInvalid :: globalShared :: globalExclusiveClean :: Nil = Enum(3){ UFix() }

  val xactInitReadShared :: xactInitReadExclusive :: xactInitReadUncached :: xactInitWriteUncached :: Nil = Enum(4){ UFix() }
  val xactReplyReadShared :: xactReplyReadExclusive :: xactReplyReadUncached :: xactReplyWriteUncached :: xactReplyReadExclusiveAck :: Nil = Enum(5){ UFix() } 
  val probeReqInvalidate :: probeReqDowngrade :: probeReqCopy :: Nil = Enum(3){ UFix() }
  val probeRepInvalidateData :: probeRepDowngradeData :: probeRepCopyData :: probeRepInvalidateAck :: probeRepDowngradeAck :: probeRepCopyAck :: Nil = Enum(6){ UFix() }

  def isHit (cmd: Bits, state: UFix): Bool = {
    val (read, write) = cpuCmdToRW(cmd)
    Mux(write, (state === tileExclusiveClean || state === tileExclusiveDirty),
        (state === tileShared || state === tileExclusiveClean || state === tileExclusiveDirty))
  }
  def isValid (state: UFix): Bool = {
    state != tileInvalid
  }

  def needsTransactionOnSecondaryMiss(cmd: Bits, outstanding: TransactionInit): Bool = {
    val (read, write) = cpuCmdToRW(cmd)
    (read && (outstanding.x_type === xactInitReadUncached || outstanding.x_type === xactInitWriteUncached)) ||
      (write && (outstanding.x_type != xactInitReadExclusive))
  }
  def needsTransactionOnCacheControl(cmd: Bits, state: UFix): Bool = {
    MuxLookup(cmd, (state === tileExclusiveDirty), Array(
      M_INV -> (state === tileExclusiveDirty),
      M_CLN -> (state === tileExclusiveDirty)
    ))
  }
  def needsWriteback (state: UFix): Bool = {
    needsTransactionOnCacheControl(M_INV, state)
  }

  def newStateOnHit(cmd: Bits, state: UFix): UFix = { 
    val (read, write) = cpuCmdToRW(cmd)
    Mux(write, tileExclusiveDirty, state)
  }
  def newStateOnCacheControl(cmd: Bits) = {
    MuxLookup(cmd, tileInvalid, Array(
      M_INV -> tileInvalid,
      M_CLN -> tileExclusiveClean
    ))
  }
  def newStateOnWriteback() = newStateOnCacheControl(M_INV)
  def newStateOnFlush() = newStateOnCacheControl(M_INV)
  def newStateOnTransactionReply(incoming: TransactionReply, outstanding: TransactionInit): UFix = {
    MuxLookup(incoming.x_type, tileInvalid, Array(
      xactReplyReadShared -> tileShared,
      xactReplyReadExclusive  -> Mux(outstanding.x_type === xactInitReadExclusive, tileExclusiveDirty, tileExclusiveClean),
      xactReplyReadExclusiveAck -> tileExclusiveDirty, 
      xactReplyReadUncached -> tileInvalid,
      xactReplyWriteUncached -> tileInvalid
    ))
  } 
  def newStateOnProbeRequest(incoming: ProbeRequest, state: UFix): Bits = {
    MuxLookup(incoming.p_type, state, Array(
      probeReqInvalidate -> tileInvalid,
      probeReqDowngrade  -> tileShared,
      probeReqCopy       -> state
    ))
  }

  def getTransactionInitTypeOnPrimaryMiss(cmd: Bits, state: UFix): UFix = {
    val (read, write) = cpuCmdToRW(cmd)
    Mux(write || cmd === M_PFW, xactInitReadExclusive, xactInitReadShared)
  }
  def getTransactionInitTypeOnSecondaryMiss(cmd: Bits, state: UFix, outstanding: TransactionInit): UFix = {
    val (read, write) = cpuCmdToRW(cmd)
    Mux(write, xactInitReadExclusive, outstanding.x_type)
  }
  def getTransactionInitTypeOnCacheControl(cmd: Bits): Bits = xactInitWriteUncached
  def getTransactionInitTypeOnWriteback(): Bits = getTransactionInitTypeOnCacheControl(M_INV)

  def newProbeReply (incoming: ProbeRequest, state: UFix): ProbeReply = {
    val reply = Wire() { new ProbeReply() }
    val with_data = MuxLookup(incoming.p_type, probeRepInvalidateData, Array(
      probeReqInvalidate -> probeRepInvalidateData,
      probeReqDowngrade  -> probeRepDowngradeData,
      probeReqCopy       -> probeRepCopyData
    ))
    val without_data = MuxLookup(incoming.p_type, probeRepInvalidateAck, Array(
      probeReqInvalidate -> probeRepInvalidateAck,
      probeReqDowngrade  -> probeRepDowngradeAck,
      probeReqCopy       -> probeRepCopyAck
    ))
    reply.p_type := Mux(needsWriteback(state), with_data, without_data)
    reply.global_xact_id := incoming.global_xact_id
    reply
  }

  def hasData (reply: ProbeReply): Bool = {
    (reply.p_type === probeRepInvalidateData ||
     reply.p_type === probeRepDowngradeData ||
     reply.p_type === probeRepCopyData)
  }
  def hasData (init: TransactionInit): Bool = {
    (init.x_type === xactInitWriteUncached)
  }
  def hasData (reply: TransactionReply): Bool = {
    (reply.x_type != xactReplyWriteUncached && reply.x_type != xactReplyReadExclusiveAck)
  }

  def isCoherenceConflict(addr1: Bits, addr2: Bits): Bool = (addr1 === addr2)

  def getTransactionReplyType(x_type: UFix, count: UFix): Bits = {
    MuxLookup(x_type, xactReplyReadUncached, Array(
      xactInitReadShared    -> Mux(count > UFix(0), xactReplyReadShared, xactReplyReadExclusive),
      xactInitReadExclusive -> xactReplyReadExclusive,
      xactInitReadUncached  -> xactReplyReadUncached,
      xactInitWriteUncached -> xactReplyWriteUncached
    ))
  }

  def getProbeRequestType(x_type: UFix, global_state: UFix): UFix = {
    MuxLookup(x_type, probeReqCopy, Array(
      xactInitReadShared -> probeReqDowngrade,
      xactInitReadExclusive -> probeReqInvalidate, 
      xactInitReadUncached -> probeReqCopy, 
      xactInitWriteUncached -> probeReqInvalidate
    ))
  }

  def needsMemRead(x_type: UFix, global_state: UFix): Bool = {
      (x_type != xactInitWriteUncached)
  }
  def needsMemWrite(x_type: UFix, global_state: UFix): Bool = {
      (x_type === xactInitWriteUncached)
  }
  def needsAckReply(x_type: UFix, global_state: UFix): Bool = {
      (x_type === xactInitWriteUncached)
  }
}
