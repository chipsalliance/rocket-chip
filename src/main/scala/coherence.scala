package rocket

import Chisel._
import Constants._

class TransactionInit extends Bundle {
  val x_type = Bits(width = X_INIT_TYPE_BITS)
  val tile_xact_id = Bits(width = TILE_XACT_ID_BITS)
  val address = UFix(width = PADDR_BITS - OFFSET_BITS)
}

class TransactionInitData extends MemData

class TransactionAbort extends Bundle {
  val tile_xact_id = Bits(width = TILE_XACT_ID_BITS)
}

class ProbeRequest extends Bundle {
  val p_type = Bits(width = P_REQ_TYPE_BITS)
  val global_xact_id = Bits(width = GLOBAL_XACT_ID_BITS)
  val address = Bits(width = PADDR_BITS - OFFSET_BITS)
}

class ProbeReply extends Bundle {
  val p_type = Bits(width = P_REP_TYPE_BITS)
  val global_xact_id = Bits(width = GLOBAL_XACT_ID_BITS)
}

class ProbeReplyData extends MemData

class TransactionReply extends MemData {
  val x_type = Bits(width = X_REP_TYPE_BITS)
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

trait CoherencePolicy { }

trait ThreeStateIncoherence extends CoherencePolicy {
  val tileInvalid :: tileClean :: tileDirty :: Nil = Enum(3){ UFix() }
  val xactInitReadShared    = UFix(0, 2)
  val xactInitReadExclusive = UFix(1, 2)
  val xactInitWriteUncached = UFix(3, 2)
  val xactReplyReadShared    = UFix(0, X_REP_TYPE_BITS)
  val xactReplyReadExclusive = UFix(1, X_REP_TYPE_BITS)
  val xactReplyWriteUncached = UFix(3, X_REP_TYPE_BITS)
  val probeRepInvalidateAck  = UFix(3, P_REP_TYPE_BITS)

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
  def newStateOnCacheControl(cmd: Bits) = tileInvalid
  def newState(cmd: Bits, state: UFix): UFix = {
    val (read, write) = cpuCmdToRW(cmd)
    Mux(write, tileDirty, Mux(read, Mux(state === tileDirty, tileDirty, tileClean), state))
  }
  def newStateOnHit(cmd: Bits, state: UFix): UFix = newState(cmd, state)
  def getTransactionInitTypeOnPrimaryMiss(cmd: Bits, state: UFix): UFix = {
    val (read, write) = cpuCmdToRW(cmd)
    Mux(write || cmd === M_PFW, xactInitReadExclusive, xactInitReadShared)
  }
  def getTransactionInitTypeOnSecondaryMiss(cmd: Bits, state: UFix, outstanding: TransactionInit): UFix = {
    val (read, write) = cpuCmdToRW(cmd)
    Mux(write, xactInitReadExclusive, outstanding.x_type)
  }
  def needsTransactionOnSecondaryMiss(cmd: Bits, outstanding: TransactionInit): Bool = Bool(false)
  def newStateOnTransactionReply(incoming: TransactionReply, outstanding: TransactionInit): UFix = {
    Mux(outstanding.x_type === xactInitReadExclusive, tileDirty, tileClean)
  } 
  def newStateOnProbeRequest(incoming: ProbeRequest, state: UFix): Bits = state
  def newProbeReply (incoming: ProbeRequest, has_data: Bool): ProbeReply = {
    val reply = Wire() { new ProbeReply() }
    reply.p_type := probeRepInvalidateAck
    reply.global_xact_id := UFix(0)
    reply
  }
  def probeReplyHasData (reply: ProbeReply): Bool = Bool(false)
  def transactionInitHasData (init: TransactionInit): Bool = (init.x_type === xactInitWriteUncached)
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
