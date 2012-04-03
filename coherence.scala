package rocket

import Chisel._
import Constants._

class TransactionInit extends Bundle {
  val t_type = Bits(width = X_INIT_TYPE_BITS)
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
  val t_type = Bits(width = X_REP_TYPE_BITS)
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
  val X_INIT_READ_SHARED    = UFix(0, 2)
  val X_INIT_READ_EXCLUSIVE = UFix(1, 2)
  val X_INIT_WRITE_UNCACHED = UFix(3, 2)
  val X_REP_READ_SHARED    = UFix(0, X_REP_TYPE_BITS)
  val X_REP_READ_EXCLUSIVE = UFix(1, X_REP_TYPE_BITS)
  val X_REP_WRITE_UNCACHED = UFix(3, X_REP_TYPE_BITS)
  val P_REP_INVALIDATE_ACK  = UFix(3, P_REP_TYPE_BITS)

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
    Mux(write || cmd === M_PFW, X_INIT_READ_EXCLUSIVE, X_INIT_READ_SHARED)
  }
  def getTransactionInitTypeOnSecondaryMiss(cmd: Bits, state: UFix, outstanding: TransactionInit): UFix = {
    val (read, write) = cpuCmdToRW(cmd)
    Mux(write, X_INIT_READ_EXCLUSIVE, outstanding.t_type)
  }
  def needsTransactionOnSecondaryMiss(cmd: Bits, outstanding: TransactionInit): Bool = Bool(false)
  def newStateOnTransactionReply(incoming: TransactionReply, outstanding: TransactionInit): UFix = {
    Mux(outstanding.t_type === X_INIT_READ_EXCLUSIVE, tileDirty, tileClean)
  } 
  def newStateOnProbeRequest(incoming: ProbeRequest, state: UFix): Bits = state
  def newProbeReply (incoming: ProbeRequest, has_data: Bool): ProbeReply = {
    val reply = Wire() { new ProbeReply() }
    reply.p_type := P_REP_INVALIDATE_ACK
    reply.global_xact_id := UFix(0)
    reply
  }
  def probeReplyHasData (reply: ProbeReply): Bool = Bool(false)
  def transactionInitHasData (init: TransactionInit): Bool = (init.t_type === X_INIT_WRITE_UNCACHED)
}

trait FourStateCoherence extends CoherencePolicy {

  val tileInvalid :: tileShared :: tileExclusiveClean :: tileExclusiveDirty :: Nil = Enum(4){ UFix() }
  val globalInvalid :: globalShared :: globalExclusiveClean :: Nil = Enum(3){ UFix() }
  val probeInvalidate :: probeDowngrade :: probeCopy :: Nil = Enum(3){ UFix() }

  val X_INIT_READ_SHARED    = UFix(0, X_INIT_TYPE_BITS)
  val X_INIT_READ_EXCLUSIVE = UFix(1, X_INIT_TYPE_BITS)
  val X_INIT_READ_UNCACHED  = UFix(2, X_INIT_TYPE_BITS)
  val X_INIT_WRITE_UNCACHED = UFix(3, X_INIT_TYPE_BITS)

  val X_REP_READ_SHARED    = UFix(0, X_REP_TYPE_BITS)
  val X_REP_READ_EXCLUSIVE = UFix(1, X_REP_TYPE_BITS)
  val X_REP_READ_UNCACHED  = UFix(2, X_REP_TYPE_BITS)
  val X_REP_WRITE_UNCACHED = UFix(3, X_REP_TYPE_BITS)
  val X_REP_READ_EXCLUSIVE_ACK = UFix(4, X_REP_TYPE_BITS)

  val P_REQ_INVALIDATE = UFix(0, P_REQ_TYPE_BITS)
  val P_REQ_DOWNGRADE  = UFix(1, P_REQ_TYPE_BITS)
  val P_REQ_COPY       = UFix(2, P_REQ_TYPE_BITS)

  val P_REP_INVALIDATE_DATA = UFix(0, P_REP_TYPE_BITS)
  val P_REP_DOWNGRADE_DATA  = UFix(1, P_REP_TYPE_BITS)
  val P_REP_COPY_DATA       = UFix(2, P_REP_TYPE_BITS)
  val P_REP_INVALIDATE_ACK  = UFix(3, P_REP_TYPE_BITS)
  val P_REP_DOWNGRADE_ACK   = UFix(4, P_REP_TYPE_BITS)
  val P_REP_COPY_ACK        = UFix(5, P_REP_TYPE_BITS)


  def isHit ( cmd: Bits, state: UFix): Bool = {
    val (read, write) = cpuCmdToRW(cmd)
    Mux(write, (state === tileExclusiveClean || state === tileExclusiveDirty),
        (state === tileShared || state === tileExclusiveClean || state === tileExclusiveDirty))
  }
  def isValid (state: UFix): Bool = {
    state != tileInvalid
  }

  def needsTransactionOnSecondaryMiss(cmd: Bits, outstanding: TransactionInit): Bool = {
    val (read, write) = cpuCmdToRW(cmd)
    (read && (outstanding.t_type === X_INIT_READ_UNCACHED || outstanding.t_type === X_INIT_WRITE_UNCACHED)) ||
      (write && (outstanding.t_type != X_INIT_READ_EXCLUSIVE))
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
    MuxLookup(incoming.t_type, tileInvalid, Array(
      X_REP_READ_SHARED -> tileShared,
      X_REP_READ_EXCLUSIVE  -> Mux(outstanding.t_type === X_INIT_READ_EXCLUSIVE, tileExclusiveDirty, tileExclusiveClean),
      X_REP_READ_EXCLUSIVE_ACK -> tileExclusiveDirty, 
      X_REP_READ_UNCACHED -> tileInvalid,
      X_REP_WRITE_UNCACHED -> tileInvalid
    ))
  } 
  def newStateOnProbeRequest(incoming: ProbeRequest, state: UFix): Bits = {
    MuxLookup(incoming.p_type, state, Array(
      probeInvalidate -> tileInvalid,
      probeDowngrade  -> tileShared,
      probeCopy       -> state
    ))
  }

  def getTransactionInitTypeOnPrimaryMiss(cmd: Bits, state: UFix): UFix = {
    val (read, write) = cpuCmdToRW(cmd)
    Mux(write || cmd === M_PFW, X_INIT_READ_EXCLUSIVE, X_INIT_READ_SHARED)
  }
  def getTransactionInitTypeOnSecondaryMiss(cmd: Bits, state: UFix, outstanding: TransactionInit): UFix = {
    val (read, write) = cpuCmdToRW(cmd)
    Mux(write, X_INIT_READ_EXCLUSIVE, outstanding.t_type)
  }
  def getTransactionInitTypeOnCacheControl(cmd: Bits): Bits = X_INIT_WRITE_UNCACHED
  def getTransactionInitTypeOnWriteback(): Bits = getTransactionInitTypeOnCacheControl(M_INV)

  def newProbeReply (incoming: ProbeRequest, state: UFix): ProbeReply = {
    val reply = Wire() { new ProbeReply() }
    val with_data = MuxLookup(incoming.p_type, P_REP_INVALIDATE_DATA, Array(
      probeInvalidate -> P_REP_INVALIDATE_DATA,
      probeDowngrade  -> P_REP_DOWNGRADE_DATA,
      probeCopy       -> P_REP_COPY_DATA
    ))
    val without_data = MuxLookup(incoming.p_type, P_REP_INVALIDATE_ACK, Array(
      probeInvalidate -> P_REP_INVALIDATE_ACK,
      probeDowngrade  -> P_REP_DOWNGRADE_ACK,
      probeCopy       -> P_REP_COPY_ACK
    ))
    reply.p_type := Mux(needsWriteback(state), with_data, without_data)
    reply.global_xact_id := incoming.global_xact_id
    reply
  }

  def hasData (reply: ProbeReply): Bool = {
    (reply.p_type === P_REP_INVALIDATE_DATA ||
     reply.p_type === P_REP_DOWNGRADE_DATA ||
     reply.p_type === P_REP_COPY_DATA)
  }
  def hasData (init: TransactionInit): Bool = {
    (init.t_type === X_INIT_WRITE_UNCACHED)
  }
  def hasData (reply: TransactionReply): Bool = {
    (reply.t_type != X_REP_WRITE_UNCACHED && reply.t_type != X_REP_READ_EXCLUSIVE_ACK)
  }

  def isCoherenceConflict(addr1: Bits, addr2: Bits): Bool = (addr1 === addr2)

  def getTransactionReplyType(t_type: UFix, count: UFix): Bits = {
    MuxLookup(t_type, X_REP_READ_UNCACHED, Array(
      X_INIT_READ_SHARED    -> Mux(count > UFix(0), X_REP_READ_SHARED, X_REP_READ_EXCLUSIVE),
      X_INIT_READ_EXCLUSIVE -> X_REP_READ_EXCLUSIVE,
      X_INIT_READ_UNCACHED  -> X_REP_READ_UNCACHED,
      X_INIT_WRITE_UNCACHED -> X_REP_WRITE_UNCACHED
    ))
  }

  def getProbeRequestType(t_type: UFix, global_state: UFix): UFix = {
    MuxLookup(t_type, P_REQ_COPY, Array(
      X_INIT_READ_SHARED -> P_REQ_DOWNGRADE,
      X_INIT_READ_EXCLUSIVE -> P_REQ_INVALIDATE, 
      X_INIT_READ_UNCACHED -> P_REQ_COPY, 
      X_INIT_WRITE_UNCACHED -> P_REQ_INVALIDATE
    ))
  }

  def needsMemRead(t_type: UFix, global_state: UFix): Bool = {
      (t_type != X_INIT_WRITE_UNCACHED)
  }
  def needsMemWrite(t_type: UFix, global_state: UFix): Bool = {
      (t_type === X_INIT_WRITE_UNCACHED)
  }
  def needsAckReply(t_type: UFix, global_state: UFix): Bool = {
      (t_type === X_INIT_WRITE_UNCACHED)
  }
}
