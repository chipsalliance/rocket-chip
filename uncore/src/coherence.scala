package uncore

import Chisel._
import Constants._

trait CoherenceAgentRole
trait ClientCoherenceAgent extends CoherenceAgentRole
trait MasterCoherenceAgent extends CoherenceAgentRole

object cpuCmdToRW {
  def apply(cmd: Bits): (Bool, Bool) = (isRead(cmd) || isPrefetch(cmd), isWrite(cmd))
}

abstract class CoherencePolicy {
  def isHit (cmd: Bits, state: UFix): Bool
  def isValid (state: UFix): Bool

  def needsTransactionOnSecondaryMiss(cmd: Bits, outstanding: Acquire): Bool
  def needsTransactionOnCacheControl(cmd: Bits, state: UFix): Bool
  def needsWriteback (state: UFix): Bool

  def newStateOnHit(cmd: Bits, state: UFix): UFix
  def newStateOnCacheControl(cmd: Bits): UFix
  def newStateOnWriteback(): UFix
  def newStateOnFlush(): UFix
  def newStateOnGrant(incoming: Grant, outstanding: Acquire): UFix
  def newStateOnProbe(incoming: Probe, state: UFix): Bits

  def getAcquireTypeOnPrimaryMiss(cmd: Bits, state: UFix): UFix
  def getAcquireTypeOnSecondaryMiss(cmd: Bits, state: UFix, outstanding: Acquire): UFix
  def getReleaseTypeOnCacheControl(cmd: Bits): Bits
  def getReleaseTypeOnVoluntaryWriteback(): Bits

  def getVoluntaryWriteback(addr: UFix, client_id: UFix, master_id: UFix): Release
  def newRelease (incoming: Probe, state: UFix, id: UFix): Release

  def messageHasData (rel: SourcedMessage): Bool
  def messageUpdatesDataArray (reply: Grant): Bool
  def messageIsUncached(acq: Acquire): Bool

  def isCoherenceConflict(addr1: Bits, addr2: Bits): Bool
  def isVoluntary(rel: Release): Bool
  def isVoluntary(gnt: Grant): Bool
  def getGrantType(a_type: UFix, count: UFix): Bits
  def getGrantType(rel: Release, count: UFix): Bits
  def getProbeType(a_type: UFix, global_state: UFix): UFix
  def needsOuterRead(a_type: UFix, global_state: UFix): Bool
  def needsOuterWrite(a_type: UFix, global_state: UFix): Bool
  def needsAckReply(a_type: UFix, global_state: UFix): Bool
  def needsSelfProbe(acq: Acquire): Bool
  def requiresAck(grant: Grant): Bool
  def requiresAck(release: Release): Bool
  def pendingVoluntaryReleaseIsSufficient(r_type: UFix, p_type: UFix): Bool

  def uFixListContains(list: List[UFix], elem: UFix): Bool = list.map(elem === _).reduceLeft(_||_)
}

trait UncachedTransactions {
  def getUncachedReadAcquire(addr: UFix, id: UFix): Acquire
  def getUncachedWriteAcquire(addr: UFix, id: UFix): Acquire
  def getUncachedReadWordAcquire(addr: UFix, id: UFix): Acquire
  def getUncachedWriteWordAcquire(addr: UFix, id: UFix, write_mask: Bits): Acquire
  def getUncachedAtomicAcquire(addr: UFix, id: UFix, subword_addr: UFix, atomic_op: UFix): Acquire
  def isUncachedReadTransaction(acq: Acquire): Bool
}

abstract class CoherencePolicyWithUncached extends CoherencePolicy with UncachedTransactions

abstract class IncoherentPolicy extends CoherencePolicy {
  // UNIMPLEMENTED
  def newStateOnProbe(incoming: Probe, state: UFix): Bits = state
  def newRelease (incoming: Probe, state: UFix, id: UFix): Release = Release( UFix(0), UFix(0), UFix(0), UFix(0))
  def isCoherenceConflict(addr1: Bits, addr2: Bits): Bool = Bool(false)
  def getGrantType(a_type: UFix, count: UFix): Bits = Bits(0)
  def getGrantType(rel: Release, count: UFix): Bits = Bits(0)
  def getProbeType(a_type: UFix, global_state: UFix): UFix = UFix(0)
  def needsOuterRead(a_type: UFix, global_state: UFix): Bool = Bool(false)
  def needsOuterWrite(a_type: UFix, global_state: UFix): Bool = Bool(false)
  def needsAckReply(a_type: UFix, global_state: UFix): Bool = Bool(false)
  def needsSelfProbe(acq: Acquire) = Bool(false)
  def requiresAck(grant: Grant) = Bool(true)
  def requiresAck(release: Release) = Bool(false)
  def pendingVoluntaryReleaseIsSufficient(r_type: UFix, p_type: UFix): Bool = Bool(false)

}

class ThreeStateIncoherence extends IncoherentPolicy {
  val tileInvalid :: tileClean :: tileDirty :: Nil = Enum(3){ UFix() }
  val acquireReadClean :: acquireReadDirty :: acquireWriteback :: Nil = Enum(3){ UFix() }
  val grantVoluntaryAck :: grantData :: grantAck :: Nil = Enum(3){ UFix() }
  val releaseVoluntaryInvalidateData :: releaseInvalidateAck :: Nil = Enum(2){ UFix() }
  val uncachedAcquireTypeList = List() 
  val hasDataAcquireTypeList = List(acquireWriteback)
  val hasDataReleaseTypeList = List(acquireWriteback)
  val hasDataGrantTypeList = List(grantData)

  def isHit ( cmd: Bits, state: UFix): Bool = (state === tileClean || state === tileDirty)
  def isValid (state: UFix): Bool = state != tileInvalid

  def needsTransactionOnSecondaryMiss(cmd: Bits, outstanding: Acquire) = Bool(false)
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
  def newStateOnGrant(incoming: Grant, outstanding: Acquire) = {
    MuxLookup(incoming.g_type, tileInvalid, Array(
      grantData -> Mux(outstanding.a_type === acquireReadDirty, tileDirty, tileClean),
      grantAck  -> tileInvalid
    ))
  }

  def getVoluntaryWriteback(addr: UFix, client_id: UFix, master_id: UFix) = Release(releaseVoluntaryInvalidateData, addr, client_id, master_id)
  def isVoluntary(rel: Release) = rel.r_type === releaseVoluntaryInvalidateData
  def isVoluntary(gnt: Grant) = gnt.g_type === grantVoluntaryAck

  def getAcquireTypeOnPrimaryMiss(cmd: Bits, state: UFix): UFix = {
    val (read, write) = cpuCmdToRW(cmd)
    Mux(write || cmd === M_PFW, acquireReadDirty, acquireReadClean)
  }
  def getAcquireTypeOnSecondaryMiss(cmd: Bits, state: UFix, outstanding: Acquire): UFix = {
    val (read, write) = cpuCmdToRW(cmd)
    Mux(write, acquireReadDirty, outstanding.a_type)
  }
  def getReleaseTypeOnCacheControl(cmd: Bits): Bits = releaseVoluntaryInvalidateData // TODO
  def getReleaseTypeOnVoluntaryWriteback(): Bits = releaseVoluntaryInvalidateData

  def messageHasData( msg: SourcedMessage ) = msg match {
    case acq: Acquire => uFixListContains(hasDataAcquireTypeList, acq.a_type)
    case grant: Grant => uFixListContains(hasDataGrantTypeList, grant.g_type) 
    case rel: Release => Bool(false)
    case _ => Bool(false)
  }
  def messageUpdatesDataArray (reply: Grant) = (reply.g_type === grantData)
  def messageIsUncached(acq: Acquire): Bool = uFixListContains(uncachedAcquireTypeList, acq.a_type)
}

class MICoherence extends CoherencePolicyWithUncached {

  val tileInvalid :: tileValid :: Nil = Enum(2){ UFix() }
  val globalInvalid :: globalValid :: Nil = Enum(2){ UFix() }

  val acquireReadExclusive :: acquireReadUncached :: acquireWriteUncached :: acquireReadWordUncached :: acquireWriteWordUncached :: acquireAtomicUncached :: Nil = Enum(6){ UFix() }
  val grantVoluntaryAck :: grantReadExclusive :: grantReadUncached :: grantWriteUncached :: grantReadWordUncached :: grantWriteWordUncached :: grantAtomicUncached :: Nil = Enum(7){ UFix() }
  val probeInvalidate :: probeCopy :: Nil = Enum(2){ UFix() }
  val releaseVoluntaryInvalidateData :: releaseInvalidateData :: releaseCopyData :: releaseInvalidateAck :: releaseCopyAck :: Nil = Enum(5){ UFix() }
  val uncachedAcquireTypeList = List(acquireReadUncached, acquireWriteUncached, acquireReadWordUncached, acquireWriteWordUncached, acquireAtomicUncached) 
  val hasDataAcquireTypeList = List(acquireWriteUncached, acquireWriteWordUncached, acquireAtomicUncached)
  val hasDataReleaseTypeList = List(releaseVoluntaryInvalidateData, releaseInvalidateData, releaseCopyData)
  val hasDataGrantTypeList = List(grantReadExclusive, grantReadUncached, grantReadWordUncached, grantAtomicUncached)

  def isHit (cmd: Bits, state: UFix): Bool = state != tileInvalid
  def isValid (state: UFix): Bool = state != tileInvalid

  def needsTransactionOnSecondaryMiss(cmd: Bits, outstanding: Acquire): Bool = (outstanding.a_type != acquireReadExclusive)
  def needsTransactionOnCacheControl(cmd: Bits, state: UFix): Bool = {
    MuxLookup(cmd, (state === tileValid), Array(
      M_INV -> (state === tileValid),
      M_CLN -> (state === tileValid)
    ))
  }
  def needsWriteback (state: UFix): Bool = {
    needsTransactionOnCacheControl(M_INV, state)
  }

  def newStateOnHit(cmd: Bits, state: UFix): UFix = state
  def newStateOnCacheControl(cmd: Bits) = {
    MuxLookup(cmd, tileInvalid, Array(
      M_INV -> tileInvalid,
      M_CLN -> tileValid
    ))
  }
  def newStateOnWriteback() = newStateOnCacheControl(M_INV)
  def newStateOnFlush() = newStateOnCacheControl(M_INV)
  def newStateOnGrant(incoming: Grant, outstanding: Acquire): UFix = {
    MuxLookup(incoming.g_type, tileInvalid, Array(
      grantReadExclusive -> tileValid,
      grantReadUncached  -> tileInvalid,
      grantWriteUncached -> tileInvalid,
      grantReadWordUncached -> tileInvalid,
      grantWriteWordUncached -> tileInvalid,
      grantAtomicUncached -> tileInvalid
    ))
  } 
  def newStateOnProbe(incoming: Probe, state: UFix): Bits = {
    MuxLookup(incoming.p_type, state, Array(
      probeInvalidate -> tileInvalid,
      probeCopy       -> state
    ))
  }

  def getUncachedReadAcquire(addr: UFix, id: UFix) = Acquire(acquireReadUncached, addr, id)
  def getUncachedWriteAcquire(addr: UFix, id: UFix) = Acquire(acquireWriteUncached, addr, id)
  def getUncachedReadWordAcquire(addr: UFix, id: UFix) = Acquire(acquireReadWordUncached, addr, id)
  def getUncachedWriteWordAcquire(addr: UFix, id: UFix, write_mask: Bits) = Acquire(acquireWriteWordUncached, addr, id, write_mask)
  def getUncachedAtomicAcquire(addr: UFix, id: UFix, subword_addr: UFix, atomic_op: UFix) = Acquire(acquireAtomicUncached, addr, id, subword_addr, atomic_op)
  def getVoluntaryWriteback(addr: UFix, client_id: UFix, master_id: UFix) = Release(releaseVoluntaryInvalidateData, addr, client_id, master_id)
  def isUncachedReadTransaction(acq: Acquire) = acq.a_type === acquireReadUncached
  def isVoluntary(rel: Release) = rel.r_type === releaseVoluntaryInvalidateData
  def isVoluntary(gnt: Grant) = gnt.g_type === grantVoluntaryAck

  def getAcquireTypeOnPrimaryMiss(cmd: Bits, state: UFix): UFix = acquireReadExclusive
  def getAcquireTypeOnSecondaryMiss(cmd: Bits, state: UFix, outstanding: Acquire): UFix = acquireReadExclusive
  def getReleaseTypeOnCacheControl(cmd: Bits): Bits = releaseVoluntaryInvalidateData // TODO
  def getReleaseTypeOnVoluntaryWriteback(): Bits = getReleaseTypeOnCacheControl(M_INV)

  def newRelease (incoming: Probe, state: UFix, id: UFix): Release = {
    val with_data = MuxLookup(incoming.p_type, releaseInvalidateData, Array(
      probeInvalidate -> releaseInvalidateData,
      probeCopy       -> releaseCopyData
    ))
    val without_data = MuxLookup(incoming.p_type, releaseInvalidateAck, Array(
      probeInvalidate -> releaseInvalidateAck,
      probeCopy       -> releaseCopyAck
    ))
    Release( Mux(needsWriteback(state), with_data, without_data), incoming.addr, id, incoming.master_xact_id)
  }

  def messageHasData(msg: SourcedMessage) = msg match {
    case acq: Acquire => uFixListContains(hasDataAcquireTypeList, acq.a_type)
    case grant: Grant => uFixListContains(hasDataGrantTypeList, grant.g_type) 
    case rel: Release => uFixListContains(hasDataReleaseTypeList, rel.r_type) 
    case _ => Bool(false)
  }
  def messageUpdatesDataArray (reply: Grant): Bool = {
    (reply.g_type === grantReadExclusive)
  }
  def messageIsUncached(acq: Acquire): Bool = uFixListContains(uncachedAcquireTypeList, acq.a_type)

  def isCoherenceConflict(addr1: Bits, addr2: Bits): Bool = (addr1 === addr2)

  def getGrantType(a_type: UFix, count: UFix): Bits = {
    MuxLookup(a_type, grantReadUncached, Array(
      acquireReadExclusive -> grantReadExclusive,
      acquireReadUncached  -> grantReadUncached,
      acquireWriteUncached -> grantWriteUncached,
      acquireReadWordUncached  -> grantReadWordUncached,
      acquireWriteWordUncached -> grantWriteWordUncached,
      acquireAtomicUncached -> grantAtomicUncached
    ))
  }

  def getGrantType(rel: Release, count: UFix): Bits = {
    MuxLookup(rel.r_type, grantReadUncached, Array(
      releaseVoluntaryInvalidateData -> grantVoluntaryAck
    ))
  }

  def getProbeType(a_type: UFix, global_state: UFix): UFix = {
    MuxLookup(a_type, probeCopy, Array(
      acquireReadExclusive -> probeInvalidate, 
      acquireReadUncached -> probeCopy, 
      acquireWriteUncached -> probeInvalidate,
      acquireReadWordUncached -> probeCopy, 
      acquireWriteWordUncached -> probeInvalidate,
      acquireAtomicUncached -> probeInvalidate
    ))
  }

  def needsOuterRead(a_type: UFix, global_state: UFix): Bool = {
      (a_type != acquireWriteUncached)
  }
  def needsOuterWrite(a_type: UFix, global_state: UFix): Bool = {
      (a_type === acquireWriteUncached)
  }
  def needsAckReply(a_type: UFix, global_state: UFix): Bool = {
      (a_type === acquireWriteUncached)
  }
  def requiresAck(grant: Grant) = Bool(true)
  def requiresAck(release: Release) = Bool(false)
  def needsSelfProbe(acq: Acquire) = acq.a_type === acquireReadUncached
  def pendingVoluntaryReleaseIsSufficient(r_type: UFix, p_type: UFix): Bool = (r_type === releaseVoluntaryInvalidateData)
}

class MEICoherence extends CoherencePolicyWithUncached {

  val tileInvalid :: tileExclusiveClean :: tileExclusiveDirty :: Nil = Enum(3){ UFix() }
  val globalInvalid :: globalExclusiveClean :: Nil = Enum(2){ UFix() }

  val acquireReadExclusiveClean :: acquireReadExclusiveDirty :: acquireReadUncached :: acquireWriteUncached :: acquireReadWordUncached :: acquireWriteWordUncached :: acquireAtomicUncached :: Nil = Enum(7){ UFix() }
  val grantVoluntaryAck :: grantReadExclusive :: grantReadUncached :: grantWriteUncached :: grantReadExclusiveAck :: grantReadWordUncached :: grantWriteWordUncached :: grantAtomicUncached :: Nil = Enum(8){ UFix() }
  val probeInvalidate :: probeDowngrade :: probeCopy :: Nil = Enum(3){ UFix() }
  val releaseVoluntaryInvalidateData :: releaseInvalidateData :: releaseDowngradeData :: releaseCopyData :: releaseInvalidateAck :: releaseDowngradeAck :: releaseCopyAck :: Nil = Enum(7){ UFix() }

  val uncachedAcquireTypeList = List(acquireReadUncached, acquireWriteUncached, acquireReadWordUncached, acquireWriteWordUncached, acquireAtomicUncached) 
  val hasDataAcquireTypeList = List(acquireWriteUncached, acquireWriteWordUncached, acquireAtomicUncached) 
  val hasDataReleaseTypeList = List(releaseVoluntaryInvalidateData, releaseInvalidateData, releaseDowngradeData, releaseCopyData)
  val hasDataGrantTypeList = List(grantReadExclusive, grantReadUncached, grantReadWordUncached, grantAtomicUncached)

  def isHit (cmd: Bits, state: UFix): Bool = state != tileInvalid
  def isValid (state: UFix): Bool = state != tileInvalid

  def needsTransactionOnSecondaryMiss(cmd: Bits, outstanding: Acquire): Bool = {
    val (read, write) = cpuCmdToRW(cmd)
    (read && messageIsUncached(outstanding)) ||
      (write && (outstanding.a_type != acquireReadExclusiveDirty))
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
  def newStateOnGrant(incoming: Grant, outstanding: Acquire): UFix = {
    MuxLookup(incoming.g_type, tileInvalid, Array(
      grantReadExclusive  -> Mux(outstanding.a_type === acquireReadExclusiveDirty, tileExclusiveDirty, tileExclusiveClean),
      grantReadExclusiveAck -> tileExclusiveDirty, 
      grantReadUncached -> tileInvalid,
      grantWriteUncached -> tileInvalid,
      grantReadWordUncached -> tileInvalid,
      grantWriteWordUncached -> tileInvalid,
      grantAtomicUncached -> tileInvalid
    ))
  } 
  def newStateOnProbe(incoming: Probe, state: UFix): Bits = {
    MuxLookup(incoming.p_type, state, Array(
      probeInvalidate -> tileInvalid,
      probeDowngrade  -> tileExclusiveClean,
      probeCopy       -> state
    ))
  }

  def getUncachedReadAcquire(addr: UFix, id: UFix) = Acquire(acquireReadUncached, addr, id)
  def getUncachedWriteAcquire(addr: UFix, id: UFix) = Acquire(acquireWriteUncached, addr, id)
  def getUncachedReadWordAcquire(addr: UFix, id: UFix) = Acquire(acquireReadWordUncached, addr, id)
  def getUncachedWriteWordAcquire(addr: UFix, id: UFix, write_mask: Bits) = Acquire(acquireWriteWordUncached, addr, id, write_mask)
  def getUncachedAtomicAcquire(addr: UFix, id: UFix, subword_addr: UFix, atomic_op: UFix) = Acquire(acquireAtomicUncached, addr, id, subword_addr, atomic_op)
  def getVoluntaryWriteback(addr: UFix, client_id: UFix, master_id: UFix) = Release(releaseVoluntaryInvalidateData, addr, client_id, master_id)
  def isUncachedReadTransaction(acq: Acquire) = acq.a_type === acquireReadUncached
  def isVoluntary(rel: Release) = rel.r_type === releaseVoluntaryInvalidateData
  def isVoluntary(gnt: Grant) = gnt.g_type === grantVoluntaryAck

  def getAcquireTypeOnPrimaryMiss(cmd: Bits, state: UFix): UFix = {
    val (read, write) = cpuCmdToRW(cmd)
    Mux(write, acquireReadExclusiveDirty, acquireReadExclusiveClean)
  }
  def getAcquireTypeOnSecondaryMiss(cmd: Bits, state: UFix, outstanding: Acquire): UFix = {
    val (read, write) = cpuCmdToRW(cmd)
    Mux(write, acquireReadExclusiveDirty, outstanding.a_type)
  }
  def getReleaseTypeOnCacheControl(cmd: Bits): Bits = releaseVoluntaryInvalidateData // TODO
  def getReleaseTypeOnVoluntaryWriteback(): Bits = getReleaseTypeOnCacheControl(M_INV)

  def newRelease (incoming: Probe, state: UFix, id: UFix): Release = {
    val with_data = MuxLookup(incoming.p_type, releaseInvalidateData, Array(
      probeInvalidate -> releaseInvalidateData,
      probeDowngrade  -> releaseDowngradeData,
      probeCopy       -> releaseCopyData
    ))
    val without_data = MuxLookup(incoming.p_type, releaseInvalidateAck, Array(
      probeInvalidate -> releaseInvalidateAck,
      probeDowngrade  -> releaseDowngradeAck,
      probeCopy       -> releaseCopyAck
    ))
    Release( Mux(needsWriteback(state), with_data, without_data), incoming.addr, id, incoming.master_xact_id)
  }

  def messageHasData(msg: SourcedMessage) = msg match {
    case acq: Acquire => uFixListContains(hasDataAcquireTypeList, acq.a_type)
    case grant: Grant => uFixListContains(hasDataGrantTypeList, grant.g_type) 
    case rel: Release => uFixListContains(hasDataReleaseTypeList, rel.r_type) 
    case _ => Bool(false)
  }
  def messageUpdatesDataArray (reply: Grant): Bool = {
    (reply.g_type === grantReadExclusive)
  }
  def messageIsUncached(acq: Acquire): Bool = uFixListContains(uncachedAcquireTypeList, acq.a_type)

  def isCoherenceConflict(addr1: Bits, addr2: Bits): Bool = (addr1 === addr2)

  def getGrantType(a_type: UFix, count: UFix): Bits = {
    MuxLookup(a_type, grantReadUncached, Array(
      acquireReadExclusiveClean -> grantReadExclusive,
      acquireReadExclusiveDirty -> grantReadExclusive,
      acquireReadUncached  -> grantReadUncached,
      acquireWriteUncached -> grantWriteUncached,
      acquireReadWordUncached  -> grantReadWordUncached,
      acquireWriteWordUncached -> grantWriteWordUncached,
      acquireAtomicUncached -> grantAtomicUncached
    ))
  }
  def getGrantType(rel: Release, count: UFix): Bits = {
    MuxLookup(rel.r_type, grantReadUncached, Array(
      releaseVoluntaryInvalidateData -> grantVoluntaryAck
    ))
  }


  def getProbeType(a_type: UFix, global_state: UFix): UFix = {
    MuxLookup(a_type, probeCopy, Array(
      acquireReadExclusiveClean -> probeInvalidate,
      acquireReadExclusiveDirty -> probeInvalidate, 
      acquireReadUncached -> probeCopy, 
      acquireWriteUncached -> probeInvalidate,
      acquireReadWordUncached -> probeCopy, 
      acquireWriteWordUncached -> probeInvalidate,
      acquireAtomicUncached -> probeInvalidate
    ))
  }

  def needsOuterRead(a_type: UFix, global_state: UFix): Bool = {
      (a_type != acquireWriteUncached)
  }
  def needsOuterWrite(a_type: UFix, global_state: UFix): Bool = {
      (a_type === acquireWriteUncached)
  }
  def needsAckReply(a_type: UFix, global_state: UFix): Bool = {
      (a_type === acquireWriteUncached)
  }
  def requiresAck(grant: Grant) = Bool(true)
  def requiresAck(release: Release) = Bool(false)
  def needsSelfProbe(acq: Acquire) = acq.a_type === acquireReadUncached

  def pendingVoluntaryReleaseIsSufficient(r_type: UFix, p_type: UFix): Bool = (r_type === releaseVoluntaryInvalidateData)
}

class MSICoherence extends CoherencePolicyWithUncached {

  val tileInvalid :: tileShared :: tileExclusiveDirty :: Nil = Enum(3){ UFix() }
  val globalInvalid :: globalShared :: globalExclusive :: Nil = Enum(3){ UFix() }

  val acquireReadShared :: acquireReadExclusive :: acquireReadUncached :: acquireWriteUncached :: acquireReadWordUncached :: acquireWriteWordUncached :: acquireAtomicUncached :: Nil = Enum(7){ UFix() }
  val grantVoluntaryAck :: grantReadShared :: grantReadExclusive :: grantReadUncached :: grantWriteUncached :: grantReadExclusiveAck :: grantReadWordUncached :: grantWriteWordUncached :: grantAtomicUncached :: Nil = Enum(9){ UFix() }
  val probeInvalidate :: probeDowngrade :: probeCopy :: Nil = Enum(3){ UFix() }
  val releaseVoluntaryInvalidateData :: releaseInvalidateData :: releaseDowngradeData :: releaseCopyData :: releaseInvalidateAck :: releaseDowngradeAck :: releaseCopyAck :: Nil = Enum(7){ UFix() }
  val uncachedAcquireTypeList = List(acquireReadUncached, acquireWriteUncached, acquireReadWordUncached, acquireWriteWordUncached, acquireAtomicUncached) 
  val hasDataAcquireTypeList = List(acquireWriteUncached, acquireWriteWordUncached, acquireAtomicUncached)
  val hasDataReleaseTypeList = List(releaseVoluntaryInvalidateData, releaseInvalidateData, releaseDowngradeData, releaseCopyData)
  val hasDataGrantTypeList = List(grantReadShared, grantReadExclusive, grantReadUncached, grantReadWordUncached, grantAtomicUncached)

  def isHit (cmd: Bits, state: UFix): Bool = {
    val (read, write) = cpuCmdToRW(cmd)
    Mux(write, (state === tileExclusiveDirty),
        (state === tileShared || state === tileExclusiveDirty))
  }
  def isValid (state: UFix): Bool = {
    state != tileInvalid
  }

  def needsTransactionOnSecondaryMiss(cmd: Bits, outstanding: Acquire): Bool = {
    val (read, write) = cpuCmdToRW(cmd)
    (read && messageIsUncached(outstanding)) || 
      (write && (outstanding.a_type != acquireReadExclusive))
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
      M_CLN -> tileShared
    ))
  }
  def newStateOnWriteback() = newStateOnCacheControl(M_INV)
  def newStateOnFlush() = newStateOnCacheControl(M_INV)
  def newStateOnGrant(incoming: Grant, outstanding: Acquire): UFix = {
    MuxLookup(incoming.g_type, tileInvalid, Array(
      grantReadShared -> tileShared,
      grantReadExclusive  -> tileExclusiveDirty,
      grantReadExclusiveAck -> tileExclusiveDirty, 
      grantReadUncached -> tileInvalid,
      grantWriteUncached -> tileInvalid,
      grantReadWordUncached -> tileInvalid,
      grantWriteWordUncached -> tileInvalid,
      grantAtomicUncached -> tileInvalid
    ))
  } 
  def newStateOnProbe(incoming: Probe, state: UFix): Bits = {
    MuxLookup(incoming.p_type, state, Array(
      probeInvalidate -> tileInvalid,
      probeDowngrade  -> tileShared,
      probeCopy       -> state
    ))
  }

  def getUncachedReadAcquire(addr: UFix, id: UFix) = Acquire(acquireReadUncached, addr, id)
  def getUncachedWriteAcquire(addr: UFix, id: UFix) = Acquire(acquireWriteUncached, addr, id)
  def getUncachedReadWordAcquire(addr: UFix, id: UFix) = Acquire(acquireReadWordUncached, addr, id)
  def getUncachedWriteWordAcquire(addr: UFix, id: UFix, write_mask: Bits) = Acquire(acquireWriteWordUncached, addr, id, write_mask)
  def getUncachedAtomicAcquire(addr: UFix, id: UFix, subword_addr: UFix, atomic_op: UFix) = Acquire(acquireAtomicUncached, addr, id, subword_addr, atomic_op)
  def getVoluntaryWriteback(addr: UFix, client_id: UFix, master_id: UFix) = Release(releaseVoluntaryInvalidateData, addr, client_id, master_id)
  def isUncachedReadTransaction(acq: Acquire) = acq.a_type === acquireReadUncached
  def isVoluntary(rel: Release) = rel.r_type === releaseVoluntaryInvalidateData
  def isVoluntary(gnt: Grant) = gnt.g_type === grantVoluntaryAck

  def getAcquireTypeOnPrimaryMiss(cmd: Bits, state: UFix): UFix = {
    val (read, write) = cpuCmdToRW(cmd)
    Mux(write || cmd === M_PFW, acquireReadExclusive, acquireReadShared)
  }
  def getAcquireTypeOnSecondaryMiss(cmd: Bits, state: UFix, outstanding: Acquire): UFix = {
    val (read, write) = cpuCmdToRW(cmd)
    Mux(write, acquireReadExclusive, outstanding.a_type)
  }
  def getReleaseTypeOnCacheControl(cmd: Bits): Bits = releaseVoluntaryInvalidateData // TODO
  def getReleaseTypeOnVoluntaryWriteback(): Bits = getReleaseTypeOnCacheControl(M_INV)

  def newRelease (incoming: Probe, state: UFix, id: UFix): Release = {
    val with_data = MuxLookup(incoming.p_type, releaseInvalidateData, Array(
      probeInvalidate -> releaseInvalidateData,
      probeDowngrade  -> releaseDowngradeData,
      probeCopy       -> releaseCopyData
    ))
    val without_data = MuxLookup(incoming.p_type, releaseInvalidateAck, Array(
      probeInvalidate -> releaseInvalidateAck,
      probeDowngrade  -> releaseDowngradeAck,
      probeCopy       -> releaseCopyAck
    ))
    Release( Mux(needsWriteback(state), with_data, without_data), incoming.addr, id, incoming.master_xact_id)
  }

  def messageHasData(msg: SourcedMessage) = msg match {
    case acq: Acquire => uFixListContains(hasDataAcquireTypeList, acq.a_type)
    case grant: Grant => uFixListContains(hasDataGrantTypeList, grant.g_type) 
    case rel: Release => uFixListContains(hasDataReleaseTypeList, rel.r_type) 
    case _ => Bool(false)
  }
  def messageUpdatesDataArray (reply: Grant): Bool = {
    (reply.g_type === grantReadShared || reply.g_type === grantReadExclusive)
  }
  def messageIsUncached(acq: Acquire): Bool = uFixListContains(uncachedAcquireTypeList, acq.a_type)

  def isCoherenceConflict(addr1: Bits, addr2: Bits): Bool = (addr1 === addr2)

  def getGrantType(a_type: UFix, count: UFix): Bits = {
    MuxLookup(a_type, grantReadUncached, Array(
      acquireReadShared    -> Mux(count > UFix(0), grantReadShared, grantReadExclusive),
      acquireReadExclusive -> grantReadExclusive,
      acquireReadUncached  -> grantReadUncached,
      acquireWriteUncached -> grantWriteUncached,
      acquireReadWordUncached  -> grantReadWordUncached,
      acquireWriteWordUncached -> grantWriteWordUncached,
      acquireAtomicUncached -> grantAtomicUncached
    ))
  }
  def getGrantType(rel: Release, count: UFix): Bits = {
    MuxLookup(rel.r_type, grantReadUncached, Array(
      releaseVoluntaryInvalidateData -> grantVoluntaryAck
    ))
  }


  def getProbeType(a_type: UFix, global_state: UFix): UFix = {
    MuxLookup(a_type, probeCopy, Array(
      acquireReadShared -> probeDowngrade,
      acquireReadExclusive -> probeInvalidate, 
      acquireReadUncached -> probeCopy, 
      acquireWriteUncached -> probeInvalidate
    ))
  }

  def needsOuterRead(a_type: UFix, global_state: UFix): Bool = {
      (a_type != acquireWriteUncached)
  }
  def needsOuterWrite(a_type: UFix, global_state: UFix): Bool = {
      (a_type === acquireWriteUncached)
  }
  def needsAckReply(a_type: UFix, global_state: UFix): Bool = {
      (a_type === acquireWriteUncached)
  }
  def requiresAck(grant: Grant) = Bool(true)
  def requiresAck(release: Release) = Bool(false)
  def needsSelfProbe(acq: Acquire) = acq.a_type === acquireReadUncached

  def pendingVoluntaryReleaseIsSufficient(r_type: UFix, p_type: UFix): Bool = (r_type === releaseVoluntaryInvalidateData)
}

class MESICoherence extends CoherencePolicyWithUncached {

  val tileInvalid :: tileShared :: tileExclusiveClean :: tileExclusiveDirty :: Nil = Enum(4){ UFix() }
  val globalInvalid :: globalShared :: globalExclusiveClean :: Nil = Enum(3){ UFix() }

  val acquireReadShared :: acquireReadExclusive :: acquireReadUncached :: acquireWriteUncached :: acquireReadWordUncached :: acquireWriteWordUncached :: acquireAtomicUncached :: Nil = Enum(7){ UFix() }
  val grantVoluntaryAck :: grantReadShared :: grantReadExclusive :: grantReadUncached :: grantWriteUncached :: grantReadExclusiveAck :: grantReadWordUncached :: grantWriteWordUncached :: grantAtomicUncached :: Nil = Enum(9){ UFix() }
  val probeInvalidate :: probeDowngrade :: probeCopy :: Nil = Enum(3){ UFix() }
  val releaseVoluntaryInvalidateData :: releaseInvalidateData :: releaseDowngradeData :: releaseCopyData :: releaseInvalidateAck :: releaseDowngradeAck :: releaseCopyAck :: Nil = Enum(7){ UFix() }

  val uncachedAcquireTypeList = List(acquireReadUncached, acquireWriteUncached, acquireReadWordUncached, acquireWriteWordUncached, acquireAtomicUncached) 
  val hasDataAcquireTypeList = List(acquireWriteUncached, acquireWriteWordUncached, acquireAtomicUncached) 
  val hasDataReleaseTypeList = List(releaseVoluntaryInvalidateData, releaseInvalidateData, releaseDowngradeData, releaseCopyData)
  val hasDataGrantTypeList = List(grantReadShared, grantReadExclusive, grantReadUncached, grantReadWordUncached, grantAtomicUncached)

  def isHit (cmd: Bits, state: UFix): Bool = {
    val (read, write) = cpuCmdToRW(cmd)
    Mux(write, (state === tileExclusiveClean || state === tileExclusiveDirty),
        (state === tileShared || state === tileExclusiveClean || state === tileExclusiveDirty))
  }
  def isValid (state: UFix): Bool = {
    state != tileInvalid
  }

  def needsTransactionOnSecondaryMiss(cmd: Bits, outstanding: Acquire): Bool = {
    val (read, write) = cpuCmdToRW(cmd)
    (read && messageIsUncached(outstanding)) ||
      (write && (outstanding.a_type != acquireReadExclusive))
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
      M_CLN -> tileShared
    ))
  }
  def newStateOnWriteback() = newStateOnCacheControl(M_INV)
  def newStateOnFlush() = newStateOnCacheControl(M_INV)
  def newStateOnGrant(incoming: Grant, outstanding: Acquire): UFix = {
    MuxLookup(incoming.g_type, tileInvalid, Array(
      grantReadShared -> tileShared,
      grantReadExclusive  -> Mux(outstanding.a_type === acquireReadExclusive, tileExclusiveDirty, tileExclusiveClean),
      grantReadExclusiveAck -> tileExclusiveDirty, 
      grantReadUncached -> tileInvalid,
      grantWriteUncached -> tileInvalid,
      grantReadWordUncached -> tileInvalid,
      grantWriteWordUncached -> tileInvalid,
      grantAtomicUncached -> tileInvalid
    ))
  } 
  def newStateOnProbe(incoming: Probe, state: UFix): Bits = {
    MuxLookup(incoming.p_type, state, Array(
      probeInvalidate -> tileInvalid,
      probeDowngrade  -> tileShared,
      probeCopy       -> state
    ))
  }

  def getUncachedReadAcquire(addr: UFix, id: UFix) = Acquire(acquireReadUncached, addr, id)
  def getUncachedWriteAcquire(addr: UFix, id: UFix) = Acquire(acquireWriteUncached, addr, id)
  def getUncachedReadWordAcquire(addr: UFix, id: UFix) = Acquire(acquireReadWordUncached, addr, id)
  def getUncachedWriteWordAcquire(addr: UFix, id: UFix, write_mask: Bits) = Acquire(acquireWriteWordUncached, addr, id, write_mask)
  def getUncachedAtomicAcquire(addr: UFix, id: UFix, subword_addr: UFix, atomic_op: UFix) = Acquire(acquireAtomicUncached, addr, id, subword_addr, atomic_op)
  def getVoluntaryWriteback(addr: UFix, client_id: UFix, master_id: UFix) = Release(releaseVoluntaryInvalidateData, addr, client_id, master_id)
  def isUncachedReadTransaction(acq: Acquire) = acq.a_type === acquireReadUncached
  def isVoluntary(rel: Release) = rel.r_type === releaseVoluntaryInvalidateData
  def isVoluntary(gnt: Grant) = gnt.g_type === grantVoluntaryAck

  def getAcquireTypeOnPrimaryMiss(cmd: Bits, state: UFix): UFix = {
    val (read, write) = cpuCmdToRW(cmd)
    Mux(write || cmd === M_PFW, acquireReadExclusive, acquireReadShared)
  }
  def getAcquireTypeOnSecondaryMiss(cmd: Bits, state: UFix, outstanding: Acquire): UFix = {
    val (read, write) = cpuCmdToRW(cmd)
    Mux(write, acquireReadExclusive, outstanding.a_type)
  }
  def getReleaseTypeOnCacheControl(cmd: Bits): Bits = releaseVoluntaryInvalidateData // TODO
  def getReleaseTypeOnVoluntaryWriteback(): Bits = getReleaseTypeOnCacheControl(M_INV)

  def newRelease (incoming: Probe, state: UFix, id: UFix): Release = {
    val with_data = MuxLookup(incoming.p_type, releaseInvalidateData, Array(
      probeInvalidate -> releaseInvalidateData,
      probeDowngrade  -> releaseDowngradeData,
      probeCopy       -> releaseCopyData
    ))
    val without_data = MuxLookup(incoming.p_type, releaseInvalidateAck, Array(
      probeInvalidate -> releaseInvalidateAck,
      probeDowngrade  -> releaseDowngradeAck,
      probeCopy       -> releaseCopyAck
    ))
    Release( Mux(needsWriteback(state), with_data, without_data), incoming.addr, id, incoming.master_xact_id)
  }

  def messageHasData(msg: SourcedMessage) = msg match {
    case acq: Acquire => uFixListContains(hasDataAcquireTypeList, acq.a_type)
    case grant: Grant => uFixListContains(hasDataGrantTypeList, grant.g_type) 
    case rel: Release => uFixListContains(hasDataReleaseTypeList, rel.r_type) 
    case _ => Bool(false)
  }
  def messageUpdatesDataArray (reply: Grant): Bool = {
    (reply.g_type === grantReadShared || reply.g_type === grantReadExclusive)
  }
  def messageIsUncached(acq: Acquire): Bool = uFixListContains(uncachedAcquireTypeList, acq.a_type)

  def isCoherenceConflict(addr1: Bits, addr2: Bits): Bool = (addr1 === addr2)

  def getGrantType(a_type: UFix, count: UFix): Bits = {
    MuxLookup(a_type, grantReadUncached, Array(
      acquireReadShared    -> Mux(count > UFix(0), grantReadShared, grantReadExclusive),
      acquireReadExclusive -> grantReadExclusive,
      acquireReadUncached  -> grantReadUncached,
      acquireWriteUncached -> grantWriteUncached,
      acquireReadWordUncached  -> grantReadWordUncached,
      acquireWriteWordUncached -> grantWriteWordUncached,
      acquireAtomicUncached -> grantAtomicUncached
    ))
  }
  def getGrantType(rel: Release, count: UFix): Bits = {
    MuxLookup(rel.r_type, grantReadUncached, Array(
      releaseVoluntaryInvalidateData -> grantVoluntaryAck
    ))
  }


  def getProbeType(a_type: UFix, global_state: UFix): UFix = {
    MuxLookup(a_type, probeCopy, Array(
      acquireReadShared -> probeDowngrade,
      acquireReadExclusive -> probeInvalidate, 
      acquireReadUncached -> probeCopy, 
      acquireWriteUncached -> probeInvalidate,
      acquireReadWordUncached -> probeCopy, 
      acquireWriteWordUncached -> probeInvalidate,
      acquireAtomicUncached -> probeInvalidate
    ))
  }

  def needsOuterRead(a_type: UFix, global_state: UFix): Bool = {
      (a_type != acquireWriteUncached)
  }
  def needsOuterWrite(a_type: UFix, global_state: UFix): Bool = {
      (a_type === acquireWriteUncached)
  }
  def needsAckReply(a_type: UFix, global_state: UFix): Bool = {
      (a_type === acquireWriteUncached)
  }

  def requiresAck(grant: Grant) = Bool(true)
  def requiresAck(release: Release) = Bool(false)
  def needsSelfProbe(acq: Acquire) = acq.a_type === acquireReadUncached

  def pendingVoluntaryReleaseIsSufficient(r_type: UFix, p_type: UFix): Bool = (r_type === releaseVoluntaryInvalidateData)
}

class MigratoryCoherence extends CoherencePolicyWithUncached {

  val tileInvalid :: tileShared :: tileExclusiveClean :: tileExclusiveDirty :: tileSharedByTwo :: tileMigratoryClean :: tileMigratoryDirty :: Nil = Enum(7){ UFix() }

  val acquireReadShared :: acquireReadExclusive :: acquireReadUncached :: acquireWriteUncached :: acquireReadWordUncached :: acquireWriteWordUncached :: acquireAtomicUncached :: acquireInvalidateOthers :: Nil = Enum(8){ UFix() }
  val grantVoluntaryAck :: grantReadShared :: grantReadExclusive :: grantReadUncached :: grantWriteUncached :: grantReadExclusiveAck :: grantReadWordUncached :: grantWriteWordUncached :: grantAtomicUncached :: grantReadMigratory :: Nil = Enum(10){ UFix() }
  val probeInvalidate :: probeDowngrade :: probeCopy :: probeInvalidateOthers :: Nil = Enum(4){ UFix() }
  val releaseVoluntaryInvalidateData :: releaseInvalidateData :: releaseDowngradeData :: releaseCopyData :: releaseInvalidateAck :: releaseDowngradeAck :: releaseCopyAck :: releaseDowngradeDataMigratory :: releaseDowngradeAckHasCopy :: releaseInvalidateDataMigratory :: releaseInvalidateAckMigratory :: Nil = Enum(11){ UFix() }

  val uncachedAcquireTypeList = List(acquireReadUncached, acquireWriteUncached, acquireReadWordUncached, acquireWriteWordUncached, acquireAtomicUncached) 
  val hasDataAcquireTypeList = List(acquireWriteUncached, acquireWriteWordUncached, acquireAtomicUncached)
  val hasDataGrantTypeList = List(grantReadShared, grantReadExclusive, grantReadUncached, grantReadMigratory, grantReadWordUncached, grantAtomicUncached)
  val hasDataReleaseTypeList = List(releaseVoluntaryInvalidateData, releaseInvalidateData, releaseDowngradeData, releaseCopyData, releaseInvalidateDataMigratory, releaseDowngradeDataMigratory)

  def isHit (cmd: Bits, state: UFix): Bool = {
    val (read, write) = cpuCmdToRW(cmd)
    Mux(write, uFixListContains(List(tileExclusiveClean, tileExclusiveDirty, tileMigratoryClean, tileMigratoryDirty), state), (state != tileInvalid))
  }
  def isValid (state: UFix): Bool = {
    state != tileInvalid
  }

  def needsTransactionOnSecondaryMiss(cmd: Bits, outstanding: Acquire): Bool = {
    val (read, write) = cpuCmdToRW(cmd)
    (read && messageIsUncached(outstanding)) ||
      (write && (outstanding.a_type != acquireReadExclusive && outstanding.a_type != acquireInvalidateOthers))
  }
  def needsTransactionOnCacheControl(cmd: Bits, state: UFix): Bool = {
    MuxLookup(cmd, (state === tileExclusiveDirty), Array(
      M_INV -> uFixListContains(List(tileExclusiveDirty,tileMigratoryDirty),state),
      M_CLN -> uFixListContains(List(tileExclusiveDirty,tileMigratoryDirty),state)
    ))
  }
  def needsWriteback (state: UFix): Bool = {
    needsTransactionOnCacheControl(M_INV, state)
  }

  def newStateOnHit(cmd: Bits, state: UFix): UFix = { 
    val (read, write) = cpuCmdToRW(cmd)
    Mux(write, MuxLookup(state, tileExclusiveDirty, Array(
                tileExclusiveClean -> tileExclusiveDirty,
                tileMigratoryClean -> tileMigratoryDirty)), state)
  }
  def newStateOnCacheControl(cmd: Bits) = {
    MuxLookup(cmd, tileInvalid, Array(
      M_INV -> tileInvalid,
      M_CLN -> tileShared
    ))
  }
  def newStateOnWriteback() = newStateOnCacheControl(M_INV)
  def newStateOnFlush() = newStateOnCacheControl(M_INV)
  def newStateOnGrant(incoming: Grant, outstanding: Acquire): UFix = {
    MuxLookup(incoming.g_type, tileInvalid, Array(
      grantReadShared -> tileShared,
      grantReadExclusive  -> MuxLookup(outstanding.a_type, tileExclusiveDirty,  Array(
                                   acquireReadExclusive -> tileExclusiveDirty,
                                   acquireReadShared -> tileExclusiveClean)),
      grantReadExclusiveAck -> tileExclusiveDirty, 
      grantReadUncached -> tileInvalid,
      grantWriteUncached -> tileInvalid,
      grantReadWordUncached -> tileInvalid,
      grantWriteWordUncached -> tileInvalid,
      grantAtomicUncached -> tileInvalid,
      grantReadMigratory -> MuxLookup(outstanding.a_type, tileMigratoryDirty, Array(
                                  acquireInvalidateOthers -> tileMigratoryDirty,
                                  acquireReadExclusive -> tileMigratoryDirty,
                                  acquireReadShared -> tileMigratoryClean))
    ))
  } 
  def newStateOnProbe(incoming: Probe, state: UFix): Bits = {
    MuxLookup(incoming.p_type, state, Array(
      probeInvalidate -> tileInvalid,
      probeInvalidateOthers -> tileInvalid,
      probeCopy -> state,
      probeDowngrade -> MuxLookup(state, tileShared, Array(
                              tileExclusiveClean -> tileSharedByTwo,
                              tileExclusiveDirty -> tileSharedByTwo,
                              tileSharedByTwo    -> tileShared,
                              tileMigratoryClean -> tileSharedByTwo,
                              tileMigratoryDirty -> tileInvalid))
    ))
  }

  def getUncachedReadAcquire(addr: UFix, id: UFix) = Acquire(acquireReadUncached, addr, id)
  def getUncachedWriteAcquire(addr: UFix, id: UFix) = Acquire(acquireWriteUncached, addr, id)
  def getUncachedReadWordAcquire(addr: UFix, id: UFix) = Acquire(acquireReadWordUncached, addr, id)
  def getUncachedWriteWordAcquire(addr: UFix, id: UFix, write_mask: Bits) = Acquire(acquireWriteWordUncached, addr, id, write_mask)
  def getUncachedAtomicAcquire(addr: UFix, id: UFix, subword_addr: UFix, atomic_op: UFix) = Acquire(acquireAtomicUncached, addr, id, subword_addr, atomic_op)
  def getVoluntaryWriteback(addr: UFix, client_id: UFix, master_id: UFix) = Release(releaseVoluntaryInvalidateData, addr, client_id, master_id)
  def isUncachedReadTransaction(acq: Acquire) = acq.a_type === acquireReadUncached
  def isVoluntary(rel: Release) = rel.r_type === releaseVoluntaryInvalidateData
  def isVoluntary(gnt: Grant) = gnt.g_type === grantVoluntaryAck

  def getAcquireTypeOnPrimaryMiss(cmd: Bits, state: UFix): UFix = {
    val (read, write) = cpuCmdToRW(cmd)
    Mux(write || cmd === M_PFW, Mux(state === tileInvalid, acquireReadExclusive, acquireInvalidateOthers), acquireReadShared)
  }
  def getAcquireTypeOnSecondaryMiss(cmd: Bits, state: UFix, outstanding: Acquire): UFix = {
    val (read, write) = cpuCmdToRW(cmd)
    Mux(write, Mux(state === tileInvalid, acquireReadExclusive, acquireInvalidateOthers), outstanding.a_type)
  }
  def getReleaseTypeOnCacheControl(cmd: Bits): Bits = releaseVoluntaryInvalidateData // TODO
  def getReleaseTypeOnVoluntaryWriteback(): Bits = getReleaseTypeOnCacheControl(M_INV)

  def newRelease (incoming: Probe, state: UFix, id: UFix): Release = {
    Assert( incoming.p_type === probeInvalidateOthers && needsWriteback(state), "Bad probe request type, should be impossible.")
    val with_data = MuxLookup(incoming.p_type, releaseInvalidateData, Array(
      probeInvalidate       -> Mux(uFixListContains(List(tileExclusiveDirty, tileMigratoryDirty), state), 
                                    releaseInvalidateDataMigratory, releaseInvalidateData),
      probeDowngrade        -> Mux(state === tileMigratoryDirty, releaseDowngradeDataMigratory, releaseDowngradeData),
      probeCopy       -> releaseCopyData
    ))
    val without_data = MuxLookup(incoming.p_type, releaseInvalidateAck, Array(
      probeInvalidate       -> Mux(tileExclusiveClean === state, releaseInvalidateAckMigratory, releaseInvalidateAck),
      probeInvalidateOthers -> Mux(state === tileSharedByTwo, releaseInvalidateAckMigratory, releaseInvalidateAck),
      probeDowngrade  -> Mux(state != tileInvalid, releaseDowngradeAckHasCopy, releaseDowngradeAck),
      probeCopy       -> releaseCopyAck
    ))
    Release( Mux(needsWriteback(state), with_data, without_data), incoming.addr, id, incoming.master_xact_id)
  }

  def messageHasData(msg: SourcedMessage) = msg match {
    case acq: Acquire => uFixListContains(hasDataAcquireTypeList, acq.a_type)
    case grant: Grant => uFixListContains(hasDataGrantTypeList, grant.g_type) 
    case rel: Release => uFixListContains(hasDataReleaseTypeList, rel.r_type) 
    case _ => Bool(false)
  }
  def messageUpdatesDataArray (reply: Grant): Bool = {
    uFixListContains(List(grantReadShared, grantReadExclusive, grantReadMigratory), reply.g_type)
  }
  def messageIsUncached(acq: Acquire): Bool = uFixListContains(uncachedAcquireTypeList, acq.a_type)

  def isCoherenceConflict(addr1: Bits, addr2: Bits): Bool = (addr1 === addr2)

  def getGrantType(a_type: UFix, count: UFix): Bits = {
    MuxLookup(a_type, grantReadUncached, Array(
      acquireReadShared    -> Mux(count > UFix(0), grantReadShared, grantReadExclusive), //TODO: what is count? Depend on release.p_type???
      acquireReadExclusive -> grantReadExclusive,                                            
      acquireReadUncached  -> grantReadUncached,
      acquireWriteUncached -> grantWriteUncached,
      acquireReadWordUncached  -> grantReadWordUncached,
      acquireWriteWordUncached -> grantWriteWordUncached,
      acquireAtomicUncached -> grantAtomicUncached,
      acquireInvalidateOthers -> grantReadExclusiveAck                                      //TODO: add this to MESI?
    ))
  }
  def getGrantType(rel: Release, count: UFix): Bits = {
    MuxLookup(rel.r_type, grantReadUncached, Array(
      releaseVoluntaryInvalidateData -> grantVoluntaryAck
    ))
  }


  def getProbeType(a_type: UFix, global_state: UFix): UFix = {
    MuxLookup(a_type, probeCopy, Array(
      acquireReadShared -> probeDowngrade,
      acquireReadExclusive -> probeInvalidate, 
      acquireReadUncached -> probeCopy, 
      acquireWriteUncached -> probeInvalidate,
      acquireReadWordUncached -> probeCopy, 
      acquireWriteWordUncached -> probeInvalidate,
      acquireAtomicUncached -> probeInvalidate,
      acquireInvalidateOthers -> probeInvalidateOthers
    ))
  }

  def needsOuterRead(a_type: UFix, global_state: UFix): Bool = {
      (a_type != acquireWriteUncached && a_type != acquireInvalidateOthers)
  }
  def needsOuterWrite(a_type: UFix, global_state: UFix): Bool = {
      (a_type === acquireWriteUncached || a_type === acquireWriteWordUncached || a_type === acquireAtomicUncached)
  }
  def needsAckReply(a_type: UFix, global_state: UFix): Bool = {
      (a_type === acquireWriteUncached || a_type === acquireWriteWordUncached ||a_type === acquireInvalidateOthers)
  }
  def requiresAck(grant: Grant) = Bool(true)
  def requiresAck(release: Release) = Bool(false)
  def needsSelfProbe(acq: Acquire) = acq.a_type === acquireReadUncached

  def pendingVoluntaryReleaseIsSufficient(r_type: UFix, p_type: UFix): Bool = (r_type === releaseVoluntaryInvalidateData)
}
