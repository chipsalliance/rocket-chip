package uncore
import Chisel._

abstract class CoherencePolicy {
  def nClientStates: Int
  def nMasterStates: Int
  def nAcquireTypes: Int
  def nProbeTypes: Int
  def nReleaseTypes: Int
  def nGrantTypes: Int
  def clientStateWidth = log2Up(nClientStates)
  def masterStateWidth = log2Up(nMasterStates)
  def acquireTypeWidth = log2Up(nAcquireTypes)
  def probeTypeWidth = log2Up(nProbeTypes)
  def releaseTypeWidth = log2Up(nReleaseTypes)
  def grantTypeWidth = log2Up(nGrantTypes)

  def isHit (cmd: UInt, state: UInt): Bool
  def isValid (state: UInt): Bool

  def needsTransactionOnSecondaryMiss(cmd: UInt, outstanding: Acquire): Bool
  def needsTransactionOnCacheControl(cmd: UInt, state: UInt): Bool
  def needsWriteback (state: UInt): Bool

  def newStateOnHit(cmd: UInt, state: UInt): UInt
  def newStateOnCacheControl(cmd: UInt): UInt
  def newStateOnWriteback(): UInt
  def newStateOnFlush(): UInt
  def newStateOnGrant(incoming: Grant, outstanding: Acquire): UInt
  def newStateOnProbe(incoming: Probe, state: UInt): UInt

  def getAcquireTypeOnPrimaryMiss(cmd: UInt, state: UInt): UInt
  def getAcquireTypeOnSecondaryMiss(cmd: UInt, state: UInt, outstanding: Acquire): UInt
  def getProbeType(a_type: UInt, global_state: UInt): UInt
  def getReleaseTypeOnCacheControl(cmd: UInt): UInt
  def getReleaseTypeOnVoluntaryWriteback(): UInt
  def getReleaseTypeOnProbe(incoming: Probe, state: UInt): UInt
  def getGrantType(a_type: UInt, count: UInt): UInt
  def getGrantType(rel: Release, count: UInt): UInt

  def messageHasData (rel: SourcedMessage): Bool
  def messageUpdatesDataArray (reply: Grant): Bool
  def messageIsUncached(acq: Acquire): Bool

  def isCoherenceConflict(addr1: UInt, addr2: UInt): Bool
  def isVoluntary(rel: Release): Bool
  def isVoluntary(gnt: Grant): Bool
  def needsOuterRead(a_type: UInt, global_state: UInt): Bool
  def needsOuterWrite(a_type: UInt, global_state: UInt): Bool
  def requiresOuterAcquire(a_type: UInt, global_state: UInt): Bool
  def requiresSelfProbe(a_type: UInt): Bool
  def requiresAckForGrant(g_type: UInt): Bool
  def requiresAckForRelease(r_type: UInt): Bool
  def pendingVoluntaryReleaseIsSufficient(r_type: UInt, p_type: UInt): Bool

  def uIntListContains(list: List[UInt], elem: UInt): Bool = list.map(elem === _).reduceLeft(_||_)
}

trait UncachedTransactions {
  def getUncachedReadAcquireType: UInt
  def getUncachedWriteAcquireType: UInt
  def getUncachedReadWordAcquireType: UInt
  def getUncachedWriteWordAcquireType: UInt
  def getUncachedAtomicAcquireType: UInt
  def isUncachedReadTransaction(acq: Acquire): Bool
}

abstract class CoherencePolicyWithUncached extends CoherencePolicy with UncachedTransactions

abstract class IncoherentPolicy extends CoherencePolicy {
  // UNIMPLEMENTED
  def newStateOnProbe(incoming: Probe, state: UInt): UInt = state
  def getReleaseTypeOnProbe(incoming: Probe, state: UInt): UInt = UInt(0)
  def isCoherenceConflict(addr1: UInt, addr2: UInt): Bool = Bool(false)
  def getGrantType(a_type: UInt, count: UInt): UInt = UInt(0)
  def getGrantType(rel: Release, count: UInt): UInt = UInt(0)
  def getProbeType(a_type: UInt, global_state: UInt): UInt = UInt(0)
  def needsOuterRead(a_type: UInt, global_state: UInt): Bool = Bool(false)
  def needsOuterWrite(a_type: UInt, global_state: UInt): Bool = Bool(false)
  def requiresOuterAcquire(a_type: UInt, global_state: UInt): Bool = Bool(false)
  def requiresSelfProbe(a_type: UInt) = Bool(false)
  def requiresAckForGrant(g_type: UInt) = Bool(true)
  def requiresAckForRelease(r_type: UInt) = Bool(false)
  def pendingVoluntaryReleaseIsSufficient(r_type: UInt, p_type: UInt): Bool = Bool(false)

}

class ThreeStateIncoherence extends IncoherentPolicy {
  def nClientStates = 3
  def nMasterStates = 0
  def nAcquireTypes = 3
  def nProbeTypes = 0
  def nReleaseTypes = 2
  def nGrantTypes = 3
  val tileInvalid :: tileClean :: tileDirty :: Nil = Enum(UInt(), nClientStates)
  val acquireReadClean :: acquireReadDirty :: acquireWriteback :: Nil = Enum(UInt(), nAcquireTypes)
  val releaseVoluntaryInvalidateData :: releaseInvalidateAck :: Nil = Enum(UInt(), nReleaseTypes)
  val grantVoluntaryAck :: grantData :: grantAck :: Nil = Enum(UInt(), nGrantTypes)
  val uncachedAcquireTypeList = List() 
  val hasDataAcquireTypeList = List(acquireWriteback)
  val hasDataReleaseTypeList = List(acquireWriteback)
  val hasDataGrantTypeList = List(grantData)

  def isHit ( cmd: UInt, state: UInt): Bool = (state === tileClean || state === tileDirty)
  def isValid (state: UInt): Bool = state != tileInvalid

  def needsTransactionOnSecondaryMiss(cmd: UInt, outstanding: Acquire) = Bool(false)
  def needsTransactionOnCacheControl(cmd: UInt, state: UInt): Bool = state === tileDirty
  def needsWriteback (state: UInt): Bool = state === tileDirty

  def newState(cmd: UInt, state: UInt): UInt = {
    Mux(isWrite(cmd), tileDirty, Mux(isRead(cmd), Mux(state === tileDirty, tileDirty, tileClean), state))
  }
  def newStateOnHit(cmd: UInt, state: UInt): UInt = newState(cmd, state)
  def newStateOnCacheControl(cmd: UInt) = tileInvalid //TODO
  def newStateOnWriteback() = tileInvalid
  def newStateOnFlush() = tileInvalid
  def newStateOnGrant(incoming: Grant, outstanding: Acquire) = {
    MuxLookup(incoming.g_type, tileInvalid, Array(
      grantData -> Mux(outstanding.a_type === acquireReadDirty, tileDirty, tileClean),
      grantAck  -> tileInvalid
    ))
  }

  def isVoluntary(rel: Release) = rel.r_type === releaseVoluntaryInvalidateData
  def isVoluntary(gnt: Grant) = gnt.g_type === grantVoluntaryAck

  def getAcquireTypeOnPrimaryMiss(cmd: UInt, state: UInt): UInt = {
    Mux(isWriteIntent(cmd), acquireReadDirty, acquireReadClean)
  }
  def getAcquireTypeOnSecondaryMiss(cmd: UInt, state: UInt, outstanding: Acquire): UInt = {
    Mux(isWriteIntent(cmd), acquireReadDirty, outstanding.a_type)
  }
  def getReleaseTypeOnCacheControl(cmd: UInt): UInt = releaseVoluntaryInvalidateData // TODO
  def getReleaseTypeOnVoluntaryWriteback(): UInt = releaseVoluntaryInvalidateData

  def messageHasData( msg: SourcedMessage ) = msg match {
    case acq: Acquire => uIntListContains(hasDataAcquireTypeList, acq.a_type)
    case grant: Grant => uIntListContains(hasDataGrantTypeList, grant.g_type) 
    case rel: Release => Bool(false)
    case _ => Bool(false)
  }
  def messageUpdatesDataArray (reply: Grant) = (reply.g_type === grantData)
  def messageIsUncached(acq: Acquire): Bool = uIntListContains(uncachedAcquireTypeList, acq.a_type)
}

class MICoherence extends CoherencePolicyWithUncached {
  def nClientStates = 2
  def nMasterStates = 2
  def nAcquireTypes = 6
  def nProbeTypes = 2
  def nReleaseTypes = 5
  def nGrantTypes = 7

  val tileInvalid :: tileValid :: Nil = Enum(UInt(), nClientStates)
  val globalInvalid :: globalValid :: Nil = Enum(UInt(), nMasterStates)

  val acquireReadExclusive :: acquireReadUncached :: acquireWriteUncached :: acquireReadWordUncached :: acquireWriteWordUncached :: acquireAtomicUncached :: Nil = Enum(UInt(), nAcquireTypes)
  val probeInvalidate :: probeCopy :: Nil = Enum(UInt(), nProbeTypes)
  val releaseVoluntaryInvalidateData :: releaseInvalidateData :: releaseCopyData :: releaseInvalidateAck :: releaseCopyAck :: Nil = Enum(UInt(), nReleaseTypes)
  val grantVoluntaryAck :: grantReadExclusive :: grantReadUncached :: grantWriteUncached :: grantReadWordUncached :: grantWriteWordUncached :: grantAtomicUncached :: Nil = Enum(UInt(), nGrantTypes)

  val uncachedAcquireTypeList = List(acquireReadUncached, acquireWriteUncached, acquireReadWordUncached, acquireWriteWordUncached, acquireAtomicUncached) 
  val hasDataAcquireTypeList = List(acquireWriteUncached, acquireWriteWordUncached, acquireAtomicUncached)
  val hasDataReleaseTypeList = List(releaseVoluntaryInvalidateData, releaseInvalidateData, releaseCopyData)
  val hasDataGrantTypeList = List(grantReadExclusive, grantReadUncached, grantReadWordUncached, grantAtomicUncached)

  def isHit (cmd: UInt, state: UInt): Bool = state != tileInvalid
  def isValid (state: UInt): Bool = state != tileInvalid

  def needsTransactionOnSecondaryMiss(cmd: UInt, outstanding: Acquire): Bool = (outstanding.a_type != acquireReadExclusive)
  def needsTransactionOnCacheControl(cmd: UInt, state: UInt): Bool = {
    MuxLookup(cmd, (state === tileValid), Array(
      M_INV -> (state === tileValid),
      M_CLN -> (state === tileValid)
    ))
  }
  def needsWriteback (state: UInt): Bool = {
    needsTransactionOnCacheControl(M_INV, state)
  }

  def newStateOnHit(cmd: UInt, state: UInt): UInt = state
  def newStateOnCacheControl(cmd: UInt) = {
    MuxLookup(cmd, tileInvalid, Array(
      M_INV -> tileInvalid,
      M_CLN -> tileValid
    ))
  }
  def newStateOnWriteback() = newStateOnCacheControl(M_INV)
  def newStateOnFlush() = newStateOnCacheControl(M_INV)
  def newStateOnGrant(incoming: Grant, outstanding: Acquire): UInt = {
    MuxLookup(incoming.g_type, tileInvalid, Array(
      grantReadExclusive -> tileValid,
      grantReadUncached  -> tileInvalid,
      grantWriteUncached -> tileInvalid,
      grantReadWordUncached -> tileInvalid,
      grantWriteWordUncached -> tileInvalid,
      grantAtomicUncached -> tileInvalid
    ))
  } 
  def newStateOnProbe(incoming: Probe, state: UInt): UInt = {
    MuxLookup(incoming.p_type, state, Array(
      probeInvalidate -> tileInvalid,
      probeCopy       -> state
    ))
  }

  def getUncachedReadAcquireType = acquireReadUncached
  def getUncachedWriteAcquireType = acquireWriteUncached
  def getUncachedReadWordAcquireType = acquireReadWordUncached
  def getUncachedWriteWordAcquireType = acquireWriteWordUncached
  def getUncachedAtomicAcquireType = acquireAtomicUncached
  def isUncachedReadTransaction(acq: Acquire) = acq.a_type === acquireReadUncached
  def isVoluntary(rel: Release) = rel.r_type === releaseVoluntaryInvalidateData
  def isVoluntary(gnt: Grant) = gnt.g_type === grantVoluntaryAck

  def getAcquireTypeOnPrimaryMiss(cmd: UInt, state: UInt): UInt = acquireReadExclusive
  def getAcquireTypeOnSecondaryMiss(cmd: UInt, state: UInt, outstanding: Acquire): UInt = acquireReadExclusive
  def getReleaseTypeOnCacheControl(cmd: UInt): UInt = releaseVoluntaryInvalidateData // TODO
  def getReleaseTypeOnVoluntaryWriteback(): UInt = getReleaseTypeOnCacheControl(M_INV)
  def getReleaseTypeOnProbe(incoming: Probe, state: UInt): UInt = {
    val with_data = MuxLookup(incoming.p_type, releaseInvalidateData, Array(
      probeInvalidate -> releaseInvalidateData,
      probeCopy       -> releaseCopyData
    ))
    val without_data = MuxLookup(incoming.p_type, releaseInvalidateAck, Array(
      probeInvalidate -> releaseInvalidateAck,
      probeCopy       -> releaseCopyAck
    ))
    Mux(needsWriteback(state), with_data, without_data)
  }

  def messageHasData(msg: SourcedMessage) = msg match {
    case acq: Acquire => uIntListContains(hasDataAcquireTypeList, acq.a_type)
    case grant: Grant => uIntListContains(hasDataGrantTypeList, grant.g_type) 
    case rel: Release => uIntListContains(hasDataReleaseTypeList, rel.r_type) 
    case _ => Bool(false)
  }
  def messageUpdatesDataArray (reply: Grant): Bool = {
    (reply.g_type === grantReadExclusive)
  }
  def messageIsUncached(acq: Acquire): Bool = uIntListContains(uncachedAcquireTypeList, acq.a_type)

  def isCoherenceConflict(addr1: UInt, addr2: UInt): Bool = (addr1 === addr2)

  def getGrantType(a_type: UInt, count: UInt): UInt = {
    MuxLookup(a_type, grantReadUncached, Array(
      acquireReadExclusive -> grantReadExclusive,
      acquireReadUncached  -> grantReadUncached,
      acquireWriteUncached -> grantWriteUncached,
      acquireReadWordUncached  -> grantReadWordUncached,
      acquireWriteWordUncached -> grantWriteWordUncached,
      acquireAtomicUncached -> grantAtomicUncached
    ))
  }

  def getGrantType(rel: Release, count: UInt): UInt = {
    MuxLookup(rel.r_type, grantReadUncached, Array(
      releaseVoluntaryInvalidateData -> grantVoluntaryAck
    ))
  }

  def getProbeType(a_type: UInt, global_state: UInt): UInt = {
    MuxLookup(a_type, probeCopy, Array(
      acquireReadExclusive -> probeInvalidate, 
      acquireReadUncached -> probeCopy, 
      acquireWriteUncached -> probeInvalidate,
      acquireReadWordUncached -> probeCopy, 
      acquireWriteWordUncached -> probeInvalidate,
      acquireAtomicUncached -> probeInvalidate
    ))
  }

  def needsOuterRead(a_type: UInt, global_state: UInt): Bool = {
      (a_type != acquireWriteUncached)
  }
  def needsOuterWrite(a_type: UInt, global_state: UInt): Bool = {
      (a_type === acquireWriteUncached)
  }
  def requiresOuterAcquire(a_type: UInt, global_state: UInt): Bool = {
    needsOuterRead(a_type, global_state) || needsOuterWrite(a_type, global_state)
  }
  def requiresAckForGrant(g_type: UInt) = g_type != grantVoluntaryAck
  def requiresAckForRelease(r_type: UInt) = Bool(false)
  def requiresSelfProbe(a_type: UInt) = Bool(false)
  def pendingVoluntaryReleaseIsSufficient(r_type: UInt, p_type: UInt): Bool = (r_type === releaseVoluntaryInvalidateData)
}

class MEICoherence extends CoherencePolicyWithUncached {
  def nClientStates = 3
  def nMasterStates = 2
  def nAcquireTypes = 7
  def nProbeTypes = 3
  def nReleaseTypes = 7
  def nGrantTypes = 8

  val tileInvalid :: tileExclusiveClean :: tileExclusiveDirty :: Nil = Enum(UInt(), nClientStates)
  val globalInvalid :: globalExclusiveClean :: Nil = Enum(UInt(), nMasterStates)

  val acquireReadExclusiveClean :: acquireReadExclusiveDirty :: acquireReadUncached :: acquireWriteUncached :: acquireReadWordUncached :: acquireWriteWordUncached :: acquireAtomicUncached :: Nil = Enum(UInt(), nAcquireTypes)
  val probeInvalidate :: probeDowngrade :: probeCopy :: Nil = Enum(UInt(), nProbeTypes)
  val releaseVoluntaryInvalidateData :: releaseInvalidateData :: releaseDowngradeData :: releaseCopyData :: releaseInvalidateAck :: releaseDowngradeAck :: releaseCopyAck :: Nil = Enum(UInt(), nReleaseTypes)
  val grantVoluntaryAck :: grantReadExclusive :: grantReadUncached :: grantWriteUncached :: grantReadExclusiveAck :: grantReadWordUncached :: grantWriteWordUncached :: grantAtomicUncached :: Nil = Enum(UInt(), nGrantTypes)

  val uncachedAcquireTypeList = List(acquireReadUncached, acquireWriteUncached, acquireReadWordUncached, acquireWriteWordUncached, acquireAtomicUncached) 
  val hasDataAcquireTypeList = List(acquireWriteUncached, acquireWriteWordUncached, acquireAtomicUncached) 
  val hasDataReleaseTypeList = List(releaseVoluntaryInvalidateData, releaseInvalidateData, releaseDowngradeData, releaseCopyData)
  val hasDataGrantTypeList = List(grantReadExclusive, grantReadUncached, grantReadWordUncached, grantAtomicUncached)

  def isHit (cmd: UInt, state: UInt): Bool = state != tileInvalid
  def isValid (state: UInt): Bool = state != tileInvalid

  def needsTransactionOnSecondaryMiss(cmd: UInt, outstanding: Acquire): Bool = {
    (isRead(cmd) && messageIsUncached(outstanding)) ||
      (isWriteIntent(cmd) && (outstanding.a_type != acquireReadExclusiveDirty))
  }
  def needsTransactionOnCacheControl(cmd: UInt, state: UInt): Bool = {
    MuxLookup(cmd, (state === tileExclusiveDirty), Array(
      M_INV -> (state === tileExclusiveDirty),
      M_CLN -> (state === tileExclusiveDirty)
    ))
  }
  def needsWriteback (state: UInt): Bool = {
    needsTransactionOnCacheControl(M_INV, state)
  }

  def newStateOnHit(cmd: UInt, state: UInt): UInt = { 
    Mux(isWrite(cmd), tileExclusiveDirty, state)
  }
  def newStateOnCacheControl(cmd: UInt) = {
    MuxLookup(cmd, tileInvalid, Array(
      M_INV -> tileInvalid,
      M_CLN -> tileExclusiveClean
    ))
  }
  def newStateOnWriteback() = newStateOnCacheControl(M_INV)
  def newStateOnFlush() = newStateOnCacheControl(M_INV)
  def newStateOnGrant(incoming: Grant, outstanding: Acquire): UInt = {
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
  def newStateOnProbe(incoming: Probe, state: UInt): UInt = {
    MuxLookup(incoming.p_type, state, Array(
      probeInvalidate -> tileInvalid,
      probeDowngrade  -> tileExclusiveClean,
      probeCopy       -> state
    ))
  }

  def getUncachedReadAcquireType = acquireReadUncached
  def getUncachedWriteAcquireType = acquireWriteUncached
  def getUncachedReadWordAcquireType = acquireReadWordUncached
  def getUncachedWriteWordAcquireType = acquireWriteWordUncached
  def getUncachedAtomicAcquireType = acquireAtomicUncached
  def isUncachedReadTransaction(acq: Acquire) = acq.a_type === acquireReadUncached
  def isVoluntary(rel: Release) = rel.r_type === releaseVoluntaryInvalidateData
  def isVoluntary(gnt: Grant) = gnt.g_type === grantVoluntaryAck

  def getAcquireTypeOnPrimaryMiss(cmd: UInt, state: UInt): UInt = {
    Mux(isWriteIntent(cmd), acquireReadExclusiveDirty, acquireReadExclusiveClean)
  }
  def getAcquireTypeOnSecondaryMiss(cmd: UInt, state: UInt, outstanding: Acquire): UInt = {
    Mux(isWriteIntent(cmd), acquireReadExclusiveDirty, outstanding.a_type)
  }
  def getReleaseTypeOnCacheControl(cmd: UInt): UInt = releaseVoluntaryInvalidateData // TODO
  def getReleaseTypeOnVoluntaryWriteback(): UInt = getReleaseTypeOnCacheControl(M_INV)
  def getReleaseTypeOnProbe(incoming: Probe, state: UInt): UInt = {
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
    Mux(needsWriteback(state), with_data, without_data)
  }

  def messageHasData(msg: SourcedMessage) = msg match {
    case acq: Acquire => uIntListContains(hasDataAcquireTypeList, acq.a_type)
    case grant: Grant => uIntListContains(hasDataGrantTypeList, grant.g_type) 
    case rel: Release => uIntListContains(hasDataReleaseTypeList, rel.r_type) 
    case _ => Bool(false)
  }
  def messageUpdatesDataArray (reply: Grant): Bool = {
    (reply.g_type === grantReadExclusive)
  }
  def messageIsUncached(acq: Acquire): Bool = uIntListContains(uncachedAcquireTypeList, acq.a_type)

  def isCoherenceConflict(addr1: UInt, addr2: UInt): Bool = (addr1 === addr2)

  def getGrantType(a_type: UInt, count: UInt): UInt = {
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
  def getGrantType(rel: Release, count: UInt): UInt = {
    MuxLookup(rel.r_type, grantReadUncached, Array(
      releaseVoluntaryInvalidateData -> grantVoluntaryAck
    ))
  }


  def getProbeType(a_type: UInt, global_state: UInt): UInt = {
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

  def needsOuterRead(a_type: UInt, global_state: UInt): Bool = {
      (a_type != acquireWriteUncached)
  }
  def needsOuterWrite(a_type: UInt, global_state: UInt): Bool = {
      (a_type === acquireWriteUncached)
  }
  def requiresOuterAcquire(a_type: UInt, global_state: UInt): Bool = {
    needsOuterRead(a_type, global_state) || needsOuterWrite(a_type, global_state)
  }
  def requiresAckForGrant(g_type: UInt) = g_type != grantVoluntaryAck
  def requiresAckForRelease(r_type: UInt) = Bool(false)
  def requiresSelfProbe(a_type: UInt) = Bool(false)

  def pendingVoluntaryReleaseIsSufficient(r_type: UInt, p_type: UInt): Bool = (r_type === releaseVoluntaryInvalidateData)
}

class MSICoherence extends CoherencePolicyWithUncached {
  def nClientStates = 3
  def nMasterStates = 3
  def nAcquireTypes = 7
  def nProbeTypes = 3
  def nReleaseTypes = 7
  def nGrantTypes = 9

  val tileInvalid :: tileShared :: tileExclusiveDirty :: Nil = Enum(UInt(), nClientStates)
  val globalInvalid :: globalShared :: globalExclusive :: Nil = Enum(UInt(), nMasterStates)

  val acquireReadShared :: acquireReadExclusive :: acquireReadUncached :: acquireWriteUncached :: acquireReadWordUncached :: acquireWriteWordUncached :: acquireAtomicUncached :: Nil = Enum(UInt(), nAcquireTypes)
  val probeInvalidate :: probeDowngrade :: probeCopy :: Nil = Enum(UInt(), nProbeTypes)
  val releaseVoluntaryInvalidateData :: releaseInvalidateData :: releaseDowngradeData :: releaseCopyData :: releaseInvalidateAck :: releaseDowngradeAck :: releaseCopyAck :: Nil = Enum(UInt(), nReleaseTypes)
  val grantVoluntaryAck :: grantReadShared :: grantReadExclusive :: grantReadUncached :: grantWriteUncached :: grantReadExclusiveAck :: grantReadWordUncached :: grantWriteWordUncached :: grantAtomicUncached :: Nil = Enum(UInt(), nGrantTypes)

  val uncachedAcquireTypeList = List(acquireReadUncached, acquireWriteUncached, acquireReadWordUncached, acquireWriteWordUncached, acquireAtomicUncached) 
  val hasDataAcquireTypeList = List(acquireWriteUncached, acquireWriteWordUncached, acquireAtomicUncached)
  val hasDataReleaseTypeList = List(releaseVoluntaryInvalidateData, releaseInvalidateData, releaseDowngradeData, releaseCopyData)
  val hasDataGrantTypeList = List(grantReadShared, grantReadExclusive, grantReadUncached, grantReadWordUncached, grantAtomicUncached)

  def isHit (cmd: UInt, state: UInt): Bool = {
    Mux(isWriteIntent(cmd), (state === tileExclusiveDirty),
        (state === tileShared || state === tileExclusiveDirty))
  }
  def isValid (state: UInt): Bool = {
    state != tileInvalid
  }

  def needsTransactionOnSecondaryMiss(cmd: UInt, outstanding: Acquire): Bool = {
    (isRead(cmd) && messageIsUncached(outstanding)) || 
      (isWriteIntent(cmd) && (outstanding.a_type != acquireReadExclusive))
  }
  def needsTransactionOnCacheControl(cmd: UInt, state: UInt): Bool = {
    MuxLookup(cmd, (state === tileExclusiveDirty), Array(
      M_INV -> (state === tileExclusiveDirty),
      M_CLN -> (state === tileExclusiveDirty)
    ))
  }
  def needsWriteback (state: UInt): Bool = {
    needsTransactionOnCacheControl(M_INV, state)
  }

  def newStateOnHit(cmd: UInt, state: UInt): UInt = { 
    Mux(isWrite(cmd), tileExclusiveDirty, state)
  }
  def newStateOnCacheControl(cmd: UInt) = {
    MuxLookup(cmd, tileInvalid, Array(
      M_INV -> tileInvalid,
      M_CLN -> tileShared
    ))
  }
  def newStateOnWriteback() = newStateOnCacheControl(M_INV)
  def newStateOnFlush() = newStateOnCacheControl(M_INV)
  def newStateOnGrant(incoming: Grant, outstanding: Acquire): UInt = {
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
  def newStateOnProbe(incoming: Probe, state: UInt): UInt = {
    MuxLookup(incoming.p_type, state, Array(
      probeInvalidate -> tileInvalid,
      probeDowngrade  -> tileShared,
      probeCopy       -> state
    ))
  }

  def getUncachedReadAcquireType = acquireReadUncached
  def getUncachedWriteAcquireType = acquireWriteUncached
  def getUncachedReadWordAcquireType = acquireReadWordUncached
  def getUncachedWriteWordAcquireType = acquireWriteWordUncached
  def getUncachedAtomicAcquireType = acquireAtomicUncached
  def isUncachedReadTransaction(acq: Acquire) = acq.a_type === acquireReadUncached
  def isVoluntary(rel: Release) = rel.r_type === releaseVoluntaryInvalidateData
  def isVoluntary(gnt: Grant) = gnt.g_type === grantVoluntaryAck

  def getAcquireTypeOnPrimaryMiss(cmd: UInt, state: UInt): UInt = {
    Mux(isWriteIntent(cmd), acquireReadExclusive, acquireReadShared)
  }
  def getAcquireTypeOnSecondaryMiss(cmd: UInt, state: UInt, outstanding: Acquire): UInt = {
    Mux(isWriteIntent(cmd), acquireReadExclusive, outstanding.a_type)
  }
  def getReleaseTypeOnCacheControl(cmd: UInt): UInt = releaseVoluntaryInvalidateData // TODO
  def getReleaseTypeOnVoluntaryWriteback(): UInt = getReleaseTypeOnCacheControl(M_INV)
  def getReleaseTypeOnProbe(incoming: Probe, state: UInt): UInt = {
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
    Mux(needsWriteback(state), with_data, without_data)
  }

  def messageHasData(msg: SourcedMessage) = msg match {
    case acq: Acquire => uIntListContains(hasDataAcquireTypeList, acq.a_type)
    case grant: Grant => uIntListContains(hasDataGrantTypeList, grant.g_type) 
    case rel: Release => uIntListContains(hasDataReleaseTypeList, rel.r_type) 
    case _ => Bool(false)
  }
  def messageUpdatesDataArray (reply: Grant): Bool = {
    (reply.g_type === grantReadShared || reply.g_type === grantReadExclusive)
  }
  def messageIsUncached(acq: Acquire): Bool = uIntListContains(uncachedAcquireTypeList, acq.a_type)

  def isCoherenceConflict(addr1: UInt, addr2: UInt): Bool = (addr1 === addr2)

  def getGrantType(a_type: UInt, count: UInt): UInt = {
    MuxLookup(a_type, grantReadUncached, Array(
      acquireReadShared    -> Mux(count > UInt(0), grantReadShared, grantReadExclusive),
      acquireReadExclusive -> grantReadExclusive,
      acquireReadUncached  -> grantReadUncached,
      acquireWriteUncached -> grantWriteUncached,
      acquireReadWordUncached  -> grantReadWordUncached,
      acquireWriteWordUncached -> grantWriteWordUncached,
      acquireAtomicUncached -> grantAtomicUncached
    ))
  }
  def getGrantType(rel: Release, count: UInt): UInt = {
    MuxLookup(rel.r_type, grantReadUncached, Array(
      releaseVoluntaryInvalidateData -> grantVoluntaryAck
    ))
  }


  def getProbeType(a_type: UInt, global_state: UInt): UInt = {
    MuxLookup(a_type, probeCopy, Array(
      acquireReadShared -> probeDowngrade,
      acquireReadExclusive -> probeInvalidate, 
      acquireReadUncached -> probeCopy, 
      acquireWriteUncached -> probeInvalidate
    ))
  }

  def needsOuterRead(a_type: UInt, global_state: UInt): Bool = {
      (a_type != acquireWriteUncached)
  }
  def needsOuterWrite(a_type: UInt, global_state: UInt): Bool = {
      (a_type === acquireWriteUncached)
  }
  def requiresOuterAcquire(a_type: UInt, global_state: UInt): Bool = {
    needsOuterRead(a_type, global_state) || needsOuterWrite(a_type, global_state)
  }
  def requiresAckForGrant(g_type: UInt) = g_type != grantVoluntaryAck
  def requiresAckForRelease(r_type: UInt) = Bool(false)
  def requiresSelfProbe(a_type: UInt) = Bool(false)

  def pendingVoluntaryReleaseIsSufficient(r_type: UInt, p_type: UInt): Bool = (r_type === releaseVoluntaryInvalidateData)
}

class MESICoherence extends CoherencePolicyWithUncached {
  def nClientStates = 4
  def nMasterStates = 3
  def nAcquireTypes = 7
  def nProbeTypes = 3
  def nReleaseTypes = 7
  def nGrantTypes = 9

  val tileInvalid :: tileShared :: tileExclusiveClean :: tileExclusiveDirty :: Nil = Enum(UInt(), nClientStates)
  val globalInvalid :: globalShared :: globalExclusiveClean :: Nil = Enum(UInt(), nMasterStates)

  val acquireReadShared :: acquireReadExclusive :: acquireReadUncached :: acquireWriteUncached :: acquireReadWordUncached :: acquireWriteWordUncached :: acquireAtomicUncached :: Nil = Enum(UInt(), nAcquireTypes)
  val probeInvalidate :: probeDowngrade :: probeCopy :: Nil = Enum(UInt(), nProbeTypes)
  val releaseVoluntaryInvalidateData :: releaseInvalidateData :: releaseDowngradeData :: releaseCopyData :: releaseInvalidateAck :: releaseDowngradeAck :: releaseCopyAck :: Nil = Enum(UInt(), nReleaseTypes)
  val grantVoluntaryAck :: grantReadShared :: grantReadExclusive :: grantReadUncached :: grantWriteUncached :: grantReadExclusiveAck :: grantReadWordUncached :: grantWriteWordUncached :: grantAtomicUncached :: Nil = Enum(UInt(), nGrantTypes)

  val uncachedAcquireTypeList = List(acquireReadUncached, acquireWriteUncached, acquireReadWordUncached, acquireWriteWordUncached, acquireAtomicUncached) 
  val hasDataAcquireTypeList = List(acquireWriteUncached, acquireWriteWordUncached, acquireAtomicUncached) 
  val hasDataReleaseTypeList = List(releaseVoluntaryInvalidateData, releaseInvalidateData, releaseDowngradeData, releaseCopyData)
  val hasDataGrantTypeList = List(grantReadShared, grantReadExclusive, grantReadUncached, grantReadWordUncached, grantAtomicUncached)

  def isHit (cmd: UInt, state: UInt): Bool = {
    Mux(isWriteIntent(cmd), (state === tileExclusiveClean || state === tileExclusiveDirty),
        (state === tileShared || state === tileExclusiveClean || state === tileExclusiveDirty))
  }
  def isValid (state: UInt): Bool = {
    state != tileInvalid
  }

  def needsTransactionOnSecondaryMiss(cmd: UInt, outstanding: Acquire): Bool = {
    (isRead(cmd) && messageIsUncached(outstanding)) ||
      (isWriteIntent(cmd) && (outstanding.a_type != acquireReadExclusive))
  }
  def needsTransactionOnCacheControl(cmd: UInt, state: UInt): Bool = {
    MuxLookup(cmd, (state === tileExclusiveDirty), Array(
      M_INV -> (state === tileExclusiveDirty),
      M_CLN -> (state === tileExclusiveDirty)
    ))
  }
  def needsWriteback (state: UInt): Bool = {
    needsTransactionOnCacheControl(M_INV, state)
  }

  def newStateOnHit(cmd: UInt, state: UInt): UInt = { 
    Mux(isWrite(cmd), tileExclusiveDirty, state)
  }
  def newStateOnCacheControl(cmd: UInt) = {
    MuxLookup(cmd, tileInvalid, Array(
      M_INV -> tileInvalid,
      M_CLN -> tileShared
    ))
  }
  def newStateOnWriteback() = newStateOnCacheControl(M_INV)
  def newStateOnFlush() = newStateOnCacheControl(M_INV)
  def newStateOnGrant(incoming: Grant, outstanding: Acquire): UInt = {
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
  def newStateOnProbe(incoming: Probe, state: UInt): UInt = {
    MuxLookup(incoming.p_type, state, Array(
      probeInvalidate -> tileInvalid,
      probeDowngrade  -> tileShared,
      probeCopy       -> state
    ))
  }

  def getUncachedReadAcquireType = acquireReadUncached
  def getUncachedWriteAcquireType = acquireWriteUncached
  def getUncachedReadWordAcquireType = acquireReadWordUncached
  def getUncachedWriteWordAcquireType = acquireWriteWordUncached
  def getUncachedAtomicAcquireType = acquireAtomicUncached
  def isUncachedReadTransaction(acq: Acquire) = acq.a_type === acquireReadUncached
  def isVoluntary(rel: Release) = rel.r_type === releaseVoluntaryInvalidateData
  def isVoluntary(gnt: Grant) = gnt.g_type === grantVoluntaryAck

  def getAcquireTypeOnPrimaryMiss(cmd: UInt, state: UInt): UInt = {
    Mux(isWriteIntent(cmd), acquireReadExclusive, acquireReadShared)
  }
  def getAcquireTypeOnSecondaryMiss(cmd: UInt, state: UInt, outstanding: Acquire): UInt = {
    Mux(isWriteIntent(cmd), acquireReadExclusive, outstanding.a_type)
  }
  def getReleaseTypeOnCacheControl(cmd: UInt): UInt = releaseVoluntaryInvalidateData // TODO
  def getReleaseTypeOnVoluntaryWriteback(): UInt = getReleaseTypeOnCacheControl(M_INV)
  def getReleaseTypeOnProbe(incoming: Probe, state: UInt): UInt = {
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
    Mux(needsWriteback(state), with_data, without_data)
  }

  def messageHasData(msg: SourcedMessage) = msg match {
    case acq: Acquire => uIntListContains(hasDataAcquireTypeList, acq.a_type)
    case grant: Grant => uIntListContains(hasDataGrantTypeList, grant.g_type) 
    case rel: Release => uIntListContains(hasDataReleaseTypeList, rel.r_type) 
    case _ => Bool(false)
  }
  def messageUpdatesDataArray (reply: Grant): Bool = {
    (reply.g_type === grantReadShared || reply.g_type === grantReadExclusive)
  }
  def messageIsUncached(acq: Acquire): Bool = uIntListContains(uncachedAcquireTypeList, acq.a_type)

  def isCoherenceConflict(addr1: UInt, addr2: UInt): Bool = (addr1 === addr2)

  def getGrantType(a_type: UInt, count: UInt): UInt = {
    MuxLookup(a_type, grantReadUncached, Array(
      acquireReadShared    -> Mux(count > UInt(0), grantReadShared, grantReadExclusive),
      acquireReadExclusive -> grantReadExclusive,
      acquireReadUncached  -> grantReadUncached,
      acquireWriteUncached -> grantWriteUncached,
      acquireReadWordUncached  -> grantReadWordUncached,
      acquireWriteWordUncached -> grantWriteWordUncached,
      acquireAtomicUncached -> grantAtomicUncached
    ))
  }
  def getGrantType(rel: Release, count: UInt): UInt = {
    MuxLookup(rel.r_type, grantReadUncached, Array(
      releaseVoluntaryInvalidateData -> grantVoluntaryAck
    ))
  }


  def getProbeType(a_type: UInt, global_state: UInt): UInt = {
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

  def needsOuterRead(a_type: UInt, global_state: UInt): Bool = {
      (a_type != acquireWriteUncached)
  }
  def needsOuterWrite(a_type: UInt, global_state: UInt): Bool = {
      (a_type === acquireWriteUncached)
  }
  def requiresOuterAcquire(a_type: UInt, global_state: UInt): Bool = {
    needsOuterRead(a_type, global_state) || needsOuterWrite(a_type, global_state)
  }

  def requiresAckForGrant(g_type: UInt) = g_type != grantVoluntaryAck
  def requiresAckForRelease(r_type: UInt) = Bool(false)
  def requiresSelfProbe(a_type: UInt) = Bool(false)

  def pendingVoluntaryReleaseIsSufficient(r_type: UInt, p_type: UInt): Bool = (r_type === releaseVoluntaryInvalidateData)
}

class MigratoryCoherence extends CoherencePolicyWithUncached {
  def nClientStates = 7
  def nMasterStates = 0
  def nAcquireTypes = 8
  def nProbeTypes = 4
  def nReleaseTypes = 11
  def nGrantTypes = 9

  val tileInvalid :: tileShared :: tileExclusiveClean :: tileExclusiveDirty :: tileSharedByTwo :: tileMigratoryClean :: tileMigratoryDirty :: Nil = Enum(UInt(), nClientStates)

  val acquireReadShared :: acquireReadExclusive :: acquireReadUncached :: acquireWriteUncached :: acquireReadWordUncached :: acquireWriteWordUncached :: acquireAtomicUncached :: acquireInvalidateOthers :: Nil = Enum(UInt(), nAcquireTypes)
  val probeInvalidate :: probeDowngrade :: probeCopy :: probeInvalidateOthers :: Nil = Enum(UInt(), nProbeTypes)
  val releaseVoluntaryInvalidateData :: releaseInvalidateData :: releaseDowngradeData :: releaseCopyData :: releaseInvalidateAck :: releaseDowngradeAck :: releaseCopyAck :: releaseDowngradeDataMigratory :: releaseDowngradeAckHasCopy :: releaseInvalidateDataMigratory :: releaseInvalidateAckMigratory :: Nil = Enum(UInt(), nReleaseTypes)
  val grantVoluntaryAck :: grantReadShared :: grantReadExclusive :: grantReadUncached :: grantWriteUncached :: grantReadExclusiveAck :: grantReadWordUncached :: grantWriteWordUncached :: grantAtomicUncached :: grantReadMigratory :: Nil = Enum(UInt(), nGrantTypes)

  val uncachedAcquireTypeList = List(acquireReadUncached, acquireWriteUncached, acquireReadWordUncached, acquireWriteWordUncached, acquireAtomicUncached) 
  val hasDataAcquireTypeList = List(acquireWriteUncached, acquireWriteWordUncached, acquireAtomicUncached)
  val hasDataGrantTypeList = List(grantReadShared, grantReadExclusive, grantReadUncached, grantReadMigratory, grantReadWordUncached, grantAtomicUncached)
  val hasDataReleaseTypeList = List(releaseVoluntaryInvalidateData, releaseInvalidateData, releaseDowngradeData, releaseCopyData, releaseInvalidateDataMigratory, releaseDowngradeDataMigratory)

  def isHit (cmd: UInt, state: UInt): Bool = {
    Mux(isWriteIntent(cmd), uIntListContains(List(tileExclusiveClean, tileExclusiveDirty, tileMigratoryClean, tileMigratoryDirty), state), (state != tileInvalid))
  }
  def isValid (state: UInt): Bool = {
    state != tileInvalid
  }

  def needsTransactionOnSecondaryMiss(cmd: UInt, outstanding: Acquire): Bool = {
    (isRead(cmd) && messageIsUncached(outstanding)) ||
      (isWriteIntent(cmd) && (outstanding.a_type != acquireReadExclusive && outstanding.a_type != acquireInvalidateOthers))
  }
  def needsTransactionOnCacheControl(cmd: UInt, state: UInt): Bool = {
    MuxLookup(cmd, (state === tileExclusiveDirty), Array(
      M_INV -> uIntListContains(List(tileExclusiveDirty,tileMigratoryDirty),state),
      M_CLN -> uIntListContains(List(tileExclusiveDirty,tileMigratoryDirty),state)
    ))
  }
  def needsWriteback (state: UInt): Bool = {
    needsTransactionOnCacheControl(M_INV, state)
  }

  def newStateOnHit(cmd: UInt, state: UInt): UInt = { 
    Mux(isWrite(cmd), MuxLookup(state, tileExclusiveDirty, Array(
                tileExclusiveClean -> tileExclusiveDirty,
                tileMigratoryClean -> tileMigratoryDirty)), state)
  }
  def newStateOnCacheControl(cmd: UInt) = {
    MuxLookup(cmd, tileInvalid, Array(
      M_INV -> tileInvalid,
      M_CLN -> tileShared
    ))
  }
  def newStateOnWriteback() = newStateOnCacheControl(M_INV)
  def newStateOnFlush() = newStateOnCacheControl(M_INV)
  def newStateOnGrant(incoming: Grant, outstanding: Acquire): UInt = {
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
  def newStateOnProbe(incoming: Probe, state: UInt): UInt = {
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

  def getUncachedReadAcquireType = acquireReadUncached
  def getUncachedWriteAcquireType = acquireWriteUncached
  def getUncachedReadWordAcquireType = acquireReadWordUncached
  def getUncachedWriteWordAcquireType = acquireWriteWordUncached
  def getUncachedAtomicAcquireType = acquireAtomicUncached
  def isUncachedReadTransaction(acq: Acquire) = acq.a_type === acquireReadUncached
  def isVoluntary(rel: Release) = rel.r_type === releaseVoluntaryInvalidateData
  def isVoluntary(gnt: Grant) = gnt.g_type === grantVoluntaryAck

  def getAcquireTypeOnPrimaryMiss(cmd: UInt, state: UInt): UInt = {
    Mux(isWriteIntent(cmd), Mux(state === tileInvalid, acquireReadExclusive, acquireInvalidateOthers), acquireReadShared)
  }
  def getAcquireTypeOnSecondaryMiss(cmd: UInt, state: UInt, outstanding: Acquire): UInt = {
    Mux(isWriteIntent(cmd), Mux(state === tileInvalid, acquireReadExclusive, acquireInvalidateOthers), outstanding.a_type)
  }
  def getReleaseTypeOnCacheControl(cmd: UInt): UInt = releaseVoluntaryInvalidateData // TODO
  def getReleaseTypeOnVoluntaryWriteback(): UInt = getReleaseTypeOnCacheControl(M_INV)
  def getReleaseTypeOnProbe(incoming: Probe, state: UInt): UInt = {
    val with_data = MuxLookup(incoming.p_type, releaseInvalidateData, Array(
      probeInvalidate -> Mux(uIntListContains(List(tileExclusiveDirty, tileMigratoryDirty), state), 
                                    releaseInvalidateDataMigratory, releaseInvalidateData),
      probeDowngrade -> Mux(state === tileMigratoryDirty, releaseDowngradeDataMigratory, releaseDowngradeData),
      probeCopy -> releaseCopyData
    ))
    val without_data = MuxLookup(incoming.p_type, releaseInvalidateAck, Array(
      probeInvalidate -> Mux(tileExclusiveClean === state, releaseInvalidateAckMigratory, releaseInvalidateAck),
      probeInvalidateOthers -> Mux(state === tileSharedByTwo, releaseInvalidateAckMigratory, releaseInvalidateAck),
      probeDowngrade  -> Mux(state != tileInvalid, releaseDowngradeAckHasCopy, releaseDowngradeAck),
      probeCopy       -> releaseCopyAck
    ))
    Mux(needsWriteback(state), with_data, without_data)
  }

  def messageHasData(msg: SourcedMessage) = msg match {
    case acq: Acquire => uIntListContains(hasDataAcquireTypeList, acq.a_type)
    case grant: Grant => uIntListContains(hasDataGrantTypeList, grant.g_type) 
    case rel: Release => uIntListContains(hasDataReleaseTypeList, rel.r_type) 
    case _ => Bool(false)
  }
  def messageUpdatesDataArray (reply: Grant): Bool = {
    uIntListContains(List(grantReadShared, grantReadExclusive, grantReadMigratory), reply.g_type)
  }
  def messageIsUncached(acq: Acquire): Bool = uIntListContains(uncachedAcquireTypeList, acq.a_type)

  def isCoherenceConflict(addr1: UInt, addr2: UInt): Bool = (addr1 === addr2)

  def getGrantType(a_type: UInt, count: UInt): UInt = {
    MuxLookup(a_type, grantReadUncached, Array(
      acquireReadShared    -> Mux(count > UInt(0), grantReadShared, grantReadExclusive), //TODO: what is count? Depend on release.p_type???
      acquireReadExclusive -> grantReadExclusive,                                            
      acquireReadUncached  -> grantReadUncached,
      acquireWriteUncached -> grantWriteUncached,
      acquireReadWordUncached  -> grantReadWordUncached,
      acquireWriteWordUncached -> grantWriteWordUncached,
      acquireAtomicUncached -> grantAtomicUncached,
      acquireInvalidateOthers -> grantReadExclusiveAck                                      //TODO: add this to MESI?
    ))
  }
  def getGrantType(rel: Release, count: UInt): UInt = {
    MuxLookup(rel.r_type, grantReadUncached, Array(
      releaseVoluntaryInvalidateData -> grantVoluntaryAck
    ))
  }


  def getProbeType(a_type: UInt, global_state: UInt): UInt = {
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

  def needsOuterRead(a_type: UInt, global_state: UInt): Bool = {
      (a_type != acquireWriteUncached && a_type != acquireInvalidateOthers)
  }
  def needsOuterWrite(a_type: UInt, global_state: UInt): Bool = {
      (a_type === acquireWriteUncached || a_type === acquireWriteWordUncached || a_type === acquireAtomicUncached)
  }
  def requiresOuterAcquire(a_type: UInt, global_state: UInt): Bool = {
    needsOuterRead(a_type, global_state) || needsOuterWrite(a_type, global_state)
  }

  def requiresAckForGrant(g_type: UInt) = g_type != grantVoluntaryAck
  def requiresAckForRelease(r_type: UInt) = Bool(false)
  def requiresSelfProbe(a_type: UInt) = Bool(false)

  def pendingVoluntaryReleaseIsSufficient(r_type: UInt, p_type: UInt): Bool = (r_type === releaseVoluntaryInvalidateData)
}
