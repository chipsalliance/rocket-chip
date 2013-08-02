package uncore
import Chisel._

abstract trait CoherenceAgentRole
abstract trait ClientCoherenceAgent extends CoherenceAgentRole
abstract trait MasterCoherenceAgent extends CoherenceAgentRole

abstract class CoherencePolicy {
  def nClientStates: Int
  def nMasterStates: Int
  def nAcquireTypes: Int
  def nProbeTypes: Int
  def nReleaseTypes: Int
  def nGrantTypes: Int
  def clientStateBits = log2Up(nClientStates)
  def masterStateBits = log2Up(nMasterStates)
  def acquireTypeBits = log2Up(nAcquireTypes)
  def probeTypeBits = log2Up(nProbeTypes)
  def releaseTypeBits = log2Up(nReleaseTypes)
  def grantTypeBits = log2Up(nGrantTypes)

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
  def getProbeType(a_type: UFix, global_state: UFix): UFix
  def getReleaseTypeOnCacheControl(cmd: Bits): Bits
  def getReleaseTypeOnVoluntaryWriteback(): Bits
  def getReleaseTypeOnProbe(incoming: Probe, state: UFix): Bits
  def getGrantType(a_type: UFix, count: UFix): Bits
  def getGrantType(rel: Release, count: UFix): Bits

  def messageHasData (rel: SourcedMessage): Bool
  def messageUpdatesDataArray (reply: Grant): Bool
  def messageIsUncached(acq: Acquire): Bool

  def isCoherenceConflict(addr1: Bits, addr2: Bits): Bool
  def isVoluntary(rel: Release): Bool
  def isVoluntary(gnt: Grant): Bool
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
  def getUncachedReadAcquireType: Bits
  def getUncachedWriteAcquireType: Bits
  def getUncachedReadWordAcquireType: Bits
  def getUncachedWriteWordAcquireType: Bits
  def getUncachedAtomicAcquireType: Bits
  def isUncachedReadTransaction(acq: Acquire): Bool
}

abstract class CoherencePolicyWithUncached extends CoherencePolicy with UncachedTransactions

abstract class IncoherentPolicy extends CoherencePolicy {
  // UNIMPLEMENTED
  def newStateOnProbe(incoming: Probe, state: UFix): Bits = state
  def getReleaseTypeOnProbe(incoming: Probe, state: UFix): Bits = Bits(0)
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
  def nClientStates = 3
  def nMasterStates = 0
  def nAcquireTypes = 3
  def nProbeTypes = 0
  def nReleaseTypes = 2
  def nGrantTypes = 3
  val tileInvalid :: tileClean :: tileDirty :: Nil = Enum(nClientStates){ UFix() }
  val acquireReadClean :: acquireReadDirty :: acquireWriteback :: Nil = Enum(nAcquireTypes){ UFix() }
  val releaseVoluntaryInvalidateData :: releaseInvalidateAck :: Nil = Enum(nReleaseTypes){ UFix() }
  val grantVoluntaryAck :: grantData :: grantAck :: Nil = Enum(nGrantTypes){ UFix() }
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
    Mux(isWrite(cmd), tileDirty, Mux(isRead(cmd), Mux(state === tileDirty, tileDirty, tileClean), state))
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

  def isVoluntary(rel: Release) = rel.r_type === releaseVoluntaryInvalidateData
  def isVoluntary(gnt: Grant) = gnt.g_type === grantVoluntaryAck

  def getAcquireTypeOnPrimaryMiss(cmd: Bits, state: UFix): UFix = {
    Mux(isWriteIntent(cmd), acquireReadDirty, acquireReadClean)
  }
  def getAcquireTypeOnSecondaryMiss(cmd: Bits, state: UFix, outstanding: Acquire): UFix = {
    Mux(isWriteIntent(cmd), acquireReadDirty, outstanding.a_type)
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
  def nClientStates = 2
  def nMasterStates = 2
  def nAcquireTypes = 6
  def nProbeTypes = 2
  def nReleaseTypes = 5
  def nGrantTypes = 7

  val tileInvalid :: tileValid :: Nil = Enum(nClientStates){ UFix() }
  val globalInvalid :: globalValid :: Nil = Enum(nMasterStates){ UFix() }

  val acquireReadExclusive :: acquireReadUncached :: acquireWriteUncached :: acquireReadWordUncached :: acquireWriteWordUncached :: acquireAtomicUncached :: Nil = Enum(nAcquireTypes){ UFix() }
  val probeInvalidate :: probeCopy :: Nil = Enum(nProbeTypes){ UFix() }
  val releaseVoluntaryInvalidateData :: releaseInvalidateData :: releaseCopyData :: releaseInvalidateAck :: releaseCopyAck :: Nil = Enum(nReleaseTypes){ UFix() }
  val grantVoluntaryAck :: grantReadExclusive :: grantReadUncached :: grantWriteUncached :: grantReadWordUncached :: grantWriteWordUncached :: grantAtomicUncached :: Nil = Enum(nGrantTypes){ UFix() }

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

  def getUncachedReadAcquireType = acquireReadUncached
  def getUncachedWriteAcquireType = acquireWriteUncached
  def getUncachedReadWordAcquireType = acquireReadWordUncached
  def getUncachedWriteWordAcquireType = acquireWriteWordUncached
  def getUncachedAtomicAcquireType = acquireAtomicUncached
  def isUncachedReadTransaction(acq: Acquire) = acq.a_type === acquireReadUncached
  def isVoluntary(rel: Release) = rel.r_type === releaseVoluntaryInvalidateData
  def isVoluntary(gnt: Grant) = gnt.g_type === grantVoluntaryAck

  def getAcquireTypeOnPrimaryMiss(cmd: Bits, state: UFix): UFix = acquireReadExclusive
  def getAcquireTypeOnSecondaryMiss(cmd: Bits, state: UFix, outstanding: Acquire): UFix = acquireReadExclusive
  def getReleaseTypeOnCacheControl(cmd: Bits): Bits = releaseVoluntaryInvalidateData // TODO
  def getReleaseTypeOnVoluntaryWriteback(): Bits = getReleaseTypeOnCacheControl(M_INV)
  def getReleaseTypeOnProbe(incoming: Probe, state: UFix): Bits = {
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
  def requiresAck(grant: Grant) = grant.g_type != grantVoluntaryAck
  def requiresAck(release: Release) = Bool(false)
  def needsSelfProbe(acq: Acquire) = Bool(false)
  def pendingVoluntaryReleaseIsSufficient(r_type: UFix, p_type: UFix): Bool = (r_type === releaseVoluntaryInvalidateData)
}

class MEICoherence extends CoherencePolicyWithUncached {
  def nClientStates = 3
  def nMasterStates = 2
  def nAcquireTypes = 7
  def nProbeTypes = 3
  def nReleaseTypes = 7
  def nGrantTypes = 8

  val tileInvalid :: tileExclusiveClean :: tileExclusiveDirty :: Nil = Enum(nClientStates){ UFix() }
  val globalInvalid :: globalExclusiveClean :: Nil = Enum(nMasterStates){ UFix() }

  val acquireReadExclusiveClean :: acquireReadExclusiveDirty :: acquireReadUncached :: acquireWriteUncached :: acquireReadWordUncached :: acquireWriteWordUncached :: acquireAtomicUncached :: Nil = Enum(nAcquireTypes){ UFix() }
  val probeInvalidate :: probeDowngrade :: probeCopy :: Nil = Enum(nProbeTypes){ UFix() }
  val releaseVoluntaryInvalidateData :: releaseInvalidateData :: releaseDowngradeData :: releaseCopyData :: releaseInvalidateAck :: releaseDowngradeAck :: releaseCopyAck :: Nil = Enum(nReleaseTypes){ UFix() }
  val grantVoluntaryAck :: grantReadExclusive :: grantReadUncached :: grantWriteUncached :: grantReadExclusiveAck :: grantReadWordUncached :: grantWriteWordUncached :: grantAtomicUncached :: Nil = Enum(nGrantTypes){ UFix() }

  val uncachedAcquireTypeList = List(acquireReadUncached, acquireWriteUncached, acquireReadWordUncached, acquireWriteWordUncached, acquireAtomicUncached) 
  val hasDataAcquireTypeList = List(acquireWriteUncached, acquireWriteWordUncached, acquireAtomicUncached) 
  val hasDataReleaseTypeList = List(releaseVoluntaryInvalidateData, releaseInvalidateData, releaseDowngradeData, releaseCopyData)
  val hasDataGrantTypeList = List(grantReadExclusive, grantReadUncached, grantReadWordUncached, grantAtomicUncached)

  def isHit (cmd: Bits, state: UFix): Bool = state != tileInvalid
  def isValid (state: UFix): Bool = state != tileInvalid

  def needsTransactionOnSecondaryMiss(cmd: Bits, outstanding: Acquire): Bool = {
    (isRead(cmd) && messageIsUncached(outstanding)) ||
      (isWriteIntent(cmd) && (outstanding.a_type != acquireReadExclusiveDirty))
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
    Mux(isWrite(cmd), tileExclusiveDirty, state)
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

  def getUncachedReadAcquireType = acquireReadUncached
  def getUncachedWriteAcquireType = acquireWriteUncached
  def getUncachedReadWordAcquireType = acquireReadWordUncached
  def getUncachedWriteWordAcquireType = acquireWriteWordUncached
  def getUncachedAtomicAcquireType = acquireAtomicUncached
  def isUncachedReadTransaction(acq: Acquire) = acq.a_type === acquireReadUncached
  def isVoluntary(rel: Release) = rel.r_type === releaseVoluntaryInvalidateData
  def isVoluntary(gnt: Grant) = gnt.g_type === grantVoluntaryAck

  def getAcquireTypeOnPrimaryMiss(cmd: Bits, state: UFix): UFix = {
    Mux(isWriteIntent(cmd), acquireReadExclusiveDirty, acquireReadExclusiveClean)
  }
  def getAcquireTypeOnSecondaryMiss(cmd: Bits, state: UFix, outstanding: Acquire): UFix = {
    Mux(isWriteIntent(cmd), acquireReadExclusiveDirty, outstanding.a_type)
  }
  def getReleaseTypeOnCacheControl(cmd: Bits): Bits = releaseVoluntaryInvalidateData // TODO
  def getReleaseTypeOnVoluntaryWriteback(): Bits = getReleaseTypeOnCacheControl(M_INV)
  def getReleaseTypeOnProbe(incoming: Probe, state: UFix): Bits = {
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
  def requiresAck(grant: Grant) = grant.g_type != grantVoluntaryAck
  def requiresAck(release: Release) = Bool(false)
  def needsSelfProbe(acq: Acquire) = Bool(false)

  def pendingVoluntaryReleaseIsSufficient(r_type: UFix, p_type: UFix): Bool = (r_type === releaseVoluntaryInvalidateData)
}

class MSICoherence extends CoherencePolicyWithUncached {
  def nClientStates = 3
  def nMasterStates = 3
  def nAcquireTypes = 7
  def nProbeTypes = 3
  def nReleaseTypes = 7
  def nGrantTypes = 9

  val tileInvalid :: tileShared :: tileExclusiveDirty :: Nil = Enum(nClientStates){ UFix() }
  val globalInvalid :: globalShared :: globalExclusive :: Nil = Enum(nMasterStates){ UFix() }

  val acquireReadShared :: acquireReadExclusive :: acquireReadUncached :: acquireWriteUncached :: acquireReadWordUncached :: acquireWriteWordUncached :: acquireAtomicUncached :: Nil = Enum(nAcquireTypes){ UFix() }
  val probeInvalidate :: probeDowngrade :: probeCopy :: Nil = Enum(nProbeTypes){ UFix() }
  val releaseVoluntaryInvalidateData :: releaseInvalidateData :: releaseDowngradeData :: releaseCopyData :: releaseInvalidateAck :: releaseDowngradeAck :: releaseCopyAck :: Nil = Enum(nReleaseTypes){ UFix() }
  val grantVoluntaryAck :: grantReadShared :: grantReadExclusive :: grantReadUncached :: grantWriteUncached :: grantReadExclusiveAck :: grantReadWordUncached :: grantWriteWordUncached :: grantAtomicUncached :: Nil = Enum(nGrantTypes){ UFix() }

  val uncachedAcquireTypeList = List(acquireReadUncached, acquireWriteUncached, acquireReadWordUncached, acquireWriteWordUncached, acquireAtomicUncached) 
  val hasDataAcquireTypeList = List(acquireWriteUncached, acquireWriteWordUncached, acquireAtomicUncached)
  val hasDataReleaseTypeList = List(releaseVoluntaryInvalidateData, releaseInvalidateData, releaseDowngradeData, releaseCopyData)
  val hasDataGrantTypeList = List(grantReadShared, grantReadExclusive, grantReadUncached, grantReadWordUncached, grantAtomicUncached)

  def isHit (cmd: Bits, state: UFix): Bool = {
    Mux(isWriteIntent(cmd), (state === tileExclusiveDirty),
        (state === tileShared || state === tileExclusiveDirty))
  }
  def isValid (state: UFix): Bool = {
    state != tileInvalid
  }

  def needsTransactionOnSecondaryMiss(cmd: Bits, outstanding: Acquire): Bool = {
    (isRead(cmd) && messageIsUncached(outstanding)) || 
      (isWriteIntent(cmd) && (outstanding.a_type != acquireReadExclusive))
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
    Mux(isWrite(cmd), tileExclusiveDirty, state)
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

  def getUncachedReadAcquireType = acquireReadUncached
  def getUncachedWriteAcquireType = acquireWriteUncached
  def getUncachedReadWordAcquireType = acquireReadWordUncached
  def getUncachedWriteWordAcquireType = acquireWriteWordUncached
  def getUncachedAtomicAcquireType = acquireAtomicUncached
  def isUncachedReadTransaction(acq: Acquire) = acq.a_type === acquireReadUncached
  def isVoluntary(rel: Release) = rel.r_type === releaseVoluntaryInvalidateData
  def isVoluntary(gnt: Grant) = gnt.g_type === grantVoluntaryAck

  def getAcquireTypeOnPrimaryMiss(cmd: Bits, state: UFix): UFix = {
    Mux(isWriteIntent(cmd), acquireReadExclusive, acquireReadShared)
  }
  def getAcquireTypeOnSecondaryMiss(cmd: Bits, state: UFix, outstanding: Acquire): UFix = {
    Mux(isWriteIntent(cmd), acquireReadExclusive, outstanding.a_type)
  }
  def getReleaseTypeOnCacheControl(cmd: Bits): Bits = releaseVoluntaryInvalidateData // TODO
  def getReleaseTypeOnVoluntaryWriteback(): Bits = getReleaseTypeOnCacheControl(M_INV)
  def getReleaseTypeOnProbe(incoming: Probe, state: UFix): Bits = {
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
  def requiresAck(grant: Grant) = grant.g_type != grantVoluntaryAck
  def requiresAck(release: Release) = Bool(false)
  def needsSelfProbe(acq: Acquire) = Bool(false)

  def pendingVoluntaryReleaseIsSufficient(r_type: UFix, p_type: UFix): Bool = (r_type === releaseVoluntaryInvalidateData)
}

class MESICoherence extends CoherencePolicyWithUncached {
  def nClientStates = 4
  def nMasterStates = 3
  def nAcquireTypes = 7
  def nProbeTypes = 3
  def nReleaseTypes = 7
  def nGrantTypes = 9

  val tileInvalid :: tileShared :: tileExclusiveClean :: tileExclusiveDirty :: Nil = Enum(nClientStates){ UFix() }
  val globalInvalid :: globalShared :: globalExclusiveClean :: Nil = Enum(nMasterStates){ UFix() }

  val acquireReadShared :: acquireReadExclusive :: acquireReadUncached :: acquireWriteUncached :: acquireReadWordUncached :: acquireWriteWordUncached :: acquireAtomicUncached :: Nil = Enum(nAcquireTypes){ UFix() }
  val probeInvalidate :: probeDowngrade :: probeCopy :: Nil = Enum(nProbeTypes){ UFix() }
  val releaseVoluntaryInvalidateData :: releaseInvalidateData :: releaseDowngradeData :: releaseCopyData :: releaseInvalidateAck :: releaseDowngradeAck :: releaseCopyAck :: Nil = Enum(nReleaseTypes){ UFix() }
  val grantVoluntaryAck :: grantReadShared :: grantReadExclusive :: grantReadUncached :: grantWriteUncached :: grantReadExclusiveAck :: grantReadWordUncached :: grantWriteWordUncached :: grantAtomicUncached :: Nil = Enum(nGrantTypes){ UFix() }

  val uncachedAcquireTypeList = List(acquireReadUncached, acquireWriteUncached, acquireReadWordUncached, acquireWriteWordUncached, acquireAtomicUncached) 
  val hasDataAcquireTypeList = List(acquireWriteUncached, acquireWriteWordUncached, acquireAtomicUncached) 
  val hasDataReleaseTypeList = List(releaseVoluntaryInvalidateData, releaseInvalidateData, releaseDowngradeData, releaseCopyData)
  val hasDataGrantTypeList = List(grantReadShared, grantReadExclusive, grantReadUncached, grantReadWordUncached, grantAtomicUncached)

  def isHit (cmd: Bits, state: UFix): Bool = {
    Mux(isWriteIntent(cmd), (state === tileExclusiveClean || state === tileExclusiveDirty),
        (state === tileShared || state === tileExclusiveClean || state === tileExclusiveDirty))
  }
  def isValid (state: UFix): Bool = {
    state != tileInvalid
  }

  def needsTransactionOnSecondaryMiss(cmd: Bits, outstanding: Acquire): Bool = {
    (isRead(cmd) && messageIsUncached(outstanding)) ||
      (isWriteIntent(cmd) && (outstanding.a_type != acquireReadExclusive))
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
    Mux(isWrite(cmd), tileExclusiveDirty, state)
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

  def getUncachedReadAcquireType = acquireReadUncached
  def getUncachedWriteAcquireType = acquireWriteUncached
  def getUncachedReadWordAcquireType = acquireReadWordUncached
  def getUncachedWriteWordAcquireType = acquireWriteWordUncached
  def getUncachedAtomicAcquireType = acquireAtomicUncached
  def isUncachedReadTransaction(acq: Acquire) = acq.a_type === acquireReadUncached
  def isVoluntary(rel: Release) = rel.r_type === releaseVoluntaryInvalidateData
  def isVoluntary(gnt: Grant) = gnt.g_type === grantVoluntaryAck

  def getAcquireTypeOnPrimaryMiss(cmd: Bits, state: UFix): UFix = {
    Mux(isWriteIntent(cmd), acquireReadExclusive, acquireReadShared)
  }
  def getAcquireTypeOnSecondaryMiss(cmd: Bits, state: UFix, outstanding: Acquire): UFix = {
    Mux(isWriteIntent(cmd), acquireReadExclusive, outstanding.a_type)
  }
  def getReleaseTypeOnCacheControl(cmd: Bits): Bits = releaseVoluntaryInvalidateData // TODO
  def getReleaseTypeOnVoluntaryWriteback(): Bits = getReleaseTypeOnCacheControl(M_INV)
  def getReleaseTypeOnProbe(incoming: Probe, state: UFix): Bits = {
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

  def requiresAck(grant: Grant) = grant.g_type != grantVoluntaryAck
  def requiresAck(release: Release) = Bool(false)
  def needsSelfProbe(acq: Acquire) = Bool(false)

  def pendingVoluntaryReleaseIsSufficient(r_type: UFix, p_type: UFix): Bool = (r_type === releaseVoluntaryInvalidateData)
}

class MigratoryCoherence extends CoherencePolicyWithUncached {
  def nClientStates = 7
  def nMasterStates = 0
  def nAcquireTypes = 8
  def nProbeTypes = 4
  def nReleaseTypes = 11
  def nGrantTypes = 9

  val tileInvalid :: tileShared :: tileExclusiveClean :: tileExclusiveDirty :: tileSharedByTwo :: tileMigratoryClean :: tileMigratoryDirty :: Nil = Enum(nClientStates){ UFix() }

  val acquireReadShared :: acquireReadExclusive :: acquireReadUncached :: acquireWriteUncached :: acquireReadWordUncached :: acquireWriteWordUncached :: acquireAtomicUncached :: acquireInvalidateOthers :: Nil = Enum(nAcquireTypes){ UFix() }
  val probeInvalidate :: probeDowngrade :: probeCopy :: probeInvalidateOthers :: Nil = Enum(nProbeTypes){ UFix() }
  val releaseVoluntaryInvalidateData :: releaseInvalidateData :: releaseDowngradeData :: releaseCopyData :: releaseInvalidateAck :: releaseDowngradeAck :: releaseCopyAck :: releaseDowngradeDataMigratory :: releaseDowngradeAckHasCopy :: releaseInvalidateDataMigratory :: releaseInvalidateAckMigratory :: Nil = Enum(nReleaseTypes){ UFix() }
  val grantVoluntaryAck :: grantReadShared :: grantReadExclusive :: grantReadUncached :: grantWriteUncached :: grantReadExclusiveAck :: grantReadWordUncached :: grantWriteWordUncached :: grantAtomicUncached :: grantReadMigratory :: Nil = Enum(nGrantTypes){ UFix() }

  val uncachedAcquireTypeList = List(acquireReadUncached, acquireWriteUncached, acquireReadWordUncached, acquireWriteWordUncached, acquireAtomicUncached) 
  val hasDataAcquireTypeList = List(acquireWriteUncached, acquireWriteWordUncached, acquireAtomicUncached)
  val hasDataGrantTypeList = List(grantReadShared, grantReadExclusive, grantReadUncached, grantReadMigratory, grantReadWordUncached, grantAtomicUncached)
  val hasDataReleaseTypeList = List(releaseVoluntaryInvalidateData, releaseInvalidateData, releaseDowngradeData, releaseCopyData, releaseInvalidateDataMigratory, releaseDowngradeDataMigratory)

  def isHit (cmd: Bits, state: UFix): Bool = {
    Mux(isWriteIntent(cmd), uFixListContains(List(tileExclusiveClean, tileExclusiveDirty, tileMigratoryClean, tileMigratoryDirty), state), (state != tileInvalid))
  }
  def isValid (state: UFix): Bool = {
    state != tileInvalid
  }

  def needsTransactionOnSecondaryMiss(cmd: Bits, outstanding: Acquire): Bool = {
    (isRead(cmd) && messageIsUncached(outstanding)) ||
      (isWriteIntent(cmd) && (outstanding.a_type != acquireReadExclusive && outstanding.a_type != acquireInvalidateOthers))
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
    Mux(isWrite(cmd), MuxLookup(state, tileExclusiveDirty, Array(
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

  def getUncachedReadAcquireType = acquireReadUncached
  def getUncachedWriteAcquireType = acquireWriteUncached
  def getUncachedReadWordAcquireType = acquireReadWordUncached
  def getUncachedWriteWordAcquireType = acquireWriteWordUncached
  def getUncachedAtomicAcquireType = acquireAtomicUncached
  def isUncachedReadTransaction(acq: Acquire) = acq.a_type === acquireReadUncached
  def isVoluntary(rel: Release) = rel.r_type === releaseVoluntaryInvalidateData
  def isVoluntary(gnt: Grant) = gnt.g_type === grantVoluntaryAck

  def getAcquireTypeOnPrimaryMiss(cmd: Bits, state: UFix): UFix = {
    Mux(isWriteIntent(cmd), Mux(state === tileInvalid, acquireReadExclusive, acquireInvalidateOthers), acquireReadShared)
  }
  def getAcquireTypeOnSecondaryMiss(cmd: Bits, state: UFix, outstanding: Acquire): UFix = {
    Mux(isWriteIntent(cmd), Mux(state === tileInvalid, acquireReadExclusive, acquireInvalidateOthers), outstanding.a_type)
  }
  def getReleaseTypeOnCacheControl(cmd: Bits): Bits = releaseVoluntaryInvalidateData // TODO
  def getReleaseTypeOnVoluntaryWriteback(): Bits = getReleaseTypeOnCacheControl(M_INV)
  def getReleaseTypeOnProbe(incoming: Probe, state: UFix): Bits = {
    val with_data = MuxLookup(incoming.p_type, releaseInvalidateData, Array(
      probeInvalidate -> Mux(uFixListContains(List(tileExclusiveDirty, tileMigratoryDirty), state), 
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
  def requiresAck(grant: Grant) = grant.g_type != grantVoluntaryAck
  def requiresAck(release: Release) = Bool(false)
  def needsSelfProbe(acq: Acquire) = Bool(false)

  def pendingVoluntaryReleaseIsSufficient(r_type: UFix, p_type: UFix): Bool = (r_type === releaseVoluntaryInvalidateData)
}
