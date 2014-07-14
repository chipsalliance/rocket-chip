// See LICENSE for license details.

package uncore
import Chisel._

object MuxBundle {
  def apply[T <: Data] (default: T, mapping: Seq[(Bool, T)]): T = {
    mapping.reverse.foldLeft(default)((b, a) => Mux(a._1, a._2, b))
  }
}

abstract class CoherenceMetadata extends Bundle

object ClientMetadata {
  def apply(state: UInt)(implicit c: CoherencePolicy) = {
    val m = new ClientMetadata
    m.state := state
    m
  }
}
class ClientMetadata(implicit c: CoherencePolicy) extends CoherenceMetadata {
  val state = UInt(width = c.clientStateWidth)
  def ===(right: ClientMetadata): Bool = this.state === right.state
  override def clone = new ClientMetadata()(c).asInstanceOf[this.type]
}

object MasterMetadata {
  def apply(state: UInt)(implicit c: CoherencePolicy): MasterMetadata = {
    val m = new MasterMetadata
    m.state := state
    m.sharers.flush()
    m
  }
  def apply(state: UInt, sharers: DirectoryRepresentation)(implicit c: CoherencePolicy): MasterMetadata = {
    val m = apply(state)
    m.sharers := sharers
    m
  }
}
class MasterMetadata(implicit c: CoherencePolicy) extends CoherenceMetadata {
  val state = UInt(width = c.masterStateWidth)
  val sharers = c.dir()
  override def clone = new MasterMetadata()(c).asInstanceOf[this.type]
}
/*
class MixedMetadata(inner: CoherencePolicy, outer: CoherencePolicy) extends CoherenceMetadata {
  val cstate = UInt(width = outer.clientStateWidth)
  val mstate = UInt(width = inner.masterStateWidth)
  val sharers = inner.dir.sharers.clone
}
*/

abstract class DirectoryRepresentation extends Bundle {
  val internal: UInt
  def pop(id: UInt): DirectoryRepresentation
  def push(id: UInt): DirectoryRepresentation
  def flush(dummy: Int = 0): DirectoryRepresentation
  def none(dummy: Int = 0): Bool
  def one(dummy: Int = 0): Bool
  def count(dummy: Int = 0): UInt
  def next(dummy: Int = 0): UInt
}

class NullRepresentation extends DirectoryRepresentation {
  val internal = UInt(0)
  def pop(id: UInt) = this
  def push(id: UInt) = this
  def flush(dummy: Int = 0) = this
  def none(dummy: Int = 0) = Bool(false)
  def one(dummy: Int = 0) = Bool(false)
  def count(dummy: Int = 0) = UInt(0)
  def next(dummy: Int = 0) = UInt(0)
}

class FullRepresentation(nClients: Int) extends DirectoryRepresentation {
  val internal = UInt(width = nClients)
  def pop(id: UInt) = { internal := internal & ~UIntToOH(id); this } // make new FullRep to return?
  def push(id: UInt) = { internal := internal | UIntToOH(id); this }
  def flush(dummy: Int = 0) = { internal := UInt(0, width = nClients); this }
  def none(dummy: Int = 0) = internal === UInt(0)
  def one(dummy: Int = 0) = PopCount(internal) === UInt(1)
  def count(dummy: Int = 0) = PopCount(internal)
  def next(dummy: Int = 0) = PriorityEncoder(internal)
  override def clone = new FullRepresentation(nClients).asInstanceOf[this.type]
}

abstract class CoherencePolicy(val dir: () => DirectoryRepresentation) {
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

  def isHit (cmd: UInt, m: ClientMetadata): Bool
  def isValid (m: ClientMetadata): Bool
  def isHit (incoming: Acquire, m: MasterMetadata): Bool
  def isValid (m: MasterMetadata): Bool

  def needsTransactionOnSecondaryMiss(cmd: UInt, outstanding: Acquire): Bool
  def needsTransactionOnCacheControl(cmd: UInt, m: ClientMetadata): Bool
  def needsWriteback(m: ClientMetadata): Bool

  def clientMetadataOnHit(cmd: UInt, m: ClientMetadata): ClientMetadata
  def clientMetadataOnCacheControl(cmd: UInt): ClientMetadata 
  def clientMetadataOnFlush: ClientMetadata 
  def clientMetadataOnGrant(incoming: Grant, outstanding: Acquire): ClientMetadata 
  def clientMetadataOnProbe(incoming: Probe, m: ClientMetadata): ClientMetadata 
  def masterMetadataOnFlush: MasterMetadata 
  def masterMetadataOnRelease(incoming: Release, m: MasterMetadata, src: UInt): MasterMetadata

  def getAcquireTypeOnPrimaryMiss(cmd: UInt, m: ClientMetadata): UInt
  def getAcquireTypeOnSecondaryMiss(cmd: UInt, m: ClientMetadata, outstanding: Acquire): UInt
  def getProbeType(a: Acquire, m: MasterMetadata): UInt
  def getReleaseTypeOnCacheControl(cmd: UInt): UInt
  def getReleaseTypeOnVoluntaryWriteback(): UInt
  def getReleaseTypeOnProbe(p: Probe, m: ClientMetadata): UInt
  def getGrantType(a: Acquire, m: MasterMetadata): UInt
  def getGrantType(r: Release, m: MasterMetadata): UInt
  //def getGrantType(a: Acquire) = getGrantType(a, new NullRepresentation) // TODO
  //def getGrantType(r: Release) = getGrantType(r, new NullRepresentation)

  def messageHasData (rel: SourcedMessage): Bool
  def messageUpdatesDataArray (reply: Grant): Bool
  def messageIsUncached(acq: Acquire): Bool

  def isCoherenceConflict(addr1: UInt, addr2: UInt): Bool
  def isVoluntary(rel: Release): Bool
  def isVoluntary(gnt: Grant): Bool
  def requiresOuterRead(a_type: UInt, m: MasterMetadata): Bool
  def requiresOuterWrite(a_type: UInt, m: MasterMetadata): Bool
  def requiresOuterRead(a_type: UInt): Bool
  def requiresOuterWrite(a_type: UInt): Bool
  def requiresSelfProbe(a_type: UInt): Bool
  def requiresAckForGrant(g_type: UInt): Bool
  def requiresAckForRelease(r_type: UInt): Bool
  def pendingVoluntaryReleaseIsSufficient(r_type: UInt, p_type: UInt): Bool
}

trait UncachedTransactions {
  def getUncachedReadAcquireType: UInt
  def getUncachedWriteAcquireType: UInt
  def getUncachedReadWordAcquireType: UInt
  def getUncachedWriteWordAcquireType: UInt
  def getUncachedAtomicAcquireType: UInt
  def isUncachedReadTransaction(acq: Acquire): Bool
}

abstract class CoherencePolicyWithUncached(dir: () => DirectoryRepresentation) extends CoherencePolicy(dir)
  with UncachedTransactions

class MICoherence(dir: () => DirectoryRepresentation) extends CoherencePolicyWithUncached(dir) {
  def nClientStates = 2
  def nMasterStates = 2
  def nAcquireTypes = 6
  def nProbeTypes = 2
  def nReleaseTypes = 5
  def nGrantTypes = 7

  val clientInvalid :: clientValid :: Nil = Enum(UInt(), nClientStates)
  val masterInvalid :: masterValid :: Nil = Enum(UInt(), nMasterStates)

  val acquireReadExclusive :: acquireReadUncached :: acquireWriteUncached :: acquireReadWordUncached :: acquireWriteWordUncached :: acquireAtomicUncached :: Nil = Enum(UInt(), nAcquireTypes)
  val probeInvalidate :: probeCopy :: Nil = Enum(UInt(), nProbeTypes)
  val releaseVoluntaryInvalidateData :: releaseInvalidateData :: releaseCopyData :: releaseInvalidateAck :: releaseCopyAck :: Nil = Enum(UInt(), nReleaseTypes)
  val grantVoluntaryAck :: grantReadExclusive :: grantReadUncached :: grantWriteUncached :: grantReadWordUncached :: grantWriteWordUncached :: grantAtomicUncached :: Nil = Enum(UInt(), nGrantTypes)

  val uncachedAcquireTypeVec = Vec(acquireReadUncached, acquireWriteUncached, acquireReadWordUncached, acquireWriteWordUncached, acquireAtomicUncached) 
  val hasDataAcquireTypeVec = Vec(acquireWriteUncached, acquireWriteWordUncached, acquireAtomicUncached)
  val hasDataReleaseTypeVec = Vec(releaseVoluntaryInvalidateData, releaseInvalidateData, releaseCopyData)
  val hasDataGrantTypeVec = Vec(grantReadExclusive, grantReadUncached, grantReadWordUncached, grantAtomicUncached)

  def isHit (cmd: UInt, m: ClientMetadata): Bool = isValid(m)
  def isValid (m: ClientMetadata): Bool = m.state != clientInvalid
  def isHit (incoming: Acquire, m: MasterMetadata): Bool = isValid(m)
  def isValid (m: MasterMetadata): Bool = m.state != masterInvalid

  def needsTransactionOnSecondaryMiss(cmd: UInt, outstanding: Acquire): Bool = (outstanding.a_type != acquireReadExclusive)
  def needsTransactionOnCacheControl(cmd: UInt, m: ClientMetadata): Bool = {
    MuxLookup(cmd, (m.state === clientValid), Array(
      M_INV -> (m.state === clientValid),
      M_CLN -> (m.state === clientValid)
    ))
  }
  def needsWriteback (m: ClientMetadata): Bool = {
    needsTransactionOnCacheControl(M_INV, m)
  }

  def clientMetadataOnHit(cmd: UInt, m: ClientMetadata) = m
  def clientMetadataOnCacheControl(cmd: UInt) = ClientMetadata(
    MuxLookup(cmd, clientInvalid, Array(
      M_INV -> clientInvalid,
      M_CLN -> clientValid
    )))(this)
  def clientMetadataOnFlush = clientMetadataOnCacheControl(M_INV)
  def clientMetadataOnGrant(incoming: Grant, outstanding: Acquire) = ClientMetadata(
    MuxLookup(incoming.g_type, clientInvalid, Array(
      grantReadExclusive -> clientValid,
      grantReadUncached  -> clientInvalid,
      grantWriteUncached -> clientInvalid,
      grantReadWordUncached -> clientInvalid,
      grantWriteWordUncached -> clientInvalid,
      grantAtomicUncached -> clientInvalid
    )))(this)
  def clientMetadataOnProbe(incoming: Probe, m: ClientMetadata) = ClientMetadata(
    MuxLookup(incoming.p_type, m.state, Array(
      probeInvalidate -> clientInvalid,
      probeCopy       -> m.state
    )))(this)
  def masterMetadataOnFlush = MasterMetadata(masterInvalid)(this)
  def masterMetadataOnRelease(incoming: Release, m: MasterMetadata, src: UInt) = {
    val popped = m.sharers.pop(src)
    val next = MasterMetadata(Mux(popped.none(), masterInvalid, masterValid), popped)(this)
    def is(r: UInt) = incoming.r_type === r
    MuxBundle(m, Array(
      is(releaseVoluntaryInvalidateData) -> next,
      is(releaseInvalidateData) -> next,
      is(releaseCopyData) -> m,
      is(releaseInvalidateAck) -> next,
      is(releaseCopyAck) -> m
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

  def getAcquireTypeOnPrimaryMiss(cmd: UInt, m: ClientMetadata): UInt = acquireReadExclusive
  def getAcquireTypeOnSecondaryMiss(cmd: UInt, m: ClientMetadata, outstanding: Acquire): UInt = acquireReadExclusive
  def getReleaseTypeOnCacheControl(cmd: UInt): UInt = releaseVoluntaryInvalidateData // TODO
  def getReleaseTypeOnVoluntaryWriteback(): UInt = getReleaseTypeOnCacheControl(M_INV)
  def getReleaseTypeOnProbe(incoming: Probe, m: ClientMetadata): UInt = {
    val with_data = MuxLookup(incoming.p_type, releaseInvalidateData, Array(
      probeInvalidate -> releaseInvalidateData,
      probeCopy       -> releaseCopyData
    ))
    val without_data = MuxLookup(incoming.p_type, releaseInvalidateAck, Array(
      probeInvalidate -> releaseInvalidateAck,
      probeCopy       -> releaseCopyAck
    ))
    Mux(needsWriteback(m), with_data, without_data)
  }

  def messageHasData(msg: SourcedMessage) = msg match {
    case acq: Acquire => hasDataAcquireTypeVec.contains(acq.a_type)
    case grant: Grant => hasDataGrantTypeVec.contains(grant.g_type) 
    case rel: Release => hasDataReleaseTypeVec.contains(rel.r_type) 
    case _ => Bool(false)
  }
  def messageUpdatesDataArray (reply: Grant): Bool = {
    (reply.g_type === grantReadExclusive)
  }
  def messageIsUncached(acq: Acquire): Bool = uncachedAcquireTypeVec.contains(acq.a_type)

  def isCoherenceConflict(addr1: UInt, addr2: UInt): Bool = (addr1 === addr2)

  def getGrantType(a: Acquire, m: MasterMetadata): UInt = {
    MuxLookup(a.a_type, grantReadUncached, Array(
      acquireReadExclusive -> grantReadExclusive,
      acquireReadUncached  -> grantReadUncached,
      acquireWriteUncached -> grantWriteUncached,
      acquireReadWordUncached  -> grantReadWordUncached,
      acquireWriteWordUncached -> grantWriteWordUncached,
      acquireAtomicUncached -> grantAtomicUncached
    ))
  }

  def getGrantType(r: Release, m: MasterMetadata): UInt = {
    MuxLookup(r.r_type, grantReadUncached, Array(
      releaseVoluntaryInvalidateData -> grantVoluntaryAck
    ))
  }

  def getProbeType(a: Acquire, m: MasterMetadata): UInt = {
    MuxLookup(a.a_type, probeCopy, Array(
      acquireReadExclusive -> probeInvalidate, 
      acquireReadUncached -> probeCopy, 
      acquireWriteUncached -> probeInvalidate,
      acquireReadWordUncached -> probeCopy, 
      acquireWriteWordUncached -> probeInvalidate,
      acquireAtomicUncached -> probeInvalidate
    ))
  }

  def requiresOuterRead(a_type: UInt) = {
      (a_type != acquireWriteUncached)
  }
  def requiresOuterWrite(a_type: UInt) = {
      (a_type === acquireWriteUncached)
  }
  def requiresOuterRead(a_type: UInt, m: MasterMetadata) = requiresOuterRead(a_type)
  def requiresOuterWrite(a_type: UInt, m: MasterMetadata) = requiresOuterWrite(a_type)
  def requiresAckForGrant(g_type: UInt) = g_type != grantVoluntaryAck
  def requiresAckForRelease(r_type: UInt) = Bool(false)
  def requiresSelfProbe(a_type: UInt) = Bool(false)
  def pendingVoluntaryReleaseIsSufficient(r_type: UInt, p_type: UInt): Bool = (r_type === releaseVoluntaryInvalidateData)
}

class MEICoherence(dir: () => DirectoryRepresentation) extends CoherencePolicyWithUncached(dir) {
  def nClientStates = 3
  def nMasterStates = 2
  def nAcquireTypes = 7
  def nProbeTypes = 3
  def nReleaseTypes = 7
  def nGrantTypes = 8

  val clientInvalid :: clientExclusiveClean :: clientExclusiveDirty :: Nil = Enum(UInt(), nClientStates)
  val masterInvalid :: masterValid :: Nil = Enum(UInt(), nMasterStates)

  val acquireReadExclusiveClean :: acquireReadExclusiveDirty :: acquireReadUncached :: acquireWriteUncached :: acquireReadWordUncached :: acquireWriteWordUncached :: acquireAtomicUncached :: Nil = Enum(UInt(), nAcquireTypes)
  val probeInvalidate :: probeDowngrade :: probeCopy :: Nil = Enum(UInt(), nProbeTypes)
  val releaseVoluntaryInvalidateData :: releaseInvalidateData :: releaseDowngradeData :: releaseCopyData :: releaseInvalidateAck :: releaseDowngradeAck :: releaseCopyAck :: Nil = Enum(UInt(), nReleaseTypes)
  val grantVoluntaryAck :: grantReadExclusive :: grantReadUncached :: grantWriteUncached :: grantReadExclusiveAck :: grantReadWordUncached :: grantWriteWordUncached :: grantAtomicUncached :: Nil = Enum(UInt(), nGrantTypes)

  val uncachedAcquireTypeVec = Vec(acquireReadUncached, acquireWriteUncached, acquireReadWordUncached, acquireWriteWordUncached, acquireAtomicUncached) 
  val hasDataAcquireTypeVec = Vec(acquireWriteUncached, acquireWriteWordUncached, acquireAtomicUncached) 
  val hasDataReleaseTypeVec = Vec(releaseVoluntaryInvalidateData, releaseInvalidateData, releaseDowngradeData, releaseCopyData)
  val hasDataGrantTypeVec = Vec(grantReadExclusive, grantReadUncached, grantReadWordUncached, grantAtomicUncached)

  def isHit (cmd: UInt, m: ClientMetadata) = isValid(m)
  def isValid (m: ClientMetadata) = m.state != clientInvalid
  def isHit (incoming: Acquire, m: MasterMetadata) = isValid(m)
  def isValid (m: MasterMetadata) = m.state != masterInvalid

  def needsTransactionOnSecondaryMiss(cmd: UInt, outstanding: Acquire): Bool = {
    (isRead(cmd) && messageIsUncached(outstanding)) ||
      (isWriteIntent(cmd) && (outstanding.a_type != acquireReadExclusiveDirty))
  }
  def needsTransactionOnCacheControl(cmd: UInt, m: ClientMetadata): Bool = {
    MuxLookup(cmd, (m.state === clientExclusiveDirty), Array(
      M_INV -> (m.state === clientExclusiveDirty),
      M_CLN -> (m.state === clientExclusiveDirty)
    ))
  }
  def needsWriteback (m: ClientMetadata): Bool = {
    needsTransactionOnCacheControl(M_INV, m)
  }

  def clientMetadataOnHit(cmd: UInt, m: ClientMetadata) = 
    ClientMetadata(Mux(isWrite(cmd), clientExclusiveDirty, m.state))(this)
  
  def clientMetadataOnCacheControl(cmd: UInt) = ClientMetadata(
    MuxLookup(cmd, clientInvalid, Array(
      M_INV -> clientInvalid,
      M_CLN -> clientExclusiveClean
    )))(this)
  def clientMetadataOnFlush() = clientMetadataOnCacheControl(M_INV)
  def clientMetadataOnGrant(incoming: Grant, outstanding: Acquire) = ClientMetadata(
    MuxLookup(incoming.g_type, clientInvalid, Array(
      grantReadExclusive  -> Mux(outstanding.a_type === acquireReadExclusiveDirty, 
        clientExclusiveDirty, clientExclusiveClean),
      grantReadExclusiveAck -> clientExclusiveDirty, 
      grantReadUncached -> clientInvalid,
      grantWriteUncached -> clientInvalid,
      grantReadWordUncached -> clientInvalid,
      grantWriteWordUncached -> clientInvalid,
      grantAtomicUncached -> clientInvalid
    )))(this)
  def clientMetadataOnProbe(incoming: Probe, m: ClientMetadata) = ClientMetadata(
    MuxLookup(incoming.p_type, m.state, Array(
      probeInvalidate -> clientInvalid,
      probeDowngrade  -> clientExclusiveClean,
      probeCopy       -> m.state
    )))(this)
  def masterMetadataOnFlush = MasterMetadata(masterInvalid)(this)
  def masterMetadataOnRelease(incoming: Release, m: MasterMetadata, src: UInt) = {
    val popped = m.sharers.pop(src)
    val next = MasterMetadata(Mux(popped.none(), masterInvalid, masterValid), popped)(this)
    def is(r: UInt) = incoming.r_type === r
    MuxBundle(m, Array(
      is(releaseVoluntaryInvalidateData) -> next,
      is(releaseInvalidateData) -> next,
      is(releaseDowngradeData) -> m,
      is(releaseCopyData) -> m,
      is(releaseInvalidateAck) -> next,
      is(releaseDowngradeAck) -> m,
      is(releaseCopyAck) -> m
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

  def getAcquireTypeOnPrimaryMiss(cmd: UInt, m: ClientMetadata): UInt = {
    Mux(isWriteIntent(cmd), acquireReadExclusiveDirty, acquireReadExclusiveClean)
  }
  def getAcquireTypeOnSecondaryMiss(cmd: UInt, m: ClientMetadata, outstanding: Acquire): UInt = {
    Mux(isWriteIntent(cmd), acquireReadExclusiveDirty, outstanding.a_type)
  }
  def getReleaseTypeOnCacheControl(cmd: UInt): UInt = releaseVoluntaryInvalidateData // TODO
  def getReleaseTypeOnVoluntaryWriteback(): UInt = getReleaseTypeOnCacheControl(M_INV)
  def getReleaseTypeOnProbe(incoming: Probe, m: ClientMetadata): UInt = {
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
    Mux(needsWriteback(m), with_data, without_data)
  }

  def messageHasData(msg: SourcedMessage) = msg match {
    case acq: Acquire => hasDataAcquireTypeVec.contains(acq.a_type)
    case grant: Grant => hasDataGrantTypeVec.contains(grant.g_type) 
    case rel: Release => hasDataReleaseTypeVec.contains(rel.r_type) 
    case _ => Bool(false)
  }
  def messageUpdatesDataArray (reply: Grant): Bool = {
    (reply.g_type === grantReadExclusive)
  }
  def messageIsUncached(acq: Acquire): Bool = uncachedAcquireTypeVec.contains(acq.a_type)

  def isCoherenceConflict(addr1: UInt, addr2: UInt): Bool = (addr1 === addr2)

  def getGrantType(a: Acquire, m: MasterMetadata): UInt = {
    MuxLookup(a.a_type, grantReadUncached, Array(
      acquireReadExclusiveClean -> grantReadExclusive,
      acquireReadExclusiveDirty -> grantReadExclusive,
      acquireReadUncached  -> grantReadUncached,
      acquireWriteUncached -> grantWriteUncached,
      acquireReadWordUncached  -> grantReadWordUncached,
      acquireWriteWordUncached -> grantWriteWordUncached,
      acquireAtomicUncached -> grantAtomicUncached
    ))
  }
  def getGrantType(r: Release, m: MasterMetadata): UInt = {
    MuxLookup(r.r_type, grantReadUncached, Array(
      releaseVoluntaryInvalidateData -> grantVoluntaryAck
    ))
  }


  def getProbeType(a: Acquire, m: MasterMetadata): UInt = {
    MuxLookup(a.a_type, probeCopy, Array(
      acquireReadExclusiveClean -> probeInvalidate,
      acquireReadExclusiveDirty -> probeInvalidate, 
      acquireReadUncached -> probeCopy, 
      acquireWriteUncached -> probeInvalidate,
      acquireReadWordUncached -> probeCopy, 
      acquireWriteWordUncached -> probeInvalidate,
      acquireAtomicUncached -> probeInvalidate
    ))
  }

  def requiresOuterRead(a_type: UInt) = {
      (a_type != acquireWriteUncached)
  }
  def requiresOuterWrite(a_type: UInt) = {
      (a_type === acquireWriteUncached)
  }
  def requiresOuterRead(a_type: UInt, m: MasterMetadata) = requiresOuterRead(a_type)
  def requiresOuterWrite(a_type: UInt, m: MasterMetadata) = requiresOuterWrite(a_type)
  def requiresAckForGrant(g_type: UInt) = g_type != grantVoluntaryAck
  def requiresAckForRelease(r_type: UInt) = Bool(false)
  def requiresSelfProbe(a_type: UInt) = Bool(false)

  def pendingVoluntaryReleaseIsSufficient(r_type: UInt, p_type: UInt): Bool = (r_type === releaseVoluntaryInvalidateData)
}

class MSICoherence(dir: () => DirectoryRepresentation) extends CoherencePolicyWithUncached(dir) {
  def nClientStates = 3
  def nMasterStates = 3
  def nAcquireTypes = 7
  def nProbeTypes = 3
  def nReleaseTypes = 7
  def nGrantTypes = 9

  val clientInvalid :: clientShared :: clientExclusiveDirty :: Nil = Enum(UInt(), nClientStates)
  val masterInvalid :: masterShared :: masterExclusive :: Nil = Enum(UInt(), nMasterStates)

  val acquireReadShared :: acquireReadExclusive :: acquireReadUncached :: acquireWriteUncached :: acquireReadWordUncached :: acquireWriteWordUncached :: acquireAtomicUncached :: Nil = Enum(UInt(), nAcquireTypes)
  val probeInvalidate :: probeDowngrade :: probeCopy :: Nil = Enum(UInt(), nProbeTypes)
  val releaseVoluntaryInvalidateData :: releaseInvalidateData :: releaseDowngradeData :: releaseCopyData :: releaseInvalidateAck :: releaseDowngradeAck :: releaseCopyAck :: Nil = Enum(UInt(), nReleaseTypes)
  val grantVoluntaryAck :: grantReadShared :: grantReadExclusive :: grantReadUncached :: grantWriteUncached :: grantReadExclusiveAck :: grantReadWordUncached :: grantWriteWordUncached :: grantAtomicUncached :: Nil = Enum(UInt(), nGrantTypes)

  val uncachedAcquireTypeVec = Vec(acquireReadUncached, acquireWriteUncached, acquireReadWordUncached, acquireWriteWordUncached, acquireAtomicUncached) 
  val hasDataAcquireTypeVec = Vec(acquireWriteUncached, acquireWriteWordUncached, acquireAtomicUncached)
  val hasDataReleaseTypeVec = Vec(releaseVoluntaryInvalidateData, releaseInvalidateData, releaseDowngradeData, releaseCopyData)
  val hasDataGrantTypeVec = Vec(grantReadShared, grantReadExclusive, grantReadUncached, grantReadWordUncached, grantAtomicUncached)

  def isHit (cmd: UInt, m: ClientMetadata): Bool = {
    Mux(isWriteIntent(cmd), (m.state === clientExclusiveDirty),
        (m.state === clientShared || m.state === clientExclusiveDirty))
  }
  def isValid (m: ClientMetadata): Bool = {
    m.state != clientInvalid
  }
  def isHit (incoming: Acquire, m: MasterMetadata) = isValid(m)
  def isValid (m: MasterMetadata) = m.state != masterInvalid

  def needsTransactionOnSecondaryMiss(cmd: UInt, outstanding: Acquire): Bool = {
    (isRead(cmd) && messageIsUncached(outstanding)) || 
      (isWriteIntent(cmd) && (outstanding.a_type != acquireReadExclusive))
  }
  def needsTransactionOnCacheControl(cmd: UInt, m: ClientMetadata): Bool = {
    MuxLookup(cmd, (m.state === clientExclusiveDirty), Array(
      M_INV -> (m.state === clientExclusiveDirty),
      M_CLN -> (m.state === clientExclusiveDirty)
    ))
  }
  def needsWriteback (m: ClientMetadata): Bool = {
    needsTransactionOnCacheControl(M_INV, m)
  }

  def clientMetadataOnHit(cmd: UInt, m: ClientMetadata) =
    ClientMetadata(Mux(isWrite(cmd), clientExclusiveDirty, m.state))(this)
  def clientMetadataOnCacheControl(cmd: UInt) = ClientMetadata(
    MuxLookup(cmd, clientInvalid, Array(
      M_INV -> clientInvalid,
      M_CLN -> clientShared
    )))(this)
  def clientMetadataOnFlush() = clientMetadataOnCacheControl(M_INV)
  def clientMetadataOnGrant(incoming: Grant, outstanding: Acquire) = ClientMetadata(
    MuxLookup(incoming.g_type, clientInvalid, Array(
      grantReadShared -> clientShared,
      grantReadExclusive  -> clientExclusiveDirty,
      grantReadExclusiveAck -> clientExclusiveDirty, 
      grantReadUncached -> clientInvalid,
      grantWriteUncached -> clientInvalid,
      grantReadWordUncached -> clientInvalid,
      grantWriteWordUncached -> clientInvalid,
      grantAtomicUncached -> clientInvalid
    )))(this)
  def clientMetadataOnProbe(incoming: Probe, m: ClientMetadata) = ClientMetadata(
    MuxLookup(incoming.p_type, m.state, Array(
      probeInvalidate -> clientInvalid,
      probeDowngrade  -> clientShared,
      probeCopy       -> m.state
    )))(this)
  def masterMetadataOnFlush = MasterMetadata(masterInvalid)(this)
  def masterMetadataOnRelease(incoming: Release, m: MasterMetadata, src: UInt) = {
    val popped = m.sharers.pop(src)
    val next = MasterMetadata(
                Mux(popped.none(), masterInvalid, 
                  Mux(popped.one(), masterExclusive, masterShared)), popped)(this)
    def is(r: UInt) = incoming.r_type === r
    MuxBundle(m, Array(
      is(releaseVoluntaryInvalidateData) -> next,
      is(releaseInvalidateData) -> next,
      is(releaseDowngradeData) -> m,
      is(releaseCopyData) -> m,
      is(releaseInvalidateAck) -> next,
      is(releaseDowngradeAck) -> m,
      is(releaseCopyAck) -> m
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

  def getAcquireTypeOnPrimaryMiss(cmd: UInt, m: ClientMetadata): UInt = {
    Mux(isWriteIntent(cmd), acquireReadExclusive, acquireReadShared)
  }
  def getAcquireTypeOnSecondaryMiss(cmd: UInt, m: ClientMetadata, outstanding: Acquire): UInt = {
    Mux(isWriteIntent(cmd), acquireReadExclusive, outstanding.a_type)
  }
  def getReleaseTypeOnCacheControl(cmd: UInt): UInt = releaseVoluntaryInvalidateData // TODO
  def getReleaseTypeOnVoluntaryWriteback(): UInt = getReleaseTypeOnCacheControl(M_INV)
  def getReleaseTypeOnProbe(incoming: Probe, m: ClientMetadata): UInt = {
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
    Mux(needsWriteback(m), with_data, without_data)
  }

  def messageHasData(msg: SourcedMessage) = msg match {
    case acq: Acquire => hasDataAcquireTypeVec.contains(acq.a_type)
    case grant: Grant => hasDataGrantTypeVec.contains(grant.g_type) 
    case rel: Release => hasDataReleaseTypeVec.contains(rel.r_type) 
    case _ => Bool(false)
  }
  def messageUpdatesDataArray (reply: Grant): Bool = {
    (reply.g_type === grantReadShared || reply.g_type === grantReadExclusive)
  }
  def messageIsUncached(acq: Acquire): Bool = uncachedAcquireTypeVec.contains(acq.a_type)

  def isCoherenceConflict(addr1: UInt, addr2: UInt): Bool = (addr1 === addr2)

  def getGrantType(a: Acquire, m: MasterMetadata): UInt = {
    MuxLookup(a.a_type, grantReadUncached, Array(
      acquireReadShared    -> Mux(m.sharers.count() > UInt(0), grantReadShared, grantReadExclusive),
      acquireReadExclusive -> grantReadExclusive,
      acquireReadUncached  -> grantReadUncached,
      acquireWriteUncached -> grantWriteUncached,
      acquireReadWordUncached  -> grantReadWordUncached,
      acquireWriteWordUncached -> grantWriteWordUncached,
      acquireAtomicUncached -> grantAtomicUncached
    ))
  }
  def getGrantType(r: Release, m: MasterMetadata): UInt = {
    MuxLookup(r.r_type, grantReadUncached, Array(
      releaseVoluntaryInvalidateData -> grantVoluntaryAck
    ))
  }

  def getProbeType(a: Acquire, m: MasterMetadata): UInt = {
    MuxLookup(a.a_type, probeCopy, Array(
      acquireReadShared -> probeDowngrade,
      acquireReadExclusive -> probeInvalidate, 
      acquireReadUncached -> probeCopy, 
      acquireWriteUncached -> probeInvalidate
    ))
  }

  def requiresOuterRead(a_type: UInt) = {
      (a_type != acquireWriteUncached)
  }
  def requiresOuterWrite(a_type: UInt) = {
      (a_type === acquireWriteUncached)
  }
  def requiresOuterRead(a_type: UInt, m: MasterMetadata) = requiresOuterRead(a_type)
  def requiresOuterWrite(a_type: UInt, m: MasterMetadata) = requiresOuterWrite(a_type)
  def requiresAckForGrant(g_type: UInt) = g_type != grantVoluntaryAck
  def requiresAckForRelease(r_type: UInt) = Bool(false)
  def requiresSelfProbe(a_type: UInt) = Bool(false)

  def pendingVoluntaryReleaseIsSufficient(r_type: UInt, p_type: UInt): Bool = (r_type === releaseVoluntaryInvalidateData)
}

class MESICoherence(dir: () => DirectoryRepresentation) extends CoherencePolicyWithUncached(dir) {
  def nClientStates = 4
  def nMasterStates = 3
  def nAcquireTypes = 7
  def nProbeTypes = 3
  def nReleaseTypes = 7
  def nGrantTypes = 9

  val clientInvalid :: clientShared :: clientExclusiveClean :: clientExclusiveDirty :: Nil = Enum(UInt(), nClientStates)
  val masterInvalid :: masterShared :: masterExclusive :: Nil = Enum(UInt(), nMasterStates)

  val acquireReadShared :: acquireReadExclusive :: acquireReadUncached :: acquireWriteUncached :: acquireReadWordUncached :: acquireWriteWordUncached :: acquireAtomicUncached :: Nil = Enum(UInt(), nAcquireTypes)
  val probeInvalidate :: probeDowngrade :: probeCopy :: Nil = Enum(UInt(), nProbeTypes)
  val releaseVoluntaryInvalidateData :: releaseInvalidateData :: releaseDowngradeData :: releaseCopyData :: releaseInvalidateAck :: releaseDowngradeAck :: releaseCopyAck :: Nil = Enum(UInt(), nReleaseTypes)
  val grantVoluntaryAck :: grantReadShared :: grantReadExclusive :: grantReadUncached :: grantWriteUncached :: grantReadExclusiveAck :: grantReadWordUncached :: grantWriteWordUncached :: grantAtomicUncached :: Nil = Enum(UInt(), nGrantTypes)

  val uncachedAcquireTypeVec = Vec(acquireReadUncached, acquireWriteUncached, acquireReadWordUncached, acquireWriteWordUncached, acquireAtomicUncached) 
  val hasDataAcquireTypeVec = Vec(acquireWriteUncached, acquireWriteWordUncached, acquireAtomicUncached) 
  val hasDataReleaseTypeVec = Vec(releaseVoluntaryInvalidateData, releaseInvalidateData, releaseDowngradeData, releaseCopyData)
  val hasDataGrantTypeVec = Vec(grantReadShared, grantReadExclusive, grantReadUncached, grantReadWordUncached, grantAtomicUncached)

  def isHit (cmd: UInt, m: ClientMetadata): Bool = {
    Mux(isWriteIntent(cmd), (m.state === clientExclusiveClean || m.state === clientExclusiveDirty),
        (m.state === clientShared || m.state === clientExclusiveClean || m.state === clientExclusiveDirty))
  }
  def isValid (m: ClientMetadata): Bool = {
    m.state != clientInvalid
  }
  def isHit (incoming: Acquire, m: MasterMetadata) = isValid(m)
  def isValid (m: MasterMetadata) = m.state != masterInvalid

  def needsTransactionOnSecondaryMiss(cmd: UInt, outstanding: Acquire): Bool = {
    (isRead(cmd) && messageIsUncached(outstanding)) ||
      (isWriteIntent(cmd) && (outstanding.a_type != acquireReadExclusive))
  }
  def needsTransactionOnCacheControl(cmd: UInt, m: ClientMetadata): Bool = {
    MuxLookup(cmd, (m.state === clientExclusiveDirty), Array(
      M_INV -> (m.state === clientExclusiveDirty),
      M_CLN -> (m.state === clientExclusiveDirty)
    ))
  }
  def needsWriteback (m: ClientMetadata): Bool = {
    needsTransactionOnCacheControl(M_INV, m)
  }

  def clientMetadataOnHit(cmd: UInt, m: ClientMetadata) = 
    ClientMetadata(Mux(isWrite(cmd), clientExclusiveDirty, m.state))(this)

  def clientMetadataOnCacheControl(cmd: UInt) = ClientMetadata(
    MuxLookup(cmd, clientInvalid, Array(
      M_INV -> clientInvalid,
      M_CLN -> clientShared
    )))(this)
  def clientMetadataOnFlush = clientMetadataOnCacheControl(M_INV)
  def clientMetadataOnGrant(incoming: Grant, outstanding: Acquire) = ClientMetadata(
    MuxLookup(incoming.g_type, clientInvalid, Array(
      grantReadShared -> clientShared,
      grantReadExclusive  -> Mux(outstanding.a_type === acquireReadExclusive, clientExclusiveDirty, clientExclusiveClean),
      grantReadExclusiveAck -> clientExclusiveDirty, 
      grantReadUncached -> clientInvalid,
      grantWriteUncached -> clientInvalid,
      grantReadWordUncached -> clientInvalid,
      grantWriteWordUncached -> clientInvalid,
      grantAtomicUncached -> clientInvalid
    )))(this)
  def clientMetadataOnProbe(incoming: Probe, m: ClientMetadata) = ClientMetadata(
    MuxLookup(incoming.p_type, m.state, Array(
      probeInvalidate -> clientInvalid,
      probeDowngrade  -> clientShared,
      probeCopy       -> m.state
    )))(this)
  def masterMetadataOnFlush = MasterMetadata(masterInvalid)(this)
  def masterMetadataOnRelease(incoming: Release, m: MasterMetadata, src: UInt) = {
    val popped = m.sharers.pop(src)
    val next = MasterMetadata(
                Mux(popped.none(), masterInvalid, 
                  Mux(popped.one(), masterExclusive, masterShared)), popped)(this)
    def is(r: UInt) = incoming.r_type === r
    MuxBundle(m, Array(
      is(releaseVoluntaryInvalidateData) -> next,
      is(releaseInvalidateData) -> next,
      is(releaseDowngradeData) -> m,
      is(releaseCopyData) -> m,
      is(releaseInvalidateAck) -> next,
      is(releaseDowngradeAck) -> m,
      is(releaseCopyAck) -> m
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

  def getAcquireTypeOnPrimaryMiss(cmd: UInt, m: ClientMetadata): UInt = {
    Mux(isWriteIntent(cmd), acquireReadExclusive, acquireReadShared)
  }
  def getAcquireTypeOnSecondaryMiss(cmd: UInt, m: ClientMetadata, outstanding: Acquire): UInt = {
    Mux(isWriteIntent(cmd), acquireReadExclusive, outstanding.a_type)
  }
  def getReleaseTypeOnCacheControl(cmd: UInt): UInt = releaseVoluntaryInvalidateData // TODO
  def getReleaseTypeOnVoluntaryWriteback(): UInt = getReleaseTypeOnCacheControl(M_INV)
  def getReleaseTypeOnProbe(incoming: Probe, m: ClientMetadata): UInt = {
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
    Mux(needsWriteback(m), with_data, without_data)
  }

  def messageHasData(msg: SourcedMessage) = msg match {
    case acq: Acquire => hasDataAcquireTypeVec.contains(acq.a_type)
    case grant: Grant => hasDataGrantTypeVec.contains(grant.g_type) 
    case rel: Release => hasDataReleaseTypeVec.contains(rel.r_type) 
    case _ => Bool(false)
  }
  def messageUpdatesDataArray (reply: Grant): Bool = {
    (reply.g_type === grantReadShared || reply.g_type === grantReadExclusive)
  }
  def messageIsUncached(acq: Acquire): Bool = uncachedAcquireTypeVec.contains(acq.a_type)

  def isCoherenceConflict(addr1: UInt, addr2: UInt): Bool = (addr1 === addr2)

  def getGrantType(a: Acquire, m: MasterMetadata): UInt = {
    MuxLookup(a.a_type, grantReadUncached, Array(
      acquireReadShared    -> Mux(m.sharers.count() > UInt(0), grantReadShared, grantReadExclusive),
      acquireReadExclusive -> grantReadExclusive,
      acquireReadUncached  -> grantReadUncached,
      acquireWriteUncached -> grantWriteUncached,
      acquireReadWordUncached  -> grantReadWordUncached,
      acquireWriteWordUncached -> grantWriteWordUncached,
      acquireAtomicUncached -> grantAtomicUncached
    ))
  }
  def getGrantType(r: Release, m: MasterMetadata): UInt = {
    MuxLookup(r.r_type, grantReadUncached, Array(
      releaseVoluntaryInvalidateData -> grantVoluntaryAck
    ))
  }


  def getProbeType(a: Acquire, m: MasterMetadata): UInt = {
    MuxLookup(a.a_type, probeCopy, Array(
      acquireReadShared -> probeDowngrade,
      acquireReadExclusive -> probeInvalidate, 
      acquireReadUncached -> probeCopy, 
      acquireWriteUncached -> probeInvalidate,
      acquireReadWordUncached -> probeCopy, 
      acquireWriteWordUncached -> probeInvalidate,
      acquireAtomicUncached -> probeInvalidate
    ))
  }

  def requiresOuterRead(a_type: UInt) = {
      (a_type != acquireWriteUncached)
  }
  def requiresOuterWrite(a_type: UInt) = {
      (a_type === acquireWriteUncached)
  }
  def requiresOuterRead(a_type: UInt, m: MasterMetadata) = requiresOuterRead(a_type)
  def requiresOuterWrite(a_type: UInt, m: MasterMetadata) = requiresOuterWrite(a_type)

  def requiresAckForGrant(g_type: UInt) = g_type != grantVoluntaryAck
  def requiresAckForRelease(r_type: UInt) = Bool(false)
  def requiresSelfProbe(a_type: UInt) = Bool(false)

  def pendingVoluntaryReleaseIsSufficient(r_type: UInt, p_type: UInt): Bool = (r_type === releaseVoluntaryInvalidateData)
}

class MigratoryCoherence(dir: () => DirectoryRepresentation) extends CoherencePolicyWithUncached(dir) {
  def nClientStates = 7
  def nMasterStates = 3
  def nAcquireTypes = 8
  def nProbeTypes = 4
  def nReleaseTypes = 11
  def nGrantTypes = 9

  val clientInvalid :: clientShared :: clientExclusiveClean :: clientExclusiveDirty :: clientSharedByTwo :: clientMigratoryClean :: clientMigratoryDirty :: Nil = Enum(UInt(), nClientStates)
  val masterInvalid :: masterShared :: masterExclusive :: Nil = Enum(UInt(), nMasterStates)

  val acquireReadShared :: acquireReadExclusive :: acquireReadUncached :: acquireWriteUncached :: acquireReadWordUncached :: acquireWriteWordUncached :: acquireAtomicUncached :: acquireInvalidateOthers :: Nil = Enum(UInt(), nAcquireTypes)
  val probeInvalidate :: probeDowngrade :: probeCopy :: probeInvalidateOthers :: Nil = Enum(UInt(), nProbeTypes)
  val releaseVoluntaryInvalidateData :: releaseInvalidateData :: releaseDowngradeData :: releaseCopyData :: releaseInvalidateAck :: releaseDowngradeAck :: releaseCopyAck :: releaseDowngradeDataMigratory :: releaseDowngradeAckHasCopy :: releaseInvalidateDataMigratory :: releaseInvalidateAckMigratory :: Nil = Enum(UInt(), nReleaseTypes)
  val grantVoluntaryAck :: grantReadShared :: grantReadExclusive :: grantReadUncached :: grantWriteUncached :: grantReadExclusiveAck :: grantReadWordUncached :: grantWriteWordUncached :: grantAtomicUncached :: grantReadMigratory :: Nil = Enum(UInt(), nGrantTypes)

  val uncachedAcquireTypeVec = Vec(acquireReadUncached, acquireWriteUncached, acquireReadWordUncached, acquireWriteWordUncached, acquireAtomicUncached) 
  val hasDataAcquireTypeVec = Vec(acquireWriteUncached, acquireWriteWordUncached, acquireAtomicUncached)
  val hasDataGrantTypeVec = Vec(grantReadShared, grantReadExclusive, grantReadUncached, grantReadMigratory, grantReadWordUncached, grantAtomicUncached)
  val hasDataReleaseTypeVec = Vec(releaseVoluntaryInvalidateData, releaseInvalidateData, releaseDowngradeData, releaseCopyData, releaseInvalidateDataMigratory, releaseDowngradeDataMigratory)

  def isHit (cmd: UInt, m: ClientMetadata): Bool = {
    Mux(isWriteIntent(cmd), Vec(clientExclusiveClean, clientExclusiveDirty, clientMigratoryClean, clientMigratoryDirty).contains(m.state), (m.state != clientInvalid))
  }
  def isValid (m: ClientMetadata): Bool = {
    m.state != clientInvalid
  }
  def isHit (incoming: Acquire, m: MasterMetadata) = isValid(m)
  def isValid (m: MasterMetadata) = m.state != masterInvalid

  def needsTransactionOnSecondaryMiss(cmd: UInt, outstanding: Acquire): Bool = {
    (isRead(cmd) && messageIsUncached(outstanding)) ||
      (isWriteIntent(cmd) && (outstanding.a_type != acquireReadExclusive && outstanding.a_type != acquireInvalidateOthers))
  }
  def needsTransactionOnCacheControl(cmd: UInt, m: ClientMetadata): Bool = {
    MuxLookup(cmd, (m.state === clientExclusiveDirty), Array(
      M_INV -> Vec(clientExclusiveDirty,clientMigratoryDirty).contains(m.state),
      M_CLN -> Vec(clientExclusiveDirty,clientMigratoryDirty).contains(m.state)
    ))
  }
  def needsWriteback (m: ClientMetadata): Bool = {
    needsTransactionOnCacheControl(M_INV, m)
  }

  def clientMetadataOnHit(cmd: UInt, m: ClientMetadata) = ClientMetadata(
    Mux(isWrite(cmd), MuxLookup(m.state, clientExclusiveDirty, Array(
                clientExclusiveClean -> clientExclusiveDirty,
                clientMigratoryClean -> clientMigratoryDirty)), m.state))(this)
  def clientMetadataOnCacheControl(cmd: UInt) = ClientMetadata(
    MuxLookup(cmd, clientInvalid, Array(
      M_INV -> clientInvalid,
      M_CLN -> clientShared
    )))(this)
  def clientMetadataOnFlush = clientMetadataOnCacheControl(M_INV)
  def clientMetadataOnGrant(incoming: Grant, outstanding: Acquire) = ClientMetadata(
    MuxLookup(incoming.g_type, clientInvalid, Array(
      grantReadShared -> clientShared,
      grantReadExclusive  -> MuxLookup(outstanding.a_type, clientExclusiveDirty,  Array(
                                   acquireReadExclusive -> clientExclusiveDirty,
                                   acquireReadShared -> clientExclusiveClean)),
      grantReadExclusiveAck -> clientExclusiveDirty, 
      grantReadUncached -> clientInvalid,
      grantWriteUncached -> clientInvalid,
      grantReadWordUncached -> clientInvalid,
      grantWriteWordUncached -> clientInvalid,
      grantAtomicUncached -> clientInvalid,
      grantReadMigratory -> MuxLookup(outstanding.a_type, clientMigratoryDirty, Array(
                                  acquireInvalidateOthers -> clientMigratoryDirty,
                                  acquireReadExclusive -> clientMigratoryDirty,
                                  acquireReadShared -> clientMigratoryClean))
    )))(this)
  def clientMetadataOnProbe(incoming: Probe, m: ClientMetadata) = ClientMetadata(
    MuxLookup(incoming.p_type, m.state, Array(
      probeInvalidate -> clientInvalid,
      probeInvalidateOthers -> clientInvalid,
      probeCopy -> m.state,
      probeDowngrade -> MuxLookup(m.state, clientShared, Array(
                              clientExclusiveClean -> clientSharedByTwo,
                              clientExclusiveDirty -> clientSharedByTwo,
                              clientSharedByTwo    -> clientShared,
                              clientMigratoryClean -> clientSharedByTwo,
                              clientMigratoryDirty -> clientInvalid))
    )))(this)
  def masterMetadataOnFlush = MasterMetadata(masterInvalid)(this)
  def masterMetadataOnRelease(incoming: Release, m: MasterMetadata, src: UInt) = {
    val popped = m.sharers.pop(src)
    val next = MasterMetadata(
                Mux(popped.none(), masterInvalid, 
                  Mux(popped.one(), masterExclusive, masterShared)),
                popped)(this)
    def is(r: UInt) = incoming.r_type === r
    MuxBundle(m, Array(
      is(releaseVoluntaryInvalidateData) -> next,
      is(releaseInvalidateData) -> next,
      is(releaseDowngradeData) -> m,
      is(releaseCopyData) -> m,
      is(releaseInvalidateAck) -> next,
      is(releaseDowngradeAck) -> m,
      is(releaseCopyAck) -> m,
      is(releaseDowngradeDataMigratory) -> m,
      is(releaseDowngradeAckHasCopy) -> m,
      is(releaseInvalidateDataMigratory) -> next,
      is(releaseInvalidateAckMigratory) -> next
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

  def getAcquireTypeOnPrimaryMiss(cmd: UInt, m: ClientMetadata): UInt = {
    Mux(isWriteIntent(cmd), Mux(m.state === clientInvalid, acquireReadExclusive, acquireInvalidateOthers), acquireReadShared)
  }
  def getAcquireTypeOnSecondaryMiss(cmd: UInt, m: ClientMetadata, outstanding: Acquire): UInt = {
    Mux(isWriteIntent(cmd), Mux(m.state === clientInvalid, acquireReadExclusive, acquireInvalidateOthers), outstanding.a_type)
  }
  def getReleaseTypeOnCacheControl(cmd: UInt): UInt = releaseVoluntaryInvalidateData // TODO
  def getReleaseTypeOnVoluntaryWriteback(): UInt = getReleaseTypeOnCacheControl(M_INV)
  def getReleaseTypeOnProbe(incoming: Probe, m: ClientMetadata): UInt = {
    val with_data = MuxLookup(incoming.p_type, releaseInvalidateData, Array(
      probeInvalidate -> Mux(Vec(clientExclusiveDirty, clientMigratoryDirty).contains(m.state), 
                                    releaseInvalidateDataMigratory, releaseInvalidateData),
      probeDowngrade -> Mux(m.state === clientMigratoryDirty, releaseDowngradeDataMigratory, releaseDowngradeData),
      probeCopy -> releaseCopyData
    ))
    val without_data = MuxLookup(incoming.p_type, releaseInvalidateAck, Array(
      probeInvalidate -> Mux(clientExclusiveClean === m.state, releaseInvalidateAckMigratory, releaseInvalidateAck),
      probeInvalidateOthers -> Mux(m.state === clientSharedByTwo, releaseInvalidateAckMigratory, releaseInvalidateAck),
      probeDowngrade  -> Mux(m.state != clientInvalid, releaseDowngradeAckHasCopy, releaseDowngradeAck),
      probeCopy       -> releaseCopyAck
    ))
    Mux(needsWriteback(m), with_data, without_data)
  }

  def messageHasData(msg: SourcedMessage) = msg match {
    case acq: Acquire => hasDataAcquireTypeVec.contains(acq.a_type)
    case grant: Grant => hasDataGrantTypeVec.contains(grant.g_type) 
    case rel: Release => hasDataReleaseTypeVec.contains(rel.r_type) 
    case _ => Bool(false)
  }
  def messageUpdatesDataArray (reply: Grant): Bool = Vec(grantReadShared, grantReadExclusive, grantReadMigratory).contains(reply.g_type)
  def messageIsUncached(acq: Acquire): Bool = uncachedAcquireTypeVec.contains(acq.a_type)

  def isCoherenceConflict(addr1: UInt, addr2: UInt): Bool = (addr1 === addr2)

  def getGrantType(a: Acquire, m: MasterMetadata): UInt = {
    MuxLookup(a.a_type, grantReadUncached, Array(
      acquireReadShared    -> Mux(m.sharers.count() > UInt(0), grantReadShared, grantReadExclusive), //TODO: what is count? Depend on release.p_type???
      acquireReadExclusive -> grantReadExclusive,                                            
      acquireReadUncached  -> grantReadUncached,
      acquireWriteUncached -> grantWriteUncached,
      acquireReadWordUncached  -> grantReadWordUncached,
      acquireWriteWordUncached -> grantWriteWordUncached,
      acquireAtomicUncached -> grantAtomicUncached,
      acquireInvalidateOthers -> grantReadExclusiveAck  //TODO: add this to MESI?
    ))
  }
  def getGrantType(r: Release, m: MasterMetadata): UInt = {
    MuxLookup(r.r_type, grantReadUncached, Array(
      releaseVoluntaryInvalidateData -> grantVoluntaryAck
    ))
  }


  def getProbeType(a: Acquire, m: MasterMetadata): UInt = {
    MuxLookup(a.a_type, probeCopy, Array(
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

  def requiresOuterRead(a_type: UInt) = {
      (a_type != acquireWriteUncached && a_type != acquireInvalidateOthers)
  }
  def requiresOuterWrite(a_type: UInt) = {
      (a_type === acquireWriteUncached || a_type === acquireWriteWordUncached || a_type === acquireAtomicUncached)
  }
  def requiresOuterRead(a_type: UInt, m: MasterMetadata) = requiresOuterRead(a_type)
  def requiresOuterWrite(a_type: UInt, m: MasterMetadata) = requiresOuterWrite(a_type)

  def requiresAckForGrant(g_type: UInt) = g_type != grantVoluntaryAck
  def requiresAckForRelease(r_type: UInt) = Bool(false)
  def requiresSelfProbe(a_type: UInt) = Bool(false)

  def pendingVoluntaryReleaseIsSufficient(r_type: UInt, p_type: UInt): Bool = (r_type === releaseVoluntaryInvalidateData)
}
