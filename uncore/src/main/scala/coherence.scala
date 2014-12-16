// See LICENSE for license details.

package uncore
import Chisel._

abstract class CoherenceMetadata extends Bundle with CoherenceAgentParameters

class ClientMetadata extends CoherenceMetadata {
  val state = UInt(width = co.clientStateWidth)
  def ===(right: ClientMetadata): Bool = this.state === right.state
  override def clone = new ClientMetadata().asInstanceOf[this.type]
}
object ClientMetadata {
  def apply(state: UInt) = {
    val m = new ClientMetadata
    m.state := state
    m
  }
}

class MasterMetadata extends CoherenceMetadata {
  val state = UInt(width = co.masterStateWidth)
  val sharers = UInt(width = co.dir.width)
  override def clone = new MasterMetadata().asInstanceOf[this.type]
}
object MasterMetadata {
  def apply(state: UInt, sharers: UInt) = {
    val m = new MasterMetadata
    m.state := state
    m.sharers := sharers
    m
  }
  def apply(state: UInt): MasterMetadata = apply(state, new MasterMetadata().co.dir.flush)
}

abstract class DirectoryRepresentation(val width: Int) {
  def pop(prev: UInt, id: UInt): UInt
  def push(prev: UInt, id: UInt): UInt
  def flush: UInt
  def none(s: UInt): Bool
  def one(s: UInt): Bool
  def count(s: UInt): UInt
  def next(s: UInt): UInt
  def full(s: UInt): UInt
}

class NullRepresentation(nClients: Int) extends DirectoryRepresentation(1) {
  def pop(prev: UInt, id: UInt) = UInt(0)
  def push(prev: UInt, id: UInt) = UInt(0)
  def flush  = UInt(0)
  def none(s: UInt) = Bool(false)
  def one(s: UInt) = Bool(false)
  def count(s: UInt) = UInt(nClients)
  def next(s: UInt) = UInt(0)
  def full(s: UInt) = SInt(-1, width = nClients).toUInt
}

class FullRepresentation(nClients: Int) extends DirectoryRepresentation(nClients) {
  def pop(prev: UInt, id: UInt) =  prev &  ~UIntToOH(id)
  def push(prev: UInt, id: UInt) = prev | UIntToOH(id)
  def flush = UInt(0, width = width)
  def none(s: UInt) = s === UInt(0)
  def one(s: UInt) = PopCount(s) === UInt(1)
  def count(s: UInt) = PopCount(s)
  def next(s: UInt) = PriorityEncoder(s)
  def full(s: UInt) = s
}

abstract class CoherencePolicy(val dir: DirectoryRepresentation) {
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
  def needsWriteback(m: MasterMetadata): Bool

  def clientMetadataOnHit(cmd: UInt, m: ClientMetadata): ClientMetadata
  def clientMetadataOnCacheControl(cmd: UInt): ClientMetadata 
  def clientMetadataOnFlush: ClientMetadata 
  def clientMetadataOnGrant(incoming: Grant, outstanding: Acquire): ClientMetadata 
  def clientMetadataOnProbe(incoming: Probe, m: ClientMetadata): ClientMetadata 
  def masterMetadataOnFlush: MasterMetadata 
  def masterMetadataOnRelease(incoming: Release, m: MasterMetadata, src: UInt): MasterMetadata
  def masterMetadataOnGrant(outgoing: Grant, m: MasterMetadata, dst: UInt): MasterMetadata

  def getAcquireTypeOnPrimaryMiss(cmd: UInt, m: ClientMetadata): UInt
  def getAcquireTypeOnSecondaryMiss(cmd: UInt, m: ClientMetadata, outstanding: Acquire): UInt
  def getProbeType(a: Acquire, m: MasterMetadata): UInt
  def getProbeTypeOnVoluntaryWriteback: UInt
  def getReleaseTypeOnCacheControl(cmd: UInt): UInt
  def getReleaseTypeOnVoluntaryWriteback(): UInt
  def getReleaseTypeOnProbe(p: Probe, m: ClientMetadata): UInt
  def getGrantType(a: Acquire, m: MasterMetadata): UInt
  def getGrantTypeOnVoluntaryWriteback(m: MasterMetadata): UInt

  def messageHasData(rel: SourcedMessage): Bool
  def messageUpdatesDataArray(g: Grant): Bool
  def isVoluntary(rel: Release): Bool
  def isVoluntary(gnt: Grant): Bool
  def requiresOuterRead(acq: Acquire, m: MasterMetadata): Bool
  def requiresOuterWrite(acq: Acquire, m: MasterMetadata): Bool
  def requiresSelfProbe(a: Acquire): Bool
  def requiresProbes(a: Acquire, m: MasterMetadata): Bool
  def requiresAckForGrant(g: Grant): Bool
  def requiresAckForRelease(r: Release): Bool
  def pendingVoluntaryReleaseIsSufficient(r_type: UInt, p_type: UInt): Bool

  def isCoherenceConflict(addr1: UInt, addr2: UInt): Bool

  def getGrantTypeForUncached(a: Acquire, m: MasterMetadata): UInt = {
    MuxLookup(a.a_type, Grant.uncachedRead, Array(
      Acquire.uncachedRead -> Grant.uncachedRead,
      Acquire.uncachedWrite -> Grant.uncachedWrite,
      Acquire.uncachedAtomic -> Grant.uncachedAtomic
    ))
  }
}

class MICoherence(dir: DirectoryRepresentation) extends CoherencePolicy(dir) {
  def nClientStates = 2
  def nMasterStates = 2
  def nAcquireTypes = 1
  def nProbeTypes = 2
  def nReleaseTypes = 5
  def nGrantTypes = 2

  val clientInvalid :: clientValid :: Nil = Enum(UInt(), nClientStates)
  val masterInvalid :: masterValid :: Nil = Enum(UInt(), nMasterStates)

  val acquireReadExclusive :: Nil = Enum(UInt(), nAcquireTypes)
  val probeInvalidate :: probeCopy :: Nil = Enum(UInt(), nProbeTypes)
  val releaseVoluntaryInvalidateData :: releaseInvalidateData :: releaseCopyData :: releaseInvalidateAck :: releaseCopyAck :: Nil = Enum(UInt(), nReleaseTypes)
  val grantVoluntaryAck :: grantReadExclusive :: Nil = Enum(UInt(), nGrantTypes)

  val hasDataReleaseTypeVec = Vec(releaseVoluntaryInvalidateData, releaseInvalidateData, releaseCopyData)
  val hasDataGrantTypeVec = Vec(grantReadExclusive)

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
  def needsWriteback(m: MasterMetadata) = isValid(m)

  def clientMetadataOnHit(cmd: UInt, m: ClientMetadata) = m
  def clientMetadataOnCacheControl(cmd: UInt) = ClientMetadata(
    MuxLookup(cmd, clientInvalid, Array(
      M_INV -> clientInvalid,
      M_CLN -> clientValid
    )))
  def clientMetadataOnFlush = clientMetadataOnCacheControl(M_INV)
  def clientMetadataOnGrant(incoming: Grant, outstanding: Acquire) = 
    ClientMetadata(Mux(incoming.uncached, clientInvalid, clientValid))
  def clientMetadataOnProbe(incoming: Probe, m: ClientMetadata) = ClientMetadata(
    MuxLookup(incoming.p_type, m.state, Array(
      probeInvalidate -> clientInvalid,
      probeCopy       -> m.state
    )))
  def masterMetadataOnFlush = MasterMetadata(masterInvalid)
  def masterMetadataOnRelease(r: Release, m: MasterMetadata, src: UInt) = {
    val next = MasterMetadata(masterValid, dir.pop(m.sharers, src))
    MuxBundle(m, Array(
      r.is(releaseVoluntaryInvalidateData) -> next,
      r.is(releaseInvalidateData) -> next,
      r.is(releaseInvalidateAck) -> next
    ))
  }
  def masterMetadataOnGrant(g: Grant, m: MasterMetadata, dst: UInt) = {
    val cached = MasterMetadata(masterValid, dir.push(m.sharers, dst))
    val uncached = MasterMetadata(masterValid, m.sharers)
    Mux(g.uncached, uncached, cached)
  }

  def isVoluntary(rel: Release) = rel.r_type === releaseVoluntaryInvalidateData
  def isVoluntary(gnt: Grant) = !gnt.uncached && gnt.g_type === grantVoluntaryAck

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
    case acq: Acquire => Mux(acq.uncached, Acquire.hasData(acq.a_type), Bool(false))
    case gnt: Grant => Mux(gnt.uncached, Grant.hasData(gnt.g_type), hasDataGrantTypeVec.contains(gnt.g_type))
    case rel: Release => hasDataReleaseTypeVec.contains(rel.r_type) 
    case _ => Bool(false)
  }
  def messageUpdatesDataArray(g: Grant): Bool = {
    Mux(g.uncached, Bool(false), 
      (g.g_type === grantReadExclusive))
  }

  def isCoherenceConflict(addr1: UInt, addr2: UInt): Bool = (addr1 === addr2)

  def getGrantType(a: Acquire, m: MasterMetadata): UInt = 
    Mux(a.uncached, getGrantTypeForUncached(a, m), grantReadExclusive)

  def getGrantTypeOnVoluntaryWriteback(m: MasterMetadata): UInt = grantVoluntaryAck

  def getProbeType(a: Acquire, m: MasterMetadata): UInt = {
    Mux(a.uncached, 
      MuxLookup(a.a_type, probeCopy, Array(
        Acquire.uncachedRead -> probeCopy, 
        Acquire.uncachedWrite -> probeInvalidate,
        Acquire.uncachedAtomic -> probeInvalidate
      )), probeInvalidate)
  }

  def getProbeTypeOnVoluntaryWriteback: UInt = probeInvalidate
  def requiresOuterRead(acq: Acquire, m: MasterMetadata) = 
    Mux(acq.uncached, Acquire.requiresOuterRead(acq.a_type), Bool(true))
  def requiresOuterWrite(acq: Acquire, m: MasterMetadata) =
    Mux(acq.uncached, Acquire.requiresOuterWrite(acq.a_type), Bool(false))

  def requiresAckForGrant(g: Grant) = g.uncached || g.g_type != grantVoluntaryAck
  def requiresAckForRelease(r: Release) = Bool(false)
  def requiresSelfProbe(a: Acquire) = a.uncached && a.a_type === Acquire.uncachedRead
  def requiresProbes(a: Acquire, m: MasterMetadata) = !dir.none(m.sharers)
  def pendingVoluntaryReleaseIsSufficient(r_type: UInt, p_type: UInt): Bool = (r_type === releaseVoluntaryInvalidateData)
}

class MEICoherence(dir: DirectoryRepresentation) extends CoherencePolicy(dir) {
  def nClientStates = 3
  def nMasterStates = 2
  def nAcquireTypes = 2
  def nProbeTypes = 3
  def nReleaseTypes = 7
  def nGrantTypes = 2

  val clientInvalid :: clientExclusiveClean :: clientExclusiveDirty :: Nil = Enum(UInt(), nClientStates)
  val masterInvalid :: masterValid :: Nil = Enum(UInt(), nMasterStates)

  val acquireReadExclusiveClean :: acquireReadExclusiveDirty :: Nil = Enum(UInt(), nAcquireTypes)
  val probeInvalidate :: probeDowngrade :: probeCopy :: Nil = Enum(UInt(), nProbeTypes)
  val releaseVoluntaryInvalidateData :: releaseInvalidateData :: releaseDowngradeData :: releaseCopyData :: releaseInvalidateAck :: releaseDowngradeAck :: releaseCopyAck :: Nil = Enum(UInt(), nReleaseTypes)
  val grantVoluntaryAck :: grantReadExclusive :: Nil = Enum(UInt(), nGrantTypes)

  val hasDataReleaseTypeVec = Vec(releaseVoluntaryInvalidateData, releaseInvalidateData, releaseDowngradeData, releaseCopyData)
  val hasDataGrantTypeVec = Vec(grantReadExclusive)

  def isHit (cmd: UInt, m: ClientMetadata) = isValid(m)
  def isValid (m: ClientMetadata) = m.state != clientInvalid
  def isHit (incoming: Acquire, m: MasterMetadata) = isValid(m)
  def isValid (m: MasterMetadata) = m.state != masterInvalid

  def needsTransactionOnSecondaryMiss(cmd: UInt, outstanding: Acquire): Bool = {
    (isRead(cmd) && outstanding.uncached) ||
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
  def needsWriteback(m: MasterMetadata) = isValid(m)

  def clientMetadataOnHit(cmd: UInt, m: ClientMetadata) = 
    ClientMetadata(Mux(isWrite(cmd), clientExclusiveDirty, m.state))
  
  def clientMetadataOnCacheControl(cmd: UInt) = ClientMetadata(
    MuxLookup(cmd, clientInvalid, Array(
      M_INV -> clientInvalid,
      M_CLN -> clientExclusiveClean
    )))
  def clientMetadataOnFlush() = clientMetadataOnCacheControl(M_INV)
  def clientMetadataOnGrant(incoming: Grant, outstanding: Acquire) = ClientMetadata(
    Mux(incoming.uncached, clientInvalid,
      Mux(outstanding.a_type === acquireReadExclusiveDirty, clientExclusiveDirty, 
        clientExclusiveClean)))
  def clientMetadataOnProbe(incoming: Probe, m: ClientMetadata) = ClientMetadata(
    MuxLookup(incoming.p_type, m.state, Array(
      probeInvalidate -> clientInvalid,
      probeDowngrade  -> clientExclusiveClean,
      probeCopy       -> m.state
    )))
  def masterMetadataOnFlush = MasterMetadata(masterInvalid)
  def masterMetadataOnRelease(r: Release, m: MasterMetadata, src: UInt) = {
    val next = MasterMetadata(masterValid, dir.pop(m.sharers,src))
    MuxBundle(m, Array(
      r.is(releaseVoluntaryInvalidateData) -> next,
      r.is(releaseInvalidateData) -> next,
      r.is(releaseInvalidateAck) -> next
    ))
  }
  def masterMetadataOnGrant(g: Grant, m: MasterMetadata, dst: UInt) = {
    val cached = MasterMetadata(masterValid, dir.push(m.sharers, dst))
    val uncached = MasterMetadata(masterValid, m.sharers)
    Mux(g.uncached, uncached, cached)
  }

  def isVoluntary(rel: Release) = rel.r_type === releaseVoluntaryInvalidateData
  def isVoluntary(gnt: Grant) = !gnt.uncached && gnt.g_type === grantVoluntaryAck

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
    case acq: Acquire => Mux(acq.uncached, Acquire.hasData(acq.a_type), Bool(false))
    case gnt: Grant => Mux(gnt.uncached, Grant.hasData(gnt.g_type), hasDataGrantTypeVec.contains(gnt.g_type))
    case rel: Release => hasDataReleaseTypeVec.contains(rel.r_type) 
    case _ => Bool(false)
  }
  def messageUpdatesDataArray(g: Grant): Bool = {
    Mux(g.uncached, Bool(false), 
      (g.g_type === grantReadExclusive))
  }

  def isCoherenceConflict(addr1: UInt, addr2: UInt): Bool = (addr1 === addr2)

  def getGrantType(a: Acquire, m: MasterMetadata): UInt = {
    Mux(a.uncached, getGrantTypeForUncached(a, m), grantReadExclusive)
  }
  def getGrantTypeOnVoluntaryWriteback(m: MasterMetadata): UInt = grantVoluntaryAck

  def getProbeType(a: Acquire, m: MasterMetadata): UInt = {
    Mux(a.uncached, 
      MuxLookup(a.a_type, probeCopy, Array(
        Acquire.uncachedRead -> probeCopy, 
        Acquire.uncachedWrite -> probeInvalidate,
        Acquire.uncachedAtomic -> probeInvalidate
      )), probeInvalidate)
  }
  def getProbeTypeOnVoluntaryWriteback: UInt = probeInvalidate

  def requiresOuterRead(acq: Acquire, m: MasterMetadata) = 
    Mux(acq.uncached, Acquire.requiresOuterRead(acq.a_type), Bool(true))
  def requiresOuterWrite(acq: Acquire, m: MasterMetadata) =
    Mux(acq.uncached, Acquire.requiresOuterWrite(acq.a_type), Bool(false))

  def requiresAckForGrant(g: Grant) = g.uncached || g.g_type != grantVoluntaryAck
  def requiresAckForRelease(r: Release) = Bool(false)
  def requiresSelfProbe(a: Acquire) = a.uncached && a.a_type === Acquire.uncachedRead
  def requiresProbes(a: Acquire, m: MasterMetadata) = !dir.none(m.sharers)

  def pendingVoluntaryReleaseIsSufficient(r_type: UInt, p_type: UInt): Bool = (r_type === releaseVoluntaryInvalidateData)
}

class MSICoherence(dir: DirectoryRepresentation) extends CoherencePolicy(dir) {
  def nClientStates = 3
  def nMasterStates = 2
  def nAcquireTypes = 2
  def nProbeTypes = 3
  def nReleaseTypes = 7
  def nGrantTypes = 3

  val clientInvalid :: clientShared :: clientExclusiveDirty :: Nil = Enum(UInt(), nClientStates)
  //val masterInvalid :: masterShared :: masterExclusive :: Nil = Enum(UInt(), nMasterStates)
  val masterInvalid :: masterValid :: Nil = Enum(UInt(), nMasterStates)

  val acquireReadShared :: acquireReadExclusive :: Nil = Enum(UInt(), nAcquireTypes)
  val probeInvalidate :: probeDowngrade :: probeCopy :: Nil = Enum(UInt(), nProbeTypes)
  val releaseVoluntaryInvalidateData :: releaseInvalidateData :: releaseDowngradeData :: releaseCopyData :: releaseInvalidateAck :: releaseDowngradeAck :: releaseCopyAck :: Nil = Enum(UInt(), nReleaseTypes)
  val grantVoluntaryAck :: grantReadShared :: grantReadExclusive :: Nil = Enum(UInt(), nGrantTypes)

  val hasDataReleaseTypeVec = Vec(releaseVoluntaryInvalidateData, releaseInvalidateData, releaseDowngradeData, releaseCopyData)
  val hasDataGrantTypeVec = Vec(grantReadShared, grantReadExclusive)

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
    (isRead(cmd) && outstanding.uncached) || 
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
  def needsWriteback(m: MasterMetadata) = isValid(m)

  def clientMetadataOnHit(cmd: UInt, m: ClientMetadata) =
    ClientMetadata(Mux(isWrite(cmd), clientExclusiveDirty, m.state))
  def clientMetadataOnCacheControl(cmd: UInt) = ClientMetadata(
    MuxLookup(cmd, clientInvalid, Array(
      M_INV -> clientInvalid,
      M_CLN -> clientShared
    )))
  def clientMetadataOnFlush() = clientMetadataOnCacheControl(M_INV)
  def clientMetadataOnGrant(incoming: Grant, outstanding: Acquire) = ClientMetadata(
    Mux(incoming.uncached, clientInvalid,
      Mux(incoming.g_type === grantReadShared, clientShared, 
        clientExclusiveDirty)))
  def clientMetadataOnProbe(incoming: Probe, m: ClientMetadata) = ClientMetadata(
    MuxLookup(incoming.p_type, m.state, Array(
      probeInvalidate -> clientInvalid,
      probeDowngrade  -> clientShared,
      probeCopy       -> m.state
    )))
  def masterMetadataOnFlush = MasterMetadata(masterInvalid)
  def masterMetadataOnRelease(r: Release, m: MasterMetadata, src: UInt) = {
    val next = MasterMetadata(masterValid, dir.pop(m.sharers,src))
    MuxBundle(m, Array(
      r.is(releaseVoluntaryInvalidateData) -> next,
      r.is(releaseInvalidateData) -> next,
      r.is(releaseInvalidateAck) -> next
    ))
  }
  def masterMetadataOnGrant(g: Grant, m: MasterMetadata, dst: UInt) = {
    val cached = MasterMetadata(masterValid, dir.push(m.sharers, dst))
    val uncached = MasterMetadata(masterValid, m.sharers)
    Mux(g.uncached, uncached, cached)
  }

  def isVoluntary(rel: Release) = rel.r_type === releaseVoluntaryInvalidateData
  def isVoluntary(gnt: Grant) = !gnt.uncached && gnt.g_type === grantVoluntaryAck

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
    case acq: Acquire => Mux(acq.uncached, Acquire.hasData(acq.a_type), Bool(false))
    case gnt: Grant => Mux(gnt.uncached, Grant.hasData(gnt.g_type), hasDataGrantTypeVec.contains(gnt.g_type))
    case rel: Release => hasDataReleaseTypeVec.contains(rel.r_type) 
    case _ => Bool(false)
  }
  def messageUpdatesDataArray(g: Grant): Bool = {
    Mux(g.uncached, Bool(false), 
      (g.g_type === grantReadShared || g.g_type === grantReadExclusive))
  }

  def isCoherenceConflict(addr1: UInt, addr2: UInt): Bool = (addr1 === addr2)

  def getGrantType(a: Acquire, m: MasterMetadata): UInt = {
    Mux(a.uncached, getGrantTypeForUncached(a, m),
      Mux(a.a_type === acquireReadShared,
        Mux(!dir.none(m.sharers), grantReadShared, grantReadExclusive),
        grantReadExclusive))
  }
  def getGrantTypeOnVoluntaryWriteback(m: MasterMetadata): UInt = grantVoluntaryAck

  def getProbeType(a: Acquire, m: MasterMetadata): UInt = {
    Mux(a.uncached, 
      MuxLookup(a.a_type, probeCopy, Array(
        Acquire.uncachedRead -> probeCopy, 
        Acquire.uncachedWrite -> probeInvalidate,
        Acquire.uncachedAtomic -> probeInvalidate
      )),
      MuxLookup(a.a_type, probeInvalidate, Array(
        acquireReadShared -> probeDowngrade,
        acquireReadExclusive -> probeInvalidate
      )))
  }
  def getProbeTypeOnVoluntaryWriteback: UInt = probeInvalidate

  def requiresOuterRead(acq: Acquire, m: MasterMetadata) = 
    Mux(acq.uncached, Acquire.requiresOuterRead(acq.a_type), Bool(true))
  def requiresOuterWrite(acq: Acquire, m: MasterMetadata) =
    Mux(acq.uncached, Acquire.requiresOuterWrite(acq.a_type), Bool(false))

  def requiresAckForGrant(g: Grant) = g.uncached || g.g_type != grantVoluntaryAck
  def requiresAckForRelease(r: Release) = Bool(false)
  def requiresSelfProbe(a: Acquire) = a.uncached && a.a_type === Acquire.uncachedRead
  def requiresProbes(a: Acquire, m: MasterMetadata) = !dir.none(m.sharers)

  def pendingVoluntaryReleaseIsSufficient(r_type: UInt, p_type: UInt): Bool = (r_type === releaseVoluntaryInvalidateData)
}

class MESICoherence(dir: DirectoryRepresentation) extends CoherencePolicy(dir) {
  def nClientStates = 4
  def nMasterStates = 2
  def nAcquireTypes = 2
  def nProbeTypes = 3
  def nReleaseTypes = 7
  def nGrantTypes = 4

  val clientInvalid :: clientShared :: clientExclusiveClean :: clientExclusiveDirty :: Nil = Enum(UInt(), nClientStates)
  //val masterInvalid :: masterShared :: masterExclusive :: Nil = Enum(UInt(), nMasterStates)
  val masterInvalid :: masterValid :: Nil = Enum(UInt(), nMasterStates)

  val acquireReadShared :: acquireReadExclusive :: Nil = Enum(UInt(), nAcquireTypes)
  val probeInvalidate :: probeDowngrade :: probeCopy :: Nil = Enum(UInt(), nProbeTypes)
  val releaseVoluntaryInvalidateData :: releaseInvalidateData :: releaseDowngradeData :: releaseCopyData :: releaseInvalidateAck :: releaseDowngradeAck :: releaseCopyAck :: Nil = Enum(UInt(), nReleaseTypes)
  val grantVoluntaryAck :: grantReadShared :: grantReadExclusive :: grantReadExclusiveAck :: Nil = Enum(UInt(), nGrantTypes)

  val hasDataReleaseTypeVec = Vec(releaseVoluntaryInvalidateData, releaseInvalidateData, releaseDowngradeData, releaseCopyData)
  val hasDataGrantTypeVec = Vec(grantReadShared, grantReadExclusive)

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
    (isRead(cmd) && outstanding.uncached) ||
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
  def needsWriteback(m: MasterMetadata) = isValid(m)

  def clientMetadataOnHit(cmd: UInt, m: ClientMetadata) = 
    ClientMetadata(Mux(isWrite(cmd), clientExclusiveDirty, m.state))

  def clientMetadataOnCacheControl(cmd: UInt) = ClientMetadata(
    MuxLookup(cmd, clientInvalid, Array(
      M_INV -> clientInvalid,
      M_CLN -> clientShared
    )))
  def clientMetadataOnFlush = clientMetadataOnCacheControl(M_INV)
  def clientMetadataOnGrant(incoming: Grant, outstanding: Acquire) = ClientMetadata(
    Mux(incoming.uncached, clientInvalid,
      MuxLookup(incoming.g_type, clientInvalid, Array(
        grantReadShared -> clientShared,
        grantReadExclusive -> Mux(outstanding.a_type === acquireReadExclusive, 
                                clientExclusiveDirty, clientExclusiveClean),
        grantReadExclusiveAck -> clientExclusiveDirty
      ))))
  def clientMetadataOnProbe(incoming: Probe, m: ClientMetadata) = ClientMetadata(
    MuxLookup(incoming.p_type, m.state, Array(
      probeInvalidate -> clientInvalid,
      probeDowngrade  -> clientShared,
      probeCopy       -> m.state
    )))
  def masterMetadataOnFlush = MasterMetadata(masterInvalid)
  def masterMetadataOnRelease(r: Release, m: MasterMetadata, src: UInt) = {
    val next = MasterMetadata(masterValid, dir.pop(m.sharers,src))
    MuxBundle(m, Array(
      r.is(releaseVoluntaryInvalidateData) -> next,
      r.is(releaseInvalidateData) -> next,
      r.is(releaseInvalidateAck) -> next
    ))
  }
  def masterMetadataOnGrant(g: Grant, m: MasterMetadata, dst: UInt) = {
    val cached = MasterMetadata(masterValid, dir.push(m.sharers, dst))
    val uncached = MasterMetadata(masterValid, m.sharers)
    Mux(g.uncached, uncached, cached)
  }

  def isVoluntary(rel: Release) = rel.r_type === releaseVoluntaryInvalidateData
  def isVoluntary(gnt: Grant) = !gnt.uncached && gnt.g_type === grantVoluntaryAck

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
    case acq: Acquire => Mux(acq.uncached, Acquire.hasData(acq.a_type), Bool(false))
    case gnt: Grant => Mux(gnt.uncached, Grant.hasData(gnt.g_type), hasDataGrantTypeVec.contains(gnt.g_type))
    case rel: Release => hasDataReleaseTypeVec.contains(rel.r_type) 
    case _ => Bool(false)
  }
  def messageUpdatesDataArray(g: Grant): Bool = {
    Mux(g.uncached, Bool(false), 
      (g.g_type === grantReadShared || g.g_type === grantReadExclusive))
  }

  def isCoherenceConflict(addr1: UInt, addr2: UInt): Bool = (addr1 === addr2)

  def getGrantType(a: Acquire, m: MasterMetadata): UInt = {
    Mux(a.uncached, getGrantTypeForUncached(a, m),
      Mux(a.a_type === acquireReadShared,
        Mux(!dir.none(m.sharers), grantReadShared, grantReadExclusive),
        grantReadExclusive))
  }
  def getGrantTypeOnVoluntaryWriteback(m: MasterMetadata): UInt = grantVoluntaryAck

  def getProbeType(a: Acquire, m: MasterMetadata): UInt = {
    Mux(a.uncached, 
      MuxLookup(a.a_type, probeCopy, Array(
        Acquire.uncachedRead -> probeCopy, 
        Acquire.uncachedWrite -> probeInvalidate,
        Acquire.uncachedAtomic -> probeInvalidate
      )),
      MuxLookup(a.a_type, probeCopy, Array(
        acquireReadShared -> probeDowngrade,
        acquireReadExclusive -> probeInvalidate
      )))
  }
  def getProbeTypeOnVoluntaryWriteback: UInt = probeInvalidate

  def requiresOuterRead(acq: Acquire, m: MasterMetadata) = 
    Mux(acq.uncached, Acquire.requiresOuterRead(acq.a_type), Bool(true))
  def requiresOuterWrite(acq: Acquire, m: MasterMetadata) =
    Mux(acq.uncached, Acquire.requiresOuterWrite(acq.a_type), Bool(false))

  def requiresAckForGrant(g: Grant) = g.uncached || g.g_type != grantVoluntaryAck
  def requiresAckForRelease(r: Release) = Bool(false)
  def requiresSelfProbe(a: Acquire) = a.uncached && a.a_type === Acquire.uncachedRead
  def requiresProbes(a: Acquire, m: MasterMetadata) = !dir.none(m.sharers)

  def pendingVoluntaryReleaseIsSufficient(r_type: UInt, p_type: UInt): Bool = (r_type === releaseVoluntaryInvalidateData)
}

class MigratoryCoherence(dir: DirectoryRepresentation) extends CoherencePolicy(dir) {
  def nClientStates = 7
  def nMasterStates = 2
  def nAcquireTypes = 3
  def nProbeTypes = 4
  def nReleaseTypes = 11
  def nGrantTypes = 5

  val clientInvalid :: clientShared :: clientExclusiveClean :: clientExclusiveDirty :: clientSharedByTwo :: clientMigratoryClean :: clientMigratoryDirty :: Nil = Enum(UInt(), nClientStates)
  //val masterInvalid :: masterShared :: masterExclusive :: Nil = Enum(UInt(), nMasterStates)
  val masterInvalid :: masterValid :: Nil = Enum(UInt(), nMasterStates)

  val acquireReadShared :: acquireReadExclusive :: acquireInvalidateOthers :: Nil = Enum(UInt(), nAcquireTypes)
  val probeInvalidate :: probeDowngrade :: probeCopy :: probeInvalidateOthers :: Nil = Enum(UInt(), nProbeTypes)
  val releaseVoluntaryInvalidateData :: releaseInvalidateData :: releaseDowngradeData :: releaseCopyData :: releaseInvalidateAck :: releaseDowngradeAck :: releaseCopyAck :: releaseDowngradeDataMigratory :: releaseDowngradeAckHasCopy :: releaseInvalidateDataMigratory :: releaseInvalidateAckMigratory :: Nil = Enum(UInt(), nReleaseTypes)
  val grantVoluntaryAck :: grantReadShared :: grantReadExclusive :: grantReadExclusiveAck :: grantReadMigratory :: Nil = Enum(UInt(), nGrantTypes)

  val hasDataGrantTypeVec = Vec(grantReadShared, grantReadExclusive, grantReadMigratory)
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
    (isRead(cmd) && outstanding.uncached) ||
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
  def needsWriteback(m: MasterMetadata) = isValid(m)

  def clientMetadataOnHit(cmd: UInt, m: ClientMetadata) = ClientMetadata(
    Mux(isWrite(cmd), MuxLookup(m.state, clientExclusiveDirty, Array(
                clientExclusiveClean -> clientExclusiveDirty,
                clientMigratoryClean -> clientMigratoryDirty)), m.state))
  def clientMetadataOnCacheControl(cmd: UInt) = ClientMetadata(
    MuxLookup(cmd, clientInvalid, Array(
      M_INV -> clientInvalid,
      M_CLN -> clientShared
    )))
  def clientMetadataOnFlush = clientMetadataOnCacheControl(M_INV)
  def clientMetadataOnGrant(incoming: Grant, outstanding: Acquire) = ClientMetadata(
    Mux(incoming.uncached, clientInvalid,
      MuxLookup(incoming.g_type, clientInvalid, Array(
        grantReadShared -> clientShared,
        grantReadExclusive  -> MuxLookup(outstanding.a_type, clientExclusiveDirty,  Array(
                                     acquireReadExclusive -> clientExclusiveDirty,
                                     acquireReadShared -> clientExclusiveClean)),
        grantReadExclusiveAck -> clientExclusiveDirty, 
        grantReadMigratory -> MuxLookup(outstanding.a_type, clientMigratoryDirty, Array(
                                    acquireInvalidateOthers -> clientMigratoryDirty,
                                    acquireReadExclusive -> clientMigratoryDirty,
                                    acquireReadShared -> clientMigratoryClean))
      ))))
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
    )))
  def masterMetadataOnFlush = MasterMetadata(masterInvalid)
  def masterMetadataOnRelease(r: Release, m: MasterMetadata, src: UInt) = {
    val next = MasterMetadata(masterValid, dir.pop(m.sharers,src))
    MuxBundle(m, Array(
      r.is(releaseVoluntaryInvalidateData) -> next,
      r.is(releaseInvalidateData) -> next,
      r.is(releaseInvalidateAck) -> next,
      r.is(releaseInvalidateDataMigratory) -> next,
      r.is(releaseInvalidateAckMigratory) -> next
    ))
  }
  def masterMetadataOnGrant(g: Grant, m: MasterMetadata, dst: UInt) = {
    val cached = MasterMetadata(masterValid, dir.push(m.sharers, dst))
    val uncached = MasterMetadata(masterValid, m.sharers)
    Mux(g.uncached, uncached, cached)
  }


  def isVoluntary(rel: Release) = rel.r_type === releaseVoluntaryInvalidateData
  def isVoluntary(gnt: Grant) = !gnt.uncached && gnt.g_type === grantVoluntaryAck

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
    case acq: Acquire => Mux(acq.uncached, Acquire.hasData(acq.a_type), Bool(false))
    case gnt: Grant => Mux(gnt.uncached, Grant.hasData(gnt.g_type), hasDataGrantTypeVec.contains(gnt.g_type))
    case rel: Release => hasDataReleaseTypeVec.contains(rel.r_type) 
    case _ => Bool(false)
  }
  def messageUpdatesDataArray(g: Grant): Bool = {
    Mux(g.uncached, Bool(false), 
      Vec(grantReadShared, grantReadExclusive, grantReadMigratory).contains(g.g_type))
  }

  def isCoherenceConflict(addr1: UInt, addr2: UInt): Bool = (addr1 === addr2)

  def getGrantType(a: Acquire, m: MasterMetadata): UInt = {
    Mux(a.uncached, getGrantTypeForUncached(a, m),
      MuxLookup(a.a_type, grantReadShared, Array(
        acquireReadShared    -> Mux(!dir.none(m.sharers), grantReadShared, grantReadExclusive),
        acquireReadExclusive -> grantReadExclusive,                                            
        acquireInvalidateOthers -> grantReadExclusiveAck  //TODO: add this to MESI for broadcast?
      )))
  }
  def getGrantTypeOnVoluntaryWriteback(m: MasterMetadata): UInt = grantVoluntaryAck

  def getProbeType(a: Acquire, m: MasterMetadata): UInt = {
    Mux(a.uncached, 
      MuxLookup(a.a_type, probeCopy, Array(
        Acquire.uncachedRead -> probeCopy, 
        Acquire.uncachedWrite -> probeInvalidate,
        Acquire.uncachedAtomic -> probeInvalidate
      )),
      MuxLookup(a.a_type, probeCopy, Array(
        acquireReadShared -> probeDowngrade,
        acquireReadExclusive -> probeInvalidate, 
        acquireInvalidateOthers -> probeInvalidateOthers
      )))
  }
  def getProbeTypeOnVoluntaryWriteback: UInt = probeInvalidate

  def requiresOuterRead(acq: Acquire, m: MasterMetadata) = 
    Mux(acq.uncached, Acquire.requiresOuterRead(acq.a_type), acq.a_type != acquireInvalidateOthers)
  def requiresOuterWrite(acq: Acquire, m: MasterMetadata) =
    Mux(acq.uncached, Acquire.requiresOuterWrite(acq.a_type), Bool(false))

  def requiresAckForGrant(g: Grant) = g.uncached || g.g_type != grantVoluntaryAck
  def requiresAckForRelease(r: Release) = Bool(false)
  def requiresSelfProbe(a: Acquire) = a.uncached && a.a_type === Acquire.uncachedRead
  def requiresProbes(a: Acquire, m: MasterMetadata) = !dir.none(m.sharers)

  def pendingVoluntaryReleaseIsSufficient(r_type: UInt, p_type: UInt): Bool = (r_type === releaseVoluntaryInvalidateData)
}
