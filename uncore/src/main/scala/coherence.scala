// See LICENSE for license details.

package uncore
import Chisel._

// Classes to represent coherence information in clients and managers
abstract class CoherenceMetadata extends Bundle with CoherenceAgentParameters

class ClientMetadata extends CoherenceMetadata {
  val state = UInt(width = co.clientStateWidth)
  def ===(right: ClientMetadata): Bool = this.state === right.state
  override def clone = new ClientMetadata().asInstanceOf[this.type]
}
object ClientMetadata {
  def apply(state: UInt) = {
    val meta = new ClientMetadata
    meta.state := state
    meta
  }
}

class ManagerMetadata extends CoherenceMetadata {
  val state = UInt(width = co.masterStateWidth)
  val sharers = UInt(width = co.dir.width)
  override def clone = new ManagerMetadata().asInstanceOf[this.type]
}
object ManagerMetadata {
  def apply(state: UInt, sharers: UInt) = {
    val meta = new ManagerMetadata
    meta.state := state
    meta.sharers := sharers
    meta
  }
  def apply(state: UInt): ManagerMetadata = apply(state, new ManagerMetadata().co.dir.flush)
}

// This class encapsulates transformations on different directory information
// storage formats
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

// Coherence policy inferface for clients and managers
abstract class CoherencePolicy(val dir: DirectoryRepresentation) {
  def nClientStates: Int
  def nManagerStates: Int
  def nAcquireTypes: Int
  def nProbeTypes: Int
  def nReleaseTypes: Int
  def nGrantTypes: Int
  def clientStateWidth = log2Up(nClientStates)
  def masterStateWidth = log2Up(nManagerStates)
  def acquireTypeWidth = log2Up(nAcquireTypes)
  def probeTypeWidth = log2Up(nProbeTypes)
  def releaseTypeWidth = log2Up(nReleaseTypes)
  def grantTypeWidth = log2Up(nGrantTypes)

  val clientStatesWithReadPermission: Vec[UInt]
  val clientStatesWithWritePermission: Vec[UInt]
  val clientStatesWithDirtyData: Vec[UInt]
  val acquireTypesWithData = Nil // Only built-in Acquire types have data for now
  val releaseTypesWithData: Vec[UInt] 
  val grantTypesWithData: Vec[UInt]

  def isValid(meta: ClientMetadata): Bool
  def isValid(meta: ManagerMetadata): Bool

  def isHit(cmd: UInt, meta: ClientMetadata): Bool = {
    Mux(isWriteIntent(cmd), 
      clientStatesWithWritePermission.contains(meta.state),
      clientStatesWithReadPermission.contains(meta.state))
  }
  //TODO: Use outer protocol's clientState instead, remove this function:
  def isHit(incoming: Acquire, meta: ManagerMetadata) = isValid(meta)

  def needsTransactionOnSecondaryMiss(cmd: UInt, outstanding: Acquire): Bool
  //TODO: Assumes all cache ctrl ops writeback dirty data, and
  //      doesn't issue transaction when e.g. downgrading Exclusive to Shared:
  def needsTransactionOnCacheControl(cmd: UInt, meta: ClientMetadata): Bool =
    clientStatesWithDirtyData.contains(meta.state) 
  def needsWriteback(meta: ClientMetadata): Bool =
    needsTransactionOnCacheControl(M_FLUSH, meta)

  def clientMetadataOnGrant(incoming: Grant, outstanding: Acquire): ClientMetadata 
  def clientMetadataOnProbe(incoming: Probe, meta: ClientMetadata): ClientMetadata 
  def clientMetadataOnHit(cmd: UInt, meta: ClientMetadata): ClientMetadata
  def clientMetadataOnCacheControl(cmd: UInt, meta: ClientMetadata): ClientMetadata 
  def clientMetadataOnFlush: ClientMetadata

  def managerMetadataOnRelease(incoming: Release, meta: ManagerMetadata, src: UInt): ManagerMetadata
  def managerMetadataOnGrant(outgoing: Grant, meta: ManagerMetadata, dst: UInt): ManagerMetadata
  def managerMetadataOnCacheControl(cmd: UInt, meta: ManagerMetadata): ManagerMetadata 
  def managerMetadataOnFlush: ManagerMetadata

  def getAcquireTypeOnPrimaryMiss(cmd: UInt, meta: ClientMetadata): UInt
  def getAcquireTypeOnSecondaryMiss(cmd: UInt, meta: ClientMetadata, outstanding: Acquire): UInt
  def getReleaseType(p: Probe, meta: ClientMetadata): UInt
  def getReleaseType(cmd: UInt, meta: ClientMetadata): UInt

  def getGrantType(acq: Acquire, meta: ManagerMetadata): UInt
  def getProbeType(acq: Acquire, meta: ManagerMetadata): UInt
  def getProbeType(cmd: UInt, meta: ManagerMetadata): UInt

  def requiresOuterRead(acq: Acquire, meta: ManagerMetadata): Bool
  def requiresOuterWrite(acq: Acquire, meta: ManagerMetadata): Bool
  def requiresProbes(acq: Acquire, meta: ManagerMetadata): Bool
  def requiresProbes(cmd: UInt, meta: ManagerMetadata): Bool
  def requiresProbesOnVoluntaryWriteback(meta: ManagerMetadata): Bool = requiresProbes(M_FLUSH, meta)
}

class MICoherence(dir: DirectoryRepresentation) extends CoherencePolicy(dir) {
  def nClientStates = 2
  def nManagerStates = 2
  def nAcquireTypes = 1
  def nProbeTypes = 2
  def nReleaseTypes = 4
  def nGrantTypes = 1

  val clientInvalid :: clientValid :: Nil = Enum(UInt(), nClientStates)
  val managerInvalid :: managerValid :: Nil = Enum(UInt(), nManagerStates)

  val acquireExclusive :: Nil = Enum(UInt(), nAcquireTypes)
  val probeInvalidate :: probeCopy :: Nil = Enum(UInt(), nProbeTypes)
  val releaseInvalidateData :: releaseCopyData :: releaseInvalidateAck :: releaseCopyAck :: Nil = Enum(UInt(), nReleaseTypes)
  val grantExclusive :: Nil = Enum(UInt(), nGrantTypes)

  val clientStatesWithReadPermission = Vec(clientValid)
  val clientStatesWithWritePermission = Vec(clientValid)
  val clientStatesWithDirtyData = Vec(clientValid)
  val releaseTypesWithData = Vec(releaseInvalidateData, releaseCopyData)
  val grantTypesWithData = Vec(grantExclusive)

  def isValid (meta: ClientMetadata): Bool = meta.state != clientInvalid
  def isValid (meta: ManagerMetadata): Bool = meta.state != managerInvalid

  def needsTransactionOnSecondaryMiss(cmd: UInt, outstanding: Acquire): Bool = 
    (outstanding.a_type != acquireExclusive)

  def clientMetadataOnHit(cmd: UInt, meta: ClientMetadata) = meta

  def clientMetadataOnFlush = ClientMetadata(clientInvalid)
  def clientMetadataOnCacheControl(cmd: UInt, meta: ClientMetadata) =
    ClientMetadata(Mux(cmd === M_FLUSH, clientInvalid, meta.state))

  def clientMetadataOnGrant(incoming: Grant, outstanding: Acquire) =
    ClientMetadata(Mux(incoming.builtin_type, clientInvalid, clientValid))

  def clientMetadataOnProbe(incoming: Probe, meta: ClientMetadata) =
    ClientMetadata(Mux(incoming.p_type === probeInvalidate,
                       clientInvalid, meta.state))

  def managerMetadataOnRelease(r: Release, meta: ManagerMetadata, src: UInt) = {
    val next = ManagerMetadata(managerValid, dir.pop(meta.sharers, src))
    MuxBundle(meta, Array(
      r.is(releaseInvalidateData) -> next,
      r.is(releaseInvalidateAck) -> next
    ))
  }

  def managerMetadataOnGrant(g: Grant, meta: ManagerMetadata, dst: UInt) =
    Mux(g.builtin_type, 
      ManagerMetadata(managerValid, meta.sharers),
      ManagerMetadata(managerValid, dir.push(meta.sharers, dst)))

  def managerMetadataOnFlush = ManagerMetadata(managerInvalid)

  def managerMetadataOnCacheControl(cmd: UInt, meta: ManagerMetadata) =
    ManagerMetadata(Mux(cmd === M_FLUSH, managerInvalid, meta.state))

  def getAcquireTypeOnPrimaryMiss(cmd: UInt, meta: ClientMetadata): UInt = acquireExclusive

  def getAcquireTypeOnSecondaryMiss(cmd: UInt, meta: ClientMetadata, outstanding: Acquire): UInt = acquireExclusive

  def getReleaseType(cmd: UInt, meta: ClientMetadata): UInt = {
    val dirty = clientStatesWithDirtyData.contains(meta.state)
    MuxLookup(cmd, releaseCopyAck, Array(
      M_FLUSH   -> Mux(dirty, releaseInvalidateData, releaseInvalidateAck),
      M_PRODUCE -> Mux(dirty, releaseCopyData, releaseCopyAck),
      M_CLEAN   -> Mux(dirty, releaseCopyData, releaseCopyAck)))
  }

  def getReleaseType(incoming: Probe, meta: ClientMetadata): UInt =
    MuxLookup(incoming.p_type, releaseInvalidateAck, Array(
      probeInvalidate -> getReleaseType(M_FLUSH, meta),
      probeCopy       -> getReleaseType(M_CLEAN, meta)))

  def isCoherenceConflict(addr1: UInt, addr2: UInt): Bool = (addr1 === addr2)

  def getGrantType(a: Acquire, meta: ManagerMetadata): UInt = 
    Mux(a.builtin_type, Grant.getGrantTypeForUncached(a), grantExclusive)

  def getProbeType(a: Acquire, meta: ManagerMetadata): UInt =
    Mux(a.builtin_type, 
      MuxLookup(a.a_type, probeCopy, Array(
        Acquire.uncachedReadBlock -> probeCopy, 
        Acquire.uncachedWriteBlock -> probeInvalidate,
        Acquire.uncachedRead -> probeCopy, 
        Acquire.uncachedWrite -> probeInvalidate,
        Acquire.uncachedAtomic -> probeInvalidate)), 
      probeInvalidate)

  def getProbeType(cmd: UInt, meta: ManagerMetadata): UInt =
    MuxLookup(cmd, probeCopy, Array(
      M_FLUSH -> probeInvalidate))

  def requiresOuterRead(acq: Acquire, meta: ManagerMetadata) = 
    Mux(acq.builtin_type, Acquire.requiresOuterRead(acq.a_type), Bool(true))

  def requiresOuterWrite(acq: Acquire, meta: ManagerMetadata) =
    Mux(acq.builtin_type, Acquire.requiresOuterWrite(acq.a_type), Bool(false))

  def requiresProbes(a: Acquire, meta: ManagerMetadata) = !dir.none(meta.sharers)

  def requiresProbes(cmd: UInt, meta: ManagerMetadata) = !dir.none(meta.sharers)
}

class MEICoherence(dir: DirectoryRepresentation) extends CoherencePolicy(dir) {
  def nClientStates = 3
  def nManagerStates = 2
  def nAcquireTypes = 2
  def nProbeTypes = 3
  def nReleaseTypes = 6
  def nGrantTypes = 1

  val clientInvalid :: clientExclusiveClean :: clientExclusiveDirty :: Nil = Enum(UInt(), nClientStates)
  val managerInvalid :: managerValid :: Nil = Enum(UInt(), nManagerStates)

  val acquireExclusiveClean :: acquireExclusiveDirty :: Nil = Enum(UInt(), nAcquireTypes)
  val probeInvalidate :: probeDowngrade :: probeCopy :: Nil = Enum(UInt(), nProbeTypes)
  val releaseInvalidateData :: releaseDowngradeData :: releaseCopyData :: releaseInvalidateAck :: releaseDowngradeAck :: releaseCopyAck :: Nil = Enum(UInt(), nReleaseTypes)
  val grantExclusive :: Nil = Enum(UInt(), nGrantTypes)

  val clientStatesWithReadPermission = Vec(clientExclusiveClean, clientExclusiveDirty)
  val clientStatesWithWritePermission = Vec(clientExclusiveClean, clientExclusiveDirty)
  val clientStatesWithDirtyData = Vec(clientExclusiveDirty)
  val releaseTypesWithData = Vec(releaseInvalidateData, releaseDowngradeData, releaseCopyData)
  val grantTypesWithData = Vec(grantExclusive)

  def isValid (meta: ClientMetadata) = meta.state != clientInvalid
  def isValid (meta: ManagerMetadata) = meta.state != managerInvalid

  def needsTransactionOnSecondaryMiss(cmd: UInt, outstanding: Acquire): Bool =
    (isRead(cmd) && outstanding.builtin_type) ||
      (isWriteIntent(cmd) && (outstanding.a_type != acquireExclusiveDirty))

  def clientMetadataOnHit(cmd: UInt, meta: ClientMetadata) = 
    ClientMetadata(Mux(isWrite(cmd), clientExclusiveDirty, meta.state))

  def clientMetadataOnFlush = ClientMetadata(clientInvalid)
  def clientMetadataOnCacheControl(cmd: UInt, meta: ClientMetadata) =
    ClientMetadata(
      MuxLookup(cmd, meta.state, Array(
        M_FLUSH -> clientInvalid,
        M_CLEAN -> Mux(meta.state === clientExclusiveDirty, clientExclusiveClean, meta.state))))

  def clientMetadataOnGrant(incoming: Grant, outstanding: Acquire) =
    ClientMetadata(
      Mux(incoming.builtin_type, clientInvalid,
        Mux(outstanding.a_type === acquireExclusiveDirty, clientExclusiveDirty, 
          clientExclusiveClean)))

  def clientMetadataOnProbe(incoming: Probe, meta: ClientMetadata) =
    ClientMetadata(
      MuxLookup(incoming.p_type, meta.state, Array(
        probeInvalidate -> clientInvalid,
        probeDowngrade  -> clientExclusiveClean,
        probeCopy       -> meta.state)))

  def managerMetadataOnRelease(r: Release, meta: ManagerMetadata, src: UInt) = {
    val next = ManagerMetadata(managerValid, dir.pop(meta.sharers,src))
    MuxBundle(meta, Array(
      r.is(releaseInvalidateData) -> next,
      r.is(releaseInvalidateAck) -> next
    ))
  }

  def managerMetadataOnGrant(g: Grant, meta: ManagerMetadata, dst: UInt) =
    Mux(g.builtin_type, 
      ManagerMetadata(managerValid, meta.sharers),
      ManagerMetadata(managerValid, dir.push(meta.sharers, dst)))

  def managerMetadataOnFlush = ManagerMetadata(managerInvalid)

  def managerMetadataOnCacheControl(cmd: UInt, meta: ManagerMetadata) = 
    ManagerMetadata(Mux(cmd === M_FLUSH, managerInvalid, meta.state))

  def getAcquireTypeOnPrimaryMiss(cmd: UInt, meta: ClientMetadata): UInt =
    Mux(isWriteIntent(cmd), acquireExclusiveDirty, acquireExclusiveClean)

  def getAcquireTypeOnSecondaryMiss(cmd: UInt, meta: ClientMetadata, outstanding: Acquire): UInt =
    Mux(isWriteIntent(cmd), acquireExclusiveDirty, outstanding.a_type)

  def getReleaseType(cmd: UInt, meta: ClientMetadata): UInt = {
    val dirty = clientStatesWithDirtyData.contains(meta.state)
    MuxLookup(cmd, releaseCopyAck, Array(
      M_FLUSH   -> Mux(dirty, releaseInvalidateData, releaseInvalidateAck),
      M_PRODUCE -> Mux(dirty, releaseDowngradeData, releaseDowngradeAck),
      M_CLEAN   -> Mux(dirty, releaseCopyData, releaseCopyAck)))
  }

  def getReleaseType(incoming: Probe, meta: ClientMetadata): UInt =
    MuxLookup(incoming.p_type, releaseInvalidateAck, Array(
      probeInvalidate -> getReleaseType(M_FLUSH, meta),
      probeDowngrade  -> getReleaseType(M_PRODUCE, meta),
      probeCopy       -> getReleaseType(M_CLEAN, meta)))

  def isCoherenceConflict(addr1: UInt, addr2: UInt): Bool = (addr1 === addr2)

  def getGrantType(a: Acquire, meta: ManagerMetadata): UInt =
    Mux(a.builtin_type, Grant.getGrantTypeForUncached(a), grantExclusive)

  def getProbeType(a: Acquire, meta: ManagerMetadata): UInt =
    Mux(a.builtin_type, 
      MuxLookup(a.a_type, probeCopy, Array(
        Acquire.uncachedReadBlock -> probeCopy, 
        Acquire.uncachedWriteBlock -> probeInvalidate,
        Acquire.uncachedRead -> probeCopy, 
        Acquire.uncachedWrite -> probeInvalidate,
        Acquire.uncachedAtomic -> probeInvalidate)),
      probeInvalidate)

  def getProbeType(cmd: UInt, meta: ManagerMetadata): UInt =
    MuxLookup(cmd, probeCopy, Array(
      M_FLUSH -> probeInvalidate,
      M_PRODUCE -> probeDowngrade))

  def requiresOuterRead(acq: Acquire, meta: ManagerMetadata) = 
    Mux(acq.builtin_type, Acquire.requiresOuterRead(acq.a_type), Bool(true))

  def requiresOuterWrite(acq: Acquire, meta: ManagerMetadata) =
    Mux(acq.builtin_type, Acquire.requiresOuterWrite(acq.a_type), Bool(false))

  def requiresProbes(a: Acquire, meta: ManagerMetadata) =
    Mux(dir.none(meta.sharers), Bool(false), 
      Mux(dir.one(meta.sharers), Bool(true), //TODO: for now we assume it's Exclusive
        Mux(a.builtin_type, a.hasData(), Bool(true))))

  def requiresProbes(cmd: UInt, meta: ManagerMetadata) = !dir.none(meta.sharers)
}

class MSICoherence(dir: DirectoryRepresentation) extends CoherencePolicy(dir) {
  def nClientStates = 3
  def nManagerStates = 2
  def nAcquireTypes = 2
  def nProbeTypes = 3
  def nReleaseTypes = 6
  def nGrantTypes = 3

  val clientInvalid :: clientShared :: clientExclusiveDirty :: Nil = Enum(UInt(), nClientStates)
  //val managerInvalid :: masterShared :: masterExclusive :: Nil = Enum(UInt(), nManagerStates)
  val managerInvalid :: managerValid :: Nil = Enum(UInt(), nManagerStates)

  val acquireShared :: acquireExclusive :: Nil = Enum(UInt(), nAcquireTypes)
  val probeInvalidate :: probeDowngrade :: probeCopy :: Nil = Enum(UInt(), nProbeTypes)
  val releaseInvalidateData :: releaseDowngradeData :: releaseCopyData :: releaseInvalidateAck :: releaseDowngradeAck :: releaseCopyAck :: Nil = Enum(UInt(), nReleaseTypes)
  val grantShared :: grantExclusive :: grantExclusiveAck :: Nil = Enum(UInt(), nGrantTypes)

  val clientStatesWithReadPermission = Vec(clientShared, clientExclusiveDirty)
  val clientStatesWithWritePermission = Vec(clientExclusiveDirty)
  val clientStatesWithDirtyData = Vec(clientExclusiveDirty)
  val releaseTypesWithData = Vec(releaseInvalidateData, releaseDowngradeData, releaseCopyData)
  val grantTypesWithData = Vec(grantShared, grantExclusive)

  def isValid(meta: ClientMetadata): Bool = meta.state != clientInvalid
  def isValid(meta: ManagerMetadata) = meta.state != managerInvalid

  def needsTransactionOnSecondaryMiss(cmd: UInt, outstanding: Acquire): Bool =
    (isRead(cmd) && outstanding.builtin_type) ||
      (isWriteIntent(cmd) && (outstanding.a_type != acquireExclusive))

  def clientMetadataOnHit(cmd: UInt, meta: ClientMetadata) =
    ClientMetadata(Mux(isWrite(cmd), clientExclusiveDirty, meta.state))

  def clientMetadataOnFlush = ClientMetadata(clientInvalid)

  def clientMetadataOnCacheControl(cmd: UInt, meta: ClientMetadata) =
    ClientMetadata(
      MuxLookup(cmd, meta.state, Array(
        M_FLUSH   -> clientInvalid,
        M_PRODUCE -> Mux(clientStatesWithWritePermission.contains(meta.state), 
                      clientShared, meta.state))))

  def clientMetadataOnGrant(incoming: Grant, outstanding: Acquire) =
    ClientMetadata(
      Mux(incoming.builtin_type, clientInvalid,
        MuxLookup(incoming.g_type, clientInvalid, Array(
          grantShared -> clientShared,
          grantExclusive -> clientExclusiveDirty,
          grantExclusiveAck -> clientExclusiveDirty))))

  def clientMetadataOnProbe(incoming: Probe, meta: ClientMetadata) = 
    ClientMetadata(
      MuxLookup(incoming.p_type, meta.state, Array(
        probeInvalidate -> clientInvalid,
        probeDowngrade  -> clientShared,
        probeCopy       -> meta.state)))

  def managerMetadataOnRelease(r: Release, meta: ManagerMetadata, src: UInt) = {
    val next = ManagerMetadata(managerValid, dir.pop(meta.sharers,src))
    MuxBundle(meta, Array(
      r.is(releaseInvalidateData) -> next,
      r.is(releaseInvalidateAck) -> next
    ))
  }

  def managerMetadataOnGrant(g: Grant, meta: ManagerMetadata, dst: UInt) =
    Mux(g.builtin_type, 
      ManagerMetadata(managerValid, meta.sharers),
      ManagerMetadata(managerValid, dir.push(meta.sharers, dst)))

  def managerMetadataOnFlush = ManagerMetadata(managerInvalid)

  def managerMetadataOnCacheControl(cmd: UInt, meta: ManagerMetadata) =
    ManagerMetadata(Mux(cmd === M_FLUSH, managerInvalid, meta.state))

  def getAcquireTypeOnPrimaryMiss(cmd: UInt, meta: ClientMetadata): UInt =
    Mux(isWriteIntent(cmd), acquireExclusive, acquireShared)

  def getAcquireTypeOnSecondaryMiss(cmd: UInt, meta: ClientMetadata, outstanding: Acquire): UInt =
    Mux(isWriteIntent(cmd), acquireExclusive, outstanding.a_type)

  def getReleaseType(cmd: UInt, meta: ClientMetadata): UInt = {
    val dirty = clientStatesWithDirtyData.contains(meta.state)
    MuxLookup(cmd, releaseCopyAck, Array(
      M_FLUSH   -> Mux(dirty, releaseInvalidateData, releaseInvalidateAck),
      M_PRODUCE -> Mux(dirty, releaseDowngradeData, releaseDowngradeAck),
      M_CLEAN   -> Mux(dirty, releaseCopyData, releaseCopyAck)))
  }

  def getReleaseType(incoming: Probe, meta: ClientMetadata): UInt =
    MuxLookup(incoming.p_type, releaseInvalidateAck, Array(
      probeInvalidate -> getReleaseType(M_FLUSH, meta),
      probeDowngrade  -> getReleaseType(M_PRODUCE, meta),
      probeCopy       -> getReleaseType(M_CLEAN, meta)))

  def isCoherenceConflict(addr1: UInt, addr2: UInt): Bool = (addr1 === addr2)

  def getGrantType(a: Acquire, meta: ManagerMetadata): UInt =
    Mux(a.builtin_type, Grant.getGrantTypeForUncached(a),
      Mux(a.a_type === acquireShared,
        Mux(!dir.none(meta.sharers), grantShared, grantExclusive),
        grantExclusive))

  def getProbeType(a: Acquire, meta: ManagerMetadata): UInt =
    Mux(a.builtin_type, 
      MuxLookup(a.a_type, probeCopy, Array(
        Acquire.uncachedReadBlock -> probeCopy, 
        Acquire.uncachedWriteBlock -> probeInvalidate,
        Acquire.uncachedRead -> probeCopy, 
        Acquire.uncachedWrite -> probeInvalidate,
        Acquire.uncachedAtomic -> probeInvalidate)),
      MuxLookup(a.a_type, probeCopy, Array(
        acquireShared -> probeDowngrade,
        acquireExclusive -> probeInvalidate)))

  def getProbeType(cmd: UInt, meta: ManagerMetadata): UInt =
    MuxLookup(cmd, probeCopy, Array(
      M_FLUSH -> probeInvalidate,
      M_PRODUCE -> probeDowngrade))

  def requiresOuterRead(acq: Acquire, meta: ManagerMetadata) =
    Mux(acq.builtin_type, Acquire.requiresOuterRead(acq.a_type), Bool(true))

  def requiresOuterWrite(acq: Acquire, meta: ManagerMetadata) =
    Mux(acq.builtin_type, Acquire.requiresOuterWrite(acq.a_type), Bool(false))

  def requiresProbes(a: Acquire, meta: ManagerMetadata) =
    Mux(dir.none(meta.sharers), Bool(false), 
      Mux(dir.one(meta.sharers), Bool(true), //TODO: for now we assume it's Exclusive
        Mux(a.builtin_type, a.hasData(), a.a_type != acquireShared)))

  def requiresProbes(cmd: UInt, meta: ManagerMetadata) = !dir.none(meta.sharers)
}

class MESICoherence(dir: DirectoryRepresentation) extends CoherencePolicy(dir) {
  def nClientStates = 4
  def nManagerStates = 2
  def nAcquireTypes = 2
  def nProbeTypes = 3
  def nReleaseTypes = 6
  def nGrantTypes = 3

  val clientInvalid :: clientShared :: clientExclusiveClean :: clientExclusiveDirty :: Nil = Enum(UInt(), nClientStates)
  //val managerInvalid :: masterShared :: masterExclusive :: Nil = Enum(UInt(), nManagerStates)
  val managerInvalid :: managerValid :: Nil = Enum(UInt(), nManagerStates)

  val acquireShared :: acquireExclusive :: Nil = Enum(UInt(), nAcquireTypes)
  val probeInvalidate :: probeDowngrade :: probeCopy :: Nil = Enum(UInt(), nProbeTypes)
  val releaseInvalidateData :: releaseDowngradeData :: releaseCopyData :: releaseInvalidateAck :: releaseDowngradeAck :: releaseCopyAck :: Nil = Enum(UInt(), nReleaseTypes)
  val grantShared :: grantExclusive :: grantExclusiveAck :: Nil = Enum(UInt(), nGrantTypes)

  val clientStatesWithReadPermission = Vec(clientShared, clientExclusiveClean, clientExclusiveDirty)
  val clientStatesWithWritePermission = Vec(clientExclusiveClean, clientExclusiveDirty)
  val clientStatesWithDirtyData = Vec(clientExclusiveDirty)
  val releaseTypesWithData = Vec(releaseInvalidateData, releaseDowngradeData, releaseCopyData)
  val grantTypesWithData = Vec(grantShared, grantExclusive)

  def isValid (meta: ClientMetadata): Bool = meta.state != clientInvalid
  def isValid (meta: ManagerMetadata) = meta.state != managerInvalid

  def needsTransactionOnSecondaryMiss(cmd: UInt, outstanding: Acquire): Bool =
    (isRead(cmd) && outstanding.builtin_type) ||
      (isWriteIntent(cmd) && (outstanding.a_type != acquireExclusive))

  def clientMetadataOnHit(cmd: UInt, meta: ClientMetadata) =
    ClientMetadata(Mux(isWrite(cmd), clientExclusiveDirty, meta.state))

  def clientMetadataOnFlush = ClientMetadata(clientInvalid)

  def clientMetadataOnCacheControl(cmd: UInt, meta: ClientMetadata) =
    ClientMetadata(
      MuxLookup(cmd, meta.state, Array(
        M_FLUSH   -> clientInvalid,
        M_PRODUCE -> Mux(clientStatesWithWritePermission.contains(meta.state), 
                      clientShared, meta.state),
        M_CLEAN   -> Mux(meta.state === clientExclusiveDirty, clientExclusiveClean, meta.state))))

  def clientMetadataOnGrant(incoming: Grant, outstanding: Acquire) =
    ClientMetadata(
      Mux(incoming.builtin_type, clientInvalid,
        MuxLookup(incoming.g_type, clientInvalid, Array(
          grantShared -> clientShared,
          grantExclusive -> Mux(outstanding.a_type === acquireExclusive, 
                                  clientExclusiveDirty, clientExclusiveClean),
          grantExclusiveAck -> clientExclusiveDirty))))

  def clientMetadataOnProbe(incoming: Probe, meta: ClientMetadata) =
    ClientMetadata(
      MuxLookup(incoming.p_type, meta.state, Array(
        probeInvalidate -> clientInvalid,
        probeDowngrade  -> clientShared,
        probeCopy       -> meta.state)))

  def managerMetadataOnRelease(r: Release, meta: ManagerMetadata, src: UInt) = {
    val next = ManagerMetadata(managerValid, dir.pop(meta.sharers,src))
    MuxBundle(meta, Array(
      r.is(releaseInvalidateData) -> next,
      r.is(releaseInvalidateAck) -> next
    ))
  }

  def managerMetadataOnGrant(g: Grant, meta: ManagerMetadata, dst: UInt) =
    Mux(g.builtin_type, 
      ManagerMetadata(managerValid, meta.sharers),
      ManagerMetadata(managerValid, dir.push(meta.sharers, dst)))

  def managerMetadataOnFlush = ManagerMetadata(managerInvalid)

  def managerMetadataOnCacheControl(cmd: UInt, meta: ManagerMetadata) =
    ManagerMetadata(Mux(cmd === M_FLUSH, managerInvalid, meta.state))

  def getAcquireTypeOnPrimaryMiss(cmd: UInt, meta: ClientMetadata): UInt =
    Mux(isWriteIntent(cmd), acquireExclusive, acquireShared)

  def getAcquireTypeOnSecondaryMiss(cmd: UInt, meta: ClientMetadata, outstanding: Acquire): UInt =
    Mux(isWriteIntent(cmd), acquireExclusive, outstanding.a_type)
  
  def getReleaseType(cmd: UInt, meta: ClientMetadata): UInt = {
    val dirty = clientStatesWithDirtyData.contains(meta.state)
    MuxLookup(cmd, releaseCopyAck, Array(
      M_FLUSH   -> Mux(dirty, releaseInvalidateData, releaseInvalidateAck),
      M_PRODUCE -> Mux(dirty, releaseDowngradeData, releaseDowngradeAck),
      M_CLEAN   -> Mux(dirty, releaseCopyData, releaseCopyAck)))
  }

  def getReleaseType(incoming: Probe, meta: ClientMetadata): UInt =
    MuxLookup(incoming.p_type, releaseInvalidateAck, Array(
      probeInvalidate -> getReleaseType(M_FLUSH, meta),
      probeDowngrade  -> getReleaseType(M_PRODUCE, meta),
      probeCopy       -> getReleaseType(M_CLEAN, meta)))

  def isCoherenceConflict(addr1: UInt, addr2: UInt): Bool = (addr1 === addr2)

  def getGrantType(a: Acquire, meta: ManagerMetadata): UInt =
    Mux(a.builtin_type, Grant.getGrantTypeForUncached(a),
      Mux(a.a_type === acquireShared,
        Mux(!dir.none(meta.sharers), grantShared, grantExclusive),
        grantExclusive))

  def getProbeType(a: Acquire, meta: ManagerMetadata): UInt =
    Mux(a.builtin_type, 
      MuxLookup(a.a_type, probeCopy, Array(
        Acquire.uncachedReadBlock -> probeCopy, 
        Acquire.uncachedWriteBlock -> probeInvalidate,
        Acquire.uncachedRead -> probeCopy, 
        Acquire.uncachedWrite -> probeInvalidate,
        Acquire.uncachedAtomic -> probeInvalidate)),
      MuxLookup(a.a_type, probeCopy, Array(
        acquireShared -> probeDowngrade,
        acquireExclusive -> probeInvalidate)))

  def getProbeType(cmd: UInt, meta: ManagerMetadata): UInt =
    MuxLookup(cmd, probeCopy, Array(
      M_FLUSH -> probeInvalidate,
      M_PRODUCE -> probeDowngrade))

  def requiresOuterRead(acq: Acquire, meta: ManagerMetadata) =
    Mux(acq.builtin_type, Acquire.requiresOuterRead(acq.a_type), Bool(true))

  def requiresOuterWrite(acq: Acquire, meta: ManagerMetadata) =
    Mux(acq.builtin_type, Acquire.requiresOuterWrite(acq.a_type), Bool(false))

  def requiresProbes(a: Acquire, meta: ManagerMetadata) =
    Mux(dir.none(meta.sharers), Bool(false), 
      Mux(dir.one(meta.sharers), Bool(true), //TODO: for now we assume it's Exclusive
        Mux(a.builtin_type, a.hasData(), a.a_type != acquireShared)))

  def requiresProbes(cmd: UInt, meta: ManagerMetadata) = !dir.none(meta.sharers)
}

class MigratoryCoherence(dir: DirectoryRepresentation) extends CoherencePolicy(dir) {
  def nClientStates = 7
  def nManagerStates = 2
  def nAcquireTypes = 3
  def nProbeTypes = 4
  def nReleaseTypes = 10
  def nGrantTypes = 4

  val clientInvalid :: clientShared :: clientExclusiveClean :: clientExclusiveDirty :: clientSharedByTwo :: clientMigratoryClean :: clientMigratoryDirty :: Nil = Enum(UInt(), nClientStates)
  //val managerInvalid :: masterShared :: masterExclusive :: Nil = Enum(UInt(), nManagerStates)
  val managerInvalid :: managerValid :: Nil = Enum(UInt(), nManagerStates)

  val acquireShared :: acquireExclusive :: acquireInvalidateOthers :: Nil = Enum(UInt(), nAcquireTypes)
  val probeInvalidate :: probeDowngrade :: probeCopy :: probeInvalidateOthers :: Nil = Enum(UInt(), nProbeTypes)
  val releaseInvalidateData :: releaseDowngradeData :: releaseCopyData :: releaseInvalidateAck :: releaseDowngradeAck :: releaseCopyAck :: releaseDowngradeDataMigratory :: releaseDowngradeAckHasCopy :: releaseInvalidateDataMigratory :: releaseInvalidateAckMigratory :: Nil = Enum(UInt(), nReleaseTypes)
  val grantShared :: grantExclusive :: grantExclusiveAck :: grantReadMigratory :: Nil = Enum(UInt(), nGrantTypes)

  val clientStatesWithReadPermission = Vec(clientShared, clientExclusiveClean, clientExclusiveDirty, clientSharedByTwo, clientMigratoryClean, clientMigratoryDirty)
  val clientStatesWithWritePermission = Vec(clientExclusiveClean, clientExclusiveDirty, clientMigratoryClean, clientMigratoryDirty)
  val clientStatesWithDirtyData = Vec(clientExclusiveDirty, clientMigratoryDirty)
  val releaseTypesWithData = Vec(releaseInvalidateData, releaseDowngradeData, releaseCopyData, releaseInvalidateDataMigratory, releaseDowngradeDataMigratory)
  val grantTypesWithData = Vec(grantShared, grantExclusive, grantReadMigratory)

  def isValid (meta: ClientMetadata): Bool = meta.state != clientInvalid
  def isValid (meta: ManagerMetadata) = meta.state != managerInvalid

  def needsTransactionOnSecondaryMiss(cmd: UInt, outstanding: Acquire): Bool =
    (isRead(cmd) && outstanding.builtin_type) ||
      (isWriteIntent(cmd) && !Vec(acquireExclusive, acquireInvalidateOthers).contains(outstanding.a_type)) 

  def clientMetadataOnHit(cmd: UInt, meta: ClientMetadata) =
    ClientMetadata(
      Mux(isWrite(cmd), MuxLookup(meta.state, clientExclusiveDirty, Array(
                          clientExclusiveClean -> clientExclusiveDirty,
                          clientMigratoryClean -> clientMigratoryDirty)),
                        meta.state))

  def clientMetadataOnFlush = ClientMetadata(clientInvalid)

  def clientMetadataOnCacheControl(cmd: UInt, meta: ClientMetadata) =
    ClientMetadata(
      MuxLookup(cmd, meta.state, Array(
        M_FLUSH   -> clientInvalid,
        M_PRODUCE -> Mux(clientStatesWithWritePermission.contains(meta.state), 
                       clientShared, meta.state),
        M_CLEAN   -> MuxLookup(meta.state, meta.state, Array(
                       clientExclusiveDirty -> clientExclusiveClean,
                       clientMigratoryDirty -> clientMigratoryClean)))))

  def clientMetadataOnGrant(incoming: Grant, outstanding: Acquire) =
    ClientMetadata(
      Mux(incoming.builtin_type, clientInvalid,
        MuxLookup(incoming.g_type, clientInvalid, Array(
          grantShared       -> clientShared,
          grantExclusive    -> MuxLookup(outstanding.a_type, clientExclusiveDirty, Array(
                                 acquireExclusive -> clientExclusiveDirty,
                                 acquireShared -> clientExclusiveClean)),
          grantExclusiveAck  -> clientExclusiveDirty, 
          grantReadMigratory -> MuxLookup(outstanding.a_type, clientMigratoryDirty, Array(
                                  acquireInvalidateOthers -> clientMigratoryDirty,
                                  acquireExclusive -> clientMigratoryDirty,
                                  acquireShared -> clientMigratoryClean))))))

  def clientMetadataOnProbe(incoming: Probe, meta: ClientMetadata) =
    ClientMetadata(
      MuxLookup(incoming.p_type, meta.state, Array(
        probeInvalidate -> clientInvalid,
        probeInvalidateOthers -> clientInvalid,
        probeCopy -> meta.state,
        probeDowngrade -> MuxLookup(meta.state, clientShared, Array(
                                clientExclusiveClean -> clientSharedByTwo,
                                clientExclusiveDirty -> clientSharedByTwo,
                                clientSharedByTwo    -> clientShared,
                                clientMigratoryClean -> clientSharedByTwo,
                                clientMigratoryDirty -> clientInvalid)))))

  def managerMetadataOnRelease(r: Release, meta: ManagerMetadata, src: UInt) = {
    val next = ManagerMetadata(managerValid, dir.pop(meta.sharers,src))
    MuxBundle(meta, Array(
      r.is(releaseInvalidateData) -> next,
      r.is(releaseInvalidateAck) -> next,
      r.is(releaseInvalidateDataMigratory) -> next,
      r.is(releaseInvalidateAckMigratory) -> next))
  }

  def managerMetadataOnGrant(g: Grant, meta: ManagerMetadata, dst: UInt) =
    Mux(g.builtin_type, 
      ManagerMetadata(managerValid, meta.sharers),
      ManagerMetadata(managerValid, dir.push(meta.sharers, dst)))

  def managerMetadataOnFlush = ManagerMetadata(managerInvalid)

  def managerMetadataOnCacheControl(cmd: UInt, meta: ManagerMetadata) =
    ManagerMetadata(Mux(cmd === M_FLUSH, managerInvalid, meta.state))

  def getAcquireTypeOnPrimaryMiss(cmd: UInt, meta: ClientMetadata): UInt =
    Mux(isWriteIntent(cmd), 
      Mux(meta.state === clientInvalid, acquireExclusive, acquireInvalidateOthers), 
      acquireShared)

  def getAcquireTypeOnSecondaryMiss(cmd: UInt, meta: ClientMetadata, outstanding: Acquire): UInt =
    Mux(isWriteIntent(cmd), 
      Mux(meta.state === clientInvalid, acquireExclusive, acquireInvalidateOthers), 
      outstanding.a_type)

  def getReleaseType(cmd: UInt, meta: ClientMetadata): UInt = {
    val dirty = clientStatesWithDirtyData.contains(meta.state)
    MuxLookup(cmd, releaseCopyAck, Array(
      M_FLUSH   -> Mux(dirty, releaseInvalidateData, releaseInvalidateAck),
      M_PRODUCE -> Mux(dirty, releaseDowngradeData, releaseDowngradeAck),
      M_CLEAN   -> Mux(dirty, releaseCopyData, releaseCopyAck)))
  }

  def getReleaseType(incoming: Probe, meta: ClientMetadata): UInt = {
    val dirty = clientStatesWithDirtyData.contains(meta.state)
    val with_data = MuxLookup(incoming.p_type, releaseInvalidateData, Array(
      probeInvalidate -> Mux(Vec(clientExclusiveDirty, clientMigratoryDirty).contains(meta.state),
                          releaseInvalidateDataMigratory, releaseInvalidateData),
      probeDowngrade -> Mux(meta.state === clientMigratoryDirty,
                          releaseDowngradeDataMigratory, releaseDowngradeData),
      probeCopy -> releaseCopyData))
    val without_data = MuxLookup(incoming.p_type, releaseInvalidateAck, Array(
      probeInvalidate -> Mux(clientExclusiveClean === meta.state,
                           releaseInvalidateAckMigratory, releaseInvalidateAck),
      probeInvalidateOthers -> Mux(clientSharedByTwo === meta.state,
                                 releaseInvalidateAckMigratory, releaseInvalidateAck),
      probeDowngrade  -> Mux(meta.state != clientInvalid,
                           releaseDowngradeAckHasCopy, releaseDowngradeAck),
      probeCopy       -> releaseCopyAck))
    Mux(dirty, with_data, without_data)
  }

  def isCoherenceConflict(addr1: UInt, addr2: UInt): Bool = (addr1 === addr2)

  def getGrantType(a: Acquire, meta: ManagerMetadata): UInt =
    Mux(a.builtin_type, Grant.getGrantTypeForUncached(a),
      MuxLookup(a.a_type, grantShared, Array(
        acquireShared    -> Mux(!dir.none(meta.sharers), grantShared, grantExclusive),
        acquireExclusive -> grantExclusive,                                            
        acquireInvalidateOthers -> grantExclusiveAck)))  //TODO: add this to MESI for broadcast?

  def getProbeType(a: Acquire, meta: ManagerMetadata): UInt =
    Mux(a.builtin_type, 
      MuxLookup(a.a_type, probeCopy, Array(
        Acquire.uncachedReadBlock -> probeCopy, 
        Acquire.uncachedWriteBlock -> probeInvalidate,
        Acquire.uncachedRead -> probeCopy, 
        Acquire.uncachedWrite -> probeInvalidate,
        Acquire.uncachedAtomic -> probeInvalidate)),
      MuxLookup(a.a_type, probeCopy, Array(
        acquireShared -> probeDowngrade,
        acquireExclusive -> probeInvalidate, 
        acquireInvalidateOthers -> probeInvalidateOthers)))

  def getProbeType(cmd: UInt, meta: ManagerMetadata): UInt =
    MuxLookup(cmd, probeCopy, Array(
      M_FLUSH -> probeInvalidate,
      M_PRODUCE -> probeDowngrade))

  def requiresOuterRead(acq: Acquire, meta: ManagerMetadata) =
    Mux(acq.builtin_type, Acquire.requiresOuterRead(acq.a_type), acq.a_type != acquireInvalidateOthers)

  def requiresOuterWrite(acq: Acquire, meta: ManagerMetadata) =
    Mux(acq.builtin_type, Acquire.requiresOuterWrite(acq.a_type), Bool(false))

  def requiresProbes(a: Acquire, meta: ManagerMetadata) =
    Mux(dir.none(meta.sharers), Bool(false),
      Mux(dir.one(meta.sharers), Bool(true), //TODO: for now we assume it's Exclusive
        Mux(a.builtin_type, a.hasData(), a.a_type != acquireShared)))

  def requiresProbes(cmd: UInt, meta: ManagerMetadata) = !dir.none(meta.sharers)
}
