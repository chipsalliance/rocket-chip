// See LICENSE for license details.

package uncore
import Chisel._

/** The entire CoherencePolicy API consists of the following three traits:
  * HasCustomTileLinkMessageTypes, used to define custom messages
  * HasClientSideCoherencePolicy, for client coherence agents
  * HasManagerSideCoherencePolicy, for manager coherence agents
  */
abstract class CoherencePolicy(val dir: DirectoryRepresentation) extends 
  HasCustomTileLinkMessageTypes with
  HasClientSideCoherencePolicy with
  HasManagerSideCoherencePolicy

/** This API defines the custom, coherence-policy-defined message types,
  * as opposed to the built-in ones found in tilelink.scala.
  * Policies must enumerate the custom messages to be sent over each
  * channel, as well as which of them have associated data.
  */
trait HasCustomTileLinkMessageTypes {
  val nAcquireTypes: Int
  def acquireTypeWidth = log2Up(nAcquireTypes)
  val nProbeTypes: Int
  def probeTypeWidth = log2Up(nProbeTypes)
  val nReleaseTypes: Int
  def releaseTypeWidth = log2Up(nReleaseTypes)
  val nGrantTypes: Int
  def grantTypeWidth = log2Up(nGrantTypes)

  val acquireTypesWithData = Nil // Only built-in Acquire types have data for now
  def releaseTypesWithData: Vec[UInt]
  def grantTypesWithData: Vec[UInt]
}

/** This API contains all functions required for client coherence agents.
  * Policies must enumerate the number of client states and define their 
  * permissions with respect to memory operations. Policies must fill in functions
  * to control which messages are sent and how metadata is updated in response
  * to coherence events. These funtions are generally called from within the 
  * ClientMetadata class in metadata.scala
  */
trait HasClientSideCoherencePolicy {
  // Client coherence states and their permissions
  val nClientStates: Int
  def clientStateWidth = log2Ceil(nClientStates)
  def clientStatesWithReadPermission: Vec[UInt]
  def clientStatesWithWritePermission: Vec[UInt]
  def clientStatesWithDirtyData: Vec[UInt]

  // Transaction initiation logic
  def isValid(meta: ClientMetadata): Bool
  def isHit(cmd: UInt, meta: ClientMetadata): Bool = {
    Mux(isWriteIntent(cmd), 
      clientStatesWithWritePermission.contains(meta.state),
      clientStatesWithReadPermission.contains(meta.state))
  }
  //TODO: Assumes all states with write permissions also have read permissions
  def requiresAcquireOnSecondaryMiss(
        first_cmd: UInt,
        second_cmd: UInt,
        meta: ClientMetadata): Bool = {
    isWriteIntent(second_cmd) && !isWriteIntent(first_cmd)
  }
  //TODO: Assumes all cache ctrl ops writeback dirty data, and
  //      doesn't issue transaction when e.g. downgrading Exclusive to Shared:
  def requiresReleaseOnCacheControl(cmd: UInt, meta: ClientMetadata): Bool =
    clientStatesWithDirtyData.contains(meta.state) 

  // Determine which custom message type to use
  def getAcquireType(cmd: UInt, meta: ClientMetadata): UInt
  def getReleaseType(cmd: UInt, meta: ClientMetadata): UInt
  def getReleaseType(p: Probe, meta: ClientMetadata): UInt

  // Mutate ClientMetadata based on messages or cmds
  def clientMetadataOnReset: ClientMetadata
  def clientMetadataOnHit(cmd: UInt, meta: ClientMetadata): ClientMetadata
  def clientMetadataOnCacheControl(cmd: UInt, meta: ClientMetadata): ClientMetadata 
  def clientMetadataOnGrant(incoming: Grant, cmd: UInt, meta: ClientMetadata): ClientMetadata 
  def clientMetadataOnProbe(incoming: Probe, meta: ClientMetadata): ClientMetadata 
}

/** This API contains all functions required for manager coherence agents.
  * Policies must enumerate the number of manager states. Policies must fill
  * in functions to control which Probe and Grant messages are sent and how 
  * metadata should be updated in response to coherence events. These funtions
  * are generally called from within the ManagerMetadata class in metadata.scala
  */
trait HasManagerSideCoherencePolicy extends HasDirectoryRepresentation {
  val nManagerStates: Int
  def masterStateWidth = log2Ceil(nManagerStates)

  // Transaction probing logic
  def requiresProbes(acq: Acquire, meta: ManagerMetadata): Bool
  def requiresProbes(cmd: UInt, meta: ManagerMetadata): Bool

  // Determine which custom message type to use in response
  def getProbeType(cmd: UInt, meta: ManagerMetadata): UInt
  def getProbeType(acq: Acquire, meta: ManagerMetadata): UInt
  def getGrantType(acq: Acquire, meta: ManagerMetadata): UInt
  def getExclusiveGrantType(): UInt

  // Mutate ManagerMetadata based on messages or cmds
  def managerMetadataOnReset: ManagerMetadata
  def managerMetadataOnRelease(incoming: Release, src: UInt, meta: ManagerMetadata): ManagerMetadata
  def managerMetadataOnGrant(outgoing: Grant, dst: UInt, meta: ManagerMetadata) =
    ManagerMetadata(sharers=Mux(outgoing.isBuiltInType(), // Assumes all built-ins are uncached
                                meta.sharers,
                                dir.push(meta.sharers, dst)))
                    //state = meta.state)  TODO: Fix 0-width wires in Chisel
}

/** The following concrete implementations of CoherencePolicy each provide the 
  * functionality of one particular protocol.
  */

/** A simple protocol with only two Client states.
  * Data is always assumed to be dirty.
  * Only a single client may ever have a copy of a block at a time.
  */
class MICoherence(dir: DirectoryRepresentation) extends CoherencePolicy(dir) {
  // Message types
  val nAcquireTypes = 1
  val nProbeTypes = 2
  val nReleaseTypes = 4
  val nGrantTypes = 1

  val acquireExclusive :: Nil = Enum(UInt(), nAcquireTypes)
  val probeInvalidate :: probeCopy :: Nil = Enum(UInt(), nProbeTypes)
  val releaseInvalidateData :: releaseCopyData :: releaseInvalidateAck :: releaseCopyAck :: Nil = Enum(UInt(), nReleaseTypes)
  val grantExclusive :: Nil = Enum(UInt(), nGrantTypes)

  def releaseTypesWithData = Vec(releaseInvalidateData, releaseCopyData)
  def grantTypesWithData = Vec(grantExclusive)

  // Client states and functions
  val nClientStates = 2
  val clientInvalid :: clientValid :: Nil = Enum(UInt(), nClientStates)

  def clientStatesWithReadPermission = Vec(clientValid)
  def clientStatesWithWritePermission = Vec(clientValid)
  def clientStatesWithDirtyData = Vec(clientValid)

  def isValid (meta: ClientMetadata): Bool = meta.state != clientInvalid

  def getAcquireType(cmd: UInt, meta: ClientMetadata): UInt = acquireExclusive

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

  def clientMetadataOnReset = ClientMetadata(clientInvalid)

  def clientMetadataOnHit(cmd: UInt, meta: ClientMetadata) = meta

  def clientMetadataOnCacheControl(cmd: UInt, meta: ClientMetadata) =
    ClientMetadata(Mux(cmd === M_FLUSH, clientInvalid, meta.state))

  def clientMetadataOnGrant(incoming: Grant, cmd: UInt, meta: ClientMetadata) =
    ClientMetadata(Mux(incoming.isBuiltInType(), clientInvalid, clientValid))

  def clientMetadataOnProbe(incoming: Probe, meta: ClientMetadata) =
    ClientMetadata(Mux(incoming.p_type === probeInvalidate,
                       clientInvalid, meta.state))

  // Manager states and functions:
  val nManagerStates = 0 // We don't actually need any states for this protocol

  def requiresProbes(a: Acquire, meta: ManagerMetadata) = !dir.none(meta.sharers)

  def requiresProbes(cmd: UInt, meta: ManagerMetadata) = !dir.none(meta.sharers)

  def getProbeType(cmd: UInt, meta: ManagerMetadata): UInt =
    MuxLookup(cmd, probeCopy, Array(
      M_FLUSH -> probeInvalidate))

  def getProbeType(a: Acquire, meta: ManagerMetadata): UInt =
    Mux(a.isBuiltInType(), 
      MuxLookup(a.a_type, probeCopy, Array(
        Acquire.getBlockType -> probeCopy, 
        Acquire.putBlockType -> probeInvalidate,
        Acquire.getType -> probeCopy, 
        Acquire.putType -> probeInvalidate,
        Acquire.putAtomicType -> probeInvalidate)), 
      probeInvalidate)

  def getGrantType(a: Acquire, meta: ManagerMetadata): UInt = grantExclusive
  def getExclusiveGrantType(): UInt = grantExclusive

  def managerMetadataOnReset = ManagerMetadata()

  def managerMetadataOnRelease(incoming: Release, src: UInt, meta: ManagerMetadata) = {
    val popped = ManagerMetadata(sharers=dir.pop(meta.sharers, src))
    MuxBundle(meta, Array(
      incoming.is(releaseInvalidateData) -> popped,
      incoming.is(releaseInvalidateAck)  -> popped))
  }
}

/** A simple protocol with only three Client states.
  * Data is marked as dirty when written.
  * Only a single client may ever have a copy of a block at a time.
  */
class MEICoherence(dir: DirectoryRepresentation) extends CoherencePolicy(dir) {
  // Message types
  val nAcquireTypes = 2
  val nProbeTypes = 3
  val nReleaseTypes = 6
  val nGrantTypes = 1

  val acquireExclusiveClean :: acquireExclusiveDirty :: Nil = Enum(UInt(), nAcquireTypes)
  val probeInvalidate :: probeDowngrade :: probeCopy :: Nil = Enum(UInt(), nProbeTypes)
  val releaseInvalidateData :: releaseDowngradeData :: releaseCopyData :: releaseInvalidateAck :: releaseDowngradeAck :: releaseCopyAck :: Nil = Enum(UInt(), nReleaseTypes)
  val grantExclusive :: Nil = Enum(UInt(), nGrantTypes)

  def releaseTypesWithData = Vec(releaseInvalidateData, releaseDowngradeData, releaseCopyData)
  def grantTypesWithData = Vec(grantExclusive)

  // Client states and functions
  val nClientStates = 3
  val clientInvalid :: clientExclusiveClean :: clientExclusiveDirty :: Nil = Enum(UInt(), nClientStates)

  def clientStatesWithReadPermission = Vec(clientExclusiveClean, clientExclusiveDirty)
  def clientStatesWithWritePermission = Vec(clientExclusiveClean, clientExclusiveDirty)
  def clientStatesWithDirtyData = Vec(clientExclusiveDirty)

  def isValid (meta: ClientMetadata) = meta.state != clientInvalid

  def getAcquireType(cmd: UInt, meta: ClientMetadata): UInt =
    Mux(isWriteIntent(cmd), acquireExclusiveDirty, acquireExclusiveClean)

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

  def clientMetadataOnReset = ClientMetadata(clientInvalid)

  def clientMetadataOnHit(cmd: UInt, meta: ClientMetadata) = 
    ClientMetadata(Mux(isWrite(cmd), clientExclusiveDirty, meta.state))

  def clientMetadataOnCacheControl(cmd: UInt, meta: ClientMetadata) =
    ClientMetadata(
      MuxLookup(cmd, meta.state, Array(
        M_FLUSH -> clientInvalid,
        M_CLEAN -> Mux(meta.state === clientExclusiveDirty, clientExclusiveClean, meta.state))))

  def clientMetadataOnGrant(incoming: Grant, cmd: UInt, meta: ClientMetadata) =
    ClientMetadata(
      Mux(incoming.isBuiltInType(), clientInvalid,
        Mux(isWrite(cmd), clientExclusiveDirty, clientExclusiveClean)))

  def clientMetadataOnProbe(incoming: Probe, meta: ClientMetadata) =
    ClientMetadata(
      MuxLookup(incoming.p_type, meta.state, Array(
        probeInvalidate -> clientInvalid,
        probeDowngrade  -> clientExclusiveClean,
        probeCopy       -> meta.state)))

  // Manager states and functions:
  val nManagerStates = 0 // We don't actually need any states for this protocol

  def requiresProbes(a: Acquire, meta: ManagerMetadata) = !dir.none(meta.sharers)
  def requiresProbes(cmd: UInt, meta: ManagerMetadata) = !dir.none(meta.sharers)

  def getProbeType(cmd: UInt, meta: ManagerMetadata): UInt =
    MuxLookup(cmd, probeCopy, Array(
      M_FLUSH -> probeInvalidate,
      M_PRODUCE -> probeDowngrade))

  def getProbeType(a: Acquire, meta: ManagerMetadata): UInt =
    Mux(a.isBuiltInType(), 
      MuxLookup(a.a_type, probeCopy, Array(
        Acquire.getBlockType -> probeCopy, 
        Acquire.putBlockType -> probeInvalidate,
        Acquire.getType -> probeCopy, 
        Acquire.putType -> probeInvalidate,
        Acquire.putAtomicType -> probeInvalidate)),
      probeInvalidate)

  def getGrantType(a: Acquire, meta: ManagerMetadata): UInt = grantExclusive
  def getExclusiveGrantType(): UInt = grantExclusive

  def managerMetadataOnReset = ManagerMetadata()

  def managerMetadataOnRelease(incoming: Release, src: UInt, meta: ManagerMetadata) = {
    val popped = ManagerMetadata(sharers=dir.pop(meta.sharers, src))
    MuxBundle(meta, Array(
      incoming.is(releaseInvalidateData) -> popped,
      incoming.is(releaseInvalidateAck)  -> popped))
  }
}

/** A protocol with only three Client states.
  * Data is always assumed to be dirty.
  * Multiple clients may share read permissions on a block at the same time.
  */
class MSICoherence(dir: DirectoryRepresentation) extends CoherencePolicy(dir) {
  // Message types
  val nAcquireTypes = 2
  val nProbeTypes = 3
  val nReleaseTypes = 6
  val nGrantTypes = 3

  val acquireShared :: acquireExclusive :: Nil = Enum(UInt(), nAcquireTypes)
  val probeInvalidate :: probeDowngrade :: probeCopy :: Nil = Enum(UInt(), nProbeTypes)
  val releaseInvalidateData :: releaseDowngradeData :: releaseCopyData :: releaseInvalidateAck :: releaseDowngradeAck :: releaseCopyAck :: Nil = Enum(UInt(), nReleaseTypes)
  val grantShared :: grantExclusive :: grantExclusiveAck :: Nil = Enum(UInt(), nGrantTypes)

  def releaseTypesWithData = Vec(releaseInvalidateData, releaseDowngradeData, releaseCopyData)
  def grantTypesWithData = Vec(grantShared, grantExclusive)

  // Client states and functions
  val nClientStates = 3
  val clientInvalid :: clientShared :: clientExclusiveDirty :: Nil = Enum(UInt(), nClientStates)

  def clientStatesWithReadPermission = Vec(clientShared, clientExclusiveDirty)
  def clientStatesWithWritePermission = Vec(clientExclusiveDirty)
  def clientStatesWithDirtyData = Vec(clientExclusiveDirty)

  def isValid(meta: ClientMetadata): Bool = meta.state != clientInvalid

  def getAcquireType(cmd: UInt, meta: ClientMetadata): UInt =
    Mux(isWriteIntent(cmd), acquireExclusive, acquireShared)

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

  def clientMetadataOnReset = ClientMetadata(clientInvalid)

  def clientMetadataOnHit(cmd: UInt, meta: ClientMetadata) =
    ClientMetadata(Mux(isWrite(cmd), clientExclusiveDirty, meta.state))

  def clientMetadataOnCacheControl(cmd: UInt, meta: ClientMetadata) =
    ClientMetadata(
      MuxLookup(cmd, meta.state, Array(
        M_FLUSH   -> clientInvalid,
        M_PRODUCE -> Mux(clientStatesWithWritePermission.contains(meta.state), 
                      clientShared, meta.state))))

  def clientMetadataOnGrant(incoming: Grant, cmd: UInt, meta: ClientMetadata) =
    ClientMetadata(
      Mux(incoming.isBuiltInType(), clientInvalid,
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

  // Manager states and functions:
  val nManagerStates = 0 // TODO: We could add a Shared state to avoid probing
                         //        only a single sharer (also would need 
                         //        notification msg to track clean drops)
                         //        Also could avoid probes on outer WBs.

  def requiresProbes(a: Acquire, meta: ManagerMetadata) =
    Mux(dir.none(meta.sharers), Bool(false), 
      Mux(dir.one(meta.sharers), Bool(true), //TODO: for now we assume it's Exclusive
        Mux(a.isBuiltInType(), a.hasData(), a.a_type != acquireShared)))

  def requiresProbes(cmd: UInt, meta: ManagerMetadata) = !dir.none(meta.sharers)

  def getProbeType(cmd: UInt, meta: ManagerMetadata): UInt =
    MuxLookup(cmd, probeCopy, Array(
      M_FLUSH -> probeInvalidate,
      M_PRODUCE -> probeDowngrade))

  def getProbeType(a: Acquire, meta: ManagerMetadata): UInt =
    Mux(a.isBuiltInType(), 
      MuxLookup(a.a_type, probeCopy, Array(
        Acquire.getBlockType -> probeCopy, 
        Acquire.putBlockType -> probeInvalidate,
        Acquire.getType -> probeCopy, 
        Acquire.putType -> probeInvalidate,
        Acquire.putAtomicType -> probeInvalidate)),
      MuxLookup(a.a_type, probeCopy, Array(
        acquireShared -> probeDowngrade,
        acquireExclusive -> probeInvalidate)))

  def getGrantType(a: Acquire, meta: ManagerMetadata): UInt =
    Mux(a.a_type === acquireShared,
      Mux(!dir.none(meta.sharers), grantShared, grantExclusive),
      grantExclusive)
  def getExclusiveGrantType(): UInt = grantExclusive

  def managerMetadataOnReset = ManagerMetadata()

  def managerMetadataOnRelease(incoming: Release, src: UInt, meta: ManagerMetadata) = {
    val popped = ManagerMetadata(sharers=dir.pop(meta.sharers, src))
    MuxBundle(meta, Array(
      incoming.is(releaseInvalidateData) -> popped,
      incoming.is(releaseInvalidateAck)  -> popped))
  }
}

/** A protocol with four Client states.
  * Data is marked as dirty when written.
  * Multiple clients may share read permissions on a block at the same time.
  */
class MESICoherence(dir: DirectoryRepresentation) extends CoherencePolicy(dir) {
  // Message types
  val nAcquireTypes = 2
  val nProbeTypes = 3
  val nReleaseTypes = 6
  val nGrantTypes = 3

  val acquireShared :: acquireExclusive :: Nil = Enum(UInt(), nAcquireTypes)
  val probeInvalidate :: probeDowngrade :: probeCopy :: Nil = Enum(UInt(), nProbeTypes)
  val releaseInvalidateData :: releaseDowngradeData :: releaseCopyData :: releaseInvalidateAck :: releaseDowngradeAck :: releaseCopyAck :: Nil = Enum(UInt(), nReleaseTypes)
  val grantShared :: grantExclusive :: grantExclusiveAck :: Nil = Enum(UInt(), nGrantTypes)

  def releaseTypesWithData = Vec(releaseInvalidateData, releaseDowngradeData, releaseCopyData)
  def grantTypesWithData = Vec(grantShared, grantExclusive)

  // Client states and functions
  val nClientStates = 4
  val clientInvalid :: clientShared :: clientExclusiveClean :: clientExclusiveDirty :: Nil = Enum(UInt(), nClientStates)

  def clientStatesWithReadPermission = Vec(clientShared, clientExclusiveClean, clientExclusiveDirty)
  def clientStatesWithWritePermission = Vec(clientExclusiveClean, clientExclusiveDirty)
  def clientStatesWithDirtyData = Vec(clientExclusiveDirty)

  def isValid (meta: ClientMetadata): Bool = meta.state != clientInvalid

  def getAcquireType(cmd: UInt, meta: ClientMetadata): UInt =
    Mux(isWriteIntent(cmd), acquireExclusive, acquireShared)

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

  def clientMetadataOnReset = ClientMetadata(clientInvalid)

  def clientMetadataOnHit(cmd: UInt, meta: ClientMetadata) =
    ClientMetadata(Mux(isWrite(cmd), clientExclusiveDirty, meta.state))

  def clientMetadataOnCacheControl(cmd: UInt, meta: ClientMetadata) =
    ClientMetadata(
      MuxLookup(cmd, meta.state, Array(
        M_FLUSH   -> clientInvalid,
        M_PRODUCE -> Mux(clientStatesWithWritePermission.contains(meta.state), 
                      clientShared, meta.state),
        M_CLEAN   -> Mux(meta.state === clientExclusiveDirty, clientExclusiveClean, meta.state))))

  def clientMetadataOnGrant(incoming: Grant, cmd: UInt, meta: ClientMetadata) =
    ClientMetadata(
      Mux(incoming.isBuiltInType(), clientInvalid,
        MuxLookup(incoming.g_type, clientInvalid, Array(
          grantShared -> clientShared,
          grantExclusive -> Mux(isWrite(cmd), clientExclusiveDirty, clientExclusiveClean),
          grantExclusiveAck -> clientExclusiveDirty))))

  def clientMetadataOnProbe(incoming: Probe, meta: ClientMetadata) =
    ClientMetadata(
      MuxLookup(incoming.p_type, meta.state, Array(
        probeInvalidate -> clientInvalid,
        probeDowngrade  -> clientShared,
        probeCopy       -> meta.state)))

  // Manager states and functions:
  val nManagerStates = 0 // TODO: We could add a Shared state to avoid probing
                         //        only a single sharer (also would need 
                         //        notification msg to track clean drops)
                         //        Also could avoid probes on outer WBs.

  def requiresProbes(a: Acquire, meta: ManagerMetadata) =
    Mux(dir.none(meta.sharers), Bool(false), 
      Mux(dir.one(meta.sharers), Bool(true), //TODO: for now we assume it's Exclusive
        Mux(a.isBuiltInType(), a.hasData(), a.a_type != acquireShared)))

  def requiresProbes(cmd: UInt, meta: ManagerMetadata) = !dir.none(meta.sharers)

  def getProbeType(cmd: UInt, meta: ManagerMetadata): UInt =
    MuxLookup(cmd, probeCopy, Array(
      M_FLUSH -> probeInvalidate,
      M_PRODUCE -> probeDowngrade))

  def getProbeType(a: Acquire, meta: ManagerMetadata): UInt =
    Mux(a.isBuiltInType(), 
      MuxLookup(a.a_type, probeCopy, Array(
        Acquire.getBlockType -> probeCopy, 
        Acquire.putBlockType -> probeInvalidate,
        Acquire.getType -> probeCopy, 
        Acquire.putType -> probeInvalidate,
        Acquire.putAtomicType -> probeInvalidate)),
      MuxLookup(a.a_type, probeCopy, Array(
        acquireShared -> probeDowngrade,
        acquireExclusive -> probeInvalidate)))

  def getGrantType(a: Acquire, meta: ManagerMetadata): UInt =
    Mux(a.a_type === acquireShared,
      Mux(!dir.none(meta.sharers), grantShared, grantExclusive),
      grantExclusive)
  def getExclusiveGrantType(): UInt = grantExclusive

  def managerMetadataOnReset = ManagerMetadata()

  def managerMetadataOnRelease(incoming: Release, src: UInt, meta: ManagerMetadata) = {
    val popped = ManagerMetadata(sharers=dir.pop(meta.sharers, src))
    MuxBundle(meta, Array(
      incoming.is(releaseInvalidateData) -> popped,
      incoming.is(releaseInvalidateAck)  -> popped))
  }
}

class MigratoryCoherence(dir: DirectoryRepresentation) extends CoherencePolicy(dir) {
  // Message types
  val nAcquireTypes = 3
  val nProbeTypes = 4
  val nReleaseTypes = 10
  val nGrantTypes = 4

  val acquireShared :: acquireExclusive :: acquireInvalidateOthers :: Nil = Enum(UInt(), nAcquireTypes)
  val probeInvalidate :: probeDowngrade :: probeCopy :: probeInvalidateOthers :: Nil = Enum(UInt(), nProbeTypes)
  val releaseInvalidateData :: releaseDowngradeData :: releaseCopyData :: releaseInvalidateAck :: releaseDowngradeAck :: releaseCopyAck :: releaseDowngradeDataMigratory :: releaseDowngradeAckHasCopy :: releaseInvalidateDataMigratory :: releaseInvalidateAckMigratory :: Nil = Enum(UInt(), nReleaseTypes)
  val grantShared :: grantExclusive :: grantExclusiveAck :: grantReadMigratory :: Nil = Enum(UInt(), nGrantTypes)

  def releaseTypesWithData = Vec(releaseInvalidateData, releaseDowngradeData, releaseCopyData, releaseInvalidateDataMigratory, releaseDowngradeDataMigratory)
  def grantTypesWithData = Vec(grantShared, grantExclusive, grantReadMigratory)

  // Client states and functions
  val nClientStates = 7
  val clientInvalid :: clientShared :: clientExclusiveClean :: clientExclusiveDirty :: clientSharedByTwo :: clientMigratoryClean :: clientMigratoryDirty :: Nil = Enum(UInt(), nClientStates)

  def clientStatesWithReadPermission = Vec(clientShared, clientExclusiveClean, clientExclusiveDirty, clientSharedByTwo, clientMigratoryClean, clientMigratoryDirty)
  def clientStatesWithWritePermission = Vec(clientExclusiveClean, clientExclusiveDirty, clientMigratoryClean, clientMigratoryDirty)
  def clientStatesWithDirtyData = Vec(clientExclusiveDirty, clientMigratoryDirty)

  def isValid (meta: ClientMetadata): Bool = meta.state != clientInvalid

  def getAcquireType(cmd: UInt, meta: ClientMetadata): UInt =
    Mux(isWriteIntent(cmd), 
      Mux(meta.state === clientInvalid, acquireExclusive, acquireInvalidateOthers), 
      acquireShared)

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

  def clientMetadataOnReset = ClientMetadata(clientInvalid)

  def clientMetadataOnHit(cmd: UInt, meta: ClientMetadata) =
    ClientMetadata(
      Mux(isWrite(cmd), MuxLookup(meta.state, clientExclusiveDirty, Array(
                          clientExclusiveClean -> clientExclusiveDirty,
                          clientMigratoryClean -> clientMigratoryDirty)),
                        meta.state))

  def clientMetadataOnCacheControl(cmd: UInt, meta: ClientMetadata) =
    ClientMetadata(
      MuxLookup(cmd, meta.state, Array(
        M_FLUSH   -> clientInvalid,
        M_PRODUCE -> Mux(clientStatesWithWritePermission.contains(meta.state), 
                       clientShared, meta.state),
        M_CLEAN   -> MuxLookup(meta.state, meta.state, Array(
                       clientExclusiveDirty -> clientExclusiveClean,
                       clientMigratoryDirty -> clientMigratoryClean)))))

  def clientMetadataOnGrant(incoming: Grant, cmd: UInt, meta: ClientMetadata) =
    ClientMetadata(
      Mux(incoming.isBuiltInType(), clientInvalid,
        MuxLookup(incoming.g_type, clientInvalid, Array(
          grantShared        -> clientShared,
          grantExclusive     -> Mux(isWrite(cmd), clientExclusiveDirty, clientExclusiveClean),
          grantExclusiveAck  -> clientExclusiveDirty, 
          grantReadMigratory -> Mux(isWrite(cmd), clientMigratoryDirty, clientMigratoryClean)))))

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

  // Manager states and functions:
  val nManagerStates = 0 // TODO: we could add some states to reduce the number of message types

  def requiresProbes(a: Acquire, meta: ManagerMetadata) =
    Mux(dir.none(meta.sharers), Bool(false),
      Mux(dir.one(meta.sharers), Bool(true), //TODO: for now we assume it's Exclusive
        Mux(a.isBuiltInType(), a.hasData(), a.a_type != acquireShared)))

  def requiresProbes(cmd: UInt, meta: ManagerMetadata) = !dir.none(meta.sharers)

  def getProbeType(cmd: UInt, meta: ManagerMetadata): UInt =
    MuxLookup(cmd, probeCopy, Array(
      M_FLUSH -> probeInvalidate,
      M_PRODUCE -> probeDowngrade))

  def getProbeType(a: Acquire, meta: ManagerMetadata): UInt =
    Mux(a.isBuiltInType(), 
      MuxLookup(a.a_type, probeCopy, Array(
        Acquire.getBlockType -> probeCopy, 
        Acquire.putBlockType -> probeInvalidate,
        Acquire.getType -> probeCopy, 
        Acquire.putType -> probeInvalidate,
        Acquire.putAtomicType -> probeInvalidate)),
      MuxLookup(a.a_type, probeCopy, Array(
        acquireShared -> probeDowngrade,
        acquireExclusive -> probeInvalidate, 
        acquireInvalidateOthers -> probeInvalidateOthers)))

  def getGrantType(a: Acquire, meta: ManagerMetadata): UInt =
      MuxLookup(a.a_type, grantShared, Array(
        acquireShared    -> Mux(!dir.none(meta.sharers), grantShared, grantExclusive),
        acquireExclusive -> grantExclusive,                                            
        acquireInvalidateOthers -> grantExclusiveAck))  //TODO: add this to MESI for broadcast?
  def getExclusiveGrantType(): UInt = grantExclusive

  def managerMetadataOnReset = ManagerMetadata()

  def managerMetadataOnRelease(incoming: Release, src: UInt, meta: ManagerMetadata) = {
    val popped = ManagerMetadata(sharers=dir.pop(meta.sharers, src))
    MuxBundle(meta, Array(
      incoming.is(releaseInvalidateData) -> popped,
      incoming.is(releaseInvalidateAck)  -> popped,
      incoming.is(releaseInvalidateDataMigratory) -> popped,
      incoming.is(releaseInvalidateAckMigratory) -> popped))
  }
}
