// See LICENSE for license details.

package uncore
import Chisel._
import junctions._
import scala.math.max
import cde.{Parameters, Field}

case object TLId extends Field[String]
case class TLKey(id: String) extends Field[TileLinkParameters]

/** Parameters exposed to the top-level design, set based on 
  * external requirements or design space exploration
  *
  * Coherency policy used to define custom mesage types
  * Number of manager agents
  * Number of client agents that cache data and use custom [[uncore.Acquire]] types
  * Number of client agents that do not cache data and use built-in [[uncore.Acquire]] types
  * Maximum number of unique outstanding transactions per client
  * Maximum number of clients multiplexed onto a single port
  * Maximum number of unique outstanding transactions per manager
  * Width of cache block addresses
  * Total amount of data per cache block
  * Number of data beats per cache block
  **/

case class TileLinkParameters(
    coherencePolicy: CoherencePolicy,
    nManagers: Int,
    nCachingClients: Int,
    nCachelessClients: Int,
    maxClientXacts: Int,
    maxClientsPerPort: Int,
    maxManagerXacts: Int,
    dataBits: Int,
    dataBeats: Int = 4,
    overrideDataBitsPerBeat: Option[Int] = None
    ) {
  val nClients = nCachingClients + nCachelessClients
  val writeMaskBits: Int  = ((dataBits / dataBeats) - 1) / 8 + 1
  val dataBitsPerBeat: Int = overrideDataBitsPerBeat.getOrElse(dataBits / dataBeats)
}

  
/** Utility trait for building Modules and Bundles that use TileLink parameters */
trait HasTileLinkParameters {
  implicit val p: Parameters
  val tlExternal = p(TLKey(p(TLId)))
  val tlCoh = tlExternal.coherencePolicy
  val tlNManagers = tlExternal.nManagers
  val tlNCachingClients = tlExternal.nCachingClients
  val tlNCachelessClients = tlExternal.nCachelessClients
  val tlNClients = tlExternal.nClients
  val tlClientIdBits =  log2Up(tlNClients)
  val tlManagerIdBits =  log2Up(tlNManagers)
  val tlMaxClientXacts = tlExternal.maxClientXacts
  val tlMaxClientsPerPort = tlExternal.maxClientsPerPort
  val tlMaxManagerXacts = tlExternal.maxManagerXacts
  val tlClientXactIdBits = log2Up(tlMaxClientXacts*tlMaxClientsPerPort)
  val tlManagerXactIdBits = log2Up(tlMaxManagerXacts)
  val tlBlockAddrBits = p(PAddrBits) - p(CacheBlockOffsetBits)
  val tlDataBeats = tlExternal.dataBeats
  val tlDataBits = tlExternal.dataBitsPerBeat
  val tlDataBytes = tlDataBits/8
  val tlWriteMaskBits = tlExternal.writeMaskBits
  val tlBeatAddrBits = log2Up(tlDataBeats)
  val tlByteAddrBits = log2Up(tlWriteMaskBits)
  val tlMemoryOpcodeBits = M_SZ
  val tlMemoryOperandSizeBits = MT_SZ
  val tlAcquireTypeBits = max(log2Up(Acquire.nBuiltInTypes), 
                              tlCoh.acquireTypeWidth)
  val tlAcquireUnionBits = max(tlWriteMaskBits,
                                 (tlByteAddrBits +
                                   tlMemoryOperandSizeBits +
                                   tlMemoryOpcodeBits)) + 1
  val tlGrantTypeBits = max(log2Up(Grant.nBuiltInTypes), 
                              tlCoh.grantTypeWidth) + 1
/** Whether the underlying physical network preserved point-to-point ordering of messages */
  val tlNetworkPreservesPointToPointOrdering = false
  val tlNetworkDoesNotInterleaveBeats = true
  val amoAluOperandBits = p(AmoAluOperandBits)
  val amoAluOperandBytes = amoAluOperandBits/8
}

abstract class TLModule(implicit val p: Parameters) extends Module
  with HasTileLinkParameters
abstract class TLBundle(implicit val p: Parameters) extends junctions.ParameterizedBundle()(p)
  with HasTileLinkParameters

/** Base trait for all TileLink channels */
abstract class TileLinkChannel(implicit p: Parameters) extends TLBundle()(p) {
  def hasData(dummy: Int = 0): Bool
  def hasMultibeatData(dummy: Int = 0): Bool
}
/** Directionality of message channel. Used to hook up logical network ports to physical network ports */
abstract class ClientToManagerChannel(implicit p: Parameters) extends TileLinkChannel()(p)
/** Directionality of message channel. Used to hook up logical network ports to physical network ports */
abstract class ManagerToClientChannel(implicit p: Parameters) extends TileLinkChannel()(p)
/** Directionality of message channel. Used to hook up logical network ports to physical network ports */
abstract class ClientToClientChannel(implicit p: Parameters) extends TileLinkChannel()(p) // Unused for now

/** Common signals that are used in multiple channels.
  * These traits are useful for type parameterizing bundle wiring functions.
  */

/** Address of a cache block. */
trait HasCacheBlockAddress extends HasTileLinkParameters {
  val addr_block = UInt(width = tlBlockAddrBits)

  def conflicts(that: HasCacheBlockAddress) = this.addr_block === that.addr_block
  def conflicts(addr: UInt) = this.addr_block === addr
}

/** Sub-block address or beat id of multi-beat data */
trait HasTileLinkBeatId extends HasTileLinkParameters {
  val addr_beat = UInt(width = tlBeatAddrBits)
}

/* Client-side transaction id. Usually Miss Status Handling Register File index */
trait HasClientTransactionId extends HasTileLinkParameters {
  val client_xact_id = Bits(width = tlClientXactIdBits)
}

/** Manager-side transaction id. Usually Transaction Status Handling Register File index. */
trait HasManagerTransactionId extends HasTileLinkParameters {
  val manager_xact_id = Bits(width = tlManagerXactIdBits)
}

/** A single beat of cache block data */
trait HasTileLinkData extends HasTileLinkBeatId {
  val data = UInt(width = tlDataBits)

  def hasData(dummy: Int = 0): Bool
  def hasMultibeatData(dummy: Int = 0): Bool
}

/** An entire cache block of data */
trait HasTileLinkBlock extends HasTileLinkParameters {
  val data_buffer = Vec(tlDataBeats, UInt(width = tlDataBits))
  val wmask_buffer = Vec(tlDataBeats, UInt(width = tlWriteMaskBits))
}

/** The id of a client source or destination. Used in managers. */
trait HasClientId extends HasTileLinkParameters {
  val client_id = UInt(width = tlClientIdBits)
}

trait HasAcquireUnion extends HasTileLinkParameters {
  val union = Bits(width = tlAcquireUnionBits)

  // Utility funcs for accessing subblock union:
  def isBuiltInType(t: UInt): Bool
  val opCodeOff = 1
  val opSizeOff = tlMemoryOpcodeBits + opCodeOff
  val addrByteOff = tlMemoryOperandSizeBits + opSizeOff
  val addrByteMSB = tlByteAddrBits + addrByteOff
  /** Hint whether to allocate the block in any interveneing caches */
  def allocate(dummy: Int = 0) = union(0)
  /** Op code for [[uncore.PutAtomic]] operations */
  def op_code(dummy: Int = 0) = Mux(
    isBuiltInType(Acquire.putType) || isBuiltInType(Acquire.putBlockType),
    M_XWR, union(opSizeOff-1, opCodeOff))
  /** Operand size for [[uncore.PutAtomic]] */
  def op_size(dummy: Int = 0) = union(addrByteOff-1, opSizeOff)
  /** Byte address for [[uncore.PutAtomic]] operand */
  def addr_byte(dummy: Int = 0) = union(addrByteMSB-1, addrByteOff)
  def amo_offset(dummy: Int = 0) = addr_byte()(tlByteAddrBits-1, log2Up(amoAluOperandBytes))
  /** Bit offset of [[uncore.PutAtomic]] operand */
  def amo_shift_bytes(dummy: Int = 0) = UInt(amoAluOperandBytes)*amo_offset()
  /** Write mask for [[uncore.Put]], [[uncore.PutBlock]], [[uncore.PutAtomic]] */
  def wmask(dummy: Int = 0): UInt = {
    Mux(isBuiltInType(Acquire.putAtomicType), 
      FillInterleaved(amoAluOperandBits/8, UIntToOH(amo_offset())),
      Mux(isBuiltInType(Acquire.putBlockType) || isBuiltInType(Acquire.putType),
        union(tlWriteMaskBits, 1),
        UInt(0, width = tlWriteMaskBits)))
  }
  /** Full, beat-sized writemask */
  def full_wmask(dummy: Int = 0) = FillInterleaved(8, wmask())
}

trait HasAcquireType extends HasTileLinkParameters {
  val is_builtin_type = Bool()
  val a_type = UInt(width = tlAcquireTypeBits)

  /** Message type equality */
  def is(t: UInt) = a_type === t //TODO: make this more opaque; def ===?

  /** Is this message a built-in or custom type */
  def isBuiltInType(dummy: Int = 0): Bool = is_builtin_type
  /** Is this message a particular built-in type */
  def isBuiltInType(t: UInt): Bool = is_builtin_type && a_type === t 

  /** Does this message refer to subblock operands using info in the Acquire.union subbundle */ 
  def isSubBlockType(dummy: Int = 0): Bool = isBuiltInType() && Acquire.typesOnSubBlocks.contains(a_type) 

  /** Is this message a built-in prefetch message */
  def isPrefetch(dummy: Int = 0): Bool = isBuiltInType() &&
                                           (is(Acquire.getPrefetchType) || is(Acquire.putPrefetchType))

  /** Does this message contain data? Assumes that no custom message types have data. */
  def hasData(dummy: Int = 0): Bool = isBuiltInType() && Acquire.typesWithData.contains(a_type)

  /** Does this message contain multiple beats of data? Assumes that no custom message types have data. */
  def hasMultibeatData(dummy: Int = 0): Bool = Bool(tlDataBeats > 1) && isBuiltInType() &&
                                           Acquire.typesWithMultibeatData.contains(a_type)

  /** Does this message require the manager to probe the client the very client that sent it?
    * Needed if multiple caches are attached to the same port.
    */
  def requiresSelfProbe(dummy: Int = 0) = Bool(false)

  /** Mapping between each built-in Acquire type and a built-in Grant type.  */
  def getBuiltInGrantType(dummy: Int = 0): UInt = Acquire.getBuiltInGrantType(this.a_type)
}

trait HasProbeType extends HasTileLinkParameters {
  val p_type = UInt(width = tlCoh.probeTypeWidth)

  def is(t: UInt) = p_type === t
  def hasData(dummy: Int = 0) = Bool(false)
  def hasMultibeatData(dummy: Int = 0) = Bool(false)
}

trait HasReleaseType extends HasTileLinkParameters {
  val voluntary = Bool()
  val r_type = UInt(width = tlCoh.releaseTypeWidth)

  def is(t: UInt) = r_type === t
  def hasData(dummy: Int = 0) = tlCoh.releaseTypesWithData.contains(r_type)
  def hasMultibeatData(dummy: Int = 0) = Bool(tlDataBeats > 1) &&
                                           tlCoh.releaseTypesWithData.contains(r_type)
  def isVoluntary(dummy: Int = 0) = voluntary
  def requiresAck(dummy: Int = 0) = !Bool(tlNetworkPreservesPointToPointOrdering)
}

trait HasGrantType extends HasTileLinkParameters {
  val is_builtin_type = Bool()
  val g_type = UInt(width = tlGrantTypeBits)

  // Helper funcs
  def isBuiltInType(dummy: Int = 0): Bool = is_builtin_type
  def isBuiltInType(t: UInt): Bool = is_builtin_type && g_type === t 
  def is(t: UInt):Bool = g_type === t
  def hasData(dummy: Int = 0): Bool = Mux(isBuiltInType(),
                                        Grant.typesWithData.contains(g_type),
                                        tlCoh.grantTypesWithData.contains(g_type))
  def hasMultibeatData(dummy: Int = 0): Bool = 
    Bool(tlDataBeats > 1) && Mux(isBuiltInType(),
                               Grant.typesWithMultibeatData.contains(g_type),
                               tlCoh.grantTypesWithData.contains(g_type))
  def isVoluntary(dummy: Int = 0): Bool = isBuiltInType() && (g_type === Grant.voluntaryAckType)
  def requiresAck(dummy: Int = 0): Bool = !Bool(tlNetworkPreservesPointToPointOrdering) && !isVoluntary()
}

/** TileLink channel bundle definitions */

/** The Acquire channel is used to intiate coherence protocol transactions in
  * order to gain access to a cache block's data with certain permissions
  * enabled. Messages sent over this channel may be custom types defined by
  * a [[uncore.CoherencePolicy]] for cached data accesse or may be built-in types
  * used for uncached data accesses. Acquires may contain data for Put or
  * PutAtomic built-in types. After sending an Acquire, clients must
  * wait for a manager to send them a [[uncore.Grant]] message in response.
  */
class AcquireMetadata(implicit p: Parameters) extends ClientToManagerChannel
    with HasCacheBlockAddress 
    with HasClientTransactionId
    with HasTileLinkBeatId
    with HasAcquireType
    with HasAcquireUnion {
  /** Complete physical address for block, beat or operand */
  def full_addr(dummy: Int = 0) = Cat(this.addr_block, this.addr_beat, this.addr_byte())
}

/** [[uncore.AcquireMetadata]] with an extra field containing the data beat */
class Acquire(implicit p: Parameters) extends AcquireMetadata
  with HasTileLinkData

/** [[uncore.AcquireMetadata]] with an extra field containing the entire cache block */
class BufferedAcquire(implicit p: Parameters) extends AcquireMetadata
  with HasTileLinkBlock

/** [[uncore.Acquire]] with an extra field stating its source id */
class AcquireFromSrc(implicit p: Parameters) extends Acquire
  with HasClientId

/** [[uncore.BufferedAcquire]] with an extra field stating its source id */
class BufferedAcquireFromSrc(implicit p: Parameters) extends BufferedAcquire
  with HasClientId 

/** Used to track metadata for transactions where multiple secondary misses have been merged
  * and handled by a single transaction tracker.
  */
class SecondaryMissInfo(implicit p: Parameters) extends TLBundle
  with HasClientTransactionId
  with HasTileLinkBeatId
  with HasClientId
  with HasAcquireType

/** Contains definitions of the the built-in Acquire types and a factory
  * for [[uncore.Acquire]]
  *
  * In general you should avoid using this factory directly and use
  * [[uncore.ClientMetadata.makeAcquire]] for custom cached Acquires and
  * [[uncore.Get]], [[uncore.Put]], etc. for built-in uncached Acquires.
  *
  * @param is_builtin_type built-in or custom type message?
  * @param a_type built-in type enum or custom type enum
  * @param client_xact_id client's transaction id
  * @param addr_block address of the cache block
  * @param addr_beat sub-block address (which beat)
  * @param data data being put outwards
  * @param union additional fields used for uncached types
  */
object Acquire {
  val nBuiltInTypes = 5
  //TODO: Use Enum
  def getType         = UInt("b000") // Get a single beat of data
  def getBlockType    = UInt("b001") // Get a whole block of data
  def putType         = UInt("b010") // Put a single beat of data
  def putBlockType    = UInt("b011") // Put a whole block of data
  def putAtomicType   = UInt("b100") // Perform an atomic memory op
  def getPrefetchType = UInt("b101") // Prefetch a whole block of data
  def putPrefetchType = UInt("b110") // Prefetch a whole block of data, with intent to write
  def typesWithData = Vec(putType, putBlockType, putAtomicType)
  def typesWithMultibeatData = Vec(putBlockType)
  def typesOnSubBlocks = Vec(putType, getType, putAtomicType)

  /** Mapping between each built-in Acquire type and a built-in Grant type. */
  def getBuiltInGrantType(a_type: UInt): UInt = {
    MuxLookup(a_type, Grant.putAckType, Array(
      Acquire.getType       -> Grant.getDataBeatType,
      Acquire.getBlockType  -> Grant.getDataBlockType,
      Acquire.putType       -> Grant.putAckType,
      Acquire.putBlockType  -> Grant.putAckType,
      Acquire.putAtomicType -> Grant.getDataBeatType,
      Acquire.getPrefetchType -> Grant.prefetchAckType,
      Acquire.putPrefetchType -> Grant.prefetchAckType))
  }

  def makeUnion(
        a_type: UInt,
        addr_byte: UInt,
        operand_size: UInt,
        opcode: UInt,
        wmask: UInt,
        alloc: Bool): UInt = {
    MuxLookup(a_type, UInt(0), Array(
      Acquire.getType       -> Cat(addr_byte, operand_size, opcode, alloc),
      Acquire.getBlockType  -> Cat(operand_size, opcode, alloc),
      Acquire.putType       -> Cat(wmask, alloc),
      Acquire.putBlockType  -> Cat(wmask, alloc),
      Acquire.putAtomicType -> Cat(addr_byte, operand_size, opcode, alloc),
      Acquire.getPrefetchType -> Cat(M_XRD, alloc),
      Acquire.putPrefetchType -> Cat(M_XWR, alloc)))
  }

  def fullWriteMask(implicit p: Parameters) = SInt(-1, width = p(TLKey(p(TLId))).writeMaskBits).toUInt

  // Most generic constructor
  def apply(
        is_builtin_type: Bool,
        a_type: Bits,
        client_xact_id: UInt,
        addr_block: UInt,
        addr_beat: UInt = UInt(0),
        data: UInt = UInt(0),
        union: UInt = UInt(0))
      (implicit p: Parameters): Acquire = {
    val acq = Wire(new Acquire)
    acq.is_builtin_type := is_builtin_type
    acq.a_type := a_type
    acq.client_xact_id := client_xact_id
    acq.addr_block := addr_block
    acq.addr_beat := addr_beat
    acq.data := data
    acq.union := union
    acq
  }

  // Copy constructor
  def apply(a: Acquire): Acquire = {
    val acq = Wire(new Acquire()(a.p))
    acq := a
    acq
  }
}

object BuiltInAcquireBuilder {
  def apply(
        a_type: UInt,
        client_xact_id: UInt,
        addr_block: UInt,
        addr_beat: UInt = UInt(0),
        data: UInt = UInt(0),
        addr_byte: UInt = UInt(0),
        operand_size: UInt = MT_Q,
        opcode: UInt = UInt(0),
        wmask: UInt = UInt(0),
        alloc: Bool = Bool(true))
      (implicit p: Parameters): Acquire = {
    Acquire(
        is_builtin_type = Bool(true),
        a_type = a_type,
        client_xact_id = client_xact_id,
        addr_block = addr_block,
        addr_beat = addr_beat,
        data = data,
        union = Acquire.makeUnion(a_type, addr_byte, operand_size, opcode, wmask, alloc))
  }
}

/** Get a single beat of data from the outer memory hierarchy
  *
  * The client can hint whether he block containing this beat should be 
  * allocated in the intervening levels of the hierarchy.
  *
  * @param client_xact_id client's transaction id
  * @param addr_block address of the cache block
  * @param addr_beat sub-block address (which beat)
  * @param addr_byte sub-block address (which byte)
  * @param operand_size {byte, half, word, double} from [[uncore.MemoryOpConstants]]
  * @param alloc hint whether the block should be allocated in intervening caches
  */
object Get {
  def apply(
        client_xact_id: UInt,
        addr_block: UInt,
        addr_beat: UInt,
        alloc: Bool = Bool(true))
      (implicit p: Parameters): Acquire = {
    BuiltInAcquireBuilder(
      a_type = Acquire.getType,
      client_xact_id = client_xact_id,
      addr_block = addr_block,
      addr_beat = addr_beat,
      opcode = M_XRD,
      alloc = alloc)
  }
  def apply(
        client_xact_id: UInt,
        addr_block: UInt,
        addr_beat: UInt,
        addr_byte: UInt,
        operand_size: UInt,
        alloc: Bool)
      (implicit p: Parameters): Acquire = {
    BuiltInAcquireBuilder(
      a_type = Acquire.getType,
      client_xact_id = client_xact_id,
      addr_block = addr_block,
      addr_beat = addr_beat,
      addr_byte = addr_byte, 
      operand_size = operand_size,
      opcode = M_XRD,
      alloc = alloc)
  }
}

/** Get a whole cache block of data from the outer memory hierarchy
  *
  * The client can hint whether the block should be allocated in the 
  * intervening levels of the hierarchy.
  *
  * @param client_xact_id client's transaction id
  * @param addr_block address of the cache block
  * @param alloc hint whether the block should be allocated in intervening caches
  */
object GetBlock {
  def apply(
        client_xact_id: UInt = UInt(0),
        addr_block: UInt,
        alloc: Bool = Bool(true))
      (implicit p: Parameters): Acquire = {
    BuiltInAcquireBuilder(
      a_type = Acquire.getBlockType,
      client_xact_id = client_xact_id, 
      addr_block = addr_block,
      opcode = M_XRD,
      alloc = alloc)
  }
}

/** Prefetch a cache block into the next-outermost level of the memory hierarchy
  * with read permissions.
  *
  * @param client_xact_id client's transaction id
  * @param addr_block address of the cache block
  */
object GetPrefetch {
  def apply(
       client_xact_id: UInt,
       addr_block: UInt)
      (implicit p: Parameters): Acquire = {
    BuiltInAcquireBuilder(
      a_type = Acquire.getPrefetchType,
      client_xact_id = client_xact_id,
      addr_block = addr_block)
  }
}

/** Put a single beat of data into the outer memory hierarchy
  *
  * The block will be allocated in the next-outermost level of the hierarchy.
  *
  * @param client_xact_id client's transaction id
  * @param addr_block address of the cache block
  * @param addr_beat sub-block address (which beat)
  * @param data data being refilled to the original requestor
  * @param wmask per-byte write mask for this beat
  * @param alloc hint whether the block should be allocated in intervening caches
  */
object Put {
  def apply(
        client_xact_id: UInt,
        addr_block: UInt,
        addr_beat: UInt,
        data: UInt,
        wmask: Option[UInt]= None,
        alloc: Bool = Bool(true))
      (implicit p: Parameters): Acquire = {
    BuiltInAcquireBuilder(
      a_type = Acquire.putType,
      addr_block = addr_block,
      addr_beat = addr_beat,
      client_xact_id = client_xact_id,
      data = data,
      wmask = wmask.getOrElse(Acquire.fullWriteMask),
      alloc = alloc)
  }
}

/** Put a whole cache block of data into the outer memory hierarchy
  *
  * If the write mask is not full, the block will be allocated in the
  * next-outermost level of the hierarchy. If the write mask is full, the
  * client can hint whether the block should be allocated or not.
  *
  * @param client_xact_id client's transaction id
  * @param addr_block address of the cache block
  * @param addr_beat sub-block address (which beat of several)
  * @param data data being refilled to the original requestor
  * @param wmask per-byte write mask for this beat
  * @param alloc hint whether the block should be allocated in intervening caches
  */
object PutBlock {
  def apply(
        client_xact_id: UInt,
        addr_block: UInt,
        addr_beat: UInt,
        data: UInt,
        wmask: UInt)
      (implicit p: Parameters): Acquire = {
    BuiltInAcquireBuilder(
      a_type = Acquire.putBlockType,
      client_xact_id = client_xact_id,
      addr_block = addr_block,
      addr_beat = addr_beat,
      data = data,
      wmask = wmask,
      alloc = Bool(true))
  }
  def apply(
        client_xact_id: UInt,
        addr_block: UInt,
        addr_beat: UInt,
        data: UInt,
        alloc: Bool = Bool(true))
      (implicit p: Parameters): Acquire = {
    BuiltInAcquireBuilder(
      a_type = Acquire.putBlockType,
      client_xact_id = client_xact_id,
      addr_block = addr_block,
      addr_beat = addr_beat,
      data = data,
      wmask = Acquire.fullWriteMask,
      alloc = alloc)
  }
}

/** Prefetch a cache block into the next-outermost level of the memory hierarchy
  * with write permissions.
  *
  * @param client_xact_id client's transaction id
  * @param addr_block address of the cache block
  */
object PutPrefetch {
  def apply(
        client_xact_id: UInt,
        addr_block: UInt)
      (implicit p: Parameters): Acquire = {
    BuiltInAcquireBuilder(
      a_type = Acquire.putPrefetchType,
      client_xact_id = client_xact_id,
      addr_block = addr_block)
  }
}

/** Perform an atomic memory operation in the next-outermost level of the memory hierarchy
  *
  * @param client_xact_id client's transaction id
  * @param addr_block address of the cache block
  * @param addr_beat sub-block address (within which beat)
  * @param addr_byte sub-block address (which byte)
  * @param atomic_opcode {swap, add, xor, and, min, max, minu, maxu} from [[uncore.MemoryOpConstants]]
  * @param operand_size {byte, half, word, double} from [[uncore.MemoryOpConstants]]
  * @param data source operand data
  */
object PutAtomic {
  def apply(
        client_xact_id: UInt,
        addr_block: UInt,
        addr_beat: UInt,
        addr_byte: UInt,
        atomic_opcode: UInt,
        operand_size: UInt,
        data: UInt)
      (implicit p: Parameters): Acquire = {
    BuiltInAcquireBuilder(
      a_type = Acquire.putAtomicType,
      client_xact_id = client_xact_id, 
      addr_block = addr_block, 
      addr_beat = addr_beat, 
      data = data,
      addr_byte = addr_byte,
      operand_size = operand_size,
      opcode = atomic_opcode)
  }
}

/** The Probe channel is used to force clients to release data or cede permissions
  * on a cache block. Clients respond to Probes with [[uncore.Release]] messages.
  * The available types of Probes are customized by a particular
  * [[uncore.CoherencePolicy]].
  */
class Probe(implicit p: Parameters) extends ManagerToClientChannel
  with HasCacheBlockAddress 
  with HasProbeType

/** [[uncore.Probe]] with an extra field stating its destination id */
class ProbeToDst(implicit p: Parameters) extends Probe()(p) with HasClientId

/** Contains factories for [[uncore.Probe]] and [[uncore.ProbeToDst]]
  *
  * In general you should avoid using these factories directly and use
  * [[uncore.ManagerMetadata.makeProbe(UInt,Acquire)* makeProbe]] instead.
  *
  * @param dst id of client to which probe should be sent
  * @param p_type custom probe type
  * @param addr_block address of the cache block
  */
object Probe {
  def apply(p_type: UInt, addr_block: UInt)(implicit p: Parameters): Probe = {
    val prb = Wire(new Probe)
    prb.p_type := p_type
    prb.addr_block := addr_block
    prb
  }
  def apply(dst: UInt, p_type: UInt, addr_block: UInt)(implicit p: Parameters): ProbeToDst = {
    val prb = Wire(new ProbeToDst)
    prb.client_id := dst
    prb.p_type := p_type
    prb.addr_block := addr_block
    prb
  }
}

/** The Release channel is used to release data or permission back to the manager
  * in response to [[uncore.Probe]] messages. It can also be used to voluntarily
  * write back data, for example in the event that dirty data must be evicted on
  * a cache miss. The available types of Release messages are always customized by
  * a particular [[uncore.CoherencePolicy]]. Releases may contain data or may be
  * simple acknowledgements. Voluntary Releases are acknowledged with [[uncore.Grant Grants]].
  */
class ReleaseMetadata(implicit p: Parameters) extends ClientToManagerChannel
    with HasTileLinkBeatId
    with HasCacheBlockAddress 
    with HasClientTransactionId 
    with HasReleaseType {
  def full_addr(dummy: Int = 0) = Cat(this.addr_block, this.addr_beat, UInt(0, width = tlByteAddrBits))
}

/** [[uncore.ReleaseMetadata]] with an extra field containing the data beat */
class Release(implicit p: Parameters) extends ReleaseMetadata
  with HasTileLinkData

/** [[uncore.ReleaseMetadata]] with an extra field containing the entire cache block */
class BufferedRelease(implicit p: Parameters) extends ReleaseMetadata
  with HasTileLinkBlock

/** [[uncore.Release]] with an extra field stating its source id */
class ReleaseFromSrc(implicit p: Parameters) extends Release
  with HasClientId

/** [[uncore.BufferedRelease]] with an extra field stating its source id */
class BufferedReleaseFromSrc(implicit p: Parameters) extends BufferedRelease
  with HasClientId

/** Contains a [[uncore.Release]] factory
  *
  * In general you should avoid using this factory directly and use
  * [[uncore.ClientMetadata.makeRelease]] instead.
  *
  * @param voluntary is this a voluntary writeback
  * @param r_type type enum defined by coherence protocol
  * @param client_xact_id client's transaction id
  * @param addr_block address of the cache block
  * @param addr_beat beat id of the data
  * @param data data being written back
  */
object Release {
  def apply(
        voluntary: Bool,
        r_type: UInt,
        client_xact_id: UInt,
        addr_block: UInt,
        addr_beat: UInt = UInt(0),
        data: UInt = UInt(0))
      (implicit p: Parameters): Release = {
    val rel = Wire(new Release)
    rel.r_type := r_type
    rel.client_xact_id := client_xact_id
    rel.addr_block := addr_block
    rel.addr_beat := addr_beat
    rel.data := data
    rel.voluntary := voluntary
    rel
  }
}

/** The Grant channel is used to refill data or grant permissions requested of the 
  * manager agent via an [[uncore.Acquire]] message. It is also used to acknowledge
  * the receipt of voluntary writeback from clients in the form of [[uncore.Release]]
  * messages. There are built-in Grant messages used for Gets and Puts, and
  * coherence policies may also define custom Grant types. Grants may contain data
  * or may be simple acknowledgements. Grants are responded to with [[uncore.Finish]].
  */
class GrantMetadata(implicit p: Parameters) extends ManagerToClientChannel
    with HasTileLinkBeatId
    with HasClientTransactionId 
    with HasManagerTransactionId
    with HasGrantType {
  def makeFinish(dummy: Int = 0): Finish = {
    val f = Wire(new Finish)
    f.manager_xact_id := this.manager_xact_id
    f
  }
}

/** [[uncore.GrantMetadata]] with an extra field containing a single beat of data */
class Grant(implicit p: Parameters) extends GrantMetadata
  with HasTileLinkData

/** [[uncore.Grant]] with an extra field stating its destination */
class GrantToDst(implicit p: Parameters) extends Grant
  with HasClientId

/** [[uncore.GrantMetadata]] with an extra field containing an entire cache block */
class BufferedGrant(implicit p: Parameters) extends GrantMetadata
  with HasTileLinkBlock

/** [[uncore.BufferedGrant]] with an extra field stating its destination */
class BufferedGrantToDst(implicit p: Parameters) extends BufferedGrant
  with HasClientId

/** Contains definitions of the the built-in grant types and factories 
  * for [[uncore.Grant]] and [[uncore.GrantToDst]]
  *
  * In general you should avoid using these factories directly and use
  * [[uncore.ManagerMetadata.makeGrant(uncore.AcquireFromSrc* makeGrant]] instead.
  *
  * @param dst id of client to which grant should be sent
  * @param is_builtin_type built-in or custom type message?
  * @param g_type built-in type enum or custom type enum
  * @param client_xact_id client's transaction id
  * @param manager_xact_id manager's transaction id
  * @param addr_beat beat id of the data
  * @param data data being refilled to the original requestor
  */
object Grant {
  val nBuiltInTypes = 5
  def voluntaryAckType = UInt("b000") // For acking Releases
  def prefetchAckType  = UInt("b001") // For acking any kind of Prefetch
  def putAckType       = UInt("b011") // For acking any kind of non-prfetch Put
  def getDataBeatType  = UInt("b100") // Supplying a single beat of Get
  def getDataBlockType = UInt("b101") // Supplying all beats of a GetBlock
  def typesWithData = Vec(getDataBlockType, getDataBeatType)
  def typesWithMultibeatData= Vec(getDataBlockType)

  def apply(
        is_builtin_type: Bool,
        g_type: UInt,
        client_xact_id: UInt, 
        manager_xact_id: UInt,
        addr_beat: UInt,
        data: UInt)
      (implicit p: Parameters): Grant = {
    val gnt = Wire(new Grant)
    gnt.is_builtin_type := is_builtin_type
    gnt.g_type := g_type
    gnt.client_xact_id := client_xact_id
    gnt.manager_xact_id := manager_xact_id
    gnt.addr_beat := addr_beat
    gnt.data := data
    gnt
  }

  def apply(
        dst: UInt,
        is_builtin_type: Bool,
        g_type: UInt,
        client_xact_id: UInt,
        manager_xact_id: UInt,
        addr_beat: UInt = UInt(0),
        data: UInt = UInt(0))
      (implicit p: Parameters): GrantToDst = {
    val gnt = Wire(new GrantToDst)
    gnt.client_id := dst
    gnt.is_builtin_type := is_builtin_type
    gnt.g_type := g_type
    gnt.client_xact_id := client_xact_id
    gnt.manager_xact_id := manager_xact_id
    gnt.addr_beat := addr_beat
    gnt.data := data
    gnt
  }
}

/** The Finish channel is used to provide a global ordering of transactions
  * in networks that do not guarantee point-to-point ordering of messages.
  * A Finsish message is sent as acknowledgement of receipt of a [[uncore.Grant]].
  * When a Finish message is received, a manager knows it is safe to begin
  * processing other transactions that touch the same cache block.
  */
class Finish(implicit p: Parameters) extends ClientToManagerChannel()(p)
    with HasManagerTransactionId {
  def hasData(dummy: Int = 0) = Bool(false)
  def hasMultibeatData(dummy: Int = 0) = Bool(false)
}

/** Complete IO definition for incoherent TileLink, including networking headers */
class UncachedTileLinkIO(implicit p: Parameters) extends TLBundle()(p) {
  val acquire   = new DecoupledIO(new LogicalNetworkIO(new Acquire))
  val grant     = new DecoupledIO(new LogicalNetworkIO(new Grant)).flip
  val finish = new DecoupledIO(new LogicalNetworkIO(new Finish))
}

/** Complete IO definition for coherent TileLink, including networking headers */
class TileLinkIO(implicit p: Parameters) extends UncachedTileLinkIO()(p) {
  val probe     = new DecoupledIO(new LogicalNetworkIO(new Probe)).flip
  val release   = new DecoupledIO(new LogicalNetworkIO(new Release))
}

/** This version of UncachedTileLinkIO does not contain network headers. 
  * It is intended for use within client agents.
  *
  * Headers are provided in the top-level that instantiates the clients and network,
  * probably using a [[uncore.ClientTileLinkNetworkPort]] module.
  * By eliding the header subbundles within the clients we can enable 
  * hierarchical P-and-R while minimizing unconnected port errors in GDS.
  *
  * Secondly, this version of the interface elides [[uncore.Finish]] messages, with the
  * assumption that a [[uncore.FinishUnit]] has been coupled to the TileLinkIO port
  * to deal with acking received [[uncore.Grant Grants]].
  */
class ClientUncachedTileLinkIO(implicit p: Parameters) extends TLBundle()(p) {
  val acquire   = new DecoupledIO(new Acquire)
  val grant     = new DecoupledIO(new Grant).flip
}

/** This version of TileLinkIO does not contain network headers. 
  * It is intended for use within client agents.
  */
class ClientTileLinkIO(implicit p: Parameters) extends ClientUncachedTileLinkIO()(p) {
  val probe     = new DecoupledIO(new Probe).flip
  val release   = new DecoupledIO(new Release)
}

/** This version of TileLinkIO does not contain network headers, but
  * every channel does include an extra client_id subbundle.
  * It is intended for use within Management agents.
  *
  * Managers need to track where [[uncore.Acquire]] and [[uncore.Release]] messages
  * originated so that they can send a [[uncore.Grant]] to the right place. 
  * Similarly they must be able to issues Probes to particular clients.
  * However, we'd still prefer to have [[uncore.ManagerTileLinkNetworkPort]] fill in
  * the header.src to enable hierarchical p-and-r of the managers. Additionally, 
  * coherent clients might be mapped to random network port ids, and we'll leave it to the
  * [[uncore.ManagerTileLinkNetworkPort]] to apply the correct mapping. Managers do need to
  * see Finished so they know when to allow new transactions on a cache
  * block to proceed.
  */
class ManagerTileLinkIO(implicit p: Parameters) extends TLBundle()(p) {
  val acquire   = new DecoupledIO(new AcquireFromSrc).flip
  val grant     = new DecoupledIO(new GrantToDst)
  val finish    = new DecoupledIO(new Finish).flip
  val probe     = new DecoupledIO(new ProbeToDst)
  val release   = new DecoupledIO(new ReleaseFromSrc).flip
}

/** Utilities for safely wrapping a *UncachedTileLink by pinning probe.ready and release.valid low */
object TileLinkIOWrapper {
  def apply(tl: ClientUncachedTileLinkIO)(implicit p: Parameters): ClientTileLinkIO = {
    val conv = Module(new ClientTileLinkIOWrapper)
    conv.io.in <> tl
    conv.io.out
  }
  def apply(tl: UncachedTileLinkIO)(implicit p: Parameters): TileLinkIO = {
    val conv = Module(new TileLinkIOWrapper)
    conv.io.in <> tl
    conv.io.out
  }
  def apply(tl: ClientTileLinkIO): ClientTileLinkIO = tl
  def apply(tl: TileLinkIO): TileLinkIO = tl
}

class TileLinkIOWrapper(implicit p: Parameters) extends TLModule()(p) {
  val io = new Bundle {
    val in = new UncachedTileLinkIO().flip
    val out = new TileLinkIO
  }
  io.out.acquire <> io.in.acquire
  io.in.grant <> io.out.grant
  io.out.finish <> io.in.finish
  io.out.probe.ready := Bool(true)
  io.out.release.valid := Bool(false)
}

class ClientTileLinkIOWrapper(implicit p: Parameters) extends TLModule()(p) {
  val io = new Bundle {
    val in = new ClientUncachedTileLinkIO().flip
    val out = new ClientTileLinkIO
  }
  io.out.acquire <> io.in.acquire
  io.in.grant <> io.out.grant
  io.out.probe.ready := Bool(true)
  io.out.release.valid := Bool(false)
}

/** A helper module that automatically issues [[uncore.Finish]] messages in repsonse
  * to [[uncore.Grant]] that it receives from a manager and forwards to a client
  */
class FinishUnit(srcId: Int = 0, outstanding: Int = 2)(implicit p: Parameters) extends TLModule()(p)
    with HasDataBeatCounters {
  val io = new Bundle {
    val grant = Decoupled(new LogicalNetworkIO(new Grant)).flip
    val refill = Decoupled(new Grant)
    val finish = Decoupled(new LogicalNetworkIO(new Finish))
    val ready = Bool(OUTPUT)
  }

  val g = io.grant.bits.payload

  if(tlNetworkPreservesPointToPointOrdering) {
    io.finish.valid := Bool(false)
    io.refill.valid := io.grant.valid
    io.refill.bits := g
    io.grant.ready := io.refill.ready
    io.ready := Bool(true)
  } else {
    // We only want to send Finishes after we have collected all beats of
    // a multibeat Grant. But Grants from multiple managers or transactions may
    // get interleaved, so we could need a counter for each.
    val done = if(tlNetworkDoesNotInterleaveBeats) {
      connectIncomingDataBeatCounterWithHeader(io.grant)
    } else {
      val entries = 1 << tlClientXactIdBits
      def getId(g: LogicalNetworkIO[Grant]) = g.payload.client_xact_id
      assert(getId(io.grant.bits) <= UInt(entries), "Not enough grant beat counters, only " + entries + " entries.")
      connectIncomingDataBeatCountersWithHeader(io.grant, entries, getId).reduce(_||_)
    }
    val q = Module(new FinishQueue(outstanding))
    q.io.enq.valid := io.grant.fire() && g.requiresAck() && (!g.hasMultibeatData() || done)
    q.io.enq.bits.fin := g.makeFinish()
    q.io.enq.bits.dst := io.grant.bits.header.src

    io.finish.bits.header.src := UInt(srcId)
    io.finish.bits.header.dst := q.io.deq.bits.dst
    io.finish.bits.payload := q.io.deq.bits.fin
    io.finish.valid := q.io.deq.valid
    q.io.deq.ready := io.finish.ready

    io.refill.valid := (q.io.enq.ready || !g.requiresAck()) && io.grant.valid
    io.refill.bits := g
    io.grant.ready := (q.io.enq.ready || !g.requiresAck()) && io.refill.ready
    io.ready := q.io.enq.ready
  }
}

class FinishQueueEntry(implicit p: Parameters) extends TLBundle()(p) {
    val fin = new Finish
    val dst = UInt(width = log2Up(p(LNEndpoints)))
}

class FinishQueue(entries: Int)(implicit p: Parameters) extends Queue(new FinishQueueEntry()(p), entries)

/** A port to convert [[uncore.ClientTileLinkIO]].flip into [[uncore.TileLinkIO]]
  *
  * Creates network headers for [[uncore.Acquire]] and [[uncore.Release]] messages,
  * calculating header.dst and filling in header.src.
  * Strips headers from [[uncore.Probe Probes]].
  * Responds to [[uncore.Grant]] by automatically issuing [[uncore.Finish]] to the granting managers.
  *
  * @param clientId network port id of this agent
  * @param addrConvert how a physical address maps to a destination manager port id
  */
class ClientTileLinkNetworkPort(clientId: Int, addrConvert: UInt => UInt)
                               (implicit p: Parameters) extends TLModule()(p) {
  val io = new Bundle {
    val client = new ClientTileLinkIO().flip
    val network = new TileLinkIO
  }

  val finisher = Module(new FinishUnit(clientId))
  finisher.io.grant <> io.network.grant
  io.network.finish <> finisher.io.finish

  val acq_with_header = ClientTileLinkHeaderCreator(io.client.acquire, clientId, addrConvert)
  val rel_with_header = ClientTileLinkHeaderCreator(io.client.release, clientId, addrConvert)
  val prb_without_header = DecoupledLogicalNetworkIOUnwrapper(io.network.probe)
  val gnt_without_header = finisher.io.refill

  io.network.acquire.bits := acq_with_header.bits
  io.network.acquire.valid := acq_with_header.valid && finisher.io.ready
  acq_with_header.ready := io.network.acquire.ready && finisher.io.ready
  io.network.release <> rel_with_header
  io.client.probe <> prb_without_header
  io.client.grant <> gnt_without_header
}

object ClientTileLinkHeaderCreator {
  def apply[T <: ClientToManagerChannel with HasCacheBlockAddress](
        in: DecoupledIO[T],
        clientId: Int,
        addrConvert: UInt => UInt)
      (implicit p: Parameters): DecoupledIO[LogicalNetworkIO[T]] = {
    val out = Wire(new DecoupledIO(new LogicalNetworkIO(in.bits)))
    out.bits.payload := in.bits
    out.bits.header.src := UInt(clientId)
    out.bits.header.dst := addrConvert(in.bits.addr_block)
    out.valid := in.valid
    in.ready := out.ready
    out
  }
}

/** A port to convert [[uncore.ManagerTileLinkIO]].flip into [[uncore.TileLinkIO]].flip
  *
  * Creates network headers for [[uncore.Probe]] and [[uncore.Grant]] messagess,
  * calculating header.dst and filling in header.src.
  * Strips headers from [[uncore.Acquire]], [[uncore.Release]] and [[uncore.Finish]],
  * but supplies client_id instead.
  *
  * @param managerId the network port id of this agent
  * @param idConvert how a sharer id maps to a destination client port id
  */
class ManagerTileLinkNetworkPort(managerId: Int, idConvert: UInt => UInt)
                                (implicit p: Parameters) extends TLModule()(p) {
  val io = new Bundle {
    val manager = new ManagerTileLinkIO().flip
    val network = new TileLinkIO().flip
  }
  io.network.grant <> ManagerTileLinkHeaderCreator(io.manager.grant, managerId, (u: UInt) => u)
  io.network.probe <> ManagerTileLinkHeaderCreator(io.manager.probe, managerId, idConvert)
  io.manager.acquire.bits.client_id := io.network.acquire.bits.header.src
  io.manager.acquire <> DecoupledLogicalNetworkIOUnwrapper(io.network.acquire)
  io.manager.release.bits.client_id := io.network.release.bits.header.src
  io.manager.release <> DecoupledLogicalNetworkIOUnwrapper(io.network.release)
  io.manager.finish <> DecoupledLogicalNetworkIOUnwrapper(io.network.finish)
}

object ManagerTileLinkHeaderCreator {
  def apply[T <: ManagerToClientChannel with HasClientId](
        in: DecoupledIO[T],
        managerId: Int,
        idConvert: UInt => UInt)
      (implicit p: Parameters): DecoupledIO[LogicalNetworkIO[T]] = {
    val out = Wire(new DecoupledIO(new LogicalNetworkIO(in.bits)))
    out.bits.payload := in.bits
    out.bits.header.src := UInt(managerId)
    out.bits.header.dst := idConvert(in.bits.client_id)
    out.valid := in.valid
    in.ready := out.ready
    out
  }
}

/** Struct for describing per-channel queue depths */
case class TileLinkDepths(acq: Int, prb: Int, rel: Int, gnt: Int, fin: Int)

/** Optionally enqueues each [[uncore.TileLinkChannel]] individually */
class TileLinkEnqueuer(depths: TileLinkDepths)(implicit p: Parameters) extends Module {
  val io = new Bundle {
    val client = new TileLinkIO().flip
    val manager = new TileLinkIO
  }
  io.manager.acquire <> (if(depths.acq > 0) Queue(io.client.acquire, depths.acq) else io.client.acquire)
  io.client.probe    <> (if(depths.prb > 0) Queue(io.manager.probe,  depths.prb) else io.manager.probe)
  io.manager.release <> (if(depths.rel > 0) Queue(io.client.release, depths.rel) else io.client.release)
  io.client.grant    <> (if(depths.gnt > 0) Queue(io.manager.grant,  depths.gnt) else io.manager.grant)
  io.manager.finish  <> (if(depths.fin > 0) Queue(io.client.finish,  depths.fin) else io.client.finish)
}

object TileLinkEnqueuer {
  def apply(in: TileLinkIO, depths: TileLinkDepths)(implicit p: Parameters): TileLinkIO = {
    val t = Module(new TileLinkEnqueuer(depths))
    t.io.client <> in
    t.io.manager
  }
  def apply(in: TileLinkIO, depth: Int)(implicit p: Parameters): TileLinkIO = {
    apply(in, TileLinkDepths(depth, depth, depth, depth, depth))
  }
}

class ClientTileLinkEnqueuer(depths: TileLinkDepths)(implicit p: Parameters) extends Module {
  val io = new Bundle {
    val inner = new ClientTileLinkIO().flip
    val outer = new ClientTileLinkIO
  }

  io.outer.acquire <> (if(depths.acq > 0) Queue(io.inner.acquire, depths.acq) else io.inner.acquire)
  io.inner.probe   <> (if(depths.prb > 0) Queue(io.outer.probe,   depths.prb) else io.outer.probe)
  io.outer.release <> (if(depths.rel > 0) Queue(io.inner.release, depths.rel) else io.inner.release)
  io.inner.grant   <> (if(depths.gnt > 0) Queue(io.outer.grant,   depths.gnt) else io.outer.grant)
}

object ClientTileLinkEnqueuer {
  def apply(in: ClientTileLinkIO, depths: TileLinkDepths)(implicit p: Parameters): ClientTileLinkIO = {
    val t = Module(new ClientTileLinkEnqueuer(depths))
    t.io.inner <> in
    t.io.outer
  }
  def apply(in: ClientTileLinkIO, depth: Int)(implicit p: Parameters): ClientTileLinkIO = {
    apply(in, TileLinkDepths(depth, depth, depth, depth, depth))
  }
}

/** Utility functions for constructing TileLinkIO arbiters */
trait TileLinkArbiterLike extends HasTileLinkParameters {
  // Some shorthand type variables
  type ManagerSourcedWithId = ManagerToClientChannel with HasClientTransactionId
  type ClientSourcedWithId = ClientToManagerChannel with HasClientTransactionId
  type ClientSourcedWithIdAndData = ClientToManagerChannel with HasClientTransactionId with HasTileLinkData

  val arbN: Int // The number of ports on the client side

  // These abstract funcs are filled in depending on whether the arbiter mucks with the 
  // outgoing client ids to track sourcing and then needs to revert them on the way back
  def clientSourcedClientXactId(in: ClientSourcedWithId, id: Int): Bits
  def managerSourcedClientXactId(in: ManagerSourcedWithId): Bits
  def arbIdx(in: ManagerSourcedWithId): UInt

  // The following functions are all wiring helpers for each of the different types of TileLink channels

  def hookupClientSource[M <: ClientSourcedWithIdAndData](
      clts: Seq[DecoupledIO[LogicalNetworkIO[M]]],
      mngr: DecoupledIO[LogicalNetworkIO[M]]) {
    def hasData(m: LogicalNetworkIO[M]) = m.payload.hasMultibeatData()
    val arb = Module(new LockingRRArbiter(mngr.bits, arbN, tlDataBeats, Some(hasData _)))
    clts.zipWithIndex.zip(arb.io.in).map{ case ((req, id), arb) => {
      arb.valid := req.valid
      arb.bits := req.bits
      arb.bits.payload.client_xact_id := clientSourcedClientXactId(req.bits.payload, id)
      req.ready := arb.ready
    }}
    mngr <> arb.io.out
  }

  def hookupClientSourceHeaderless[M <: ClientSourcedWithIdAndData](
      clts: Seq[DecoupledIO[M]],
      mngr: DecoupledIO[M]) {
    def hasData(m: M) = m.hasMultibeatData()
    val arb = Module(new LockingRRArbiter(mngr.bits, arbN, tlDataBeats, Some(hasData _)))
    clts.zipWithIndex.zip(arb.io.in).map{ case ((req, id), arb) => {
      arb.valid := req.valid
      arb.bits := req.bits
      arb.bits.client_xact_id := clientSourcedClientXactId(req.bits, id)
      req.ready := arb.ready
    }}
    mngr <> arb.io.out
  }

  def hookupManagerSourceWithHeader[M <: ManagerToClientChannel](
      clts: Seq[DecoupledIO[LogicalNetworkIO[M]]], 
      mngr: DecoupledIO[LogicalNetworkIO[M]]) {
    mngr.ready := Bool(false)
    for (i <- 0 until arbN) {
      clts(i).valid := Bool(false)
      when (mngr.bits.header.dst === UInt(i)) {
        clts(i).valid := mngr.valid
        mngr.ready := clts(i).ready
      }
      clts(i).bits := mngr.bits
    }
  }

  def hookupManagerSourceWithId[M <: ManagerSourcedWithId](
      clts: Seq[DecoupledIO[LogicalNetworkIO[M]]], 
      mngr: DecoupledIO[LogicalNetworkIO[M]]) {
    mngr.ready := Bool(false)
    for (i <- 0 until arbN) {
      clts(i).valid := Bool(false)
      when (arbIdx(mngr.bits.payload) === UInt(i)) {
        clts(i).valid := mngr.valid
        mngr.ready := clts(i).ready
      }
      clts(i).bits := mngr.bits
      clts(i).bits.payload.client_xact_id := managerSourcedClientXactId(mngr.bits.payload)
    }
  }

  def hookupManagerSourceHeaderlessWithId[M <: ManagerSourcedWithId](
      clts: Seq[DecoupledIO[M]], 
      mngr: DecoupledIO[M]) {
    mngr.ready := Bool(false)
    for (i <- 0 until arbN) {
      clts(i).valid := Bool(false)
      when (arbIdx(mngr.bits) === UInt(i)) {
        clts(i).valid := mngr.valid
        mngr.ready := clts(i).ready
      }
      clts(i).bits := mngr.bits
      clts(i).bits.client_xact_id := managerSourcedClientXactId(mngr.bits)
    }
  }

  def hookupManagerSourceBroadcast[M <: Data](clts: Seq[DecoupledIO[M]], mngr: DecoupledIO[M]) {
    clts.map{ _.valid := mngr.valid }
    clts.map{ _.bits := mngr.bits }
    mngr.ready := clts.map(_.ready).reduce(_&&_)
  }

  def hookupFinish[M <: LogicalNetworkIO[Finish]]( clts: Seq[DecoupledIO[M]], mngr: DecoupledIO[M]) {
    val arb = Module(new RRArbiter(mngr.bits, arbN))
    arb.io.in <> clts
    mngr <> arb.io.out
  }
}

/** Abstract base case for any Arbiters that have UncachedTileLinkIOs */
abstract class UncachedTileLinkIOArbiter(val arbN: Int)(implicit val p: Parameters) extends Module
    with TileLinkArbiterLike {
  val io = new Bundle {
    val in = Vec(new UncachedTileLinkIO, arbN).flip
    val out = new UncachedTileLinkIO
  }
  hookupClientSource(io.in.map(_.acquire), io.out.acquire)
  hookupFinish(io.in.map(_.finish), io.out.finish)
  hookupManagerSourceWithId(io.in.map(_.grant), io.out.grant)
}

/** Abstract base case for any Arbiters that have cached TileLinkIOs */
abstract class TileLinkIOArbiter(val arbN: Int)(implicit val p: Parameters) extends Module
    with TileLinkArbiterLike {
  val io = new Bundle {
    val in = Vec(new TileLinkIO, arbN).flip
    val out = new TileLinkIO
  }
  hookupClientSource(io.in.map(_.acquire), io.out.acquire)
  hookupClientSource(io.in.map(_.release), io.out.release)
  hookupFinish(io.in.map(_.finish), io.out.finish)
  hookupManagerSourceBroadcast(io.in.map(_.probe), io.out.probe)
  hookupManagerSourceWithId(io.in.map(_.grant), io.out.grant)
}

/** Appends the port index of the arbiter to the client_xact_id */
trait AppendsArbiterId extends TileLinkArbiterLike {
  def clientSourcedClientXactId(in: ClientSourcedWithId, id: Int) =
    Cat(in.client_xact_id, UInt(id, log2Up(arbN)))
  def managerSourcedClientXactId(in: ManagerSourcedWithId) = 
    in.client_xact_id >> log2Up(arbN)
  def arbIdx(in: ManagerSourcedWithId) = in.client_xact_id(log2Up(arbN)-1,0).toUInt
}

/** Uses the client_xact_id as is (assumes it has been set to port index) */
trait PassesId extends TileLinkArbiterLike {
  def clientSourcedClientXactId(in: ClientSourcedWithId, id: Int) = in.client_xact_id
  def managerSourcedClientXactId(in: ManagerSourcedWithId) = in.client_xact_id
  def arbIdx(in: ManagerSourcedWithId) = in.client_xact_id
}

/** Overwrites some default client_xact_id with the port idx */
trait UsesNewId extends TileLinkArbiterLike {
  def clientSourcedClientXactId(in: ClientSourcedWithId, id: Int) = UInt(id, log2Up(arbN))
  def managerSourcedClientXactId(in: ManagerSourcedWithId) = UInt(0)
  def arbIdx(in: ManagerSourcedWithId) = in.client_xact_id
}

// Now we can mix-in thevarious id-generation traits to make concrete arbiter classes
class UncachedTileLinkIOArbiterThatAppendsArbiterId(val n: Int)(implicit p: Parameters) extends UncachedTileLinkIOArbiter(n)(p) with AppendsArbiterId
class UncachedTileLinkIOArbiterThatPassesId(val n: Int)(implicit p: Parameters) extends UncachedTileLinkIOArbiter(n)(p) with PassesId
class UncachedTileLinkIOArbiterThatUsesNewId(val n: Int)(implicit p: Parameters) extends UncachedTileLinkIOArbiter(n)(p) with UsesNewId
class TileLinkIOArbiterThatAppendsArbiterId(val n: Int)(implicit p: Parameters) extends TileLinkIOArbiter(n)(p) with AppendsArbiterId
class TileLinkIOArbiterThatPassesId(val n: Int)(implicit p: Parameters) extends TileLinkIOArbiter(n)(p) with PassesId
class TileLinkIOArbiterThatUsesNewId(val n: Int)(implicit p: Parameters) extends TileLinkIOArbiter(n)(p) with UsesNewId

/** Concrete uncached client-side arbiter that appends the arbiter's port id to client_xact_id */
class ClientUncachedTileLinkIOArbiter(val arbN: Int)(implicit val p: Parameters) extends Module with TileLinkArbiterLike with AppendsArbiterId {
  val io = new Bundle {
    val in = Vec(new ClientUncachedTileLinkIO, arbN).flip
    val out = new ClientUncachedTileLinkIO
  }
  hookupClientSourceHeaderless(io.in.map(_.acquire), io.out.acquire)
  hookupManagerSourceHeaderlessWithId(io.in.map(_.grant), io.out.grant)
}

/** Concrete client-side arbiter that appends the arbiter's port id to client_xact_id */
class ClientTileLinkIOArbiter(val arbN: Int)(implicit val p: Parameters) extends Module with TileLinkArbiterLike with AppendsArbiterId {
  val io = new Bundle {
    val in = Vec(new ClientTileLinkIO, arbN).flip
    val out = new ClientTileLinkIO
  }
  hookupClientSourceHeaderless(io.in.map(_.acquire), io.out.acquire)
  hookupClientSourceHeaderless(io.in.map(_.release), io.out.release)
  hookupManagerSourceBroadcast(io.in.map(_.probe), io.out.probe)
  hookupManagerSourceHeaderlessWithId(io.in.map(_.grant), io.out.grant)
}

/** Utility trait containing wiring functions to keep track of how many data beats have 
  * been sent or recieved over a particular [[uncore.TileLinkChannel]] or pair of channels. 
  *
  * Won't count message types that don't have data. 
  * Used in [[uncore.XactTracker]] and [[uncore.FinishUnit]].
  */
trait HasDataBeatCounters {
  type HasBeat = TileLinkChannel with HasTileLinkBeatId

  /** Returns the current count on this channel and when a message is done
    * @param inc increment the counter (usually .valid or .fire())
    * @param data the actual channel data
    * @param beat count to return for single-beat messages
    */
  def connectDataBeatCounter[S <: TileLinkChannel](inc: Bool, data: S, beat: UInt) = {
    val multi = data.hasMultibeatData()
    val (multi_cnt, multi_done) = Counter(inc && multi, data.tlDataBeats)
    val cnt = Mux(multi, multi_cnt, beat)
    val done = Mux(multi, multi_done, inc)
    (cnt, done)
  }

  /** Counter for beats on outgoing [[chisel.DecoupledIO]] */
  def connectOutgoingDataBeatCounter[T <: TileLinkChannel](
      out: DecoupledIO[T],
      beat: UInt = UInt(0)): (UInt, Bool) =
    connectDataBeatCounter(out.fire(), out.bits, beat)

  /** Returns done but not cnt. Use the addr_beat subbundle instead of cnt for beats on 
    * incoming channels in case of network reordering.
    */
  def connectIncomingDataBeatCounter[T <: TileLinkChannel](in: DecoupledIO[T]): Bool =
    connectDataBeatCounter(in.fire(), in.bits, UInt(0))._2

  /** Counter for beats on incoming DecoupledIO[LogicalNetworkIO[]]s returns done */
  def connectIncomingDataBeatCounterWithHeader[T <: TileLinkChannel](in: DecoupledIO[LogicalNetworkIO[T]]): Bool =
    connectDataBeatCounter(in.fire(), in.bits.payload, UInt(0))._2

  /** If the network might interleave beats from different messages, we need a Vec of counters,
    * one for every outstanding message id that might be interleaved.
    *
    * @param getId mapping from Message to counter id
    */
  def connectIncomingDataBeatCountersWithHeader[T <: TileLinkChannel with HasClientTransactionId](
      in: DecoupledIO[LogicalNetworkIO[T]],
      entries: Int,
      getId: LogicalNetworkIO[T] => UInt): Vec[Bool] = {
    Vec((0 until entries).map { i =>
      connectDataBeatCounter(in.fire() && getId(in.bits) === UInt(i), in.bits.payload, UInt(0))._2 
    })
  }

  /** Provides counters on two channels, as well a meta-counter that tracks how many
    * messages have been sent over the up channel but not yet responded to over the down channel
    *
    * @param max max number of outstanding ups with no down
    * @param up outgoing channel
    * @param down incoming channel
    * @param beat overrides cnts on single-beat messages
    * @param track whether up's message should be tracked
    * @return a tuple containing whether their are outstanding messages, up's count,
    *         up's done, down's count, down's done
    */
  def connectTwoWayBeatCounter[T <: TileLinkChannel, S <: TileLinkChannel](
      max: Int,
      up: DecoupledIO[T],
      down: DecoupledIO[S],
      beat: UInt = UInt(0),
      track: T => Bool = (t: T) => Bool(true)): (Bool, UInt, Bool, UInt, Bool) = {
    val (up_idx, up_done) = connectDataBeatCounter(up.fire(), up.bits, beat)
    val (down_idx, down_done) = connectDataBeatCounter(down.fire(), down.bits, beat)
    val do_inc = up_done && track(up.bits)
    val do_dec = down_done
    val cnt = TwoWayCounter(do_inc, do_dec, max)
    (cnt > UInt(0), up_idx, up_done, down_idx, down_done)
  }
}

class ClientTileLinkIOUnwrapper(implicit p: Parameters) extends TLModule()(p) {
  val io = new Bundle {
    val in = new ClientTileLinkIO().flip
    val out = new ClientUncachedTileLinkIO
  }

  def needsRoqEnq(channel: HasTileLinkData): Bool =
    !channel.hasMultibeatData() || channel.addr_beat === UInt(0)

  def needsRoqDeq(channel: HasTileLinkData): Bool =
    !channel.hasMultibeatData() || channel.addr_beat === UInt(tlDataBeats - 1)

  val acqArb = Module(new LockingRRArbiter(new Acquire, 2, tlDataBeats,
    Some((acq: Acquire) => acq.hasMultibeatData())))

  val acqRoq = Module(new ReorderQueue(
    Bool(), tlClientXactIdBits, tlMaxClientsPerPort))

  val relRoq = Module(new ReorderQueue(
    Bool(), tlClientXactIdBits, tlMaxClientsPerPort))

  val iacq = io.in.acquire.bits
  val irel = io.in.release.bits
  val ognt = io.out.grant.bits

  val acq_roq_enq = needsRoqEnq(iacq)
  val rel_roq_enq = needsRoqEnq(irel)

  val acq_roq_ready = !acq_roq_enq || acqRoq.io.enq.ready
  val rel_roq_ready = !rel_roq_enq || relRoq.io.enq.ready

  val acq_helper = DecoupledHelper(
    io.in.acquire.valid,
    acq_roq_ready,
    acqArb.io.in(0).ready)

  val rel_helper = DecoupledHelper(
    io.in.release.valid,
    rel_roq_ready,
    acqArb.io.in(1).ready)

  acqRoq.io.enq.valid := acq_helper.fire(acq_roq_ready, acq_roq_enq)
  acqRoq.io.enq.bits.data := iacq.isBuiltInType()
  acqRoq.io.enq.bits.tag := iacq.client_xact_id

  acqArb.io.in(0).valid := acq_helper.fire(acqArb.io.in(0).ready)
  acqArb.io.in(0).bits := Acquire(
    is_builtin_type = Bool(true),
    a_type = Mux(iacq.isBuiltInType(),
      iacq.a_type, Acquire.getBlockType),
    client_xact_id = iacq.client_xact_id,
    addr_block = iacq.addr_block,
    addr_beat = iacq.addr_beat,
    data = iacq.data,
    union = Mux(iacq.isBuiltInType(),
      iacq.union, Cat(MT_Q, M_XRD, Bool(true))))
  io.in.acquire.ready := acq_helper.fire(io.in.acquire.valid)

  relRoq.io.enq.valid := rel_helper.fire(rel_roq_ready, rel_roq_enq)
  relRoq.io.enq.bits.data := irel.isVoluntary()
  relRoq.io.enq.bits.tag := irel.client_xact_id

  acqArb.io.in(1).valid := rel_helper.fire(acqArb.io.in(1).ready)
  acqArb.io.in(1).bits := PutBlock(
    client_xact_id = irel.client_xact_id,
    addr_block = irel.addr_block,
    addr_beat = irel.addr_beat,
    data = irel.data,
    wmask = Acquire.fullWriteMask)
  io.in.release.ready := rel_helper.fire(io.in.release.valid)

  io.out.acquire <> acqArb.io.out

  acqRoq.io.deq.valid := io.out.grant.fire() && needsRoqDeq(ognt)
  acqRoq.io.deq.tag := ognt.client_xact_id

  relRoq.io.deq.valid := io.out.grant.fire() && needsRoqDeq(ognt)
  relRoq.io.deq.tag := ognt.client_xact_id

  val gnt_builtin = acqRoq.io.deq.data
  val gnt_voluntary = relRoq.io.deq.data

  val acq_grant = Grant(
    is_builtin_type = gnt_builtin,
    g_type = Mux(gnt_builtin, ognt.g_type, tlCoh.getExclusiveGrantType),
    client_xact_id = ognt.client_xact_id,
    manager_xact_id = ognt.manager_xact_id,
    addr_beat = ognt.addr_beat,
    data = ognt.data)

  val rel_grant = Grant(
    is_builtin_type = Bool(true),
    g_type = Mux(gnt_voluntary, Grant.voluntaryAckType, ognt.g_type),
    client_xact_id = ognt.client_xact_id,
    manager_xact_id = ognt.manager_xact_id,
    addr_beat = ognt.addr_beat,
    data = ognt.data)

  io.in.grant.valid := io.out.grant.valid
  io.in.grant.bits := Mux(acqRoq.io.deq.matches, acq_grant, rel_grant)
  io.out.grant.ready := io.in.grant.ready

  io.in.probe.valid := Bool(false)
}

class NastiIOTileLinkIOConverterInfo(implicit p: Parameters) extends TLBundle()(p) {
  val addr_beat = UInt(width = tlBeatAddrBits)
  val byteOff = UInt(width = tlByteAddrBits)
  val subblock = Bool()
}

class NastiIOTileLinkIOConverter(implicit p: Parameters) extends TLModule()(p)
    with HasNastiParameters {
  val io = new Bundle {
    val tl = new ClientUncachedTileLinkIO().flip
    val nasti = new NastiIO
  }

  private def opSizeToXSize(ops: UInt) = MuxLookup(ops, UInt("b111"), Seq(
    MT_B  -> UInt(0),
    MT_BU -> UInt(0),
    MT_H  -> UInt(1),
    MT_HU -> UInt(1),
    MT_W  -> UInt(2),
    MT_D  -> UInt(3),
    MT_Q  -> UInt(log2Up(tlDataBytes))))

  val dataBits = tlDataBits*tlDataBeats 
  require(tlDataBits == nastiXDataBits, "Data sizes between LLC and MC don't agree") // TODO: remove this restriction
  require(tlDataBeats < (1 << nastiXLenBits), "Can't have that many beats")
  require(tlClientXactIdBits <= nastiXIdBits, "NastiIO converter is going truncate tags: " + tlClientXactIdBits + " > " + nastiXIdBits)

  val has_data = io.tl.acquire.bits.hasData()

  val is_subblock = io.tl.acquire.bits.isSubBlockType()
  val is_multibeat = io.tl.acquire.bits.hasMultibeatData()
  val (tl_cnt_out, tl_wrap_out) = Counter(
    io.tl.acquire.fire() && is_multibeat, tlDataBeats)

  val get_valid = io.tl.acquire.valid && !has_data
  val put_valid = io.tl.acquire.valid && has_data

  // Reorder queue saves extra information needed to send correct
  // grant back to TL client
  val roq = Module(new ReorderQueue(
    new NastiIOTileLinkIOConverterInfo,
    nastiRIdBits, tlMaxClientsPerPort))

  // For Get/GetBlock, make sure Reorder queue can accept new entry
  val get_helper = DecoupledHelper(
    get_valid,
    roq.io.enq.ready,
    io.nasti.ar.ready)

  val w_inflight = Reg(init = Bool(false))

  // For Put/PutBlock, make sure aw and w channel are both ready before
  // we send the first beat
  val aw_ready = w_inflight || io.nasti.aw.ready
  val put_helper = DecoupledHelper(
    put_valid,
    aw_ready,
    io.nasti.w.ready)

  val (nasti_cnt_out, nasti_wrap_out) = Counter(
    io.nasti.r.fire() && !roq.io.deq.data.subblock, tlDataBeats)

  roq.io.enq.valid := get_helper.fire(roq.io.enq.ready)
  roq.io.enq.bits.tag := io.nasti.ar.bits.id
  roq.io.enq.bits.data.addr_beat := io.tl.acquire.bits.addr_beat
  roq.io.enq.bits.data.byteOff := io.tl.acquire.bits.addr_byte()
  roq.io.enq.bits.data.subblock := is_subblock
  roq.io.deq.valid := io.nasti.r.fire() && (nasti_wrap_out || roq.io.deq.data.subblock)
  roq.io.deq.tag := io.nasti.r.bits.id

  // Decompose outgoing TL Acquires into Nasti address and data channels
  io.nasti.ar.valid := get_helper.fire(io.nasti.ar.ready)
  io.nasti.ar.bits := NastiReadAddressChannel(
    id = io.tl.acquire.bits.client_xact_id,
    addr = io.tl.acquire.bits.full_addr(),
    size = Mux(is_subblock,
      opSizeToXSize(io.tl.acquire.bits.op_size()),
      UInt(log2Ceil(tlDataBytes))),
    len = Mux(is_subblock, UInt(0), UInt(tlDataBeats - 1)))

  io.nasti.aw.valid := put_helper.fire(aw_ready, !w_inflight)
  io.nasti.aw.bits := NastiWriteAddressChannel(
    id = io.tl.acquire.bits.client_xact_id,
    addr = io.tl.acquire.bits.full_addr(),
    size = UInt(log2Ceil(tlDataBytes)),
    len = Mux(is_multibeat, UInt(tlDataBeats - 1), UInt(0)))

  io.nasti.w.valid := put_helper.fire(io.nasti.w.ready)
  io.nasti.w.bits := NastiWriteDataChannel(
    data = io.tl.acquire.bits.data,
    strb = io.tl.acquire.bits.wmask(),
    last = tl_wrap_out || (io.tl.acquire.fire() && is_subblock))

  io.tl.acquire.ready := Mux(has_data,
    put_helper.fire(put_valid),
    get_helper.fire(get_valid))

  when (!w_inflight && io.tl.acquire.fire() && is_multibeat) {
    w_inflight := Bool(true)
  }

  when (w_inflight) {
    when (tl_wrap_out) { w_inflight := Bool(false) }
  }

  // Aggregate incoming NASTI responses into TL Grants
  val (tl_cnt_in, tl_wrap_in) = Counter(
    io.tl.grant.fire() && io.tl.grant.bits.hasMultibeatData(), tlDataBeats)
  val gnt_arb = Module(new Arbiter(new GrantToDst, 2))
  io.tl.grant <> gnt_arb.io.out

  val r_aligned_data = Mux(roq.io.deq.data.subblock,
    io.nasti.r.bits.data << Cat(roq.io.deq.data.byteOff, UInt(0, 3)),
    io.nasti.r.bits.data)

  gnt_arb.io.in(0).valid := io.nasti.r.valid
  io.nasti.r.ready := gnt_arb.io.in(0).ready
  gnt_arb.io.in(0).bits := Grant(
    is_builtin_type = Bool(true),
    g_type = Mux(roq.io.deq.data.subblock,
      Grant.getDataBeatType, Grant.getDataBlockType),
    client_xact_id = io.nasti.r.bits.id,
    manager_xact_id = UInt(0),
    addr_beat = Mux(roq.io.deq.data.subblock, roq.io.deq.data.addr_beat, tl_cnt_in),
    data = r_aligned_data)

  gnt_arb.io.in(1).valid := io.nasti.b.valid
  io.nasti.b.ready := gnt_arb.io.in(1).ready
  gnt_arb.io.in(1).bits := Grant(
    is_builtin_type = Bool(true),
    g_type = Grant.putAckType,
    client_xact_id = io.nasti.b.bits.id,
    manager_xact_id = UInt(0),
    addr_beat = UInt(0),
    data = Bits(0))

  assert(!io.nasti.r.valid || io.nasti.r.bits.resp === UInt(0), "NASTI read error")
  assert(!io.nasti.b.valid || io.nasti.b.bits.resp === UInt(0), "NASTI write error")
}

class TileLinkIONarrower(innerTLId: String, outerTLId: String)
    (implicit p: Parameters) extends TLModule()(p) {

  val innerParams = p(TLKey(innerTLId))
  val outerParams = p(TLKey(outerTLId)) 
  val innerDataBeats = innerParams.dataBeats
  val innerDataBits = innerParams.dataBitsPerBeat
  val innerWriteMaskBits = innerParams.writeMaskBits
  val innerByteAddrBits = log2Up(innerWriteMaskBits)
  val outerDataBeats = outerParams.dataBeats
  val outerDataBits = outerParams.dataBitsPerBeat
  val outerWriteMaskBits = outerParams.writeMaskBits
  val outerByteAddrBits = log2Up(outerWriteMaskBits)
  val outerBeatAddrBits = log2Up(outerDataBeats)
  val outerBlockOffset = outerBeatAddrBits + outerByteAddrBits
  val outerMaxClients = outerParams.maxClientsPerPort
  val outerIdBits = log2Up(outerParams.maxClientXacts * outerMaxClients)

  require(outerDataBeats >= innerDataBeats)
  require(outerDataBeats % innerDataBeats == 0)
  require(outerDataBits <= innerDataBits)
  require(outerDataBits * outerDataBeats == innerDataBits * innerDataBeats)

  val factor = outerDataBeats / innerDataBeats

  val io = new Bundle {
    val in = new ClientUncachedTileLinkIO()(p.alterPartial({case TLId => innerTLId})).flip
    val out = new ClientUncachedTileLinkIO()(p.alterPartial({case TLId => outerTLId}))
  }

  if (factor > 1) {
    val iacq = io.in.acquire.bits
    val ognt = io.out.grant.bits

    val stretch = iacq.a_type === Acquire.putBlockType
    val shrink = iacq.a_type === Acquire.getBlockType
    val smallput = iacq.a_type === Acquire.putType
    val smallget = iacq.a_type === Acquire.getType

    val acq_data_buffer = Reg(UInt(width = innerDataBits))
    val acq_wmask_buffer = Reg(UInt(width = innerWriteMaskBits))
    val acq_client_id = Reg(iacq.client_xact_id)
    val acq_addr_block = Reg(iacq.addr_block)
    val acq_addr_beat = Reg(iacq.addr_beat)
    val oacq_ctr = Counter(factor)

    // this part of the address shifts from the inner byte address 
    // to the outer beat address
    val readshift = iacq.full_addr()(innerByteAddrBits - 1, outerByteAddrBits)
    val outer_beat_addr = iacq.full_addr()(outerBlockOffset - 1, outerByteAddrBits)
    val outer_byte_addr = iacq.full_addr()(outerByteAddrBits - 1, 0)

    val mask_chunks = Vec.tabulate(factor) { i =>
      val lsb = i * outerWriteMaskBits
      val msb = (i + 1) * outerWriteMaskBits - 1
      iacq.wmask()(msb, lsb)
    }

    val data_chunks = Vec.tabulate(factor) { i =>
      val lsb = i * outerDataBits
      val msb = (i + 1) * outerDataBits - 1
      iacq.data(msb, lsb)
    }

    val beat_sel = Cat(mask_chunks.map(mask => mask.orR).reverse)

    val smallput_data = Mux1H(beat_sel, data_chunks)
    val smallput_wmask = Mux1H(beat_sel, mask_chunks)
    val smallput_beat = Cat(iacq.addr_beat, PriorityEncoder(beat_sel))

    assert(!io.in.acquire.valid || !smallput || PopCount(beat_sel) <= UInt(1),
      "Can't perform Put wider than outer width")

    val read_size_ok = MuxLookup(iacq.op_size(), Bool(false), Seq(
      MT_B  -> Bool(true),
      MT_BU -> Bool(true),
      MT_H  -> Bool(outerDataBits >= 16),
      MT_HU -> Bool(outerDataBits >= 16),
      MT_W  -> Bool(outerDataBits >= 32),
      MT_D  -> Bool(outerDataBits >= 64),
      MT_Q  -> Bool(false)))

    assert(!io.in.acquire.valid || !smallget || read_size_ok,
      "Can't perform Get wider than outer width")

    val outerConfig = p.alterPartial({ case TLId => outerTLId })
    val innerConfig = p.alterPartial({ case TLId => innerTLId })

    val get_block_acquire = GetBlock(
      client_xact_id = iacq.client_xact_id,
      addr_block = iacq.addr_block,
      alloc = iacq.allocate())(outerConfig)

    val put_block_acquire = PutBlock(
      client_xact_id = acq_client_id,
      addr_block = acq_addr_block,
      addr_beat = if (factor > 1)
                    Cat(acq_addr_beat, oacq_ctr.value)
                  else acq_addr_beat,
      data = acq_data_buffer(outerDataBits - 1, 0),
      wmask = acq_wmask_buffer(outerWriteMaskBits - 1, 0))(outerConfig)

    val get_acquire = Get(
      client_xact_id = iacq.client_xact_id,
      addr_block = iacq.addr_block,
      addr_beat = outer_beat_addr,
      addr_byte = outer_byte_addr,
      operand_size = iacq.op_size(),
      alloc = iacq.allocate())(outerConfig)

    val put_acquire = Put(
      client_xact_id = iacq.client_xact_id,
      addr_block = iacq.addr_block,
      addr_beat = smallput_beat,
      data = smallput_data,
      wmask = Some(smallput_wmask))(outerConfig)

    val sending_put = Reg(init = Bool(false))

    val pass_valid = io.in.acquire.valid && !stretch && !smallget
    val smallget_valid = smallget && io.in.acquire.valid

    val smallget_roq = Module(new ReorderQueue(
      readshift, outerIdBits, outerMaxClients))

    val smallget_helper = DecoupledHelper(
      smallget_valid,
      smallget_roq.io.enq.ready,
      io.out.acquire.ready)

    smallget_roq.io.enq.valid := smallget_helper.fire(
      smallget_roq.io.enq.ready, !sending_put)
    smallget_roq.io.enq.bits.data := readshift
    smallget_roq.io.enq.bits.tag := iacq.client_xact_id

    io.out.acquire.bits := MuxBundle(iacq, Seq(
      (sending_put, put_block_acquire),
      (shrink, get_block_acquire),
      (smallput, put_acquire),
      (smallget, get_acquire)))
    io.out.acquire.valid := sending_put || pass_valid ||
      smallget_helper.fire(io.out.acquire.ready)
    io.in.acquire.ready := !sending_put && (stretch ||
      (!smallget && io.out.acquire.ready) ||
      smallget_helper.fire(smallget_valid))

    when (io.in.acquire.fire() && stretch) {
      acq_data_buffer := iacq.data
      acq_wmask_buffer := iacq.wmask()
      acq_client_id := iacq.client_xact_id
      acq_addr_block := iacq.addr_block
      acq_addr_beat := iacq.addr_beat
      sending_put := Bool(true)
    }

    when (sending_put && io.out.acquire.ready) {
      acq_data_buffer := acq_data_buffer >> outerDataBits
      acq_wmask_buffer := acq_wmask_buffer >> outerWriteMaskBits
      when (oacq_ctr.inc()) { sending_put := Bool(false) }
    }

    val ognt_block = ognt.hasMultibeatData()
    val gnt_data_buffer = Reg(Vec(factor, UInt(width = outerDataBits)))
    val gnt_client_id = Reg(ognt.client_xact_id)
    val gnt_manager_id = Reg(ognt.manager_xact_id)

    val ignt_ctr = Counter(innerDataBeats)
    val ognt_ctr = Counter(factor)
    val sending_get = Reg(init = Bool(false))

    val get_block_grant = Grant(
      is_builtin_type = Bool(true),
      g_type = Grant.getDataBlockType,
      client_xact_id = gnt_client_id,
      manager_xact_id = gnt_manager_id,
      addr_beat = ignt_ctr.value,
      data = gnt_data_buffer.toBits)(innerConfig)

    val smallget_grant = ognt.g_type === Grant.getDataBeatType
    val get_grant_shift = Cat(smallget_roq.io.deq.data,
                              UInt(0, outerByteAddrBits + 3))

    smallget_roq.io.deq.valid := io.out.grant.fire() && smallget_grant
    smallget_roq.io.deq.tag := ognt.client_xact_id

    val get_grant = Grant(
      is_builtin_type = Bool(true),
      g_type = Grant.getDataBeatType,
      client_xact_id = ognt.client_xact_id,
      manager_xact_id = ognt.manager_xact_id,
      addr_beat = ognt.addr_beat >> UInt(log2Up(factor)),
      data = ognt.data << get_grant_shift)(innerConfig)

    io.in.grant.valid := sending_get || (io.out.grant.valid && !ognt_block)
    io.out.grant.ready := !sending_get && (ognt_block || io.in.grant.ready)
    io.in.grant.bits := MuxBundle(ognt, Seq(
      sending_get -> get_block_grant,
      smallget_grant -> get_grant))

    when (io.out.grant.valid && ognt_block && !sending_get) {
      gnt_data_buffer(ognt_ctr.value) := ognt.data
      when (ognt_ctr.inc()) {
        gnt_client_id := ognt.client_xact_id
        gnt_manager_id := ognt.manager_xact_id
        sending_get := Bool(true)
      }
    }

    when (io.in.grant.ready && sending_get) {
      ignt_ctr.inc()
      sending_get := Bool(false)
    }
  } else { io.out <> io.in }
}
