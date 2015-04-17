// See LICENSE for license details.

package uncore
import Chisel._
import scala.math.max
import scala.reflect._
import scala.reflect.runtime.universe._

// Parameters exposed to the top-level design, set based on 
// external requirements or design space exploration
//
case object TLId extends Field[String] // Unique name per network
case object TLCoherencePolicy extends Field[CoherencePolicy]
case object TLNManagers extends Field[Int]
case object TLNClients extends Field[Int]
case object TLNCoherentClients extends Field[Int]
case object TLNIncoherentClients extends Field[Int]
case object TLMaxClientXacts extends Field[Int]
case object TLMaxClientPorts extends Field[Int]
case object TLMaxManagerXacts extends Field[Int]
case object TLBlockAddrBits extends Field[Int]
case object TLDataBits extends Field[Int]
case object TLDataBeats extends Field[Int]
case object TLNetworkIsOrderedP2P extends Field[Boolean]

trait TileLinkParameters extends UsesParameters {
  val tlCoh = params(TLCoherencePolicy)
  val tlNManagers = params(TLNManagers)
  val tlNClients = params(TLNClients)
  val tlNCoherentClients = params(TLNCoherentClients)
  val tlNIncoherentClients = params(TLNIncoherentClients)
  val tlClientIdBits =  log2Up(tlNClients)
  val tlManagerIdBits =  log2Up(tlNManagers)
  val tlMaxClientXacts = params(TLMaxClientXacts)
  val tlMaxClientPorts = params(TLMaxClientPorts)
  val tlMaxManagerXacts = params(TLMaxManagerXacts)
  val tlClientXactIdBits = log2Up(tlMaxClientXacts*tlMaxClientPorts)
  val tlManagerXactIdBits = log2Up(tlMaxManagerXacts)
  val tlBlockAddrBits = params(TLBlockAddrBits)
  val tlDataBits = params(TLDataBits)
  val tlDataBytes = tlDataBits/8
  val tlDataBeats = params(TLDataBeats)
  val tlWriteMaskBits = if(tlDataBits/8 < 1) 1 else tlDataBits/8
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
  val tlNetworkPreservesPointToPointOrdering = params(TLNetworkIsOrderedP2P)
  val tlNetworkDoesNotInterleaveBeats = true
  val amoAluOperandBits = params(AmoAluOperandBits)
}

abstract class TLBundle extends Bundle with TileLinkParameters
abstract class TLModule extends Module with TileLinkParameters

// Directionality of message channel
// Used to hook up logical network ports to physical network ports
trait TileLinkChannel extends TLBundle {
  def hasData(dummy: Int = 0): Bool
  def hasMultibeatData(dummy: Int = 0): Bool
}
trait ClientToManagerChannel extends TileLinkChannel
trait ManagerToClientChannel extends TileLinkChannel
trait ClientToClientChannel extends TileLinkChannel // Unused for now

// Common signals that are used in multiple channels.
// These traits are useful for type parameterizing bundle wiring functions.
//
trait HasCacheBlockAddress extends TLBundle {
  val addr_block = UInt(width = tlBlockAddrBits)

  def conflicts(that: HasCacheBlockAddress) = this.addr_block === that.addr_block
  def conflicts(addr: UInt) = this.addr_block === addr
}

trait HasTileLinkBeatId extends TLBundle {
  val addr_beat = UInt(width = tlBeatAddrBits)
}

trait HasClientTransactionId extends TLBundle {
  val client_xact_id = Bits(width = tlClientXactIdBits)
}

trait HasManagerTransactionId extends TLBundle {
  val manager_xact_id = Bits(width = tlManagerXactIdBits)
}

trait HasTileLinkData extends HasTileLinkBeatId {
  val data = UInt(width = tlDataBits)

  def hasData(dummy: Int = 0): Bool
  def hasMultibeatData(dummy: Int = 0): Bool
}

trait HasClientId extends TLBundle {
  val client_id = UInt(width = tlClientIdBits)
}

// Actual TileLink channel bundle definitions

class Acquire extends ClientToManagerChannel 
    with HasCacheBlockAddress 
    with HasClientTransactionId 
    with HasTileLinkData {
  // Actual bundle fields
  val is_builtin_type = Bool()
  val a_type = UInt(width = tlAcquireTypeBits)
  val union = Bits(width = tlAcquireUnionBits)

  // Utility funcs for accessing subblock union
  val opCodeOff = 1
  val opSizeOff = tlMemoryOpcodeBits + opCodeOff
  val addrByteOff = tlMemoryOperandSizeBits + opSizeOff
  val addrByteMSB = tlByteAddrBits + addrByteOff
  def allocate(dummy: Int = 0) = union(0)
  def op_code(dummy: Int = 0) = Mux(isBuiltInType(Acquire.putType) || isBuiltInType(Acquire.putBlockType),
    M_XWR, union(opSizeOff-1, opCodeOff))
  def op_size(dummy: Int = 0) = union(addrByteOff-1, opSizeOff)
  def addr_byte(dummy: Int = 0) = union(addrByteMSB-1, addrByteOff)
  private def amo_offset(dummy: Int = 0) = addr_byte()(tlByteAddrBits-1, log2Up(amoAluOperandBits/8))
  def amo_shift_bits(dummy: Int = 0) = UInt(amoAluOperandBits)*amo_offset()
  def wmask(dummy: Int = 0) = 
    Mux(isBuiltInType(Acquire.putAtomicType), 
      FillInterleaved(amoAluOperandBits/8, UIntToOH(amo_offset())),
      Mux(isBuiltInType(Acquire.putBlockType) || isBuiltInType(Acquire.putType),
        union(tlWriteMaskBits, 1),
        UInt(0, width = tlWriteMaskBits)))
  def full_wmask(dummy: Int = 0) = FillInterleaved(8, wmask())

  def addr(dummy: Int = 0) = Cat(this.addr_block, this.addr_beat, this.addr_byte())

  // Other helper funcs
  def is(t: UInt) = a_type === t //TODO: make this more opaque; def ===?

  def isBuiltInType(dummy: Int = 0): Bool = is_builtin_type
  def isBuiltInType(t: UInt): Bool = is_builtin_type && a_type === t 

  def isSubBlockType(dummy: Int = 0): Bool = isBuiltInType() && Acquire.typesOnSubBlocks.contains(a_type) 

  def isPrefetch(dummy: Int = 0): Bool = isBuiltInType() && is(Acquire.prefetchType) 

  // Assumes no custom types have data
  def hasData(dummy: Int = 0): Bool = isBuiltInType() && Acquire.typesWithData.contains(a_type)

  def hasMultibeatData(dummy: Int = 0): Bool = Bool(tlDataBeats > 1) && isBuiltInType() &&
                                           Acquire.typesWithMultibeatData.contains(a_type)

  def requiresSelfProbe(dummy: Int = 0) = Bool(false)

  def getBuiltInGrantType(dummy: Int = 0): UInt = {
    MuxLookup(this.a_type, Grant.putAckType, Array(
      Acquire.getType       -> Grant.getDataBeatType,
      Acquire.getBlockType  -> Grant.getDataBlockType,
      Acquire.putType       -> Grant.putAckType,
      Acquire.putBlockType  -> Grant.putAckType,
      Acquire.putAtomicType -> Grant.getDataBeatType,
      Acquire.prefetchType  -> Grant.prefetchAckType))
  }
}

object Acquire {
  val nBuiltInTypes = 5
  //TODO: Use Enum
  def getType       = UInt("b000")
  def getBlockType  = UInt("b001")
  def putType       = UInt("b010")
  def putBlockType  = UInt("b011")
  def putAtomicType = UInt("b100")
  def prefetchType = UInt("b101")
  def typesWithData = Vec(putType, putBlockType, putAtomicType)
  def typesWithMultibeatData = Vec(putBlockType)
  def typesOnSubBlocks = Vec(putType, getType, putAtomicType)

  def fullWriteMask = SInt(-1, width = new Acquire().tlWriteMaskBits).toUInt

  // Most generic constructor
  def apply(
      is_builtin_type: Bool,
      a_type: Bits,
      client_xact_id: UInt,
      addr_block: UInt,
      addr_beat: UInt = UInt(0),
      data: UInt = UInt(0),
      union: UInt = UInt(0)): Acquire = {
    val acq = new Acquire
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
    val acq = new Acquire
    acq := a
    acq
  }
}

// Asks for a single TileLink beat of data
object Get {
  def apply(
      client_xact_id: UInt,
      addr_block: UInt,
      addr_beat: UInt,
      alloc: Bool = Bool(true)): Acquire = {
    Acquire(
      is_builtin_type = Bool(true),
      a_type = Acquire.getType,
      client_xact_id = client_xact_id,
      addr_block = addr_block,
      addr_beat = addr_beat,
      union = Cat(M_XRD, alloc))
  }
}

// Asks for an entire cache block of data
object GetBlock {
  def apply(
      client_xact_id: UInt = UInt(0),
      addr_block: UInt,
      alloc: Bool = Bool(true)): Acquire = {
    Acquire(
      is_builtin_type = Bool(true),
      a_type = Acquire.getBlockType,
      client_xact_id = client_xact_id, 
      addr_block = addr_block,
      union = Cat(M_XRD, alloc))
  }
}

// Prefetch a cache block into the next level of the memory hierarchy
// with read permissions
object GetPrefetch {
  def apply(
      client_xact_id: UInt,
      addr_block: UInt): Acquire = {
    Acquire(
      is_builtin_type = Bool(true),
      a_type = Acquire.prefetchType,
      client_xact_id = client_xact_id,
      addr_block = addr_block,
      addr_beat = UInt(0),
      union = Cat(M_XRD, Bool(true)))
  }
}

// Writes up to a single TileLink beat of data, using mask
object Put {
  def apply(
      client_xact_id: UInt,
      addr_block: UInt,
      addr_beat: UInt,
      data: UInt,
      wmask: UInt = Acquire.fullWriteMask): Acquire = {
    Acquire(
      is_builtin_type = Bool(true),
      a_type = Acquire.putType,
      addr_block = addr_block,
      addr_beat = addr_beat,
      client_xact_id = client_xact_id,
      data = data,
      union = Cat(wmask, Bool(true)))
  }
}

// Writes an entire cache block of data
object PutBlock {
  def apply(
      client_xact_id: UInt,
      addr_block: UInt,
      addr_beat: UInt,
      data: UInt,
      wmask: UInt): Acquire = {
    Acquire(
      is_builtin_type = Bool(true),
      a_type = Acquire.putBlockType,
      client_xact_id = client_xact_id,
      addr_block = addr_block,
      addr_beat = addr_beat,
      data = data,
      union = Cat(wmask, (wmask != Acquire.fullWriteMask)))
  }
  def apply(
      client_xact_id: UInt,
      addr_block: UInt,
      addr_beat: UInt,
      data: UInt,
      alloc: Bool = Bool(true)): Acquire = {
    Acquire(
      is_builtin_type = Bool(true),
      a_type = Acquire.putBlockType,
      client_xact_id = client_xact_id,
      addr_block = addr_block,
      addr_beat = addr_beat,
      data = data,
      union = Cat(Acquire.fullWriteMask, alloc))
  }
}

// Performs an atomic operation in the outer memory
object PutAtomic {
  def apply(
      client_xact_id: UInt,
      addr_block: UInt,
      addr_beat: UInt,
      addr_byte: UInt,
      atomic_opcode: UInt,
      operand_size: UInt,
      data: UInt): Acquire = {
    Acquire(
      is_builtin_type = Bool(true),
      a_type = Acquire.putAtomicType,
      client_xact_id = client_xact_id, 
      addr_block = addr_block, 
      addr_beat = addr_beat, 
      data = data,
      union = Cat(addr_byte, operand_size, atomic_opcode, Bool(true)))
  }
}

// Prefetch a cache block into the next level of the memory hierarchy
// with write permissions
object PutPrefetch {
  def apply(
      client_xact_id: UInt,
      addr_block: UInt): Acquire = {
    Acquire(
      is_builtin_type = Bool(true),
      a_type = Acquire.prefetchType,
      client_xact_id = client_xact_id,
      addr_block = addr_block,
      addr_beat = UInt(0),
      union = Cat(M_XWR, Bool(true)))
  }
}

class Probe extends ManagerToClientChannel 
    with HasCacheBlockAddress {
  val p_type = UInt(width = tlCoh.probeTypeWidth)

  def is(t: UInt) = p_type === t
  def hasData(dummy: Int = 0) = Bool(false)
  def hasMultibeatData(dummy: Int = 0) = Bool(false)
}

object Probe {
  def apply(p_type: UInt, addr_block: UInt): Probe = {
    val prb = new Probe
    prb.p_type := p_type
    prb.addr_block := addr_block
    prb
  }
  def apply(dst: UInt, p_type: UInt, addr_block: UInt): ProbeToDst = {
    val prb = new ProbeToDst
    prb.client_id := dst
    prb.p_type := p_type
    prb.addr_block := addr_block
    prb
  }
}

class Release extends ClientToManagerChannel 
    with HasCacheBlockAddress 
    with HasClientTransactionId 
    with HasTileLinkData {
  val r_type = UInt(width = tlCoh.releaseTypeWidth)
  val voluntary = Bool()

  // Helper funcs
  def is(t: UInt) = r_type === t
  def hasData(dummy: Int = 0) = tlCoh.releaseTypesWithData.contains(r_type)
  //TODO: Assumes all releases write back full cache blocks:
  def hasMultibeatData(dummy: Int = 0) = Bool(tlDataBeats > 1) && tlCoh.releaseTypesWithData.contains(r_type)
  def isVoluntary(dummy: Int = 0) = voluntary
  def requiresAck(dummy: Int = 0) = !Bool(tlNetworkPreservesPointToPointOrdering)
}

object Release {
  def apply(
      voluntary: Bool,
      r_type: UInt,
      client_xact_id: UInt,
      addr_block: UInt,
      addr_beat: UInt = UInt(0),
      data: UInt = UInt(0)): Release = {
    val rel = new Release
    rel.r_type := r_type
    rel.client_xact_id := client_xact_id
    rel.addr_block := addr_block
    rel.addr_beat := addr_beat
    rel.data := data
    rel.voluntary := voluntary
    rel
  }
}

class Grant extends ManagerToClientChannel 
    with HasTileLinkData 
    with HasClientTransactionId 
    with HasManagerTransactionId {
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
  def makeFinish(dummy: Int = 0): Finish = {
    val f = Bundle(new Finish, { case TLMaxManagerXacts => tlMaxManagerXacts })
    f.manager_xact_id := this.manager_xact_id
    f
  }
}

object Grant {
  val nBuiltInTypes = 5
  def voluntaryAckType = UInt("b000")
  def putAckType       = UInt("b001")
  def prefetchAckType  = UInt("b011")
  def getDataBeatType  = UInt("b100")
  def getDataBlockType = UInt("b101")
  def typesWithData = Vec(getDataBlockType, getDataBeatType)
  def typesWithMultibeatData= Vec(getDataBlockType)

  def apply(
      is_builtin_type: Bool,
      g_type: UInt,
      client_xact_id: UInt, 
      manager_xact_id: UInt,
      addr_beat: UInt,
      data: UInt): Grant = {
    val gnt = new Grant
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
      data: UInt = UInt(0)): GrantToDst = {
    val gnt = new GrantToDst
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

class Finish extends ClientToManagerChannel with HasManagerTransactionId {
  def hasData(dummy: Int = 0) = Bool(false)
  def hasMultibeatData(dummy: Int = 0) = Bool(false)
}

// These subtypes include a field for the source or destination ClientId
class AcquireFromSrc extends Acquire with HasClientId
class ProbeToDst extends Probe with HasClientId
class ReleaseFromSrc extends Release with HasClientId
class GrantToDst extends Grant with HasClientId

// Complete IO definitions for two types of TileLink clients, including
// networking headers
class UncachedTileLinkIO extends TLBundle {
  val acquire   = new DecoupledIO(new LogicalNetworkIO(new Acquire))
  val grant     = new DecoupledIO(new LogicalNetworkIO(new Grant)).flip
  val finish = new DecoupledIO(new LogicalNetworkIO(new Finish))
}

class TileLinkIO extends UncachedTileLinkIO {
  val probe     = new DecoupledIO(new LogicalNetworkIO(new Probe)).flip
  val release   = new DecoupledIO(new LogicalNetworkIO(new Release))
}

// Converts UncachedTileLinkIO to regular TileLinkIO by pinning
// probe.ready and release.valid low
class TileLinkIOWrapper extends TLModule {
  val io = new Bundle {
    val in = new UncachedTileLinkIO().flip
    val out = new TileLinkIO
  }
  io.out.acquire <> io.in.acquire
  io.out.grant <> io.in.grant
  io.out.finish <> io.in.finish
  io.out.probe.ready := Bool(true)
  io.out.release.valid := Bool(false)
}

// This version of TileLinkIO does not contain network headers. The headers
// are provided in the top-level that instantiates the clients and network,
// probably using a TileLinkClientPort module.
// By eliding the header subbundles within the clients we can enable 
// hierarchical P&R while minimizing unconnected port errors in GDS.
// Secondly, this version of the interface elides Finish messages, with the
// assumption that a FinishUnit has been coupled to the TileLinkIO port
// to deal with acking received Grants.
class ClientUncachedTileLinkIO extends TLBundle {
  val acquire   = new DecoupledIO(new Acquire)
  val grant     = new DecoupledIO(new Grant).flip
}

class ClientTileLinkIO extends ClientUncachedTileLinkIO {
  val probe     = new DecoupledIO(new Probe).flip
  val release   = new DecoupledIO(new Release)
}

class ManagerTileLinkIO extends TLBundle {
  val acquire   = new DecoupledIO(new AcquireFromSrc).flip
  val grant     = new DecoupledIO(new GrantToDst)
  val finish    = new DecoupledIO(new Finish).flip
  val probe     = new DecoupledIO(new ProbeToDst)
  val release   = new DecoupledIO(new ReleaseFromSrc).flip
}

class ClientTileLinkIOWrapper extends TLModule {
  val io = new Bundle {
    val in = new ClientUncachedTileLinkIO().flip
    val out = new ClientTileLinkIO
  }
  io.out.acquire <> io.in.acquire
  io.out.grant <> io.in.grant
  io.out.probe.ready := Bool(true)
  io.out.release.valid := Bool(false)
}

object TileLinkIOWrapper {
  def apply(utl: ClientUncachedTileLinkIO, p: Parameters): ClientTileLinkIO = {
    val conv = Module(new ClientTileLinkIOWrapper)(p)
    conv.io.in <> utl
    conv.io.out
  }
  def apply(utl: ClientUncachedTileLinkIO): ClientTileLinkIO = {
    val conv = Module(new ClientTileLinkIOWrapper)
    conv.io.in <> utl
    conv.io.out
  }
  def apply(tl: ClientTileLinkIO): ClientTileLinkIO = tl
  def apply(utl: UncachedTileLinkIO, p: Parameters): TileLinkIO = {
    val conv = Module(new TileLinkIOWrapper)(p)
    conv.io.in <> utl
    conv.io.out
  }
  def apply(utl: UncachedTileLinkIO): TileLinkIO = {
    val conv = Module(new TileLinkIOWrapper)
    conv.io.in <> utl
    conv.io.out
  }
  def apply(tl: TileLinkIO): TileLinkIO = tl
}

class FinishQueueEntry extends TLBundle {
    val fin = new Finish
    val dst = UInt(width = log2Up(params(LNEndpoints)))
}

class FinishQueue(entries: Int) extends Queue(new FinishQueueEntry, entries)

class FinishUnit(srcId: Int = 0, outstanding: Int = 2) extends TLModule with HasDataBeatCounters {
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

    io.refill.valid := io.grant.valid
    io.refill.bits := g
    io.grant.ready := (q.io.enq.ready || !g.requiresAck()) && io.refill.ready
    io.ready := q.io.enq.ready
  }
}

object ClientTileLinkHeaderCreator {
  def apply[T <: ClientToManagerChannel with HasCacheBlockAddress : ClassTag](
      in: DecoupledIO[T],
      clientId: Int,
      addrConvert: UInt => UInt): DecoupledIO[LogicalNetworkIO[T]] = {
    val out = new DecoupledIO(new LogicalNetworkIO(in.bits.clone)).asDirectionless
    out.bits.payload := in.bits
    out.bits.header.src := UInt(clientId)
    out.bits.header.dst := addrConvert(in.bits.addr_block)
    out.valid := in.valid
    in.ready := out.ready
    out
  }
}

object ManagerTileLinkHeaderCreator {
  def apply[T <: ManagerToClientChannel with HasClientId : ClassTag](
      in: DecoupledIO[T],
      managerId: Int,
      idConvert: UInt => UInt): DecoupledIO[LogicalNetworkIO[T]] = {
    val out = new DecoupledIO(new LogicalNetworkIO(in.bits.clone)).asDirectionless
    out.bits.payload := in.bits
    out.bits.header.src := UInt(managerId)
    out.bits.header.dst := idConvert(in.bits.client_id)
    out.valid := in.valid
    in.ready := out.ready
    out
  }
}

class ClientTileLinkNetworkPort(clientId: Int, addrConvert: UInt => UInt) extends TLModule {
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

class ManagerTileLinkNetworkPort(managerId: Int, idConvert: UInt => UInt) extends TLModule {
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

case class TileLinkDepths(acq: Int, prb: Int, rel: Int, gnt: Int, fin: Int)

class TileLinkEnqueuer(depths: TileLinkDepths) extends Module {
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
  def apply(in: TileLinkIO, depths: TileLinkDepths)(p: Parameters): TileLinkIO = {
    val t = Module(new TileLinkEnqueuer(depths))(p)
    t.io.client <> in
    t.io.manager
  }
  def apply(in: TileLinkIO, depth: Int)(p: Parameters): TileLinkIO = {
    apply(in, TileLinkDepths(depth, depth, depth, depth, depth))(p)
  }
}

/** Utility functions for constructing TileLinkIO arbiters */
trait TileLinkArbiterLike extends TileLinkParameters {
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
    val arb = Module(new LockingRRArbiter(mngr.bits.clone, arbN, tlDataBeats, Some(hasData _)))
    clts.zipWithIndex.zip(arb.io.in).map{ case ((req, id), arb) => {
      arb.valid := req.valid
      arb.bits := req.bits
      arb.bits.payload.client_xact_id := clientSourcedClientXactId(req.bits.payload, id)
      req.ready := arb.ready
    }}
    arb.io.out <> mngr
  }

  def hookupClientSourceHeaderless[M <: ClientSourcedWithIdAndData](
      clts: Seq[DecoupledIO[M]],
      mngr: DecoupledIO[M]) {
    def hasData(m: M) = m.hasMultibeatData()
    val arb = Module(new LockingRRArbiter(mngr.bits.clone, arbN, tlDataBeats, Some(hasData _)))
    clts.zipWithIndex.zip(arb.io.in).map{ case ((req, id), arb) => {
      arb.valid := req.valid
      arb.bits := req.bits
      arb.bits.client_xact_id := clientSourcedClientXactId(req.bits, id)
      req.ready := arb.ready
    }}
    arb.io.out <> mngr
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
    val arb = Module(new RRArbiter(mngr.bits.clone, arbN))
    arb.io.in <> clts
    arb.io.out <> mngr
  }
}

/** Abstract base case for any Arbiters that have UncachedTileLinkIOs */
abstract class UncachedTileLinkIOArbiter(val arbN: Int) extends Module with TileLinkArbiterLike {
  val io = new Bundle {
    val in = Vec.fill(arbN){new UncachedTileLinkIO}.flip
    val out = new UncachedTileLinkIO
  }
  hookupClientSource(io.in.map(_.acquire), io.out.acquire)
  hookupFinish(io.in.map(_.finish), io.out.finish)
  hookupManagerSourceWithId(io.in.map(_.grant), io.out.grant)
}

/** Abstract base case for any Arbiters that have cached TileLinkIOs */
abstract class TileLinkIOArbiter(val arbN: Int) extends Module with TileLinkArbiterLike {
  val io = new Bundle {
    val in = Vec.fill(arbN){new TileLinkIO}.flip
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
    in.client_xact_id >> UInt(log2Up(arbN))
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
class UncachedTileLinkIOArbiterThatAppendsArbiterId(val n: Int) extends UncachedTileLinkIOArbiter(n) with AppendsArbiterId
class UncachedTileLinkIOArbiterThatPassesId(val n: Int) extends UncachedTileLinkIOArbiter(n) with PassesId
class UncachedTileLinkIOArbiterThatUsesNewId(val n: Int) extends UncachedTileLinkIOArbiter(n) with UsesNewId
class TileLinkIOArbiterThatAppendsArbiterId(val n: Int) extends TileLinkIOArbiter(n) with AppendsArbiterId
class TileLinkIOArbiterThatPassesId(val n: Int) extends TileLinkIOArbiter(n) with PassesId
class TileLinkIOArbiterThatUsesNewId(val n: Int) extends TileLinkIOArbiter(n) with UsesNewId

/** Concrete uncached client-side arbiter that appends the arbiter's port id to client_xact_id */
class ClientUncachedTileLinkIOArbiter(val arbN: Int) extends Module with TileLinkArbiterLike with AppendsArbiterId {
  val io = new Bundle {
    val in = Vec.fill(arbN){new ClientUncachedTileLinkIO}.flip
    val out = new ClientUncachedTileLinkIO
  }
  hookupClientSourceHeaderless(io.in.map(_.acquire), io.out.acquire)
  hookupManagerSourceHeaderlessWithId(io.in.map(_.grant), io.out.grant)
}

/** Concrete client-side arbiter that appends the arbiter's port id to client_xact_id */
class ClientTileLinkIOArbiter(val arbN: Int) extends Module with TileLinkArbiterLike with AppendsArbiterId {
  val io = new Bundle {
    val in = Vec.fill(arbN){new ClientTileLinkIO}.flip
    val out = new ClientTileLinkIO
  }
  hookupClientSourceHeaderless(io.in.map(_.acquire), io.out.acquire)
  hookupClientSourceHeaderless(io.in.map(_.release), io.out.release)
  hookupManagerSourceBroadcast(io.in.map(_.probe), io.out.probe)
  hookupManagerSourceHeaderlessWithId(io.in.map(_.grant), io.out.grant)
}

/** Utility trait containing wiring functions to keep track of how many data beats have 
  * been sent or recieved over a particular TileLinkChannel or pair of channels. 
  *
  * Won't count message types that don't have data. 
  * Used in XactTrackers and FinishUnit.
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

  /** Counter for beats on outgoing DecoupledIOs */
  def connectOutgoingDataBeatCounter[T <: TileLinkChannel](in: DecoupledIO[T], beat: UInt = UInt(0)): (UInt, Bool) =
    connectDataBeatCounter(in.fire(), in.bits, beat)

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
    val cnt = Reg(init = UInt(0, width = log2Up(max+1)))
    val (up_idx, up_done) = connectDataBeatCounter(up.fire(), up.bits, beat)
    val (down_idx, down_done) = connectDataBeatCounter(down.fire(), down.bits, beat)
    val do_inc = up_done && track(up.bits)
    val do_dec = down_done
    cnt := Mux(do_dec,
            Mux(do_inc, cnt, cnt - UInt(1)),
            Mux(do_inc, cnt + UInt(1), cnt))
    (cnt > UInt(0), up_idx, up_done, down_idx, down_done)
  }
}
