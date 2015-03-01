// See LICENSE for license details.

package uncore
import Chisel._
import scala.math.max

// Parameters exposed to the top-level design, set based on 
// external requirements or design space exploration
//
case object TLId extends Field[String] // Unique name per network
case object TLCoherencePolicy extends Field[CoherencePolicy]
case object TLBlockAddrBits extends Field[Int]
case object TLManagerXactIdBits extends Field[Int]
case object TLClientXactIdBits extends Field[Int]
case object TLDataBits extends Field[Int]
case object TLDataBeats extends Field[Int]
case object TLNetworkIsOrderedP2P extends Field[Boolean]

abstract trait TileLinkParameters extends UsesParameters {
  val tlBlockAddrBits = params(TLBlockAddrBits)
  val tlClientXactIdBits = params(TLClientXactIdBits)
  val tlManagerXactIdBits = params(TLManagerXactIdBits)
  val tlDataBits = params(TLDataBits)
  val tlDataBeats = params(TLDataBeats)
  val tlCoh = params(TLCoherencePolicy)
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
}

abstract class TLBundle extends Bundle with TileLinkParameters
abstract class TLModule extends Module with TileLinkParameters

// Directionality of message channel
// Used to hook up logical network ports to physical network ports
trait TileLinkChannel extends TLBundle
trait ClientToManagerChannel extends TileLinkChannel
trait ManagerToClientChannel extends TileLinkChannel
trait ClientToClientChannel extends TileLinkChannel // Unused for now

// Common signals that are used in multiple channels.
// These traits are useful for type parameterization.
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
  def op_code(dummy: Int = 0) = Mux(hasData(), M_XWR, union(opSizeOff-1, opCodeOff))
  def op_size(dummy: Int = 0) = union(addrByteOff-1, opSizeOff)
  def addr_byte(dummy: Int = 0) = union(addrByteMSB-1, addrByteOff)
  def write_mask(dummy: Int = 0) = union(tlWriteMaskBits, 1)
  def addr(dummy: Int = 0) = Cat(this.addr_block, this.addr_beat, this.addr_byte())

  // Other helper funcs
  def is(t: UInt) = a_type === t

  def isBuiltInType(dummy: Int = 0): Bool = is_builtin_type

  def isSubBlockType(dummy: Int = 0): Bool = isBuiltInType() && Acquire.typesOnSubBlocks.contains(a_type) 

  // Assumes no custom types have data
  def hasData(dummy: Int = 0): Bool = isBuiltInType() && Acquire.typesWithData.contains(a_type)

  def hasMultibeatData(dummy: Int = 0): Bool = Bool(tlDataBeats > 1) && isBuiltInType() &&
                                           Acquire.typesWithMultibeatData.contains(a_type)

  //TODO: This function is a hack to support Rocket icache snooping Rocket nbdcache:
  def requiresSelfProbe(dummy: Int = 0) = isBuiltInType() && a_type === Acquire.getBlockType

  def getBuiltInGrantType(dummy: Int = 0): UInt = {
    MuxLookup(this.a_type, Grant.ackType, Array(
      Acquire.getType -> Grant.dataBeatType,
      Acquire.getBlockType -> Grant.dataBlockType,
      Acquire.putType -> Grant.ackType,
      Acquire.putBlockType -> Grant.ackType,
      Acquire.putAtomicType -> Grant.dataBeatType))
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

// Writes up to a single TileLink beat of data, using mask
object Put {
  def apply(
      client_xact_id: UInt,
      addr_block: UInt,
      addr_beat: UInt,
      data: UInt,
      write_mask: UInt = Acquire.fullWriteMask): Acquire = {
    Acquire(
      is_builtin_type = Bool(true),
      a_type = Acquire.putType,
      addr_block = addr_block,
      addr_beat = addr_beat,
      client_xact_id = client_xact_id,
      data = data,
      union = Cat(write_mask, Bool(true)))
  }
}

// Writes an entire cache block of data
object PutBlock {
  def apply(
      client_xact_id: UInt,
      addr_block: UInt,
      addr_beat: UInt,
      data: UInt,
      write_mask: UInt): Acquire = {
    Acquire(
      is_builtin_type = Bool(true),
      a_type = Acquire.putBlockType,
      client_xact_id = client_xact_id,
      addr_block = addr_block,
      addr_beat = addr_beat,
      data = data,
      union = Cat(write_mask, (write_mask != Acquire.fullWriteMask)))
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

class Probe extends ManagerToClientChannel 
    with HasCacheBlockAddress {
  val p_type = UInt(width = tlCoh.probeTypeWidth)

  def is(t: UInt) = p_type === t
}

object Probe {
  def apply(p_type: UInt, addr_block: UInt) = {
    val prb = new Probe
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
    val f = Bundle(new Finish, { case TLManagerXactIdBits => tlManagerXactIdBits })
    f.manager_xact_id := this.manager_xact_id
    f
  }
}

object Grant {
  val nBuiltInTypes = 4
  def voluntaryAckType = UInt("b00")
  def ackType          = UInt("b01")
  def dataBeatType     = UInt("b10")
  def dataBlockType    = UInt("b11")
  def typesWithData = Vec(dataBlockType, dataBeatType)
  def typesWithMultibeatData= Vec(dataBlockType)

  def apply(
      is_builtin_type: Bool,
      g_type: UInt,
      client_xact_id: UInt, 
      manager_xact_id: UInt,
      addr_beat: UInt = UInt(0),
      data: UInt = UInt(0)): Grant = {
    val gnt = new Grant
    gnt.is_builtin_type := is_builtin_type
    gnt.g_type := g_type
    gnt.client_xact_id := client_xact_id
    gnt.manager_xact_id := manager_xact_id
    gnt.addr_beat := addr_beat
    gnt.data := data
    gnt
  }
}

class Finish extends ClientToManagerChannel with HasManagerTransactionId

// Complete IO definitions for two types of TileLink clients 
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
  io.out.probe.ready := Bool(false)
  io.out.release.valid := Bool(false)
}
object TileLinkIOWrapper {
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
  def apply(tl: TileLinkIO) = tl
}

abstract trait HasArbiterTypes {
  val arbN: Int
  type ManagerSourcedWithId = ManagerToClientChannel with HasClientTransactionId
  type ClientSourcedWithId = ClientToManagerChannel with HasClientTransactionId
  type ClientSourcedWithIdAndData = ClientToManagerChannel with 
                                      HasClientTransactionId with 
                                      HasTileLinkData
}
// Utility functions for constructing TileLinkIO arbiters
abstract class TileLinkArbiterLike(val arbN: Int) extends TLModule
    with HasArbiterTypes {

  // These are filled in depending on whether the arbiter mucks with the 
  // client ids and then needs to revert them on the way back
  def clientSourcedClientXactId(in: ClientSourcedWithId, id: Int): Bits
  def managerSourcedClientXactId(in: ManagerSourcedWithId): Bits
  def arbIdx(in: ManagerSourcedWithId): UInt

  def hookupClientSource[M <: ClientSourcedWithIdAndData]
                        (ins: Seq[DecoupledIO[LogicalNetworkIO[M]]], 
                         out: DecoupledIO[LogicalNetworkIO[M]]) {
    def hasData(m: LogicalNetworkIO[M]) = m.payload.hasMultibeatData()
    val arb = Module(new LockingRRArbiter(out.bits.clone, arbN, params(TLDataBeats), Some(hasData _)))
    out <> arb.io.out
    ins.zipWithIndex.zip(arb.io.in).map{ case ((req,id), arb) => {
      arb.valid := req.valid
      arb.bits := req.bits
      arb.bits.payload.client_xact_id := clientSourcedClientXactId(req.bits.payload, id)
      req.ready := arb.ready
    }}
  }

  def hookupManagerSource[M <: ManagerSourcedWithId]
                        (ins: Seq[DecoupledIO[LogicalNetworkIO[M]]], 
                         out: DecoupledIO[LogicalNetworkIO[M]]) {
    out.ready := Bool(false)
    for (i <- 0 until arbN) {
      ins(i).valid := Bool(false)
      when (arbIdx(out.bits.payload) === UInt(i)) {
        ins(i).valid := out.valid
        out.ready := ins(i).ready
      }
      ins(i).bits := out.bits
      ins(i).bits.payload.client_xact_id := managerSourcedClientXactId(out.bits.payload) 
    }
  }
}

abstract class UncachedTileLinkIOArbiter(n: Int) 
    extends TileLinkArbiterLike(n) {
  val io = new Bundle {
    val in = Vec.fill(n){new UncachedTileLinkIO}.flip
    val out = new UncachedTileLinkIO
  }

  hookupClientSource(io.in.map(_.acquire), io.out.acquire)
  hookupManagerSource(io.in.map(_.grant), io.out.grant)

  val finish_arb = Module(new RRArbiter(new LogicalNetworkIO(new Finish), n))
  io.out.finish <> finish_arb.io.out
  finish_arb.io.in zip io.in map { case (arb, req) => arb <> req.finish }
}

abstract class TileLinkIOArbiter(n: Int) extends TileLinkArbiterLike(n) {
  val io = new Bundle {
    val in = Vec.fill(n){new TileLinkIO}.flip
    val out = new TileLinkIO
  }

  hookupClientSource(io.in.map(_.acquire), io.out.acquire)
  hookupClientSource(io.in.map(_.release), io.out.release)
  hookupManagerSource(io.in.map(_.grant), io.out.grant)

  io.in.map{ _.probe.valid := io.out.probe.valid }
  io.in.map{ _.probe.bits := io.out.probe.bits }
  io.out.probe.ready := io.in.map(_.probe.ready).reduce(_||_)

  val finish_arb = Module(new RRArbiter(new LogicalNetworkIO(new Finish), n))
  io.out.finish <> finish_arb.io.out
  finish_arb.io.in zip io.in map { case (arb, req) => arb <> req.finish }
}

// Appends the port index of the arbiter to the client_xact_id
abstract trait AppendsArbiterId extends HasArbiterTypes {
  def clientSourcedClientXactId(in: ClientSourcedWithId, id: Int) =
    Cat(in.client_xact_id, UInt(id, log2Up(arbN)))
  def managerSourcedClientXactId(in: ManagerSourcedWithId) = 
    in.client_xact_id >> UInt(log2Up(arbN))
  def arbIdx(in: ManagerSourcedWithId) = in.client_xact_id(log2Up(arbN)-1,0).toUInt
}

// Uses the client_xact_id as is (assumes it has been set to port index)
abstract trait PassesId extends HasArbiterTypes {
  def clientSourcedClientXactId(in: ClientSourcedWithId, id: Int) = in.client_xact_id
  def managerSourcedClientXactId(in: ManagerSourcedWithId) = in.client_xact_id
  def arbIdx(in: ManagerSourcedWithId) = in.client_xact_id
}

// Overwrites some default client_xact_id with the port idx
abstract trait UsesNewId extends HasArbiterTypes {
  def clientSourcedClientXactId(in: ClientSourcedWithId, id: Int) = UInt(id, log2Up(arbN))
  def managerSourcedClientXactId(in: ManagerSourcedWithId) = UInt(0)
  def arbIdx(in: ManagerSourcedWithId) = in.client_xact_id
}

// Mix-in id generation traits to make concrete arbiter classes
class UncachedTileLinkIOArbiterThatAppendsArbiterId(val n: Int) extends UncachedTileLinkIOArbiter(n) with AppendsArbiterId
class UncachedTileLinkIOArbiterThatPassesId(val n: Int) extends UncachedTileLinkIOArbiter(n) with PassesId
class UncachedTileLinkIOArbiterThatUsesNewId(val n: Int) extends UncachedTileLinkIOArbiter(n) with UsesNewId
class TileLinkIOArbiterThatAppendsArbiterId(val n: Int) extends TileLinkIOArbiter(n) with AppendsArbiterId
class TileLinkIOArbiterThatPassesId(val n: Int) extends TileLinkIOArbiter(n) with PassesId
class TileLinkIOArbiterThatUsesNewId(val n: Int) extends TileLinkIOArbiter(n) with UsesNewId
