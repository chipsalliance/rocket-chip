// See LICENSE for license details.

package uncore
import Chisel._
import scala.math.max

// Parameters exposed to the top-level design, set based on 
// external requirements or design space exploration
//
case object TLId extends Field[String] // Unique name per network
case object TLCoherence extends Field[CoherencePolicy]
case object TLAddrBits extends Field[Int]
case object TLManagerXactIdBits extends Field[Int]
case object TLClientXactIdBits extends Field[Int]
case object TLDataBits extends Field[Int]
case object TLDataBeats extends Field[Int]

abstract trait TileLinkParameters extends UsesParameters {
  val tlBlockAddrBits = params(TLAddrBits)
  val tlClientXactIdBits = params(TLClientXactIdBits)
  val tlManagerXactIdBits = params(TLManagerXactIdBits)
  val tlDataBits = params(TLDataBits)
  val tlDataBeats = params(TLDataBeats)
  val tlWriteMaskBits = if(tlDataBits/8 < 1) 1 else tlDataBits/8
  val tlBeatAddrBits = log2Up(tlDataBeats)
  val tlByteAddrBits = log2Up(tlWriteMaskBits)
  val tlAtomicOpcodeBits = M_SZ
  val tlUncachedOperandSizeBits = MT_SZ
  val tlSubblockUnionBits = max(tlWriteMaskBits, 
                                 (tlByteAddrBits +
                                   tlUncachedOperandSizeBits + 
                                   tlAtomicOpcodeBits)) + 1
  val co = params(TLCoherence)
  val networkPreservesPointToPointOrdering = false //TODO: check physical network type
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

abstract trait HasTileLinkData extends HasTileLinkBeatId {
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
  val builtin_type = Bool()
  val a_type = UInt(width = max(log2Up(Acquire.nBuiltinAcquireTypes), co.acquireTypeWidth))
  val subblock =  Bits(width = tlSubblockUnionBits)

  // Utility funcs for accessing subblock union
  val opSizeOff = tlByteAddrBits + 1
  val opCodeOff = tlUncachedOperandSizeBits + opSizeOff
  val opMSB = tlAtomicOpcodeBits + opCodeOff
  def allocate(dummy: Int = 0) = subblock(0)
  def addr_byte(dummy: Int = 0) = subblock(opSizeOff-1, 1)
  def op_size(dummy: Int = 0) = subblock(opCodeOff-1, opSizeOff)
  def op_code(dummy: Int = 0) = subblock(opMSB-1, opCodeOff)
  def write_mask(dummy: Int = 0) = subblock(tlWriteMaskBits, 1)
  def addr(dummy: Int = 0) = Cat(addr_block, addr_beat, this.addr_byte(0))

  // Other helper funcs
  def is(t: UInt) = a_type === t

  def hasData(dummy: Int = 0): Bool = builtin_type && Acquire.typesWithData.contains(a_type)

  def hasMultibeatData(dummy: Int = 0): Bool = Bool(tlDataBeats > 1) && builtin_type &&
                                           Acquire.typesWithMultibeatData.contains(a_type)

  //TODO: This function is a hack to support Rocket icache snooping Rocket nbdcache:
  def requiresSelfProbe(dummy: Int = 0) = builtin_type && Acquire.requiresSelfProbe(a_type)

  def makeProbe(meta: ManagerMetadata = co.managerMetadataOnFlush): Probe =
    Probe(co.getProbeType(this, meta), this.addr_block)

  def makeGrant(
      manager_xact_id: UInt, 
      meta: ManagerMetadata = co.managerMetadataOnFlush,
      addr_beat: UInt = UInt(0),
      data: UInt = UInt(0)): Grant = {
    Grant(
      builtin_type = this.builtin_type,
      g_type = co.getGrantType(this, meta),
      client_xact_id = this.client_xact_id,
      manager_xact_id = manager_xact_id,
      addr_beat = addr_beat,
      data = data
    )
  }
}

object Acquire {
  val nBuiltinAcquireTypes = 5
  //TODO: Use Enum
  def uncachedRead       = UInt(0)
  def uncachedReadBlock  = UInt(1)
  def uncachedWrite      = UInt(2)
  def uncachedWriteBlock = UInt(3)
  def uncachedAtomic     = UInt(4)
  def typesWithData = Vec(uncachedWrite, uncachedWriteBlock, uncachedAtomic)
  def typesWithMultibeatData = Vec(uncachedWriteBlock)
  def requiresOuterRead(a_type: UInt) = a_type != uncachedWriteBlock
  def requiresOuterWrite(a_type: UInt) = typesWithData.contains(a_type)
  //TODO: This function is a hack to support Rocket icache snooping Rocket nbdcache:
  def requiresSelfProbe(a_type: UInt) = a_type === uncachedReadBlock 

  def fullWriteMask = SInt(-1, width = new Acquire().tlWriteMaskBits).toUInt

  // Most generic constructor
  def apply(
      builtin_type: Bool,
      a_type: Bits,
      client_xact_id: UInt,
      addr_block: UInt,
      addr_beat: UInt = UInt(0),
      data: UInt = UInt(0),
      subblock: UInt = UInt(0)): Acquire = {
    val acq = new Acquire
    acq.builtin_type := builtin_type
    acq.a_type := a_type
    acq.client_xact_id := client_xact_id
    acq.addr_block := addr_block
    acq.addr_beat := addr_beat
    acq.data := data
    acq.subblock := subblock
    acq
  }
  // For cached types
  def apply(a_type: Bits, client_xact_id: UInt, addr_block: UInt): Acquire = {
    apply(
      builtin_type = Bool(false),
      a_type = a_type,
      client_xact_id = client_xact_id,
      addr_block = addr_block)
  }
  // Copy constructor
  def apply(a: Acquire): Acquire = {
    val acq = new Acquire
    acq := a
    acq
  }
}

// Asks for a single TileLink beat of data
object UncachedRead {
  def apply(
      client_xact_id: UInt, 
      addr_block: UInt, 
      addr_beat: UInt, 
      alloc: Bool = Bool(true)): Acquire = {
    Acquire(
      builtin_type = Bool(true),
      a_type = Acquire.uncachedRead,
      client_xact_id = client_xact_id,
      addr_block = addr_block,
      addr_beat = addr_beat,
      subblock = alloc)
  }
}

// Asks for an entire cache block of data
object UncachedReadBlock {
  def apply(
      client_xact_id: UInt = UInt(0), 
      addr_block: UInt, 
      alloc: Bool = Bool(true)): Acquire = {
    Acquire(
      builtin_type = Bool(true),
      a_type = Acquire.uncachedReadBlock,
      client_xact_id = client_xact_id, 
      addr_block = addr_block,
      subblock = alloc.toUInt)
  }
}

object UncachedWrite {
  def apply(
      client_xact_id: UInt,
      addr_block: UInt,
      addr_beat: UInt,
      data: UInt,
      write_mask: UInt = Acquire.fullWriteMask,
      alloc: Bool = Bool(true)): Acquire = {
    Acquire(
      builtin_type = Bool(true),
      a_type = Acquire.uncachedWrite,
      addr_block = addr_block,
      addr_beat = addr_beat,
      client_xact_id = client_xact_id,
      data = data,
      subblock = Cat(write_mask, alloc))
  }
}

// For full block of data
object UncachedWriteBlock {
  def apply(
      client_xact_id: UInt,
      addr_block: UInt,
      addr_beat: UInt,
      data: UInt,
      alloc: Bool = Bool(true)): Acquire = {
    Acquire(
      builtin_type = Bool(true),
      a_type = Acquire.uncachedWriteBlock,
      client_xact_id = client_xact_id,
      addr_block = addr_block,
      addr_beat = addr_beat,
      data = data,
      subblock = Cat(Acquire.fullWriteMask, alloc))
  }
}

object UncachedAtomic {
  def apply(
      client_xact_id: UInt,
      addr_block: UInt,
      addr_beat: UInt,
      addr_byte: UInt,
      atomic_opcode: UInt,
      operand_size: UInt,
      data: UInt): Acquire = {
    Acquire(
      builtin_type = Bool(true),
      a_type = Acquire.uncachedAtomic, 
      client_xact_id = client_xact_id, 
      addr_block = addr_block, 
      addr_beat = addr_beat, 
      data = data,
      subblock = Cat(atomic_opcode, operand_size, addr_byte, Bool(true)))
  }
}

class Probe extends ManagerToClientChannel 
    with HasCacheBlockAddress {
  val p_type = UInt(width = co.probeTypeWidth)

  def is(t: UInt) = p_type === t
  def makeRelease(
      client_xact_id: UInt,
      meta: ClientMetadata = co.clientMetadataOnFlush,
      addr_beat: UInt = UInt(0),
      data: UInt = UInt(0)): Release = {
    Release(
      voluntary = Bool(false),
      r_type = co.getReleaseType(this, meta),
      client_xact_id = client_xact_id,
      addr_block = this.addr_block,
      addr_beat = addr_beat,
      data = data)
  }
}

object Probe {
  val co = new Probe().co
  def apply(p_type: UInt, addr_block: UInt) = {
    val prb = new Probe
    prb.p_type := p_type
    prb.addr_block := addr_block
    prb
  }

  def onVoluntaryWriteback(meta: ManagerMetadata, addr_block: UInt): Probe = {
    apply(co.getProbeType(M_FLUSH, meta), addr_block)
  }
}


class Release extends ClientToManagerChannel 
    with HasCacheBlockAddress 
    with HasClientTransactionId 
    with HasTileLinkData {
  val r_type = UInt(width = co.releaseTypeWidth)
  val voluntary = Bool()

  // Helper funcs
  def is(t: UInt) = r_type === t
  def hasData(dummy: Int = 0) = co.releaseTypesWithData.contains(r_type)
  //TODO: Assumes all releases write back full cache blocks:
  def hasMultibeatData(dummy: Int = 0) = Bool(tlDataBeats > 1) && co.releaseTypesWithData.contains(r_type)
  def isVoluntary(dummy: Int = 0) = voluntary
  def requiresAck(dummy: Int = 0) = !Bool(networkPreservesPointToPointOrdering)

  def makeGrant(
      manager_xact_id: UInt,
      meta: ManagerMetadata = co.managerMetadataOnFlush): Grant = {
    Grant(
      g_type = Grant.voluntaryAck,
      builtin_type = Bool(true), // Grant.voluntaryAck is built-in type
      client_xact_id = this.client_xact_id,
      manager_xact_id = manager_xact_id
    )
  }
}

object Release {
  val co = new Release().co
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

  def makeVoluntaryWriteback(
      meta: ClientMetadata,
      client_xact_id: UInt,
      addr_block: UInt,
      addr_beat: UInt = UInt(0),
      data: UInt = UInt(0)): Release = {
    Release(
      voluntary = Bool(true),
      r_type = co.getReleaseType(M_FLUSH, meta),
      client_xact_id = client_xact_id,
      addr_block = addr_block,
      addr_beat = addr_beat,
      data = data)
  }
}

class Grant extends ManagerToClientChannel 
    with HasTileLinkData 
    with HasClientTransactionId 
    with HasManagerTransactionId {
  val builtin_type = Bool()
  val g_type = UInt(width = max(log2Up(Grant.nBuiltinGrantTypes), co.grantTypeWidth))

  // Helper funcs
  def is(t: UInt) = g_type === t
  def hasData(dummy: Int = 0): Bool = Mux(builtin_type,
                                        Grant.typesWithData.contains(g_type),
                                        co.grantTypesWithData.contains(g_type))
  def hasMultibeatData(dummy: Int = 0): Bool = 
    Bool(tlDataBeats > 1) && Mux(builtin_type,
                               Grant.typesWithMultibeatData.contains(g_type),
                               co.grantTypesWithData.contains(g_type))
  def isVoluntary(dummy: Int = 0): Bool = builtin_type && (g_type === Grant.voluntaryAck)
  def requiresAck(dummy: Int = 0): Bool = !Bool(networkPreservesPointToPointOrdering) && !isVoluntary()
  def makeFinish(dummy: Int = 0): Finish = {
    val f = new Finish
    f.manager_xact_id := this.manager_xact_id
    f
  }
}

object Grant {
  val nBuiltinGrantTypes = 5
  //TODO Use Enum
  def voluntaryAck       = UInt(0)
  def uncachedRead       = UInt(1)
  def uncachedReadBlock  = UInt(2)
  def uncachedWrite      = UInt(3)
  def uncachedAtomic     = UInt(4)
  def typesWithData = Vec(uncachedRead, uncachedReadBlock, uncachedAtomic)
  def typesWithMultibeatData= Vec(uncachedReadBlock)

  def apply(
      builtin_type: Bool,
      g_type: UInt,
      client_xact_id: UInt, 
      manager_xact_id: UInt,
      addr_beat: UInt = UInt(0),
      data: UInt = UInt(0)): Grant = {
    val gnt = new Grant
    gnt.builtin_type := builtin_type
    gnt.g_type := g_type
    gnt.client_xact_id := client_xact_id
    gnt.manager_xact_id := manager_xact_id
    gnt.addr_beat := addr_beat
    gnt.data := data
    gnt
  }

  def getGrantTypeForUncached(a: Acquire): UInt =  {
    MuxLookup(a.a_type, Grant.uncachedRead, Array(
      Acquire.uncachedRead -> Grant.uncachedRead,
      Acquire.uncachedReadBlock -> Grant.uncachedReadBlock,
      Acquire.uncachedWrite -> Grant.uncachedWrite,
      Acquire.uncachedWriteBlock -> Grant.uncachedWrite,
      Acquire.uncachedAtomic -> Grant.uncachedAtomic
    ))
  }
}

class Finish extends ClientToManagerChannel with HasManagerTransactionId


// Complete IO definitions for two types of TileLink clients 
class UncachedTileLinkIO extends Bundle {
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
  def apply[T <: Data](utl: UncachedTileLinkIO) = {
    val conv = Module(new TileLinkIOWrapper)
    conv.io.in <> utl
    conv.io.out
  }
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
