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
  val tlAddrBits = params(TLAddrBits)
  val tlClientXactIdBits = params(TLClientXactIdBits)
  val tlManagerXactIdBits = params(TLManagerXactIdBits)
  val tlDataBits = params(TLDataBits)
  val tlDataBeats = params(TLDataBeats)
  val tlWriteMaskBits = if(tlDataBits/8 < 1) 1 else tlDataBits
  val tlSubblockAddrBits = log2Up(tlWriteMaskBits)
  val tlAtomicOpcodeBits = log2Up(NUM_XA_OPS)
  val tlUncachedOperandSizeBits = MT_SZ
  val tlSubblockUnionBits = max(tlWriteMaskBits, 
                             (tlSubblockAddrBits + 
                                tlUncachedOperandSizeBits + 
                                tlAtomicOpcodeBits)) + 1
  val co = params(TLCoherence)
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
trait HasPhysicalAddress extends TLBundle {
  val addr = UInt(width = tlAddrBits)
}

trait HasClientTransactionId extends TLBundle {
  val client_xact_id = Bits(width = tlClientXactIdBits)
}

trait HasManagerTransactionId extends TLBundle {
  val manager_xact_id = Bits(width = tlManagerXactIdBits)
}

trait HasTileLinkData extends TLBundle {
  val data = UInt(width = tlDataBits)
}

// Actual TileLink channel bundle definitions

class Acquire extends ClientToManagerChannel 
    with HasPhysicalAddress 
    with HasClientTransactionId 
    with HasTileLinkData {
  val uncached = Bool()
  val a_type = UInt(width = max(log2Up(Acquire.nUncachedAcquireTypes), co.acquireTypeWidth))
  val subblock =  Bits(width = tlSubblockUnionBits)
  val sbAddrOff = tlSubblockAddrBits + tlUncachedOperandSizeBits
  val opSzOff = tlUncachedOperandSizeBits + sbAddrOff
  def allocate(dummy: Int = 0) = subblock(0)
  def operand_sz(dummy: Int = 0) = subblock(tlUncachedOperandSizeBits, 1)
  def subblock_addr(dummy: Int = 0) = subblock(sbAddrOff, tlUncachedOperandSizeBits+1)
  def atomic_op(dummy: Int = 0) = subblock(opSzOff, sbAddrOff+1)
  def write_mask(dummy: Int = 0) = subblock(tlWriteMaskBits, 1)
  def is(t: UInt) = a_type === t
}

object Acquire {
  val nUncachedAcquireTypes = 3
  //TODO: val uncachedRead :: uncachedWrite :: uncachedAtomic :: Nil = Enum(UInt(), nUncachedAcquireTypes)
  def uncachedRead = UInt(0)
  def uncachedWrite = UInt(1)
  def uncachedAtomic = UInt(2)
  def hasData(a_type: UInt) = Vec(uncachedWrite, uncachedAtomic).contains(a_type)
  def requiresOuterRead(a_type: UInt) = a_type != uncachedWrite
  def requiresOuterWrite(a_type: UInt) = a_type === uncachedWrite

  def apply(uncached: Bool, a_type: Bits, addr: UInt, client_xact_id: UInt, data: UInt, subblock: UInt): Acquire = {
    val acq = new Acquire
    acq.uncached := uncached
    acq.a_type := a_type
    acq.addr := addr
    acq.client_xact_id := client_xact_id
    acq.data := data
    acq.subblock := subblock
    acq
  }
  def apply(a_type: Bits, addr: UInt, client_xact_id: UInt, data: UInt): Acquire = {
    apply(Bool(false), a_type, addr, client_xact_id, data, UInt(0))
  }
  def apply(a_type: Bits, addr: UInt, client_xact_id: UInt): Acquire = {
    apply(a_type, addr, client_xact_id, UInt(0))
  }
  def apply(a: Acquire): Acquire = {
    val acq = new Acquire
    acq := a
    acq
  }
}

object UncachedRead {
  def apply(addr: UInt, client_xact_id: UInt, subblock_addr: UInt, operand_sz: UInt, alloc: Bool): Acquire = {
    val acq = Acquire(Acquire.uncachedRead, addr, client_xact_id)
    acq.uncached := Bool(true)
    acq.subblock := Cat(subblock_addr, operand_sz, alloc)
    acq
  }
  def apply(addr: UInt, client_xact_id: UInt): Acquire = {
    apply(addr, client_xact_id, UInt(0), MT_CB, Bool(true))
  }
  def apply(addr: UInt): Acquire = {
    apply(addr, UInt(0))
  }
}

object UncachedWrite {
  def apply(addr: UInt, client_xact_id: UInt, write_mask: Bits, alloc: Bool, data: UInt): Acquire = {
    val acq = Acquire(Acquire.uncachedWrite, addr, client_xact_id, data)
    acq.uncached := Bool(true)
    acq.subblock := Cat(write_mask, alloc)
    acq
  }
  def apply(addr: UInt, client_xact_id: UInt, data: UInt): Acquire = {
    apply(addr, client_xact_id, SInt(-1), Bool(true), data)
  }
  def apply(addr: UInt, data: UInt): Acquire = {
    apply(addr, UInt(0), data)
  }
}

object UncachedAtomic {
  def apply(addr: UInt, client_xact_id: UInt, atomic_opcode: UInt, 
      subblock_addr: UInt, operand_sz: UInt, data: UInt): Acquire = {
    val acq = Acquire(Acquire.uncachedAtomic, addr, client_xact_id, data)
    acq.uncached := Bool(true)
    acq.subblock := Cat(atomic_opcode, subblock_addr, operand_sz, Bool(true))
    acq
  }
}

class Probe extends ManagerToClientChannel 
    with HasPhysicalAddress {
  val p_type = UInt(width = co.probeTypeWidth)
  def is(t: UInt) = p_type === t
}

object Probe {
  def apply(p_type: UInt, addr: UInt) = {
    val prb = new Probe
    prb.p_type := p_type
    prb.addr := addr
    prb
  }
}

class Release extends ClientToManagerChannel 
    with HasPhysicalAddress 
    with HasClientTransactionId 
    with HasTileLinkData {
  val r_type = UInt(width = co.releaseTypeWidth)
  def is(t: UInt) = r_type === t
}

object Release {
  def apply(r_type: UInt, addr: UInt, client_xact_id: UInt, data: UInt): Release = {
    val rel = new Release
    rel.r_type := r_type
    rel.addr := addr
    rel.client_xact_id := client_xact_id
    rel.data := data
    rel
  }
  def apply(r_type: UInt, addr: UInt, client_xact_id: UInt): Release = {
    apply(r_type, addr, client_xact_id, UInt(0))
  }
  def apply(r_type: UInt, addr: UInt): Release = {
    apply(r_type, addr, UInt(0), UInt(0))
  }
}

class Grant extends ManagerToClientChannel 
    with HasTileLinkData 
    with HasClientTransactionId 
    with HasManagerTransactionId {
  val uncached = Bool()
  val g_type = UInt(width = max(log2Up(Grant.nUncachedGrantTypes), co.grantTypeWidth))
  def is(t: UInt) = g_type === t
}

object Grant {
  val nUncachedGrantTypes = 3
  //TODO val uncachedRead :: uncachedWrite :: uncachedAtomic :: Nil = Enum(UInt(), nUncachedGrantTypes)
  def uncachedRead = UInt(0)
  def uncachedWrite = UInt(1)
  def uncachedAtomic = UInt(2)
  def hasData(g_type: UInt) = Vec(uncachedRead, uncachedAtomic).contains(g_type)

  def apply(uncached: Bool, g_type: UInt, client_xact_id: UInt, manager_xact_id: UInt, data: UInt): Grant = {
    val gnt = new Grant
    gnt.uncached := uncached
    gnt.g_type := g_type
    gnt.client_xact_id := client_xact_id
    gnt.manager_xact_id := manager_xact_id
    gnt.data := data
    gnt
  }
  def apply(uncached: Bool, g_type: UInt, client_xact_id: UInt, manager_xact_id: UInt): Grant = {
    apply(uncached, g_type, client_xact_id, manager_xact_id, UInt(0))
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
  def apply[T <: Data](uncached: UncachedTileLinkIO) = {
    val conv = Module(new TileLinkIOWrapper)
    conv.io.in <> uncached
    conv.io.out
  }
}

// Utility functions for constructing TileLinkIO arbiters
abstract class TileLinkArbiterLike(val arbN: Int) extends TLModule {
  type ManagerSourcedWithId = ManagerToClientChannel with HasClientTransactionId
  type ClientSourcedWithId = ClientToManagerChannel with HasClientTransactionId 

  // These are filled in depending on whether the arbiter mucks with the 
  // client ids and then needs to revert them on the way back
  def clientSourcedClientXactId(in: ClientSourcedWithId, id: Int): Bits
  def managerSourcedClientXactId(in: ManagerSourcedWithId): Bits
  def arbIdx(in: ManagerSourcedWithId): UInt

  def hookupClientSource[M <: ClientSourcedWithId]
                        (ins: Seq[DecoupledIO[LogicalNetworkIO[M]]], 
                         out: DecoupledIO[LogicalNetworkIO[M]]) {
    def hasData(m: LogicalNetworkIO[M]) = co.messageHasData(m.payload)
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
abstract trait AppendsArbiterId {
  val arbN: Int
  def clientSourcedClientXactId(in: ClientToManagerChannel with HasClientTransactionId, id: Int) =
    Cat(in.client_xact_id, UInt(id, log2Up(arbN)))
  def managerSourcedClientXactId(in: ManagerToClientChannel with HasClientTransactionId) = 
    in.client_xact_id >> UInt(log2Up(arbN))
  def arbIdx(in: ManagerToClientChannel with HasClientTransactionId) = 
    in.client_xact_id(log2Up(arbN)-1,0).toUInt
}

// Uses the client_xact_id as is (assumes it has been set to port index)
abstract trait PassesId {
  def clientSourcedClientXactId(in: ClientToManagerChannel with HasClientTransactionId, id: Int) = 
    in.client_xact_id
  def managerSourcedClientXactId(in: ManagerToClientChannel with HasClientTransactionId) = 
    in.client_xact_id
  def arbIdx(in: ManagerToClientChannel with HasClientTransactionId) = 
    in.client_xact_id
}

// Overwrites some default client_xact_id with the port idx
abstract trait UsesNewId {
  val arbN: Int
  def clientSourcedClientXactId(in: ClientToManagerChannel with HasClientTransactionId, id: Int) = 
    UInt(id, log2Up(arbN))
  def managerSourcedClientXactId(in: ManagerToClientChannel with HasClientTransactionId) = 
    UInt(0)
  def arbIdx(in: ManagerToClientChannel with HasClientTransactionId) = 
    in.client_xact_id
}

// Mix-in id generation traits to make concrete arbiter classes
class UncachedTileLinkIOArbiterThatAppendsArbiterId(val n: Int) extends UncachedTileLinkIOArbiter(n) with AppendsArbiterId
class UncachedTileLinkIOArbiterThatPassesId(val n: Int) extends UncachedTileLinkIOArbiter(n) with PassesId
class UncachedTileLinkIOArbiterThatUsesNewId(val n: Int) extends UncachedTileLinkIOArbiter(n) with UsesNewId
class TileLinkIOArbiterThatAppendsArbiterId(val n: Int) extends TileLinkIOArbiter(n) with AppendsArbiterId
class TileLinkIOArbiterThatPassesId(val n: Int) extends TileLinkIOArbiter(n) with PassesId
class TileLinkIOArbiterThatUsesNewId(val n: Int) extends TileLinkIOArbiter(n) with UsesNewId
