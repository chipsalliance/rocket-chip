// See LICENSE for license details.

package uncore
import Chisel._
import scala.reflect._
import scala.reflect.runtime.universe._

case object NReleaseTransactors extends Field[Int]
case object NProbeTransactors extends Field[Int]
case object NAcquireTransactors extends Field[Int]
case object NIncoherentClients extends Field[Int]
case object NCoherentClients extends Field[Int]
case object L2CoherencePolicy extends Field[CoherencePolicy]

trait CoherenceAgentParameters extends UsesParameters {
  val nReleaseTransactors = 1
  val nAcquireTransactors = params(NAcquireTransactors)
  val nTransactors = nReleaseTransactors + nAcquireTransactors
  def outerTLParams = params.alterPartial({ case TLId => params(OuterTLId)})
  val outerDataBeats = outerTLParams(TLDataBeats)
  val outerDataBits = outerTLParams(TLDataBits)
  val outerBeatAddrBits = log2Up(outerDataBeats)
  val outerByteAddrBits = log2Up(outerDataBits/8)
  def innerTLParams = params.alterPartial({case TLId => params(InnerTLId)})
  val innerDataBeats = innerTLParams(TLDataBeats)
  val innerDataBits = innerTLParams(TLDataBits)
  val innerBeatAddrBits = log2Up(innerDataBeats)
  val innerByteAddrBits = log2Up(innerDataBits/8)
  require(outerDataBeats == innerDataBeats) //TODO: must fix all xact_data Vecs to remove this requirement
}
abstract class CoherenceAgentBundle extends Bundle with CoherenceAgentParameters
abstract class CoherenceAgentModule extends Module with CoherenceAgentParameters

trait HasCoherenceAgentWiringHelpers {
  def doOutputArbitration[T <: TileLinkChannel : ClassTag](
      out: DecoupledIO[LogicalNetworkIO[T]],
      ins: Seq[DecoupledIO[LogicalNetworkIO[T]]]) {
    def lock(o: LogicalNetworkIO[T]) = o.payload.hasMultibeatData()
    val arb = Module(new LockingRRArbiter( out.bits.clone, ins.size, out.bits.payload.tlDataBeats, lock _))
    out <> arb.io.out
    arb.io.in zip ins map { case (a, in) => a <> in }
  }

  def doInputRouting[T <: HasManagerTransactionId](
        in: DecoupledIO[LogicalNetworkIO[T]],
        outs: Seq[DecoupledIO[LogicalNetworkIO[T]]]) {
    val idx = in.bits.payload.manager_xact_id
    outs.map(_.bits := in.bits)
    outs.zipWithIndex.map { case (o,i) => o.valid := in.valid && idx === UInt(i) }
    in.ready := Vec(outs.map(_.ready)).read(idx)
  }
}

trait HasInnerTLIO extends CoherenceAgentBundle {
  val inner = Bundle(new TileLinkIO)(innerTLParams).flip
  val incoherent = Vec.fill(inner.tlNCoherentClients){Bool()}.asInput
  def iacq(dummy: Int = 0) = inner.acquire.bits.payload
  def iprb(dummy: Int = 0) = inner.probe.bits.payload
  def irel(dummy: Int = 0) = inner.release.bits.payload
  def ignt(dummy: Int = 0) = inner.grant.bits.payload
  def ifin(dummy: Int = 0) = inner.finish.bits.payload
}

trait HasUncachedOuterTLIO extends CoherenceAgentBundle {
  val outer = Bundle(new HeaderlessUncachedTileLinkIO)(outerTLParams)
  def oacq(dummy: Int = 0) = outer.acquire.bits
  def ognt(dummy: Int = 0) = outer.grant.bits
}

trait HasCachedOuterTLIO extends CoherenceAgentBundle {
  val outer = Bundle(new HeaderlessTileLinkIO)(outerTLParams)
  def oacq(dummy: Int = 0) = outer.acquire.bits
  def oprb(dummy: Int = 0) = outer.probe.bits
  def orel(dummy: Int = 0) = outer.release.bits
  def ognt(dummy: Int = 0) = outer.grant.bits
}

class ManagerTLIO extends HasInnerTLIO with HasUncachedOuterTLIO

abstract class CoherenceAgent extends CoherenceAgentModule {
  def innerTL: TileLinkIO
  def outerTL: HeaderlessTileLinkIO
  def incoherent: Vec[Bool]
}

abstract class ManagerCoherenceAgent extends CoherenceAgent
    with HasCoherenceAgentWiringHelpers {
  val io = new ManagerTLIO
  def innerTL = io.inner
  def outerTL = TileLinkIOWrapper(io.outer, outerTLParams)
  def incoherent = io.incoherent
}

class HierarchicalTLIO extends HasInnerTLIO with HasCachedOuterTLIO

abstract class HierarchicalCoherenceAgent extends CoherenceAgent {
  val io = new HierarchicalTLIO
  def innerTL = io.inner
  def outerTL = io.outer
  def incoherent = io.incoherent
}

trait HasTrackerConflictIO extends Bundle {
  val has_acquire_conflict = Bool(OUTPUT)
  val has_acquire_match = Bool(OUTPUT)
  val has_release_match = Bool(OUTPUT)
}

class ManagerXactTrackerIO extends ManagerTLIO with HasTrackerConflictIO
class HierarchicalXactTrackerIO extends HierarchicalTLIO with HasTrackerConflictIO

abstract class XactTracker extends CoherenceAgentModule
    with HasDataBeatCounters {
  def addPendingBitWhenBeat[T <: HasBeat](inc: Bool, in: T): UInt =
    Fill(in.tlDataBeats, inc) &  UIntToOH(in.addr_beat)
  def dropPendingBitWhenBeat[T <: HasBeat](dec: Bool, in: T): UInt =
    ~Fill(in.tlDataBeats, dec) | ~UIntToOH(in.addr_beat)

  def addPendingBitWhenBeatHasData[T <: Data : TypeTag](in: DecoupledIO[T]): UInt = {
    in.bits match {
      case p: HasBeat if typeTag[T].tpe <:< typeTag[HasBeat].tpe =>
        addPendingBitWhenBeat(in.fire() && p.hasData(), p)
      case ln: LNAcquire if typeTag[T].tpe <:< typeTag[LNAcquire].tpe =>
        addPendingBitWhenBeat(in.fire() && ln.payload.hasData(), ln.payload)
      case ln: LNRelease if typeTag[T].tpe <:< typeTag[LNRelease].tpe =>
        addPendingBitWhenBeat(in.fire() && ln.payload.hasData(), ln.payload)
      case ln: LNGrant if typeTag[T].tpe <:< typeTag[LNGrant].tpe =>
        addPendingBitWhenBeat(in.fire() && ln.payload.hasData(), ln.payload)
      case _ => { require(false, "Don't know how track beats of " + typeTag[T].tpe); UInt(0) }
    }
  }

  def addPendingBitWhenBeatIsGetOrAtomic(in: DecoupledIO[LogicalNetworkIO[Acquire]]): UInt = {
    val a = in.bits.payload
    val isGetOrAtomic = a.isBuiltInType() &&
                        (Vec(Acquire.getType, Acquire.getBlockType, Acquire.putAtomicType).contains(a.a_type))
    addPendingBitWhenBeat(in.fire() && isGetOrAtomic, in.bits.payload)
  }

  def dropPendingBitWhenBeatHasData[T <: Data : TypeTag](in: DecoupledIO[T]): UInt = {
    in.bits match {
      case p: HasBeat if typeTag[T].tpe <:< typeTag[HasBeat].tpe =>
        dropPendingBitWhenBeat(in.fire() && p.hasData(), p)
      case ln: LNAcquire if typeTag[T].tpe <:< typeTag[LNAcquire].tpe =>
        dropPendingBitWhenBeat(in.fire() && ln.payload.hasData(), ln.payload)
      case ln: LNRelease if typeTag[T].tpe <:< typeTag[LNRelease].tpe =>
        dropPendingBitWhenBeat(in.fire() && ln.payload.hasData(), ln.payload)
      case ln: LNGrant if typeTag[T].tpe <:< typeTag[LNGrant].tpe =>
        dropPendingBitWhenBeat(in.fire() && ln.payload.hasData(), ln.payload)
      case _ => { require(false, "Don't know how track beats of " + typeTag[T].tpe); UInt(0) }
    }
  }

  def dropPendingBitAtDest(in: DecoupledIO[LogicalNetworkIO[Probe]]): UInt = {
    ~Fill(in.bits.payload.tlNCoherentClients, in.fire()) | ~UIntToOH(in.bits.header.dst)
  }
}
