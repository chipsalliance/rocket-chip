// See LICENSE for license details.

package uncore
import Chisel._
import scala.reflect.ClassTag

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
  val nCoherentClients = params(NCoherentClients)
  val nIncoherentClients = params(NIncoherentClients)
  val nClients = nCoherentClients + nIncoherentClients
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
  def doOutputArbitration[T <: Data : ClassTag](
      out: DecoupledIO[T],
      ins: Seq[DecoupledIO[T]]) {
    val arb = Module(new RRArbiter(out.bits.clone, ins.size))
    out <> arb.io.out
    arb.io.in zip ins map { case (a, in) => a <> in }
  }

  def doOutputArbitration[T <: HasTileLinkData : ClassTag, S <: LogicalNetworkIO[T] : ClassTag](
      out: DecoupledIO[S],
      ins: Seq[DecoupledIO[S]]) {
    def lock(o: LogicalNetworkIO[T]) = o.payload.hasMultibeatData()
    val arb = Module(new LockingRRArbiter(
                          out.bits.clone,
                          ins.size, 
                          out.bits.payload.tlDataBeats,
                          lock _))
    out <> arb.io.out
    arb.io.in zip ins map { case (a, in) => a <> in }
  }

  def doInputRouting[T <: HasL2Id](in: ValidIO[T], outs: Seq[ValidIO[T]]) {
    val idx = in.bits.id
    outs.map(_.bits := in.bits)
    outs.zipWithIndex.map { case (o,i) => o.valid := in.valid && idx === UInt(i) }
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
  val incoherent = Vec.fill(nCoherentClients){Bool()}.asInput
  def iacq(dummy: Int = 0) = inner.acquire.bits.payload
  def iprb(dummy: Int = 0) = inner.probe.bits.payload
  def irel(dummy: Int = 0) = inner.release.bits.payload
  def ignt(dummy: Int = 0) = inner.grant.bits.payload
  def ifin(dummy: Int = 0) = inner.finish.bits.payload
}

trait HasUncachedOuterTLIO extends CoherenceAgentBundle {
  val outer = Bundle(new UncachedTileLinkIO)(outerTLParams)
  def oacq(dummy: Int = 0) = outer.acquire.bits.payload
  def ognt(dummy: Int = 0) = outer.grant.bits.payload
  def ofin(dummy: Int = 0) = outer.finish.bits.payload
}

trait HasCachedOuterTLIO extends CoherenceAgentBundle {
  val outer = Bundle(new TileLinkIO)(outerTLParams)
  def oacq(dummy: Int = 0) = outer.acquire.bits.payload
  def oprb(dummy: Int = 0) = outer.probe.bits.payload
  def orel(dummy: Int = 0) = outer.release.bits.payload
  def ognt(dummy: Int = 0) = outer.grant.bits.payload
  def ofin(dummy: Int = 0) = outer.finish.bits.payload
}

class ManagerTLIO extends HasInnerTLIO with HasUncachedOuterTLIO

abstract class CoherenceAgent extends CoherenceAgentModule {
  def innerTL: TileLinkIO
  def outerTL: TileLinkIO
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

abstract class XactTracker extends CoherenceAgentModule {
  def connectDataBeatCounter[S <: HasTileLinkData : ClassTag](inc: Bool, data: S, beat: UInt) = {
    val multi = data.hasMultibeatData()
    val (multi_cnt, multi_done) = Counter(inc && multi, data.tlDataBeats)
    val cnt = Mux(multi, multi_cnt, beat)
    val done = Mux(multi, multi_done, inc)
    (cnt, done)
  }

  def connectOutgoingDataBeatCounter[T <: HasTileLinkData : ClassTag](
      in: DecoupledIO[LogicalNetworkIO[T]], 
      beat: UInt = UInt(0)) = {
    connectDataBeatCounter(in.fire(), in.bits.payload, beat)
  }

  def connectIncomingDataBeatCounter[T <: HasTileLinkData : ClassTag](in: DecoupledIO[LogicalNetworkIO[T]]) = {
    connectDataBeatCounter(in.fire(), in.bits.payload, UInt(0))._2
  }

  def addPendingBitWhenHasData[T <: HasTileLinkData with HasTileLinkBeatId](in: DecoupledIO[LogicalNetworkIO[T]]) = {
    (Fill(in.bits.payload.tlDataBeats, in.fire() && in.bits.payload.hasData()) &
      UIntToOH(in.bits.payload.addr_beat))
  }

  def addPendingBitWhenWmaskIsNotFull(in: DecoupledIO[LogicalNetworkIO[Acquire]]) = {
    (Fill(in.bits.payload.tlDataBeats, in.fire() && !in.bits.payload.wmask().andR) &
      UIntToOH(in.bits.payload.addr_beat))
  }
}
