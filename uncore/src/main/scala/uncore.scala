// See LICENSE for license details.

package uncore
import Chisel._

case object NReleaseTransactors extends Field[Int]
case object NProbeTransactors extends Field[Int]
case object NAcquireTransactors extends Field[Int]

trait HasCoherenceAgentParameters {
  implicit val p: Parameters
  val nReleaseTransactors = 1
  val nAcquireTransactors = p(NAcquireTransactors)
  val nTransactors = nReleaseTransactors + nAcquireTransactors
  val outerTLParams = p.alterPartial({ case TLId => p(OuterTLId)})
  val outerDataBeats = outerTLParams(TLDataBeats)
  val outerDataBits = outerTLParams(TLDataBits)
  val outerBeatAddrBits = log2Up(outerDataBeats)
  val outerByteAddrBits = log2Up(outerDataBits/8)
  val innerTLParams = p.alterPartial({case TLId => p(InnerTLId)})
  val innerDataBeats = innerTLParams(TLDataBeats)
  val innerDataBits = innerTLParams(TLDataBits)
  val innerWriteMaskBits = innerTLParams(TLWriteMaskBits)
  val innerBeatAddrBits = log2Up(innerDataBeats)
  val innerByteAddrBits = log2Up(innerDataBits/8)
  require(outerDataBeats == innerDataBeats) //TODO: must fix all xact_data Vecs to remove this requirement
}

abstract class CoherenceAgentModule(implicit val p: Parameters) extends Module
  with HasCoherenceAgentParameters
abstract class CoherenceAgentBundle(implicit val p: Parameters) extends junctions.ParameterizedBundle()(p)
   with HasCoherenceAgentParameters

trait HasCoherenceAgentWiringHelpers {
  def doOutputArbitration[T <: TileLinkChannel](
      out: DecoupledIO[T],
      ins: Seq[DecoupledIO[T]]) {
    def lock(o: T) = o.hasMultibeatData()
    val arb = Module(new LockingRRArbiter(out.bits, ins.size, out.bits.tlDataBeats, lock _))
    out <> arb.io.out
    arb.io.in <> ins
  }

  def doInputRouting[T <: Bundle with HasManagerTransactionId](
        in: DecoupledIO[T],
        outs: Seq[DecoupledIO[T]]) {
    val idx = in.bits.manager_xact_id
    outs.map(_.bits := in.bits)
    outs.zipWithIndex.map { case (o,i) => o.valid := in.valid && idx === UInt(i) }
    in.ready := Vec(outs.map(_.ready)).read(idx)
  }
}

trait HasInnerTLIO extends HasCoherenceAgentParameters {
  val inner = Bundle(new ManagerTileLinkIO)(innerTLParams)
  val incoherent = Vec(Bool(), inner.tlNCachingClients).asInput
  def iacq(dummy: Int = 0) = inner.acquire.bits
  def iprb(dummy: Int = 0) = inner.probe.bits
  def irel(dummy: Int = 0) = inner.release.bits
  def ignt(dummy: Int = 0) = inner.grant.bits
  def ifin(dummy: Int = 0) = inner.finish.bits
}

trait HasUncachedOuterTLIO extends HasCoherenceAgentParameters {
  val outer = Bundle(new ClientUncachedTileLinkIO)(outerTLParams)
  def oacq(dummy: Int = 0) = outer.acquire.bits
  def ognt(dummy: Int = 0) = outer.grant.bits
}

trait HasCachedOuterTLIO extends HasCoherenceAgentParameters {
  val outer = Bundle(new ClientTileLinkIO)(outerTLParams)
  def oacq(dummy: Int = 0) = outer.acquire.bits
  def oprb(dummy: Int = 0) = outer.probe.bits
  def orel(dummy: Int = 0) = outer.release.bits
  def ognt(dummy: Int = 0) = outer.grant.bits
}

class ManagerTLIO(implicit p: Parameters) extends CoherenceAgentBundle()(p)
  with HasInnerTLIO
  with HasUncachedOuterTLIO

abstract class CoherenceAgent(implicit p: Parameters) extends CoherenceAgentModule()(p) {
  def innerTL: ManagerTileLinkIO
  def outerTL: ClientTileLinkIO
  def incoherent: Vec[Bool]
}

abstract class ManagerCoherenceAgent(implicit p: Parameters) extends CoherenceAgent()(p)
    with HasCoherenceAgentWiringHelpers {
  val io = new ManagerTLIO
  def innerTL = io.inner
  def outerTL = TileLinkIOWrapper(io.outer)(outerTLParams)
  def incoherent = io.incoherent
}

class HierarchicalTLIO(implicit p: Parameters) extends CoherenceAgentBundle()(p)
  with HasInnerTLIO
  with HasCachedOuterTLIO

abstract class HierarchicalCoherenceAgent(implicit p: Parameters) extends CoherenceAgent()(p) {
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

class ManagerXactTrackerIO(implicit p: Parameters) extends ManagerTLIO()(p)
  with HasTrackerConflictIO

class HierarchicalXactTrackerIO(implicit p: Parameters) extends HierarchicalTLIO()(p)
  with HasTrackerConflictIO

abstract class XactTracker(implicit p: Parameters) extends CoherenceAgentModule()(p)
    with HasDataBeatCounters {
  def addPendingBitWhenBeat[T <: HasBeat](inc: Bool, in: T): UInt =
    Fill(in.tlDataBeats, inc) &  UIntToOH(in.addr_beat)
  def dropPendingBitWhenBeat[T <: HasBeat](dec: Bool, in: T): UInt =
    ~Fill(in.tlDataBeats, dec) | ~UIntToOH(in.addr_beat)

  def addPendingBitWhenBeatHasData[T <: HasBeat](in: DecoupledIO[T]): UInt =
    addPendingBitWhenBeat(in.fire() && in.bits.hasData(), in.bits)

  def addPendingBitWhenBeatHasDataAndAllocs(in: DecoupledIO[AcquireFromSrc]): UInt =
    addPendingBitWhenBeat(in.fire() && in.bits.hasData() && in.bits.allocate(), in.bits)

  def addPendingBitWhenBeatIsGetOrAtomic(in: DecoupledIO[AcquireFromSrc]): UInt = {
    val a = in.bits
    val isGetOrAtomic = a.isBuiltInType() &&
      (Vec(Acquire.getType, Acquire.getBlockType, Acquire.putAtomicType).contains(a.a_type))
    addPendingBitWhenBeat(in.fire() && isGetOrAtomic, a)
  }

  def dropPendingBitWhenBeatHasData[T <: HasBeat](in: DecoupledIO[T]): UInt =
    dropPendingBitWhenBeat(in.fire() && in.bits.hasData(), in.bits)

  def dropPendingBitAtDest(in: DecoupledIO[ProbeToDst]): UInt =
    ~Fill(in.bits.tlNCachingClients, in.fire()) | ~UIntToOH(in.bits.client_id)
}
