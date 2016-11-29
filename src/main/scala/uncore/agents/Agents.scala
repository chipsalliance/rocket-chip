// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package uncore.agents

import Chisel._
import config._
import junctions.PAddrBits
import uncore.tilelink._
import uncore.converters._
import uncore.coherence._
import uncore.util._
import util._

case object NReleaseTransactors extends Field[Int]
case object NProbeTransactors extends Field[Int]
case object NAcquireTransactors extends Field[Int]

trait HasCoherenceAgentParameters {
  implicit val p: Parameters
  val nReleaseTransactors = 1
  val nAcquireTransactors = p(NAcquireTransactors)
  val nTransactors = nReleaseTransactors + nAcquireTransactors
  val blockAddrBits = p(PAddrBits) - p(CacheBlockOffsetBits)
  val outerTLId = p(OuterTLId)
  val outerTLParams = p(TLKey(outerTLId))
  val outerDataBeats = outerTLParams.dataBeats
  val outerDataBits = outerTLParams.dataBitsPerBeat
  val outerBeatAddrBits = log2Up(outerDataBeats)
  val outerByteAddrBits = log2Up(outerDataBits/8)
  val outerWriteMaskBits = outerTLParams.writeMaskBits
  val innerTLId = p(InnerTLId)
  val innerTLParams = p(TLKey(innerTLId))
  val innerDataBeats = innerTLParams.dataBeats
  val innerDataBits = innerTLParams.dataBitsPerBeat
  val innerWriteMaskBits = innerTLParams.writeMaskBits
  val innerBeatAddrBits = log2Up(innerDataBeats)
  val innerByteAddrBits = log2Up(innerDataBits/8)
  val innerNCachingClients = innerTLParams.nCachingClients
  val maxManagerXacts = innerTLParams.maxManagerXacts
  require(outerDataBeats == innerDataBeats) //TODO: fix all xact_data Vecs to remove this requirement
}

abstract class CoherenceAgentModule(implicit val p: Parameters) extends Module
  with HasCoherenceAgentParameters
abstract class CoherenceAgentBundle(implicit val p: Parameters) extends ParameterizedBundle()(p)
   with HasCoherenceAgentParameters

trait HasCoherenceAgentWiringHelpers {
  def doOutputArbitration[T <: TileLinkChannel](
      out: DecoupledIO[T],
      ins: Seq[DecoupledIO[T]]) {
    def lock(o: T) = o.hasMultibeatData()
    val arb = Module(new LockingRRArbiter(out.bits, ins.size, out.bits.tlDataBeats, Some(lock _)))
    out <> arb.io.out
    arb.io.in <> ins
  }

  def doInputRouting[T <: Bundle with HasManagerTransactionId](
        in: DecoupledIO[T],
        outs: Seq[DecoupledIO[T]]) {
    val idx = in.bits.manager_xact_id
    outs.map(_.bits := in.bits)
    outs.zipWithIndex.map { case (o,i) => o.valid := in.valid && idx === UInt(i) }
    in.ready := outs.map(_.ready).apply(idx)
  }

  /** Broadcasts valid messages on this channel to all trackers,
    * but includes logic to allocate a new tracker in the case where
    * no previously allocated tracker matches the new req's addr.
    *
    * When a match is reported, if ready is high the new transaction
    * is merged; when ready is low the transaction is being blocked.
    * When no match is reported, any high idles are presumed to be
    * from trackers that are available for allocation, and one is
    * assigned via alloc based on priority; if no idles are high then
    * all trackers are busy with other transactions. If idle is high
    * but ready is low, the tracker will be allocated but does not
    * have sufficient buffering for the data.
    */ 
  def doInputRoutingWithAllocation[T <: TileLinkChannel with HasTileLinkData](
        in: DecoupledIO[T],
        outs: Seq[DecoupledIO[T]],
        allocs: Seq[TrackerAllocation],
        dataOverrides: Option[Seq[UInt]] = None,
        allocOverride: Option[Bool] = None,
        matchOverride: Option[Bool] = None) {
    val ready_bits = outs.map(_.ready).asUInt
    val can_alloc_bits = allocs.map(_.can).asUInt
    val should_alloc_bits = PriorityEncoderOH(can_alloc_bits)
    val match_bits = allocs.map(_.matches).asUInt
    val no_matches = !match_bits.orR
    val alloc_ok = allocOverride.getOrElse(Bool(true))
    val match_ok = matchOverride.getOrElse(Bool(true))
    in.ready := (Mux(no_matches, can_alloc_bits, match_bits) & ready_bits).orR && alloc_ok && match_ok
    outs.zip(allocs).zipWithIndex.foreach { case((out, alloc), i) =>
      out.valid := in.valid && match_ok && alloc_ok
      out.bits := in.bits
      dataOverrides foreach { d => out.bits.data := d(i) }
      alloc.should := should_alloc_bits(i) && no_matches && alloc_ok
    }
  }
}

trait HasInnerTLIO extends HasCoherenceAgentParameters {
  val inner = new ManagerTileLinkIO()(p.alterPartial({case TLId => p(InnerTLId)}))
  val incoherent = Vec(inner.tlNCachingClients, Bool()).asInput
  def iacq(dummy: Int = 0) = inner.acquire.bits
  def iprb(dummy: Int = 0) = inner.probe.bits
  def irel(dummy: Int = 0) = inner.release.bits
  def ignt(dummy: Int = 0) = inner.grant.bits
  def ifin(dummy: Int = 0) = inner.finish.bits
}

trait HasUncachedOuterTLIO extends HasCoherenceAgentParameters {
  val outer = new ClientUncachedTileLinkIO()(p.alterPartial({case TLId => p(OuterTLId)}))
  def oacq(dummy: Int = 0) = outer.acquire.bits
  def ognt(dummy: Int = 0) = outer.grant.bits
}

trait HasCachedOuterTLIO extends HasCoherenceAgentParameters {
  val outer = new ClientTileLinkIO()(p.alterPartial({case TLId => p(OuterTLId)}))
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
  def outerTL = TileLinkIOWrapper(io.outer)
  def incoherent = io.incoherent
}

class HierarchicalTLIO(implicit p: Parameters) extends CoherenceAgentBundle()(p)
  with HasInnerTLIO
  with HasCachedOuterTLIO

abstract class HierarchicalCoherenceAgent(implicit p: Parameters) extends CoherenceAgent()(p)
    with HasCoherenceAgentWiringHelpers {
  val io = new HierarchicalTLIO
  def innerTL = io.inner
  def outerTL = io.outer
  def incoherent = io.incoherent

  // TODO: Remove this function (and all its calls) when we support probing the L2
  def disconnectOuterProbeAndFinish() {
    io.outer.probe.ready := Bool(false)
    io.outer.finish.valid := Bool(false)
    assert(!io.outer.probe.valid, "L2 agent got illegal probe")
  }
}
