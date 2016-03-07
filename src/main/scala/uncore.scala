// See LICENSE for license details.

package uncore
import Chisel._
import cde.{Parameters, Field}

case object NReleaseTransactors extends Field[Int]
case object NProbeTransactors extends Field[Int]
case object NAcquireTransactors extends Field[Int]

/** Identifies the TLId of the inner network in a hierarchical cache controller */ 
case object InnerTLId extends Field[String]
/** Identifies the TLId of the outer network in a hierarchical cache controller */ 
case object OuterTLId extends Field[String]

trait HasCoherenceAgentParameters {
  implicit val p: Parameters
  val nReleaseTransactors = 1
  val nAcquireTransactors = p(NAcquireTransactors)
  val nTransactors = nReleaseTransactors + nAcquireTransactors
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
  val maxManagerXacts = innerTLParams.maxManagerXacts
  require(outerDataBeats == innerDataBeats) //TODO: fix all xact_data Vecs to remove this requirement
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

  /** Broadcasts valid messages on this channel to all trackers,
    * but includes logic to allocate a new tracker in the case where
    * no previously allocated tracker matches the new req's addr.
    *
    * When a match is reported, if ready is high the new transaction
    * is merged; when ready is low the transaction is being blocked.
    * When no match is reported, any high readys are presumed to be
    * from trackers that are available for allocation, and one is
    * assigned via alloc based on priority; f no readys are high then
    * all trackers are busy with other transactions.
    */ 
  def doInputRoutingWithAllocation[T <: TileLinkChannel with HasTileLinkData](
        in: DecoupledIO[T],
        outs: Seq[DecoupledIO[T]],
        matches: Seq[Bool],
        allocs: Seq[Bool],
        dataOverrides: Option[Seq[UInt]] = None,
        allocOverride: Option[Bool] = None) {
    val ready_bits = Vec(outs.map(_.ready)).toBits
    val alloc_bits = PriorityEncoderOH(ready_bits)
    val match_bits = Vec(matches).toBits
    val no_matches = !match_bits.orR
    val do_alloc = allocOverride.getOrElse(Bool(true))
    in.ready := Mux(no_matches, ready_bits.orR, (match_bits & ready_bits).orR) && do_alloc
    outs.zip(allocs).zipWithIndex.foreach { case((out, a), i) =>
      out.valid := in.valid
      out.bits := in.bits
      dataOverrides foreach { d => out.bits.data := d(i) }
      a := alloc_bits(i) & no_matches & do_alloc
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
  lazy val outerTL = TileLinkIOWrapper(io.outer)(p.alterPartial({case TLId => p(OuterTLId)}))
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

trait HasTrackerAllocationIO extends Bundle {
  val matches = new Bundle {
    val iacq = Bool(OUTPUT)
    val irel = Bool(OUTPUT)
    val oprb = Bool(OUTPUT)
  }
  val alloc = new Bundle {
    val iacq = Bool(INPUT)
    val irel = Bool(INPUT)
    val oprb = Bool(INPUT)
  }
}

class ManagerXactTrackerIO(implicit p: Parameters) extends ManagerTLIO()(p)
  with HasTrackerAllocationIO

class HierarchicalXactTrackerIO(implicit p: Parameters) extends HierarchicalTLIO()(p)
  with HasTrackerAllocationIO

abstract class XactTracker(implicit p: Parameters) extends CoherenceAgentModule()(p)
    with HasDataBeatCounters {
  def addPendingBitWhenBeat[T <: HasBeat](inc: Bool, in: T): UInt =
    Fill(in.tlDataBeats, inc) &  UIntToOH(in.addr_beat)

  def dropPendingBitWhenBeat[T <: HasBeat](dec: Bool, in: T): UInt =
    ~Fill(in.tlDataBeats, dec) | ~UIntToOH(in.addr_beat)

  def addPendingBitWhenId[T <: HasClientId](inc: Bool, in: T): UInt =
    Fill(in.tlNCachingClients, inc) &  UIntToOH(in.client_id)

  def dropPendingBitWhenId[T <: HasClientId](dec: Bool, in: T): UInt =
    ~Fill(in.tlNCachingClients, dec) | ~UIntToOH(in.client_id)

  def addPendingBitWhenBeatHasData[T <: HasBeat](in: DecoupledIO[T], inc: Bool = Bool(true)): UInt =
    addPendingBitWhenBeat(in.fire() && in.bits.hasData() && inc, in.bits)

  def addPendingBitWhenBeatHasDataAndAllocs(in: DecoupledIO[AcquireFromSrc]): UInt =
    addPendingBitWhenBeatHasData(in, in.bits.allocate())

  def addPendingBitWhenBeatIsGetOrAtomic(in: DecoupledIO[AcquireFromSrc]): UInt = {
    val a = in.bits
    val isGetOrAtomic = a.isBuiltInType() &&
      (Vec(Acquire.getType, Acquire.getBlockType, Acquire.putAtomicType).contains(a.a_type))
    addPendingBitWhenBeat(in.fire() && isGetOrAtomic, a)
  }

  def addPendingBitsFromAcquire(a: SecondaryMissInfo): UInt =
    Mux(a.hasMultibeatData(), Fill(a.tlDataBeats, UInt(1, 1)), UIntToOH(a.addr_beat))

  def dropPendingBitWhenBeatHasData[T <: HasBeat](in: DecoupledIO[T]): UInt =
    dropPendingBitWhenBeat(in.fire() && in.bits.hasData(), in.bits)

  def dropPendingBitAtDest[T <: HasId](in: DecoupledIO[T]): UInt =
    dropPendingBitWhenId(in.fire(), in.bits)

  def dropPendingBitAtDestWhenVoluntary[T <: HasId with MightBeVoluntary](in: DecoupledIO[T]): UInt =
    dropPendingBitWhenId(in.fire() && in.bits.isVoluntary(), in.bits)

  def addPendingBitAtSrc[T <: HasId](in: DecoupledIO[T]): UInt =
    addPendingBitWhenId(in.fire(), in.bits)

  def addPendingBitAtSrcWhenVoluntary[T <: HasId with MightBeVoluntary](in: DecoupledIO[T]): UInt =
    addPendingBitWhenId(in.fire() && in.bits.isVoluntary(), in.bits)

  def pinAllReadyValidLow[T <: Data](b: Bundle) {
    b.elements.foreach {
      _._2 match {
        case d: DecoupledIO[_] =>
          if(d.ready.dir == OUTPUT) d.ready := Bool(false)
          else if(d.valid.dir == OUTPUT) d.valid := Bool(false)
        case v: ValidIO[_] => if(v.valid.dir == OUTPUT) v.valid := Bool(false) 
        case b: Bundle => pinAllReadyValidLow(b)
        case _ =>
      }
    }
  }
}
