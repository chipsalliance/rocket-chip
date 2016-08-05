// See LICENSE for license details.

package uncore.agents
import Chisel._
import uncore.tilelink._
import cde.{Parameters, Field}

case object L2StoreDataQueueDepth extends Field[Int]

trait HasStoreDataQueueParameters extends HasCoherenceAgentParameters {
  val sdqDepth = p(L2StoreDataQueueDepth)*innerDataBeats
  val dqIdxBits = math.max(log2Up(nReleaseTransactors) + 1, log2Up(sdqDepth))
  val nDataQueueLocations = 3 //Stores, VoluntaryWBs, Releases
}

class DataQueueLocation(implicit p: Parameters) extends CoherenceAgentBundle()(p)
    with HasStoreDataQueueParameters {
  val idx = UInt(width = dqIdxBits)
  val loc = UInt(width = log2Ceil(nDataQueueLocations))
} 

object DataQueueLocation {
  def apply(idx: UInt, loc: UInt)(implicit p: Parameters) = {
    val d = Wire(new DataQueueLocation)
    d.idx := idx
    d.loc := loc
    d
  }
}

trait HasStoreDataQueue extends HasStoreDataQueueParameters {
  val io: HierarchicalTLIO
  val trackerIOsList: Seq[HierarchicalXactTrackerIO]

  val internalDataBits = new DataQueueLocation().getWidth
  val inStoreQueue :: inVolWBQueue :: inClientReleaseQueue :: Nil = Enum(UInt(), nDataQueueLocations)

  val usingStoreDataQueue = p.alterPartial({
    case TLKey(`innerTLId`) => innerTLParams.copy(overrideDataBitsPerBeat = Some(internalDataBits))
    case TLKey(`outerTLId`) => outerTLParams.copy(overrideDataBitsPerBeat = Some(internalDataBits))
  })

  // Queue to store impending Put data
  lazy val sdq = Reg(Vec(sdqDepth, io.iacq().data))
  lazy val sdq_val = Reg(init=Bits(0, sdqDepth))
  lazy val sdq_alloc_id = PriorityEncoder(~sdq_val)
  lazy val sdq_rdy = !sdq_val.andR
  lazy val sdq_enq = trackerIOsList.map( t =>
                  (t.alloc.iacq.should || t.alloc.iacq.matches) &&
                    t.inner.acquire.fire() &&
                    t.iacq().hasData()
                ).reduce(_||_)

  lazy val sdqLoc = List.fill(nTransactors) {
    DataQueueLocation(sdq_alloc_id, inStoreQueue).asUInt
  }

  /*
  doInputRoutingWithAllocation(
    in = io.inner.acquire,
    outs = trackerList.map(_.io.inner.acquire),
    allocs = trackerList.map(_.io.alloc._iacq),
    dataOverride = Some(sdqLoc),
    allocOverride = Some(sdq_rdy && !irel_vs_iacq_conflict))
  */

  // Queue to store impending Voluntary Release data
  lazy val voluntary = io.irel().isVoluntary()
  lazy val vwbdq_enq = io.inner.release.fire() && voluntary && io.irel().hasData()
  lazy val (rel_data_cnt, rel_data_done) = Counter(vwbdq_enq, innerDataBeats) //TODO Zero width
  lazy val vwbdq = Reg(Vec(innerDataBeats, io.irel().data)) //TODO Assumes nReleaseTransactors == 1
  

  lazy val vwbqLoc = (0 until nTransactors).map(i =>
    (DataQueueLocation(rel_data_cnt,
                       (if(i < nReleaseTransactors) inVolWBQueue
                        else inClientReleaseQueue)).asUInt))
  /*
  doInputRoutingWithAllocation(
    io.inner.release,
    trackerList.map(_.io.inner.release),
    trackerList.map(_.io.matches.irel),
    trackerList.map(_.io.alloc.irel),
    Some(vwbqLoc))
  */

  val outer_arb: ClientTileLinkIOArbiter
  lazy val outer_data_ptr = new DataQueueLocation().fromBits(outer_arb.io.out.acquire.bits.data)
  /*
  val outer_arb = Module(new ClientTileLinkIOArbiter(trackerList.size)
                    (usingStoreDataQueue.alterPartial({ case TLId => p(OuterTLId) })))
  outer_arb.io.in <> trackerList
  */
  // Get the pending data out of the store data queue
  lazy val is_in_sdq = outer_data_ptr.loc === inStoreQueue
  lazy val free_sdq = io.outer.acquire.fire() &&
                  io.outer.acquire.bits.hasData() &&
                  outer_data_ptr.loc === inStoreQueue
  /*
  io.outer <> outer_arb.io.out
  io.outer.acquire.bits.data := MuxLookup(outer_data_ptr.loc, io.irel().data, Array(
                                          inStoreQueue -> sdq(outer_data_ptr.idx),
                                          inVolWBQueue -> vwbdq(outer_data_ptr.idx)))
  */

  // Enqueue SDQ data
  def sdqEnqueue() {
    when (sdq_enq) { sdq(sdq_alloc_id) := io.iacq().data }
    when(vwbdq_enq) { vwbdq(rel_data_cnt) := io.irel().data }
  }

  // Update SDQ valid bits
  def sdqUpdate() {
    when (io.outer.acquire.valid || sdq_enq) {
      sdq_val := sdq_val & ~(UIntToOH(outer_data_ptr.idx) & Fill(sdqDepth, free_sdq)) | 
                 PriorityEncoderOH(~sdq_val(sdqDepth-1,0)) & Fill(sdqDepth, sdq_enq)
    }
  }
}
