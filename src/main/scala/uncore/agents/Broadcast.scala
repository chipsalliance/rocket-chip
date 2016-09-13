// See LICENSE for license details.

package uncore.agents

import Chisel._
import uncore.coherence._
import uncore.tilelink._
import uncore.constants._
import uncore.util._
import cde.Parameters

class L2BroadcastHub(_clock: Clock = null, _reset: Bool = null)
    (implicit p: Parameters) extends HierarchicalCoherenceAgent(_clock, _reset)(p) {

  // Create TSHRs for outstanding transactions
  val irelTrackerList =
    (0 until nReleaseTransactors).map(id =>
      Module(new BufferedBroadcastVoluntaryReleaseTracker(id)))
  val iacqTrackerList = 
    (nReleaseTransactors until nTransactors).map(id =>
      Module(new BufferedBroadcastAcquireTracker(id)))
  val trackerList = irelTrackerList ++ iacqTrackerList

  // Propagate incoherence flags
  trackerList.map(_.io.incoherent) foreach { _ := io.incoherent }

  // Create an arbiter for the one memory port
  val outerList = trackerList.map(_.io.outer)
  val outer_arb = Module(new ClientTileLinkIOArbiter(outerList.size)
                                                    (p.alterPartial({ case TLId => p(OuterTLId) })))
  outer_arb.io.in <> outerList
  io.outer <> outer_arb.io.out

  // Handle acquire transaction initiation
  val irel_vs_iacq_conflict =
    io.inner.acquire.valid &&
    io.inner.release.valid &&
    io.irel().conflicts(io.iacq())

  doInputRoutingWithAllocation(
    in = io.inner.acquire,
    outs = trackerList.map(_.io.inner.acquire),
    allocs = trackerList.map(_.io.alloc.iacq),
    allocOverride = Some(!irel_vs_iacq_conflict))

  // Handle releases, which might be voluntary and might have data
  doInputRoutingWithAllocation(
    in = io.inner.release,
    outs = trackerList.map(_.io.inner.release),
    allocs = trackerList.map(_.io.alloc.irel))

  // Wire probe requests and grant reply to clients, finish acks from clients
  doOutputArbitration(io.inner.probe, trackerList.map(_.io.inner.probe))

  doOutputArbitration(io.inner.grant, trackerList.map(_.io.inner.grant))

  doInputRouting(io.inner.finish, trackerList.map(_.io.inner.finish))

  disconnectOuterProbeAndFinish()
}

class BroadcastXactTracker(implicit p: Parameters) extends XactTracker()(p) {
  val io = new HierarchicalXactTrackerIO
  pinAllReadyValidLow(io)
}

trait BroadcastsToAllClients extends HasCoherenceAgentParameters {
  val coh = HierarchicalMetadata.onReset
  val inner_coh = coh.inner
  val outer_coh = coh.outer
  def full_representation = ~UInt(0, width = innerNCachingClients)
}

abstract class BroadcastVoluntaryReleaseTracker(trackerId: Int)(implicit p: Parameters)
    extends VoluntaryReleaseTracker(trackerId)(p) 
    with EmitsVoluntaryReleases
    with BroadcastsToAllClients {
  val io = new HierarchicalXactTrackerIO
  pinAllReadyValidLow(io)

  // Checks for illegal behavior
  assert(!(state === s_idle && io.inner.release.fire() && io.alloc.irel.should && !io.irel().isVoluntary()),
    "VoluntaryReleaseTracker accepted Release that wasn't voluntary!")
}

abstract class BroadcastAcquireTracker(trackerId: Int)(implicit p: Parameters)
    extends AcquireTracker(trackerId)(p) 
    with EmitsVoluntaryReleases
    with BroadcastsToAllClients {
  val io = new HierarchicalXactTrackerIO
  pinAllReadyValidLow(io)

  val alwaysWriteFullBeat = false
  val nSecondaryMisses = 1
  def iacq_can_merge = Bool(false)

  // Checks for illegal behavior
  // TODO: this could be allowed, but is a useful check against allocation gone wild
  assert(!(state === s_idle && io.inner.acquire.fire() && io.alloc.iacq.should &&
    io.iacq().hasMultibeatData() && !io.iacq().first()),
    "AcquireTracker initialized with a tail data beat.")

  assert(!(state =/= s_idle && pending_ignt && xact_iacq.isPrefetch()),
    "Broadcast Hub does not support Prefetches.")

  assert(!(state =/= s_idle && pending_ignt && xact_iacq.isAtomic()),
    "Broadcast Hub does not support PutAtomics.")
}

class BufferedBroadcastVoluntaryReleaseTracker(trackerId: Int)(implicit p: Parameters)
    extends BroadcastVoluntaryReleaseTracker(trackerId)(p)
    with HasDataBuffer {

  // Tell the parent if any incoming messages conflict with the ongoing transaction
  routeInParent(irelCanAlloc = Bool(true))

  // Start transaction by accepting inner release
  innerRelease(block_vol_ignt = pending_orel || vol_ognt_counter.pending)

  // A release beat can be accepted if we are idle, if its a mergeable transaction, or if its a tail beat
  io.inner.release.ready := state === s_idle || irel_can_merge || irel_same_xact

  when(io.inner.release.fire()) { data_buffer(io.irel().addr_beat) := io.irel().data }

  // Dispatch outer release
  outerRelease(
    coh = outer_coh.onHit(M_XWR),
    data = data_buffer(vol_ognt_counter.up.idx),
    add_pending_send_bit = irel_is_allocating)

  quiesce() {}
}

class BufferedBroadcastAcquireTracker(trackerId: Int)(implicit p: Parameters)
    extends BroadcastAcquireTracker(trackerId)(p)
    with HasByteWriteMaskBuffer {

  // Setup IOs used for routing in the parent
  routeInParent(iacqCanAlloc = Bool(true))

  // First, take care of accpeting new acquires or secondary misses
  // Handling of primary and secondary misses' data and write mask merging
  innerAcquire(
    can_alloc = Bool(false),
    next = s_inner_probe)

  io.inner.acquire.ready := state === s_idle || iacq_can_merge || iacq_same_xact_multibeat

  // Track which clients yet need to be probed and make Probe message
  // If a writeback occurs, we can forward its data via the buffer,
  // and skip having to go outwards
  val skip_outer_acquire = pending_ignt_data.andR

  innerProbe(
    inner_coh.makeProbe(curr_probe_dst, xact_iacq, xact_addr_block),
    Mux(!skip_outer_acquire, s_outer_acquire, s_busy))

  // Handle incoming releases from clients, which may reduce sharer counts
  // and/or write back dirty data, and may be unexpected voluntary releases
  def irel_can_merge = io.irel().conflicts(xact_addr_block) &&
                         io.irel().isVoluntary() &&
                         !state.isOneOf(s_idle, s_meta_write) &&
                         !all_pending_done &&
                         !io.outer.grant.fire() &&
                         !io.inner.grant.fire() &&
                         !vol_ignt_counter.pending &&
                         !blockInnerRelease()

  innerRelease(block_vol_ignt = vol_ognt_counter.pending) 

  //TODO: accept vol irels when state === s_idle, operate like the VolRelTracker
  io.inner.release.ready := irel_can_merge || irel_same_xact

  mergeDataInner(io.inner.release)

  // If there was a writeback, forward it outwards
  outerRelease(
    coh = outer_coh.onHit(M_XWR),
    data = data_buffer(vol_ognt_counter.up.idx))

  // Send outer request for miss
  outerAcquire(
    caching = !xact_iacq.isBuiltInType(),
    coh = outer_coh,
    data = data_buffer(ognt_counter.up.idx),
    wmask = wmask_buffer(ognt_counter.up.idx),
    next = s_busy)
    
  // Handle the response from outer memory
  mergeDataOuter(io.outer.grant)

  // Acknowledge or respond with data
  innerGrant(
    data = data_buffer(ignt_data_idx),
    external_pending = pending_orel || ognt_counter.pending || vol_ognt_counter.pending)

  when(iacq_is_allocating) {
    initializeProbes()
  }

  initDataInner(io.inner.acquire, iacq_is_allocating || iacq_is_merging)

  // Wait for everything to quiesce
  quiesce() { clearWmaskBuffer() }
}
