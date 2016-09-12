// See LICENSE for license details.

package uncore.agents

import Chisel._
import uncore.coherence._
import uncore.tilelink._
import uncore.constants._
import cde.Parameters


class BufferlessBroadcastHub(clockSignal: Clock = null, resetSignal: Bool = null)
    (implicit p: Parameters) extends HierarchicalCoherenceAgent(clockSignal, resetSignal)(p) {

  // Create TSHRs for outstanding transactions
  val irelTrackerList =
    (0 until nReleaseTransactors).map(id =>
      Module(new BufferlessBroadcastVoluntaryReleaseTracker(id)))
  val iacqTrackerList = 
    (nReleaseTransactors until nTransactors).map(id =>
      Module(new BufferlessBroadcastAcquireTracker(id)))
  val trackerList = irelTrackerList ++ iacqTrackerList

  // Propagate incoherence flags
  trackerList.map(_.io.incoherent) foreach { _ := io.incoherent }

  // Create an arbiter for the one memory port
  val outerList = trackerList.map(_.io.outer)
  val outer_arb = Module(new ClientTileLinkIOArbiter(outerList.size)
                                                    (p.alterPartial({ case TLId => p(OuterTLId) })))
  outer_arb.io.in <> outerList
  io.outer <> outer_arb.io.out

  val iacq = Queue(io.inner.acquire, 1, pipe=true)
  val irel = Queue(io.inner.release, 1, pipe=true)

  // Handle acquire transaction initiation
  val irel_vs_iacq_conflict =
    iacq.valid &&
    irel.valid &&
    irel.bits.conflicts(iacq.bits)

  doInputRoutingWithAllocation(
    in = iacq,
    outs = trackerList.map(_.io.inner.acquire),
    allocs = trackerList.map(_.io.alloc.iacq),
    allocOverride = Some(!irel_vs_iacq_conflict))
  io.outer.acquire.bits.data := iacq.bits.data
  when (io.oacq().hasData()) {
    io.outer.acquire.bits.addr_beat := iacq.bits.addr_beat
  }

  // Handle releases, which might be voluntary and might have data
  doInputRoutingWithAllocation(
    in = irel,
    outs = trackerList.map(_.io.inner.release),
    allocs = trackerList.map(_.io.alloc.irel))
  io.outer.release.bits.data := irel.bits.data
  when (io.orel().hasData()) {
    io.outer.release.bits.addr_beat := irel.bits.addr_beat
  }

  // Wire probe requests and grant reply to clients, finish acks from clients
  doOutputArbitration(io.inner.probe, trackerList.map(_.io.inner.probe))

  doOutputArbitration(io.inner.grant, trackerList.map(_.io.inner.grant))
  io.inner.grant.bits.data := io.outer.grant.bits.data
  io.inner.grant.bits.addr_beat := io.outer.grant.bits.addr_beat

  doInputRouting(io.inner.finish, trackerList.map(_.io.inner.finish))

  disconnectOuterProbeAndFinish()
}

class BufferlessBroadcastVoluntaryReleaseTracker(trackerId: Int)(implicit p: Parameters)
    extends BroadcastVoluntaryReleaseTracker(trackerId)(p) {

  // Tell the parent if any incoming messages conflict with the ongoing transaction
  routeInParent(irelCanAlloc = Bool(true))

  // Start transaction by accepting inner release
  innerRelease(block_vol_ignt = pending_orel || vol_ognt_counter.pending)

  // A release beat can be accepted if we are idle, if its a mergeable transaction, or if its a tail beat
  // and if the outer relase path is clear 
  io.inner.release.ready := Mux(io.irel().hasData(),
    (state =/= s_idle) && (irel_can_merge || irel_same_xact) && io.outer.release.ready,
    (state === s_idle) || irel_can_merge || irel_same_xact)

  // Dispatch outer release
  outerRelease(coh = outer_coh.onHit(M_XWR), buffering = Bool(false))

  quiesce() {}
}

class BufferlessBroadcastAcquireTracker(trackerId: Int)(implicit p: Parameters)
    extends BroadcastAcquireTracker(trackerId)(p) {

  // Setup IOs used for routing in the parent
  routeInParent(iacqCanAlloc = Bool(true))

  // First, take care of accpeting new acquires or secondary misses
  // Handling of primary and secondary misses' data and write mask merging
  innerAcquire(
    can_alloc = Bool(false),
    next = s_inner_probe)

  // We are never going to merge anything in the bufferless hub
  // Therefore, we only need to concern ourselves with the allocated
  // transaction and (in case of PutBlock) subsequent tail beats
  val iacq_can_forward = iacq_same_xact && !vol_ognt_counter.pending
  io.inner.acquire.ready := Mux(io.iacq().hasData(),
    state === s_outer_acquire && iacq_can_forward && io.outer.acquire.ready,
    state === s_idle && io.alloc.iacq.should)

  // Track which clients yet need to be probed and make Probe message
  innerProbe(
    inner_coh.makeProbe(curr_probe_dst, xact_iacq, xact_addr_block),
    s_outer_acquire)

  // Handle incoming releases from clients, which may reduce sharer counts
  // and/or write back dirty data, and may be unexpected voluntary releases
  def irel_can_merge = io.irel().conflicts(xact_addr_block) &&
                         io.irel().isVoluntary() &&
                         !vol_ignt_counter.pending &&
                         !(io.irel().hasData() && ognt_counter.pending) &&
                         (state =/= s_idle)

  innerRelease(block_vol_ignt = vol_ognt_counter.pending) 

  val irel_could_accept = irel_can_merge || irel_same_xact
  io.inner.release.ready := irel_could_accept &&
    (!io.irel().hasData() || io.outer.release.ready)

  // If there was a writeback, forward it outwards
  outerRelease(
    coh = outer_coh.onHit(M_XWR),
    buffering = Bool(false),
    block_orel = !irel_could_accept)

  // Send outer request for miss
  outerAcquire(
    caching = !xact_iacq.isBuiltInType(),
    block_outer_acquire = vol_ognt_counter.pending,
    buffering = Bool(false),
    coh = outer_coh,
    next = s_busy)

  // Handle the response from outer memory
  when (ognt_counter.pending && io.ognt().hasData()) {
    io.outer.grant.ready := io.inner.grant.ready // bypass data
  }

  // Acknowledge or respond with data
  innerGrant(
    external_pending = pending_orel || vol_ognt_counter.pending,
    buffering = Bool(false))

  when(iacq_is_allocating) { initializeProbes() }

  // Wait for everything to quiesce
  quiesce() {}
}
