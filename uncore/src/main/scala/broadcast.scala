// See LICENSE for license details.

package uncore
import Chisel._
import cde.{Parameters, Field}

case object L2StoreDataQueueDepth extends Field[Int]

trait HasBroadcastHubParameters extends HasCoherenceAgentParameters {
  val sdqDepth = p(L2StoreDataQueueDepth)*innerDataBeats
  val dqIdxBits = math.max(log2Up(nReleaseTransactors) + 1, log2Up(sdqDepth))
  val nDataQueueLocations = 3 //Stores, VoluntaryWBs, Releases
}

class DataQueueLocation(implicit p: Parameters) extends CoherenceAgentBundle()(p)
    with HasBroadcastHubParameters {
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

class L2BroadcastHub(implicit p: Parameters) extends ManagerCoherenceAgent()(p)
    with HasBroadcastHubParameters {
  val internalDataBits = new DataQueueLocation().getWidth
  val inStoreQueue :: inVolWBQueue :: inClientReleaseQueue :: Nil = Enum(UInt(), nDataQueueLocations)

  val usingStoreDataQueue = p.alterPartial({
    case TLKey(`innerTLId`) => innerTLParams.copy(overrideDataBitsPerBeat = Some(internalDataBits))
    case TLKey(`outerTLId`) => outerTLParams.copy(overrideDataBitsPerBeat = Some(internalDataBits))
  })

  // Create SHRs for outstanding transactions
  val trackerList =
    (0 until nReleaseTransactors).map(id =>
      Module(new BroadcastVoluntaryReleaseTracker(id)(usingStoreDataQueue))) ++
    (nReleaseTransactors until nTransactors).map(id =>
      Module(new BroadcastAcquireTracker(id)(usingStoreDataQueue)))

  // Propagate incoherence flags
  trackerList.map(_.io.incoherent := io.incoherent)

  // Queue to store impending Put data
  val sdq = Reg(Vec(sdqDepth, io.iacq().data))
  val sdq_val = Reg(init=Bits(0, sdqDepth))
  val sdq_alloc_id = PriorityEncoder(~sdq_val)
  val sdq_rdy = !sdq_val.andR
  val sdq_enq = io.inner.acquire.fire() && io.iacq().hasData()
  when (sdq_enq) { sdq(sdq_alloc_id) := io.iacq().data }

  // Handle acquire transaction initiation
  val trackerAcquireIOs = trackerList.map(_.io.inner.acquire)
  val acquireConflicts = Vec(trackerList.map(_.io.has_acquire_conflict)).toBits
  val acquireMatches = Vec(trackerList.map(_.io.has_acquire_match)).toBits
  val acquireReadys = Vec(trackerAcquireIOs.map(_.ready)).toBits
  val acquire_idx = Mux(acquireMatches.orR,
                      PriorityEncoder(acquireMatches),
                      PriorityEncoder(acquireReadys))

  val block_acquires = acquireConflicts.orR || !sdq_rdy
  io.inner.acquire.ready := acquireReadys.orR && !block_acquires
  trackerAcquireIOs.zipWithIndex.foreach {
    case(tracker, i) =>
      tracker.bits := io.inner.acquire.bits
      tracker.bits.data := DataQueueLocation(sdq_alloc_id, inStoreQueue).toBits
      tracker.valid := io.inner.acquire.valid && !block_acquires && (acquire_idx === UInt(i))
  }

  // Queue to store impending Voluntary Release data
  val voluntary = io.irel().isVoluntary()
  val vwbdq_enq = io.inner.release.fire() && voluntary && io.irel().hasData()
  val (rel_data_cnt, rel_data_done) = Counter(vwbdq_enq, innerDataBeats) //TODO Zero width
  val vwbdq = Reg(Vec(innerDataBeats, io.irel().data)) //TODO Assumes nReleaseTransactors == 1
  when(vwbdq_enq) { vwbdq(rel_data_cnt) := io.irel().data }

  // Handle releases, which might be voluntary and might have data
  val trackerReleaseIOs = trackerList.map(_.io.inner.release)
  val releaseReadys = Vec(trackerReleaseIOs.map(_.ready)).toBits
  val releaseMatches = Vec(trackerList.map(_.io.has_release_match)).toBits
  val release_idx = PriorityEncoder(releaseMatches)
  io.inner.release.ready := releaseReadys(release_idx)
  trackerReleaseIOs.zipWithIndex.foreach {
    case(tracker, i) =>
      tracker.valid := io.inner.release.valid && (release_idx === UInt(i))
      tracker.bits := io.inner.release.bits
      tracker.bits.data := DataQueueLocation(rel_data_cnt,
                                     (if(i < nReleaseTransactors) inVolWBQueue
                                      else inClientReleaseQueue)).toBits
  }
  assert(!(io.inner.release.valid && !releaseMatches.orR),
    "Non-voluntary release should always have a Tracker waiting for it.")

  // Wire probe requests and grant reply to clients, finish acks from clients
  // Note that we bypass the Grant data subbundles
  doOutputArbitration(io.inner.grant, trackerList.map(_.io.inner.grant))
  io.inner.grant.bits.data := io.outer.grant.bits.data
  io.inner.grant.bits.addr_beat := io.outer.grant.bits.addr_beat
  doOutputArbitration(io.inner.probe, trackerList.map(_.io.inner.probe))
  doInputRouting(io.inner.finish, trackerList.map(_.io.inner.finish))

  // Create an arbiter for the one memory port
  val outer_arb = Module(new ClientUncachedTileLinkIOArbiter(trackerList.size)
                    (usingStoreDataQueue.alterPartial({ case TLId => p(OuterTLId) })))
  outer_arb.io.in <> trackerList.map(_.io.outer)
  // Get the pending data out of the store data queue
  val outer_data_ptr = new DataQueueLocation().fromBits(outer_arb.io.out.acquire.bits.data)
  val is_in_sdq = outer_data_ptr.loc === inStoreQueue
  val free_sdq = io.outer.acquire.fire() &&
                  io.outer.acquire.bits.hasData() &&
                  outer_data_ptr.loc === inStoreQueue
  io.outer <> outer_arb.io.out
  io.outer.acquire.bits.data := MuxLookup(outer_data_ptr.loc, io.irel().data, Array(
                                          inStoreQueue -> sdq(outer_data_ptr.idx),
                                          inVolWBQueue -> vwbdq(outer_data_ptr.idx)))

  // Update SDQ valid bits
  when (io.outer.acquire.valid || sdq_enq) {
    sdq_val := sdq_val & ~(UIntToOH(outer_data_ptr.idx) & Fill(sdqDepth, free_sdq)) | 
               PriorityEncoderOH(~sdq_val(sdqDepth-1,0)) & Fill(sdqDepth, sdq_enq)
  }
}

class BroadcastXactTracker(implicit p: Parameters) extends XactTracker()(p) {
  val io = new ManagerXactTrackerIO
}

class BroadcastVoluntaryReleaseTracker(trackerId: Int)
                                      (implicit p: Parameters) extends BroadcastXactTracker()(p) {
  val s_idle :: s_outer :: s_grant :: s_ack :: Nil = Enum(UInt(), 4)
  val state = Reg(init=s_idle)

  val xact = Reg(new BufferedReleaseFromSrc()(p.alterPartial({ case TLId => innerTLId })))
  val coh = ManagerMetadata.onReset

  val collect_irel_data = Reg(init=Bool(false))
  val irel_data_valid = Reg(init=Bits(0, width = innerDataBeats))
  val irel_data_done = connectIncomingDataBeatCounter(io.inner.release)
  val (oacq_data_cnt, oacq_data_done) = connectOutgoingDataBeatCounter(io.outer.acquire)

  io.has_acquire_conflict := Bool(false)
  io.has_release_match := io.irel().isVoluntary()
  io.has_acquire_match := Bool(false)

  io.outer.acquire.valid := Bool(false)
  io.outer.grant.ready := Bool(false)
  io.inner.acquire.ready := Bool(false)
  io.inner.probe.valid := Bool(false)
  io.inner.release.ready := Bool(false)
  io.inner.grant.valid := Bool(false)
  io.inner.finish.ready := Bool(false)

  io.inner.grant.bits := coh.makeGrant(xact, UInt(trackerId))

  //TODO: Use io.outer.release instead?
  io.outer.acquire.bits := PutBlock( 
                               client_xact_id = UInt(trackerId),
                               addr_block = xact.addr_block,
                               addr_beat = oacq_data_cnt,
                               data = xact.data_buffer(oacq_data_cnt))
                             (p.alterPartial({ case TLId => outerTLId }))

  when(collect_irel_data) {
    io.inner.release.ready := Bool(true)
    when(io.inner.release.valid) {
      xact.data_buffer(io.irel().addr_beat) := io.irel().data
      irel_data_valid := irel_data_valid.bitSet(io.irel().addr_beat, Bool(true))
    }
    when(irel_data_done) { collect_irel_data := Bool(false) }
  }

  switch (state) {
    is(s_idle) {
      io.inner.release.ready := Bool(true)
      when( io.inner.release.valid ) {
        xact := io.irel()
        xact.data_buffer(UInt(0)) := io.irel().data
        collect_irel_data := io.irel().hasMultibeatData()
        irel_data_valid := io.irel().hasData() << io.irel().addr_beat
        state := Mux(io.irel().hasData(), s_outer,
                   Mux(io.irel().requiresAck(), s_ack, s_idle))
      }
    }
    is(s_outer) {
      io.outer.acquire.valid := !collect_irel_data || irel_data_valid(oacq_data_cnt)
      when(oacq_data_done) { 
        state := s_grant // converted irel to oacq, so expect grant TODO: Mux(xact.requiresAck(), s_grant, s_idle) ?
      }
    }
    is(s_grant) { // Forward the Grant.voluntaryAck
      io.outer.grant.ready := io.inner.grant.ready
      io.inner.grant.valid := io.outer.grant.valid 
      when(io.inner.grant.fire()) {
        state := Mux(io.ignt().requiresAck(), s_ack, s_idle)
      }
    }
    is(s_ack) {
      // TODO: This state is unnecessary if no client will ever issue the
      // pending Acquire that caused this writeback until it receives the 
      // Grant.voluntaryAck for this writeback
      io.inner.finish.ready := Bool(true)
      when(io.inner.finish.valid) { state := s_idle }
    }
  }
}

class BroadcastAcquireTracker(trackerId: Int)
                             (implicit p: Parameters) extends BroadcastXactTracker()(p) {
  val s_idle :: s_probe :: s_mem_read :: s_mem_write :: s_make_grant :: s_mem_resp :: s_ack :: Nil = Enum(UInt(), 7)
  val state = Reg(init=s_idle)

  val xact = Reg(new BufferedAcquireFromSrc()(p.alterPartial({ case TLId => innerTLId })))
  val coh = ManagerMetadata.onReset

  assert(!(state =/= s_idle && xact.isBuiltInType() && 
      Vec(Acquire.putAtomicType, Acquire.getPrefetchType, Acquire.putPrefetchType).contains(xact.a_type)),
    "Broadcast Hub does not support PutAtomics or prefetches") // TODO

  val release_count = Reg(init=UInt(0, width = log2Up(io.inner.tlNCachingClients+1)))
  val pending_probes = Reg(init=Bits(0, width = io.inner.tlNCachingClients))
  val curr_p_id = PriorityEncoder(pending_probes)
  val mask_self = coh.full().bitSet(io.inner.acquire.bits.client_id, io.inner.acquire.bits.requiresSelfProbe())
  val mask_incoherent = mask_self & ~io.incoherent.toBits

  val collect_iacq_data = Reg(init=Bool(false))
  val iacq_data_valid = Reg(init=Bits(0, width = innerDataBeats))
  val iacq_data_done = connectIncomingDataBeatCounter(io.inner.acquire)
  val irel_data_done = connectIncomingDataBeatCounter(io.inner.release)
  val (ignt_data_cnt, ignt_data_done) = connectOutgoingDataBeatCounter(io.inner.grant)
  val (oacq_data_cnt, oacq_data_done) = connectOutgoingDataBeatCounter(io.outer.acquire)
  val ognt_data_done = connectIncomingDataBeatCounter(io.outer.grant)
  val pending_ognt_ack = Reg(init=Bool(false))
  val pending_outer_write = xact.hasData()
  val pending_outer_write_ = io.iacq().hasData()
  val pending_outer_read = io.ignt().hasData()
  val pending_outer_read_ = coh.makeGrant(io.iacq(), UInt(trackerId)).hasData()
  val subblock_type = xact.isSubBlockType()

  io.has_acquire_conflict := xact.conflicts(io.iacq()) && 
                              (state =/= s_idle) &&
                              !collect_iacq_data
  io.has_acquire_match := xact.conflicts(io.iacq()) &&
                              collect_iacq_data
  io.has_release_match := xact.conflicts(io.irel()) &&
                            !io.irel().isVoluntary() &&
                            (state === s_probe)

  val outerParams = p.alterPartial({ case TLId => outerTLId })

  val oacq_probe = PutBlock(
    client_xact_id = UInt(trackerId),
    addr_block = io.irel().addr_block,
    addr_beat = io.irel().addr_beat,
    data = io.irel().data)(outerParams)

  val oacq_write_beat = Put(
    client_xact_id = UInt(trackerId),
    addr_block = xact.addr_block,
    addr_beat = xact.addr_beat,
    data = xact.data_buffer(0),
    wmask = xact.wmask())(outerParams)

  val oacq_write_block = PutBlock(
    client_xact_id = UInt(trackerId),
    addr_block = xact.addr_block,
    addr_beat = oacq_data_cnt,
    data = xact.data_buffer(oacq_data_cnt),
    wmask = xact.wmask_buffer(oacq_data_cnt))(outerParams)

  val oacq_read_beat = Get(
    client_xact_id = UInt(trackerId),
    addr_block = xact.addr_block,
    addr_beat = xact.addr_beat,
    addr_byte = xact.addr_byte(),
    operand_size = xact.op_size(),
    alloc = Bool(false))(outerParams)

  val oacq_read_block = GetBlock(
    client_xact_id = UInt(trackerId),
    addr_block = xact.addr_block)(outerParams)

  io.outer.acquire.valid := Bool(false)
  io.outer.acquire.bits := Mux(state === s_probe, oacq_probe,
    Mux(state === s_mem_write,
      Mux(subblock_type, oacq_write_beat, oacq_write_block),
      Mux(subblock_type, oacq_read_beat, oacq_read_block)))
  io.outer.grant.ready := Bool(false)

  io.inner.probe.valid := Bool(false)
  io.inner.probe.bits := coh.makeProbe(curr_p_id, xact)

  io.inner.grant.valid := Bool(false)
  io.inner.grant.bits := coh.makeGrant(xact, UInt(trackerId)) // Data bypassed in parent

  io.inner.acquire.ready := Bool(false)
  io.inner.release.ready := Bool(false)
  io.inner.finish.ready := Bool(false)

  assert(!(state =/= s_idle && collect_iacq_data && io.inner.acquire.fire() &&
    io.iacq().client_id =/= xact.client_id),
    "AcquireTracker accepted data beat from different network source than initial request.")

  assert(!(state =/= s_idle && collect_iacq_data && io.inner.acquire.fire() &&
    io.iacq().client_xact_id =/= xact.client_xact_id),
    "AcquireTracker accepted data beat from different client transaction than initial request.")

  assert(!(state === s_idle && io.inner.acquire.fire() &&
    io.iacq().hasMultibeatData() && io.iacq().addr_beat =/= UInt(0)),
    "AcquireTracker initialized with a tail data beat.")

  when(collect_iacq_data) {
    io.inner.acquire.ready := Bool(true)
    when(io.inner.acquire.valid) {
      xact.data_buffer(io.iacq().addr_beat) := io.iacq().data
      xact.wmask_buffer(io.iacq().addr_beat) := io.iacq().wmask()
      iacq_data_valid := iacq_data_valid.bitSet(io.iacq().addr_beat, Bool(true))
    }
    when(iacq_data_done) { collect_iacq_data := Bool(false) }
  }

  when(pending_ognt_ack) {
    io.outer.grant.ready := Bool(true)
    when(io.outer.grant.valid) { pending_ognt_ack := Bool(false) }
    //TODO add finish queue if this isnt the last level manager
  }

  switch (state) {
    is(s_idle) {
      io.inner.acquire.ready := Bool(true)
      when(io.inner.acquire.valid) {
        xact := io.iacq()
        xact.data_buffer(UInt(0)) := io.iacq().data
        xact.wmask_buffer(UInt(0)) := io.iacq().wmask()
        collect_iacq_data := io.iacq().hasMultibeatData()
        iacq_data_valid := io.iacq().hasData() << io.iacq().addr_beat
        val needs_probes = mask_incoherent.orR
        when(needs_probes) {
          pending_probes := mask_incoherent
          release_count := PopCount(mask_incoherent)
        }
        state := Mux(needs_probes, s_probe,
                  Mux(pending_outer_write_, s_mem_write,
                    Mux(pending_outer_read_, s_mem_read, s_make_grant)))
      }
    }
    is(s_probe) {
      // Generate probes
      io.inner.probe.valid := pending_probes.orR
      when(io.inner.probe.ready) {
        pending_probes := pending_probes & ~UIntToOH(curr_p_id)
      }

      // Handle releases, which may have data to be written back
      io.inner.release.ready := !io.irel().hasData() || io.outer.acquire.ready
      when(io.inner.release.valid) {
        when(io.irel().hasData()) {
          io.outer.acquire.valid := Bool(true)
          when(io.outer.acquire.ready) {
            when(oacq_data_done) {
              pending_ognt_ack := Bool(true)
              release_count := release_count - UInt(1)
              when(release_count === UInt(1)) {
                state := Mux(pending_outer_write, s_mem_write,
                          Mux(pending_outer_read, s_mem_read, s_make_grant))
              }
            }
          }
        } .otherwise {
          release_count := release_count - UInt(1)
          when(release_count === UInt(1)) {
            state := Mux(pending_outer_write, s_mem_write, 
                      Mux(pending_outer_read, s_mem_read, s_make_grant))
          }
        }
      }
    }
    is(s_mem_write) { // Write data to outer memory
      io.outer.acquire.valid := !pending_ognt_ack && (!collect_iacq_data || iacq_data_valid(oacq_data_cnt))
      when(oacq_data_done) {
        pending_ognt_ack := Bool(true)
        state := Mux(pending_outer_read, s_mem_read, s_mem_resp)
      }
    }
    is(s_mem_read) { // Read data from outer memory (possibly what was just written)
      io.outer.acquire.valid := !pending_ognt_ack
      when(io.outer.acquire.fire()) { state := s_mem_resp }
    }
    is(s_mem_resp) { // Wait to forward grants from outer memory
      io.outer.grant.ready := io.inner.grant.ready
      io.inner.grant.valid := io.outer.grant.valid
      when(ignt_data_done) { 
        state := Mux(io.ignt().requiresAck(), s_ack, s_idle)
      }
    }
    is(s_make_grant) { // Manufacture a local grant (some kind of permission upgrade)
      io.inner.grant.valid := Bool(true)
      when(io.inner.grant.ready) { 
        state := Mux(io.ignt().requiresAck(), s_ack, s_idle)
      }
    }
    is(s_ack) { // Wait for transaction to complete
      io.inner.finish.ready := Bool(true)
      when(io.inner.finish.valid) { state := s_idle }
    }
  }
}
