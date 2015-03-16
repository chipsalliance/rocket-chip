// See LICENSE for license details.

package uncore
import Chisel._

case object L2StoreDataQueueDepth extends Field[Int]

trait BroadcastHubParameters extends CoherenceAgentParameters {
  val sdqDepth = params(L2StoreDataQueueDepth)*innerDataBeats
  val dqIdxBits = math.max(log2Up(nReleaseTransactors) + 1, log2Up(sdqDepth))
  val nDataQueueLocations = 3 //Stores, VoluntaryWBs, Releases
}

class DataQueueLocation extends Bundle with BroadcastHubParameters {
  val idx = UInt(width = dqIdxBits)
  val loc = UInt(width = log2Ceil(nDataQueueLocations))
} 

object DataQueueLocation {
  def apply(idx: UInt, loc: UInt) = {
    val d = new DataQueueLocation
    d.idx := idx
    d.loc := loc
    d
  }
}

class L2BroadcastHub(bankId: Int) extends ManagerCoherenceAgent
    with BroadcastHubParameters {
  val internalDataBits = new DataQueueLocation().getWidth
  val inStoreQueue :: inVolWBQueue :: inClientReleaseQueue :: Nil = Enum(UInt(), nDataQueueLocations)

  // Create SHRs for outstanding transactions
  val trackerList = (0 until nReleaseTransactors).map(id => 
    Module(new BroadcastVoluntaryReleaseTracker(id, bankId), {case TLDataBits => internalDataBits})) ++ 
      (nReleaseTransactors until nTransactors).map(id => 
        Module(new BroadcastAcquireTracker(id, bankId), {case TLDataBits => internalDataBits}))
  
  // Propagate incoherence flags
  trackerList.map(_.io.incoherent := io.incoherent.toBits)

  // Queue to store impending Put data
  val sdq = Vec.fill(sdqDepth){ Reg(io.iacq().data) }
  val sdq_val = Reg(init=Bits(0, sdqDepth))
  val sdq_alloc_id = PriorityEncoder(~sdq_val)
  val sdq_rdy = !sdq_val.andR
  val sdq_enq = io.inner.acquire.fire() && io.iacq().hasData()
  when (sdq_enq) { sdq(sdq_alloc_id) := io.iacq().data }

  // Handle acquire transaction initiation
  val alloc_arb = Module(new Arbiter(Bool(), trackerList.size))
  val trackerAcquireIOs =  trackerList.map(_.io.inner.acquire)
  val acquireMatchList = trackerList.map(_.io.has_acquire_match)
  val any_acquire_matches = acquireMatchList.reduce(_||_)
  val alloc_idx = Vec(alloc_arb.io.in.map(_.ready)).lastIndexWhere{b: Bool => b}
  val match_idx = Vec(acquireMatchList).indexWhere{b: Bool => b}
  val acquire_idx = Mux(any_acquire_matches, match_idx, alloc_idx)
  trackerAcquireIOs.zip(alloc_arb.io.in).zipWithIndex.foreach {
    case((tracker, arb), i) =>
      arb.valid := tracker.ready
      tracker.bits := io.inner.acquire.bits
      tracker.bits.payload.data :=
        DataQueueLocation(sdq_alloc_id, inStoreQueue).toBits
      tracker.valid := arb.ready && (acquire_idx === UInt(i))
  }
  val block_acquires = trackerList.map(_.io.has_acquire_conflict).reduce(_||_)
  io.inner.acquire.ready := trackerAcquireIOs.map(_.ready).reduce(_||_) &&
                              sdq_rdy && !block_acquires
  alloc_arb.io.out.ready := io.inner.acquire.valid && sdq_rdy && !block_acquires

  // Queue to store impending Voluntary Release data
  val voluntary = io.irel().isVoluntary()
  val vwbdq_enq = io.inner.release.fire() && voluntary && io.irel().hasData()
  val (rel_data_cnt, rel_data_done) = Counter(vwbdq_enq, innerDataBeats) //TODO Zero width
  val vwbdq = Vec.fill(innerDataBeats){ Reg(io.irel().data) } //TODO Assumes nReleaseTransactors == 1 
  when(vwbdq_enq) { vwbdq(rel_data_cnt) := io.irel().data }

  // Handle releases, which might be voluntary and might have data
  val release_idx = Vec(trackerList.map(_.io.has_release_match)).indexWhere{b: Bool => b}
  val trackerReleaseIOs = trackerList.map(_.io.inner.release)
  trackerReleaseIOs.zipWithIndex.foreach {
    case(tracker, i) =>
      tracker.bits := io.inner.release.bits
      tracker.bits.payload.data := DataQueueLocation(rel_data_cnt,
                                     (if(i < nReleaseTransactors) inVolWBQueue
                                      else inClientReleaseQueue)).toBits
      tracker.valid := io.inner.release.valid && (release_idx === UInt(i))
  }
  io.inner.release.ready := Vec(trackerReleaseIOs.map(_.ready)).read(release_idx)

  // Wire probe requests and grant reply to clients, finish acks from clients
  // Note that we bypass the Grant data subbundles
  io.inner.grant.bits.payload.data := io.outer.grant.bits.payload.data
  io.inner.grant.bits.payload.addr_beat := io.outer.grant.bits.payload.addr_beat
  doOutputArbitration(io.inner.grant, trackerList.map(_.io.inner.grant))
  doOutputArbitration(io.inner.probe, trackerList.map(_.io.inner.probe))
  doInputRouting(io.inner.finish, trackerList.map(_.io.inner.finish))

  // Create an arbiter for the one memory port
  val outer_arb = Module(new UncachedTileLinkIOArbiterThatPassesId(trackerList.size),
                         { case TLId => params(OuterTLId)
                           case TLDataBits => internalDataBits })
  outer_arb.io.in zip  trackerList map { case(arb, t) => arb <> t.io.outer }
  // Get the pending data out of the store data queue
  val outer_data_ptr = new DataQueueLocation().fromBits(outer_arb.io.out.acquire.bits.payload.data)
  val is_in_sdq = outer_data_ptr.loc === inStoreQueue
  val free_sdq = io.outer.acquire.fire() &&
                  io.outer.acquire.bits.payload.hasData() &&
                  outer_data_ptr.loc === inStoreQueue
  io.outer.acquire.bits.payload.data := MuxLookup(outer_data_ptr.loc, io.irel().data, Array(
                                          inStoreQueue -> sdq(outer_data_ptr.idx),
                                          inVolWBQueue -> vwbdq(outer_data_ptr.idx)))
  io.outer <> outer_arb.io.out

  // Update SDQ valid bits
  when (io.outer.acquire.valid || sdq_enq) {
    sdq_val := sdq_val & ~(UIntToOH(outer_data_ptr.idx) & Fill(sdqDepth, free_sdq)) | 
               PriorityEncoderOH(~sdq_val(sdqDepth-1,0)) & Fill(sdqDepth, sdq_enq)
  }
}

class BroadcastXactTracker extends XactTracker {
  val io = new ManagerXactTrackerIO
}

class BroadcastVoluntaryReleaseTracker(trackerId: Int, bankId: Int) extends BroadcastXactTracker {
  val s_idle :: s_outer :: s_grant :: s_ack :: Nil = Enum(UInt(), 4)
  val state = Reg(init=s_idle)

  val xact_src = Reg(io.inner.release.bits.header.src.clone)
  val xact = Reg(Bundle(new Release, { case TLId => params(InnerTLId); case TLDataBits => 0 }))
  val data_buffer = Vec.fill(innerDataBeats){ Reg(io.irel().data.clone) }
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
  io.outer.finish.valid := Bool(false)
  io.inner.acquire.ready := Bool(false)
  io.inner.probe.valid := Bool(false)
  io.inner.release.ready := Bool(false)
  io.inner.grant.valid := Bool(false)
  io.inner.finish.ready := Bool(false)

  io.inner.grant.bits.header.src := UInt(bankId)
  io.inner.grant.bits.header.dst := xact_src
  io.inner.grant.bits.payload := coh.makeGrant(xact, UInt(trackerId))

  //TODO: Use io.outer.release instead?
  io.outer.acquire.bits.payload := Bundle(PutBlock( 
                                            client_xact_id = UInt(trackerId),
                                            addr_block = xact.addr_block,
                                            addr_beat = oacq_data_cnt,
                                            data = data_buffer(oacq_data_cnt)))(outerTLParams)

  when(collect_irel_data) {
    io.inner.release.ready := Bool(true)
    when(io.inner.release.valid) {
      data_buffer(io.irel().addr_beat) := io.irel().data
      irel_data_valid(io.irel().addr_beat) := Bool(true)
    }
    when(irel_data_done) { collect_irel_data := Bool(false) }
  }

  switch (state) {
    is(s_idle) {
      io.inner.release.ready := Bool(true)
      when( io.inner.release.valid ) {
        xact_src := io.inner.release.bits.header.src
        xact := io.irel()
        data_buffer(UInt(0)) := io.irel().data
        collect_irel_data := io.irel().hasMultibeatData()
        irel_data_valid := io.irel().hasData() << io.irel().addr_beat
        state := Mux(io.irel().hasData(), s_outer,
                   Mux(io.irel().requiresAck(), s_ack, s_idle))
      }
    }
    is(s_outer) {
      io.outer.acquire.valid := !collect_irel_data || irel_data_valid(oacq_data_cnt)
      when(oacq_data_done) { 
        state := Mux(xact.requiresAck(), s_grant, s_idle)
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

class BroadcastAcquireTracker(trackerId: Int, bankId: Int) extends BroadcastXactTracker {
  val s_idle :: s_probe :: s_mem_read :: s_mem_write :: s_make_grant :: s_mem_resp :: s_ack :: Nil = Enum(UInt(), 7)
  val state = Reg(init=s_idle)

  val xact_src = Reg(io.inner.acquire.bits.header.src.clone)
  val xact = Reg(Bundle(new Acquire, { case TLId => params(InnerTLId); case TLDataBits => 0 }))
  val data_buffer = Vec.fill(innerDataBeats){ Reg(io.iacq().data.clone) }
  val coh = ManagerMetadata.onReset

  assert(!(state != s_idle && xact.isBuiltInType() && 
      Vec(Acquire.getType, Acquire.putType, Acquire.putAtomicType,
        Acquire.prefetchType).contains(xact.a_type)),
    "Broadcast Hub does not support PutAtomics, subblock Gets/Puts, or prefetches") // TODO

  val release_count = Reg(init=UInt(0, width = log2Up(nCoherentClients+1)))
  val pending_probes = Reg(init=Bits(0, width = nCoherentClients))
  val curr_p_id = PriorityEncoder(pending_probes)
  val full_sharers = coh.full()
  val probe_self = io.inner.acquire.bits.payload.requiresSelfProbe()
  val mask_self = Mux(probe_self,
                    full_sharers | UInt(UInt(1) << xact_src, width = nCoherentClients),
                    full_sharers & ~UInt(UInt(1) << xact_src, width = nCoherentClients))
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

  io.has_acquire_conflict := xact.conflicts(io.iacq()) && 
                              (state != s_idle) &&
                              !collect_iacq_data
  io.has_acquire_match := xact.conflicts(io.iacq()) &&
                              collect_iacq_data
  io.has_release_match := xact.conflicts(io.irel()) &&
                            !io.irel().isVoluntary() &&
                            (state === s_probe)

  val outer_write_acq = Bundle(PutBlock(
                                client_xact_id = UInt(trackerId),
                                addr_block = xact.addr_block,
                                addr_beat = oacq_data_cnt,
                                data = data_buffer(oacq_data_cnt)))(outerTLParams)
  val outer_write_rel = Bundle(PutBlock(
                                client_xact_id = UInt(trackerId),
                                addr_block = xact.addr_block,
                                addr_beat = io.irel().addr_beat,
                                data = io.irel().data))(outerTLParams)
  val outer_read = Bundle(GetBlock(
                            client_xact_id = UInt(trackerId),
                            addr_block = xact.addr_block))(outerTLParams)

  io.outer.acquire.valid := Bool(false)
  io.outer.acquire.bits.payload := outer_read //default
  io.outer.grant.ready := Bool(false)

  io.inner.probe.valid := Bool(false)
  io.inner.probe.bits.header.src := UInt(bankId)
  io.inner.probe.bits.header.dst := curr_p_id
  io.inner.probe.bits.payload := coh.makeProbe(xact)

  io.inner.grant.valid := Bool(false)
  io.inner.grant.bits.header.src := UInt(bankId)
  io.inner.grant.bits.header.dst := xact_src
  io.inner.grant.bits.payload := coh.makeGrant(xact, UInt(trackerId)) // Data bypassed in parent

  io.inner.acquire.ready := Bool(false)
  io.inner.release.ready := Bool(false)
  io.inner.finish.ready := Bool(false)

  assert(!(state != s_idle && collect_iacq_data && io.inner.acquire.fire() &&
    io.inner.acquire.bits.header.src != xact_src),
    "AcquireTracker accepted data beat from different network source than initial request.")

  assert(!(state != s_idle && collect_iacq_data && io.inner.acquire.fire() &&
    io.iacq().client_xact_id != xact.client_xact_id),
    "AcquireTracker accepted data beat from different client transaction than initial request.")

  assert(!(state === s_idle && io.inner.acquire.fire() &&
    io.iacq().addr_beat != UInt(0)),
    "AcquireTracker initialized with a tail data beat.")

  when(collect_iacq_data) {
    io.inner.acquire.ready := Bool(true)
    when(io.inner.acquire.valid) {
      data_buffer(io.iacq().addr_beat) := io.iacq().data
      iacq_data_valid(io.iacq().addr_beat) := Bool(true)
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
        xact_src := io.inner.acquire.bits.header.src
        xact := io.iacq()
        data_buffer(UInt(0)) := io.iacq().data
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
          io.outer.acquire.bits.payload := outer_write_rel
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
      io.outer.acquire.valid := !pending_ognt_ack || !collect_iacq_data || iacq_data_valid(oacq_data_cnt)
      io.outer.acquire.bits.payload := outer_write_acq
      when(oacq_data_done) {
        pending_ognt_ack := Bool(true)
        state := Mux(pending_outer_read, s_mem_read, s_mem_resp)
      }
    }
    is(s_mem_read) { // Read data from outer memory (possibly what was just written)
      io.outer.acquire.valid := !pending_ognt_ack
      io.outer.acquire.bits.payload := outer_read
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
