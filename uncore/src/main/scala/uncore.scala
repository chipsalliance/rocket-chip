// See LICENSE for license details.

package uncore
import Chisel._

case object NReleaseTransactors extends Field[Int]
case object NProbeTransactors extends Field[Int]
case object NAcquireTransactors extends Field[Int]
case object NIncoherentClients extends Field[Int]
case object NCoherentClients extends Field[Int]
case object L2StoreDataQueueDepth extends Field[Int]
case object L2CoherencePolicy extends Field[DirectoryRepresentation => CoherencePolicy]
case object L2DirectoryRepresentation extends Field[DirectoryRepresentation]

abstract trait CoherenceAgentParameters extends UsesParameters 
    with TileLinkParameters {
  val nReleaseTransactors = 1
  val nAcquireTransactors = params(NAcquireTransactors)
  val nTransactors = nReleaseTransactors + nAcquireTransactors
  val nCoherentClients = params(NCoherentClients)
  val nIncoherentClients = params(NIncoherentClients)
  val nClients = nCoherentClients + nIncoherentClients
  val sdqDepth = params(L2StoreDataQueueDepth)*tlDataBeats
  val dqIdxBits = math.max(log2Up(nReleaseTransactors) + 1, log2Up(sdqDepth))
  val nDataQueueLocations = 3 //Stores, VoluntaryWBs, Releases
}

class DataQueueLocation extends Bundle with CoherenceAgentParameters {
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

abstract class CoherenceAgent(innerId: String, outerId: String) extends Module
    with CoherenceAgentParameters {
  val io = new Bundle {
    val inner = Bundle(new TileLinkIO, {case TLId => innerId}).flip
    val outer = Bundle(new UncachedTileLinkIO, {case TLId => outerId})
    val incoherent = Vec.fill(nClients){Bool()}.asInput
  }
}

class L2BroadcastHub(bankId: Int, innerId: String, outerId: String) extends 
    CoherenceAgent(innerId, outerId) {

  val internalDataBits = new DataQueueLocation().getWidth
  val inStoreQueue :: inVolWBQueue :: inClientReleaseQueue :: Nil = Enum(UInt(), nDataQueueLocations)

  // Create SHRs for outstanding transactions
  val trackerList = (0 until nReleaseTransactors).map(id => 
    Module(new VoluntaryReleaseTracker(id, bankId, innerId, outerId), {case TLDataBits => internalDataBits})) ++ 
      (nReleaseTransactors until nTransactors).map(id => 
        Module(new AcquireTracker(id, bankId, innerId, outerId), {case TLDataBits => internalDataBits}))
  
  // Propagate incoherence flags
  trackerList.map(_.io.tile_incoherent := io.incoherent.toBits)

  // Queue to store impending UncachedWrite data
  val acquire = io.inner.acquire
  val sdq_val = Reg(init=Bits(0, sdqDepth))
  val sdq_alloc_id = PriorityEncoder(~sdq_val)
  val sdq_rdy = !sdq_val.andR
  val sdq_enq = acquire.fire() && acquire.bits.payload.hasData()
  val sdq = Vec.fill(sdqDepth){ Reg(io.inner.acquire.bits.payload.data) }
  when (sdq_enq) { sdq(sdq_alloc_id) := acquire.bits.payload.data }

  // Handle acquire transaction initiation
  val any_acquire_conflict = trackerList.map(_.io.has_acquire_conflict).reduce(_||_)
  val block_acquires = any_acquire_conflict
  val alloc_arb = Module(new Arbiter(Bool(), trackerList.size))
  for( i <- 0 until trackerList.size ) {
    val t = trackerList(i).io.inner
    alloc_arb.io.in(i).valid := t.acquire.ready
    t.acquire.bits := acquire.bits
    t.acquire.bits.payload.data := DataQueueLocation(sdq_alloc_id, inStoreQueue).toBits
    t.acquire.valid := alloc_arb.io.in(i).ready
  }
  acquire.ready := trackerList.map(_.io.inner.acquire.ready).reduce(_||_) && sdq_rdy && !block_acquires
  alloc_arb.io.out.ready := acquire.valid && sdq_rdy && !block_acquires

  // Queue to store impending Voluntary Release data
  val release = io.inner.release
  val voluntary = release.bits.payload.isVoluntary()
  val vwbdq_enq = release.fire() && voluntary && release.bits.payload.hasData()
  val (rel_data_cnt, rel_data_done) = Counter(vwbdq_enq, tlDataBeats) //TODO Zero width
  val vwbdq = Vec.fill(tlDataBeats){ Reg(release.bits.payload.data) } //TODO Assumes nReleaseTransactors == 1 
  when(vwbdq_enq) { vwbdq(rel_data_cnt) := release.bits.payload.data }

  // Handle releases, which might be voluntary and might have data
  val release_idx = Vec(trackerList.map(_.io.has_release_match)).indexWhere{b: Bool => b}
  for( i <- 0 until trackerList.size ) {
    val t = trackerList(i).io.inner
    t.release.bits := release.bits 
    t.release.bits.payload.data := (if (i < nReleaseTransactors) 
                                      DataQueueLocation(rel_data_cnt, inVolWBQueue)
                                    else DataQueueLocation(UInt(0), inClientReleaseQueue)).toBits
    t.release.valid := release.valid && (release_idx === UInt(i))
  }
  release.ready := Vec(trackerList.map(_.io.inner.release.ready)).read(release_idx)

  // Wire finished transaction acks
  val ack = io.inner.finish
  trackerList.map(_.io.inner.finish.valid := ack.valid)
  trackerList.map(_.io.inner.finish.bits := ack.bits)
  ack.ready := Bool(true)

  // Wire probe requests to clients
  val probe_arb = Module(new Arbiter(new LogicalNetworkIO(new Probe), trackerList.size))
  io.inner.probe <> probe_arb.io.out
  probe_arb.io.in zip trackerList map { case (arb, t) => arb <> t.io.inner.probe }

  // Wire grant reply to initiating client
  def hasData(m: LogicalNetworkIO[Grant]) = m.payload.hasMultibeatData()
  val grant_arb = Module(new LockingArbiter(new LogicalNetworkIO(new Grant), trackerList.size, tlDataBeats, Some(hasData _)))
  io.inner.grant.bits.payload.data := io.outer.grant.bits.payload.data
  io.inner.grant.bits.payload.addr_beat := io.outer.grant.bits.payload.addr_beat
  io.inner.grant <> grant_arb.io.out
  grant_arb.io.in zip trackerList map { case (arb, t) => arb <> t.io.inner.grant }

  // Create an arbiter for the one memory port
  val outer_arb = Module(new UncachedTileLinkIOArbiterThatPassesId(trackerList.size),
                         { case TLId => outerId; case TLDataBits => internalDataBits })
  outer_arb.io.in zip  trackerList map { case(arb, t) => arb <> t.io.outer }
  val outer_data_ptr = new DataQueueLocation().fromBits(outer_arb.io.out.acquire.bits.payload.data)
  val is_in_sdq = outer_data_ptr.loc === inStoreQueue
  val free_sdq = io.outer.acquire.fire() &&
                  io.outer.acquire.bits.payload.hasData() &&
                  outer_data_ptr.loc === inStoreQueue
  io.outer.acquire.bits.payload.data := MuxLookup(outer_data_ptr.loc, release.bits.payload.data, Array(
                                          inStoreQueue -> sdq(outer_data_ptr.idx),
                                          inVolWBQueue -> vwbdq(outer_data_ptr.idx)))
  io.outer <> outer_arb.io.out

  // Update SDQ valid bits
  when (io.outer.acquire.valid || sdq_enq) {
    sdq_val := sdq_val & ~(UIntToOH(outer_data_ptr.idx) & Fill(sdqDepth, free_sdq)) | 
               PriorityEncoderOH(~sdq_val(sdqDepth-1,0)) & Fill(sdqDepth, sdq_enq)
  }
}


abstract class XactTracker(innerId: String, outerId: String) extends Module {
  val (co, tlDataBeats) = (params(TLCoherence), params(TLDataBeats))
  val nClients = params(NCoherentClients) + params(NIncoherentClients)
  val io = new Bundle {
    val inner = Bundle(new TileLinkIO, {case TLId => innerId}).flip
    val outer = Bundle(new UncachedTileLinkIO, {case TLId => outerId})
    val tile_incoherent = Bits(INPUT, nClients)
    val has_acquire_conflict = Bool(OUTPUT)
    val has_release_match = Bool(OUTPUT)
  }

  val cacq = io.inner.acquire.bits
  val crel = io.inner.release.bits
  val cgnt = io.inner.grant.bits
  val cfin = io.inner.finish.bits
  val macq = io.outer.acquire.bits
  val mgnt = io.outer.grant.bits

}

class VoluntaryReleaseTracker(trackerId: Int, bankId: Int, innerId: String, outerId: String) extends XactTracker(innerId, outerId) {
  val s_idle :: s_outer :: s_grant :: s_ack :: Nil = Enum(UInt(), 4)
  val state = Reg(init=s_idle)

  val xact_src = Reg(io.inner.release.bits.header.src.clone)
  val xact_r_type = Reg(io.inner.release.bits.payload.r_type)
  val xact_addr_block = Reg(io.inner.release.bits.payload.addr_block.clone)
  val xact_client_xact_id = Reg(io.inner.release.bits.payload.client_xact_id.clone)
  val xact_data = Vec.fill(tlDataBeats){ Reg(io.inner.release.bits.payload.data.clone) }
  val xact = Release(
    voluntary = Bool(true),
    r_type = xact_r_type, 
    client_xact_id = xact_client_xact_id,
    addr_block = xact_addr_block)

  val collect_inner_data = Reg(init=Bool(false))
  // TODO: assert that all releases have full blocks of data
  val (inner_data_cnt, inner_data_done) =
    Counter(io.inner.release.fire() && io.inner.release.bits.payload.hasMultibeatData(), tlDataBeats)
  val (outer_data_cnt, outer_data_done) =
    Counter(io.outer.acquire.fire() && io.outer.acquire.bits.payload.hasMultibeatData(), tlDataBeats)

  io.has_acquire_conflict := Bool(false)
  io.has_release_match := crel.payload.isVoluntary()

  io.outer.grant.ready := Bool(false)
  io.outer.acquire.valid := Bool(false)
  io.inner.acquire.ready := Bool(false)
  io.inner.probe.valid := Bool(false)
  io.inner.release.ready := Bool(false)
  io.inner.grant.valid := Bool(false)
  io.inner.finish.ready := Bool(false)

  io.inner.grant.bits.header.src := UInt(bankId)
  io.inner.grant.bits.header.dst := xact_src
  io.inner.grant.bits.payload := xact.makeGrant(UInt(trackerId))

  io.outer.acquire.bits.payload := Bundle(UncachedWriteBlock(
                                            client_xact_id = UInt(trackerId),
                                            addr_block = xact_addr_block,
                                            addr_beat = outer_data_cnt,
                                            data = xact_data(outer_data_cnt)),
                                    { case TLId => outerId })

  when(collect_inner_data) {
    io.inner.release.ready := Bool(true)
    when(io.inner.release.valid) {
      xact_data(inner_data_cnt) := crel.payload.data
    }
    when(inner_data_done) { collect_inner_data := Bool(false) }
  }

  switch (state) {
    is(s_idle) {
      io.inner.release.ready := Bool(true)
      when( io.inner.release.valid ) {
        xact_src := crel.header.src
        xact_r_type := crel.payload.r_type
        xact_addr_block := crel.payload.addr_block
        xact_client_xact_id := crel.payload.client_xact_id
        xact_data(UInt(0)) := crel.payload.data
        collect_inner_data := crel.payload.hasMultibeatData()
        state := Mux(crel.payload.hasData(), s_outer,
                   Mux(crel.payload.requiresAck(), s_ack, s_idle))
      }
    }
    is(s_outer) {
      io.outer.acquire.valid := (if(tlDataBeats == 1) Bool(true) 
                                  else !collect_inner_data || (outer_data_cnt < inner_data_cnt))
      when(outer_data_done) { 
        state := Mux(xact.requiresAck(), s_grant, s_idle)
      }
    }
    is(s_grant) {
      io.inner.grant.valid := Bool(true)
      when(io.inner.grant.ready) {
        state := Mux(cgnt.payload.requiresAck(), s_ack, s_idle)
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

class AcquireTracker(trackerId: Int, bankId: Int, innerId: String, outerId: String) extends XactTracker(innerId, outerId) {
  val s_idle :: s_probe :: s_mem_read :: s_mem_write :: s_make_grant :: s_mem_resp :: s_ack :: Nil = Enum(UInt(), 7)
  val state = Reg(init=s_idle)

  val xact_src = Reg(io.inner.acquire.bits.header.src.clone)
  val xact_uncached = Reg(io.inner.acquire.bits.payload.uncached.clone)
  val xact_a_type = Reg(io.inner.acquire.bits.payload.a_type.clone)
  val xact_client_xact_id = Reg(io.inner.acquire.bits.payload.client_xact_id.clone)
  val xact_addr_block = Reg(io.inner.acquire.bits.payload.addr_block.clone)
  val xact_addr_beat = Reg(io.inner.acquire.bits.payload.addr_beat.clone)
  val xact_subblock = Reg(io.inner.acquire.bits.payload.subblock.clone)
  val xact_data = Vec.fill(tlDataBeats){ Reg(io.inner.acquire.bits.payload.data.clone) }
  val xact = Acquire(
              uncached = xact_uncached,
              a_type = xact_a_type,
              client_xact_id = xact_client_xact_id,
              addr_block = xact_addr_block,
              addr_beat = xact_addr_beat,
              data = UInt(0),
              subblock = xact_subblock)

  val collect_inner_data = Reg(init=Bool(false))
  //TODO: Assert that if xact.uncached, xact_a_type is ReadBlock or WriteBlock
  val (inner_data_cnt, inner_data_done) = 
    Counter(io.inner.acquire.fire() && cacq.payload.hasMultibeatData(), tlDataBeats)
  val (outer_data_cnt, outer_data_done) = 
    Counter(io.outer.acquire.fire() && macq.payload.hasMultibeatData(), tlDataBeats)
  val (cgnt_data_cnt, cgnt_data_done) =
    Counter(io.inner.grant.fire() && cgnt.payload.hasMultibeatData(), tlDataBeats)


  val release_count = Reg(init=UInt(0, width = log2Up(nClients)))
  val probe_flags = Reg(init=Bits(0, width = nClients))
  val curr_p_id = PriorityEncoder(probe_flags)

  val pending_outer_write = xact.hasData()
  val pending_outer_read = co.requiresOuterRead(xact, co.managerMetadataOnFlush)

  val probe_initial_flags = Bits(width = nClients)
  probe_initial_flags := Bits(0)
  // issue self-probes for uncached read xacts to facilitate I$ coherence
  val probe_self = io.inner.acquire.bits.payload.requiresSelfProbe()
  val myflag = Mux(probe_self, Bits(0), UIntToOH(cacq.header.src(log2Up(nClients)-1,0)))
  probe_initial_flags := ~(io.tile_incoherent | myflag)

  io.has_acquire_conflict := co.isCoherenceConflict(xact_addr_block, cacq.payload.addr_block) && 
                              (state != s_idle) && 
                              !collect_inner_data
  io.has_release_match := co.isCoherenceConflict(xact_addr_block, crel.payload.addr_block) && 
                              !crel.payload.isVoluntary() &&
                              (state != s_idle)

  val outer_write_acq = Bundle(UncachedWriteBlock(
                                client_xact_id = UInt(trackerId),
                                addr_block = xact_addr_block,
                                addr_beat = outer_data_cnt,
                                data = xact_data(outer_data_cnt)),
                          { case TLId => outerId })
  val outer_write_rel = Bundle(UncachedWriteBlock(
                                client_xact_id = UInt(trackerId),
                                addr_block = xact_addr_block,
                                addr_beat = crel.payload.addr_beat,
                                data = crel.payload.data),
                          { case TLId => outerId })
  val outer_read = Bundle(UncachedReadBlock(
                            client_xact_id = UInt(trackerId),
                            addr_block = xact_addr_block),
                          { case TLId => outerId })

  io.outer.acquire.valid := Bool(false)
  io.outer.acquire.bits.payload := outer_read //default
  io.outer.grant.ready := io.inner.grant.ready

  io.inner.probe.valid := Bool(false)
  io.inner.probe.bits.header.src := UInt(bankId)
  io.inner.probe.bits.header.dst := curr_p_id
  io.inner.probe.bits.payload := xact.makeProbe()

  io.inner.grant.valid := Bool(false)
  io.inner.grant.bits.header.src := UInt(bankId)
  io.inner.grant.bits.header.dst := xact_src
  io.inner.grant.bits.payload := xact.makeGrant(UInt(trackerId)) // Data bypassed in parent

  io.inner.acquire.ready := Bool(false)
  io.inner.release.ready := Bool(false)

  when(collect_inner_data) {
    io.inner.acquire.ready := Bool(true)
    when(io.inner.acquire.valid) {
      xact_data(inner_data_cnt) := cacq.payload.data
    }
    when(inner_data_done) { collect_inner_data := Bool(false) }
  }

  switch (state) {
    is(s_idle) {
      io.inner.acquire.ready := Bool(true)
      val needs_outer_write = cacq.payload.hasData()
      val needs_outer_read = co.requiresOuterRead(cacq.payload, co.managerMetadataOnFlush)
      when(io.inner.acquire.valid) {
        xact_uncached := cacq.payload.uncached
        xact_a_type := cacq.payload.a_type
        xact_addr_block := cacq.payload.addr_block
        xact_addr_beat := cacq.payload.addr_beat
        xact_client_xact_id := cacq.payload.client_xact_id
        xact_data(UInt(0)) := cacq.payload.data
        xact_subblock := cacq.payload.subblock
        xact_src := cacq.header.src
        collect_inner_data := cacq.payload.hasMultibeatData()
        probe_flags := probe_initial_flags
        release_count := PopCount(probe_initial_flags)
        state := Mux(probe_initial_flags.orR, s_probe,
                  Mux(needs_outer_write, s_mem_write,
                    Mux(needs_outer_read, s_mem_read, s_make_grant)))
      }
    }
    is(s_probe) {
      // Generate probes
      io.inner.probe.valid := probe_flags.orR
      when(io.inner.probe.ready) {
        probe_flags := probe_flags & ~(UIntToOH(curr_p_id))
      }

      // Handle releases, which may have data to be written back
      io.inner.release.ready := !crel.payload.hasData() || io.outer.acquire.ready
      when(io.inner.release.valid) {
        when(crel.payload.hasData()) {
          io.outer.acquire.valid := Bool(true)
          io.outer.acquire.bits.payload := outer_write_rel
          when(io.outer.acquire.ready) {
            when(outer_data_done) {
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
    is(s_mem_read) { // Read data from outer memory (possibly what was just written)
      io.outer.acquire.valid := Bool(true)
      io.outer.acquire.bits.payload := outer_read
      when(io.outer.acquire.ready) { state := s_mem_resp }
    }
    is(s_mem_write) { // Write data to outer memory
      io.outer.acquire.valid := (if(tlDataBeats == 1) Bool(true) 
                                  else !collect_inner_data || (outer_data_cnt < inner_data_cnt))
      io.outer.acquire.bits.payload := outer_write_acq
      when(outer_data_done) {
        state := Mux(pending_outer_read, s_mem_read, s_make_grant)
      }
    }
    is(s_make_grant) { // Manufactor a local grant (some kind of permission upgrade)
      io.inner.grant.valid := Bool(true)
      when(io.inner.grant.ready) { 
        state := Mux(cgnt.payload.requiresAck(), s_ack, s_idle)
      }
    }
    is(s_mem_resp) { // Wait to forward grants from outer memory
      when(io.outer.grant.valid && mgnt.payload.client_xact_id === UInt(trackerId)) {
        io.inner.grant.valid := Bool(true)
      }
      when(cgnt_data_done) { 
        state := Mux(cgnt.payload.requiresAck(), s_ack, s_idle)
      }
    }
    is(s_ack) { // Wait for transaction to complete
      when(io.inner.finish.valid && cfin.payload.manager_xact_id === UInt(trackerId)) {
        state := s_idle
      }
    }
  }
}
