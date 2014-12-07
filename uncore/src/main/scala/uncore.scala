// See LICENSE for license details.

package uncore
import Chisel._

case object NReleaseTransactors extends Field[Int]
case object NAcquireTransactors extends Field[Int]
case object L2StoreDataQueueDepth extends Field[Int]
case object NClients extends Field[Int]

abstract trait CoherenceAgentParameters extends UsesParameters 
    with TileLinkParameters {
  val nReleaseTransactors = 1
  val nAcquireTransactors = params(NAcquireTransactors)
  val nTransactors = nReleaseTransactors + nAcquireTransactors
  val nClients = params(NClients)
  val sdqDepth = params(L2StoreDataQueueDepth)*tlDataBeats
  val dqIdxBits = math.max(log2Up(nReleaseTransactors) + 1, log2Up(params(L2StoreDataQueueDepth))) +
                    log2Ceil(tlDataBeats)
  val nDataQueueLocations = 3 //Stores, VoluntaryWBs, Releases
}

abstract class CoherenceAgent(innerId: String, outerId: String) extends Module
    with CoherenceAgentParameters {
  val io = new Bundle {
    val inner = Bundle(new TileLinkIO, {case TLId => innerId}).flip
    val outer = Bundle(new UncachedTileLinkIO, {case TLId => outerId})
    val incoherent = Vec.fill(nClients){Bool()}.asInput
  }
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

class L2CoherenceAgent(bankId: Int, innerId: String, outerId: String) extends 
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
  val sdq_enq = acquire.fire() && co.messageHasData(acquire.bits.payload)
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
  val voluntary = co.isVoluntary(release.bits.payload)
  val vwbdq_enq = release.fire() && voluntary && co.messageHasData(release.bits.payload)
  val (rel_data_cnt, rel_data_done) = Counter(vwbdq_enq, tlDataBeats) //TODO Zero width
  val vwbdq = Vec.fill(tlDataBeats){ Reg(release.bits.payload.data) } //TODO Assumes nReleaseTransactors == 1 
  when(vwbdq_enq) { vwbdq(rel_data_cnt) := release.bits.payload.data }

  // Handle releases, which might be voluntary and might have data
  val any_release_conflict = trackerList.tail.map(_.io.has_release_conflict).reduce(_||_)
  val block_releases = Bool(false)
  val conflict_idx = Vec(trackerList.map(_.io.has_release_conflict)).lastIndexWhere{b: Bool => b}
  val release_idx = Mux(voluntary, UInt(0), conflict_idx)
  for( i <- 0 until trackerList.size ) {
    val t = trackerList(i).io.inner
    t.release.bits := release.bits 
    t.release.bits.payload.data := (if (i < nReleaseTransactors) 
                                      DataQueueLocation(rel_data_cnt, inVolWBQueue)
                                    else DataQueueLocation(UInt(0), inClientReleaseQueue)).toBits
    t.release.valid := release.valid && (release_idx === UInt(i)) && !block_releases
  }
  release.ready := Vec(trackerList.map(_.io.inner.release.ready)).read(release_idx) && !block_releases

  // Wire probe requests to clients
  val probe_arb = Module(new Arbiter(new LogicalNetworkIO(new Probe), trackerList.size))
  io.inner.probe <> probe_arb.io.out
  probe_arb.io.in zip trackerList map { case (arb, t) => arb <> t.io.inner.probe }

  // Wire grant reply to initiating client
  def hasData(m: LogicalNetworkIO[Grant]) = co.messageHasData(m.payload)
  val grant_arb = Module(new LockingArbiter(new LogicalNetworkIO(new Grant), trackerList.size, tlDataBeats, Some(hasData _)))
  io.inner.grant.bits.payload.data := io.outer.grant.bits.payload.data
  io.inner.grant <> grant_arb.io.out
  grant_arb.io.in zip trackerList map { case (arb, t) => arb <> t.io.inner.grant }

  // Wire finished transaction acks
  val ack = io.inner.finish
  trackerList.map(_.io.inner.finish.valid := ack.valid)
  trackerList.map(_.io.inner.finish.bits := ack.bits)
  ack.ready := Bool(true)

  // Create an arbiter for the one memory port
  val outer_arb = Module(new UncachedTileLinkIOArbiterThatPassesId(trackerList.size),
                         { case TLId => outerId; case TLDataBits => internalDataBits })
  outer_arb.io.in zip  trackerList map { case(arb, t) => arb <> t.io.outer }
  val outer_data_ptr = new DataQueueLocation().fromBits(outer_arb.io.out.acquire.bits.payload.data)
  val is_in_sdq = outer_data_ptr.loc === inStoreQueue
  val free_sdq = io.outer.acquire.fire() &&
                  co.messageHasData(io.outer.acquire.bits.payload) &&
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
  val (co, nClients, tlDataBeats) = (params(TLCoherence),params(NClients),params(TLDataBeats))
  val io = new Bundle {
    val inner = Bundle(new TileLinkIO, {case TLId => innerId}).flip
    val outer = Bundle(new UncachedTileLinkIO, {case TLId => outerId})
    val tile_incoherent = Bits(INPUT, params(NClients))
    val has_acquire_conflict = Bool(OUTPUT)
    val has_release_conflict = Bool(OUTPUT)
  }

  val c_acq = io.inner.acquire.bits
  val c_rel = io.inner.release.bits
  val c_gnt = io.inner.grant.bits
  val c_ack = io.inner.finish.bits
  val m_gnt = io.outer.grant.bits

}

class VoluntaryReleaseTracker(trackerId: Int, bankId: Int, innerId: String, outerId: String) extends XactTracker(innerId, outerId) {
  val s_idle :: s_outer :: s_ack :: s_busy :: Nil = Enum(UInt(), 4)
  val state = Reg(init=s_idle)
  val xact  = Reg{ new Release }
  val init_client_id = Reg(init=UInt(0, width = log2Up(nClients)))
  val data_ptrs = Vec.fill(tlDataBeats){ Reg(io.inner.release.bits.payload.data.clone) }
  val collect_inner_data = Reg(init=Bool(false))
  val (inner_data_cnt, inner_data_done) =
    Counter(io.inner.release.fire() && co.messageHasData(io.inner.release.bits.payload), tlDataBeats)
  val (outer_data_cnt, outer_data_done) =
    Counter(io.outer.acquire.fire() && co.messageHasData(io.outer.acquire.bits.payload), tlDataBeats)

  io.has_acquire_conflict := Bool(false)
  io.has_release_conflict := co.isCoherenceConflict(xact.addr, c_rel.payload.addr) && 
                               (state != s_idle)

  io.outer.grant.ready := Bool(false)
  io.outer.acquire.valid := Bool(false)
  io.outer.acquire.bits.header.src := UInt(bankId) 
  //io.outer.acquire.bits.header.dst TODO
  io.outer.acquire.bits.payload := Bundle(UncachedWrite(
                                            xact.addr,
                                            UInt(trackerId),
                                            data_ptrs(outer_data_cnt)),
                                    { case TLId => outerId })
  io.inner.acquire.ready := Bool(false)
  io.inner.probe.valid := Bool(false)
  io.inner.release.ready := Bool(false)
  io.inner.grant.valid := Bool(false)
  io.inner.grant.bits.header.src := UInt(bankId)
  io.inner.grant.bits.header.dst := init_client_id
  io.inner.grant.bits.payload := Grant(Bool(false),
                                        co.getGrantTypeOnVoluntaryWriteback(co.masterMetadataOnFlush),
                                        xact.client_xact_id,
                                        UInt(trackerId))

  when(collect_inner_data) {
    io.inner.release.ready := Bool(true)
    when(io.inner.release.valid) {
      data_ptrs(inner_data_cnt) := c_rel.payload.data
    }
    when(inner_data_done) { collect_inner_data := Bool(false) }
  }

  switch (state) {
    is(s_idle) {
      io.inner.release.ready := Bool(true)
      when( io.inner.release.valid ) {
        xact := c_rel.payload
        init_client_id := c_rel.header.src
        data_ptrs(UInt(0)) := c_rel.payload.data
        collect_inner_data := co.messageHasData(c_rel.payload)
        state := Mux(co.messageHasData(c_rel.payload), s_outer, s_ack)
      }
    }
    is(s_outer) {
      io.outer.acquire.valid := (if(tlDataBeats == 1) Bool(true) else !collect_inner_data || (outer_data_cnt < inner_data_cnt))
      when(outer_data_done) { state := s_ack }
    }
    is(s_ack) {
      io.inner.grant.valid := Bool(true)
      when(io.inner.grant.ready) { state := s_idle }
    }
  }
}

class AcquireTracker(trackerId: Int, bankId: Int, innerId: String, outerId: String) extends XactTracker(innerId, outerId) {
  val s_idle :: s_probe :: s_mem_read :: s_mem_write :: s_make_grant :: s_busy :: Nil = Enum(UInt(), 6)
  val state = Reg(init=s_idle)
  val xact  = Reg{ new Acquire }
  val init_client_id = Reg(init=UInt(0, width = log2Up(nClients)))
  //TODO: Will need id reg for merged release xacts
  val data_ptrs = Vec.fill(tlDataBeats){ Reg(io.inner.acquire.bits.payload.data.clone) }
  val collect_inner_data = Reg(init=Bool(false))
  val (inner_data_cnt, inner_data_done) = 
    Counter(io.inner.acquire.fire() && co.messageHasData(io.inner.acquire.bits.payload), tlDataBeats)
  val (outer_data_cnt, outer_data_done) = 
    Counter(io.outer.acquire.fire() && co.messageHasData(io.outer.acquire.bits.payload), tlDataBeats)

  val release_count = if (nClients == 1) UInt(0) else Reg(init=UInt(0, width = log2Up(nClients)))
  val probe_flags = Reg(init=Bits(0, width = nClients))
  val curr_p_id = PriorityEncoder(probe_flags)

  val pending_outer_write = co.messageHasData(xact)
  val pending_outer_read = co.requiresOuterRead(xact, co.masterMetadataOnFlush)
  val outer_write_acq = Bundle(UncachedWrite(xact.addr, UInt(trackerId), data_ptrs(outer_data_cnt)),
                          { case TLId => outerId })
  val outer_write_rel = Bundle(UncachedWrite(xact.addr, UInt(trackerId), c_rel.payload.data),
                          { case TLId => outerId })
  val outer_read = Bundle(UncachedRead(xact.addr, UInt(trackerId)),
                      { case TLId => outerId })

  val probe_initial_flags = Bits(width = nClients)
  probe_initial_flags := Bits(0)
  // issue self-probes for uncached read xacts to facilitate I$ coherence
  val probe_self = co.requiresSelfProbe(io.inner.acquire.bits.payload)
  val myflag = Mux(probe_self, Bits(0), UIntToOH(c_acq.header.src(log2Up(nClients)-1,0)))
  probe_initial_flags := ~(io.tile_incoherent | myflag)

  io.has_acquire_conflict := co.isCoherenceConflict(xact.addr, c_acq.payload.addr) && (state != s_idle) && !collect_inner_data
  io.has_release_conflict := co.isCoherenceConflict(xact.addr, c_rel.payload.addr) && (state != s_idle)

  io.outer.acquire.valid := Bool(false)
  io.outer.acquire.bits.header.src := UInt(bankId)
  //io.outer.acquire.bits.header.dst TODO
  io.outer.acquire.bits.payload := outer_read 
  io.outer.grant.ready := io.inner.grant.ready

  io.inner.probe.valid := Bool(false)
  io.inner.probe.bits.header.src := UInt(bankId)
  io.inner.probe.bits.header.dst := curr_p_id
  io.inner.probe.bits.payload := Probe(co.getProbeType(xact, co.masterMetadataOnFlush), xact.addr)

  io.inner.grant.valid := Bool(false)
  io.inner.grant.bits.header.src := UInt(bankId)
  io.inner.grant.bits.header.dst := init_client_id
  io.inner.grant.bits.payload := Grant(xact.uncached,
                                        co.getGrantType(xact, co.masterMetadataOnFlush),
                                        xact.client_xact_id,
                                        UInt(trackerId),
                                        UInt(0)) // Data bypassed in parent

  io.inner.acquire.ready := Bool(false)
  io.inner.release.ready := Bool(false)

  when(collect_inner_data) {
    io.inner.acquire.ready := Bool(true)
    when(io.inner.acquire.valid) {
      data_ptrs(inner_data_cnt) := c_acq.payload.data
    }
    when(inner_data_done) { collect_inner_data := Bool(false) }
  }

  switch (state) {
    is(s_idle) {
      io.inner.acquire.ready := Bool(true)
      val needs_outer_write = co.messageHasData(c_acq.payload)
      val needs_outer_read = co.requiresOuterRead(c_acq.payload, co.masterMetadataOnFlush)
      when( io.inner.acquire.valid ) {
        xact := c_acq.payload
        init_client_id := c_acq.header.src
        data_ptrs(UInt(0)) := c_acq.payload.data
        collect_inner_data := co.messageHasData(c_acq.payload)
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
      when(io.inner.release.valid) {
        when(co.messageHasData(c_rel.payload)) {
          io.outer.acquire.valid := Bool(true)
          io.outer.acquire.bits.payload := outer_write_rel
          when(io.outer.acquire.ready) {
            io.inner.release.ready := Bool(true)
            when(outer_data_done) {
              release_count := release_count - UInt(1)
              when(release_count === UInt(1)) {
                state := Mux(pending_outer_write, s_mem_write,
                          Mux(pending_outer_read, s_mem_read, s_make_grant))
              }
            }
          }
        } .otherwise {
          io.inner.release.ready := Bool(true)
          release_count := release_count - UInt(1)
          when(release_count === UInt(1)) {
            state := Mux(pending_outer_write, s_mem_write, 
                      Mux(pending_outer_read, s_mem_read, s_make_grant))
          }
        }
      }
    }
    is(s_mem_read) {
      io.outer.acquire.valid := Bool(true)
      io.outer.acquire.bits.payload := outer_read
      when(io.outer.acquire.ready) {
        state := Mux(co.requiresAckForGrant(io.inner.grant.bits.payload), s_busy, s_idle)
      }
    }
    is(s_mem_write) {
      io.outer.acquire.valid := (if(tlDataBeats == 1) Bool(true) else !collect_inner_data || (outer_data_cnt < inner_data_cnt))
      io.outer.acquire.bits.payload := outer_write_acq
      when(outer_data_done) {
        state := Mux(pending_outer_read, s_mem_read, s_make_grant)
      }
    }
    is(s_make_grant) {
      io.inner.grant.valid := Bool(true)
      when(io.inner.grant.ready) { 
        state := Mux(co.requiresAckForGrant(io.inner.grant.bits.payload), s_busy, s_idle)
      }
    }
    is(s_busy) { // Nothing left to do but wait for transaction to complete
      when(io.outer.grant.valid && m_gnt.payload.client_xact_id === UInt(trackerId)) {
        io.inner.grant.valid := Bool(true)
      }
      when(io.inner.finish.valid && c_ack.payload.master_xact_id === UInt(trackerId)) {
        state := s_idle
      }
    }
  }
}
