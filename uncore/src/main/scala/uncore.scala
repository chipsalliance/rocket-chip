// See LICENSE for license details.

package uncore
import Chisel._

case object NReleaseTransactors extends Field[Int]
case object NAcquireTransactors extends Field[Int]
case object NClients extends Field[Int]

abstract trait CoherenceAgentParameters extends UsesParameters {
  val co = params(TLCoherence)
  val nReleaseTransactors = params(NReleaseTransactors)
  val nAcquireTransactors = params(NAcquireTransactors)
  val nTransactors = nReleaseTransactors + nAcquireTransactors
  val nClients = params(NClients)
}

abstract class CoherenceAgent(innerId: String, outerId: String) extends Module
    with CoherenceAgentParameters {
  val io = new Bundle {
    val inner = Bundle(new TileLinkIO, {case TLId => innerId}).flip
    val outer = Bundle(new UncachedTileLinkIO, {case TLId => outerId})
    val incoherent = Vec.fill(nClients){Bool()}.asInput
  }
}

class L2CoherenceAgent(bankId: Int, innerId: String, outerId: String) extends 
    CoherenceAgent(innerId, outerId) {

  // Create SHRs for outstanding transactions
  val trackerList = (0 until nReleaseTransactors).map(id => 
    Module(new VoluntaryReleaseTracker(id, bankId, innerId, outerId))) ++ 
      (nReleaseTransactors until nTransactors).map(id => 
        Module(new AcquireTracker(id, bankId, innerId, outerId)))
  
  // Propagate incoherence flags
  trackerList.map(_.io.tile_incoherent := io.incoherent.toBits)

  // Handle acquire transaction initiation
  val acquire = io.inner.acquire
  val any_acquire_conflict = trackerList.map(_.io.has_acquire_conflict).reduce(_||_)
  val block_acquires = any_acquire_conflict

  val alloc_arb = Module(new Arbiter(Bool(), trackerList.size))
  for( i <- 0 until trackerList.size ) {
    val t = trackerList(i).io.inner
    alloc_arb.io.in(i).valid := t.acquire.ready
    t.acquire.bits := acquire.bits
    t.acquire.valid := alloc_arb.io.in(i).ready
  }
  acquire.ready := trackerList.map(_.io.inner.acquire.ready).reduce(_||_) && !block_acquires
  alloc_arb.io.out.ready := acquire.valid && !block_acquires

  // Handle probe request generation
  val probe_arb = Module(new Arbiter(new LogicalNetworkIO(new Probe), trackerList.size))
  io.inner.probe <> probe_arb.io.out
  probe_arb.io.in zip trackerList map { case (arb, t) => arb <> t.io.inner.probe }

  // Handle releases, which might be voluntary and might have data
  val release = io.inner.release
  val voluntary = co.isVoluntary(release.bits.payload)
  val any_release_conflict = trackerList.tail.map(_.io.has_release_conflict).reduce(_||_)
  val block_releases = Bool(false)
  val conflict_idx = Vec(trackerList.map(_.io.has_release_conflict)).lastIndexWhere{b: Bool => b}
  //val release_idx = Mux(voluntary, Mux(any_release_conflict, conflict_idx, UInt(0)), release.bits.payload.master_xact_id) // TODO: Add merging logic to allow allocated AcquireTracker to handle conflicts, send all necessary grants, use first sufficient response
  val release_idx = Mux(voluntary, UInt(0), release.bits.payload.master_xact_id)
  for( i <- 0 until trackerList.size ) {
    val t = trackerList(i).io.inner
    t.release.bits := release.bits 
    t.release.valid := release.valid && (release_idx === UInt(i)) && !block_releases
  }
  release.ready := Vec(trackerList.map(_.io.inner.release.ready)).read(release_idx) && !block_releases

  // Reply to initial requestor
  val grant_arb = Module(new Arbiter(new LogicalNetworkIO(new Grant), trackerList.size))
  io.inner.grant <> grant_arb.io.out
  grant_arb.io.in zip trackerList map { case (arb, t) => arb <> t.io.inner.grant }

  // Free finished transactions
  val ack = io.inner.finish
  trackerList.map(_.io.inner.finish.valid := ack.valid)
  trackerList.map(_.io.inner.finish.bits := ack.bits)
  ack.ready := Bool(true)

  // Create an arbiter for the one memory port
  val outer_arb = Module(new UncachedTileLinkIOArbiterThatPassesId(trackerList.size),
                         {case TLId => outerId})
  outer_arb.io.in zip  trackerList map { case(arb, t) => arb <> t.io.outer }
  io.outer <> outer_arb.io.out
}


abstract class XactTracker(innerId: String, outerId: String) extends Module {
  val (co, nClients) = (params(TLCoherence),params(NClients))
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
  val s_idle :: s_mem :: s_ack :: s_busy :: Nil = Enum(UInt(), 4)
  val state = Reg(init=s_idle)
  val xact  = Reg{ new Release }
  val init_client_id = Reg(init=UInt(0, width = log2Up(nClients)))
  val incoming_rel = io.inner.release.bits

  io.has_acquire_conflict := Bool(false)
  io.has_release_conflict := co.isCoherenceConflict(xact.addr, incoming_rel.payload.addr) && 
                               (state != s_idle)

  io.outer.grant.ready := Bool(false)
  io.outer.acquire.valid := Bool(false)
  io.outer.acquire.bits.header.src := UInt(bankId) 
  //io.outer.acquire.bits.header.dst TODO
  io.outer.acquire.bits.payload := Bundle(Acquire(co.getUncachedWriteAcquireType,
                                            xact.addr,
                                            UInt(trackerId),
                                            xact.data),
                                    { case TLId => outerId })
  io.inner.acquire.ready := Bool(false)
  io.inner.probe.valid := Bool(false)
  io.inner.release.ready := Bool(false)
  io.inner.grant.valid := Bool(false)
  io.inner.grant.bits.header.src := UInt(bankId)
  io.inner.grant.bits.header.dst := init_client_id
  io.inner.grant.bits.payload := Grant(co.getGrantType(xact, co.masterMetadataOnFlush),
                                        xact.client_xact_id,
                                        UInt(trackerId))

  switch (state) {
    is(s_idle) {
      io.inner.release.ready := Bool(true)
      when( io.inner.release.valid ) {
        xact := incoming_rel.payload
        init_client_id := incoming_rel.header.src
        state := Mux(co.messageHasData(incoming_rel.payload), s_mem, s_ack)
      }
    }
    is(s_mem) {
      io.outer.acquire.valid := Bool(true)
      when(io.outer.acquire.ready) { state := s_ack }
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

  val release_count = if (nClients == 1) UInt(0) else Reg(init=UInt(0, width = log2Up(nClients)))
  val probe_flags = Reg(init=Bits(0, width = nClients))
  val curr_p_id = PriorityEncoder(probe_flags)

  val pending_outer_write = co.messageHasData(xact)
  val pending_outer_read = co.requiresOuterRead(xact.a_type)
  val outer_write_acq = Bundle(Acquire(co.getUncachedWriteAcquireType, 
                                       xact.addr, UInt(trackerId), xact.data),
                                    { case TLId => outerId })
  val outer_write_rel = Bundle(Acquire(co.getUncachedWriteAcquireType, 
                                       xact.addr, UInt(trackerId), c_rel.payload.data),
                                    { case TLId => outerId })
  val outer_read = Bundle(Acquire(co.getUncachedReadAcquireType, xact.addr, UInt(trackerId)),
                                    { case TLId => outerId })

  val probe_initial_flags = Bits(width = nClients)
  probe_initial_flags := Bits(0)
  if (nClients > 1) {
    // issue self-probes for uncached read xacts to facilitate I$ coherence
    val probe_self = Bool(true) //co.needsSelfProbe(io.inner.acquire.bits.payload)
    val myflag = Mux(probe_self, Bits(0), UIntToOH(c_acq.header.src(log2Up(nClients)-1,0)))
    probe_initial_flags := ~(io.tile_incoherent | myflag)
  }

  io.has_acquire_conflict := co.isCoherenceConflict(xact.addr, c_acq.payload.addr) && (state != s_idle)
  io.has_release_conflict := co.isCoherenceConflict(xact.addr, c_rel.payload.addr) && (state != s_idle)

  io.outer.acquire.valid := Bool(false)
  io.outer.acquire.bits.header.src := UInt(bankId)
  //io.outer.acquire.bits.header.dst TODO
  io.outer.acquire.bits.payload := outer_read 
  io.outer.grant.ready := io.inner.grant.ready

  io.inner.probe.valid := Bool(false)
  io.inner.probe.bits.header.src := UInt(bankId)
  io.inner.probe.bits.header.dst := curr_p_id
  io.inner.probe.bits.payload := Probe(co.getProbeType(xact, co.masterMetadataOnFlush),
                                               xact.addr,
                                               UInt(trackerId))

  val grant_type = co.getGrantType(xact, co.masterMetadataOnFlush)
  io.inner.grant.valid := Bool(false)
  io.inner.grant.bits.header.src := UInt(bankId)
  io.inner.grant.bits.header.dst := init_client_id
  io.inner.grant.bits.payload := Grant(grant_type,
                                        xact.client_xact_id,
                                        UInt(trackerId),
                                        m_gnt.payload.data)

  io.inner.acquire.ready := Bool(false)
  io.inner.release.ready := Bool(false)

  switch (state) {
    is(s_idle) {
      io.inner.acquire.ready := Bool(true)
      val needs_outer_write = co.messageHasData(c_acq.payload)
      val needs_outer_read = co.requiresOuterRead(c_acq.payload.a_type)
      when( io.inner.acquire.valid ) {
        xact := c_acq.payload
        init_client_id := c_acq.header.src
        probe_flags := probe_initial_flags
        if(nClients > 1) {
          release_count := PopCount(probe_initial_flags)
          state := Mux(probe_initial_flags.orR, s_probe,
                    Mux(needs_outer_write, s_mem_write,
                      Mux(needs_outer_read, s_mem_read, s_make_grant)))
        } else state := Mux(needs_outer_write, s_mem_write,
                        Mux(needs_outer_read, s_mem_read, s_make_grant))
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
            if(nClients > 1) release_count := release_count - UInt(1)
            when(release_count === UInt(1)) {
              state := Mux(pending_outer_write, s_mem_write,
                        Mux(pending_outer_read, s_mem_read, s_make_grant))
            }
          }
        } .otherwise {
          io.inner.release.ready := Bool(true)
          if(nClients > 1) release_count := release_count - UInt(1)
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
        state := Mux(co.requiresAckForGrant(grant_type), s_busy, s_idle)
      }
    }
    is(s_mem_write) {
      io.outer.acquire.valid := Bool(true)
      io.outer.acquire.bits.payload := outer_write_acq
      when(io.outer.acquire.ready) { 
        state := Mux(pending_outer_read, s_mem_read, s_make_grant)
      }
    }
    is(s_make_grant) {
      io.inner.grant.valid := Bool(true)
      when(io.inner.grant.ready) { 
        state := Mux(co.requiresAckForGrant(grant_type), s_busy, s_idle)
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
