package uncore
import Chisel._

abstract class CoherenceAgent(implicit conf: TileLinkConfiguration) extends Module {
  val io = new Bundle {
    val client = (new TileLinkIO).flip
    val master = new UncachedTileLinkIO
    val incoherent = Vec.fill(conf.ln.nClients){Bool()}.asInput
  }
}

case class L2CoherenceAgentConfiguration(tl: TileLinkConfiguration, nReleaseTransactions: Int, nAcquireTransactions: Int)

class L2CoherenceAgent(bankId: Int)(implicit conf: L2CoherenceAgentConfiguration) extends CoherenceAgent()(conf.tl)
{
  implicit val (tl, ln, co) = (conf.tl, conf.tl.ln, conf.tl.co)

  // Create SHRs for outstanding transactions
  val nTrackers = conf.nReleaseTransactions + conf.nAcquireTransactions
  val trackerList = (0 until conf.nReleaseTransactions).map(id => Module(new VoluntaryReleaseTracker(id, bankId))) ++ 
                    (conf.nReleaseTransactions until nTrackers).map(id => Module(new AcquireTracker(id, bankId)))
  
  // Propagate incoherence flags
  trackerList.map(_.io.tile_incoherent := io.incoherent.toBits)

  // Handle acquire transaction initiation
  val acquire = io.client.acquire
  val any_acquire_conflict = trackerList.map(_.io.has_acquire_conflict).reduce(_||_)
  val block_acquires = any_acquire_conflict

  val alloc_arb = Module(new Arbiter(Bool(), trackerList.size))
  for( i <- 0 until trackerList.size ) {
    val t = trackerList(i).io.client
    alloc_arb.io.in(i).valid := t.acquire.ready
    t.acquire.bits := acquire.bits
    t.acquire.valid := alloc_arb.io.in(i).ready
  }
  acquire.ready := trackerList.map(_.io.client.acquire.ready).reduce(_||_) && !block_acquires
  alloc_arb.io.out.ready := acquire.valid && !block_acquires

  // Handle probe request generation
  val probe_arb = Module(new Arbiter(new LogicalNetworkIO(new Probe), trackerList.size))
  io.client.probe <> probe_arb.io.out
  probe_arb.io.in zip trackerList map { case (arb, t) => arb <> t.io.client.probe }

  // Handle releases, which might be voluntary and might have data
  val release = io.client.release
  val voluntary = co.isVoluntary(release.bits.payload)
  val any_release_conflict = trackerList.tail.map(_.io.has_release_conflict).reduce(_||_)
  val block_releases = Bool(false)
  val conflict_idx = Vec(trackerList.map(_.io.has_release_conflict)).lastIndexWhere{b: Bool => b}
  //val release_idx = Mux(voluntary, Mux(any_release_conflict, conflict_idx, UInt(0)), release.bits.payload.master_xact_id) // TODO: Add merging logic to allow allocated AcquireTracker to handle conflicts, send all necessary grants, use first sufficient response
  val release_idx = Mux(voluntary, UInt(0), release.bits.payload.master_xact_id)
  for( i <- 0 until trackerList.size ) {
    val t = trackerList(i).io.client
    t.release.bits := release.bits 
    t.release.valid := release.valid && (release_idx === UInt(i)) && !block_releases
  }
  release.ready := Vec(trackerList.map(_.io.client.release.ready)).read(release_idx) && !block_releases

  // Reply to initial requestor
  val grant_arb = Module(new Arbiter(new LogicalNetworkIO(new Grant), trackerList.size))
  io.client.grant <> grant_arb.io.out
  grant_arb.io.in zip trackerList map { case (arb, t) => arb <> t.io.client.grant }

  // Free finished transactions
  val ack = io.client.grant_ack
  trackerList.map(_.io.client.grant_ack.valid := ack.valid)
  trackerList.map(_.io.client.grant_ack.bits := ack.bits)
  ack.ready := Bool(true)

  // Create an arbiter for the one memory port
  val outer_arb = Module(new UncachedTileLinkIOArbiterThatPassesId(trackerList.size))
  outer_arb.io.in zip  trackerList map { case(arb, t) => arb <> t.io.master }
  io.master <> outer_arb.io.out
}


abstract class XactTracker()(implicit conf: L2CoherenceAgentConfiguration) extends Module {
  implicit val (tl, ln, co) = (conf.tl, conf.tl.ln, conf.tl.co)
  val io = new Bundle {
    val client = (new TileLinkIO).flip
    val master = new UncachedTileLinkIO
    val tile_incoherent = Bits(INPUT, ln.nClients)
    val has_acquire_conflict = Bool(OUTPUT)
    val has_release_conflict = Bool(OUTPUT)
  }

  val c_acq = io.client.acquire.bits
  val c_rel = io.client.release.bits
  val c_gnt = io.client.grant.bits
  val c_ack = io.client.grant_ack.bits
  val m_gnt = io.master.grant.bits

}

class VoluntaryReleaseTracker(trackerId: Int, bankId: Int)(implicit conf: L2CoherenceAgentConfiguration) extends XactTracker()(conf) {
  val s_idle :: s_mem :: s_ack :: s_busy :: Nil = Enum(UInt(), 4)
  val state = Reg(init=s_idle)
  val xact  = Reg{ new Release }
  val init_client_id = Reg(init=UInt(0, width = log2Up(ln.nClients)))
  val incoming_rel = io.client.release.bits

  io.has_acquire_conflict := Bool(false)
  io.has_release_conflict := co.isCoherenceConflict(xact.addr, incoming_rel.payload.addr) && 
                               (state != s_idle)

  io.master.grant.ready := Bool(false)
  io.master.acquire.valid := Bool(false)
  io.master.acquire.bits.header.src := UInt(bankId) 
  //io.master.acquire.bits.header.dst TODO
  io.master.acquire.bits.payload := Acquire(co.getUncachedWriteAcquireType,
                                            xact.addr,
                                            UInt(trackerId),
                                            xact.data)
  io.client.acquire.ready := Bool(false)
  io.client.probe.valid := Bool(false)
  io.client.release.ready := Bool(false)
  io.client.grant.valid := Bool(false)
  io.client.grant.bits.header.src := UInt(bankId)
  io.client.grant.bits.header.dst := init_client_id
  io.client.grant.bits.payload := Grant(co.getGrantType(xact, UInt(0)),
                                        xact.client_xact_id,
                                        UInt(trackerId))

  switch (state) {
    is(s_idle) {
      io.client.release.ready := Bool(true)
      when( io.client.release.valid ) {
        xact := incoming_rel.payload
        init_client_id := incoming_rel.header.src
        state := Mux(co.messageHasData(incoming_rel.payload), s_mem, s_ack)
      }
    }
    is(s_mem) {
      io.master.acquire.valid := Bool(true)
      when(io.master.acquire.ready) { state := s_ack }
    }
    is(s_ack) {
      io.client.grant.valid := Bool(true)
      when(io.client.grant.ready) { state := s_idle }
    }
  }
}

class AcquireTracker(trackerId: Int, bankId: Int)(implicit conf: L2CoherenceAgentConfiguration) extends XactTracker()(conf) {
  val s_idle :: s_probe :: s_mem_read :: s_mem_write :: s_make_grant :: s_busy :: Nil = Enum(UInt(), 6)
  val state = Reg(init=s_idle)
  val xact  = Reg{ new Acquire }
  val init_client_id = Reg(init=UInt(0, width = log2Up(ln.nClients)))
  //TODO: Will need id reg for merged release xacts

  val init_sharer_cnt = Reg(init=UInt(0, width = log2Up(ln.nClients)))
  val release_count = if (ln.nClients == 1) UInt(0) else Reg(init=UInt(0, width = log2Up(ln.nClients)))
  val probe_flags = Reg(init=Bits(0, width = ln.nClients))
  val curr_p_id = PriorityEncoder(probe_flags)

  val pending_outer_write = co.messageHasData(xact)
  val pending_outer_read = co.needsOuterRead(xact.a_type, UInt(0))
  val outer_write_acq = Acquire(co.getUncachedWriteAcquireType, 
                                       xact.addr, UInt(trackerId), xact.data)
  val outer_write_rel = Acquire(co.getUncachedWriteAcquireType, 
                                       xact.addr, UInt(trackerId), c_rel.payload.data)
  val outer_read = Acquire(co.getUncachedReadAcquireType, xact.addr, UInt(trackerId))

  val probe_initial_flags = Bits(width = ln.nClients)
  probe_initial_flags := Bits(0)
  if (ln.nClients > 1) {
    // issue self-probes for uncached read xacts to facilitate I$ coherence
    val probe_self = Bool(true) //co.needsSelfProbe(io.client.acquire.bits.payload)
    val myflag = Mux(probe_self, Bits(0), UIntToOH(c_acq.header.src(log2Up(ln.nClients)-1,0)))
    probe_initial_flags := ~(io.tile_incoherent | myflag)
  }

  io.has_acquire_conflict := co.isCoherenceConflict(xact.addr, c_acq.payload.addr) && (state != s_idle)
  io.has_release_conflict := co.isCoherenceConflict(xact.addr, c_rel.payload.addr) && (state != s_idle)

  io.master.acquire.valid := Bool(false)
  io.master.acquire.bits.header.src := UInt(bankId)
  //io.master.acquire.bits.header.dst TODO
  io.master.acquire.bits.payload := outer_read 
  io.master.grant.ready := io.client.grant.ready

  io.client.probe.valid := Bool(false)
  io.client.probe.bits.header.src := UInt(bankId)
  io.client.probe.bits.header.dst := curr_p_id
  io.client.probe.bits.payload := Probe(co.getProbeType(xact.a_type, UInt(0)),
                                               xact.addr,
                                               UInt(trackerId))

  val grant_type = co.getGrantType(xact.a_type, init_sharer_cnt)
  io.client.grant.valid := Bool(false)
  io.client.grant.bits.header.src := UInt(bankId)
  io.client.grant.bits.header.dst := init_client_id
  io.client.grant.bits.payload := Grant(grant_type,
                                        xact.client_xact_id,
                                        UInt(trackerId),
                                        m_gnt.payload.data)

  io.client.acquire.ready := Bool(false)
  io.client.release.ready := Bool(false)

  switch (state) {
    is(s_idle) {
      io.client.acquire.ready := Bool(true)
      val needs_outer_write = co.messageHasData(c_acq.payload)
      val needs_outer_read = co.needsOuterRead(c_acq.payload.a_type, UInt(0))
      when( io.client.acquire.valid ) {
        xact := c_acq.payload
        init_client_id := c_acq.header.src
        init_sharer_cnt := UInt(ln.nClients) // TODO: Broadcast only
        probe_flags := probe_initial_flags
        if(ln.nClients > 1) {
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
      io.client.probe.valid := probe_flags.orR
      when(io.client.probe.ready) {
        probe_flags := probe_flags & ~(UIntToOH(curr_p_id))
      }

      // Handle releases, which may have data to be written back
      when(io.client.release.valid) {
        when(co.messageHasData(c_rel.payload)) {
          io.master.acquire.valid := Bool(true)
          io.master.acquire.bits.payload := outer_write_rel
          when(io.master.acquire.ready) {
            io.client.release.ready := Bool(true)
            if(ln.nClients > 1) release_count := release_count - UInt(1)
            when(release_count === UInt(1)) {
              state := Mux(pending_outer_write, s_mem_write,
                        Mux(pending_outer_read, s_mem_read, s_make_grant))
            }
          }
        } .otherwise {
          io.client.release.ready := Bool(true)
          if(ln.nClients > 1) release_count := release_count - UInt(1)
          when(release_count === UInt(1)) {
            state := Mux(pending_outer_write, s_mem_write, 
                      Mux(pending_outer_read, s_mem_read, s_make_grant))
          }
        }
      }
    }
    is(s_mem_read) {
      io.master.acquire.valid := Bool(true)
      io.master.acquire.bits.payload := outer_read
      when(io.master.acquire.ready) {
        state := Mux(co.requiresAckForGrant(grant_type), s_busy, s_idle)
      }
    }
    is(s_mem_write) {
      io.master.acquire.valid := Bool(true)
      io.master.acquire.bits.payload := outer_write_acq
      when(io.master.acquire.ready) { 
        state := Mux(pending_outer_read, s_mem_read, s_make_grant)
      }
    }
    is(s_make_grant) {
      io.client.grant.valid := Bool(true)
      when(io.client.grant.ready) { 
        state := Mux(co.requiresAckForGrant(grant_type), s_busy, s_idle)
      }
    }
    is(s_busy) { // Nothing left to do but wait for transaction to complete
      when(io.master.grant.valid && m_gnt.payload.client_xact_id === UInt(trackerId)) {
        io.client.grant.valid := Bool(true)
      }
      when(io.client.grant_ack.valid && c_ack.payload.master_xact_id === UInt(trackerId)) {
        state := s_idle
      }
    }
  }
}
