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
    alloc_arb.io.in(i).valid := t.acquire.meta.ready
    t.acquire.meta.bits := acquire.meta.bits
    t.acquire.meta.valid := alloc_arb.io.in(i).ready

    t.acquire.data.bits := acquire.data.bits
    t.acquire.data.valid := acquire.data.valid
  }
  acquire.meta.ready := trackerList.map(_.io.client.acquire.meta.ready).reduce(_||_) && !block_acquires
  acquire.data.ready := trackerList.map(_.io.client.acquire.data.ready).reduce(_||_)
  alloc_arb.io.out.ready := acquire.meta.valid && !block_acquires

  // Handle probe request generation
  val probe_arb = Module(new Arbiter(new LogicalNetworkIO(new Probe), trackerList.size))
  io.client.probe <> probe_arb.io.out
  probe_arb.io.in zip trackerList map { case (arb, t) => arb <> t.io.client.probe }

  // Handle releases, which might be voluntary and might have data
  val release = io.client.release
  val voluntary = co.isVoluntary(release.meta.bits.payload)
  val any_release_conflict = trackerList.tail.map(_.io.has_release_conflict).reduce(_||_)
  val block_releases = Bool(false)
  val conflict_idx = Vec(trackerList.map(_.io.has_release_conflict)).lastIndexWhere{b: Bool => b}
  //val release_idx = Mux(voluntary, Mux(any_release_conflict, conflict_idx, UInt(0)), release.bits.payload.master_xact_id) // TODO: Add merging logic to allow allocated AcquireTracker to handle conflicts, send all necessary grants, use first sufficient response
  val release_idx = Mux(voluntary, UInt(0), release.meta.bits.payload.master_xact_id)
  for( i <- 0 until trackerList.size ) {
    val t = trackerList(i).io.client
    t.release.meta.bits := release.meta.bits 
    t.release.meta.valid := release.meta.valid && (release_idx === UInt(i)) && !block_releases
    t.release.data.bits := release.data.bits
    t.release.data.valid := release.data.valid
  }
  release.meta.ready := Vec(trackerList.map(_.io.client.release.meta.ready)).read(release_idx) && !block_releases
  release.data.ready := trackerList.map(_.io.client.release.data.ready).reduce(_||_)

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


abstract class XactTracker()(implicit conf: L2CoherenceAgentConfiguration) extends Module with OuterRequestGenerator {
  implicit val (tl, ln, co) = (conf.tl, conf.tl.ln, conf.tl.co)
  val io = new Bundle {
    val client = (new TileLinkIO).flip
    val master = new UncachedTileLinkIO
    val tile_incoherent = Bits(INPUT, ln.nClients)
    val has_acquire_conflict = Bool(OUTPUT)
    val has_release_conflict = Bool(OUTPUT)
  }
}

class VoluntaryReleaseTracker(trackerId: Int, bankId: Int)(implicit conf: L2CoherenceAgentConfiguration) extends XactTracker()(conf) {
  val s_idle :: s_mem :: s_ack :: s_busy :: Nil = Enum(UInt(), 4)
  val state = Reg(init=s_idle)
  val xact  = Reg{ new Release }
  val init_client_id_ = Reg(init=UInt(0, width = log2Up(ln.nClients)))
  val release_data_needs_write = Reg(init=Bool(false))
  val mem_cmd_sent = Reg(init=Bool(false))
  val cmd_to_write = Acquire(co.getUncachedWriteAcquireType, xact.addr, UInt(trackerId))

  io.has_acquire_conflict := Bool(false)
  io.has_release_conflict := co.isCoherenceConflict(xact.addr, io.client.release.meta.bits.payload.addr) && (state != s_idle)

  io.master.grant.ready := Bool(false)
  io.master.acquire.meta.valid := Bool(false)
  io.master.acquire.meta.bits.payload := cmd_to_write
  //TODO io.master.acquire.bits.header.dst
  io.master.acquire.meta.bits.header.src := UInt(bankId) 
  io.master.acquire.data.valid := Bool(false)
  io.master.acquire.data.bits.payload.data := UInt(0)
  //TODO io.master.acquire_data.bits.header.dst
  io.master.acquire.data.bits.header.src := UInt(bankId)
  io.client.acquire.meta.ready := Bool(false)
  io.client.acquire.data.ready := Bool(false)
  io.client.probe.valid := Bool(false)
  io.client.release.meta.ready := Bool(false)
  io.client.release.data.ready := Bool(false) // DNC
  io.client.grant.valid := Bool(false)
  io.client.grant.bits.payload.g_type := co.getGrantType(xact, UInt(0))
  io.client.grant.bits.payload.client_xact_id := xact.client_xact_id
  io.client.grant.bits.payload.master_xact_id := UInt(trackerId)
  io.client.grant.bits.payload.data := UInt(0)
  io.client.grant.bits.header.dst := init_client_id_
  io.client.grant.bits.header.src := UInt(bankId)
  io.client.grant_ack.valid := Bool(false)

  switch (state) {
    is(s_idle) {
      io.client.release.meta.ready := Bool(true)
      when( io.client.release.meta.valid ) {
        xact := io.client.release.meta.bits.payload
        init_client_id_ := io.client.release.meta.bits.header.src
        release_data_needs_write := co.messageHasData(io.client.release.meta.bits.payload)
        mem_cnt := UInt(0)
        mem_cmd_sent := Bool(false)
        state := s_mem
      }
    }
    is(s_mem) {
      when (release_data_needs_write) {
        doOuterReqWrite(io.master.acquire,
                      io.client.release.data,
                      cmd_to_write,
                      release_data_needs_write,
                      mem_cmd_sent,
                      init_client_id_)
      } . otherwise { state := s_ack }
    }
    is(s_ack) {
      io.client.grant.valid := Bool(true)
      when(io.client.grant.ready) { state := s_idle }
    }
  }
}

class AcquireTracker(trackerId: Int, bankId: Int)(implicit conf: L2CoherenceAgentConfiguration) extends XactTracker()(conf) {
  val s_idle :: s_ack :: s_mem :: s_probe :: s_busy :: Nil = Enum(UInt(), 5)
  val state = Reg(init=s_idle)
  val xact  = Reg{ new Acquire }
  val init_client_id_ = Reg(init=UInt(0, width = log2Up(ln.nClients)))
  val release_data_client_id = Reg(init=UInt(0, width = log2Up(ln.nClients)))
  //TODO: Will need id reg for merged release xacts
  val init_sharer_cnt_ = Reg(init=UInt(0, width = log2Up(ln.nClients)))
  val grant_type = co.getGrantType(xact.a_type, init_sharer_cnt_)
  val release_count = if (ln.nClients == 1) UInt(0) else Reg(init=UInt(0, width = log2Up(ln.nClients)))
  val probe_flags = Reg(init=Bits(0, width = ln.nClients))
  val curr_p_id = PriorityEncoder(probe_flags)
  val x_needs_read = Reg(init=Bool(false))
  val acquire_data_needs_write = Reg(init=Bool(false))
  val release_data_needs_write = Reg(init=Bool(false))
  val cmd_to_write = Acquire(co.getUncachedWriteAcquireType, xact.addr, UInt(trackerId))
  val cmd_to_read = Acquire(co.getUncachedReadAcquireType, xact.addr, UInt(trackerId))
  val a_w_mem_cmd_sent = Reg(init=Bool(false))
  val r_w_mem_cmd_sent = Reg(init=Bool(false))
  val probe_initial_flags = Bits(width = ln.nClients)
  probe_initial_flags := Bits(0)
  if (ln.nClients > 1) {
    // issue self-probes for uncached read xacts to facilitate I$ coherence
    val probe_self = Bool(true) //co.needsSelfProbe(io.client.acquire.bits.payload)
    val myflag = Mux(probe_self, Bits(0), UIntToOH(io.client.acquire.meta.bits.header.src(log2Up(ln.nClients)-1,0)))
    probe_initial_flags := ~(io.tile_incoherent | myflag)
  }

  io.has_acquire_conflict := co.isCoherenceConflict(xact.addr, io.client.acquire.meta.bits.payload.addr) && (state != s_idle)
  io.has_release_conflict := co.isCoherenceConflict(xact.addr, io.client.release.meta.bits.payload.addr) && (state != s_idle)
  io.master.acquire.meta.valid := Bool(false)
  io.master.acquire.meta.bits.payload := Acquire(co.getUncachedReadAcquireType, xact.addr, UInt(trackerId))
  io.master.acquire.meta.bits.header.src := UInt(bankId)
  io.master.acquire.data.valid := Bool(false)
  io.master.acquire.data.bits.payload.data := UInt(0)
  io.master.acquire.data.bits.header.src := UInt(bankId)
  io.client.probe.valid := Bool(false)
  io.client.probe.bits.payload.p_type := co.getProbeType(xact.a_type, UInt(0))
  io.client.probe.bits.payload.master_xact_id  := UInt(trackerId)
  io.client.probe.bits.payload.addr := xact.addr
  io.client.probe.bits.header.dst := UInt(0)
  io.client.probe.bits.header.src := UInt(bankId)
  io.client.grant.bits.payload.data := io.master.grant.bits.payload.data
  io.client.grant.bits.payload.g_type := grant_type
  io.client.grant.bits.payload.client_xact_id := xact.client_xact_id
  io.client.grant.bits.payload.master_xact_id := UInt(trackerId)
  io.client.grant.bits.header.dst := init_client_id_
  io.client.grant.bits.header.src := UInt(bankId)
  io.client.grant.valid := (io.master.grant.valid && (UInt(trackerId) === io.master.grant.bits.payload.client_xact_id)) 
  io.client.acquire.meta.ready := Bool(false)
  io.client.acquire.data.ready := Bool(false)
  io.client.release.meta.ready := Bool(false)
  io.client.release.data.ready := Bool(false)
  io.master.grant.ready := io.client.grant.ready
  io.client.grant_ack.valid := Bool(false)

  switch (state) {
    is(s_idle) {
      io.client.acquire.meta.ready := Bool(true)
      when( io.client.acquire.meta.valid ) {
        xact := io.client.acquire.meta.bits.payload
        init_client_id_ := io.client.acquire.meta.bits.header.src
        init_sharer_cnt_ := UInt(ln.nClients) // TODO: Broadcast only
        acquire_data_needs_write := co.messageHasData(io.client.acquire.meta.bits.payload)
        x_needs_read := co.needsOuterRead(io.client.acquire.meta.bits.payload.a_type, UInt(0))
        probe_flags := probe_initial_flags
        mem_cnt := UInt(0)
        r_w_mem_cmd_sent := Bool(false)
        a_w_mem_cmd_sent := Bool(false)
        if(ln.nClients > 1) {
          release_count := PopCount(probe_initial_flags)
          state := Mux(probe_initial_flags.orR, s_probe, s_mem)
        } else state := s_mem
      }
    }
    is(s_probe) {
      when(probe_flags.orR) {
        io.client.probe.valid := Bool(true)
        io.client.probe.bits.header.dst := curr_p_id 
      }
      when(io.client.probe.ready) {
        probe_flags := probe_flags & ~(UIntToOH(curr_p_id))
      }
      io.client.release.meta.ready := Bool(true)
      when(io.client.release.meta.valid) {
        if(ln.nClients > 1) release_count := release_count - UInt(1)
        when(release_count === UInt(1)) {
          state := s_mem
        }
        when( co.messageHasData(io.client.release.meta.bits.payload)) {
          release_data_needs_write := Bool(true)
          release_data_client_id := io.client.release.meta.bits.header.src
        }
      }
      when (release_data_needs_write) {
        doOuterReqWrite(io.master.acquire,
                      io.client.release.data, 
                      cmd_to_write,
                      release_data_needs_write, 
                      r_w_mem_cmd_sent, 
                      release_data_client_id)
      }
    }
    is(s_mem) {
      when (release_data_needs_write) {
        doOuterReqWrite(io.master.acquire,
                      io.client.release.data,
                      cmd_to_write,
                      release_data_needs_write,
                      r_w_mem_cmd_sent,
                      release_data_client_id)
      } . elsewhen(acquire_data_needs_write) {
        doOuterReqWrite(io.master.acquire,
                      io.client.acquire.data,
                      cmd_to_write,
                      acquire_data_needs_write,
                      a_w_mem_cmd_sent,
                      init_client_id_)
      } . elsewhen (x_needs_read) {    
        doOuterReqRead(io.master.acquire, cmd_to_read, x_needs_read)
      } . otherwise { 
        state := Mux(co.requiresDatalessGrant(xact.a_type, UInt(0)), s_ack, 
                  Mux(co.requiresAckForGrant(io.client.grant.bits.payload.g_type), s_busy, s_idle))
      }
    }
    is(s_ack) {
      io.client.grant.valid := Bool(true)
      when(io.client.grant.ready) { state := Mux(co.requiresAckForGrant(io.client.grant.bits.payload.g_type), s_busy, s_idle) }
    }
    is(s_busy) { // Nothing left to do but wait for transaction to complete
      when (io.client.grant_ack.valid && io.client.grant_ack.bits.payload.master_xact_id === UInt(trackerId)) {
        state := s_idle
      }
    }
  }
}

abstract trait OuterRequestGenerator {
  val mem_cnt = Reg(init=UInt(0, width = log2Up(REFILL_CYCLES)))
  val mem_cnt_next = mem_cnt + UInt(1)

  def doOuterReqWrite[T <: HasTileLinkData](master_acq: PairedDataIO[LogicalNetworkIO[Acquire],LogicalNetworkIO[AcquireData]], client_data: DecoupledIO[LogicalNetworkIO[T]], cmd: Acquire, trigger: Bool, cmd_sent: Bool, desired_client_data_src_id: UInt) {
    val do_write = client_data.valid && (client_data.bits.header.src === desired_client_data_src_id)
    master_acq.meta.bits.payload := cmd
    master_acq.data.bits.payload := client_data.bits.payload
    when(master_acq.meta.fire()) {
      cmd_sent := Bool(true)
    }
    when (do_write) {
      master_acq.meta.valid := !cmd_sent
      when (master_acq.meta.ready || cmd_sent) {
        master_acq.data.valid := client_data.valid
        when(master_acq.data.ready) {
          client_data.ready:= Bool(true)
          mem_cnt  := mem_cnt_next
          when(mem_cnt === UInt(REFILL_CYCLES-1)) {
            trigger := Bool(false)
          }
        }
      }
    }
  }

  def doOuterReqRead(master_acq: PairedDataIO[LogicalNetworkIO[Acquire],LogicalNetworkIO[AcquireData]], cmd: Acquire, trigger: Bool) {
    master_acq.meta.valid := Bool(true)
    master_acq.meta.bits.payload := cmd
    when(master_acq.meta.ready) {
      trigger := Bool(false)
    }
  }
}
