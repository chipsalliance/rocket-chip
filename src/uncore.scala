package uncore

import Chisel._
import Constants._

class TrackerDependency(implicit conf: UncoreConfiguration)  extends Bundle {
  val tracker_id = Bits(width = MASTER_XACT_ID_MAX_BITS)
  val data_src_id = Bits(width = conf.ln.idBits)
  override def clone = { new TrackerDependency().asInstanceOf[this.type] }
}

case class UncoreConfiguration(co: CoherencePolicyWithUncached, ln: LogicalNetworkConfiguration)

abstract class CoherenceAgent(implicit conf: LogicalNetworkConfiguration) extends Component with MasterCoherenceAgent {
  val io = new Bundle {
    val client = (new TileLinkIO).flip
    val master = new UncachedTileLinkIO
    val incoherent = Vec(conf.nClients) { Bool() }.asInput
  }
}

class L2CoherenceAgent(bankId: Int)(implicit conf: UncoreConfiguration) extends CoherenceAgent()(conf.ln)
{
  implicit val lnConf = conf.ln
  val co = conf.co
  require(conf.ln.nClients < NGLOBAL_REL_XACTS) //TODO: handle in config
  val trackerList = (0 until NGLOBAL_REL_XACTS).map(new VoluntaryReleaseTracker(_, bankId)) ++ 
                    (NGLOBAL_REL_XACTS until NGLOBAL_REL_XACTS + NGLOBAL_ACQ_XACTS).map(new AcquireTracker(_, bankId))
  
  trackerList.map(_.io.tile_incoherent := io.incoherent.toBits)

  // Handle acquire transaction initiation
  val acquire = io.client.acquire
  val any_acquire_conflict = trackerList.map(_.io.has_acquire_conflict).reduce(_||_)
  val block_acquires = any_acquire_conflict

  val alloc_arb = (new Arbiter(trackerList.size)) { Bool() }
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
  val probe_arb = (new Arbiter(trackerList.size)){(new LogicalNetworkIO){ new Probe }}
  io.client.probe <> probe_arb.io.out
  probe_arb.io.in zip trackerList map { case (arb, t) => arb <> t.io.client.probe }

  // Handle releases, which might be voluntary and might have data
  val release = io.client.release
  val voluntary = co.isVoluntary(release.meta.bits.payload)
  val any_release_conflict = trackerList.tail.map(_.io.has_release_conflict).reduce(_||_)
  val block_releases = Bool(false)
  val conflict_idx = Vec(trackerList.map(_.io.has_release_conflict)){Bool()}.lastIndexWhere{b: Bool => b}
  //val release_idx = Mux(voluntary, Mux(any_release_conflict, conflict_idx, UFix(0)), release.bits.payload.master_xact_id) // TODO: Add merging logic to allow allocated AcquireTracker to handle conflicts, send all necessary grants, use first sufficient response
  val release_idx = Mux(voluntary, release.meta.bits.header.src, release.meta.bits.payload.master_xact_id)
  for( i <- 0 until trackerList.size ) {
    val t = trackerList(i).io.client
    t.release.meta.bits := release.meta.bits 
    t.release.meta.valid := release.meta.valid && (release_idx === UFix(i)) && !block_releases
    t.release.data.bits := release.data.bits
    t.release.data.valid := release.data.valid
  }
  release.meta.ready := Vec(trackerList.map(_.io.client.release.meta.ready)){Bool()}(release_idx) && !block_releases
  release.data.ready := trackerList.map(_.io.client.release.data.ready).reduce(_||_)

  // Reply to initial requestor
  val grant_arb = (new Arbiter(trackerList.size)){(new LogicalNetworkIO){ new Grant }}
  io.client.grant <> grant_arb.io.out
  grant_arb.io.in zip trackerList map { case (arb, t) => arb <> t.io.client.grant }

  // Free finished transactions
  val ack = io.client.grant_ack
  trackerList.map(_.io.client.grant_ack.valid := ack.valid)
  trackerList.map(_.io.client.grant_ack.bits := ack.bits)
  ack.ready := Bool(true)

  // Create an arbiter for the one memory port
  val outer_arb = new UncachedTileLinkIOArbiter(trackerList.size, conf.co)
  outer_arb.io.in zip  trackerList map { case(arb, t) => arb <> t.io.master }
  io.master <> outer_arb.io.out
}


abstract class XactTracker()(implicit conf: UncoreConfiguration) extends Component with OuterRequestGenerator {
  val co = conf.co
  implicit val ln = conf.ln
  val io = new Bundle {
    val client = (new TileLinkIO).flip
    val master = new UncachedTileLinkIO
    val tile_incoherent = Bits(INPUT, conf.ln.nClients)
    val has_acquire_conflict = Bool(OUTPUT)
    val has_release_conflict = Bool(OUTPUT)
  }
}

class VoluntaryReleaseTracker(trackerId: Int, bankId: Int)(implicit conf: UncoreConfiguration) extends XactTracker()(conf) {
  val s_idle :: s_mem :: s_ack :: s_busy :: Nil = Enum(4){ UFix() }
  val state = Reg(resetVal = s_idle)
  val xact  = Reg{ new Release }
  val init_client_id_ = Reg(resetVal = UFix(0, width = log2Up(conf.ln.nClients)))
  val release_data_needs_write = Reg(resetVal = Bool(false))
  val mem_cmd_sent = Reg(resetVal = Bool(false))
  val cmd_to_write = co.getUncachedWriteAcquire(xact.addr, UFix(trackerId))
  val cmd_to_read = co.getUncachedReadAcquire(xact.addr, UFix(trackerId))

  io.has_acquire_conflict := Bool(false)
  io.has_release_conflict := co.isCoherenceConflict(xact.addr, io.client.release.meta.bits.payload.addr) && (state != s_idle)

  io.master.grant.ready := Bool(false)
  io.master.acquire.meta.valid := Bool(false)
  io.master.acquire.meta.bits.payload := cmd_to_write
  //TODO io.master.acquire.bits.header.dst
  io.master.acquire.meta.bits.header.src := UFix(bankId) 
  io.master.acquire.data.valid := Bool(false)
  io.master.acquire.data.bits.payload.data := UFix(0)
  //TODO io.master.acquire_data.bits.header.dst
  io.master.acquire.data.bits.header.src := UFix(bankId)
  io.client.acquire.meta.ready := Bool(false)
  io.client.acquire.data.ready := Bool(false)
  io.client.probe.valid := Bool(false)
  io.client.release.meta.ready := Bool(false)
  io.client.release.data.ready := Bool(false) // DNC
  io.client.grant.valid := Bool(false)
  io.client.grant.bits.payload.g_type := co.getGrantType(xact, UFix(0))
  io.client.grant.bits.payload.client_xact_id := xact.client_xact_id
  io.client.grant.bits.payload.master_xact_id := UFix(trackerId)
  io.client.grant.bits.payload.data := UFix(0)
  io.client.grant.bits.header.dst := init_client_id_
  io.client.grant.bits.header.src := UFix(bankId)
  io.client.grant_ack.valid := Bool(false)

  switch (state) {
    is(s_idle) {
      io.client.release.meta.ready := Bool(true)
      when( io.client.release.meta.valid ) {
        xact := io.client.release.meta.bits.payload
        init_client_id_ := io.client.release.meta.bits.header.src
        release_data_needs_write := co.messageHasData(io.client.release.meta.bits.payload)
        mem_cnt := UFix(0)
        mem_cmd_sent := Bool(false)
        state := s_mem
      }
    }
    is(s_mem) {
      when (release_data_needs_write) {
        doOuterReqWrite(io.master.acquire,
                      io.client.release.data,
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

class AcquireTracker(trackerId: Int, bankId: Int)(implicit conf: UncoreConfiguration) extends XactTracker()(conf) {
  val s_idle :: s_ack :: s_mem :: s_probe :: s_busy :: Nil = Enum(5){ UFix() }
  val state = Reg(resetVal = s_idle)
  val xact  = Reg{ new Acquire }
  val init_client_id_ = Reg(resetVal = UFix(0, width = log2Up(conf.ln.nClients)))
  val release_data_client_id = Reg(resetVal = UFix(0, width = log2Up(conf.ln.nClients)))
  //TODO: Will need id reg for merged release xacts
  val init_sharer_cnt_ = Reg(resetVal = UFix(0, width = log2Up(conf.ln.nClients)))
  val grant_type = co.getGrantType(xact.a_type, init_sharer_cnt_)
  val release_count = if (conf.ln.nClients == 1) UFix(0) else Reg(resetVal = UFix(0, width = log2Up(conf.ln.nClients)))
  val probe_flags = Reg(resetVal = Bits(0, width = conf.ln.nClients))
  val curr_p_id = PriorityEncoder(probe_flags)
  val x_needs_read = Reg(resetVal = Bool(false))
  val acquire_data_needs_write = Reg(resetVal = Bool(false))
  val release_data_needs_write = Reg(resetVal = Bool(false))
  val cmd_to_write = co.getUncachedWriteAcquire(xact.addr, UFix(trackerId))
  val cmd_to_read = co.getUncachedReadAcquire(xact.addr, UFix(trackerId))
  val a_w_mem_cmd_sent = Reg(resetVal = Bool(false))
  val r_w_mem_cmd_sent = Reg(resetVal = Bool(false))
  val probe_initial_flags = Bits(width = conf.ln.nClients)
  probe_initial_flags := Bits(0)
  if (conf.ln.nClients > 1) {
    // issue self-probes for uncached read xacts to facilitate I$ coherence
    val probe_self = Bool(true) //co.needsSelfProbe(io.client.acquire.bits.payload)
    val myflag = Mux(probe_self, Bits(0), UFixToOH(io.client.acquire.meta.bits.header.src(log2Up(conf.ln.nClients)-1,0)))
    probe_initial_flags := ~(io.tile_incoherent | myflag)
  }

  io.has_acquire_conflict := co.isCoherenceConflict(xact.addr, io.client.acquire.meta.bits.payload.addr) && (state != s_idle)
  io.has_release_conflict := co.isCoherenceConflict(xact.addr, io.client.release.meta.bits.payload.addr) && (state != s_idle)
  io.master.acquire.meta.valid := Bool(false)
  io.master.acquire.meta.bits.payload := co.getUncachedReadAcquire(xact.addr, UFix(trackerId))
  //TODO io.master.acquire.bits.header.dst
  io.master.acquire.meta.bits.header.src := UFix(bankId)
  io.master.acquire.data.valid := Bool(false)
  io.master.acquire.data.bits.payload.data := UFix(0)
  //TODO io.master.acquire_data.bits.header.dst
  io.master.acquire.data.bits.header := UFix(bankId)
  io.client.probe.valid := Bool(false)
  io.client.probe.bits.payload.p_type := co.getProbeType(xact.a_type, UFix(0))
  io.client.probe.bits.payload.master_xact_id  := UFix(trackerId)
  io.client.probe.bits.payload.addr := xact.addr
  io.client.probe.bits.header.dst := UFix(0)
  io.client.probe.bits.header.src := UFix(bankId)
  io.client.grant.bits.payload.data := io.master.grant.bits.payload.data
  io.client.grant.bits.payload.g_type := grant_type
  io.client.grant.bits.payload.client_xact_id := xact.client_xact_id
  io.client.grant.bits.payload.master_xact_id := UFix(trackerId)
  io.client.grant.bits.header.dst := init_client_id_
  io.client.grant.bits.header.src := UFix(bankId)
  io.client.grant.valid := (io.master.grant.valid && (UFix(trackerId) === io.master.grant.bits.payload.client_xact_id)) 
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
        init_sharer_cnt_ := UFix(conf.ln.nClients) // TODO: Broadcast only
        acquire_data_needs_write := co.messageHasData(io.client.acquire.meta.bits.payload)
        x_needs_read := co.needsOuterRead(io.client.acquire.meta.bits.payload.a_type, UFix(0))
        probe_flags := probe_initial_flags
        mem_cnt := UFix(0)
        r_w_mem_cmd_sent := Bool(false)
        a_w_mem_cmd_sent := Bool(false)
        if(conf.ln.nClients > 1) {
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
        probe_flags := probe_flags & ~(UFixToOH(curr_p_id))
      }
      io.client.release.meta.ready := Bool(true)
      when(io.client.release.meta.valid) {
        if(conf.ln.nClients > 1) release_count := release_count - UFix(1)
        when(release_count === UFix(1)) {
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
                      release_data_needs_write, 
                      r_w_mem_cmd_sent, 
                      release_data_client_id)
      }
    }
    is(s_mem) {
      when (release_data_needs_write) {
        doOuterReqWrite(io.master.acquire,
                      io.client.release.data,
                      release_data_needs_write,
                      r_w_mem_cmd_sent,
                      release_data_client_id)
      } . elsewhen(acquire_data_needs_write) {
        doOuterReqWrite(io.master.acquire,
                      io.client.acquire.data,
                      acquire_data_needs_write,
                      a_w_mem_cmd_sent,
                      init_client_id_)
      } . elsewhen (x_needs_read) {    
        doOuterReqRead(io.master.acquire, x_needs_read)
      } . otherwise { 
        state := Mux(co.needsAckReply(xact.a_type, UFix(0)), s_ack, 
                  Mux(co.requiresAck(io.client.grant.bits.payload), s_busy, s_idle))
      }
    }
    is(s_ack) {
      io.client.grant.valid := Bool(true)
      when(io.client.grant.ready) { state := Mux(co.requiresAck(io.client.grant.bits.payload), s_busy, s_idle) }
    }
    is(s_busy) { // Nothing left to do but wait for transaction to complete
      when (io.client.grant_ack.valid && io.client.grant_ack.bits.payload.master_xact_id === UFix(trackerId)) {
        state := s_idle
      }
    }
  }
}

abstract trait OuterRequestGenerator {
  val cmd_to_write: Acquire
  val cmd_to_read: Acquire
  val mem_cnt = Reg(resetVal = UFix(0, width = log2Up(REFILL_CYCLES)))
  val mem_cnt_next = mem_cnt + UFix(1)

  def doOuterReqWrite[T <: HasMemData](master_acq: PairedDataIO[LogicalNetworkIO[Acquire],LogicalNetworkIO[AcquireData]], client_data: FIFOIO[LogicalNetworkIO[T]], trigger: Bool, cmd_sent: Bool, desired_client_data_src_id: UFix) {
    val do_write = client_data.valid && (client_data.bits.header.src === desired_client_data_src_id)
    master_acq.meta.bits.payload := cmd_to_write
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
          when(mem_cnt === UFix(REFILL_CYCLES-1)) {
            trigger := Bool(false)
          }
        }
      }
    }
  }

  def doOuterReqRead(master_acq: PairedDataIO[LogicalNetworkIO[Acquire],LogicalNetworkIO[AcquireData]], trigger: Bool) {
    master_acq.meta.valid := Bool(true)
    master_acq.meta.bits.payload := cmd_to_read
    when(master_acq.meta.ready) {
      trigger := Bool(false)
    }
  }
}
