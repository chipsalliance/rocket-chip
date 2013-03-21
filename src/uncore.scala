package uncore

import Chisel._
import Constants._

class TrackerProbeData(implicit conf: UncoreConfiguration) extends Bundle {
  val client_id = Bits(width = conf.ln.idBits)
}

class TrackerAllocReq(implicit conf: UncoreConfiguration) extends Bundle {
  val acquire = new Acquire()
  val client_id = Bits(width = conf.ln.idBits)
  override def clone = { new TrackerAllocReq().asInstanceOf[this.type] }
}

class TrackerDependency extends Bundle {
  val master_xact_id = Bits(width = MASTER_XACT_ID_MAX_BITS)
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
  val trackerList = new WritebackTracker(0, bankId) +: (1 to NGLOBAL_XACTS).map(new AcquireTracker(_, bankId))
  val release_data_dep_q = (new Queue(NGLOBAL_XACTS)){new TrackerDependency} // depth must >= NPRIMARY
  val acquire_data_dep_q = (new Queue(NGLOBAL_XACTS)){new TrackerDependency} // depth should >= NPRIMARY
  
  trackerList.map(_.io.tile_incoherent := io.incoherent.toBits)

  // Handle transaction initiation requests
  // Only one allocation per cycle
  // Init requests may or may not have data
  val acquire = io.client.acquire
  val acquire_data = io.client.acquire_data
  val acq_dep_deq = acquire_data_dep_q.io.deq
  val any_acquire_conflict = trackerList.map(_.io.has_acquire_conflict).reduce(_||_)
  val all_busy = trackerList.map(_.io.busy).reduce(_&&_)

  val alloc_arb = (new Arbiter(trackerList.size)) { Bool() }
  for( i <- 0 until trackerList.size ) {
    alloc_arb.io.in(i).valid := !trackerList(i).io.busy
    val t = trackerList(i).io.client
    t.acquire.bits := acquire.bits
    t.acquire.valid := acquire.valid && alloc_arb.io.in(i).ready

    t.acquire_data.bits := acquire_data.bits
    t.acquire_data.valid := acquire_data.valid
    trackerList(i).io.acquire_data_dep.bits := acq_dep_deq.bits
    trackerList(i).io.acquire_data_dep.valid := acq_dep_deq.valid
  }
  acquire.ready := trackerList.map(_.io.client.acquire.ready).reduce(_||_)
  acquire_data.ready := trackerList.map(_.io.client.acquire_data.ready).reduce(_||_)
  acquire_data_dep_q.io.enq.valid := acquire.ready && co.messageHasData(acquire.bits.payload)
  acquire_data_dep_q.io.enq.bits.master_xact_id := OHToUFix(trackerList.map(_.io.client.acquire.ready))
  acq_dep_deq.ready := trackerList.map(_.io.acquire_data_dep.ready).reduce(_||_)
  alloc_arb.io.out.ready := acquire.valid

  // Handle probe request generation
  val probe_arb = (new Arbiter(trackerList.size)){(new LogicalNetworkIO){ new Probe }}
  io.client.probe <> probe_arb.io.out
  probe_arb.io.in zip trackerList map { case (arb, t) => arb <> t.io.client.probe }

  // Handle probe replies, which may or may not have data
  val release = io.client.release
  val release_data = io.client.release_data
  val voluntary = co.isVoluntary(release.bits.payload)
  val any_release_conflict = trackerList.tail.map(_.io.has_release_conflict).reduce(_||_)
  val conflict_idx = Vec(trackerList.map(_.io.has_release_conflict)){Bool()}.lastIndexWhere{b: Bool => b}
  val idx = Mux(voluntary, Mux(any_release_conflict, conflict_idx, UFix(0)), release.bits.payload.master_xact_id)
  release.ready := trackerList.map(_.io.client.release.ready).reduce(_||_)
  release_data.ready := trackerList.map(_.io.client.release_data.ready).reduce(_||_)
  release_data_dep_q.io.enq.valid := release.valid && co.messageHasData(release.bits.payload)
  release_data_dep_q.io.enq.bits.master_xact_id := idx
  release_data_dep_q.io.deq.ready := trackerList.map(_.io.release_data_dep.ready).reduce(_||_)
  for( i <- 0 until trackerList.size ) {
    val t = trackerList(i).io.client
    t.release.valid := release.valid && (idx === UFix(i))
    t.release.bits := release.bits 
    t.release_data.valid := release_data.valid
    t.release_data.bits := release_data.bits
    trackerList(i).io.release_data_dep.valid := release_data_dep_q.io.deq.valid
    trackerList(i).io.release_data_dep.bits := release_data_dep_q.io.deq.bits
  }

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
  val outer_arb = new UncachedTileLinkIOArbiter(trackerList.size)
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
    val release_data_dep = (new FIFOIO) { new TrackerDependency }.flip
    val acquire_data_dep = (new FIFOIO) { new TrackerDependency }.flip

    val busy = Bool(OUTPUT)
    val has_acquire_conflict = Bool(OUTPUT)
    val has_release_conflict = Bool(OUTPUT)
  }
}

class WritebackTracker(trackerId: Int, bankId: Int)(implicit conf: UncoreConfiguration) extends XactTracker()(conf) {
  val s_idle :: s_mem :: s_ack :: s_busy :: Nil = Enum(4){ UFix() }
  val state = Reg(resetVal = s_idle)
  val xact  = Reg{ new Release }
  val init_client_id_ = Reg(resetVal = UFix(0, width = log2Up(conf.ln.nClients)))
  val release_data_needs_write = Reg(resetVal = Bool(false))
  val mem_cmd_sent = Reg(resetVal = Bool(false))
  val cmd_to_write = co.getUncachedWriteAcquire(xact.addr, UFix(trackerId))
  val cmd_to_read = co.getUncachedReadAcquire(xact.addr, UFix(trackerId))

  io.acquire_data_dep.ready := Bool(false)
  io.release_data_dep.ready := Bool(false)  
  io.busy := Bool(true)
  io.has_acquire_conflict := Bool(false)
  io.has_release_conflict := co.isCoherenceConflict(xact.addr, io.client.release.bits.payload.addr) && (state != s_idle)

  io.master.grant.ready := Bool(false)
  io.master.acquire.valid := Bool(false)
  io.master.acquire.bits.payload := cmd_to_write
  //TODO io.master.acquire.bits.header.dst
  io.master.acquire.bits.header.src := UFix(bankId) 
  io.master.acquire_data.valid := Bool(false)
  io.master.acquire_data.bits.payload.data := UFix(0)
  //TODO io.master.acquire_data.bits.header.dst
  io.master.acquire_data.bits.header.src := UFix(bankId)
  io.client.acquire.ready := Bool(false)
  io.client.acquire_data.ready := Bool(false)
  io.client.probe.valid := Bool(false)
  io.client.release.ready := Bool(false)
  io.client.release_data.ready := Bool(false)
  io.client.grant.valid := Bool(false)
  io.client.grant.bits.payload.g_type := co.getGrantType(xact, UFix(0))
  io.client.grant.bits.payload.client_xact_id := xact.client_xact_id
  io.client.grant.bits.payload.master_xact_id := UFix(trackerId)
  io.client.grant.bits.header.dst := init_client_id_
  io.client.grant.bits.header.src := UFix(bankId)
  io.client.grant_ack.valid := Bool(false)

  switch (state) {
    is(s_idle) {
      when( io.client.release.valid ) {
        xact := io.client.release.bits.payload
        init_client_id_ := io.client.release.bits.header.src
        release_data_needs_write := co.messageHasData(io.client.release.bits.payload)
        mem_cnt := UFix(0)
        mem_cmd_sent := Bool(false)
        io.client.release.ready := Bool(true)
        state := s_mem
      }
    }
    is(s_mem) {
      when (release_data_needs_write) {
        doOuterReqWrite(io.master.acquire, 
                      io.master.acquire_data, 
                      io.client.release_data, 
                      release_data_needs_write, 
                      mem_cmd_sent, 
                      io.release_data_dep.ready, 
                      io.release_data_dep.valid && (io.release_data_dep.bits.master_xact_id === UFix(trackerId)))
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
  //TODO: Will need id reg for merged release xacts
  val init_sharer_cnt_ = Reg(resetVal = UFix(0, width = log2Up(conf.ln.nClients)))
  val grant_type = co.getGrantType(xact.a_type, init_sharer_cnt_)
  val release_count = if (conf.ln.nClients == 1) UFix(0) else Reg(resetVal = UFix(0, width = log2Up(conf.ln.nClients)))
  val probe_flags = Reg(resetVal = Bits(0, width = conf.ln.nClients))
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
    val probe_self = co.needsSelfProbe(io.client.acquire.bits.payload)
    val myflag = Mux(probe_self, Bits(0), UFixToOH(io.client.acquire.bits.header.src(log2Up(conf.ln.nClients)-1,0)))
    probe_initial_flags := ~(io.tile_incoherent | myflag)
  }

  io.busy := state != s_idle
  io.has_acquire_conflict := co.isCoherenceConflict(xact.addr, io.client.acquire.bits.payload.addr) && (state != s_idle)
  io.has_release_conflict := co.isCoherenceConflict(xact.addr, io.client.release.bits.payload.addr) && (state != s_idle)
  io.master.acquire.valid := Bool(false)
  io.master.acquire.bits.payload := co.getUncachedReadAcquire(xact.addr, UFix(trackerId))
  //TODO io.master.acquire.bits.header.dst
  io.master.acquire.bits.header.src := UFix(bankId)
  io.master.acquire_data.valid := Bool(false)
  io.master.acquire_data.bits.payload.data := UFix(0)
  //TODO io.master.acquire_data.bits.header.dst
  io.master.acquire_data.bits.header := UFix(bankId)
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
  io.client.acquire.ready := Bool(false)
  io.client.acquire_data.ready := Bool(false)
  io.acquire_data_dep.ready := Bool(false)  
  io.client.release.ready := Bool(false)
  io.client.release_data.ready := Bool(false)
  io.release_data_dep.ready := Bool(false)  
  io.master.grant.ready := io.client.grant.ready
  io.client.grant_ack.valid := Bool(false)

  switch (state) {
    is(s_idle) {
      when( io.client.acquire.valid ) {
        xact := io.client.acquire.bits.payload
        init_client_id_ := io.client.acquire.bits.header.src
        init_sharer_cnt_ := UFix(conf.ln.nClients) // TODO: Broadcast only
        acquire_data_needs_write := co.messageHasData(io.client.acquire.bits.payload)
        x_needs_read := co.needsOuterRead(io.client.acquire.bits.payload.a_type, UFix(0))
        probe_flags := probe_initial_flags
        mem_cnt := UFix(0)
        r_w_mem_cmd_sent := Bool(false)
        a_w_mem_cmd_sent := Bool(false)
        io.client.acquire.ready := Bool(true)
        if(conf.ln.nClients > 1) {
          release_count := PopCount(probe_initial_flags)
          state := Mux(probe_initial_flags.orR, s_probe, s_mem)
        } else state := s_mem
      }
    }
    is(s_probe) {
      val curr_p_id = PriorityEncoder(probe_flags)
      when(probe_flags.orR) {
        io.client.probe.valid := Bool(true)
        io.client.probe.bits.header.dst := curr_p_id 
      }
      when(io.client.probe.ready) {
        probe_flags := probe_flags & ~(UFixToOH(curr_p_id))
      }
      when(io.client.release.valid) {
        io.client.release.ready := Bool(true)
        if(conf.ln.nClients > 1) release_count := release_count - UFix(1)
        when(release_count === UFix(1)) {
          state := s_mem
        }
        release_data_needs_write := co.messageHasData(io.client.release.bits.payload)
      }
    }
    is(s_mem) {
      when (release_data_needs_write) {
        doOuterReqWrite(io.master.acquire, 
                      io.master.acquire_data, 
                      io.client.release_data, 
                      release_data_needs_write, 
                      r_w_mem_cmd_sent, 
                      io.release_data_dep.ready, 
                      io.release_data_dep.valid && (io.release_data_dep.bits.master_xact_id === UFix(trackerId)))
      } . elsewhen(acquire_data_needs_write) {
        doOuterReqWrite(io.master.acquire, 
                      io.master.acquire_data, 
                      io.client.acquire_data, 
                      acquire_data_needs_write, 
                      a_w_mem_cmd_sent, 
                      io.acquire_data_dep.ready,
                      io.acquire_data_dep.valid && (io.acquire_data_dep.bits.master_xact_id === UFix(trackerId)))
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

  def doOuterReqWrite[T <: Data](master_acq: FIFOIO[LogicalNetworkIO[Acquire]], master_acq_data: FIFOIO[LogicalNetworkIO[AcquireData]], client_data: FIFOIO[LogicalNetworkIO[T]], trigger: Bool, cmd_sent: Bool, pop_dep: Bool, at_front_of_dep_queue: Bool) {
    master_acq.bits.payload := cmd_to_write
    master_acq_data.bits.payload := client_data.bits.payload
    when(master_acq.ready && master_acq.valid) {
      cmd_sent := Bool(true)
    }
    when (at_front_of_dep_queue) {
      master_acq.valid := !cmd_sent && master_acq_data.ready && client_data.valid
      when (master_acq.ready || cmd_sent) {
        master_acq_data.valid := client_data.valid
        when(master_acq_data.ready) {
          client_data.ready:= Bool(true)
          when (client_data.valid) {
            mem_cnt  := mem_cnt_next
            when(mem_cnt === UFix(REFILL_CYCLES-1)) {
              pop_dep := Bool(true)
              trigger := Bool(false)
            }
          }
        }
      }
    }
  }

  def doOuterReqRead(master_acq: FIFOIO[LogicalNetworkIO[Acquire]], trigger: Bool) {
    master_acq.valid := Bool(true)
    master_acq.bits.payload := cmd_to_read
    when(master_acq.ready) {
      trigger := Bool(false)
    }
  }
}
