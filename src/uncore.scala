package uncore

import Chisel._
import Constants._

class TrackerProbeData(implicit conf: UncoreConfiguration) extends Bundle {
  val tile_id = Bits(width = conf.tile_id_bits)
}

class TrackerAllocReq(implicit conf: UncoreConfiguration) extends Bundle {
  val xact_init = new TransactionInit()
  val tile_id = Bits(width = conf.tile_id_bits)
  override def clone = { new TrackerAllocReq().asInstanceOf[this.type] }
}

class TrackerDependency extends Bundle {
  val global_xact_id = Bits(width = GLOBAL_XACT_ID_BITS)
}

class XactTracker(id: Int)(implicit conf: UncoreConfiguration) extends Component {
  val co = conf.co
  val io = new Bundle {
    val alloc_req       = (new FIFOIO) { new TrackerAllocReq }.flip
    val p_data          = (new PipeIO) { new TrackerProbeData }.flip
    val can_alloc       = Bool(INPUT)
    val xact_finish     = Bool(INPUT)
    val p_rep_cnt_dec   = Bits(INPUT, conf.ntiles)
    val p_req_cnt_inc   = Bits(INPUT, conf.ntiles)
    val tile_incoherent = Bits(INPUT, conf.ntiles)
    val p_rep_data      = (new PipeIO) { new ProbeReplyData }.flip
    val x_init_data     = (new PipeIO) { new TransactionInitData }.flip
    val sent_x_rep_ack  = Bool(INPUT)
    val p_rep_data_dep  = (new PipeIO) { new TrackerDependency }.flip
    val x_init_data_dep = (new PipeIO) { new TrackerDependency }.flip

    val mem_req_cmd     = (new FIFOIO) { new MemReqCmd }
    val mem_req_data    = (new FIFOIO) { new MemData }
    val mem_req_lock    = Bool(OUTPUT)
    val probe_req       = (new FIFOIO) { new ProbeRequest }
    val busy            = Bool(OUTPUT)
    val addr            = Bits(OUTPUT, PADDR_BITS - OFFSET_BITS)
    val init_tile_id    = Bits(OUTPUT, conf.tile_id_bits)
    val p_rep_tile_id   = Bits(OUTPUT, conf.tile_id_bits)
    val tile_xact_id    = Bits(OUTPUT, TILE_XACT_ID_BITS)
    val sharer_count    = Bits(OUTPUT, conf.tile_id_bits+1)
    val x_type          = Bits(OUTPUT, X_INIT_TYPE_MAX_BITS)
    val push_p_req      = Bits(OUTPUT, conf.ntiles)
    val pop_p_rep       = Bits(OUTPUT, conf.ntiles)
    val pop_p_rep_data  = Bits(OUTPUT, conf.ntiles)
    val pop_p_rep_dep   = Bits(OUTPUT, conf.ntiles)
    val pop_x_init      = Bits(OUTPUT, conf.ntiles)
    val pop_x_init_data = Bits(OUTPUT, conf.ntiles)
    val pop_x_init_dep  = Bits(OUTPUT, conf.ntiles)
    val send_x_rep_ack  = Bool(OUTPUT)
  }

  def doMemReqWrite(req_cmd: FIFOIO[MemReqCmd], req_data: FIFOIO[MemData], lock: Bool,  data: PipeIO[MemData], trigger: Bool, cmd_sent: Bool, pop_data: Bits, pop_dep: Bits, at_front_of_dep_queue: Bool, tile_id: UFix) {
    req_cmd.bits.rw := Bool(true)
    req_data.bits := data.bits
    when(req_cmd.ready && req_cmd.valid) {
      cmd_sent := Bool(true)
    }
    when (at_front_of_dep_queue) {
      req_cmd.valid := !cmd_sent && req_data.ready && data.valid
      lock := data.valid || cmd_sent
      when (req_cmd.ready || cmd_sent) {
        req_data.valid := data.valid
        when(req_data.ready) {
          pop_data := UFix(1) << tile_id
          when (data.valid) {
            mem_cnt  := mem_cnt_next
            when(mem_cnt === UFix(REFILL_CYCLES-1)) {
              pop_dep := UFix(1) << tile_id
              trigger := Bool(false)
            }
          }
        }
      }
    }
  }

  def doMemReqRead(req_cmd: FIFOIO[MemReqCmd], trigger: Bool) {
    req_cmd.valid := Bool(true)
    req_cmd.bits.rw := Bool(false)
    when(req_cmd.ready) {
      trigger := Bool(false)
    }
  }

  val s_idle :: s_ack :: s_mem :: s_probe :: s_busy :: Nil = Enum(5){ UFix() }
  val state = Reg(resetVal = s_idle)
  val addr_  = Reg{ UFix() }
  val x_type_ = Reg{ Bits() }
  val init_tile_id_ = Reg{ Bits() }
  val tile_xact_id_ = Reg{ Bits() }
  val p_rep_count = if (conf.ntiles == 1) UFix(0) else Reg(resetVal = UFix(0, width = log2Up(conf.ntiles)))
  val p_req_flags = Reg(resetVal = Bits(0, width = conf.ntiles))
  val p_rep_tile_id_ = Reg{ Bits() }
  val x_needs_read = Reg(resetVal = Bool(false))
  val x_init_data_needs_write = Reg(resetVal = Bool(false))
  val p_rep_data_needs_write = Reg(resetVal = Bool(false))
  val x_w_mem_cmd_sent = Reg(resetVal = Bool(false))
  val p_w_mem_cmd_sent = Reg(resetVal = Bool(false))
  val mem_cnt = Reg(resetVal = UFix(0, width = log2Up(REFILL_CYCLES)))
  val mem_cnt_next = mem_cnt + UFix(1)
  val mem_cnt_max = ~UFix(0, width = log2Up(REFILL_CYCLES))
  val p_req_initial_flags = Bits(width = conf.ntiles)
  p_req_initial_flags := (if (conf.ntiles == 1) Bits(0) else ~(io.tile_incoherent | UFixToOH(io.alloc_req.bits.tile_id(log2Up(conf.ntiles)-1,0)))) //TODO: Broadcast only

  io.busy := state != s_idle
  io.addr := addr_
  io.init_tile_id := init_tile_id_
  io.p_rep_tile_id := p_rep_tile_id_
  io.tile_xact_id := tile_xact_id_
  io.sharer_count := UFix(conf.ntiles) // TODO: Broadcast only
  io.x_type := x_type_

  io.mem_req_cmd.valid := Bool(false)
  io.mem_req_cmd.bits.rw := Bool(false)
  io.mem_req_cmd.bits.addr := addr_
  io.mem_req_cmd.bits.tag := UFix(id)
  io.mem_req_data.valid := Bool(false)
  io.mem_req_data.bits.data := UFix(0)
  io.mem_req_lock := Bool(false)
  io.probe_req.valid := Bool(false)
  io.probe_req.bits.p_type := co.getProbeRequestType(x_type_, UFix(0))
  io.probe_req.bits.global_xact_id  := UFix(id)
  io.probe_req.bits.addr := addr_
  io.push_p_req      := Bits(0, width = conf.ntiles)
  io.pop_p_rep       := Bits(0, width = conf.ntiles)
  io.pop_p_rep_data  := Bits(0, width = conf.ntiles)
  io.pop_p_rep_dep   := Bits(0, width = conf.ntiles)
  io.pop_x_init      := Bits(0, width = conf.ntiles)
  io.pop_x_init_data := Bits(0, width = conf.ntiles)
  io.pop_x_init_dep  := Bits(0, width = conf.ntiles)
  io.send_x_rep_ack  := Bool(false)

  switch (state) {
    is(s_idle) {
      when( io.alloc_req.valid && io.can_alloc ) {
        addr_ := io.alloc_req.bits.xact_init.addr
        x_type_ := io.alloc_req.bits.xact_init.x_type
        init_tile_id_ := io.alloc_req.bits.tile_id
        tile_xact_id_ := io.alloc_req.bits.xact_init.tile_xact_id
        x_init_data_needs_write := co.messageHasData(io.alloc_req.bits.xact_init)
        x_needs_read := co.needsMemRead(io.alloc_req.bits.xact_init.x_type, UFix(0))
        p_req_flags := p_req_initial_flags
        mem_cnt := UFix(0)
        p_w_mem_cmd_sent := Bool(false)
        x_w_mem_cmd_sent := Bool(false)
        io.pop_x_init := UFix(1) << io.alloc_req.bits.tile_id
        if(conf.ntiles > 1) {
          p_rep_count := PopCount(p_req_initial_flags)
          state := Mux(p_req_initial_flags.orR, s_probe, s_mem)
        } else state := s_mem
      }
    }
    is(s_probe) {
      when(p_req_flags.orR) {
        io.push_p_req := p_req_flags
        io.probe_req.valid := Bool(true)
      }
      when(io.p_req_cnt_inc.orR) {
        p_req_flags := p_req_flags & ~io.p_req_cnt_inc // unflag sent reqs
      }
      when(io.p_rep_cnt_dec.orR) {
        val dec = PopCount(io.p_rep_cnt_dec)
        io.pop_p_rep := io.p_rep_cnt_dec
        if(conf.ntiles > 1) p_rep_count := p_rep_count - dec
        when(p_rep_count === dec) {
          state := s_mem
        }
      }
      when(io.p_data.valid) {
        p_rep_data_needs_write := Bool(true)
        p_rep_tile_id_ := io.p_data.bits.tile_id
      }
    }
    is(s_mem) {
      when (p_rep_data_needs_write) {
        doMemReqWrite(io.mem_req_cmd, 
                      io.mem_req_data, 
                      io.mem_req_lock, 
                      io.p_rep_data, 
                      p_rep_data_needs_write, 
                      p_w_mem_cmd_sent, 
                      io.pop_p_rep_data, 
                      io.pop_p_rep_dep, 
                      io.p_rep_data_dep.valid && (io.p_rep_data_dep.bits.global_xact_id === UFix(id)),
                      p_rep_tile_id_)
      } . elsewhen(x_init_data_needs_write) {
        doMemReqWrite(io.mem_req_cmd, 
                      io.mem_req_data, 
                      io.mem_req_lock, 
                      io.x_init_data, 
                      x_init_data_needs_write, 
                      x_w_mem_cmd_sent, 
                      io.pop_x_init_data, 
                      io.pop_x_init_dep,
                      io.x_init_data_dep.valid && (io.x_init_data_dep.bits.global_xact_id === UFix(id)),
                      init_tile_id_)
      } . elsewhen (x_needs_read) {    
        doMemReqRead(io.mem_req_cmd, x_needs_read)
      } . otherwise { 
        state := Mux(co.needsAckReply(x_type_, UFix(0)), s_ack, s_busy)
      }
    }
    is(s_ack) {
      io.send_x_rep_ack := Bool(true)
      when(io.sent_x_rep_ack) { state := s_busy }
    }
    is(s_busy) { // Nothing left to do but wait for transaction to complete
      when (io.xact_finish) {
        state := s_idle
      }
    }
  }
}

case class UncoreConfiguration(ntiles: Int, tile_id_bits: Int, co: CoherencePolicy)

abstract class CoherenceHub(implicit conf: UncoreConfiguration) extends Component {
  val io = new Bundle {
    val tiles = Vec(conf.ntiles) { new ioTileLink }.flip
    val mem = new ioMem
  }
}

class CoherenceHubNull(implicit conf: UncoreConfiguration) extends CoherenceHub
{
  val co = conf.co.asInstanceOf[ThreeStateIncoherence]

  val x_init = io.tiles(0).xact_init
  val is_write = x_init.bits.x_type === co.xactInitWriteback
  x_init.ready := io.mem.req_cmd.ready && !(is_write && io.mem.resp.valid) //stall write req/resp to handle previous read resp
  io.mem.req_cmd.valid   := x_init.valid && !(is_write && io.mem.resp.valid)
  io.mem.req_cmd.bits.rw    := is_write
  io.mem.req_cmd.bits.tag   := x_init.bits.tile_xact_id
  io.mem.req_cmd.bits.addr  := x_init.bits.addr
  io.mem.req_data <> io.tiles(0).xact_init_data

  val x_rep = io.tiles(0).xact_rep
  x_rep.bits.x_type := Mux(io.mem.resp.valid, co.xactReplyData, co.xactReplyAck)
  x_rep.bits.tile_xact_id := Mux(io.mem.resp.valid, io.mem.resp.bits.tag, x_init.bits.tile_xact_id)
  x_rep.bits.global_xact_id := UFix(0) // don't care
  x_rep.bits.data := io.mem.resp.bits.data
  x_rep.bits.require_ack := Bool(true)
  x_rep.valid := io.mem.resp.valid || x_init.valid && is_write && io.mem.req_cmd.ready

  io.tiles(0).xact_abort.valid := Bool(false)
  io.tiles(0).xact_finish.ready := Bool(true)
  io.tiles(0).probe_req.valid := Bool(false)
  io.tiles(0).probe_rep.ready := Bool(true)
  io.tiles(0).probe_rep_data.ready := Bool(true)
}


class CoherenceHubBroadcast(implicit conf: UncoreConfiguration) extends CoherenceHub
{
  val co = conf.co
  val trackerList = (0 until NGLOBAL_XACTS).map(new XactTracker(_))

  val busy_arr           = Vec(NGLOBAL_XACTS){ Bool() }
  val addr_arr           = Vec(NGLOBAL_XACTS){ Bits(width=PADDR_BITS-OFFSET_BITS) }
  val init_tile_id_arr   = Vec(NGLOBAL_XACTS){ Bits(width=conf.tile_id_bits) }
  val tile_xact_id_arr   = Vec(NGLOBAL_XACTS){ Bits(width=TILE_XACT_ID_BITS) }
  val x_type_arr         = Vec(NGLOBAL_XACTS){ Bits(width=X_INIT_TYPE_MAX_BITS) }
  val sh_count_arr       = Vec(NGLOBAL_XACTS){ Bits(width=conf.tile_id_bits) }
  val send_x_rep_ack_arr = Vec(NGLOBAL_XACTS){ Bool() }

  val do_free_arr        = Vec(NGLOBAL_XACTS){ Bool() }
  val p_rep_cnt_dec_arr  = VecBuf(NGLOBAL_XACTS){ Vec(conf.ntiles){ Bool()}  }
  val p_req_cnt_inc_arr  = VecBuf(NGLOBAL_XACTS){ Vec(conf.ntiles){ Bool()}  }
  val sent_x_rep_ack_arr = Vec(NGLOBAL_XACTS){  Bool() }
  val p_data_tile_id_arr = Vec(NGLOBAL_XACTS){  Bits(width=conf.tile_id_bits) }
  val p_data_valid_arr   = Vec(NGLOBAL_XACTS){  Bool() }

  for( i <- 0 until NGLOBAL_XACTS) {
    val t = trackerList(i).io
    busy_arr(i)           := t.busy
    addr_arr(i)           := t.addr
    init_tile_id_arr(i)   := t.init_tile_id
    tile_xact_id_arr(i)   := t.tile_xact_id
    x_type_arr(i)         := t.x_type
    sh_count_arr(i)       := t.sharer_count
    send_x_rep_ack_arr(i) := t.send_x_rep_ack
    t.xact_finish         := do_free_arr(i)
    t.p_data.bits.tile_id := p_data_tile_id_arr(i)
    t.p_data.valid        := p_data_valid_arr(i)
    t.p_rep_cnt_dec       := p_rep_cnt_dec_arr(i).toBits
    t.p_req_cnt_inc       := p_req_cnt_inc_arr(i).toBits
    t.tile_incoherent     := (Vec(io.tiles.map(_.incoherent)) { Bool() }).toBits
    t.sent_x_rep_ack      := sent_x_rep_ack_arr(i)
    do_free_arr(i)        := Bool(false)
    sent_x_rep_ack_arr(i) := Bool(false)
    p_data_tile_id_arr(i) := Bits(0, width = conf.tile_id_bits)
    p_data_valid_arr(i)   := Bool(false)
    for( j <- 0 until conf.ntiles) {
      p_rep_cnt_dec_arr(i)(j) := Bool(false)    
      p_req_cnt_inc_arr(i)(j) := Bool(false)
    }
  }

  val p_rep_data_dep_list = List.fill(conf.ntiles)((new Queue(NGLOBAL_XACTS)){new TrackerDependency}) // depth must >= NPRIMARY
  val x_init_data_dep_list = List.fill(conf.ntiles)((new Queue(NGLOBAL_XACTS)){new TrackerDependency}) // depth should >= NPRIMARY

  // Free finished transactions
  for( j <- 0 until conf.ntiles ) {
    val finish = io.tiles(j).xact_finish
    when (finish.valid) {
      do_free_arr(finish.bits.global_xact_id) := Bool(true)
    }
    finish.ready := Bool(true)
  }

  // Reply to initial requestor
  // Forward memory responses from mem to tile or arbitrate to  ack
  val mem_idx = io.mem.resp.bits.tag
  val ack_idx = PriorityEncoder(send_x_rep_ack_arr.toBits)
  for( j <- 0 until conf.ntiles ) {
    val rep = io.tiles(j).xact_rep
    rep.bits.x_type := UFix(0)
    rep.bits.tile_xact_id := UFix(0)
    rep.bits.global_xact_id := UFix(0)
    rep.bits.data := io.mem.resp.bits.data
    rep.bits.require_ack := Bool(true)
    rep.valid := Bool(false)
    when(io.mem.resp.valid && (UFix(j) === init_tile_id_arr(mem_idx))) {
      rep.bits.x_type := co.getTransactionReplyType(x_type_arr(mem_idx), sh_count_arr(mem_idx))
      rep.bits.tile_xact_id := tile_xact_id_arr(mem_idx)
      rep.bits.global_xact_id := mem_idx
      rep.valid := Bool(true)
    } . otherwise {
      rep.bits.x_type := co.getTransactionReplyType(x_type_arr(ack_idx), sh_count_arr(ack_idx))
      rep.bits.tile_xact_id := tile_xact_id_arr(ack_idx)
      rep.bits.global_xact_id := ack_idx
      when (UFix(j) === init_tile_id_arr(ack_idx)) {
        rep.valid := send_x_rep_ack_arr.toBits.orR
        sent_x_rep_ack_arr(ack_idx) := rep.ready
      }
    }
  }
  io.mem.resp.ready  := io.tiles(init_tile_id_arr(mem_idx)).xact_rep.ready

  // Create an arbiter for the one memory port
  // We have to arbitrate between the different trackers' memory requests
  // and once we have picked a request, get the right write data
  val mem_req_cmd_arb = (new Arbiter(NGLOBAL_XACTS)) { new MemReqCmd() }
  val mem_req_data_arb = (new LockingArbiter(NGLOBAL_XACTS)) { new MemData() }
  for( i <- 0 until NGLOBAL_XACTS ) {
    mem_req_cmd_arb.io.in(i)    <> trackerList(i).io.mem_req_cmd
    mem_req_data_arb.io.in(i)   <> trackerList(i).io.mem_req_data
    mem_req_data_arb.io.lock(i) <> trackerList(i).io.mem_req_lock
  }
  io.mem.req_cmd  <> Queue(mem_req_cmd_arb.io.out)
  io.mem.req_data <> Queue(mem_req_data_arb.io.out)
  
  // Handle probe replies, which may or may not have data
  for( j <- 0 until conf.ntiles ) {
    val p_rep = io.tiles(j).probe_rep
    val p_rep_data = io.tiles(j).probe_rep_data
    val idx = p_rep.bits.global_xact_id
    val pop_p_reps = trackerList.map(_.io.pop_p_rep(j).toBool)
    val do_pop = foldR(pop_p_reps)(_ || _)
    p_rep.ready := Bool(true)
    p_rep_data_dep_list(j).io.enq.valid := p_rep.valid && co.messageHasData(p_rep.bits)
    p_rep_data_dep_list(j).io.enq.bits.global_xact_id := p_rep.bits.global_xact_id
    p_rep_data.ready := foldR(trackerList.map(_.io.pop_p_rep_data(j)))(_ || _)
    when (p_rep.valid && co.messageHasData(p_rep.bits)) {
      p_data_valid_arr(idx) := Bool(true)
      p_data_tile_id_arr(idx) := UFix(j)
    }
    p_rep_data_dep_list(j).io.deq.ready := foldR(trackerList.map(_.io.pop_p_rep_dep(j).toBool))(_||_)
  }
  for( i <- 0 until NGLOBAL_XACTS ) {
    trackerList(i).io.p_rep_data.valid := io.tiles(trackerList(i).io.p_rep_tile_id).probe_rep_data.valid
    trackerList(i).io.p_rep_data.bits := io.tiles(trackerList(i).io.p_rep_tile_id).probe_rep_data.bits

    trackerList(i).io.p_rep_data_dep.valid := MuxLookup(trackerList(i).io.p_rep_tile_id, p_rep_data_dep_list(0).io.deq.valid, (0 until conf.ntiles).map( j => UFix(j) -> p_rep_data_dep_list(j).io.deq.valid))
    trackerList(i).io.p_rep_data_dep.bits := MuxLookup(trackerList(i).io.p_rep_tile_id, p_rep_data_dep_list(0).io.deq.bits, (0 until conf.ntiles).map( j => UFix(j) -> p_rep_data_dep_list(j).io.deq.bits))

    for( j <- 0 until conf.ntiles) {
      val p_rep = io.tiles(j).probe_rep
      p_rep_cnt_dec_arr(i)(j) := p_rep.valid && (p_rep.bits.global_xact_id === UFix(i))
    }
  }

  // Nack conflicting transaction init attempts
  val s_idle :: s_abort_drain :: s_abort_send :: Nil = Enum(3){ UFix() }
  val abort_state_arr = Vec(conf.ntiles) { Reg(resetVal = s_idle) }
  val want_to_abort_arr = Vec(conf.ntiles) { Bool() }
  for( j <- 0 until conf.ntiles ) {
    val x_init = io.tiles(j).xact_init
    val x_init_data = io.tiles(j).xact_init_data
    val x_abort  = io.tiles(j).xact_abort
    val abort_cnt = Reg(resetVal = UFix(0, width = log2Up(REFILL_CYCLES)))
    val conflicts = Vec(NGLOBAL_XACTS) { Bool() }
    for( i <- 0 until NGLOBAL_XACTS) {
      val t = trackerList(i).io
      conflicts(i) := t.busy && x_init.valid && co.isCoherenceConflict(t.addr, x_init.bits.addr)
    }
    x_abort.bits.tile_xact_id := x_init.bits.tile_xact_id
    want_to_abort_arr(j) := x_init.valid && (conflicts.toBits.orR || busy_arr.toBits.andR || (!x_init_data_dep_list(j).io.enq.ready && co.messageHasData(x_init.bits)))
    
    x_abort.valid := Bool(false)
    switch(abort_state_arr(j)) {
      is(s_idle) {
        when(want_to_abort_arr(j)) {
          when(co.messageHasData(x_init.bits)) {
            abort_state_arr(j) := s_abort_drain
          } . otherwise {
            abort_state_arr(j) := s_abort_send
          }
        }
      }
      is(s_abort_drain) { // raises x_init_data.ready below
        when(x_init_data.valid) {
          abort_cnt := abort_cnt + UFix(1)
          when(abort_cnt === ~UFix(0, width = log2Up(REFILL_CYCLES))) {
            abort_state_arr(j) := s_abort_send
          }
        }
      }
      is(s_abort_send) { // nothing is dequeued for now
        x_abort.valid := Bool(true)
        when(x_abort.ready) { // raises x_init.ready below
          abort_state_arr(j) := s_idle
        }
      }
    }
  }
  
  // Handle transaction initiation requests
  // Only one allocation per cycle
  // Init requests may or may not have data
  val alloc_arb = (new Arbiter(NGLOBAL_XACTS)) { Bool() }
  val init_arb = (new Arbiter(conf.ntiles)) { new TrackerAllocReq }
  for( i <- 0 until NGLOBAL_XACTS ) {
    alloc_arb.io.in(i).valid := !trackerList(i).io.busy
    trackerList(i).io.can_alloc := alloc_arb.io.in(i).ready
    trackerList(i).io.alloc_req.bits := init_arb.io.out.bits
    trackerList(i).io.alloc_req.valid := init_arb.io.out.valid

    trackerList(i).io.x_init_data.bits := io.tiles(trackerList(i).io.init_tile_id).xact_init_data.bits
    trackerList(i).io.x_init_data.valid := io.tiles(trackerList(i).io.init_tile_id).xact_init_data.valid
    trackerList(i).io.x_init_data_dep.bits := MuxLookup(trackerList(i).io.init_tile_id, x_init_data_dep_list(0).io.deq.bits, (0 until conf.ntiles).map( j => UFix(j) -> x_init_data_dep_list(j).io.deq.bits))
    trackerList(i).io.x_init_data_dep.valid := MuxLookup(trackerList(i).io.init_tile_id, x_init_data_dep_list(0).io.deq.valid, (0 until conf.ntiles).map( j => UFix(j) -> x_init_data_dep_list(j).io.deq.valid))
  }
  for( j <- 0 until conf.ntiles ) {
    val x_init = io.tiles(j).xact_init
    val x_init_data = io.tiles(j).xact_init_data
    val x_init_data_dep = x_init_data_dep_list(j).io.deq
    val x_abort = io.tiles(j).xact_abort
    init_arb.io.in(j).valid := (abort_state_arr(j) === s_idle) && !want_to_abort_arr(j) && x_init.valid
    init_arb.io.in(j).bits.xact_init := x_init.bits
    init_arb.io.in(j).bits.tile_id := UFix(j)
    val pop_x_inits = trackerList.map(_.io.pop_x_init(j).toBool)
    val do_pop = foldR(pop_x_inits)(_||_)
    x_init_data_dep_list(j).io.enq.valid := do_pop && co.messageHasData(x_init.bits) && (abort_state_arr(j) === s_idle) 
    x_init_data_dep_list(j).io.enq.bits.global_xact_id := OHToUFix(pop_x_inits)
    x_init.ready := (x_abort.valid && x_abort.ready) || do_pop
    x_init_data.ready := (abort_state_arr(j) === s_abort_drain) || foldR(trackerList.map(_.io.pop_x_init_data(j).toBool))(_||_)
    x_init_data_dep.ready := foldR(trackerList.map(_.io.pop_x_init_dep(j).toBool))(_||_)
  }
  
  alloc_arb.io.out.ready := init_arb.io.out.valid

  // Handle probe request generation
  // Must arbitrate for each request port
  val p_req_arb_arr = List.fill(conf.ntiles)((new Arbiter(NGLOBAL_XACTS)) { new ProbeRequest() })
  for( j <- 0 until conf.ntiles ) {
    for( i <- 0 until NGLOBAL_XACTS ) {
      val t = trackerList(i).io
      p_req_arb_arr(j).io.in(i).bits :=  t.probe_req.bits
      p_req_arb_arr(j).io.in(i).valid := t.probe_req.valid && t.push_p_req(j)
      p_req_cnt_inc_arr(i)(j) := p_req_arb_arr(j).io.in(i).ready
    }
    p_req_arb_arr(j).io.out <> io.tiles(j).probe_req
  }

}
