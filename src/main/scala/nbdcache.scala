package rocket

import Chisel._
import Constants._

class ioReplacementWayGen extends Bundle {
  val pick_new_way = Bool(dir = INPUT)
  val way_en = Bits(width = NWAYS, dir = INPUT)
  val way_id = UFix(width = log2Up(NWAYS), dir = OUTPUT)
}

class RandomReplacementWayGen extends Component {
  val io = new ioReplacementWayGen()
  //TODO: Actually limit selection based on which ways are allowed (io.ways_en)
  io.way_id := UFix(0)
  if(NWAYS > 1) 
  {
    val rand_way_id = LFSR16(io.pick_new_way)(log2Up(NWAYS)-1,0)
    when (rand_way_id < UFix(NWAYS)) { io.way_id := rand_way_id }
  }
}

class StoreMaskGen extends Component {
  val io = new Bundle {
    val typ   = Bits(3, INPUT)
    val addr  = Bits(3, INPUT)
    val wmask = Bits(8, OUTPUT)
  }

  val word = (io.typ === MT_W) || (io.typ === MT_WU)
  val half = (io.typ === MT_H) || (io.typ === MT_HU)
  val byte_ = (io.typ === MT_B) || (io.typ === MT_BU)

  io.wmask := Mux(byte_, Bits(  1,1) <<     io.addr(2,0).toUFix,
              Mux(half, Bits(  3,2) << Cat(io.addr(2,1), Bits(0,1)).toUFix,
              Mux(word, Bits( 15,4) << Cat(io.addr(2),   Bits(0,2)).toUFix,
                        Bits(255,8))));
}

class StoreDataGen extends Component {
  val io = new Bundle {
    val typ  = Bits(3, INPUT)
    val din  = Bits(64, INPUT)
    val dout = Bits(64, OUTPUT)
  }

  val word = (io.typ === MT_W) || (io.typ === MT_WU)
  val half = (io.typ === MT_H) || (io.typ === MT_HU)
  val byte_ = (io.typ === MT_B) || (io.typ === MT_BU)

  io.dout := Mux(byte_, Fill(8, io.din( 7,0)),
             Mux(half, Fill(4, io.din(15,0)),
             Mux(word, Fill(2, io.din(31,0)),
                       io.din)))
}

// this currently requires that CPU_DATA_BITS == 64
class LoadDataGen extends Component {
  val io = new Bundle {
    val typ  = Bits(3, INPUT)
    val addr = Bits(log2Up(MEM_DATA_BITS/8), INPUT)
    val din  = Bits(MEM_DATA_BITS, INPUT)
    val dout = Bits(64, OUTPUT)
    val r_dout = Bits(64, OUTPUT)
    val r_dout_subword = Bits(64, OUTPUT)
  }

  val sext = (io.typ === MT_B) || (io.typ === MT_H) ||
             (io.typ === MT_W) || (io.typ === MT_D)
  val word = (io.typ === MT_W) || (io.typ === MT_WU)
  val half = (io.typ === MT_H) || (io.typ === MT_HU)
  val byte_ = (io.typ === MT_B) || (io.typ === MT_BU)

  val shifted = io.din >> Cat(io.addr(io.addr.width-1,2), Bits(0, 5)).toUFix
  val extended =
    Mux(word, Cat(Fill(32, sext & shifted(31)), shifted(31,0)), shifted)

  val r_extended = Reg(extended)
  val r_sext = Reg(sext)
  val r_half = Reg(half)
  val r_byte = Reg(byte_)
  val r_addr = Reg(io.addr)

  val shifted_subword = r_extended >> Cat(r_addr(1,0), Bits(0, 3)).toUFix
  val extended_subword =
    Mux(r_byte, Cat(Fill(56, r_sext & shifted_subword( 7)), shifted_subword( 7,0)),
    Mux(r_half, Cat(Fill(48, r_sext & shifted_subword(15)), shifted_subword(15,0)),
              shifted_subword))

  io.dout := extended
  io.r_dout := r_extended
  io.r_dout_subword := extended_subword
}

class MSHRReq extends Bundle {
  val tag_miss = Bool()
  val old_dirty = Bool()
  val old_tag = Bits(width = TAG_BITS)

  val tag = Bits(width = TAG_BITS)
  val idx = Bits(width = IDX_BITS)
  val way_oh = Bits(width = NWAYS)

  val offset = Bits(width = OFFSET_BITS)
  val cmd    = Bits(width = 4)
  val typ    = Bits(width = 3)
  val cpu_tag = Bits(width = DCACHE_TAG_BITS)
  val data   = Bits(width = CPU_DATA_BITS)
}

class RPQEntry extends Bundle {
  val offset = Bits(width = OFFSET_BITS)
  val cmd    = Bits(width = 4)
  val typ    = Bits(width = 3)
  val sdq_id = UFix(width = log2Up(NSDQ))
  val cpu_tag = Bits(width = DCACHE_TAG_BITS)
}

class Replay extends RPQEntry {
  val idx    = Bits(width = IDX_BITS)
  val way_oh = Bits(width = NWAYS)
}

class DataReq extends Bundle {
  val idx    = Bits(width = IDX_BITS)
  val offset = Bits(width = OFFSET_BITS)
  val cmd    = Bits(width = 4)
  val typ    = Bits(width = 3)
  val data = Bits(width = CPU_DATA_BITS)
  val way_oh = Bits(width = NWAYS)
}

class DataArrayReq extends Bundle {
  val way_en = Bits(width = NWAYS)
  val idx    = Bits(width = IDX_BITS)
  val offset = Bits(width = log2Up(REFILL_CYCLES))
  val rw     = Bool()
  val wmask  = Bits(width = MEM_DATA_BITS/8)
  val data   = Bits(width = MEM_DATA_BITS)
}

class WritebackReq extends Bundle {
  val tag = Bits(width = TAG_BITS)
  val idx = Bits(width = IDX_BITS)
  val way_oh = Bits(width = NWAYS)
  val tile_xact_id = Bits(width = TILE_XACT_ID_BITS)
}

class MetaData extends Bundle {
  val state = UFix(width = 2)
  val tag = Bits(width = TAG_BITS)
}

class MetaArrayReq extends Bundle {
  val way_en = Bits(width = NWAYS)
  val idx  = Bits(width = IDX_BITS)
  val rw  = Bool()
  val data = new MetaData()
}

class MSHR(id: Int, co: CoherencePolicy) extends Component {
  val io = new Bundle {
    val req_pri_val    = Bool(INPUT)
    val req_pri_rdy    = Bool(OUTPUT)
    val req_sec_val    = Bool(INPUT)
    val req_sec_rdy    = Bool(OUTPUT)
    val req_bits       = new MSHRReq().asInput
    val req_sdq_id     = UFix(log2Up(NSDQ), INPUT)

    val idx_match      = Bool(OUTPUT)
    val idx            = Bits(IDX_BITS, OUTPUT)
    val refill_count   = Bits(log2Up(REFILL_CYCLES), OUTPUT)
    val tag            = Bits(TAG_BITS, OUTPUT)
    val way_oh         = Bits(NWAYS, OUTPUT)

    val mem_req  = (new FIFOIO) { new TransactionInit }
    val meta_req = (new FIFOIO) { new MetaArrayReq() }
    val replay   = (new FIFOIO) { new Replay()   }
    val mem_abort = (new PipeIO) { new TransactionAbort }.flip
    val mem_rep = (new PipeIO) { new TransactionReply }.flip
    val mem_finish = (new FIFOIO) { new TransactionFinish }
    val wb_req = (new FIFOIO) { new WritebackReq }
    val probe_writeback = (new FIFOIO) { Bool() }.flip
    val probe_refill = (new FIFOIO) { Bool() }.flip
  }

  val s_invalid :: s_wb_req :: s_wb_resp :: s_meta_clear :: s_refill_req :: s_refill_resp :: s_drain_rpq :: Nil = Enum(7) { UFix() }
  val state = Reg(resetVal = s_invalid)
  val flush = Reg { Bool() }

  val xacx_type = Reg { UFix() }
  val line_state = Reg { UFix() }
  val refill_count = Reg { UFix(width = log2Up(REFILL_CYCLES)) }
  val req = Reg { new MSHRReq() }

  val req_cmd = io.req_bits.cmd
  val req_use_rpq = (req_cmd != M_PFR) && (req_cmd != M_PFW) && (req_cmd != M_FLA)
  val idx_match = req.idx === io.req_bits.idx
  val sec_rdy = idx_match && !flush && (state === s_wb_req || state === s_wb_resp || state === s_meta_clear || (state === s_refill_req || state === s_refill_resp) && !co.needsTransactionOnSecondaryMiss(req_cmd, io.mem_req.bits))

  val rpq = (new queue(NRPQ)) { new RPQEntry }
  rpq.io.enq.valid := (io.req_pri_val && io.req_pri_rdy || io.req_sec_val && sec_rdy) && req_use_rpq
  rpq.io.enq.bits := io.req_bits
  rpq.io.enq.bits.sdq_id := io.req_sdq_id
  rpq.io.deq.ready := io.replay.ready && (state === s_drain_rpq) || (state === s_invalid)

  val abort = io.mem_abort.valid && io.mem_abort.bits.tile_xact_id === UFix(id)
  val reply = io.mem_rep.valid && io.mem_rep.bits.tile_xact_id === UFix(id)
  val refill_done = reply && refill_count.andR
  val wb_done = reply && (state === s_wb_resp)

  val finish_q = (new queue(2 /* wb + refill */)) { new TransactionFinish }
  finish_q.io.enq.valid := wb_done || refill_done
  finish_q.io.enq.bits.global_xact_id := io.mem_rep.bits.global_xact_id

  when (state === s_drain_rpq && !rpq.io.deq.valid && !finish_q.io.deq.valid && io.meta_req.ready) {
    state := s_invalid
  }
  when (state === s_refill_resp) {
    when (refill_done) { state := s_drain_rpq }
    when (reply) {
      refill_count := refill_count + UFix(1)
      line_state := co.newStateOnTransactionReply(io.mem_rep.bits, io.mem_req.bits)
    }
    when (abort) { state := s_refill_req }
  }
  when (state === s_refill_req) {
    when (flush) { state := s_drain_rpq }
    .elsewhen (abort) { state := s_refill_req }
    .elsewhen (io.mem_req.ready) { state := s_refill_resp }
  }
  when (state === s_meta_clear && io.meta_req.ready) {
    state := s_refill_req
  }
  when (state === s_wb_resp) {
    when (reply) { state := s_meta_clear }
    when (abort) { state := s_wb_req }
  }
  when (state === s_wb_req) {
    when (io.probe_writeback.valid && idx_match) { state := s_refill_req }
    .elsewhen (io.wb_req.ready) { state := s_wb_resp }
  }

  when (io.req_sec_val && io.req_sec_rdy) { // s_wb_req, s_wb_resp, s_refill_req
    xacx_type := co.getTransactionInitTypeOnSecondaryMiss(req_cmd, co.newStateOnFlush(), io.mem_req.bits)
  }
  when ((state === s_invalid) && io.req_pri_val) {
    flush := req_cmd === M_FLA
    line_state := co.newStateOnFlush()
    refill_count := UFix(0)
    xacx_type := co.getTransactionInitTypeOnPrimaryMiss(req_cmd, co.newStateOnFlush())
    req := io.req_bits

    when (io.req_bits.tag_miss) {
      state := Mux(io.req_bits.old_dirty, s_wb_req, s_refill_req)
    }
  }

  io.idx_match := (state != s_invalid) && idx_match
  io.idx := req.idx
  io.tag := req.tag
  io.way_oh := req.way_oh
  io.refill_count := refill_count
  io.req_pri_rdy := (state === s_invalid)
  io.req_sec_rdy := sec_rdy && rpq.io.enq.ready

  io.meta_req.valid := (state === s_drain_rpq) && !rpq.io.deq.valid && !finish_q.io.deq.valid || (state === s_meta_clear)
  io.meta_req.bits.rw := Bool(true)
  io.meta_req.bits.idx := req.idx
  io.meta_req.bits.data.state := Mux(state === s_meta_clear, co.newStateOnFlush(), line_state)
  io.meta_req.bits.data.tag := req.tag
  io.meta_req.bits.way_en := req.way_oh

  io.wb_req.valid := (state === s_wb_req) && !(io.probe_writeback.valid && idx_match)
  io.wb_req.bits.tag := req.old_tag
  io.wb_req.bits.idx := req.idx
  io.wb_req.bits.way_oh := req.way_oh
  io.wb_req.bits.tile_xact_id := Bits(id)

  io.probe_writeback.ready := (state != s_wb_resp && state != s_meta_clear && state != s_drain_rpq) || !idx_match
  io.probe_refill.ready := (state != s_refill_resp && state != s_drain_rpq) || !idx_match

  io.mem_req.valid := (state === s_refill_req) && !flush
  io.mem_req.bits.x_type := xacx_type
  io.mem_req.bits.address := Cat(req.tag, req.idx).toUFix
  io.mem_req.bits.tile_xact_id := Bits(id)
  io.mem_finish <> finish_q.io.deq

  io.replay.valid := (state === s_drain_rpq) && rpq.io.deq.valid
  io.replay.bits <> rpq.io.deq.bits
  io.replay.bits.idx := req.idx
  io.replay.bits.way_oh := req.way_oh
}

class MSHRFile(co: CoherencePolicy) extends Component {
  val io = new Bundle {
    val req = (new FIFOIO) { new MSHRReq }.flip
    val secondary_miss = Bool(OUTPUT)

    val mem_resp_idx = Bits(IDX_BITS, OUTPUT)
    val mem_resp_offset = Bits(log2Up(REFILL_CYCLES), OUTPUT)
    val mem_resp_way_oh = Bits(NWAYS, OUTPUT)

    val fence_rdy = Bool(OUTPUT)

    val mem_req  = (new FIFOIO) { new TransactionInit }
    val meta_req = (new FIFOIO) { new MetaArrayReq() }
    val data_req   = (new FIFOIO) { new DataReq() }
    val mem_abort = (new PipeIO) { new TransactionAbort }.flip
    val mem_rep = (new PipeIO) { new TransactionReply }.flip
    val mem_finish = (new FIFOIO) { new TransactionFinish }
    val wb_req = (new FIFOIO) { new WritebackReq }
    val probe = (new FIFOIO) { Bool() }.flip

    val cpu_resp_val = Bool(OUTPUT)
    val cpu_resp_tag = Bits(DCACHE_TAG_BITS, OUTPUT)
  }

  val sdq_val = Reg(resetVal = Bits(0, NSDQ))
  val sdq_alloc_id = PriorityEncoder(~sdq_val(NSDQ-1,0))
  val sdq_rdy = !sdq_val.andR
  val (req_read, req_write) = cpuCmdToRW(io.req.bits.cmd)
  val sdq_enq = io.req.valid && io.req.ready && req_write
  val sdq = Mem(NSDQ) { io.req.bits.data.clone }
  when (sdq_enq) { sdq(sdq_alloc_id) := io.req.bits.data }

  val tag_mux = (new Mux1H(NMSHR)){ Bits(width = TAG_BITS) }
  val wb_probe_mux = (new Mux1H(NMSHR)) { new WritebackReq }
  val mem_resp_mux = (new Mux1H(NMSHR)){ new DataArrayReq }
  val meta_req_arb = (new Arbiter(NMSHR)) { new MetaArrayReq() }
  val mem_req_arb = (new Arbiter(NMSHR)) { new TransactionInit }
  val mem_finish_arb = (new Arbiter(NMSHR)) { new TransactionFinish }
  val wb_req_arb = (new Arbiter(NMSHR)) { new WritebackReq }
  val replay_arb = (new Arbiter(NMSHR)) { new Replay() }
  val alloc_arb = (new Arbiter(NMSHR)) { Bool() }

  val tag_match = tag_mux.io.out === io.req.bits.tag
  val wb_probe_match = wb_probe_mux.io.out.tag === io.req.bits.tag

  var idx_match = Bool(false)
  var pri_rdy = Bool(false)
  var fence = Bool(false)
  var sec_rdy = Bool(false)
  var writeback_probe_rdy = Bool(true)
  var refill_probe_rdy = Bool(true)

  for (i <- 0 to NMSHR-1) {
    val mshr = new MSHR(i, co)

    tag_mux.io.sel(i) := mshr.io.idx_match
    tag_mux.io.in(i) := mshr.io.tag
    wb_probe_mux.io.sel(i) := mshr.io.idx_match
    wb_probe_mux.io.in(i) := mshr.io.wb_req.bits

    alloc_arb.io.in(i).valid := mshr.io.req_pri_rdy
    mshr.io.req_pri_val := alloc_arb.io.in(i).ready

    mshr.io.req_sec_val := io.req.valid && sdq_rdy && tag_match
    mshr.io.req_bits := io.req.bits
    mshr.io.req_sdq_id := sdq_alloc_id

    mshr.io.meta_req <> meta_req_arb.io.in(i)
    mshr.io.mem_req <> mem_req_arb.io.in(i)
    mshr.io.mem_finish <> mem_finish_arb.io.in(i)
    mshr.io.wb_req <> wb_req_arb.io.in(i)
    mshr.io.replay <> replay_arb.io.in(i)
    mshr.io.probe_refill.valid := io.probe.valid && tag_match
    mshr.io.probe_writeback.valid := io.probe.valid && wb_probe_match

    mshr.io.mem_abort <> io.mem_abort
    mshr.io.mem_rep <> io.mem_rep
    mem_resp_mux.io.sel(i) := UFix(i) === io.mem_rep.bits.tile_xact_id
    mem_resp_mux.io.in(i).idx := mshr.io.idx
    mem_resp_mux.io.in(i).offset := mshr.io.refill_count
    mem_resp_mux.io.in(i).way_en := mshr.io.way_oh

    pri_rdy = pri_rdy || mshr.io.req_pri_rdy
    sec_rdy = sec_rdy || mshr.io.req_sec_rdy
    fence = fence || !mshr.io.req_pri_rdy
    idx_match = idx_match || mshr.io.idx_match
    refill_probe_rdy = refill_probe_rdy && mshr.io.probe_refill.ready
    writeback_probe_rdy = writeback_probe_rdy && mshr.io.probe_writeback.ready
  }

  alloc_arb.io.out.ready := io.req.valid && sdq_rdy && !idx_match

  meta_req_arb.io.out <> io.meta_req
  mem_req_arb.io.out <> io.mem_req
  mem_finish_arb.io.out <> io.mem_finish
  wb_req_arb.io.out <> io.wb_req

  io.req.ready := Mux(idx_match, tag_match && sec_rdy, pri_rdy) && sdq_rdy
  io.secondary_miss := idx_match
  io.mem_resp_idx := mem_resp_mux.io.out.idx
  io.mem_resp_offset := mem_resp_mux.io.out.offset
  io.mem_resp_way_oh := mem_resp_mux.io.out.way_en
  io.fence_rdy := !fence
  io.probe.ready := (refill_probe_rdy || !tag_match) && (writeback_probe_rdy || !wb_probe_match)

  val replay = Queue(replay_arb.io.out, 1, pipe = true)
  replay.ready := io.data_req.ready
  io.data_req <> replay

  val (replay_read, replay_write) = cpuCmdToRW(replay.bits.cmd)
  val sdq_free = replay.valid && replay.ready && replay_write
  sdq_val := sdq_val & ~((UFix(1) << replay.bits.sdq_id) & Fill(sdq_free, NSDQ)) | 
             PriorityEncoderOH(~sdq_val(NSDQ-1,0)) & Fill(NSDQ, sdq_enq && io.req.bits.tag_miss)
  val sdq_rdata = Reg() { io.req.bits.data.clone }
  sdq_rdata := sdq(Mux(replay.valid && !replay.ready, replay.bits.sdq_id, replay_arb.io.out.bits.sdq_id))
  io.data_req.bits.data := sdq_rdata

  io.cpu_resp_val := Reg(replay.valid && replay.ready && replay_read, resetVal = Bool(false))
  io.cpu_resp_tag := Reg(replay.bits.cpu_tag)
}


class WritebackUnit(co: CoherencePolicy) extends Component {
  val io = new Bundle {
    val req = (new FIFOIO) { new WritebackReq() }.flip
    val probe = (new FIFOIO) { new WritebackReq() }.flip
    val data_req = (new FIFOIO) { new DataArrayReq() }
    val data_resp = Bits(MEM_DATA_BITS, INPUT)
    val mem_req = (new FIFOIO) { new TransactionInit }
    val mem_req_data = (new FIFOIO) { new TransactionInitData }
    val probe_rep_data = (new FIFOIO) { new ProbeReplyData }
  }

  val valid = Reg(resetVal = Bool(false))
  val is_probe = Reg() { Bool() }
  val data_req_fired = Reg(resetVal = Bool(false))
  val cmd_sent = Reg() { Bool() }
  val cnt = Reg() { UFix(width = log2Up(REFILL_CYCLES+1)) }
  val req = Reg() { new WritebackReq() }

  val dout_rdy = Mux(is_probe, io.probe_rep_data.ready, io.mem_req_data.ready)
  data_req_fired := Bool(false)
  when (valid && io.mem_req.ready) {
    cmd_sent := Bool(true)
  }
  when (io.data_req.valid && io.data_req.ready) {
    data_req_fired := Bool(true)
    cnt := cnt + UFix(1)
  }
  when (data_req_fired && !dout_rdy) {
    data_req_fired := Bool(false)
    cnt := cnt - UFix(1)
  }
  .elsewhen (cmd_sent && (cnt === UFix(REFILL_CYCLES))) {
    valid := Bool(false)
  }
  when (io.probe.valid && io.probe.ready) {
    valid := Bool(true)
    is_probe := Bool(true)
    cmd_sent := Bool(true)
    cnt := UFix(0)
    req := io.probe.bits
  }
  when (io.req.valid && io.req.ready) {
    valid := Bool(true)
    is_probe := Bool(false)
    cmd_sent := Bool(false)
    cnt := UFix(0)
    req := io.req.bits
  }

  io.req.ready := !valid && !io.probe.valid
  io.probe.ready := !valid
  io.data_req.valid := valid && (cnt < UFix(REFILL_CYCLES))
  io.data_req.bits.way_en := req.way_oh
  io.data_req.bits.idx := req.idx
  io.data_req.bits.offset := cnt
  io.data_req.bits.rw := Bool(false)
  io.data_req.bits.wmask := Bits(0)
  io.data_req.bits.data := Bits(0)

  io.mem_req.valid := valid && !cmd_sent
  io.mem_req.bits.x_type := co.getTransactionInitTypeOnWriteback()
  io.mem_req.bits.address := Cat(req.tag, req.idx).toUFix
  io.mem_req.bits.tile_xact_id := req.tile_xact_id
  io.mem_req_data.valid := data_req_fired && !is_probe
  io.mem_req_data.bits.data := io.data_resp
  io.probe_rep_data.valid := data_req_fired && is_probe
  io.probe_rep_data.bits.data := io.data_resp
}

class ProbeUnit(co: CoherencePolicy) extends Component {
  val io = new Bundle {
    val req = (new FIFOIO) { new ProbeRequest }.flip
    val rep = (new FIFOIO) { new ProbeReply }
    val meta_req = (new FIFOIO) { new MetaArrayReq }
    val mshr_req = (new FIFOIO) { Bool() }
    val wb_req = (new FIFOIO) { new WritebackReq }
    val tag_match_way_oh = Bits(NWAYS, INPUT)
    val line_state = UFix(2, INPUT)
    val address = Bits(PADDR_BITS-OFFSET_BITS, OUTPUT)
  }

  val s_invalid :: s_meta_req :: s_meta_resp :: s_mshr_req :: s_probe_rep :: s_writeback_req :: s_writeback_resp :: Nil = Enum(7) { UFix() }
  val state = Reg(resetVal = s_invalid)
  val line_state = Reg() { UFix() }
  val way_oh = Reg() { Bits() }
  val req = Reg() { new ProbeRequest() }
  val hit = way_oh.orR

  when ((state === s_writeback_resp) && io.wb_req.ready) {
    state := s_invalid
  }
  when ((state === s_writeback_req) && io.wb_req.ready) {
    state := s_writeback_resp
  }
  when ((state === s_probe_rep) && io.meta_req.ready && io.rep.ready) {
    state := Mux(hit && co.needsWriteback(line_state), s_writeback_req, s_invalid)
  }
  when ((state === s_mshr_req) && io.mshr_req.ready) {
    state := s_meta_req
  }
  when (state === s_meta_resp) {
    way_oh := io.tag_match_way_oh
    line_state := io.line_state
    state := Mux(!io.mshr_req.ready, s_mshr_req, s_probe_rep)
  }
  when ((state === s_meta_req) && io.meta_req.ready) {
    state := s_meta_resp
  }
  when ((state === s_invalid) && io.req.valid) {
    state := s_meta_req
    req := io.req.bits
  }

  io.req.ready := state === s_invalid
  io.rep.valid := state === s_probe_rep && io.meta_req.ready
  io.rep.bits := co.newProbeReply(req, Mux(hit, line_state, co.newStateOnFlush()))

  io.meta_req.valid := state === s_meta_req || state === s_meta_resp || state === s_mshr_req || state === s_probe_rep && hit
  io.meta_req.bits.way_en := Mux(state === s_probe_rep, way_oh, ~UFix(0, NWAYS))
  io.meta_req.bits.rw := state === s_probe_rep
  io.meta_req.bits.idx := req.address
  io.meta_req.bits.data.state := co.newStateOnProbeRequest(req, line_state)
  io.meta_req.bits.data.tag := req.address >> UFix(IDX_BITS)
  io.mshr_req.valid := state === s_meta_resp || state === s_mshr_req
  io.address := req.address

  io.wb_req.valid := state === s_writeback_req
  io.wb_req.bits.way_oh := way_oh
  io.wb_req.bits.idx := req.address
  io.wb_req.bits.tag := req.address >> UFix(IDX_BITS)
}

class FlushUnit(lines: Int, co: CoherencePolicy) extends Component {
  val io = new Bundle {
    val req = (new FIFOIO) { Bool() }.flip
    val meta_req = (new FIFOIO) { new MetaArrayReq() }
    val mshr_req = (new FIFOIO) { Bool() }
  }
  
  val s_reset :: s_ready :: s_meta_read :: s_meta_wait :: Nil = Enum(4) { UFix() }
  val state = Reg(resetVal = s_reset)
  val idx_cnt = Reg(resetVal = UFix(0, log2Up(lines)))
  val next_idx_cnt = idx_cnt + UFix(1)
  val way_cnt = if (NWAYS == 1) UFix(0) else Reg(resetVal = UFix(0, log2Up(NWAYS)))
  val next_way_cnt = way_cnt + UFix(1)

  switch (state) {
    is(s_reset) { 
      when (io.meta_req.ready) { 
        state := Mux(way_cnt === UFix(NWAYS-1) && idx_cnt.andR, s_ready, s_reset); 
        when (way_cnt === UFix(NWAYS-1)) { idx_cnt := next_idx_cnt };
        if (NWAYS > 1) way_cnt := next_way_cnt;
      } 
    }
    is(s_ready) { when (io.req.valid) { state := s_meta_read } }
    is(s_meta_read) { when (io.meta_req.ready) { state := s_meta_wait } }
    is(s_meta_wait) {
      state := s_meta_read
      when (io.mshr_req.ready) {
        state := s_meta_read
        when (way_cnt === UFix(NWAYS-1)) {
          when (idx_cnt.andR) {
            state := s_ready
          }
          idx_cnt := next_idx_cnt
        }
        if (NWAYS > 1) way_cnt := next_way_cnt;
      }
    }
  }

  io.req.ready := state === s_ready
  io.mshr_req.valid := state === s_meta_wait
  io.meta_req.valid := (state === s_meta_read) || (state === s_reset)
  io.meta_req.bits.way_en := UFixToOH(way_cnt, NWAYS)
  io.meta_req.bits.idx := idx_cnt
  io.meta_req.bits.rw := (state === s_reset)
  io.meta_req.bits.data.state := co.newStateOnFlush()
  io.meta_req.bits.data.tag := UFix(0)
}

class MetaDataArrayArray(lines: Int) extends Component {
  val io = new Bundle {
    val req  = (new FIFOIO) { new MetaArrayReq() }.flip
    val resp = Vec(NWAYS){ (new MetaData).asOutput }
    val state_req = (new FIFOIO) { new MetaArrayReq() }.flip
    val way_en = Bits(width = NWAYS, dir = OUTPUT)
  }

  val permBits = io.req.bits.data.state.width
  val perms = Mem(lines) { UFix(width = permBits*NWAYS) }
  val tags = Mem(lines*NWAYS, seqRead = true) { Bits(width = TAG_BITS*NWAYS) }
  val tag = Reg() { Bits() }
  val raddr = Reg() { Bits() }
  val way_en_ = Reg { Bits(width=NWAYS) }

  when (io.state_req.valid && io.state_req.bits.rw) {
    perms.write(io.state_req.bits.idx, Fill(NWAYS, io.state_req.bits.data.state), FillInterleaved(permBits, io.state_req.bits.way_en))
  }
  when (io.req.valid) {
    when (io.req.bits.rw) {
      perms.write(io.req.bits.idx, Fill(NWAYS, io.req.bits.data.state), FillInterleaved(permBits, io.req.bits.way_en))
      tags.write(io.req.bits.idx, Fill(NWAYS, io.req.bits.data.tag), FillInterleaved(TAG_BITS, io.req.bits.way_en))
    }
    .otherwise {
      raddr := io.req.bits.idx
      tag := tags(io.req.bits.idx)
    }
    way_en_ := io.req.bits.way_en
  }

  val perm = perms(raddr)
  for(w <- 0 until NWAYS) {
    io.resp(w).state := perm(permBits*(w+1)-1, permBits*w)
    io.resp(w).tag := tag(TAG_BITS*(w+1)-1, TAG_BITS*w)
  }

  io.way_en := way_en_
  io.req.ready := Bool(true)
  io.state_req.ready := Bool(true)
}

class DataArray(lines: Int) extends Component {
  val io = new Bundle {
    val req  = (new FIFOIO) { new DataArrayReq() }.flip
    val resp = Bits(width = MEM_DATA_BITS, dir = OUTPUT)
  }

  val wmask = FillInterleaved(8, io.req.bits.wmask)
  val addr = Cat(io.req.bits.idx, io.req.bits.offset)
  val rdata = Reg() { Bits() }

  val array = Mem(lines*REFILL_CYCLES, seqRead = true){ Bits(width=MEM_DATA_BITS) }
  when (io.req.valid) {
    when (io.req.bits.rw) { array.write(addr, io.req.bits.data, wmask) }
    .otherwise { rdata := array(addr) }
  }

  io.resp := rdata
  io.req.ready := Bool(true)
}

class DataArrayArray(lines: Int) extends Component {
  val io = new Bundle {
    val req  = (new FIFOIO) { new DataArrayReq() }.flip
    val resp = Vec(NWAYS){ Bits(width = MEM_DATA_BITS, dir = OUTPUT) }
    val way_en = Bits(width = NWAYS, dir = OUTPUT)
  }

  val way_en_ = Reg { Bits(width=NWAYS) }
  when (io.req.valid && io.req.ready) {
    way_en_ := io.req.bits.way_en
  }

  for(w <- 0 until NWAYS) {
    val way = new DataArray(lines)
    way.io.req.bits <> io.req.bits
    way.io.req.valid := io.req.valid && io.req.bits.way_en(w).toBool
    way.io.resp <> io.resp(w)
  }

  io.way_en := way_en_
  io.req.ready := Bool(true)
}

class AMOALU extends Component {
  val io = new Bundle {
    val cmd = Bits(4, INPUT)
    val typ = Bits(3, INPUT)
    val lhs = UFix(64, INPUT)
    val rhs = UFix(64, INPUT)
    val out = UFix(64, OUTPUT)
  }
  
  val sgned = (io.cmd === M_XA_MIN) || (io.cmd === M_XA_MAX)
  val sub = (io.cmd === M_XA_MIN) || (io.cmd === M_XA_MINU) || (io.cmd === M_XA_MAX) || (io.cmd === M_XA_MAXU)
  val min = (io.cmd === M_XA_MIN) || (io.cmd === M_XA_MINU)
  val word = (io.typ === MT_W) || (io.typ === MT_WU)

  val adder_out = (Cat(io.lhs, UFix(0,1)).toUFix + Cat(io.rhs ^ Fill(io.rhs.width, sub), sub).toUFix) >> UFix(1)

  val cmp_lhs  = Mux(word, io.lhs(31), io.lhs(63))
  val cmp_rhs  = Mux(word, io.rhs(31), io.rhs(63))
  val cmp_diff = Mux(word, adder_out(31), adder_out(63))
  val less = Mux(cmp_lhs === cmp_rhs, cmp_diff, Mux(sgned, cmp_lhs, cmp_rhs))
  val cmp_out = Mux(min === less, io.lhs, io.rhs)

  val out = Mux(io.cmd === M_XA_ADD,  adder_out,
            Mux(io.cmd === M_XA_SWAP, io.rhs,
            Mux(io.cmd === M_XA_AND,  io.lhs & io.rhs,
            Mux(io.cmd === M_XA_OR,   io.lhs | io.rhs,
                /* MIN[U]/MAX[U] */   cmp_out))));

  io.out := Mux(word, Cat(out(31,0), out(31,0)).toUFix, out)
}

class HellaCacheReq extends Bundle {
  val kill = Bool()
  val typ  = Bits(width = 3)
  val idx  = Bits(width = PGIDX_BITS)
  val ppn  = Bits(width = PPN_BITS)
  val data = Bits(width = 64)
  val tag  = Bits(width = DCACHE_TAG_BITS)
  val cmd  = Bits(width = 4)
}

class HellaCacheResp extends Bundle {
  val miss   = Bool()
  val nack   = Bool()
  val replay = Bool()
  val typ    = Bits(width = 3)
  val data   = Bits(width = 64)
  val data_subword = Bits(width = 64)
  val tag    = Bits(width = DCACHE_TAG_BITS)
}

class AlignmentExceptions extends Bundle {
  val ld = Bool()
  val st = Bool()
}

class HellaCacheExceptions extends Bundle {
  val ma = new AlignmentExceptions
}

// interface between D$ and processor/DTLB
class ioHellaCache extends Bundle {
  val req = (new FIFOIO){ new HellaCacheReq }
  val resp = (new PipeIO){ new HellaCacheResp }.flip
  val xcpt = (new HellaCacheExceptions).asInput
}

class HellaCache(co: CoherencePolicy) extends Component {
  val io = new Bundle {
    val cpu = (new ioHellaCache).flip
    val mem = new ioTileLink
  }
 
  val lines       = 1 << IDX_BITS
  val addrbits    = PADDR_BITS
  val indexbits   = IDX_BITS
  val offsetbits  = OFFSET_BITS
  val tagmsb      = PADDR_BITS-1
  val taglsb      = indexbits+offsetbits
  val tagbits     = tagmsb-taglsb+1
  val indexmsb    = taglsb-1
  val indexlsb    = offsetbits
  val offsetmsb   = indexlsb-1
  val offsetlsb   = log2Up(CPU_DATA_BITS/8)
  val ramindexlsb = log2Up(MEM_DATA_BITS/8)
  
  val early_nack       = Reg { Bool() }
  val r_cpu_req_val_   = Reg(io.cpu.req.valid && io.cpu.req.ready, resetVal = Bool(false))
  val r_cpu_req_val    = r_cpu_req_val_ && !io.cpu.req.bits.kill && !early_nack
  val r_cpu_req_idx    = Reg() { Bits() }
  val r_cpu_req_cmd    = Reg() { Bits() }
  val r_cpu_req_type   = Reg() { Bits() }
  val r_cpu_req_tag    = Reg() { Bits() }
  val r_amo_replay_data = Reg() { Bits() }
  val r_way_oh         = Reg() { Bits() }

  val p_store_valid    = Reg(resetVal = Bool(false))
  val p_store_data     = Reg() { Bits() }
  val p_store_idx      = Reg() { Bits() }
  val p_store_cmd      = Reg() { Bits() }
  val p_store_type     = Reg() { Bits() }
  val p_store_way_oh   = Reg() { Bits() }
  val r_replay_amo     = Reg(resetVal = Bool(false))

  val req_store   = (io.cpu.req.bits.cmd === M_XWR)
  val req_load    = (io.cpu.req.bits.cmd === M_XRD)
  val req_amo     = io.cpu.req.bits.cmd(3).toBool
  val req_read    = req_load || req_amo
  val req_write   = req_store || req_amo
  val r_req_load  = (r_cpu_req_cmd === M_XRD)
  val r_req_store = (r_cpu_req_cmd === M_XWR)
  val r_req_flush = (r_cpu_req_cmd === M_FLA)
  val r_req_fence = (r_cpu_req_cmd === M_FENCE)
  val r_req_prefetch = (r_cpu_req_cmd === M_PFR) || (r_cpu_req_cmd === M_PFW)
  val r_req_amo   = r_cpu_req_cmd(3).toBool
  val r_req_read  = r_req_load || r_req_amo
  val r_req_write = r_req_store || r_req_amo
  val r_req_readwrite = r_req_read || r_req_write || r_req_prefetch
  val nack_hit = Bool()

  val wb = new WritebackUnit(co)
  val prober = new ProbeUnit(co)
  val mshr = new MSHRFile(co)
  val flusher = new FlushUnit(lines, co)
  val replay_amo_val = mshr.io.data_req.valid && mshr.io.data_req.bits.cmd(3).toBool

  // reset and flush unit
  val flushed = Reg(resetVal = Bool(true))
  flushed := flushed && (!r_cpu_req_val || r_req_flush) || r_cpu_req_val && r_req_flush && mshr.io.fence_rdy && flusher.io.req.ready
  flusher.io.req.valid := r_cpu_req_val && r_req_flush && mshr.io.fence_rdy && !flushed
  flusher.io.mshr_req.ready := mshr.io.req.ready
  
  when (io.cpu.req.valid) {
    r_cpu_req_idx  := io.cpu.req.bits.idx
    r_cpu_req_cmd  := io.cpu.req.bits.cmd
    r_cpu_req_type := io.cpu.req.bits.typ
    r_cpu_req_tag  := io.cpu.req.bits.tag
  }
  when (prober.io.meta_req.valid) {
    r_cpu_req_idx := Cat(prober.io.meta_req.bits.data.tag, prober.io.meta_req.bits.idx, mshr.io.data_req.bits.offset)(PGIDX_BITS-1,0)
  }
  when (replay_amo_val) {
    r_cpu_req_idx  := Cat(mshr.io.data_req.bits.idx, mshr.io.data_req.bits.offset)
    r_cpu_req_cmd  := mshr.io.data_req.bits.cmd
    r_cpu_req_type := mshr.io.data_req.bits.typ
    r_amo_replay_data := mshr.io.data_req.bits.data
    r_way_oh       := mshr.io.data_req.bits.way_oh
  }
  when (flusher.io.meta_req.valid) {
    r_cpu_req_idx := Cat(flusher.io.meta_req.bits.idx, mshr.io.data_req.bits.offset)
    r_cpu_req_cmd := M_FLA
    r_way_oh := flusher.io.meta_req.bits.way_en
  }
  val cpu_req_data = Mux(r_replay_amo, r_amo_replay_data, io.cpu.req.bits.data)

  val misaligned =
    (((r_cpu_req_type === MT_H) || (r_cpu_req_type === MT_HU)) && (r_cpu_req_idx(0) != Bits(0))) ||
    (((r_cpu_req_type === MT_W) || (r_cpu_req_type === MT_WU)) && (r_cpu_req_idx(1,0) != Bits(0))) ||
    ((r_cpu_req_type === MT_D) && (r_cpu_req_idx(2,0) != Bits(0)));
    
  io.cpu.xcpt.ma.ld := r_cpu_req_val_ && !early_nack && r_req_read && misaligned
  io.cpu.xcpt.ma.st := r_cpu_req_val_ && !early_nack && r_req_write && misaligned

  // tags
  val meta = new MetaDataArrayArray(lines)
  val meta_arb = (new Arbiter(4)) { new MetaArrayReq() }
  flusher.io.meta_req <> meta_arb.io.in(0)
  meta_arb.io.out <> meta.io.req

  // data
  val data = new DataArrayArray(lines)
  val data_arb = (new Arbiter(5)) { new DataArrayReq() }
  data_arb.io.out <> data.io.req

  // cpu tag check
  meta_arb.io.in(3).valid := io.cpu.req.valid
  meta_arb.io.in(3).bits.idx := io.cpu.req.bits.idx(indexmsb,indexlsb)
  meta_arb.io.in(3).bits.rw := Bool(false)
  meta_arb.io.in(3).bits.way_en := ~UFix(0, NWAYS)
  val early_tag_nack = !meta_arb.io.in(3).ready
  val cpu_req_ppn = Mux(prober.io.mshr_req.valid, prober.io.address >> UFix(PGIDX_BITS-OFFSET_BITS), io.cpu.req.bits.ppn)
  val cpu_req_tag = Cat(cpu_req_ppn, r_cpu_req_idx)(tagmsb,taglsb)
  val tag_match_arr = (0 until NWAYS).map( w => co.isValid(meta.io.resp(w).state) && (meta.io.resp(w).tag === cpu_req_tag))
  val tag_match = Cat(Bits(0),tag_match_arr:_*).orR
  val tag_match_way_oh = Cat(Bits(0),tag_match_arr.reverse:_*)(NWAYS-1, 0) //TODO: use Vec
  val tag_hit_arr = (0 until NWAYS).map( w => co.isHit(r_cpu_req_cmd, meta.io.resp(w).state) && (meta.io.resp(w).tag === cpu_req_tag))
  val tag_hit = Cat(Bits(0),tag_hit_arr:_*).orR
  val meta_resp_way_oh = Mux(meta.io.way_en === ~UFix(0, NWAYS), tag_match_way_oh, meta.io.way_en)
  val data_resp_way_oh = Mux(data.io.way_en === ~UFix(0, NWAYS), tag_match_way_oh, data.io.way_en)
  val meta_resp_mux = Mux1H(meta_resp_way_oh, meta.io.resp)
  val data_resp_mux = Mux1H(data_resp_way_oh, data.io.resp)

  // writeback unit
  wb.io.req <> mshr.io.wb_req
  wb.io.data_req <> data_arb.io.in(3)
  wb.io.data_resp <> data_resp_mux
  wb.io.probe_rep_data <> io.mem.probe_rep_data

  // replacement policy
  val replacer = new RandomReplacementWayGen()
  replacer.io.way_en := ~UFix(0, NWAYS)
  val replaced_way_oh = Mux(flusher.io.mshr_req.valid, r_way_oh, UFixToOH(replacer.io.way_id, NWAYS))
  val meta_wb_mux = Mux1H(replaced_way_oh, meta.io.resp)

  // refill response
  data_arb.io.in(0).bits.offset := mshr.io.mem_resp_offset
  data_arb.io.in(0).bits.idx := mshr.io.mem_resp_idx
  data_arb.io.in(0).bits.rw := Bool(true)
  data_arb.io.in(0).bits.wmask := ~UFix(0, MEM_DATA_BITS/8)
  data_arb.io.in(0).bits.data := io.mem.xact_rep.bits.data
  data_arb.io.in(0).bits.way_en := mshr.io.mem_resp_way_oh
  data_arb.io.in(0).valid := io.mem.xact_rep.valid && co.messageUpdatesDataArray(io.mem.xact_rep.bits)

  // load hits
  data_arb.io.in(4).bits.offset := io.cpu.req.bits.idx(offsetmsb,ramindexlsb)
  data_arb.io.in(4).bits.idx := io.cpu.req.bits.idx(indexmsb,indexlsb)
  data_arb.io.in(4).bits.rw := Bool(false)
  data_arb.io.in(4).valid := io.cpu.req.valid && req_read
  data_arb.io.in(4).bits.way_en := ~UFix(0, NWAYS) // intiate load on all ways, mux after tag check
  val early_load_nack = req_read && !data_arb.io.in(4).ready

  // store hits and AMO hits and misses use a pending store register.
  // we nack new stores if a pending store can't retire for some reason.
  // we drain a pending store if the CPU performs a store or a
  // conflictig load, or if the cache is idle, or after a miss.
  val p_store_idx_match = p_store_valid && (r_cpu_req_idx(indexmsb,indexlsb) === p_store_idx(indexmsb,indexlsb))
  val p_store_offset_match = (r_cpu_req_idx(indexlsb-1,offsetlsb) === p_store_idx(indexlsb-1,offsetlsb))
  val p_store_match = r_cpu_req_val_ && r_req_read && p_store_idx_match && p_store_offset_match
  val drain_store_val = (p_store_valid && (!io.cpu.req.valid || req_write || wb.io.data_req.valid || mshr.io.data_req.valid)) || p_store_match
  data_arb.io.in(2).bits.offset := p_store_idx(offsetmsb,ramindexlsb)
  data_arb.io.in(2).bits.idx := p_store_idx(indexmsb,indexlsb)
  data_arb.io.in(2).bits.rw := Bool(true)
  data_arb.io.in(2).valid := drain_store_val
  data_arb.io.in(2).bits.way_en :=  p_store_way_oh
  val drain_store = drain_store_val && data_arb.io.in(2).ready
  val p_amo = Reg(resetVal = Bool(false))
  val p_store_rdy = !(p_store_valid && !drain_store) && !(mshr.io.data_req.valid || r_replay_amo || p_amo)
  p_amo := r_cpu_req_val && tag_hit && r_req_amo && mshr.io.req.ready && !nack_hit || r_replay_amo
  p_store_valid := p_store_valid && !drain_store || (r_cpu_req_val && tag_hit && r_req_store && mshr.io.req.ready && !nack_hit) || p_amo

  // tag update after a store to an exclusive clean line.
  val new_hit_state = co.newStateOnHit(r_cpu_req_cmd, meta_resp_mux.state)
  val set_hit_state  = r_cpu_req_val && tag_hit && meta_resp_mux.state != new_hit_state
  meta.io.state_req.bits.rw := Bool(true)
  meta.io.state_req.bits.idx := Reg(r_cpu_req_idx(indexmsb,indexlsb))
  meta.io.state_req.bits.data.state := Reg(new_hit_state)
  meta.io.state_req.bits.way_en := Reg(tag_match_way_oh)
  meta.io.state_req.valid := Reg(set_hit_state, resetVal = Bool(false))
  
  // pending store data, also used for AMO RHS
  val amoalu = new AMOALU
  when (r_cpu_req_val_ && r_req_write && p_store_rdy || r_replay_amo) {
    p_store_idx    := r_cpu_req_idx
    p_store_type   := r_cpu_req_type
    p_store_cmd    := r_cpu_req_cmd
    p_store_way_oh := Mux(r_replay_amo, r_way_oh, tag_match_way_oh)
    p_store_data   := cpu_req_data
  }
  when (p_amo) {
    p_store_data := amoalu.io.out
  }

  // miss handling
  mshr.io.req.valid := r_cpu_req_val && r_req_readwrite && !nack_hit || flusher.io.mshr_req.valid
  mshr.io.req.bits.tag_miss := !tag_hit || flusher.io.mshr_req.valid
  mshr.io.req.bits.old_dirty := co.needsWriteback(meta_wb_mux.state) && (!tag_match || flusher.io.mshr_req.valid) // don't wb upgrades
  mshr.io.req.bits.old_tag := meta_wb_mux.tag
  mshr.io.req.bits.tag := cpu_req_tag
  mshr.io.req.bits.idx := r_cpu_req_idx(indexmsb,indexlsb)
  mshr.io.req.bits.cpu_tag := r_cpu_req_tag
  mshr.io.req.bits.offset := r_cpu_req_idx(offsetmsb,0)
  mshr.io.req.bits.cmd := r_cpu_req_cmd
  mshr.io.req.bits.typ := r_cpu_req_type
  mshr.io.req.bits.way_oh := Mux(tag_match && !flusher.io.mshr_req.valid, tag_match_way_oh, replaced_way_oh)
  mshr.io.req.bits.data := cpu_req_data

  mshr.io.mem_rep <> io.mem.xact_rep
  mshr.io.mem_abort.valid := io.mem.xact_abort.valid
  mshr.io.mem_abort.bits := io.mem.xact_abort.bits
  io.mem.xact_abort.ready := Bool(true)
  mshr.io.meta_req <> meta_arb.io.in(1)
  replacer.io.pick_new_way := mshr.io.req.valid && mshr.io.req.ready

  // replays
  val replay = mshr.io.data_req.bits
  val stall_replay = r_replay_amo || p_amo || flusher.io.meta_req.valid || p_store_valid
  val replay_val = mshr.io.data_req.valid
  val replay_fire = replay_val && !stall_replay
  val replay_rdy = data_arb.io.in(1).ready && !stall_replay
  data_arb.io.in(1).bits.offset := replay.offset(offsetmsb,ramindexlsb)
  data_arb.io.in(1).bits.idx := replay.idx
  data_arb.io.in(1).bits.rw := replay.cmd === M_XWR
  data_arb.io.in(1).valid := replay_fire
  data_arb.io.in(1).bits.way_en := mshr.io.data_req.bits.way_oh
  mshr.io.data_req.ready := replay_rdy
  r_replay_amo := replay_amo_val && replay_rdy

  // probes
  prober.io.req <> io.mem.probe_req
  prober.io.rep <> io.mem.probe_rep
  prober.io.mshr_req <> mshr.io.probe
  prober.io.wb_req <> wb.io.probe
  prober.io.tag_match_way_oh := tag_match_way_oh
  prober.io.line_state := meta_resp_mux.state
  prober.io.meta_req.ready := meta_arb.io.in(2).ready && !replay_amo_val
  meta_arb.io.in(2).valid := prober.io.meta_req.valid
  meta_arb.io.in(2).bits := prober.io.meta_req.bits

  // store write mask generation.
  // assumes store replays are higher-priority than pending stores.
  val maskgen = new StoreMaskGen
  val store_offset = Mux(!replay_fire, p_store_idx(offsetmsb,0), replay.offset)
  maskgen.io.typ := Mux(!replay_fire, p_store_type, replay.typ)
  maskgen.io.addr := store_offset(offsetlsb-1,0)
  val store_wmask_wide = maskgen.io.wmask << Cat(store_offset(ramindexlsb-1,offsetlsb), Bits(0, log2Up(CPU_DATA_BITS/8))).toUFix
  val store_data = Mux(!replay_fire, p_store_data, replay.data)
  val store_data_wide = Fill(MEM_DATA_BITS/CPU_DATA_BITS, store_data)
  data_arb.io.in(1).bits.data := store_data_wide
  data_arb.io.in(1).bits.wmask := store_wmask_wide
  data_arb.io.in(2).bits.data := store_data_wide
  data_arb.io.in(2).bits.wmask := store_wmask_wide

  // load data subword mux/sign extension.
  // subword loads are delayed by one cycle.
  val loadgen = new LoadDataGen
  val loadgen_use_replay = Reg(replay_fire)
  loadgen.io.typ := Mux(loadgen_use_replay, Reg(replay.typ), r_cpu_req_type)
  loadgen.io.addr := Mux(loadgen_use_replay, Reg(replay.offset), r_cpu_req_idx)(ramindexlsb-1,0)
  loadgen.io.din := data_resp_mux

  amoalu.io.cmd := p_store_cmd
  amoalu.io.typ := p_store_type
  amoalu.io.lhs := loadgen.io.r_dout.toUFix
  amoalu.io.rhs := p_store_data.toUFix

  early_nack := early_tag_nack || early_load_nack || r_cpu_req_val && r_req_amo || replay_amo_val || r_replay_amo

  // we usually nack rather than reporting that the cache is not ready.
  // fences and flushes are the exceptions.
  val pending_fence = Reg(resetVal = Bool(false))
  pending_fence := (r_cpu_req_val_ && r_req_fence || pending_fence) && !mshr.io.fence_rdy
  nack_hit := p_store_match || replay_val || r_req_write && !p_store_rdy ||
              p_store_idx_match && meta.io.state_req.valid
  val nack_miss  = !mshr.io.req.ready
  val nack_flush = !mshr.io.fence_rdy && (r_req_fence || r_req_flush) ||
                   !flushed && r_req_flush
  val nack = early_nack || r_req_readwrite && (nack_hit || nack_miss) || nack_flush

  io.cpu.req.ready   := flusher.io.req.ready && !(r_cpu_req_val_ && r_req_flush) && !pending_fence
  io.cpu.resp.valid  := (r_cpu_req_val && tag_hit && !mshr.io.secondary_miss && !nack && r_req_read) || mshr.io.cpu_resp_val
  io.cpu.resp.bits.nack := r_cpu_req_val_ && !io.cpu.req.bits.kill && nack
  io.cpu.resp.bits.replay := mshr.io.cpu_resp_val
  io.cpu.resp.bits.miss := r_cpu_req_val_ && (!tag_hit || mshr.io.secondary_miss) && r_req_read
  io.cpu.resp.bits.tag  := Mux(mshr.io.cpu_resp_val, mshr.io.cpu_resp_tag, r_cpu_req_tag)
  io.cpu.resp.bits.typ := loadgen.io.typ
  io.cpu.resp.bits.data := loadgen.io.dout
  io.cpu.resp.bits.data_subword := loadgen.io.r_dout_subword
  
  val xact_init_arb = (new Arbiter(2)) { new TransactionInit }
  xact_init_arb.io.in(0) <> wb.io.mem_req
  xact_init_arb.io.in(1).valid := mshr.io.mem_req.valid && prober.io.req.ready
  mshr.io.mem_req.ready := xact_init_arb.io.in(1).ready && prober.io.req.ready
  xact_init_arb.io.in(1).bits := mshr.io.mem_req.bits
  io.mem.xact_init <> xact_init_arb.io.out

  io.mem.xact_init_data <> wb.io.mem_req_data
  io.mem.xact_finish <> mshr.io.mem_finish
}
