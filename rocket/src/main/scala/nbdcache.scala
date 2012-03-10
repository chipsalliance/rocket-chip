package rocket

import Chisel._
import Constants._

class ioReplacementWayGen extends Bundle {
  val pick_new_way = Bool(dir = INPUT)
  val way_en = Bits(width = NWAYS, dir = INPUT)
  val way_id = UFix(width = log2up(NWAYS), dir = OUTPUT)
}

class RandomReplacementWayGen extends Component {
  val io = new ioReplacementWayGen()
  //TODO: Actually limit selection based on which ways are allowed (io.ways_en)
  io.way_id := UFix(0)
  if(NWAYS > 1) 
  {
    val rand_way_id = LFSR16(io.pick_new_way)(log2up(NWAYS)-1,0)
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
  val byte = (io.typ === MT_B) || (io.typ === MT_BU)

  io.wmask := Mux(byte, Bits(  1,1) <<     io.addr(2,0).toUFix,
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
  val byte = (io.typ === MT_B) || (io.typ === MT_BU)

  io.dout := Mux(byte, Fill(8, io.din( 7,0)),
             Mux(half, Fill(4, io.din(15,0)),
             Mux(word, Fill(2, io.din(31,0)),
                       io.din)))
}

// this currently requires that CPU_DATA_BITS == 64
class LoadDataGen extends Component {
  val io = new Bundle {
    val typ  = Bits(3, INPUT)
    val addr = Bits(log2up(MEM_DATA_BITS/8), INPUT)
    val din  = Bits(MEM_DATA_BITS, INPUT)
    val dout = Bits(64, OUTPUT)
    val r_dout = Bits(64, OUTPUT)
    val r_dout_subword = Bits(64, OUTPUT)
  }

  val sext = (io.typ === MT_B) || (io.typ === MT_H) ||
             (io.typ === MT_W) || (io.typ === MT_D)
  val word = (io.typ === MT_W) || (io.typ === MT_WU)
  val half = (io.typ === MT_H) || (io.typ === MT_HU)
  val byte = (io.typ === MT_B) || (io.typ === MT_BU)

  val shifted = io.din >> Cat(io.addr(io.addr.width-1,2), Bits(0, 5)).toUFix
  val extended =
    Mux(word, Cat(Fill(32, sext & shifted(31)), shifted(31,0)), shifted)

  val r_extended = Reg(extended)
  val r_sext = Reg(sext)
  val r_half = Reg(half)
  val r_byte = Reg(byte)
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
  val old_state = UFix(width = 2)
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
  val sdq_id = UFix(width = log2up(NSDQ))
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
  val idx    = Bits(width = IDX_BITS)
  val offset = Bits(width = log2up(REFILL_CYCLES))
  val rw     = Bool()
  val wmask  = Bits(width = MEM_DATA_BITS/8)
  val data   = Bits(width = MEM_DATA_BITS)
}

class DataArrayArrayReq extends Bundle {
  val inner_req = new DataArrayReq()
  val way_en = Bits(width = NWAYS)
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
  val idx  = Bits(width = IDX_BITS)
  val rw  = Bool()
  val data = new MetaData()
}

class MetaArrayArrayReq extends Bundle {
  val inner_req = new MetaArrayReq()
  val way_en = Bits(width = NWAYS)
}

class MSHR(id: Int) extends Component with FourStateCoherence {
  val io = new Bundle {
    val req_pri_val    = Bool(INPUT)
    val req_pri_rdy    = Bool(OUTPUT)
    val req_sec_val    = Bool(INPUT)
    val req_sec_rdy    = Bool(OUTPUT)
    val req_bits       = new MSHRReq().asInput
    val req_sdq_id     = UFix(log2up(NSDQ), INPUT)

    val idx_match      = Bool(OUTPUT)
    val idx            = Bits(IDX_BITS, OUTPUT)
    val refill_count   = Bits(log2up(REFILL_CYCLES), OUTPUT)
    val tag            = Bits(TAG_BITS, OUTPUT)
    val way_oh         = Bits(NWAYS, OUTPUT)

    val mem_req  = (new ioDecoupled) { new TransactionInit }
    val meta_req = (new ioDecoupled) { new MetaArrayArrayReq() }
    val replay   = (new ioDecoupled) { new Replay()   }
    val mem_abort = (new ioPipe) { new TransactionAbort }.flip
    val mem_rep = (new ioPipe) { new TransactionReply }.flip
    val mem_finish = (new ioDecoupled) { new TransactionFinish }
    val wb_req = (new ioDecoupled) { new WritebackReq }
  }

  val s_invalid :: s_wb_req :: s_wb_resp :: s_refill_req :: s_refill_resp :: s_drain_rpq :: Nil = Enum(6) { UFix() }
  val state = Reg(resetVal = s_invalid)
  val flush = Reg { Bool() }

  val xact_type = Reg { UFix() }
  val line_state = Reg { UFix() }
  val refill_count = Reg { UFix(width = log2up(REFILL_CYCLES)) }
  val req = Reg { new MSHRReq() }

  val req_cmd = io.req_bits.cmd
  val req_use_rpq = (req_cmd != M_PFR) && (req_cmd != M_PFW) && (req_cmd != M_FLA)
  val idx_match = req.idx === io.req_bits.idx
  val sec_rdy = idx_match && !flush && (state === s_wb_req || state === s_wb_resp || (state === s_refill_req || state === s_refill_resp) && !needsSecondaryXact(req_cmd, io.mem_req.bits))

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
      line_state := newStateOnTransactionRep(io.mem_rep.bits, io.mem_req.bits)
    }
    when (abort) { state := s_refill_req }
  }
  when (state === s_refill_req && io.mem_req.ready) {
    state := Mux(flush, s_drain_rpq, s_refill_resp)
  }
  when (state === s_wb_resp) {
    when (reply) { state := s_refill_req }
    when (abort) { state := s_wb_req }
  }
  when (state === s_wb_req && io.wb_req.ready) {
    state := s_wb_resp
  }

  when (io.req_sec_val && io.req_sec_rdy) { // s_wb_req, s_wb_resp, s_refill_req
    xact_type := newTransactionOnSecondaryMiss(req_cmd, newStateOnFlush(), io.mem_req.bits)
  }
  when ((state === s_invalid) && io.req_pri_val) {
    flush := req_cmd === M_FLA
    line_state := newStateOnFlush()
    refill_count := UFix(0)
    xact_type := newTransactionOnPrimaryMiss(req_cmd, newStateOnFlush())
    req := io.req_bits

    when (io.req_bits.tag_miss) {
      state := Mux(needsWriteback(io.req_bits.old_state), s_wb_req, s_refill_req)
    }
  }

  io.idx_match := (state != s_invalid) && idx_match
  io.idx := req.idx
  io.tag := req.tag
  io.way_oh := req.way_oh
  io.refill_count := refill_count
  io.req_pri_rdy := (state === s_invalid)
  io.req_sec_rdy := sec_rdy && rpq.io.enq.ready

  io.meta_req.valid := (state === s_drain_rpq) && !rpq.io.deq.valid && !finish_q.io.deq.valid
  io.meta_req.bits.inner_req.rw := Bool(true)
  io.meta_req.bits.inner_req.idx := req.idx
  io.meta_req.bits.inner_req.data.state := line_state
  io.meta_req.bits.inner_req.data.tag := req.tag
  io.meta_req.bits.way_en := req.way_oh

  io.wb_req.valid := (state === s_wb_req)
  io.wb_req.bits.tag := req.old_tag
  io.wb_req.bits.idx := req.idx
  io.wb_req.bits.way_oh := req.way_oh
  io.wb_req.bits.tile_xact_id := Bits(id)

  io.mem_req.valid := (state === s_refill_req) && !flush
  io.mem_req.bits.t_type := xact_type
  io.mem_req.bits.address := Cat(req.tag, req.idx).toUFix
  io.mem_req.bits.tile_xact_id := Bits(id)
  io.mem_finish <> finish_q.io.deq

  io.replay.valid := (state === s_drain_rpq) && rpq.io.deq.valid
  io.replay.bits <> rpq.io.deq.bits
  io.replay.bits.idx := req.idx
  io.replay.bits.way_oh := req.way_oh
}

class MSHRFile extends Component {
  val io = new Bundle {
    val req = (new ioDecoupled) { new MSHRReq }.flip

    val mem_resp_idx = Bits(IDX_BITS, OUTPUT)
    val mem_resp_offset = Bits(log2up(REFILL_CYCLES), OUTPUT)
    val mem_resp_way_oh = Bits(NWAYS, OUTPUT)

    val fence_rdy = Bool(OUTPUT)

    val mem_req  = (new ioDecoupled) { new TransactionInit }
    val meta_req = (new ioDecoupled) { new MetaArrayArrayReq() }
    val data_req   = (new ioDecoupled) { new DataReq() }
    val mem_abort = (new ioPipe) { new TransactionAbort }.flip
    val mem_rep = (new ioPipe) { new TransactionReply }.flip
    val mem_finish = (new ioDecoupled) { new TransactionFinish }
    val wb_req = (new ioDecoupled) { new WritebackReq }

    val cpu_resp_val = Bool(OUTPUT)
    val cpu_resp_tag = Bits(DCACHE_TAG_BITS, OUTPUT)
  }

  val sdq_val = Reg(resetVal = Bits(0, NSDQ))
  val sdq_alloc_id = PriorityEncoder(~sdq_val(NSDQ-1,0))
  val sdq_rdy = !sdq_val.andR
  val (req_read, req_write) = cpuCmdToRW(io.req.bits.cmd)
  val sdq_enq = io.req.valid && io.req.ready && req_write
  val sdq = Mem(NSDQ, sdq_enq, sdq_alloc_id, io.req.bits.data)
  sdq.setReadLatency(1);
  sdq.setTarget('inst)

  val tag_mux = (new Mux1H(NMSHR)){ Bits(width = TAG_BITS) }
  val mem_resp_mux = (new Mux1H(NMSHR)){ new DataArrayArrayReq }
  val meta_req_arb = (new Arbiter(NMSHR)) { new MetaArrayArrayReq() }
  val mem_req_arb = (new Arbiter(NMSHR)) { new TransactionInit }
  val mem_finish_arb = (new Arbiter(NMSHR)) { new TransactionFinish }
  val wb_req_arb = (new Arbiter(NMSHR)) { new WritebackReq }
  val replay_arb = (new Arbiter(NMSHR)) { new Replay() }
  val alloc_arb = (new Arbiter(NMSHR)) { Bool() }

  val tag_match = tag_mux.io.out === io.req.bits.tag

  var idx_match = Bool(false)
  var pri_rdy = Bool(false)
  var fence = Bool(false)
  var sec_rdy = Bool(false)

  for (i <- 0 to NMSHR-1) {
    val mshr = new MSHR(i)

    tag_mux.io.sel(i) := mshr.io.idx_match
    tag_mux.io.in(i) := mshr.io.tag

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

    mshr.io.mem_abort <> io.mem_abort
    mshr.io.mem_rep <> io.mem_rep
    mem_resp_mux.io.sel(i) := UFix(i) === io.mem_rep.bits.tile_xact_id
    mem_resp_mux.io.in(i).inner_req.idx := mshr.io.idx
    mem_resp_mux.io.in(i).inner_req.offset := mshr.io.refill_count
    mem_resp_mux.io.in(i).way_en := mshr.io.way_oh

    pri_rdy = pri_rdy || mshr.io.req_pri_rdy
    sec_rdy = sec_rdy || mshr.io.req_sec_rdy
    fence = fence || !mshr.io.req_pri_rdy
    idx_match = idx_match || mshr.io.idx_match
  }

  alloc_arb.io.out.ready := io.req.valid && sdq_rdy && !idx_match

  meta_req_arb.io.out <> io.meta_req
  mem_req_arb.io.out <> io.mem_req
  mem_finish_arb.io.out <> io.mem_finish
  wb_req_arb.io.out <> io.wb_req

  io.req.ready := Mux(idx_match, tag_match && sec_rdy, pri_rdy) && sdq_rdy
  io.mem_resp_idx := mem_resp_mux.io.out.inner_req.idx
  io.mem_resp_offset := mem_resp_mux.io.out.inner_req.offset
  io.mem_resp_way_oh := mem_resp_mux.io.out.way_en
  io.fence_rdy := !fence

  val replay = Queue(replay_arb.io.out, 1, pipe = true)
  replay.ready := io.data_req.ready
  io.data_req <> replay

  val (replay_read, replay_write) = cpuCmdToRW(replay.bits.cmd)
  val sdq_free = replay.valid && replay.ready && replay_write
  sdq_val := sdq_val & ~(sdq_free.toUFix << replay.bits.sdq_id) | 
             PriorityEncoderOH(~sdq_val(NSDQ-1,0)) & Fill(NSDQ, sdq_enq && io.req.bits.tag_miss)
  io.data_req.bits.data := sdq.read(Mux(replay.valid && !replay.ready, replay.bits.sdq_id, replay_arb.io.out.bits.sdq_id))

  io.cpu_resp_val := Reg(replay.valid && replay.ready && replay_read, resetVal = Bool(false))
  io.cpu_resp_tag := Reg(replay.bits.cpu_tag)
}

class WritebackUnit extends Component {
  val io = new Bundle {
    val req    = (new ioDecoupled) { new WritebackReq() }.flip
    val data_req  = (new ioDecoupled) { new DataArrayArrayReq() }
    val data_resp = Bits(MEM_DATA_BITS, INPUT)
    val mem_req = (new ioDecoupled) { new TransactionInit }
    val mem_req_data = (new ioDecoupled) { new TransactionInitData }
  }

  val valid = Reg(resetVal = Bool(false))
  val data_req_fired = Reg(resetVal = Bool(false))
  val cmd_sent = Reg() { Bool() }
  val cnt = Reg() { UFix(width = log2up(REFILL_CYCLES+1)) }
  val req = Reg() { new WritebackReq() }

  data_req_fired := Bool(false)
  when (valid && io.mem_req.ready) {
    cmd_sent := Bool(true)
  }
  when (io.data_req.valid && io.data_req.ready) {
    data_req_fired := Bool(true)
    cnt := cnt + UFix(1)
  }
  when (data_req_fired && !io.mem_req_data.ready) {
    data_req_fired := Bool(false)
    cnt := cnt - UFix(1)
  }
  when ((cnt === UFix(REFILL_CYCLES)) && io.mem_req_data.ready) {
    valid := Bool(false)
  }
  when (io.req.valid && io.req.ready) {
    valid := Bool(true)
    cmd_sent := Bool(false)
    cnt := UFix(0)
    req := io.req.bits
  }

  io.req.ready := !valid
  io.data_req.valid := valid && (cnt < UFix(REFILL_CYCLES))
  io.data_req.bits.way_en := req.way_oh
  io.data_req.bits.inner_req.idx := req.idx
  io.data_req.bits.inner_req.offset := cnt
  io.data_req.bits.inner_req.rw := Bool(false)
  io.data_req.bits.inner_req.wmask := Bits(0)
  io.data_req.bits.inner_req.data := Bits(0)

  io.mem_req.valid := valid && !cmd_sent
  io.mem_req.bits.t_type := X_INIT_WRITE_UNCACHED
  io.mem_req.bits.address := Cat(req.tag, req.idx).toUFix
  io.mem_req.bits.tile_xact_id := req.tile_xact_id
  io.mem_req_data.valid := data_req_fired
  io.mem_req_data.bits.data := io.data_resp
}

class FlushUnit(lines: Int) extends Component with FourStateCoherence{
  val io = new Bundle {
    val req = (new ioDecoupled) { Bool() }.flip
    val meta_req = (new ioDecoupled) { new MetaArrayArrayReq() }
    val mshr_req = (new ioDecoupled) { Bool() }
  }
  
  val s_reset :: s_ready :: s_meta_read :: s_meta_wait :: Nil = Enum(4) { UFix() }
  val state = Reg(resetVal = s_reset)
  val idx_cnt = Reg(resetVal = UFix(0, log2up(lines)))
  val next_idx_cnt = idx_cnt + UFix(1)
  val way_cnt = if (NWAYS == 1) UFix(0) else Reg(resetVal = UFix(0, log2up(NWAYS)))
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
  io.meta_req.bits.inner_req.idx := idx_cnt
  io.meta_req.bits.inner_req.rw := (state === s_reset)
  io.meta_req.bits.inner_req.data.state := newStateOnFlush()
  io.meta_req.bits.inner_req.data.tag := UFix(0)
}

class MetaDataArray(lines: Int) extends Component {
  val io = new Bundle {
    val req  = (new ioDecoupled) { new MetaArrayReq() }.flip
    val resp = (new MetaData).asOutput()
    val state_req = (new ioDecoupled) { new MetaArrayReq() }.flip
  }

  val permissions_array = Mem(lines){ UFix(width = 2) }
  permissions_array.write(io.state_req.bits.idx, io.state_req.bits.data.state, io.state_req.valid && io.state_req.bits.rw)
  permissions_array.write(io.req.bits.idx, io.req.bits.data.state, io.req.valid && io.req.bits.rw)
  val raddr = Reg() { Bits() }
  when (io.req.valid && !io.req.bits.rw) { raddr := io.req.bits.idx }
  val permissions_rdata1 = permissions_array.read(raddr)

  val tag_array = Mem(lines){ Bits(width=TAG_BITS) }
  tag_array.setReadLatency(1);
  tag_array.setTarget('inst)
  val tag_rdata = tag_array.rw(io.req.bits.idx, io.req.bits.data.tag, io.req.valid && io.req.bits.rw, cs = io.req.valid)

  io.resp.state := permissions_rdata1.toUFix
  io.resp.tag   := tag_rdata
  io.req.ready  := Bool(true)
}

class MetaDataArrayArray(lines: Int) extends Component {
  val io = new Bundle {
    val req  = (new ioDecoupled) { new MetaArrayArrayReq() }.flip
    val resp = Vec(NWAYS){ (new MetaData).asOutput }
    val state_req = (new ioDecoupled) { new MetaArrayArrayReq() }.flip
    val way_en = Bits(width = NWAYS, dir = OUTPUT)
  }

  val way_en_ = Reg { Bits(width=NWAYS) }
  when (io.req.valid && io.req.ready) {
    way_en_ := io.req.bits.way_en
  }

  var tag_ready = Bool(true)
  var state_ready = Bool(true)
  for(w <- 0 until NWAYS) {
    val way = new MetaDataArray(lines)
    way.io.req.bits <> io.req.bits.inner_req
    tag_ready = tag_ready && way.io.req.ready
    way.io.req.valid := io.req.valid && io.req.bits.way_en(w).toBool
    way.io.state_req.bits <> io.state_req.bits.inner_req
    state_ready = state_ready && way.io.state_req.ready
    way.io.state_req.valid := io.state_req.valid && io.state_req.bits.way_en(w).toBool
    way.io.resp <> io.resp(w)
  }

  io.way_en := way_en_
  io.req.ready := tag_ready
  io.state_req.ready := state_ready
}

class DataArray(lines: Int) extends Component {
  val io = new Bundle {
    val req  = (new ioDecoupled) { new DataArrayReq() }.flip
    val resp = Bits(width = MEM_DATA_BITS, dir = OUTPUT)
  }

  val wmask = FillInterleaved(8, io.req.bits.wmask)

  val array = Mem(lines*REFILL_CYCLES){ Bits(width=MEM_DATA_BITS) }
  array.setReadLatency(1);
  array.setTarget('inst)
  val addr = Cat(io.req.bits.idx, io.req.bits.offset)
  val rdata = array.rw(addr, io.req.bits.data, io.req.valid && io.req.bits.rw, wmask, cs = io.req.valid)
  io.resp := rdata
  io.req.ready := Bool(true)
}

class DataArrayArray(lines: Int) extends Component {
  val io = new Bundle {
    val req  = (new ioDecoupled) { new DataArrayArrayReq() }.flip
    val resp = Vec(NWAYS){ Bits(width = MEM_DATA_BITS, dir = OUTPUT) }
    val way_en = Bits(width = NWAYS, dir = OUTPUT)
  }

  val way_en_ = Reg { Bits(width=NWAYS) }
  when (io.req.valid && io.req.ready) {
    way_en_ := io.req.bits.way_en
  }

  //val data_ready_arr = Vec(NWAYS){ Bool() }
  var data_ready = Bool(true)
  for(w <- 0 until NWAYS) {
    val way = new DataArray(lines)
    way.io.req.bits <> io.req.bits.inner_req
    //data_ready_arr(w) := way.io.req.ready
    data_ready = data_ready && way.io.req.ready
    way.io.req.valid := io.req.valid && io.req.bits.way_en(w).toBool
    way.io.resp <> io.resp(w)
  }

  io.way_en := way_en_
  //io.req.ready := Cat(data_ready_arr).andR.toBool
  io.req.ready := data_ready
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

// interface between D$ and processor/DTLB
class ioDmem(view: List[String] = null) extends Bundle(view) {
  val req_kill  = Bool(INPUT);
  val req_val   = Bool(INPUT);
  val req_rdy   = Bool(OUTPUT);
  val req_cmd   = Bits(4, INPUT);
  val req_type  = Bits(3, INPUT);
  val req_idx   = Bits(PGIDX_BITS, INPUT);
  val req_ppn   = Bits(PPN_BITS, INPUT);
  val req_data  = Bits(64, INPUT);
  val req_tag   = Bits(DCACHE_TAG_BITS, INPUT);
  val xcpt_ma_ld  = Bool(OUTPUT); // misaligned load
  val xcpt_ma_st = Bool(OUTPUT); // misaligned store
  val resp_miss = Bool(OUTPUT);
  val resp_nack = Bool(OUTPUT);
  val resp_val  = Bool(OUTPUT);
  val resp_replay = Bool(OUTPUT);
  val resp_type = Bits(3, OUTPUT);
  val resp_data = Bits(64, OUTPUT);
  val resp_data_subword = Bits(64, OUTPUT);
  val resp_tag  = Bits(DCACHE_TAG_BITS, OUTPUT);
}
 
abstract class HellaCache extends Component {
  def isHit ( cmd: Bits, state: UFix): Bool
  def isValid (state: UFix): Bool
  def needsWriteback (state: UFix): Bool
  def newStateOnWriteback(): UFix
  def newStateOnFlush(): UFix
  def newStateOnHit(cmd: Bits, state: UFix): UFix
}

class HellaCacheUniproc extends HellaCache with FourStateCoherence {
  val io = new Bundle {
    val cpu = new ioDmem()
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
  val offsetlsb   = log2up(CPU_DATA_BITS/8)
  val ramindexlsb = log2up(MEM_DATA_BITS/8)
  
  val early_nack       = Reg { Bool() }
  val r_cpu_req_val_   = Reg(io.cpu.req_val && io.cpu.req_rdy, resetVal = Bool(false))
  val r_cpu_req_val    = r_cpu_req_val_ && !io.cpu.req_kill && !early_nack
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

  val req_store   = (io.cpu.req_cmd === M_XWR)
  val req_load    = (io.cpu.req_cmd === M_XRD)
  val req_amo     = io.cpu.req_cmd(3).toBool
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
  val nack_hit = Wire() { Bool() }

  val mshr = new MSHRFile()
  val replay_amo_val = mshr.io.data_req.valid && mshr.io.data_req.bits.cmd(3).toBool

  // reset and flush unit
  val flusher = new FlushUnit(lines)
  val flushed = Reg(resetVal = Bool(true))
  flushed := flushed && (!r_cpu_req_val_ || r_req_flush) || r_cpu_req_val_ && r_req_flush && mshr.io.fence_rdy && flusher.io.req.ready
  flusher.io.req.valid := r_cpu_req_val_ && r_req_flush && mshr.io.fence_rdy && !flushed
  flusher.io.mshr_req.ready := mshr.io.req.ready
  
  when (io.cpu.req_val) {
    r_cpu_req_idx  := io.cpu.req_idx
    r_cpu_req_cmd  := io.cpu.req_cmd
    r_cpu_req_type := io.cpu.req_type
    r_cpu_req_tag  := io.cpu.req_tag
  }
  when (replay_amo_val) {
    r_cpu_req_idx  := Cat(mshr.io.data_req.bits.idx, mshr.io.data_req.bits.offset)
    r_cpu_req_cmd  := mshr.io.data_req.bits.cmd
    r_cpu_req_type := mshr.io.data_req.bits.typ
    r_amo_replay_data := mshr.io.data_req.bits.data
    r_way_oh       := mshr.io.data_req.bits.way_oh
  }
  when (flusher.io.meta_req.valid) {
    r_cpu_req_idx := Cat(flusher.io.meta_req.bits.inner_req.idx, mshr.io.data_req.bits.offset)
    r_cpu_req_cmd := M_FLA
    r_way_oh := flusher.io.meta_req.bits.way_en
  }
  val cpu_req_data = Mux(r_replay_amo, r_amo_replay_data, io.cpu.req_data)

  val misaligned =
    (((r_cpu_req_type === MT_H) || (r_cpu_req_type === MT_HU)) && (r_cpu_req_idx(0) != Bits(0))) ||
    (((r_cpu_req_type === MT_W) || (r_cpu_req_type === MT_WU)) && (r_cpu_req_idx(1,0) != Bits(0))) ||
    ((r_cpu_req_type === MT_D) && (r_cpu_req_idx(2,0) != Bits(0)));
    
  io.cpu.xcpt_ma_ld := r_cpu_req_val_ && r_req_read && misaligned
  io.cpu.xcpt_ma_st := r_cpu_req_val_ && r_req_write && misaligned

  // tags
  val meta = new MetaDataArrayArray(lines)
  val meta_arb = (new Arbiter(3)) { new MetaArrayArrayReq() }
  flusher.io.meta_req <> meta_arb.io.in(0)
  meta_arb.io.out <> meta.io.req

  // data
  val data = new DataArrayArray(lines)
  val data_arb = (new Arbiter(5)) { new DataArrayArrayReq() }
  data_arb.io.out <> data.io.req

  // cpu tag check
  meta_arb.io.in(2).valid := io.cpu.req_val
  meta_arb.io.in(2).bits.inner_req.idx := io.cpu.req_idx(indexmsb,indexlsb)
  meta_arb.io.in(2).bits.inner_req.rw := Bool(false)
  meta_arb.io.in(2).bits.inner_req.data.state := UFix(0) // don't care
  meta_arb.io.in(2).bits.inner_req.data.tag := UFix(0)   // don't care
  meta_arb.io.in(2).bits.way_en := ~UFix(0, NWAYS)
  val early_tag_nack = !meta_arb.io.in(2).ready
  val cpu_req_tag = Cat(io.cpu.req_ppn, r_cpu_req_idx)(tagmsb,taglsb)
  val tag_match_arr = (0 until NWAYS).map( w => isHit(r_cpu_req_cmd, meta.io.resp(w).state) && (meta.io.resp(w).tag === cpu_req_tag))
  val tag_match = Cat(Bits(0),tag_match_arr:_*).orR
  val tag_hit  = r_cpu_req_val &&  tag_match
  val tag_miss = r_cpu_req_val && !tag_match
  val hit_way_oh = Cat(Bits(0),tag_match_arr.reverse:_*)(NWAYS-1, 0) //TODO: use Vec
  val meta_resp_way_oh = Mux(meta.io.way_en === ~UFix(0, NWAYS), hit_way_oh, meta.io.way_en)
  val data_resp_way_oh = Mux(data.io.way_en === ~UFix(0, NWAYS), hit_way_oh, data.io.way_en)
  val meta_resp_mux = Mux1H(meta_resp_way_oh, meta.io.resp)
  val data_resp_mux = Mux1H(data_resp_way_oh, data.io.resp)

  // writeback unit
  val wb = new WritebackUnit
  wb.io.req <> mshr.io.wb_req
  wb.io.data_req <> data_arb.io.in(3)
  wb.io.data_resp <> data_resp_mux

  // replacement policy
  val replacer = new RandomReplacementWayGen()
  replacer.io.way_en := ~UFix(0, NWAYS)
  val replaced_way_oh = Mux(flusher.io.mshr_req.valid, r_way_oh, UFixToOH(replacer.io.way_id, NWAYS))
  val meta_wb_mux = Mux1H(replaced_way_oh, meta.io.resp)

  // refill response
  data_arb.io.in(0).bits.inner_req.offset := mshr.io.mem_resp_offset
  data_arb.io.in(0).bits.inner_req.idx := mshr.io.mem_resp_idx
  data_arb.io.in(0).bits.inner_req.rw := Bool(true)
  data_arb.io.in(0).bits.inner_req.wmask := ~UFix(0, MEM_DATA_BITS/8)
  data_arb.io.in(0).bits.inner_req.data := io.mem.xact_rep.bits.data
  data_arb.io.in(0).bits.way_en := mshr.io.mem_resp_way_oh
  data_arb.io.in(0).valid := io.mem.xact_rep.valid && (io.mem.xact_rep.bits.t_type === X_REP_READ_SHARED || io.mem.xact_rep.bits.t_type === X_REP_READ_EXCLUSIVE)

  // load hits
  data_arb.io.in(4).bits.inner_req.offset := io.cpu.req_idx(offsetmsb,ramindexlsb)
  data_arb.io.in(4).bits.inner_req.idx := io.cpu.req_idx(indexmsb,indexlsb)
  data_arb.io.in(4).bits.inner_req.rw := Bool(false)
  data_arb.io.in(4).bits.inner_req.wmask := UFix(0) // don't care
  data_arb.io.in(4).bits.inner_req.data := io.mem.xact_rep.bits.data // don't care
  data_arb.io.in(4).valid := io.cpu.req_val && req_read
  data_arb.io.in(4).bits.way_en := ~UFix(0, NWAYS) // intiate load on all ways, mux after tag check
  val early_load_nack = req_read && !data_arb.io.in(4).ready

  // store hits and AMO hits and misses use a pending store register.
  // we nack new stores if a pending store can't retire for some reason.
  // we drain a pending store if the CPU performs a store or a
  // conflictig load, or if the cache is idle, or after a miss.
  val p_store_idx_match = p_store_valid && (r_cpu_req_idx(indexmsb,indexlsb) === p_store_idx(indexmsb,indexlsb))
  val p_store_offset_match = (r_cpu_req_idx(indexlsb-1,offsetlsb) === p_store_idx(indexlsb-1,offsetlsb))
  val p_store_match = r_cpu_req_val_ && r_req_read && p_store_idx_match && p_store_offset_match
  val drain_store_val = (p_store_valid && (!io.cpu.req_val || !req_read || Reg(wb.io.req.valid || mshr.io.data_req.valid))) || p_store_match
  data_arb.io.in(2).bits.inner_req.offset := p_store_idx(offsetmsb,ramindexlsb)
  data_arb.io.in(2).bits.inner_req.idx := p_store_idx(indexmsb,indexlsb)
  data_arb.io.in(2).bits.inner_req.rw := Bool(true)
  data_arb.io.in(2).valid := drain_store_val
  data_arb.io.in(2).bits.way_en :=  p_store_way_oh
  val drain_store = drain_store_val && data_arb.io.in(2).ready
  val p_amo = Reg(resetVal = Bool(false))
  val p_store_rdy = !(p_store_valid && !drain_store) && !(mshr.io.data_req.valid || r_replay_amo || p_amo)
  p_amo := tag_hit && r_req_amo && mshr.io.req.ready && !nack_hit || r_replay_amo
  p_store_valid := p_store_valid && !drain_store || (tag_hit && r_req_store && mshr.io.req.ready && !nack_hit) || p_amo

  // tag update after a store to an exclusive clean line.
  val new_hit_state = newStateOnHit(r_cpu_req_cmd, meta_resp_mux.state)
  val set_hit_state  = tag_hit && meta_resp_mux.state != new_hit_state
  meta.io.state_req.bits.inner_req.rw := Bool(true)
  meta.io.state_req.bits.inner_req.idx := Reg(r_cpu_req_idx(indexmsb,indexlsb))
  meta.io.state_req.bits.inner_req.data.state := Reg(new_hit_state)
  meta.io.state_req.bits.way_en := Reg(hit_way_oh)
  meta.io.state_req.valid := Reg(set_hit_state, resetVal = Bool(false))
  
  // pending store data, also used for AMO RHS
  val amoalu = new AMOALU
  when (r_cpu_req_val_ && r_req_write && p_store_rdy || r_replay_amo) {
    p_store_idx    := r_cpu_req_idx
    p_store_type   := r_cpu_req_type
    p_store_cmd    := r_cpu_req_cmd
    p_store_way_oh := Mux(r_replay_amo, r_way_oh, hit_way_oh)
    p_store_data   := cpu_req_data
  }
  when (p_amo) {
    p_store_data := amoalu.io.out
  }

  // miss handling
  mshr.io.req.valid := r_cpu_req_val && r_req_readwrite && !nack_hit || flusher.io.mshr_req.valid
  mshr.io.req.bits.tag_miss := tag_miss || flusher.io.mshr_req.valid
  mshr.io.req.bits.old_state := meta_wb_mux.state
  mshr.io.req.bits.old_tag := meta_wb_mux.tag
  mshr.io.req.bits.tag := cpu_req_tag
  mshr.io.req.bits.idx := r_cpu_req_idx(indexmsb,indexlsb)
  mshr.io.req.bits.cpu_tag := r_cpu_req_tag
  mshr.io.req.bits.offset := r_cpu_req_idx(offsetmsb,0)
  mshr.io.req.bits.cmd := r_cpu_req_cmd
  mshr.io.req.bits.typ := r_cpu_req_type
  mshr.io.req.bits.way_oh := replaced_way_oh
  mshr.io.req.bits.data := cpu_req_data

  mshr.io.mem_rep <> io.mem.xact_rep
  mshr.io.mem_abort.valid := io.mem.xact_abort.valid
  mshr.io.mem_abort.bits := io.mem.xact_abort.bits
  mshr.io.meta_req <> meta_arb.io.in(1)
  replacer.io.pick_new_way := mshr.io.req.valid && mshr.io.req.ready

  // replays
  val replay = mshr.io.data_req.bits
  val stall_replay = r_replay_amo || p_amo || p_store_valid || flusher.io.meta_req.valid
  val replay_val = mshr.io.data_req.valid
  val replay_fire = replay_val && !stall_replay
  val replay_rdy = data_arb.io.in(1).ready && !stall_replay
  data_arb.io.in(1).bits.inner_req.offset := replay.offset(offsetmsb,ramindexlsb)
  data_arb.io.in(1).bits.inner_req.idx := replay.idx
  data_arb.io.in(1).bits.inner_req.rw := replay.cmd === M_XWR
  data_arb.io.in(1).valid := replay_fire
  data_arb.io.in(1).bits.way_en := mshr.io.data_req.bits.way_oh
  mshr.io.data_req.ready := replay_rdy
  r_replay_amo := replay_amo_val && replay_rdy

  // store write mask generation.
  // assumes store replays are higher-priority than pending stores.
  val maskgen = new StoreMaskGen
  val store_offset = Mux(!replay_fire, p_store_idx(offsetmsb,0), replay.offset)
  maskgen.io.typ := Mux(!replay_fire, p_store_type, replay.typ)
  maskgen.io.addr := store_offset(offsetlsb-1,0)
  val store_wmask_wide = maskgen.io.wmask << Cat(store_offset(ramindexlsb-1,offsetlsb), Bits(0, log2up(CPU_DATA_BITS/8))).toUFix
  val store_data = Mux(!replay_fire, p_store_data, replay.data)
  val store_data_wide = Fill(MEM_DATA_BITS/CPU_DATA_BITS, store_data)
  data_arb.io.in(1).bits.inner_req.data := store_data_wide
  data_arb.io.in(1).bits.inner_req.wmask := store_wmask_wide
  data_arb.io.in(2).bits.inner_req.data := store_data_wide
  data_arb.io.in(2).bits.inner_req.wmask := store_wmask_wide

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

  io.cpu.req_rdy   := flusher.io.req.ready && !(r_cpu_req_val_ && r_req_flush) && !pending_fence
  io.cpu.resp_nack := r_cpu_req_val_ && !io.cpu.req_kill && nack
  io.cpu.resp_val  := (tag_hit && !nack && r_req_read) || mshr.io.cpu_resp_val
  io.cpu.resp_replay := mshr.io.cpu_resp_val
  io.cpu.resp_miss := r_cpu_req_val_ && !tag_match && r_req_read
  io.cpu.resp_tag  := Mux(mshr.io.cpu_resp_val, mshr.io.cpu_resp_tag, r_cpu_req_tag)
  io.cpu.resp_type := loadgen.io.typ
  io.cpu.resp_data := loadgen.io.dout
  io.cpu.resp_data_subword := loadgen.io.r_dout_subword
  
  val xact_init_arb = (new Arbiter(2)) { new TransactionInit }
  xact_init_arb.io.in(0) <> wb.io.mem_req
  xact_init_arb.io.in(1) <> mshr.io.mem_req
  io.mem.xact_init <> xact_init_arb.io.out

  io.mem.xact_init_data <> wb.io.mem_req_data
  io.mem.xact_finish <> mshr.io.mem_finish
}
