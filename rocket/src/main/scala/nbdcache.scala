package rocket

import Chisel._
import Node._
import Constants._
import uncore._
import Util._

case class DCacheConfig(sets: Int, ways: Int, co: CoherencePolicy,
                        nmshr: Int, nrpq: Int, nsdq: Int,
                        reqtagbits: Int = -1)
{
  require(isPow2(sets))
  require(isPow2(ways)) // TODO: relax this
  def lines = sets*ways
  def dm = ways == 1
  def ppnbits = PADDR_BITS - PGIDX_BITS
  def vpnbits = VADDR_BITS - PGIDX_BITS
  def pgidxbits = PGIDX_BITS
  def offbits = OFFSET_BITS
  def paddrbits = ppnbits + pgidxbits
  def lineaddrbits = paddrbits - offbits
  def idxbits = log2Up(sets)
  def waybits = log2Up(ways)
  def untagbits = offbits + idxbits
  def tagbits = lineaddrbits - idxbits
  def ramoffbits = log2Up(MEM_DATA_BITS/8)
  def databytes = 8 // assumed by StoreGen/LoadGen/AMOALU
  def databits = databytes*8
  def wordoffbits = log2Up(databytes)
}

abstract class ReplacementPolicy
{
  def way: UFix
  def miss: Unit
  def hit: Unit
}

class RandomReplacement(implicit conf: DCacheConfig) extends ReplacementPolicy
{
  private val replace = Bool()
  replace := Bool(false)
  val lfsr = LFSR16(replace)

  def way = if (conf.dm) UFix(0) else lfsr(conf.waybits-1,0)
  def miss = replace := Bool(true)
  def hit = {}
}

case class StoreGen(typ: Bits, addr: Bits, dat: Bits)
{
  val byte = typ === MT_B || typ === MT_BU
  val half = typ === MT_H || typ === MT_HU
  val word = typ === MT_W || typ === MT_WU
  def mask =
    Mux(byte, Bits(  1) <<     addr(2,0),
    Mux(half, Bits(  3) << Cat(addr(2,1), Bits(0,1)),
    Mux(word, Bits( 15) << Cat(addr(2),   Bits(0,2)),
              Bits(255))))
  def data =
    Mux(byte, Fill(8, dat( 7,0)),
    Mux(half, Fill(4, dat(15,0)),
    Mux(word, Fill(2, dat(31,0)),
                      dat)))
}

case class LoadGen(typ: Bits, addr: Bits, dat: Bits)
{
  val t = StoreGen(typ, addr, dat)
  val sign = typ === MT_B || typ === MT_H || typ === MT_W || typ === MT_D

  val wordShift = Mux(addr(2), dat(63,32), dat(31,0))
  val word = Cat(Mux(t.word, Fill(32, sign && wordShift(31)), dat(63,32)), wordShift)
  val halfShift = Mux(addr(1), word(31,16), word(15,0))
  val half = Cat(Mux(t.half, Fill(48, sign && halfShift(15)), word(63,16)), halfShift)
  val byteShift = Mux(addr(0), half(15,8), half(7,0))
  val byte = Cat(Mux(t.byte, Fill(56, sign && byteShift(7)), half(63,8)), byteShift)
}

class MSHRReq(implicit conf: DCacheConfig) extends Bundle {
  val old_dirty = Bool()
  val old_tag = Bits(width = conf.tagbits)

  val way_en = Bits(width = conf.ways)

  val addr = UFix(width = conf.paddrbits)
  val cmd = Bits(width = 4)
  val typ = Bits(width = 3)
  val tag = Bits(width = conf.reqtagbits)
  val data = Bits(width = conf.databits)

  override def clone = new MSHRReq().asInstanceOf[this.type]
}

class Replay(implicit conf: DCacheConfig) extends HellaCacheReq {
  val sdq_id = UFix(width = log2Up(conf.nsdq))

  override def clone = new Replay().asInstanceOf[this.type]
}

class DataReadReq(implicit conf: DCacheConfig) extends Bundle {
  val way_en = Bits(width = conf.ways)
  val addr   = Bits(width = conf.untagbits)

  override def clone = new DataReadReq().asInstanceOf[this.type]
}

class DataWriteReq(implicit conf: DCacheConfig) extends Bundle {
  val way_en = Bits(width = conf.ways)
  val addr   = Bits(width = conf.untagbits)
  val wmask  = Bits(width = MEM_DATA_BITS/conf.databits)
  val data   = Bits(width = MEM_DATA_BITS)

  override def clone = new DataWriteReq().asInstanceOf[this.type]
}

class WritebackReq(implicit conf: DCacheConfig) extends Bundle {
  val tag = Bits(width = conf.tagbits)
  val idx = Bits(width = conf.idxbits)
  val way_en = Bits(width = conf.ways)
  val tile_xact_id = Bits(width = TILE_XACT_ID_BITS)

  override def clone = new WritebackReq().asInstanceOf[this.type]
}

class MetaData(implicit conf: DCacheConfig) extends Bundle {
  val state = UFix(width = 2)
  val tag = Bits(width = conf.tagbits)

  override def clone = new MetaData().asInstanceOf[this.type]
}

class MetaArrayReq(implicit conf: DCacheConfig) extends Bundle {
  val way_en = Bits(width = conf.ways)
  val idx  = Bits(width = conf.idxbits)
  val rw  = Bool()
  val data = new MetaData()

  override def clone = new MetaArrayReq().asInstanceOf[this.type]
}

class MSHR(id: Int)(implicit conf: DCacheConfig) extends Component {
  val io = new Bundle {
    val req_pri_val    = Bool(INPUT)
    val req_pri_rdy    = Bool(OUTPUT)
    val req_sec_val    = Bool(INPUT)
    val req_sec_rdy    = Bool(OUTPUT)
    val req_bits       = new MSHRReq().asInput
    val req_sdq_id     = UFix(INPUT, log2Up(conf.nsdq))

    val idx_match      = Bool(OUTPUT)
    val tag            = Bits(OUTPUT, conf.tagbits)

    val mem_req  = (new FIFOIO) { new TransactionInit }
    val mem_resp = new DataWriteReq().asOutput
    val meta_req = (new FIFOIO) { new MetaArrayReq() }
    val replay = (new FIFOIO) { new Replay() }
    val mem_abort = (new PipeIO) { new TransactionAbort }.flip
    val mem_rep = (new PipeIO) { new TransactionReply }.flip
    val mem_finish = (new FIFOIO) { new TransactionFinish }
    val wb_req = (new FIFOIO) { new WritebackReq }
    val probe_writeback = (new FIFOIO) { Bool() }.flip
    val probe_refill = (new FIFOIO) { Bool() }.flip
  }

  val s_invalid :: s_wb_req :: s_wb_resp :: s_meta_clear :: s_refill_req :: s_refill_resp :: s_meta_write :: s_drain_rpq :: Nil = Enum(8) { UFix() }
  val state = Reg(resetVal = s_invalid)

  val xacx_type = Reg { UFix() }
  val line_state = Reg { UFix() }
  val refill_count = Reg { UFix(width = log2Up(REFILL_CYCLES)) }
  val req = Reg { new MSHRReq() }

  val req_cmd = io.req_bits.cmd
  val req_use_rpq = req_cmd != M_PFR && req_cmd != M_PFW
  val req_idx = req.addr(conf.untagbits-1,conf.offbits)
  val idx_match = req_idx === io.req_bits.addr(conf.untagbits-1,conf.offbits)
  val sec_rdy = idx_match && (state === s_wb_req || state === s_wb_resp || state === s_meta_clear || (state === s_refill_req || state === s_refill_resp) && !conf.co.needsTransactionOnSecondaryMiss(req_cmd, io.mem_req.bits))

  val rpq = (new Queue(conf.nrpq)) { new Replay }
  rpq.io.enq.valid := (io.req_pri_val && io.req_pri_rdy || io.req_sec_val && sec_rdy) && req_use_rpq
  rpq.io.enq.bits := io.req_bits
  rpq.io.enq.bits.sdq_id := io.req_sdq_id
  rpq.io.deq.ready := io.replay.ready && state === s_drain_rpq || state === s_invalid

  val abort = io.mem_abort.valid && io.mem_abort.bits.tile_xact_id === UFix(id)
  val reply = io.mem_rep.valid && io.mem_rep.bits.tile_xact_id === UFix(id)
  val refill_done = reply && refill_count.andR
  val wb_done = reply && (state === s_wb_resp)

  val finish_q = (new Queue(2 /* wb + refill */)) { new TransactionFinish }
  finish_q.io.enq.valid := wb_done || refill_done
  finish_q.io.enq.bits.global_xact_id := io.mem_rep.bits.global_xact_id

  when (state === s_drain_rpq && !rpq.io.deq.valid && !finish_q.io.deq.valid) {
    state := s_invalid
  }
  when (state === s_meta_write && io.meta_req.ready) {
    state := s_drain_rpq
  }
  when (state === s_refill_resp) {
    when (refill_done) { state := s_meta_write }
    when (reply) {
      refill_count := refill_count + UFix(1)
      line_state := conf.co.newStateOnTransactionReply(io.mem_rep.bits, io.mem_req.bits)
    }
    when (abort) { state := s_refill_req }
  }
  when (state === s_refill_req) {
    when (abort) { state := s_refill_req }
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
    when (io.probe_writeback.valid && io.probe_writeback.bits && idx_match) { state := s_refill_req }
    .elsewhen (io.wb_req.ready) { state := s_wb_resp }
  }

  when (io.req_sec_val && io.req_sec_rdy) { // s_wb_req, s_wb_resp, s_refill_req
    xacx_type := conf.co.getTransactionInitTypeOnSecondaryMiss(req_cmd, conf.co.newStateOnFlush(), io.mem_req.bits)
  }
  when ((state === s_invalid) && io.req_pri_val) {
    line_state := conf.co.newStateOnFlush()
    refill_count := UFix(0)
    xacx_type := conf.co.getTransactionInitTypeOnPrimaryMiss(req_cmd, conf.co.newStateOnFlush())
    req := io.req_bits
    state := Mux(io.req_bits.old_dirty, s_wb_req, s_refill_req)
  }

  io.idx_match := (state != s_invalid) && idx_match
  io.mem_resp := req
  io.mem_resp.addr := Cat(req_idx, refill_count) << conf.ramoffbits
  io.tag := req.addr >> conf.untagbits
  io.req_pri_rdy := (state === s_invalid)
  io.req_sec_rdy := sec_rdy && rpq.io.enq.ready

  io.meta_req.valid := state === s_meta_write || state === s_meta_clear || state === s_drain_rpq
  io.meta_req.bits.rw := state != s_drain_rpq
  io.meta_req.bits.idx := req_idx
  io.meta_req.bits.data.state := Mux(state === s_meta_clear, conf.co.newStateOnFlush(), line_state)
  io.meta_req.bits.data.tag := io.tag
  io.meta_req.bits.way_en := req.way_en

  io.wb_req.valid := (state === s_wb_req) && !(io.probe_writeback.valid && idx_match)
  io.wb_req.bits.tag := req.old_tag
  io.wb_req.bits.idx := req_idx
  io.wb_req.bits.way_en := req.way_en
  io.wb_req.bits.tile_xact_id := Bits(id)

  io.probe_writeback.ready := (state != s_wb_resp && state != s_meta_clear && state != s_drain_rpq) || !idx_match
  io.probe_refill.ready := (state != s_refill_resp && state != s_drain_rpq) || !idx_match

  io.mem_req.valid := state === s_refill_req
  io.mem_req.bits.x_type := xacx_type
  io.mem_req.bits.addr := Cat(io.tag, req_idx).toUFix
  io.mem_req.bits.tile_xact_id := Bits(id)
  io.mem_finish <> finish_q.io.deq

  io.replay.valid := state === s_drain_rpq && rpq.io.deq.valid
  io.replay.bits := rpq.io.deq.bits
  io.replay.bits.phys := Bool(true)
  io.replay.bits.addr := Cat(io.tag, req_idx, rpq.io.deq.bits.addr(conf.offbits-1,0)).toUFix

  // don't issue back-to-back replays with store->load dependence
  val r1_replay_valid = Reg(rpq.io.deq.fire())
  val r2_replay_valid = Reg(r1_replay_valid)
  val (r1_replay, r2_replay) = (Reg{new Replay}, Reg{new Replay})
  when (rpq.io.deq.fire()) { r1_replay := rpq.io.deq.bits }
  when (r1_replay_valid) { r2_replay := r1_replay }
  def offsetMatch(dst: HellaCacheReq, src: HellaCacheReq) = {
    def mask(x: HellaCacheReq) = StoreGen(x.typ, x.addr, Bits(0)).mask
    // TODO: this is overly restrictive
    dst.addr(conf.offbits-1,conf.wordoffbits) === src.addr(conf.offbits-1,conf.wordoffbits)
    // && (mask(dst) & mask(src)).orR
  }
  when (r1_replay_valid && offsetMatch(io.replay.bits, r1_replay) ||
        r2_replay_valid && offsetMatch(io.replay.bits, r2_replay)) {
    rpq.io.deq.ready := Bool(false)
    io.replay.bits.cmd := M_FENCE // NOP
  }
}

class MSHRFile(implicit conf: DCacheConfig) extends Component {
  val io = new Bundle {
    val req = (new FIFOIO) { new MSHRReq }.flip
    val secondary_miss = Bool(OUTPUT)

    val mem_req  = (new FIFOIO) { new TransactionInit }
    val mem_resp = new DataWriteReq().asOutput
    val meta_req = (new FIFOIO) { new MetaArrayReq() }
    val replay = (new FIFOIO) { new Replay }
    val mem_abort = (new PipeIO) { new TransactionAbort }.flip
    val mem_rep = (new PipeIO) { new TransactionReply }.flip
    val mem_finish = (new FIFOIO) { new TransactionFinish }
    val wb_req = (new FIFOIO) { new WritebackReq }
    val probe = (new FIFOIO) { Bool() }.flip

    val fence_rdy = Bool(OUTPUT)
  }

  val sdq_val = Reg(resetVal = Bits(0, conf.nsdq))
  val sdq_alloc_id = PriorityEncoder(~sdq_val(conf.nsdq-1,0))
  val sdq_rdy = !sdq_val.andR
  val sdq_enq = io.req.valid && io.req.ready && isWrite(io.req.bits.cmd)
  val sdq = Mem(conf.nsdq) { io.req.bits.data.clone }
  when (sdq_enq) { sdq(sdq_alloc_id) := io.req.bits.data }

  val idxMatch = Vec(conf.nmshr) { Bool() }
  val tagList = Vec(conf.nmshr) { Bits() }
  val wbTagList = Vec(conf.nmshr) { Bits() }
  val memRespMux = Vec(conf.nmshr) { new DataWriteReq }
  val meta_req_arb = (new Arbiter(conf.nmshr)) { new MetaArrayReq() }
  val mem_req_arb = (new Arbiter(conf.nmshr)) { new TransactionInit }
  val mem_finish_arb = (new Arbiter(conf.nmshr)) { new TransactionFinish }
  val wb_req_arb = (new Arbiter(conf.nmshr)) { new WritebackReq }
  val replay_arb = (new Arbiter(conf.nmshr)) { new Replay() }
  val alloc_arb = (new Arbiter(conf.nmshr)) { Bool() }

  val tag_match = Mux1H(idxMatch, tagList) === io.req.bits.addr >> conf.untagbits
  val wb_probe_match = Mux1H(idxMatch, wbTagList) === io.req.bits.addr >> conf.untagbits

  var idx_match = Bool(false)
  var pri_rdy = Bool(false)
  var fence = Bool(false)
  var sec_rdy = Bool(false)
  var writeback_probe_rdy = Bool(true)
  var refill_probe_rdy = Bool(true)

  for (i <- 0 to conf.nmshr-1) {
    val mshr = new MSHR(i)

    idxMatch(i) := mshr.io.idx_match
    tagList(i) := mshr.io.tag
    wbTagList(i) := mshr.io.wb_req.bits.tag

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
    mshr.io.probe_writeback.valid := io.probe.valid
    mshr.io.probe_writeback.bits := wb_probe_match

    mshr.io.mem_abort <> io.mem_abort
    mshr.io.mem_rep <> io.mem_rep
    memRespMux(i) := mshr.io.mem_resp

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
  io.mem_resp := memRespMux(io.mem_rep.bits.tile_xact_id)
  io.fence_rdy := !fence
  io.probe.ready := (refill_probe_rdy || !tag_match) && (writeback_probe_rdy || !wb_probe_match)

  val free_sdq = io.replay.fire() && isWrite(io.replay.bits.cmd)
  io.replay.bits.data := sdq(RegEn(replay_arb.io.out.bits.sdq_id, free_sdq))
  io.replay <> replay_arb.io.out

  when (io.replay.valid || sdq_enq) {
    sdq_val := sdq_val & ~(UFixToOH(io.replay.bits.sdq_id) & Fill(conf.nsdq, free_sdq)) | 
               PriorityEncoderOH(~sdq_val(conf.nsdq-1,0)) & Fill(conf.nsdq, sdq_enq)
  }
}


class WritebackUnit(implicit conf: DCacheConfig) extends Component {
  val io = new Bundle {
    val req = (new FIFOIO) { new WritebackReq() }.flip
    val probe = (new FIFOIO) { new WritebackReq() }.flip
    val meta_req = (new FIFOIO) { new MetaArrayReq }
    val data_req = (new FIFOIO) { new DataReadReq() }
    val data_resp = Bits(INPUT, MEM_DATA_BITS)
    val mem_req = (new FIFOIO) { new TransactionInit }
    val mem_req_data = (new FIFOIO) { new TransactionInitData }
    val probe_rep_data = (new FIFOIO) { new ProbeReplyData }
  }

  val valid = Reg(resetVal = Bool(false))
  val is_probe = Reg() { Bool() }
  val data_req_fired = Reg(resetVal = Bool(false))
  val r_data_req_fired = Reg(data_req_fired, resetVal = Bool(false))
  val cmd_sent = Reg() { Bool() }
  val cnt = Reg() { UFix(width = log2Up(REFILL_CYCLES+1)) }
  val req = Reg() { new WritebackReq() }

  val dout_rdy = Mux(is_probe, io.probe_rep_data.ready, io.mem_req_data.ready)
  data_req_fired := Bool(false)
  when (valid && io.mem_req.ready) {
    cmd_sent := Bool(true)
  }
  when (io.data_req.fire()) {
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

  val fire = valid && cnt < UFix(REFILL_CYCLES)
  io.req.ready := !valid && !io.probe.valid
  io.probe.ready := !valid
  io.data_req.valid := fire && io.meta_req.ready
  io.data_req.bits.way_en := req.way_en
  io.data_req.bits.addr := Cat(req.idx, cnt(log2Up(REFILL_CYCLES)-1,0)) << conf.ramoffbits

  io.meta_req.valid := fire && io.data_req.ready
  io.meta_req.bits.way_en := Fix(-1)
  io.meta_req.bits.rw := Bool(false)
  io.meta_req.bits.idx := req.idx
  io.meta_req.bits.data.tag := req.tag

  io.mem_req.valid := valid && !cmd_sent
  io.mem_req.bits.x_type := conf.co.getTransactionInitTypeOnWriteback()
  io.mem_req.bits.addr := Cat(req.tag, req.idx).toUFix
  io.mem_req.bits.tile_xact_id := req.tile_xact_id
  io.mem_req_data.valid := r_data_req_fired && !is_probe
  io.mem_req_data.bits.data := io.data_resp
  io.probe_rep_data.valid := r_data_req_fired && is_probe
  io.probe_rep_data.bits.data := io.data_resp
}

class ProbeUnit(implicit conf: DCacheConfig)  extends Component {
  val io = new Bundle {
    val req = (new FIFOIO) { new ProbeRequest }.flip
    val rep = (new FIFOIO) { new ProbeReply }
    val meta_req = (new FIFOIO) { new MetaArrayReq }
    val mshr_req = (new FIFOIO) { Bool() }
    val wb_req = (new FIFOIO) { new WritebackReq }
    val way_en = Bits(INPUT, conf.ways)
    val line_state = UFix(INPUT, 2)
    val addr = Bits(OUTPUT, conf.lineaddrbits)
  }

  val s_invalid :: s_meta_req :: s_meta_resp :: s_mshr_req :: s_probe_rep :: s_writeback_req :: s_writeback_resp :: s_meta_write :: Nil = Enum(8) { UFix() }
  val state = Reg(resetVal = s_invalid)
  val line_state = Reg() { UFix() }
  val way_en = Reg() { Bits() }
  val req = Reg() { new ProbeRequest() }
  val hit = way_en.orR

  when (state === s_meta_write && io.meta_req.ready) {
    state := s_invalid
  }
  when (state === s_writeback_resp && io.wb_req.ready) {
    state := s_meta_write
  }
  when (state === s_writeback_req && io.wb_req.ready) {
    state := s_writeback_resp
  }
  when (state === s_probe_rep && io.rep.ready) {
    state := s_invalid
    when (hit) {
      state := Mux(conf.co.needsWriteback(line_state), s_writeback_req, s_meta_write)
    }
  }
  when (state === s_mshr_req) {
    state := s_probe_rep
    line_state := io.line_state
    way_en := io.way_en
    when (!io.mshr_req.ready) { state := s_meta_req }
  }
  when (state === s_meta_resp) {
    state := s_mshr_req
  }
  when (state === s_meta_req && io.meta_req.ready) {
    state := s_meta_resp
  }
  when (state === s_invalid && io.req.valid) {
    state := s_meta_req
    req := io.req.bits
  }

  io.req.ready := state === s_invalid && !reset
  io.rep.valid := state === s_probe_rep
  io.rep.bits := conf.co.newProbeReply(req, Mux(hit, line_state, conf.co.newStateOnFlush))

  io.meta_req.valid := state === s_meta_req || state === s_meta_write
  io.meta_req.bits.way_en := Mux(state === s_meta_write, way_en, Fix(-1))
  io.meta_req.bits.rw := state === s_meta_write
  io.meta_req.bits.idx := req.addr
  io.meta_req.bits.data.state := conf.co.newStateOnProbeRequest(req, line_state)
  io.meta_req.bits.data.tag := req.addr >> UFix(conf.idxbits)
  io.mshr_req.valid := state === s_mshr_req
  io.addr := req.addr

  io.wb_req.valid := state === s_writeback_req
  io.wb_req.bits.way_en := way_en
  io.wb_req.bits.idx := req.addr
  io.wb_req.bits.tag := req.addr >> UFix(conf.idxbits)
}

class MetaDataArray(implicit conf: DCacheConfig) extends Component {
  val io = new Bundle {
    val req  = (new FIFOIO) { new MetaArrayReq() }.flip
    val resp = Vec(conf.ways){ (new MetaData).asOutput }
    val state_req = (new PipeIO) { new MetaArrayReq() }.flip
    val way_en = Bits(OUTPUT, conf.ways)
  }

  val rst_cnt = Reg(resetVal = UFix(0, log2Up(conf.sets+1)))
  val rst = rst_cnt < conf.sets
  when (rst) { rst_cnt := rst_cnt+1 }

  val permBits = io.req.bits.data.state.width
  val perms = Mem(conf.sets) { UFix(width = permBits*conf.ways) }
  val tags = Mem(conf.sets, seqRead = true) { Bits(width = conf.tagbits*conf.ways) }
  val tag = Reg() { Bits() }
  val raddr = Reg() { Bits() }
  val way_en_ = Reg { Bits(width = conf.ways) }

  when (rst || io.state_req.valid && io.state_req.bits.rw) {
    val addr = Mux(rst, rst_cnt, io.state_req.bits.idx)
    val data = Mux(rst, conf.co.newStateOnFlush, io.state_req.bits.data.state)
    val mask = Mux(rst, Fix(-1), io.state_req.bits.way_en)
    perms.write(addr, Fill(conf.ways, data), FillInterleaved(permBits, mask))
  }
  when (io.req.valid) {
    when (io.req.bits.rw) {
      perms.write(io.req.bits.idx, Fill(conf.ways, io.req.bits.data.state), FillInterleaved(permBits, io.req.bits.way_en))
      tags.write(io.req.bits.idx, Fill(conf.ways, io.req.bits.data.tag), FillInterleaved(conf.tagbits, io.req.bits.way_en))
    }
    .otherwise {
      raddr := io.req.bits.idx
      tag := tags(io.req.bits.idx)
    }
    way_en_ := io.req.bits.way_en
  }

  val perm = perms(raddr)
  for (w <- 0 until conf.ways) {
    io.resp(w).state := perm(permBits*(w+1)-1, permBits*w)
    io.resp(w).tag := tag(conf.tagbits*(w+1)-1, conf.tagbits*w)
  }

  io.way_en := way_en_
  io.req.ready := !rst
}

class DataArray(implicit conf: DCacheConfig) extends Component {
  val io = new Bundle {
    val read = new FIFOIO()(new DataReadReq).flip
    val write = new FIFOIO()(new DataWriteReq).flip
    val resp = Vec(conf.ways){ Bits(OUTPUT, MEM_DATA_BITS) }
  }

  val wmask = FillInterleaved(conf.databits, io.write.bits.wmask)
  val waddr = io.write.bits.addr >> conf.ramoffbits
  val raddr = io.read.bits.addr >> conf.ramoffbits

  for (w <- 0 until conf.ways) {
    val rdata = Reg() { Bits() }
    val array = Mem(conf.sets*REFILL_CYCLES, seqRead = true){ Bits(width=MEM_DATA_BITS) }
    when (io.write.bits.way_en(w) && io.write.valid) {
      array.write(waddr, io.write.bits.data, wmask)
    }
    when (io.read.bits.way_en(w) && io.read.valid) {
      rdata := array(raddr)
    }
    io.resp(w) := rdata
  }

  io.read.ready := Bool(true)
  io.write.ready := Bool(true)
}

class AMOALU(implicit conf: DCacheConfig) extends Component {
  val io = new Bundle {
    val lhs_raw = Bits(INPUT, conf.databits)
    val addr = Bits(INPUT, conf.offbits)
    val cmd = Bits(INPUT, 4)
    val typ = Bits(INPUT, 3)
    val lhs = Bits(INPUT, conf.databits)
    val rhs = Bits(INPUT, conf.databits)
    val out = Bits(OUTPUT, conf.databits)
  }

  require(conf.databytes == 8)
  
  val sgned = (io.cmd === M_XA_MIN) || (io.cmd === M_XA_MAX)
  val minmax = (io.cmd === M_XA_MIN) || (io.cmd === M_XA_MINU) || (io.cmd === M_XA_MAX) || (io.cmd === M_XA_MAXU)
  val min = (io.cmd === M_XA_MIN) || (io.cmd === M_XA_MINU)
  val word = (io.typ === MT_W) || (io.typ === MT_WU)

  val adder_out = io.lhs + io.rhs

  val cmp_lhs  = Mux(word, io.lhs(31), io.lhs(63))
  val cmp_rhs  = Mux(word, io.rhs(31), io.rhs(63))
  val cmp_diff = Mux(word, io.lhs(31,0) < io.rhs(31,0), io.lhs < io.rhs)
  val less = Mux(cmp_lhs === cmp_rhs, cmp_diff, Mux(sgned, cmp_lhs, cmp_rhs))
  val cmp_out = Mux(min === less, io.lhs, io.rhs)

  val out = Mux(io.cmd === M_XA_ADD,  adder_out,
            Mux(io.cmd === M_XA_AND,  io.lhs & io.rhs,
            Mux(io.cmd === M_XA_OR,   io.lhs | io.rhs,
            Mux(minmax,               cmp_out,
            io.rhs))))

  val wdata = Mux(word, Cat(out(31,0), out(31,0)), out)
  val wmask = FillInterleaved(8, StoreGen(io.typ, io.addr, Bits(0)).mask)
  io.out := wmask & wdata | ~wmask & io.lhs_raw
}

class HellaCacheReq(implicit conf: DCacheConfig) extends Bundle {
  val kill = Bool()
  val typ  = Bits(width = 3)
  val phys = Bool()
  val addr = UFix(width = conf.ppnbits.max(conf.vpnbits+1) + conf.pgidxbits)
  val data = Bits(width = conf.databits)
  val tag  = Bits(width = conf.reqtagbits)
  val cmd  = Bits(width = 4)

  override def clone = new HellaCacheReq().asInstanceOf[this.type]
}

class HellaCacheResp(implicit conf: DCacheConfig) extends Bundle {
  val nack = Bool() // comes 2 cycles after req.fire
  val replay = Bool()
  val typ = Bits(width = 3)
  val data = Bits(width = conf.databits)
  val data_subword = Bits(width = conf.databits)
  val tag = Bits(width = conf.reqtagbits)
  val cmd  = Bits(width = 4)
  val addr = UFix(width = conf.ppnbits.max(conf.vpnbits+1) + conf.pgidxbits)
  val store_data = Bits(width = conf.databits)

  override def clone = new HellaCacheResp().asInstanceOf[this.type]
}

class AlignmentExceptions extends Bundle {
  val ld = Bool()
  val st = Bool()
}

class HellaCacheExceptions extends Bundle {
  val ma = new AlignmentExceptions
  val pf = new AlignmentExceptions
}

// interface between D$ and processor/DTLB
class ioHellaCache(implicit conf: DCacheConfig) extends Bundle {
  val req = (new FIFOIO){ new HellaCacheReq }
  val resp = (new PipeIO){ new HellaCacheResp }.flip
  val xcpt = (new HellaCacheExceptions).asInput
  val ptw = new IOTLBPTW().flip
}

class HellaCache(implicit conf: DCacheConfig) extends Component {
  val io = new Bundle {
    val cpu = (new ioHellaCache).flip
    val mem = new ioTileLink
  }
 
  val indexmsb    = conf.untagbits-1
  val indexlsb    = conf.offbits
  val offsetmsb   = indexlsb-1
  val offsetlsb   = log2Up(conf.databytes)

  val wb = new WritebackUnit
  val prober = new ProbeUnit
  val mshr = new MSHRFile

  io.cpu.req.ready := Bool(true)
  val s1_valid = Reg(io.cpu.req.fire(), resetVal = Bool(false))
  val s1_valid_masked = s1_valid && !io.cpu.req.bits.kill
  val s1_replay = Reg(resetVal = Bool(false))
  val s1_req = Reg{io.cpu.req.bits.clone}
  val s2_req = Reg{io.cpu.req.bits.clone}

  val s2_valid = Reg(s1_valid_masked, resetVal = Bool(false))
  val s2_replay = Reg(s1_replay, resetVal = Bool(false))
  val s2_valid_masked = Bool()
  val s2_nack_hit = Bool()

  val s3_valid = Reg(resetVal = Bool(false))
  val s3_req = Reg{io.cpu.req.bits.clone}
  val s3_way = Reg{Bits()}

  val s1_read  = isRead(s1_req.cmd)
  val s1_write = isWrite(s1_req.cmd)
  val s1_readwrite = s1_read || s1_write

  val dtlb = new TLB(8)
  dtlb.io.ptw <> io.cpu.ptw
  dtlb.io.req.valid := s1_valid_masked && s1_readwrite && !s1_req.phys
  dtlb.io.req.bits.passthrough := s1_req.phys
  dtlb.io.req.bits.asid := UFix(0)
  dtlb.io.req.bits.vpn := s1_req.addr >> conf.pgidxbits
  dtlb.io.req.bits.instruction := Bool(false)
  when (!dtlb.io.req.ready && !io.cpu.req.bits.phys) { io.cpu.req.ready := Bool(false) }
  
  when (io.cpu.req.valid) {
    s1_req := io.cpu.req.bits
  }
  when (wb.io.meta_req.valid) {
    s1_req.phys := Bool(true)
    s1_req.addr := Cat(wb.io.meta_req.bits.data.tag, wb.io.meta_req.bits.idx, UFix(0, conf.offbits)).toUFix
  }
  when (prober.io.meta_req.valid) {
    s1_req.addr := Cat(prober.io.meta_req.bits.data.tag, prober.io.meta_req.bits.idx, UFix(0, conf.offbits)).toUFix
    s1_req.phys := Bool(true)
  }
  when (mshr.io.replay.valid) {
    s1_req := mshr.io.replay.bits
  }
  val s1_addr = Cat(dtlb.io.resp.ppn, s1_req.addr(conf.pgidxbits-1,0))

  when (s1_valid || s1_replay) {
    s2_req.addr := s1_addr
    s2_req.typ := s1_req.typ
    s2_req.cmd := s1_req.cmd
    s2_req.tag := s1_req.tag
    when (s1_write) {
      s2_req.data := Mux(s1_replay, mshr.io.replay.bits.data, io.cpu.req.bits.data)
    }
  }

  val misaligned =
    (((s1_req.typ === MT_H) || (s1_req.typ === MT_HU)) && (s1_req.addr(0) != Bits(0))) ||
    (((s1_req.typ === MT_W) || (s1_req.typ === MT_WU)) && (s1_req.addr(1,0) != Bits(0))) ||
    ((s1_req.typ === MT_D) && (s1_req.addr(2,0) != Bits(0)));
    
  io.cpu.xcpt.ma.ld := s1_read && misaligned
  io.cpu.xcpt.ma.st := s1_write && misaligned
  io.cpu.xcpt.pf.ld := s1_read && dtlb.io.resp.xcpt_ld
  io.cpu.xcpt.pf.st := s1_write && dtlb.io.resp.xcpt_st

  // tags
  val meta = new MetaDataArray
  val meta_arb = (new Arbiter(4)) { new MetaArrayReq() }
  meta_arb.io.out <> meta.io.req

  // data
  val data = new DataArray
  val readArb = new Arbiter(3)(new DataReadReq)
  val writeArb = new Arbiter(2)(new DataWriteReq)
  readArb.io.out.ready := !io.mem.xact_rep.valid || io.mem.xact_rep.ready // insert bubble if refill gets blocked
  readArb.io.out <> data.io.read
  writeArb.io.out <> data.io.write

  // cpu tag check
  meta_arb.io.in(3).valid := io.cpu.req.valid
  meta_arb.io.in(3).bits.idx := io.cpu.req.bits.addr(indexmsb,indexlsb)
  meta_arb.io.in(3).bits.rw := Bool(false)
  meta_arb.io.in(3).bits.way_en := Fix(-1)
  when (!meta_arb.io.in(3).ready) { io.cpu.req.ready := Bool(false) }
  def wayMap[T <: Data](f: Int => T)(gen: => T) = Vec((0 until conf.ways).map(i => f(i))){gen}
  val s1_tag_eq_way = wayMap((w: Int) => meta.io.resp(w).tag === (s1_addr >> conf.untagbits)){Bits()}.toBits
  val s1_hit_way = wayMap((w: Int) => s1_tag_eq_way(w) && conf.co.isHit(s1_req.cmd, meta.io.resp(w).state)){Bits()}.toBits
  val s1_tag_match_way = wayMap((w: Int) => s1_tag_eq_way(w) && conf.co.isValid(meta.io.resp(w).state)){Bits()}.toBits
  val s1_hit = s1_hit_way.orR
  val s1_clk_en = Reg(meta_arb.io.out.valid)
  val s2_tag_match_way = RegEn(s1_tag_match_way, s1_clk_en)
  val s2_tag_match = s2_tag_match_way.orR
  val s2_hit = Reg(s1_hit)
  val s2_data = wayMap((w: Int) => RegEn(data.io.resp(w), s1_clk_en && s1_tag_eq_way(w))){Bits()}
  val data_resp_mux = Mux1H(s2_tag_match_way, s2_data)

  // writeback unit
  wb.io.req <> mshr.io.wb_req
  wb.io.meta_req <> meta_arb.io.in(2)
  wb.io.data_req <> readArb.io.in(1)
  wb.io.data_resp <> data_resp_mux
  wb.io.probe_rep_data <> io.mem.probe_rep_data

  // replacement policy
  val replacer = new RandomReplacement
  val s1_replaced_way_en = UFixToOH(replacer.way)
  val s2_replaced_way_en = UFixToOH(RegEn(replacer.way, s1_clk_en))
  val s2_repl_state = Mux1H(s2_replaced_way_en, wayMap((w: Int) => RegEn(meta.io.resp(w).state, s1_clk_en && s1_replaced_way_en(w))){Bits()})
  val s2_repl_tag = Mux1H(s2_replaced_way_en, wayMap((w: Int) => RegEn(meta.io.resp(w).tag, s1_clk_en && s1_replaced_way_en(w))){Bits()})
  val s2_hit_state = Mux1H(s2_tag_match_way, wayMap((w: Int) => RegEn(meta.io.resp(w).state, s1_clk_en && s1_tag_eq_way(w))){Bits()})

  // refill response
  val refill = conf.co.messageUpdatesDataArray(io.mem.xact_rep.bits)
  writeArb.io.in(1).valid := io.mem.xact_rep.valid && refill
  io.mem.xact_rep.ready := writeArb.io.in(1).ready || !refill
  writeArb.io.in(1).bits := mshr.io.mem_resp
  writeArb.io.in(1).bits.wmask := Fix(-1)
  writeArb.io.in(1).bits.data := io.mem.xact_rep.bits.data

  // load hits
  readArb.io.in(2).bits.addr := io.cpu.req.bits.addr
  readArb.io.in(2).valid := io.cpu.req.valid
  readArb.io.in(2).bits.way_en := Fix(-1)
  when (!readArb.io.in(2).ready) { io.cpu.req.ready := Bool(false) }

  // store/amo hits
  def idxMatch(dst: HellaCacheReq, src: HellaCacheReq) = dst.addr(indexmsb,indexlsb) === src.addr(indexmsb,indexlsb)
  def offsetMatch(dst: HellaCacheReq, src: HellaCacheReq) = {
    def mask(x: HellaCacheReq) = StoreGen(x.typ, x.addr, Bits(0)).mask
    // TODO: this is overly restrictive. need write-combining buffer.
    isWrite(src.cmd) &&
    dst.addr(indexlsb-1,offsetlsb) === src.addr(indexlsb-1,offsetlsb) &&
    ((mask(dst) & mask(src)).orR || isWrite(dst.cmd))
  }
  def storeMatch(dst: HellaCacheReq, src: HellaCacheReq) = idxMatch(dst, src) && offsetMatch(dst, src)
  val p_store_match = s2_valid && storeMatch(s1_req, s2_req) ||
                      s3_valid && storeMatch(s1_req, s3_req)
  writeArb.io.in(0).bits.addr := s3_req.addr
  writeArb.io.in(0).bits.wmask := UFix(1) << s3_req.addr(conf.ramoffbits-1,offsetlsb).toUFix
  writeArb.io.in(0).bits.data := Fill(MEM_DATA_BITS/conf.databits, s3_req.data)
  writeArb.io.in(0).valid := s3_valid
  writeArb.io.in(0).bits.way_en :=  s3_way

  // tag update after a store to an exclusive clean line.
  val new_hit_state = conf.co.newStateOnHit(s2_req.cmd, s2_hit_state)
  meta.io.state_req.bits.rw := Bool(true)
  meta.io.state_req.bits.idx := s2_req.addr(indexmsb,indexlsb)
  meta.io.state_req.bits.data.state := new_hit_state
  meta.io.state_req.bits.way_en := s2_tag_match_way
  meta.io.state_req.valid := s2_valid_masked && s2_hit && s2_hit_state != new_hit_state
  
  // pending store data, also used for AMO RHS
  s3_valid := (s2_valid_masked && s2_hit || s2_replay) && isWrite(s2_req.cmd)
  val amoalu = new AMOALU
  when ((s2_valid || s2_replay) && isWrite(s2_req.cmd)) {
    s3_req := s2_req
    s3_req.data := amoalu.io.out
    s3_way := s2_tag_match_way
  }

  // miss handling
  mshr.io.req.valid := s2_valid_masked && !s2_hit && (isRead(s2_req.cmd) || isWrite(s2_req.cmd)) && !s2_nack_hit
  mshr.io.req.bits := s2_req
  mshr.io.req.bits.old_dirty := conf.co.needsWriteback(s2_repl_state) && !s2_tag_match // don't wb upgrades
  mshr.io.req.bits.old_tag := s2_repl_tag
  mshr.io.req.bits.way_en := Mux(s2_tag_match, s2_tag_match_way, s2_replaced_way_en)
  mshr.io.req.bits.data := s2_req.data

  mshr.io.mem_rep.valid := io.mem.xact_rep.fire()
  mshr.io.mem_rep.bits := io.mem.xact_rep.bits
  mshr.io.mem_abort.valid := io.mem.xact_abort.valid
  mshr.io.mem_abort.bits := io.mem.xact_abort.bits
  io.mem.xact_abort.ready := Bool(true)
  when (mshr.io.req.fire()) { replacer.miss }

  // replays
  readArb.io.in(0).valid := mshr.io.replay.valid
  readArb.io.in(0).bits := mshr.io.replay.bits
  readArb.io.in(0).bits.way_en := Fix(-1)
  mshr.io.replay.ready := Bool(true)
  s1_replay := mshr.io.replay.fire()
  meta_arb.io.in(0) <> mshr.io.meta_req

  // probes
  prober.io.req <> io.mem.probe_req
  prober.io.rep <> io.mem.probe_rep
  prober.io.mshr_req <> mshr.io.probe
  prober.io.wb_req <> wb.io.probe
  prober.io.way_en := s2_tag_match_way
  prober.io.line_state := s2_hit_state
  prober.io.meta_req <> meta_arb.io.in(1)

  // load data subword mux/sign extension.
  // subword loads are delayed by one cycle.
  val loadgen_data = data_resp_mux >> Cat(s2_req.addr(log2Up(MEM_DATA_BITS/8)-1,3), Bits(0,log2Up(conf.databits)))
  val loadgen = LoadGen(s2_req.typ, s2_req.addr, loadgen_data)

  amoalu.io := s2_req
  amoalu.io.lhs_raw := loadgen_data
  amoalu.io.lhs := loadgen.word
  amoalu.io.rhs := s2_req.data

  val s1_nack = p_store_match || dtlb.io.req.valid && dtlb.io.resp.miss ||
                idxMatch(s1_req, s2_req) && meta.io.state_req.valid ||
                s1_req.addr(indexmsb,indexlsb) === prober.io.meta_req.bits.idx && !prober.io.req.ready
  s2_nack_hit := Reg(s1_nack) || mshr.io.secondary_miss
  val s2_nack_miss = !s2_hit && !mshr.io.req.ready
  val s2_nack = s2_nack_hit || s2_nack_miss
  s2_valid_masked := s2_valid && !s2_nack

  // after a nack, block until nack condition resolves (saves energy)
  val block_fence = Reg(resetVal = Bool(false))
  block_fence := (s1_valid && s1_req.cmd === M_FENCE || block_fence) && !mshr.io.fence_rdy
  val block_miss = Reg(resetVal = Bool(false))
  block_miss := (s2_valid || block_miss) && s2_nack_miss
  when (block_fence || block_miss) {
    io.cpu.req.ready := Bool(false)
  }

  val s2_read = isRead(s2_req.cmd)
  io.cpu.resp.valid  := s2_read && (s2_replay || s2_valid_masked && s2_hit)
  io.cpu.resp.bits.nack := s2_valid && s2_nack
  io.cpu.resp.bits := s2_req
  io.cpu.resp.bits.replay := s2_replay && s2_read
  io.cpu.resp.bits.data := loadgen.word
  io.cpu.resp.bits.data_subword := loadgen.byte
  io.cpu.resp.bits.store_data := s2_req.data
  
  val xact_init_arb = (new Arbiter(2)) { new TransactionInit }
  xact_init_arb.io.in(0) <> wb.io.mem_req
  xact_init_arb.io.in(1).valid := mshr.io.mem_req.valid && prober.io.req.ready
  mshr.io.mem_req.ready := xact_init_arb.io.in(1).ready && prober.io.req.ready
  xact_init_arb.io.in(1).bits := mshr.io.mem_req.bits
  io.mem.xact_init <> xact_init_arb.io.out

  io.mem.xact_init_data <> wb.io.mem_req_data
  io.mem.xact_finish <> mshr.io.mem_finish
}
