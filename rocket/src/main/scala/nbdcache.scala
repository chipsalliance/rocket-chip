// See LICENSE for license details.

package rocket

import Chisel._
import uncore._
import junctions._
import cde.{Parameters, Field}
import Util._

case object WordBits extends Field[Int]
case object StoreDataQueueDepth extends Field[Int]
case object ReplayQueueDepth extends Field[Int]
case object NMSHRs extends Field[Int]
case object LRSCCycles extends Field[Int]

trait HasL1HellaCacheParameters extends HasL1CacheParameters {
  val wordBits = p(WordBits)
  val wordBytes = wordBits/8
  val wordOffBits = log2Up(wordBytes)
  val beatBytes = p(CacheBlockBytes) / outerDataBeats
  val beatWords = beatBytes / wordBytes
  val beatOffBits = log2Up(beatBytes)
  val idxMSB = untagBits-1
  val idxLSB = blockOffBits
  val offsetmsb = idxLSB-1
  val offsetlsb = wordOffBits
  val rowWords = rowBits/wordBits 
  val doNarrowRead = coreDataBits * nWays % rowBits == 0
  val encDataBits = code.width(coreDataBits)
  val encRowBits = encDataBits*rowWords
  val sdqDepth = p(StoreDataQueueDepth)
  val nMSHRs = p(NMSHRs)
  val nIOMSHRs = 1
  val lrscCycles = p(LRSCCycles)
}

abstract class L1HellaCacheModule(implicit val p: Parameters) extends Module
  with HasL1HellaCacheParameters
abstract class L1HellaCacheBundle(implicit val p: Parameters) extends junctions.ParameterizedBundle()(p)
  with HasL1HellaCacheParameters

trait HasCoreMemOp extends HasCoreParameters {
  val addr = UInt(width = coreMaxAddrBits)
  val tag  = Bits(width = coreDCacheReqTagBits)
  val cmd  = Bits(width = M_SZ)
  val typ  = Bits(width = MT_SZ)
}

trait HasCoreData extends HasCoreParameters {
  val data = Bits(width = coreDataBits)
}

trait HasSDQId extends HasL1HellaCacheParameters {
  val sdq_id = UInt(width = log2Up(sdqDepth))
}

trait HasMissInfo extends HasL1HellaCacheParameters {
  val tag_match = Bool()
  val old_meta = new L1Metadata
  val way_en = Bits(width = nWays)
}

class HellaCacheReqInternal(implicit p: Parameters) extends L1HellaCacheBundle()(p)
    with HasCoreMemOp {
  val kill = Bool()
  val phys = Bool()
}

class HellaCacheReq(implicit p: Parameters) extends HellaCacheReqInternal()(p) with HasCoreData

class HellaCacheResp(implicit p: Parameters) extends L1HellaCacheBundle()(p)
    with HasCoreMemOp
    with HasCoreData {
  val nack = Bool() // comes 2 cycles after req.fire
  val replay = Bool()
  val has_data = Bool()
  val data_word_bypass = Bits(width = coreDataBits)
  val store_data = Bits(width = coreDataBits)
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
class HellaCacheIO(implicit p: Parameters) extends CoreBundle()(p) {
  val req = Decoupled(new HellaCacheReq)
  val resp = Valid(new HellaCacheResp).flip
  val replay_next = Valid(Bits(width = coreDCacheReqTagBits)).flip
  val xcpt = (new HellaCacheExceptions).asInput
  val invalidate_lr = Bool(OUTPUT)
  val ordered = Bool(INPUT)
}

class L1DataReadReq(implicit p: Parameters) extends L1HellaCacheBundle()(p) {
  val way_en = Bits(width = nWays)
  val addr   = Bits(width = untagBits)
}

class L1DataWriteReq(implicit p: Parameters) extends L1DataReadReq()(p) {
  val wmask  = Bits(width = rowWords)
  val data   = Bits(width = encRowBits)
}

class L1RefillReq(implicit p: Parameters) extends L1DataReadReq()(p)

class L1MetaReadReq(implicit p: Parameters) extends MetaReadReq {
  val tag = Bits(width = tagBits)
  override def cloneType = new L1MetaReadReq()(p).asInstanceOf[this.type] //TODO remove
}

class L1MetaWriteReq(implicit p: Parameters) extends 
  MetaWriteReq[L1Metadata](new L1Metadata)

object L1Metadata {
  def apply(tag: Bits, coh: ClientMetadata)(implicit p: Parameters) = {
    val meta = Wire(new L1Metadata)
    meta.tag := tag
    meta.coh := coh
    meta
  }
}
class L1Metadata(implicit p: Parameters) extends Metadata()(p) with HasL1HellaCacheParameters {
  val coh = new ClientMetadata
}

class Replay(implicit p: Parameters) extends HellaCacheReqInternal()(p) with HasCoreData
class ReplayInternal(implicit p: Parameters) extends HellaCacheReqInternal()(p) with HasSDQId

class MSHRReq(implicit p: Parameters) extends Replay()(p) with HasMissInfo
class MSHRReqInternal(implicit p: Parameters) extends ReplayInternal()(p) with HasMissInfo

class ProbeInternal(implicit p: Parameters) extends Probe()(p) with HasClientTransactionId

class WritebackReq(implicit p: Parameters) extends Release()(p) with HasCacheParameters {
  val way_en = Bits(width = nWays)
}

class IOMSHR(id: Int)(implicit p: Parameters) extends L1HellaCacheModule()(p) {
  val io = new Bundle {
    val req = Decoupled(new HellaCacheReq).flip
    val acquire = Decoupled(new Acquire)
    val grant = Valid(new Grant).flip
    val resp = Decoupled(new HellaCacheResp)
  }

  def wordFromBeat(addr: UInt, dat: UInt) = {
    val offset = addr(beatOffBits - 1, wordOffBits)
    val shift = Cat(offset, UInt(0, wordOffBits + 3))
    (dat >> shift)(wordBits - 1, 0)
  }

  val req = Reg(new HellaCacheReq)
  val req_cmd_sc = req.cmd === M_XSC
  val grant_word = Reg(UInt(width = wordBits))

  val storegen = new StoreGen(req.typ, req.addr, req.data, 8)
  val loadgen = new LoadGen(req.typ, req.addr, grant_word, req_cmd_sc, 8)

  val beat_offset = req.addr(beatOffBits - 1, wordOffBits)
  val beat_mask = (storegen.mask << Cat(beat_offset, UInt(0, wordOffBits)))
  val beat_data = Fill(beatWords, storegen.data)

  val s_idle :: s_acquire :: s_grant :: s_resp :: Nil = Enum(Bits(), 4)
  val state = Reg(init = s_idle)

  io.req.ready := (state === s_idle)

  val addr_block = req.addr(paddrBits - 1, blockOffBits)
  val addr_beat  = req.addr(blockOffBits - 1, beatOffBits)
  val addr_byte  = req.addr(beatOffBits - 1, 0)

  val get_acquire = Get(
    client_xact_id = UInt(id),
    addr_block = addr_block,
    addr_beat = addr_beat,
    addr_byte = addr_byte,
    operand_size = req.typ,
    alloc = Bool(false))

  val put_acquire = Put(
    client_xact_id = UInt(id),
    addr_block = addr_block,
    addr_beat = addr_beat,
    data = beat_data,
    wmask = beat_mask,
    alloc = Bool(false))

  io.acquire.valid := (state === s_acquire)
  io.acquire.bits := Mux(isRead(req.cmd), get_acquire, put_acquire)

  io.resp.valid := (state === s_resp)
  io.resp.bits := req
  io.resp.bits.has_data := isRead(req.cmd)
  io.resp.bits.data := loadgen.data | req_cmd_sc
  io.resp.bits.store_data := req.data
  io.resp.bits.nack := Bool(false)
  io.resp.bits.replay := io.resp.valid

  when (io.req.fire()) {
    req := io.req.bits
    state := s_acquire
  }

  when (io.acquire.fire()) {
    state := s_grant
  }

  when (state === s_grant && io.grant.valid) {
    when (isRead(req.cmd)) {
      grant_word := wordFromBeat(req.addr, io.grant.bits.data)
      state := s_resp
    } .otherwise {
      state := s_idle
    }
  }

  when (io.resp.fire()) {
    state := s_idle
  }
}

class MSHR(id: Int)(implicit p: Parameters) extends L1HellaCacheModule()(p) {
  val io = new Bundle {
    val req_pri_val    = Bool(INPUT)
    val req_pri_rdy    = Bool(OUTPUT)
    val req_sec_val    = Bool(INPUT)
    val req_sec_rdy    = Bool(OUTPUT)
    val req_bits       = new MSHRReqInternal().asInput

    val idx_match       = Bool(OUTPUT)
    val tag             = Bits(OUTPUT, tagBits)

    val mem_req  = Decoupled(new Acquire)
    val refill = new L1RefillReq().asOutput // Data is bypassed
    val meta_read = Decoupled(new L1MetaReadReq)
    val meta_write = Decoupled(new L1MetaWriteReq)
    val replay = Decoupled(new ReplayInternal)
    val mem_grant = Valid(new Grant).flip
    val wb_req = Decoupled(new WritebackReq)
    val probe_rdy = Bool(OUTPUT)
  }

  val s_invalid :: s_wb_req :: s_wb_resp :: s_meta_clear :: s_refill_req :: s_refill_resp :: s_meta_write_req :: s_meta_write_resp :: s_drain_rpq :: Nil = Enum(UInt(), 9)
  val state = Reg(init=s_invalid)

  val new_coh_state = Reg(init=ClientMetadata.onReset)
  val req = Reg(new MSHRReqInternal())
  val req_idx = req.addr(untagBits-1,blockOffBits)
  val idx_match = req_idx === io.req_bits.addr(untagBits-1,blockOffBits)
  // We only accept secondary misses if we haven't yet sent an Acquire to outer memory
  // or if the Acquire that was sent will obtain a Grant with sufficient permissions
  // to let us replay this new request. I.e. we don't handle multiple outstanding
  // Acquires on the same block for now.
  val cmd_requires_second_acquire = 
    req.old_meta.coh.requiresAcquireOnSecondaryMiss(req.cmd, io.req_bits.cmd)
  val states_before_refill = Vec(s_wb_req, s_wb_resp, s_meta_clear)
  val sec_rdy = idx_match &&
                  (states_before_refill.contains(state) ||
                    (Vec(s_refill_req, s_refill_resp).contains(state) &&
                      !cmd_requires_second_acquire))
  val gnt_multi_data = io.mem_grant.bits.hasMultibeatData()
  val (refill_cnt, refill_count_done) = Counter(io.mem_grant.valid && gnt_multi_data, refillCycles) // TODO: Zero width?
  val refill_done = io.mem_grant.valid && (!gnt_multi_data || refill_count_done)

  val rpq = Module(new Queue(new ReplayInternal, p(ReplayQueueDepth)))
  rpq.io.enq.valid := (io.req_pri_val && io.req_pri_rdy || io.req_sec_val && sec_rdy) && !isPrefetch(io.req_bits.cmd)
  rpq.io.enq.bits := io.req_bits
  rpq.io.deq.ready := io.replay.ready && state === s_drain_rpq || state === s_invalid

  val coh_on_grant = req.old_meta.coh.onGrant(
                          incoming = io.mem_grant.bits,
                          pending = req.cmd)
  val coh_on_hit =  io.req_bits.old_meta.coh.onHit(io.req_bits.cmd)

  when (state === s_drain_rpq && !rpq.io.deq.valid) {
    state := s_invalid
  }
  when (state === s_meta_write_resp) {
    // this wait state allows us to catch RAW hazards on the tags via nack_victim
    state := s_drain_rpq
  }
  when (state === s_meta_write_req && io.meta_write.ready) {
    state := s_meta_write_resp
  }
  when (state === s_refill_resp) {
    when (io.mem_grant.valid) { new_coh_state := coh_on_grant }
    when (refill_done) { state := s_meta_write_req }
  }
  when (io.mem_req.fire()) { // s_refill_req
    state := s_refill_resp
  }
  when (state === s_meta_clear && io.meta_write.ready) {
    state := s_refill_req
  }
  when (state === s_wb_resp && io.mem_grant.valid) {
    state := s_meta_clear
  }
  when (io.wb_req.fire()) { // s_wb_req
    state := Mux(io.wb_req.bits.requiresAck(), s_wb_resp, s_meta_clear)
  }
  when (io.req_sec_val && io.req_sec_rdy) { // s_wb_req, s_wb_resp, s_refill_req
    //If we get a secondary miss that needs more permissions before we've sent
    //  out the primary miss's Acquire, we can upgrade the permissions we're 
    //  going to ask for in s_refill_req
    when(cmd_requires_second_acquire) {
      req.cmd := io.req_bits.cmd
    }
  }
  when (io.req_pri_val && io.req_pri_rdy) {
    val coh = io.req_bits.old_meta.coh
    req := io.req_bits
    when (io.req_bits.tag_match) {
      when(coh.isHit(io.req_bits.cmd)) { // set dirty bit
        state := s_meta_write_req
        new_coh_state := coh_on_hit
      }.otherwise { // upgrade permissions
        state := s_refill_req
      }
    }.otherwise { // writback if necessary and refill
      state := Mux(coh.requiresVoluntaryWriteback(), s_wb_req, s_meta_clear)
    }
  }

  io.idx_match := (state =/= s_invalid) && idx_match
  io.refill.way_en := req.way_en
  io.refill.addr := (if(refillCycles > 1) Cat(req_idx, refill_cnt) else req_idx) << rowOffBits
  io.tag := req.addr >> untagBits
  io.req_pri_rdy := state === s_invalid
  io.req_sec_rdy := sec_rdy && rpq.io.enq.ready

  val meta_hazard = Reg(init=UInt(0,2))
  when (meta_hazard =/= UInt(0)) { meta_hazard := meta_hazard + 1 }
  when (io.meta_write.fire()) { meta_hazard := 1 }
  io.probe_rdy := !idx_match || (!states_before_refill.contains(state) && meta_hazard === 0) 

  io.meta_write.valid := state === s_meta_write_req || state === s_meta_clear
  io.meta_write.bits.idx := req_idx
  io.meta_write.bits.data.coh := Mux(state === s_meta_clear,
                                      req.old_meta.coh.onCacheControl(M_FLUSH),
                                      new_coh_state)
  io.meta_write.bits.data.tag := io.tag
  io.meta_write.bits.way_en := req.way_en

  io.wb_req.valid := state === s_wb_req
  io.wb_req.bits := req.old_meta.coh.makeVoluntaryWriteback(
                      client_xact_id = UInt(id),
                      addr_block = Cat(req.old_meta.tag, req_idx))
  io.wb_req.bits.way_en := req.way_en

  io.mem_req.valid := state === s_refill_req
  io.mem_req.bits := req.old_meta.coh.makeAcquire(
                       addr_block = Cat(io.tag, req_idx).toUInt,
                       client_xact_id = Bits(id),
                       op_code = req.cmd)

  io.meta_read.valid := state === s_drain_rpq
  io.meta_read.bits.idx := req_idx
  io.meta_read.bits.tag := io.tag

  io.replay.valid := state === s_drain_rpq && rpq.io.deq.valid
  io.replay.bits := rpq.io.deq.bits
  io.replay.bits.phys := Bool(true)
  io.replay.bits.addr := Cat(io.tag, req_idx, rpq.io.deq.bits.addr(blockOffBits-1,0)).toUInt

  when (!io.meta_read.ready) {
    rpq.io.deq.ready := Bool(false)
    io.replay.bits.cmd := M_NOP
  }
}

class MSHRFile(implicit p: Parameters) extends L1HellaCacheModule()(p) {
  val io = new Bundle {
    val req = Decoupled(new MSHRReq).flip
    val resp = Decoupled(new HellaCacheResp)
    val secondary_miss = Bool(OUTPUT)

    val mem_req  = Decoupled(new Acquire)
    val refill = new L1RefillReq().asOutput
    val meta_read = Decoupled(new L1MetaReadReq)
    val meta_write = Decoupled(new L1MetaWriteReq)
    val replay = Decoupled(new Replay)
    val mem_grant = Valid(new Grant).flip
    val wb_req = Decoupled(new WritebackReq)

    val probe_rdy = Bool(OUTPUT)
    val fence_rdy = Bool(OUTPUT)
  }

  // determine if the request is in the memory region or mmio region
  val cacheable = io.req.bits.addr < UInt(mmioBase)

  val sdq_val = Reg(init=Bits(0, sdqDepth))
  val sdq_alloc_id = PriorityEncoder(~sdq_val(sdqDepth-1,0))
  val sdq_rdy = !sdq_val.andR
  val sdq_enq = io.req.valid && io.req.ready && cacheable && isWrite(io.req.bits.cmd)
  val sdq = Mem(sdqDepth, io.req.bits.data)
  when (sdq_enq) { sdq(sdq_alloc_id) := io.req.bits.data }

  val idxMatch = Wire(Vec(nMSHRs, Bool()))
  val tagList = Wire(Vec(nMSHRs, Bits(width = tagBits)))
  val tag_match = Mux1H(idxMatch, tagList) === io.req.bits.addr >> untagBits

  val wbTagList = Wire(Vec(nMSHRs, Bits()))
  val refillMux = Wire(Vec(nMSHRs, new L1RefillReq))
  val meta_read_arb = Module(new Arbiter(new L1MetaReadReq, nMSHRs))
  val meta_write_arb = Module(new Arbiter(new L1MetaWriteReq, nMSHRs))
  val mem_req_arb = Module(new LockingArbiter(
                                  new Acquire,
                                  nMSHRs + nIOMSHRs,
                                  outerDataBeats,
                                  (a: Acquire) => a.hasMultibeatData()))
  val wb_req_arb = Module(new Arbiter(new WritebackReq, nMSHRs))
  val replay_arb = Module(new Arbiter(new ReplayInternal, nMSHRs))
  val alloc_arb = Module(new Arbiter(Bool(), nMSHRs))

  var idx_match = Bool(false)
  var pri_rdy = Bool(false)
  var sec_rdy = Bool(false)

  io.fence_rdy := true
  io.probe_rdy := true

  for (i <- 0 until nMSHRs) {
    val mshr = Module(new MSHR(i))

    idxMatch(i) := mshr.io.idx_match
    tagList(i) := mshr.io.tag
    wbTagList(i) := mshr.io.wb_req.bits.addr_block >> idxBits

    alloc_arb.io.in(i).valid := mshr.io.req_pri_rdy
    mshr.io.req_pri_val := alloc_arb.io.in(i).ready

    mshr.io.req_sec_val := io.req.valid && sdq_rdy && tag_match
    mshr.io.req_bits := io.req.bits
    mshr.io.req_bits.sdq_id := sdq_alloc_id

    meta_read_arb.io.in(i) <> mshr.io.meta_read
    meta_write_arb.io.in(i) <> mshr.io.meta_write
    mem_req_arb.io.in(i) <> mshr.io.mem_req
    wb_req_arb.io.in(i) <> mshr.io.wb_req
    replay_arb.io.in(i) <> mshr.io.replay

    mshr.io.mem_grant.valid := io.mem_grant.valid &&
                                 io.mem_grant.bits.client_xact_id === UInt(i)
    mshr.io.mem_grant.bits := io.mem_grant.bits
    refillMux(i) := mshr.io.refill

    pri_rdy = pri_rdy || mshr.io.req_pri_rdy
    sec_rdy = sec_rdy || mshr.io.req_sec_rdy
    idx_match = idx_match || mshr.io.idx_match

    when (!mshr.io.req_pri_rdy) { io.fence_rdy := false }
    when (!mshr.io.probe_rdy) { io.probe_rdy := false }
  }

  alloc_arb.io.out.ready := io.req.valid && sdq_rdy && cacheable && !idx_match

  io.meta_read <> meta_read_arb.io.out
  io.meta_write <> meta_write_arb.io.out
  io.mem_req <> mem_req_arb.io.out
  io.wb_req <> wb_req_arb.io.out

  val mmio_alloc_arb = Module(new Arbiter(Bool(), nIOMSHRs))
  val resp_arb = Module(new Arbiter(new HellaCacheResp, nIOMSHRs))

  var mmio_rdy = Bool(false)

  for (i <- 0 until nIOMSHRs) {
    val id = nMSHRs + i
    val mshr = Module(new IOMSHR(id))

    mmio_alloc_arb.io.in(i).valid := mshr.io.req.ready
    mshr.io.req.valid := mmio_alloc_arb.io.in(i).ready
    mshr.io.req.bits := io.req.bits

    mmio_rdy = mmio_rdy || mshr.io.req.ready

    mem_req_arb.io.in(id) <> mshr.io.acquire

    mshr.io.grant.bits := io.mem_grant.bits
    mshr.io.grant.valid := io.mem_grant.valid &&
        io.mem_grant.bits.client_xact_id === UInt(id)

    resp_arb.io.in(i) <> mshr.io.resp

    when (!mshr.io.req.ready) { io.fence_rdy := Bool(false) }
  }

  mmio_alloc_arb.io.out.ready := io.req.valid && !cacheable

  io.resp <> resp_arb.io.out
  io.req.ready := Mux(!cacheable, mmio_rdy,
    Mux(idx_match, tag_match && sec_rdy, pri_rdy) && sdq_rdy)
  io.secondary_miss := idx_match
  io.refill := refillMux(io.mem_grant.bits.client_xact_id)

  val free_sdq = io.replay.fire() && isWrite(io.replay.bits.cmd)
  io.replay.bits.data := sdq(RegEnable(replay_arb.io.out.bits.sdq_id, free_sdq))
  io.replay <> replay_arb.io.out

  when (io.replay.valid || sdq_enq) {
    sdq_val := sdq_val & ~(UIntToOH(replay_arb.io.out.bits.sdq_id) & Fill(sdqDepth, free_sdq)) | 
               PriorityEncoderOH(~sdq_val(sdqDepth-1,0)) & Fill(sdqDepth, sdq_enq)
  }
}

class WritebackUnit(implicit p: Parameters) extends L1HellaCacheModule()(p) {
  val io = new Bundle {
    val req = Decoupled(new WritebackReq).flip
    val meta_read = Decoupled(new L1MetaReadReq)
    val data_req = Decoupled(new L1DataReadReq)
    val data_resp = Bits(INPUT, encRowBits)
    val release = Decoupled(new Release)
  }

  val active = Reg(init=Bool(false))
  val r1_data_req_fired = Reg(init=Bool(false))
  val r2_data_req_fired = Reg(init=Bool(false))
  val data_req_cnt = Reg(init = UInt(0, width = log2Up(refillCycles+1))) //TODO Zero width
  val buf_v = (if(refillCyclesPerBeat > 1) Reg(init=Bits(0, width = refillCyclesPerBeat-1)) else Bits(1))
  val beat_done = buf_v.andR
  val (beat_cnt, all_beats_done) = Counter(io.release.fire(), outerDataBeats)
  val req = Reg(new WritebackReq)

  io.release.valid := false
  when (active) {
    r1_data_req_fired := false
    r2_data_req_fired := r1_data_req_fired
    when (io.data_req.fire() && io.meta_read.fire()) {
      r1_data_req_fired := true
      data_req_cnt := data_req_cnt + 1
    }
    when (r2_data_req_fired) {
      io.release.valid := beat_done
      when(beat_done) {
        when(!io.release.ready) {
          r1_data_req_fired := false
          r2_data_req_fired := false
          data_req_cnt := data_req_cnt - Mux[UInt](Bool(refillCycles > 1) && r1_data_req_fired, 2, 1)
        } .otherwise { if(refillCyclesPerBeat > 1) buf_v := 0 }
      }
      when(!r1_data_req_fired) {
        // We're done if this is the final data request and the Release can be sent
        active := data_req_cnt < UInt(refillCycles) || !io.release.ready
      }
    }
  }
  when (io.req.fire()) {
    active := true
    data_req_cnt := 0
    if(refillCyclesPerBeat > 1) buf_v := 0
    req := io.req.bits
  }

  io.req.ready := !active

  val req_idx = req.addr_block(idxBits-1, 0)
  val fire = active && data_req_cnt < UInt(refillCycles)

  // We reissue the meta read as it sets up the mux ctrl for s2_data_muxed
  io.meta_read.valid := fire
  io.meta_read.bits.idx := req_idx
  io.meta_read.bits.tag := req.addr_block >> idxBits

  io.data_req.valid := fire
  io.data_req.bits.way_en := req.way_en
  io.data_req.bits.addr := (if(refillCycles > 1) 
                              Cat(req_idx, data_req_cnt(log2Up(refillCycles)-1,0))
                            else req_idx) << rowOffBits

  io.release.bits := req
  io.release.bits.addr_beat := beat_cnt
  io.release.bits.data := (if(refillCyclesPerBeat > 1) {
    // If the cache rows are narrower than a TLDataBeat, 
    //   then buffer enough data_resps to make a whole beat
    val data_buf = Reg(Bits())
    when(active && r2_data_req_fired && !beat_done) {
      data_buf := Cat(io.data_resp, data_buf((refillCyclesPerBeat-1)*encRowBits-1, encRowBits))
      buf_v := (if(refillCyclesPerBeat > 2)
                  Cat(UInt(1), buf_v(refillCyclesPerBeat-2,1))
                else UInt(1))
    }
    Cat(io.data_resp, data_buf)
  } else { io.data_resp })
}

class ProbeUnit(implicit p: Parameters) extends L1HellaCacheModule()(p) {
  val io = new Bundle {
    val req = Decoupled(new ProbeInternal).flip
    val rep = Decoupled(new Release)
    val meta_read = Decoupled(new L1MetaReadReq)
    val meta_write = Decoupled(new L1MetaWriteReq)
    val wb_req = Decoupled(new WritebackReq)
    val way_en = Bits(INPUT, nWays)
    val mshr_rdy = Bool(INPUT)
    val block_state = new ClientMetadata().asInput
  }

  val (s_invalid :: s_meta_read :: s_meta_resp :: s_mshr_req ::
       s_mshr_resp :: s_release :: s_writeback_req :: s_writeback_resp :: 
       s_meta_write :: Nil) = Enum(UInt(), 9)
  val state = Reg(init=s_invalid)
  val old_coh = Reg(new ClientMetadata)
  val way_en = Reg(Bits())
  val req = Reg(new ProbeInternal)
  val tag_matches = way_en.orR

  val miss_coh = ClientMetadata.onReset
  val reply_coh = Mux(tag_matches, old_coh, miss_coh)
  val reply = reply_coh.makeRelease(req)
  io.req.ready := state === s_invalid
  io.rep.valid := state === s_release
  io.rep.bits := reply

  assert(!io.rep.valid || !io.rep.bits.hasData(),
    "ProbeUnit should not send releases with data")

  io.meta_read.valid := state === s_meta_read
  io.meta_read.bits.idx := req.addr_block
  io.meta_read.bits.tag := req.addr_block >> idxBits

  io.meta_write.valid := state === s_meta_write
  io.meta_write.bits.way_en := way_en
  io.meta_write.bits.idx := req.addr_block
  io.meta_write.bits.data.tag := req.addr_block >> idxBits
  io.meta_write.bits.data.coh := old_coh.onProbe(req)

  io.wb_req.valid := state === s_writeback_req
  io.wb_req.bits := reply
  io.wb_req.bits.way_en := way_en

  // state === s_invalid
  when (io.req.fire()) {
    state := s_meta_read
    req := io.req.bits
  }

  // state === s_meta_read
  when (io.meta_read.fire()) {
    state := s_meta_resp
  }

  // we need to wait one cycle for the metadata to be read from the array
  when (state === s_meta_resp) {
    state := s_mshr_req
  }

  when (state === s_mshr_req) {
    state := s_mshr_resp
    old_coh := io.block_state
    way_en := io.way_en
    // if the read didn't go through, we need to retry
    when (!io.mshr_rdy) { state := s_meta_read }
  }

  when (state === s_mshr_resp) {
    val needs_writeback = tag_matches && old_coh.requiresVoluntaryWriteback() 
    state := Mux(needs_writeback, s_writeback_req, s_release)
  }

  when (state === s_release && io.rep.ready) {
    state := Mux(tag_matches, s_meta_write, s_invalid)
  }

  // state === s_writeback_req
  when (io.wb_req.fire()) {
    state := s_writeback_resp
  }

  // wait for the writeback request to finish before updating the metadata
  when (state === s_writeback_resp && io.wb_req.ready) {
    state := s_meta_write
  }

  when (io.meta_write.fire()) {
    state := s_invalid
  }
}

class DataArray(implicit p: Parameters) extends L1HellaCacheModule()(p) {
  val io = new Bundle {
    val read = Decoupled(new L1DataReadReq).flip
    val write = Decoupled(new L1DataWriteReq).flip
    val resp = Vec(nWays, Bits(OUTPUT, encRowBits))
  }

  val waddr = io.write.bits.addr >> rowOffBits
  val raddr = io.read.bits.addr >> rowOffBits

  if (doNarrowRead) {
    for (w <- 0 until nWays by rowWords) {
      val wway_en = io.write.bits.way_en(w+rowWords-1,w)
      val rway_en = io.read.bits.way_en(w+rowWords-1,w)
      val resp = Wire(Vec(rowWords, Bits(width = encRowBits)))
      val r_raddr = RegEnable(io.read.bits.addr, io.read.valid)
      for (p <- 0 until resp.size) {
        val array = SeqMem(nSets*refillCycles, Vec(rowWords, Bits(width=encDataBits)))
        when (wway_en.orR && io.write.valid && io.write.bits.wmask(p)) {
          val data = Vec.fill(rowWords)(io.write.bits.data(encDataBits*(p+1)-1,encDataBits*p))
          array.write(waddr, data, wway_en.toBools)
        }
        resp(p) := array.read(raddr, rway_en.orR && io.read.valid).toBits
      }
      for (dw <- 0 until rowWords) {
        val r = Vec(resp.map(_(encDataBits*(dw+1)-1,encDataBits*dw)))
        val resp_mux =
          if (r.size == 1) r
          else Vec(r(r_raddr(rowOffBits-1,wordOffBits)), r.tail:_*)
        io.resp(w+dw) := resp_mux.toBits
      }
    }
  } else {
    for (w <- 0 until nWays) {
      val array = SeqMem(nSets*refillCycles, Vec(rowWords, Bits(width=encDataBits)))
      when (io.write.bits.way_en(w) && io.write.valid) {
        val data = Vec.tabulate(rowWords)(i => io.write.bits.data(encDataBits*(i+1)-1,encDataBits*i))
        array.write(waddr, data, io.write.bits.wmask.toBools)
      }
      io.resp(w) := array.read(raddr, io.read.bits.way_en(w) && io.read.valid).toBits
    }
  }

  io.read.ready := Bool(true)
  io.write.ready := Bool(true)
}

class HellaCache(implicit p: Parameters) extends L1HellaCacheModule()(p) {
  val io = new Bundle {
    val cpu = (new HellaCacheIO).flip
    val ptw = new TLBPTWIO()
    val mem = new ClientTileLinkIO
  }
 
  require(lrscCycles >= 32) // ISA requires 16-insn LRSC sequences to succeed
  require(isPow2(nSets))
  require(isPow2(nWays)) // TODO: relax this
  require(rowBits <= outerDataBits)
  require(untagBits <= pgIdxBits)

  val wb = Module(new WritebackUnit)
  val prober = Module(new ProbeUnit)
  val mshrs = Module(new MSHRFile)

  io.cpu.req.ready := Bool(true)
  val s1_valid = Reg(next=io.cpu.req.fire(), init=Bool(false))
  val s1_req = Reg(io.cpu.req.bits)
  val s1_valid_masked = s1_valid && !io.cpu.req.bits.kill
  val s1_replay = Reg(init=Bool(false))
  val s1_clk_en = Reg(Bool())

  val s2_valid = Reg(next=s1_valid_masked, init=Bool(false))
  val s2_killed = Reg(next=s1_valid && io.cpu.req.bits.kill)
  val s2_req = Reg(io.cpu.req.bits)
  val s2_replay = Reg(next=s1_replay, init=Bool(false)) && s2_req.cmd =/= M_NOP
  val s2_recycle = Wire(Bool())
  val s2_valid_masked = Wire(Bool())

  val s3_valid = Reg(init=Bool(false))
  val s3_req = Reg(io.cpu.req.bits)
  val s3_way = Reg(Bits())

  val s1_recycled = RegEnable(s2_recycle, Bool(false), s1_clk_en)
  val s1_read  = isRead(s1_req.cmd)
  val s1_write = isWrite(s1_req.cmd)
  val s1_readwrite = s1_read || s1_write || isPrefetch(s1_req.cmd)

  val dtlb = Module(new TLB)
  io.ptw <> dtlb.io.ptw
  dtlb.io.req.valid := s1_valid_masked && s1_readwrite && !s1_req.phys
  dtlb.io.req.bits.passthrough := s1_req.phys
  dtlb.io.req.bits.asid := UInt(0)
  dtlb.io.req.bits.vpn := s1_req.addr >> pgIdxBits
  dtlb.io.req.bits.instruction := Bool(false)
  dtlb.io.req.bits.store := s1_write
  when (!dtlb.io.req.ready && !io.cpu.req.bits.phys) { io.cpu.req.ready := Bool(false) }
  
  when (io.cpu.req.valid) {
    s1_req := io.cpu.req.bits
  }
  when (wb.io.meta_read.valid) {
    s1_req.addr := Cat(wb.io.meta_read.bits.tag, wb.io.meta_read.bits.idx) << blockOffBits
    s1_req.phys := Bool(true)
  }
  when (prober.io.meta_read.valid) {
    s1_req.addr := Cat(prober.io.meta_read.bits.tag, prober.io.meta_read.bits.idx) << blockOffBits
    s1_req.phys := Bool(true)
  }
  when (mshrs.io.replay.valid) {
    s1_req := mshrs.io.replay.bits
  }
  when (s2_recycle) {
    s1_req := s2_req
  }
  val s1_addr = Cat(dtlb.io.resp.ppn, s1_req.addr(pgIdxBits-1,0))

  when (s1_clk_en) {
    s2_req.kill := s1_req.kill
    s2_req.typ := s1_req.typ
    s2_req.phys := s1_req.phys
    s2_req.addr := s1_addr
    when (s1_write) {
      s2_req.data := Mux(s1_replay, mshrs.io.replay.bits.data, io.cpu.req.bits.data)
    }
    when (s1_recycled) { s2_req.data := s1_req.data }
    s2_req.tag := s1_req.tag
    s2_req.cmd := s1_req.cmd
  }

  val misaligned = new StoreGen(s1_req.typ, s1_req.addr, UInt(0), 8).misaligned
  io.cpu.xcpt.ma.ld := s1_read && misaligned
  io.cpu.xcpt.ma.st := s1_write && misaligned
  io.cpu.xcpt.pf.ld := s1_read && dtlb.io.resp.xcpt_ld
  io.cpu.xcpt.pf.st := s1_write && dtlb.io.resp.xcpt_st

  assert (!(Reg(next=
    (io.cpu.xcpt.ma.ld || io.cpu.xcpt.ma.st || io.cpu.xcpt.pf.ld || io.cpu.xcpt.pf.st)) &&
    io.cpu.resp.valid), "DCache exception occurred - cache response not killed.")

  // tags
  def onReset = L1Metadata(UInt(0), ClientMetadata.onReset)
  val meta = Module(new MetadataArray(onReset _))
  val metaReadArb = Module(new Arbiter(new MetaReadReq, 5))
  val metaWriteArb = Module(new Arbiter(new L1MetaWriteReq, 2))
  meta.io.read <> metaReadArb.io.out
  meta.io.write <> metaWriteArb.io.out

  // data
  val data = Module(new DataArray)
  val readArb = Module(new Arbiter(new L1DataReadReq, 4))
  val writeArb = Module(new Arbiter(new L1DataWriteReq, 2))
  data.io.write.valid := writeArb.io.out.valid
  writeArb.io.out.ready := data.io.write.ready
  data.io.write.bits := writeArb.io.out.bits
  val wdata_encoded = (0 until rowWords).map(i => code.encode(writeArb.io.out.bits.data(coreDataBits*(i+1)-1,coreDataBits*i)))
  data.io.write.bits.data := Vec(wdata_encoded).toBits

  // tag read for new requests
  metaReadArb.io.in(4).valid := io.cpu.req.valid
  metaReadArb.io.in(4).bits.idx := io.cpu.req.bits.addr >> blockOffBits
  when (!metaReadArb.io.in(4).ready) { io.cpu.req.ready := Bool(false) }

  // data read for new requests
  readArb.io.in(3).valid := io.cpu.req.valid
  readArb.io.in(3).bits.addr := io.cpu.req.bits.addr
  readArb.io.in(3).bits.way_en := ~UInt(0, nWays)
  when (!readArb.io.in(3).ready) { io.cpu.req.ready := Bool(false) }

  // recycled requests
  metaReadArb.io.in(0).valid := s2_recycle
  metaReadArb.io.in(0).bits.idx := s2_req.addr >> blockOffBits
  readArb.io.in(0).valid := s2_recycle
  readArb.io.in(0).bits.addr := s2_req.addr
  readArb.io.in(0).bits.way_en := ~UInt(0, nWays)

  // tag check and way muxing
  def wayMap[T <: Data](f: Int => T) = Vec((0 until nWays).map(f))
  val s1_tag_eq_way = wayMap((w: Int) => meta.io.resp(w).tag === (s1_addr >> untagBits)).toBits
  val s1_tag_match_way = wayMap((w: Int) => s1_tag_eq_way(w) && meta.io.resp(w).coh.isValid()).toBits
  s1_clk_en := metaReadArb.io.out.valid //TODO: should be metaReadArb.io.out.fire(), but triggers Verilog backend bug
  val s1_writeback = s1_clk_en && !s1_valid && !s1_replay
  val s2_tag_match_way = RegEnable(s1_tag_match_way, s1_clk_en)
  val s2_tag_match = s2_tag_match_way.orR
  val s2_hit_state = Mux1H(s2_tag_match_way, wayMap((w: Int) => RegEnable(meta.io.resp(w).coh, s1_clk_en)))
  val s2_hit = s2_tag_match && 
                s2_hit_state.isHit(s2_req.cmd) && 
                s2_hit_state === s2_hit_state.onHit(s2_req.cmd)

  // load-reserved/store-conditional
  val lrsc_count = Reg(init=UInt(0))
  val lrsc_valid = lrsc_count.orR
  val lrsc_addr = Reg(UInt())
  val (s2_lr, s2_sc) = (s2_req.cmd === M_XLR, s2_req.cmd === M_XSC)
  val s2_lrsc_addr_match = lrsc_valid && lrsc_addr === (s2_req.addr >> blockOffBits)
  val s2_sc_fail = s2_sc && !s2_lrsc_addr_match
  when (lrsc_valid) { lrsc_count := lrsc_count - 1 }
  when (s2_valid_masked && s2_hit || s2_replay) {
    when (s2_lr) {
      when (!lrsc_valid) { lrsc_count := lrscCycles-1 }
      lrsc_addr := s2_req.addr >> blockOffBits
    }
    when (s2_sc) {
      lrsc_count := 0
    }
  }
  when (io.cpu.invalidate_lr) { lrsc_count := 0 }

  val s2_data = Wire(Vec(nWays, Bits(width=encRowBits)))
  for (w <- 0 until nWays) {
    val regs = Reg(Vec(rowWords, Bits(width = encDataBits)))
    val en1 = s1_clk_en && s1_tag_eq_way(w)
    for (i <- 0 until regs.size) {
      val en = en1 && ((Bool(i == 0) || !Bool(doNarrowRead)) || s1_writeback)
      when (en) { regs(i) := data.io.resp(w) >> encDataBits*i }
    }
    s2_data(w) := regs.toBits
  }
  val s2_data_muxed = Mux1H(s2_tag_match_way, s2_data)
  val s2_data_decoded = (0 until rowWords).map(i => code.decode(s2_data_muxed(encDataBits*(i+1)-1,encDataBits*i)))
  val s2_data_corrected = Vec(s2_data_decoded.map(_.corrected)).toBits
  val s2_data_uncorrected = Vec(s2_data_decoded.map(_.uncorrected)).toBits
  val s2_word_idx = if(doNarrowRead) UInt(0) else s2_req.addr(log2Up(rowWords*coreDataBytes)-1,3)
  val s2_data_correctable = Vec(s2_data_decoded.map(_.correctable)).toBits()(s2_word_idx)

  // store/amo hits
  s3_valid := (s2_valid_masked && s2_hit || s2_replay) && !s2_sc_fail && isWrite(s2_req.cmd)
  val amoalu = Module(new AMOALU)
  when ((s2_valid || s2_replay) && (isWrite(s2_req.cmd) || s2_data_correctable)) {
    s3_req := s2_req
    s3_req.data := Mux(s2_data_correctable, s2_data_corrected, amoalu.io.out)
    s3_way := s2_tag_match_way
  }

  writeArb.io.in(0).bits.addr := s3_req.addr
  val rowIdx = s3_req.addr(rowOffBits-1,offsetlsb).toUInt
  val rowWMask = UInt(1) << (if(rowOffBits > offsetlsb) rowIdx else UInt(0))
  writeArb.io.in(0).bits.wmask := rowWMask
  writeArb.io.in(0).bits.data := Fill(rowWords, s3_req.data)
  writeArb.io.in(0).valid := s3_valid
  writeArb.io.in(0).bits.way_en :=  s3_way

  // replacement policy
  val replacer = p(Replacer)()
  val s1_replaced_way_en = UIntToOH(replacer.way)
  val s2_replaced_way_en = UIntToOH(RegEnable(replacer.way, s1_clk_en))
  val s2_repl_meta = Mux1H(s2_replaced_way_en, wayMap((w: Int) => RegEnable(meta.io.resp(w), s1_clk_en && s1_replaced_way_en(w))).toSeq)

  // miss handling
  mshrs.io.req.valid := s2_valid_masked && !s2_hit && (isPrefetch(s2_req.cmd) || isRead(s2_req.cmd) || isWrite(s2_req.cmd))
  mshrs.io.req.bits := s2_req
  mshrs.io.req.bits.tag_match := s2_tag_match
  mshrs.io.req.bits.old_meta := Mux(s2_tag_match, L1Metadata(s2_repl_meta.tag, s2_hit_state), s2_repl_meta)
  mshrs.io.req.bits.way_en := Mux(s2_tag_match, s2_tag_match_way, s2_replaced_way_en)
  mshrs.io.req.bits.data := s2_req.data
  when (mshrs.io.req.fire()) { replacer.miss }
  io.mem.acquire <> mshrs.io.mem_req

  // replays
  readArb.io.in(1).valid := mshrs.io.replay.valid
  readArb.io.in(1).bits := mshrs.io.replay.bits
  readArb.io.in(1).bits.way_en := ~UInt(0, nWays)
  mshrs.io.replay.ready := readArb.io.in(1).ready
  s1_replay := mshrs.io.replay.valid && readArb.io.in(1).ready
  metaReadArb.io.in(1) <> mshrs.io.meta_read
  metaWriteArb.io.in(0) <> mshrs.io.meta_write

  // probes and releases
  val releaseArb = Module(new LockingArbiter(new Release, 2, outerDataBeats, (r: Release) => r.hasMultibeatData()))
  io.mem.release <> releaseArb.io.out

  prober.io.req.valid := io.mem.probe.valid && !lrsc_valid
  io.mem.probe.ready := prober.io.req.ready && !lrsc_valid
  prober.io.req.bits := io.mem.probe.bits
  releaseArb.io.in(1) <> prober.io.rep
  prober.io.way_en := s2_tag_match_way
  prober.io.block_state := s2_hit_state
  metaReadArb.io.in(2) <> prober.io.meta_read
  metaWriteArb.io.in(1) <> prober.io.meta_write
  prober.io.mshr_rdy := mshrs.io.probe_rdy

  // refills
  val narrow_grant = FlowThroughSerializer(io.mem.grant, refillCyclesPerBeat)
  mshrs.io.mem_grant.valid := narrow_grant.fire()
  mshrs.io.mem_grant.bits := narrow_grant.bits
  narrow_grant.ready := writeArb.io.in(1).ready || !narrow_grant.bits.hasData()
  /* The last clause here is necessary in order to prevent the responses for
   * the IOMSHRs from being written into the data array. It works because the
   * IOMSHR ids start right the ones for the regular MSHRs. */
  writeArb.io.in(1).valid := narrow_grant.valid && narrow_grant.bits.hasData() &&
                             narrow_grant.bits.client_xact_id < UInt(nMSHRs)
  writeArb.io.in(1).bits.addr := mshrs.io.refill.addr
  writeArb.io.in(1).bits.way_en := mshrs.io.refill.way_en
  writeArb.io.in(1).bits.wmask := ~UInt(0, rowWords)
  writeArb.io.in(1).bits.data := narrow_grant.bits.data(encRowBits-1,0)
  data.io.read <> readArb.io.out
  readArb.io.out.ready := !narrow_grant.valid || narrow_grant.ready // insert bubble if refill gets blocked

  // writebacks
  val wbArb = Module(new Arbiter(new WritebackReq, 2))
  wbArb.io.in(0) <> prober.io.wb_req
  wbArb.io.in(1) <> mshrs.io.wb_req
  wb.io.req <> wbArb.io.out
  metaReadArb.io.in(3) <> wb.io.meta_read
  readArb.io.in(2) <> wb.io.data_req
  wb.io.data_resp := s2_data_corrected
  releaseArb.io.in(0) <> wb.io.release

  // store->load bypassing
  val s4_valid = Reg(next=s3_valid, init=Bool(false))
  val s4_req = RegEnable(s3_req, s3_valid && metaReadArb.io.out.valid)
  val bypasses = List(
    ((s2_valid_masked || s2_replay) && !s2_sc_fail, s2_req, amoalu.io.out),
    (s3_valid, s3_req, s3_req.data),
    (s4_valid, s4_req, s4_req.data)
  ).map(r => (r._1 && (s1_addr >> wordOffBits === r._2.addr >> wordOffBits) && isWrite(r._2.cmd), r._3))
  val s2_store_bypass_data = Reg(Bits(width = coreDataBits))
  val s2_store_bypass = Reg(Bool())
  when (s1_clk_en) {
    s2_store_bypass := false
    when (bypasses.map(_._1).reduce(_||_)) {
      s2_store_bypass_data := PriorityMux(bypasses)
      s2_store_bypass := true
    }
  }

  // load data subword mux/sign extension
  val s2_data_word_prebypass = s2_data_uncorrected >> Cat(s2_word_idx, Bits(0,log2Up(coreDataBits)))
  val s2_data_word = Mux(s2_store_bypass, s2_store_bypass_data, s2_data_word_prebypass)
  val loadgen = new LoadGen(s2_req.typ, s2_req.addr, s2_data_word, s2_sc, 8)
  
  amoalu.io.addr := s2_req.addr
  amoalu.io.cmd := s2_req.cmd
  amoalu.io.typ := s2_req.typ
  amoalu.io.lhs := s2_data_word
  amoalu.io.rhs := s2_req.data

  // nack it like it's hot
  val s1_nack = dtlb.io.req.valid && dtlb.io.resp.miss ||
                s1_req.addr(idxMSB,idxLSB) === prober.io.meta_write.bits.idx && !prober.io.req.ready
  val s2_nack_hit = RegEnable(s1_nack, s1_valid || s1_replay)
  when (s2_nack_hit) { mshrs.io.req.valid := Bool(false) }
  val s2_nack_victim = s2_hit && mshrs.io.secondary_miss
  val s2_nack_miss = !s2_hit && !mshrs.io.req.ready
  val s2_nack = s2_nack_hit || s2_nack_victim || s2_nack_miss
  s2_valid_masked := s2_valid && !s2_nack

  val s2_recycle_ecc = (s2_valid || s2_replay) && s2_hit && s2_data_correctable
  val s2_recycle_next = Reg(init=Bool(false))
  when (s1_valid || s1_replay) { s2_recycle_next := s2_recycle_ecc }
  s2_recycle := s2_recycle_ecc || s2_recycle_next

  // after a nack, block until nack condition resolves to save energy
  val block_miss = Reg(init=Bool(false))
  block_miss := (s2_valid || block_miss) && s2_nack_miss
  when (block_miss) {
    io.cpu.req.ready := Bool(false)
  }

  val cache_resp = Wire(Valid(new HellaCacheResp))
  cache_resp.valid := (s2_replay || s2_valid_masked && s2_hit) && !s2_data_correctable
  cache_resp.bits := s2_req
  cache_resp.bits.has_data := isRead(s2_req.cmd)
  cache_resp.bits.data := loadgen.data | s2_sc_fail
  cache_resp.bits.store_data := s2_req.data
  cache_resp.bits.nack := s2_valid && s2_nack
  cache_resp.bits.replay := s2_replay

  val uncache_resp = Wire(Valid(new HellaCacheResp))
  uncache_resp.bits := mshrs.io.resp.bits
  uncache_resp.valid := mshrs.io.resp.valid

  val cache_pass = s2_valid || s2_killed || s2_replay
  mshrs.io.resp.ready := !cache_pass

  io.cpu.resp := Mux(cache_pass, cache_resp, uncache_resp)
  io.cpu.resp.bits.data_word_bypass := loadgen.wordData
  io.cpu.ordered := mshrs.io.fence_rdy && !s1_valid && !s2_valid
  io.cpu.replay_next.valid := s1_replay && s1_read
  io.cpu.replay_next.bits := s1_req.tag
}

// exposes a sane decoupled request interface
class SimpleHellaCacheIF(implicit p: Parameters) extends Module
{
  val io = new Bundle {
    val requestor = new HellaCacheIO().flip
    val cache = new HellaCacheIO
  }

  val replaying_cmb = Wire(Bool())
  val replaying = Reg(next = replaying_cmb, init = Bool(false))
  replaying_cmb := replaying

  val replayq1 = Module(new Queue(new HellaCacheReq, 1, flow = true))
  val replayq2 = Module(new Queue(new HellaCacheReq, 1))
  val req_arb = Module(new Arbiter(new HellaCacheReq, 2))

  req_arb.io.in(0) <> replayq1.io.deq
  req_arb.io.in(1).valid := !replaying_cmb && io.requestor.req.valid
  req_arb.io.in(1).bits := io.requestor.req.bits
  io.requestor.req.ready := !replaying_cmb && req_arb.io.in(1).ready

  val s2_nack = io.cache.resp.bits.nack
  val s3_nack = Reg(next=s2_nack)

  val s0_req_fire = io.cache.req.fire()
  val s1_req_fire = Reg(next=s0_req_fire)
  val s2_req_fire = Reg(next=s1_req_fire)

  io.cache.req <> req_arb.io.out
  io.cache.req.bits.kill := s2_nack
  io.cache.req.bits.phys := Bool(true)
  io.cache.req.bits.data := RegEnable(req_arb.io.out.bits.data, s0_req_fire)

/* replay queues:
     replayq1 holds the older request.
     replayq2 holds the newer request (for the first nack).
     We need to split the queues like this for the case where the older request
     goes through but gets nacked, while the newer request stalls.
     If this happens, the newer request will go through before the older one.
     We don't need to check replayq1.io.enq.ready and replayq2.io.enq.ready as
     there will only be two requests going through at most.
*/

  // stash d$ request in stage 2 if nacked (older request)
  replayq1.io.enq.valid := Bool(false)
  replayq1.io.enq.bits.cmd := io.cache.resp.bits.cmd
  replayq1.io.enq.bits.typ := io.cache.resp.bits.typ
  replayq1.io.enq.bits.addr := io.cache.resp.bits.addr
  replayq1.io.enq.bits.data := io.cache.resp.bits.store_data
  replayq1.io.enq.bits.tag := io.cache.resp.bits.tag

  // stash d$ request in stage 1 if nacked (newer request)
  replayq2.io.enq.valid := s2_req_fire && s3_nack
  replayq2.io.enq.bits <> io.cache.resp.bits
  replayq2.io.enq.bits.data := io.cache.resp.bits.store_data
  replayq2.io.deq.ready := Bool(false)

  when (s2_nack) {
    replayq1.io.enq.valid := Bool(true)
    replaying_cmb := Bool(true)
  }

  // when replaying request got sunk into the d$
  when (s2_req_fire && Reg(next=Reg(next=replaying_cmb)) && !s2_nack) {
    // see if there's a stashed request in replayq2
    when (replayq2.io.deq.valid) {
      replayq1.io.enq.valid := Bool(true)
      replayq1.io.enq.bits.cmd := replayq2.io.deq.bits.cmd
      replayq1.io.enq.bits.typ := replayq2.io.deq.bits.typ
      replayq1.io.enq.bits.addr := replayq2.io.deq.bits.addr
      replayq1.io.enq.bits.data := replayq2.io.deq.bits.data
      replayq1.io.enq.bits.tag := replayq2.io.deq.bits.tag
      replayq2.io.deq.ready := Bool(true)
    } .otherwise {
      replaying_cmb := Bool(false)
    }
  }

  io.requestor.resp := io.cache.resp
}
