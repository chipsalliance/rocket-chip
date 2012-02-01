package Top {

import Chisel._
import Node._;
import Constants._;
import scala.math._;

class ioReplacementWayGen extends Bundle {
  val pick_new_way = Bool(dir = INPUT)
  val way_en = Bits(width = NWAYS, dir = INPUT)
  val way_id = UFix(width = log2up(NWAYS), dir = OUTPUT)
}

class RandomReplacementWayGen extends Component {
  val io = new ioReplacementWayGen()
  //TODO: Actually limit selection based on which ways are allowed (io.ways_en)
  if(NWAYS > 1) io.way_id := LFSR16(io.pick_new_way)
  else io.way_id := UFix(0)
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

class RPQEntry extends Bundle {
  val offset = Bits(width = OFFSET_BITS)
  val cmd    = Bits(width = 4)
  val typ    = Bits(width = 3)
  val sdq_id = UFix(width = log2up(NSDQ))
  val tag    = Bits(width = DCACHE_TAG_BITS)
}

class Replay extends Bundle {
  val idx    = Bits(width = IDX_BITS)
  val offset = Bits(width = OFFSET_BITS)
  val cmd    = Bits(width = 4)
  val typ    = Bits(width = 3)
  val sdq_id = UFix(width = log2up(NSDQ))
  val tag    = Bits(width = DCACHE_TAG_BITS)
  val way_oh = Bits(width = NWAYS)
}

class DataReq extends Bundle {
  val idx    = Bits(width = IDX_BITS)
  val offset = Bits(width = OFFSET_BITS)
  val cmd    = Bits(width = 4)
  val typ    = Bits(width = 3)
  val data = Bits(width = CPU_DATA_BITS)
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

class MemReq extends Bundle {
  val rw   = Bool()
  val addr = UFix(width = PPN_BITS+IDX_BITS)
  val tag  = Bits(width = DMEM_TAG_BITS)
}

class WritebackReq extends Bundle {
  val ppn = Bits(width = PPN_BITS)
  val idx = Bits(width = IDX_BITS)
  val way_oh = Bits(width = NWAYS)
}

class MetaData extends Bundle {
  val valid = Bool()
  val dirty = Bool()
  val tag = Bits(width = PPN_BITS)
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

class MSHR(id: Int) extends Component {
  val io = new Bundle {
    val req_pri_val    = Bool(INPUT)
    val req_pri_rdy    = Bool(OUTPUT)
    val req_sec_val    = Bool(INPUT)
    val req_sec_rdy    = Bool(OUTPUT)
    val req_ppn        = Bits(PPN_BITS, INPUT)
    val req_idx        = Bits(IDX_BITS, INPUT)
    val req_offset     = Bits(OFFSET_BITS, INPUT)
    val req_cmd        = Bits(4, INPUT)
    val req_type       = Bits(3, INPUT)
    val req_sdq_id     = UFix(log2up(NSDQ), INPUT)
    val req_tag        = Bits(DCACHE_TAG_BITS, INPUT)
    val req_way_oh     = Bits(NWAYS, INPUT)

    val idx_match      = Bool(OUTPUT)
    val idx            = Bits(IDX_BITS, OUTPUT)
    val tag            = Bits(PPN_BITS, OUTPUT)
    val way_oh         = Bits(NWAYS, OUTPUT)

    val mem_resp_val = Bool(INPUT)
    val mem_req  = (new ioDecoupled) { new MemReq()  }.flip
    val meta_req = (new ioDecoupled) { new MetaArrayArrayReq() }.flip
    val replay   = (new ioDecoupled) { new Replay()   }.flip
  }

  val valid = Reg(resetVal = Bool(false))
  val dirty = Reg { Bool() }
  val requested = Reg { Bool() }
  val refilled = Reg { Bool() }
  val ppn = Reg { Bits() }
  val idx_ = Reg { Bits() }
  val way_oh_ = Reg { Bits() }

  val req_load = (io.req_cmd === M_XRD) || (io.req_cmd === M_PFR)
  val req_use_rpq = (io.req_cmd != M_PFR) && (io.req_cmd != M_PFW)
  val next_dirty = dirty || io.req_sec_val && io.req_sec_rdy && !req_load
  val sec_rdy = io.idx_match && !refilled && (dirty || !requested || req_load)

  val rpq = (new queueSimplePF(NRPQ)) { new RPQEntry() }
  rpq.io.q_reset := Bool(false)
  rpq.io.enq.valid := (io.req_pri_val && io.req_pri_rdy || io.req_sec_val && sec_rdy) && req_use_rpq
  rpq.io.enq.bits.offset := io.req_offset
  rpq.io.enq.bits.cmd := io.req_cmd
  rpq.io.enq.bits.typ := io.req_type
  rpq.io.enq.bits.sdq_id := io.req_sdq_id
  rpq.io.enq.bits.tag := io.req_tag
  rpq.io.deq.ready := io.replay.ready && refilled

  when (io.req_pri_val && io.req_pri_rdy) {
    valid <== Bool(true)
    dirty <== !req_load
    requested <== Bool(false)
    refilled <== Bool(false)
    ppn <== io.req_ppn
    idx_ <== io.req_idx
    way_oh_ <== io.req_way_oh
  }
  when (io.mem_req.valid && io.mem_req.ready) {
    requested <== Bool(true)
  }
  when (io.mem_resp_val) {
    refilled <== Bool(true)
  }
  when (io.meta_req.valid && io.meta_req.ready) {
    valid <== Bool(false)
  }
  otherwise {
    dirty <== next_dirty
  }

  io.idx_match := valid && (idx_ === io.req_idx)
  io.idx := idx_
  io.tag := ppn
  io.way_oh := way_oh_
  io.req_pri_rdy := !valid
  io.req_sec_rdy := sec_rdy && rpq.io.enq.ready

  io.meta_req.valid := valid && refilled && !rpq.io.deq.valid
  io.meta_req.bits.inner_req.rw := Bool(true)
  io.meta_req.bits.inner_req.idx := idx_
  io.meta_req.bits.inner_req.data.valid := Bool(true)
  io.meta_req.bits.inner_req.data.dirty := dirty
  io.meta_req.bits.inner_req.data.tag := ppn
  io.meta_req.bits.way_en := way_oh_

  io.mem_req.valid := valid && !requested
  //io.mem_req.bits.itm := next_dirty
  io.mem_req.bits.rw := Bool(false)
  io.mem_req.bits.addr := Cat(ppn, idx_).toUFix
  io.mem_req.bits.tag := Bits(id)

  io.replay.valid := rpq.io.deq.valid && refilled
  io.replay.bits.idx := idx_
  io.replay.bits.tag := rpq.io.deq.bits.tag
  io.replay.bits.offset := rpq.io.deq.bits.offset
  io.replay.bits.cmd := rpq.io.deq.bits.cmd
  io.replay.bits.typ := rpq.io.deq.bits.typ
  io.replay.bits.sdq_id := rpq.io.deq.bits.sdq_id
  io.replay.bits.way_oh := way_oh_
}

class MSHRFile extends Component {
  val io = new Bundle {
    val req_val    = Bool(INPUT)
    val req_rdy    = Bool(OUTPUT)
    val req_ppn    = Bits(PPN_BITS, INPUT)
    val req_idx    = Bits(IDX_BITS, INPUT)
    val req_offset = Bits(OFFSET_BITS, INPUT)
    val req_cmd    = Bits(4, INPUT)
    val req_type   = Bits(3, INPUT)
    val req_tag    = Bits(DCACHE_TAG_BITS, INPUT)
    val req_sdq_id = UFix(log2up(NSDQ), INPUT)
    val req_way_oh = Bits(NWAYS, INPUT)

    val mem_resp_val = Bool(INPUT)
    val mem_resp_tag = Bits(DMEM_TAG_BITS, INPUT)
    val mem_resp_idx = Bits(IDX_BITS, OUTPUT)
    val mem_resp_way_oh = Bits(NWAYS, OUTPUT)

    val fence_rdy = Bool(OUTPUT)

    val mem_req  = (new ioDecoupled) { new MemReq()  }.flip()
    val meta_req = (new ioDecoupled) { new MetaArrayArrayReq() }.flip()
    val replay   = (new ioDecoupled) { new Replay()   }.flip()
  }

  val tag_mux = (new Mux1H(NMSHR)){ Bits(width = PPN_BITS) }
  val mem_resp_idx_mux = (new Mux1H(NMSHR)){ Bits(width = IDX_BITS) }
  val mem_resp_way_oh_mux = (new Mux1H(NMSHR)){ Bits(width =  NWAYS) }
  val meta_req_arb = (new Arbiter(NMSHR)) { new MetaArrayArrayReq() }
  val mem_req_arb = (new Arbiter(NMSHR)) { new MemReq() }
  val replay_arb = (new Arbiter(NMSHR)) { new Replay() }

  val alloc_arb = (new Arbiter(NMSHR)) { Bool() }

  val tag_match = tag_mux.io.out === io.req_ppn

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

    mshr.io.req_sec_val := io.req_val && tag_match
    mshr.io.req_ppn := io.req_ppn
    mshr.io.req_tag := io.req_tag
    mshr.io.req_idx := io.req_idx
    mshr.io.req_offset := io.req_offset
    mshr.io.req_cmd := io.req_cmd
    mshr.io.req_type := io.req_type
    mshr.io.req_sdq_id := io.req_sdq_id
    mshr.io.req_way_oh := io.req_way_oh

    mshr.io.meta_req <> meta_req_arb.io.in(i)
    mshr.io.mem_req <> mem_req_arb.io.in(i)
    mshr.io.replay <> replay_arb.io.in(i)

    val mem_resp_val = io.mem_resp_val && (UFix(i) === io.mem_resp_tag)
    mshr.io.mem_resp_val := mem_resp_val
    mem_resp_idx_mux.io.sel(i) := (UFix(i) === io.mem_resp_tag)
    mem_resp_idx_mux.io.in(i) := mshr.io.idx
    mem_resp_way_oh_mux.io.sel(i) := (UFix(i) === io.mem_resp_tag)
    mem_resp_way_oh_mux.io.in(i) := mshr.io.way_oh

    pri_rdy = pri_rdy || mshr.io.req_pri_rdy
    sec_rdy = sec_rdy || mshr.io.req_sec_rdy
    fence = fence || !mshr.io.req_pri_rdy
    idx_match = idx_match || mshr.io.idx_match
  }

  alloc_arb.io.out.ready := io.req_val && !idx_match

  meta_req_arb.io.out <> io.meta_req
  mem_req_arb.io.out <> io.mem_req
  replay_arb.io.out <> io.replay

  io.req_rdy := Mux(idx_match, tag_match && sec_rdy, pri_rdy)
  io.mem_resp_idx := mem_resp_idx_mux.io.out
  io.mem_resp_way_oh := mem_resp_way_oh_mux.io.out
  io.fence_rdy := !fence
}

class ReplayUnit extends Component {
  val io = new Bundle {
    val sdq_enq    = (new ioDecoupled) { Bits(width = CPU_DATA_BITS) }
    val sdq_id     = UFix(log2up(NSDQ), OUTPUT)
    val way_oh     = Bits(NWAYS, OUTPUT)
    val replay     = (new ioDecoupled) { new Replay() }
    val data_req   = (new ioDecoupled) { new DataReq() }.flip()
    val cpu_resp_val = Bool(OUTPUT)
    val cpu_resp_tag = Bits(DCACHE_TAG_BITS, OUTPUT)
  }

  val sdq_val = Reg(resetVal = UFix(0, NSDQ))
  val sdq_allocator = new priorityEncoder(NSDQ)
  sdq_allocator.io.in := ~sdq_val
  val sdq_alloc_id = sdq_allocator.io.out.toUFix

  val replay_val = Reg(resetVal = Bool(false))
  val replay_retry = replay_val && !io.data_req.ready
  replay_val <== io.replay.valid || replay_retry

  val rp = Reg { new Replay() }
  when (io.replay.valid && io.replay.ready) { rp <== io.replay.bits }

  val rp_amo = rp.cmd(3).toBool
  val rp_store = (rp.cmd === M_XWR)
  val rp_load = (rp.cmd === M_XRD)
  val rp_write = rp_store || rp_amo
  val rp_read = rp_load || rp_amo

  val sdq_ren_new = io.replay.valid && (io.replay.bits.cmd != M_XRD)
  val sdq_ren_retry = replay_retry && rp_write
  val sdq_ren = sdq_ren_new || sdq_ren_retry
  val sdq_wen = io.sdq_enq.valid && io.sdq_enq.ready
  val sdq_addr = Mux(sdq_ren_retry, rp.sdq_id, Mux(sdq_ren_new, io.replay.bits.sdq_id, sdq_alloc_id))

  val sdq = Mem4(NSDQ, io.sdq_enq.bits)
  sdq.setReadLatency(1);
  sdq.setTarget('inst)
  val sdq_dout = sdq.rw(sdq_addr, io.sdq_enq.bits, sdq_wen, cs = sdq_ren || sdq_wen)

  val sdq_free = replay_val && !replay_retry && rp_write
  sdq_val <== sdq_val & ~(sdq_free.toUFix << rp.sdq_id) | (sdq_wen.toUFix << sdq_alloc_id)

  io.sdq_enq.ready := (~sdq_val != UFix(0)) && !sdq_ren
  io.sdq_id := sdq_alloc_id

  io.replay.ready := !replay_retry

  io.data_req.valid := replay_val
  io.way_oh := rp.way_oh
  io.data_req.bits.idx := rp.idx
  io.data_req.bits.offset := rp.offset
  io.data_req.bits.cmd := rp.cmd
  io.data_req.bits.typ := rp.typ
  io.data_req.bits.data := sdq_dout

  io.cpu_resp_val := Reg(replay_val && !replay_retry && rp_read, resetVal = Bool(false))
  io.cpu_resp_tag := Reg(rp.tag)
}

class WritebackUnit extends Component {
  val io = new Bundle {
    val req    = (new ioDecoupled) { new WritebackReq() }
    val data_req  = (new ioDecoupled) { new DataArrayArrayReq() }.flip()
    val data_resp = Bits(MEM_DATA_BITS, INPUT)
    val refill_req = (new ioDecoupled) { new MemReq() }
    val mem_req   = (new ioDecoupled) { new MemReq() }.flip()
    val mem_req_data = Bits(MEM_DATA_BITS, OUTPUT)
  }

  val wbq = (new queueSimplePF(REFILL_CYCLES)) { Bits(width = MEM_DATA_BITS) }
  val valid = Reg(resetVal = Bool(false))
  val cnt = Reg() { UFix(width = log2up(REFILL_CYCLES+1)) }
  val addr = Reg() { new WritebackReq() }

  // don't allow memory requests to bypass conflicting writebacks.
  // also don't allow a refill request once a writeback has started.
  // TODO: turn this into a victim buffer.
  val block_refill = valid && ((io.refill_req.bits.addr(IDX_BITS-1,0) === addr.idx) || (cnt === UFix(REFILL_CYCLES)))
  val refill_val = io.refill_req.valid && !block_refill

  wbq.io.q_reset := Bool(false)
  wbq.io.enq.valid := valid && Reg(io.data_req.valid && io.data_req.ready)
  wbq.io.enq.bits := io.data_resp
  wbq.io.deq.ready := io.mem_req.ready && !refill_val && (cnt === UFix(REFILL_CYCLES))

  when (io.req.valid && io.req.ready) { valid <== Bool(true); cnt <== UFix(0); addr <== io.req.bits }
  when (io.data_req.valid && io.data_req.ready) { cnt <== cnt + UFix(1) }
  when ((cnt === UFix(REFILL_CYCLES)) && !wbq.io.deq.valid) { valid <== Bool(false) }

  io.req.ready := !valid
  io.data_req.valid := valid && (cnt < UFix(REFILL_CYCLES))
  io.data_req.bits.way_en := addr.way_oh
  io.data_req.bits.inner_req.idx := addr.idx
  io.data_req.bits.inner_req.offset := cnt
  io.data_req.bits.inner_req.rw := Bool(false)
  io.data_req.bits.inner_req.wmask := Bits(0)
  io.data_req.bits.inner_req.data := Bits(0)

  io.refill_req.ready := io.mem_req.ready && !block_refill
  io.mem_req.valid := refill_val || wbq.io.deq.valid && (cnt === UFix(REFILL_CYCLES))
  io.mem_req.bits.rw := !refill_val
  io.mem_req.bits.addr := Mux(refill_val, io.refill_req.bits.addr, Cat(addr.ppn, addr.idx).toUFix)
  io.mem_req.bits.tag := io.refill_req.bits.tag
  io.mem_req_data := wbq.io.deq.bits
}

class FlushUnit(lines: Int) extends Component {
  val io = new Bundle {
    val req  = (new ioDecoupled) { Bits(width = DCACHE_TAG_BITS) }
    val resp = (new ioDecoupled) { Bits(width = DCACHE_TAG_BITS) }.flip()
    val meta_req   = (new ioDecoupled) { new MetaArrayArrayReq() }.flip()
    val meta_resp  = (new MetaData).asInput()
    val wb_req = (new ioDecoupled) { new WritebackReq() }.flip()
  }
  
  val s_reset :: s_ready :: s_meta_read :: s_meta_wait :: s_meta_write :: s_done :: Nil = Enum(6) { UFix() }
  val state = Reg(resetVal = s_reset)
  val tag = Reg() { Bits() }
  val idx_cnt = Reg(resetVal = UFix(0, log2up(lines)))
  val next_idx_cnt = idx_cnt + UFix(1)
  val way_cnt = Reg(resetVal = UFix(0, log2up(NWAYS)))
  val next_way_cnt = way_cnt + UFix(1)

  switch (state) {
    is(s_reset) { 
      when (io.meta_req.ready) { 
        state <== Mux(~way_cnt === UFix(0) && ~idx_cnt === UFix(0), s_ready, s_reset); 
        when (~way_cnt === UFix(0)) { idx_cnt <== next_idx_cnt };
        way_cnt <== next_way_cnt;
      } 
    }
    is(s_ready) { when (io.req.valid) { state <== s_meta_read; tag <== io.req.bits } }
    is(s_meta_read) { when (io.meta_req.ready) { state <== s_meta_wait } }
    is(s_meta_wait) { state <== Mux(io.meta_resp.valid && io.meta_resp.dirty && !io.wb_req.ready, s_meta_read, s_meta_write) }
    is(s_meta_write) {
      when (io.meta_req.ready) { 
        state <== Mux(~way_cnt === UFix(0) && ~idx_cnt === UFix(0), s_done, s_meta_read); 
        when (~way_cnt === UFix(0)) { idx_cnt <== next_idx_cnt };
        way_cnt <== next_way_cnt;
      }
    }
    is(s_done) { when (io.resp.ready) { state <== s_ready } }
  }

  io.req.ready := state === s_ready
  io.resp.valid := state === s_done
  io.resp.bits := tag
  io.meta_req.valid := (state === s_meta_read) || (state === s_meta_write) || (state === s_reset)
  io.meta_req.bits.way_en := UFixToOH(way_cnt, NWAYS)
  io.meta_req.bits.inner_req.idx := idx_cnt
  io.meta_req.bits.inner_req.rw := (state === s_meta_write) || (state === s_reset)
  io.meta_req.bits.inner_req.data.valid := Bool(false)
  io.meta_req.bits.inner_req.data.dirty := Bool(false)
  io.meta_req.bits.inner_req.data.tag := UFix(0)
  io.wb_req.valid := state === s_meta_wait
  io.wb_req.bits.ppn := io.meta_resp.tag
  io.wb_req.bits.idx := idx_cnt
  io.wb_req.bits.way_oh := UFixToOH(way_cnt, NWAYS)
}

class MetaDataArray(lines: Int) extends Component {
  val io = new Bundle {
    val req  = (new ioDecoupled) { new MetaArrayReq() }
    val resp = (new MetaData).asOutput()
    val state_req = (new ioDecoupled) { new MetaArrayReq() }
  }

  val vd_array = Mem4(lines, Bits(width = 2))
  vd_array.setReadLatency(1);
  val vd_wdata2 = Cat(io.state_req.bits.data.valid, io.state_req.bits.data.dirty)
  vd_array.write(io.state_req.bits.idx, vd_wdata2, io.state_req.valid && io.state_req.bits.rw)
  val vd_wdata1 = Cat(io.req.bits.data.valid, io.req.bits.data.dirty)
  val vd_rdata1 = vd_array.rw(io.req.bits.idx, vd_wdata1, io.req.valid && io.req.bits.rw)

  // don't allow reading and writing of vd_array in same cycle.
  // this could be eliminated if the read port were combinational.
  val vd_conflict = io.state_req.valid && (io.req.bits.idx === io.state_req.bits.idx)

  val tag_array = Mem4(lines, io.resp.tag)
  tag_array.setReadLatency(1);
  tag_array.setTarget('inst)
  val tag_rdata = tag_array.rw(io.req.bits.idx, io.req.bits.data.tag, io.req.valid && io.req.bits.rw, cs = io.req.valid)

  io.resp.valid := vd_rdata1(1).toBool
  io.resp.dirty := vd_rdata1(0).toBool
  io.resp.tag   := tag_rdata
  io.req.ready  := !vd_conflict
}

class MetaDataArrayArray(lines: Int) extends Component {
  val io = new Bundle {
    val req  = (new ioDecoupled) { new MetaArrayArrayReq() }
    val resp = Vec(NWAYS){ (new MetaData).asOutput }
    val state_req = (new ioDecoupled) { new MetaArrayArrayReq() }
    val way_en = Bits(width = NWAYS, dir = OUTPUT)
  }

  val way_en_ = Reg { Bits(width=NWAYS) }
  when (io.req.valid && io.req.ready) {
    way_en_ <== io.req.bits.way_en
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
    val req  = (new ioDecoupled) { new DataArrayReq() }
    val resp = Bits(width = MEM_DATA_BITS, dir = OUTPUT)
  }

  val wmask = FillInterleaved(8, io.req.bits.wmask)

  val array = Mem4(lines*REFILL_CYCLES, io.resp)
  array.setReadLatency(1);
  array.setTarget('inst)
  val addr = Cat(io.req.bits.idx, io.req.bits.offset)
  val rdata = array.rw(addr, io.req.bits.data, io.req.valid && io.req.bits.rw, wmask, cs = io.req.valid)
  io.resp := rdata
  io.req.ready := Bool(true)
}

class DataArrayArray(lines: Int) extends Component {
  val io = new Bundle {
    val req  = (new ioDecoupled) { new DataArrayArrayReq() }
    val resp = Vec(NWAYS){ Bits(width = MEM_DATA_BITS, dir = OUTPUT) }
    val way_en = Bits(width = NWAYS, dir = OUTPUT)
  }

  val way_en_ = Reg { Bits(width=NWAYS) }
  when (io.req.valid && io.req.ready) {
    way_en_ <== io.req.bits.way_en
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

  io.out := Mux(io.cmd === M_XA_ADD,  adder_out,
            Mux(io.cmd === M_XA_SWAP, io.rhs,
            Mux(io.cmd === M_XA_AND,  io.lhs & io.rhs,
            Mux(io.cmd === M_XA_OR,   io.lhs | io.rhs,
                /* MIN[U]/MAX[U] */   cmp_out))));
}

//class HellaCache(lines: Int, ways: Int) extends Component {
//
//}

class HellaCacheDM(lines: Int) extends Component {
  val io = new ioDCacheHella()
  
  val addrbits    = PADDR_BITS
  val indexbits   = log2up(lines)
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
  val r_cpu_req_data   = Reg() { Bits() }

  val p_store_valid    = Reg(resetVal = Bool(false))
  val p_store_data     = Reg() { Bits() }
  val p_store_idx      = Reg() { Bits() }
  val p_store_cmd      = Reg() { Bits() }
  val p_store_type     = Reg() { Bits() }
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
  val r_req_amo   = r_cpu_req_cmd(3).toBool
  val r_req_read  = r_req_load || r_req_amo
  val r_req_write = r_req_store || r_req_amo
  val r_req_readwrite = r_req_read || r_req_write

  // replay unit
  val replayer = new ReplayUnit()
  val replay_amo_val = replayer.io.data_req.valid && replayer.io.data_req.bits.cmd(3).toBool
  
  when (replay_amo_val) {
    r_cpu_req_idx  <== Cat(replayer.io.data_req.bits.idx, replayer.io.data_req.bits.offset)
    r_cpu_req_cmd  <== replayer.io.data_req.bits.cmd
    r_cpu_req_type <== replayer.io.data_req.bits.typ
    r_cpu_req_data <== replayer.io.data_req.bits.data
  }
  when (io.cpu.req_val) {
    r_cpu_req_idx  <== io.cpu.req_idx
    r_cpu_req_cmd  <== io.cpu.req_cmd
    r_cpu_req_type <== io.cpu.req_type
    r_cpu_req_tag  <== io.cpu.req_tag
    when (req_write) {
      r_cpu_req_data <== io.cpu.req_data
    }
  }

  // refill counter
  val rr_count = Reg(resetVal = UFix(0, log2up(REFILL_CYCLES)))
  val rr_count_next = rr_count + UFix(1)
  when (io.mem.resp_val) { rr_count <== rr_count_next }

  val misaligned =
    (((r_cpu_req_type === MT_H) || (r_cpu_req_type === MT_HU)) && (r_cpu_req_idx(0) != Bits(0))) ||
    (((r_cpu_req_type === MT_W) || (r_cpu_req_type === MT_WU)) && (r_cpu_req_idx(1,0) != Bits(0))) ||
    ((r_cpu_req_type === MT_D) && (r_cpu_req_idx(2,0) != Bits(0)));
    
  io.cpu.xcpt_ma_ld := r_cpu_req_val_ && r_req_read && misaligned
  io.cpu.xcpt_ma_st := r_cpu_req_val_ && r_req_write && misaligned

  // tags
  val meta = new MetaDataArray(lines)
  val meta_arb = (new Arbiter(3)) { new MetaArrayReq() }
  meta_arb.io.out <> meta.io.req

  // data
  val data = new DataArray(lines)
  val data_arb = (new Arbiter(5)) { new DataArrayReq() }
  data_arb.io.out <> data.io.req

  // writeback unit
  val wb = new WritebackUnit
  val wb_arb = (new Arbiter(2)) { new WritebackReq() }
  wb_arb.io.out <> wb.io.req
  wb.io.data_req.bits.inner_req <> data_arb.io.in(3).bits 
  wb.io.data_req.ready := data_arb.io.in(3).ready
  data_arb.io.in(3).valid := wb.io.data_req.valid
  wb.io.data_resp <> data.io.resp

  // cpu tag check
  meta_arb.io.in(2).valid := io.cpu.req_val
  meta_arb.io.in(2).bits.idx := io.cpu.req_idx(indexmsb,indexlsb)
  meta_arb.io.in(2).bits.rw := Bool(false)
  meta_arb.io.in(2).bits.data.valid := Bool(false) // don't care
  meta_arb.io.in(2).bits.data.dirty := Bool(false) // don't care
  meta_arb.io.in(2).bits.data.tag := UFix(0)       // don't care
  val early_tag_nack = !meta_arb.io.in(2).ready
  val tag_match = meta.io.resp.valid && (meta.io.resp.tag === io.cpu.req_ppn)
  val tag_hit  = r_cpu_req_val &&  tag_match
  val tag_miss = r_cpu_req_val && !tag_match
  val dirty = meta.io.resp.valid && meta.io.resp.dirty

  // refill response
  val block_during_refill = !io.mem.resp_val && (rr_count != UFix(0))
  data_arb.io.in(0).valid := io.mem.resp_val || block_during_refill
  data_arb.io.in(0).bits.offset := rr_count
  data_arb.io.in(0).bits.rw := !block_during_refill
  data_arb.io.in(0).bits.wmask := ~UFix(0, MEM_DATA_BITS/8)
  data_arb.io.in(0).bits.data := io.mem.resp_data

  // load hits
  data_arb.io.in(4).bits.offset := io.cpu.req_idx(offsetmsb,ramindexlsb)
  data_arb.io.in(4).bits.idx := io.cpu.req_idx(indexmsb,indexlsb)
  data_arb.io.in(4).bits.rw := Bool(false)
  data_arb.io.in(4).bits.wmask := UFix(0) // don't care
  data_arb.io.in(4).bits.data := io.mem.resp_data // don't care
  data_arb.io.in(4).valid := io.cpu.req_val && req_read
  val early_load_nack = req_read && !data_arb.io.in(4).ready

  // store hits and AMO hits and misses use a pending store register.
  // we nack new stores if a pending store can't retire for some reason.
  // we drain a pending store if the CPU performs a store or a
  // conflictig load, or if the cache is idle, or after a miss.
  val p_store_idx_match = p_store_valid && (r_cpu_req_idx(indexmsb,indexlsb) === p_store_idx(indexmsb,indexlsb))
  val p_store_offset_match = (r_cpu_req_idx(indexlsb-1,offsetlsb) === p_store_idx(indexlsb-1,offsetlsb))
  val p_store_match = r_cpu_req_val && r_req_read && p_store_idx_match && p_store_offset_match
  val drain_store_val = (p_store_valid && (!io.cpu.req_val || !req_read || Reg(tag_miss))) || p_store_match
  data_arb.io.in(2).bits.offset := p_store_idx(offsetmsb,ramindexlsb)
  data_arb.io.in(2).bits.idx := p_store_idx(indexmsb,indexlsb)
  data_arb.io.in(2).bits.rw := Bool(true)
  data_arb.io.in(2).valid := drain_store_val
  val drain_store = drain_store_val && data_arb.io.in(2).ready
  val p_store_rdy = !p_store_valid || drain_store
  val p_amo = Reg(tag_hit && r_req_amo && p_store_rdy && !p_store_match || r_replay_amo, resetVal = Bool(false))
  p_store_valid <== !p_store_rdy || (tag_hit && r_req_store) || p_amo

  // writeback
  val wb_rdy = wb_arb.io.in(1).ready && !p_store_idx_match
  wb_arb.io.in(1).valid := tag_miss && r_req_readwrite && dirty && !p_store_idx_match
  wb_arb.io.in(1).bits.ppn := meta.io.resp.tag
  wb_arb.io.in(1).bits.idx := r_cpu_req_idx(indexmsb,indexlsb)

  // tag update after a miss or a store to an exclusive clean line.
  val clear_valid = tag_miss && r_req_readwrite && meta.io.resp.valid && (!dirty || wb_rdy)
  val set_dirty   = tag_hit && !meta.io.resp.dirty && r_req_write
  meta.io.state_req.valid := clear_valid || set_dirty
  meta.io.state_req.bits.rw := Bool(true)
  meta.io.state_req.bits.idx := r_cpu_req_idx(indexmsb,indexlsb)
  meta.io.state_req.bits.data.tag := UFix(0) // don't care
  meta.io.state_req.bits.data.valid := tag_match
  meta.io.state_req.bits.data.dirty := tag_match
  
  // pending store data, also used for AMO RHS
  val storegen = new StoreDataGen
  val amoalu = new AMOALU
  storegen.io.typ := r_cpu_req_type
  storegen.io.din  := r_cpu_req_data
  when (p_amo) {
    p_store_data <== amoalu.io.out
  }
  when (tag_hit && r_req_write && p_store_rdy || r_replay_amo) {
    p_store_idx   <== r_cpu_req_idx
    p_store_type  <== r_cpu_req_type
    p_store_cmd   <== r_cpu_req_cmd
    p_store_data  <== storegen.io.dout
  }

  // miss handling
  val mshr = new MSHRFile()
  mshr.io.req_val := tag_miss && r_req_readwrite && (!dirty || wb_rdy) && (!r_req_write || replayer.io.sdq_enq.ready)
  mshr.io.req_ppn := io.cpu.req_ppn
  mshr.io.req_idx := r_cpu_req_idx(indexmsb,indexlsb)
  mshr.io.req_tag := r_cpu_req_tag
  mshr.io.req_offset := r_cpu_req_idx(offsetmsb,0)
  mshr.io.req_cmd := r_cpu_req_cmd
  mshr.io.req_type := r_cpu_req_type
  mshr.io.req_sdq_id := replayer.io.sdq_id
  mshr.io.mem_resp_val := io.mem.resp_val && (~rr_count === UFix(0))
  mshr.io.mem_resp_tag := io.mem.resp_tag
  mshr.io.mem_req <> wb.io.refill_req
  mshr.io.meta_req.bits.inner_req <> meta_arb.io.in(1).bits 
  mshr.io.meta_req.ready := meta_arb.io.in(1).ready
  meta_arb.io.in(1).valid := mshr.io.meta_req.valid
  mshr.io.replay <> replayer.io.replay
  replayer.io.sdq_enq.valid := tag_miss && r_req_write && (!dirty || wb_rdy) && mshr.io.req_rdy
  replayer.io.sdq_enq.bits := storegen.io.dout
  data_arb.io.in(0).bits.idx := mshr.io.mem_resp_idx

  // replays
  val replay = replayer.io.data_req.bits
  val stall_replay = r_replay_amo || p_amo || p_store_valid
  val replay_val = replayer.io.data_req.valid && !stall_replay
  val replay_rdy = data_arb.io.in(1).ready
  data_arb.io.in(1).bits.offset := replay.offset(offsetmsb,ramindexlsb)
  data_arb.io.in(1).bits.idx := replay.idx
  data_arb.io.in(1).bits.rw := replay.cmd === M_XWR
  data_arb.io.in(1).valid := replay_val
  replayer.io.data_req.ready := replay_rdy && !stall_replay
  r_replay_amo <== replay_amo_val && replay_rdy && !stall_replay

  // store write mask generation.
  // assumes store replays are higher-priority than pending stores.
  val maskgen = new StoreMaskGen
  val store_offset = Mux(!replay_val, p_store_idx(offsetmsb,0), replay.offset)
  maskgen.io.typ := Mux(!replay_val, p_store_type, replay.typ)
  maskgen.io.addr := store_offset(offsetlsb-1,0)
  val store_wmask_wide = maskgen.io.wmask << Cat(store_offset(ramindexlsb-1,offsetlsb), Bits(0, log2up(CPU_DATA_BITS/8))).toUFix
  val store_data = Mux(!replay_val, p_store_data, replay.data)
  val store_data_wide = Fill(MEM_DATA_BITS/CPU_DATA_BITS, store_data)
  data_arb.io.in(1).bits.data := store_data_wide
  data_arb.io.in(1).bits.wmask := store_wmask_wide
  data_arb.io.in(2).bits.data := store_data_wide
  data_arb.io.in(2).bits.wmask := store_wmask_wide

  // load data subword mux/sign extension.
  // subword loads are delayed by one cycle.
  val loadgen = new LoadDataGen
  val loadgen_use_replay = Reg(replay_val && replay_rdy)
  loadgen.io.typ := Mux(loadgen_use_replay, Reg(replay.typ), r_cpu_req_type)
  loadgen.io.addr := Mux(loadgen_use_replay, Reg(replay.offset), r_cpu_req_idx)(ramindexlsb-1,0)
  loadgen.io.din := data.io.resp

  amoalu.io.cmd := p_store_cmd
  amoalu.io.typ := p_store_type
  amoalu.io.lhs := loadgen.io.r_dout.toUFix
  amoalu.io.rhs := p_store_data.toUFix

  early_nack <== early_tag_nack || early_load_nack || r_cpu_req_val && r_req_amo || replay_amo_val || r_replay_amo

  // reset and flush unit
  val flusher = new FlushUnit(lines)
  val flushed = Reg(resetVal = Bool(true))
  val flush_rdy = mshr.io.fence_rdy && wb_rdy && !p_store_valid
  flushed <== flushed && !r_cpu_req_val || r_cpu_req_val && r_req_flush && flush_rdy && flusher.io.req.ready
  flusher.io.req.valid := r_cpu_req_val && r_req_flush && flush_rdy && !flushed
  flusher.io.wb_req <> wb_arb.io.in(0)
  flusher.io.meta_req.bits.inner_req <> meta_arb.io.in(0).bits 
  flusher.io.meta_req.ready :=  meta_arb.io.in(0).ready
  meta_arb.io.in(0).valid := flusher.io.meta_req.valid
  flusher.io.meta_resp <> meta.io.resp
  flusher.io.resp.ready := Bool(true) // we don't respond to flush requests

  // we usually nack rather than reporting that the cache is not ready.
  // fences and flushes are the exceptions.
  val pending_fence = Reg(resetVal = Bool(false))
  pending_fence <== (r_cpu_req_val && r_req_fence || pending_fence) && !flush_rdy
  val nack_hit   = p_store_match || r_req_write && !p_store_rdy
  val nack_miss  = dirty && !wb_rdy || !mshr.io.req_rdy || r_req_write && !replayer.io.sdq_enq.ready
  val nack_flush = !flush_rdy && (r_req_fence || r_req_flush) ||
                   !flushed && r_req_flush
  val nack = early_nack || r_req_readwrite && Mux(tag_match, nack_hit, nack_miss) || nack_flush

  io.cpu.req_rdy   := flusher.io.req.ready && !(r_cpu_req_val_ && r_req_flush) && !pending_fence
  io.cpu.resp_nack := r_cpu_req_val_ && !io.cpu.req_kill && nack
  io.cpu.resp_val  := (tag_hit && !nack_hit && r_req_read) || replayer.io.cpu_resp_val
  io.cpu.resp_replay := replayer.io.cpu_resp_val
  io.cpu.resp_miss := tag_miss && !nack_miss && r_req_read
  io.cpu.resp_tag  := Mux(replayer.io.cpu_resp_val, replayer.io.cpu_resp_tag, r_cpu_req_tag)
  io.cpu.resp_data := loadgen.io.dout
  io.cpu.resp_data_subword := loadgen.io.r_dout_subword
                      
  wb.io.mem_req.ready := io.mem.req_rdy
  io.mem.req_val   := wb.io.mem_req.valid
  io.mem.req_rw    := wb.io.mem_req.bits.rw
  io.mem.req_wdata := wb.io.mem_req_data
  io.mem.req_tag   := wb.io.mem_req.bits.tag.toUFix
  io.mem.req_addr  := wb.io.mem_req.bits.addr
}
 
class HellaCacheAssoc(lines: Int) extends Component {
  val io = new ioDCacheHella()
  
  val addrbits    = PADDR_BITS
  val indexbits   = log2up(lines)
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
  val r_cpu_req_data   = Reg() { Bits() }

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
  val r_req_amo   = r_cpu_req_cmd(3).toBool
  val r_req_read  = r_req_load || r_req_amo
  val r_req_write = r_req_store || r_req_amo
  val r_req_readwrite = r_req_read || r_req_write

  // replay unit
  val replayer = new ReplayUnit()
  val replay_amo_val = replayer.io.data_req.valid && replayer.io.data_req.bits.cmd(3).toBool
  
  when (replay_amo_val) {
    r_cpu_req_idx  <== Cat(replayer.io.data_req.bits.idx, replayer.io.data_req.bits.offset)
    r_cpu_req_cmd  <== replayer.io.data_req.bits.cmd
    r_cpu_req_type <== replayer.io.data_req.bits.typ
    r_cpu_req_data <== replayer.io.data_req.bits.data
  }
  when (io.cpu.req_val) {
    r_cpu_req_idx  <== io.cpu.req_idx
    r_cpu_req_cmd  <== io.cpu.req_cmd
    r_cpu_req_type <== io.cpu.req_type
    r_cpu_req_tag  <== io.cpu.req_tag
    when (req_write) {
      r_cpu_req_data <== io.cpu.req_data
    }
  }

  // refill counter
  val rr_count = Reg(resetVal = UFix(0, log2up(REFILL_CYCLES)))
  val rr_count_next = rr_count + UFix(1)
  when (io.mem.resp_val) { rr_count <== rr_count_next }

  val misaligned =
    (((r_cpu_req_type === MT_H) || (r_cpu_req_type === MT_HU)) && (r_cpu_req_idx(0) != Bits(0))) ||
    (((r_cpu_req_type === MT_W) || (r_cpu_req_type === MT_WU)) && (r_cpu_req_idx(1,0) != Bits(0))) ||
    ((r_cpu_req_type === MT_D) && (r_cpu_req_idx(2,0) != Bits(0)));
    
  io.cpu.xcpt_ma_ld := r_cpu_req_val_ && r_req_read && misaligned
  io.cpu.xcpt_ma_st := r_cpu_req_val_ && r_req_write && misaligned
  // tags
  val meta = new MetaDataArrayArray(lines)
  val meta_arb = (new Arbiter(3)) { new MetaArrayArrayReq() }
  meta_arb.io.out <> meta.io.req

  // data
  val data = new DataArrayArray(lines)
  val data_arb = (new Arbiter(5)) { new DataArrayArrayReq() }
  data_arb.io.out <> data.io.req

  // cpu tag check
  meta_arb.io.in(2).valid := io.cpu.req_val
  meta_arb.io.in(2).bits.inner_req.idx := io.cpu.req_idx(indexmsb,indexlsb)
  meta_arb.io.in(2).bits.inner_req.rw := Bool(false)
  meta_arb.io.in(2).bits.inner_req.data.valid := Bool(false) // don't care
  meta_arb.io.in(2).bits.inner_req.data.dirty := Bool(false) // don't care
  meta_arb.io.in(2).bits.inner_req.data.tag := UFix(0)       // don't care
  meta_arb.io.in(2).bits.way_en := ~UFix(0, NWAYS)
  val early_tag_nack = !meta_arb.io.in(2).ready
  //val tag_match_arr = meta.io.resp.map(r => r.valid && (r.tag === io.cpu_req_ppn))
  val tag_match_arr = (0 until NWAYS).map( w => meta.io.resp(w).valid && (meta.io.resp(w).tag === io.cpu.req_ppn))
  val tag_match = Cat(Bits(0),tag_match_arr:_*).orR
  val tag_hit  = r_cpu_req_val &&  tag_match
  val tag_miss = r_cpu_req_val && !tag_match
  val hit_way_oh = Cat(Bits(0),tag_match_arr.reverse:_*) //TODO: use GenArray
  val meta_resp_way_oh = Mux(meta.io.way_en === ~UFix(0, NWAYS), hit_way_oh, meta.io.way_en)
  val data_resp_way_oh = Mux(data.io.way_en === ~UFix(0, NWAYS), hit_way_oh, data.io.way_en)
  val meta_resp_mux = Mux1H(NWAYS, meta_resp_way_oh, meta.io.resp)
  //val meta_resp_mux = MuxCase(meta.io.resp(0), (0 until NWAYS).map(i => (meta_resp_way_oh(i).toBool, meta.io.resp(i))))//
  val data_resp_mux = Mux1H(NWAYS, data_resp_way_oh, data.io.resp)
  //val data_resp_mux = MuxCase(data.io.resp(0), (0 until NWAYS).map(i => (data_resp_way_oh(i).toBool, data.io.resp(i))))//

  // writeback unit
  val wb = new WritebackUnit
  val wb_arb = (new Arbiter(2)) { new WritebackReq() }
  wb_arb.io.out <> wb.io.req
  wb.io.data_req <> data_arb.io.in(3)
  wb.io.data_resp <> data_resp_mux

  // replacement policy
  val replacer = new RandomReplacementWayGen()
  replacer.io.way_en := ~UFix(0, NWAYS)
  val replaced_way_id = replacer.io.way_id 
  val replaced_way_oh = UFixToOH(replaced_way_id, NWAYS)
  val meta_wb_mux = meta.io.resp(replaced_way_id)
  val dirty = meta_wb_mux.valid && meta_wb_mux.dirty

  // refill response
  val block_during_refill = !io.mem.resp_val && (rr_count != UFix(0))
  data_arb.io.in(0).bits.inner_req.offset := rr_count
  data_arb.io.in(0).bits.inner_req.rw := !block_during_refill
  data_arb.io.in(0).bits.inner_req.wmask := ~UFix(0, MEM_DATA_BITS/8)
  data_arb.io.in(0).bits.inner_req.data := io.mem.resp_data
  data_arb.io.in(0).valid := io.mem.resp_val || block_during_refill

  // load hits
  data_arb.io.in(4).bits.inner_req.offset := io.cpu.req_idx(offsetmsb,ramindexlsb)
  data_arb.io.in(4).bits.inner_req.idx := io.cpu.req_idx(indexmsb,indexlsb)
  data_arb.io.in(4).bits.inner_req.rw := Bool(false)
  data_arb.io.in(4).bits.inner_req.wmask := UFix(0) // don't care
  data_arb.io.in(4).bits.inner_req.data := io.mem.resp_data // don't care
  data_arb.io.in(4).valid := io.cpu.req_val && req_read
  data_arb.io.in(4).bits.way_en := ~UFix(0, NWAYS) // intiate load on all ways, mux after tag check
  val early_load_nack = req_read && !data_arb.io.in(4).ready

  // store hits and AMO hits and misses use a pending store register.
  // we nack new stores if a pending store can't retire for some reason.
  // we drain a pending store if the CPU performs a store or a
  // conflictig load, or if the cache is idle, or after a miss.
  val p_store_idx_match = p_store_valid && (r_cpu_req_idx(indexmsb,indexlsb) === p_store_idx(indexmsb,indexlsb))
  val p_store_offset_match = (r_cpu_req_idx(indexlsb-1,offsetlsb) === p_store_idx(indexlsb-1,offsetlsb))
  val p_store_match = r_cpu_req_val && r_req_read && p_store_idx_match && p_store_offset_match
  val drain_store_val = (p_store_valid && (!io.cpu.req_val || !req_read || Reg(tag_miss))) || p_store_match
  data_arb.io.in(2).bits.inner_req.offset := p_store_idx(offsetmsb,ramindexlsb)
  data_arb.io.in(2).bits.inner_req.idx := p_store_idx(indexmsb,indexlsb)
  data_arb.io.in(2).bits.inner_req.rw := Bool(true)
  data_arb.io.in(2).valid := drain_store_val
  data_arb.io.in(2).bits.way_en :=  p_store_way_oh
  val drain_store = drain_store_val && data_arb.io.in(2).ready
  val p_store_rdy = !p_store_valid || drain_store
  val p_amo = Reg(tag_hit && r_req_amo && p_store_rdy && !p_store_match || r_replay_amo, resetVal = Bool(false))
  p_store_valid <== !p_store_rdy || (tag_hit && r_req_store) || p_amo

  // writeback
  val wb_rdy = wb_arb.io.in(1).ready && !p_store_idx_match
  wb_arb.io.in(1).valid := tag_miss && r_req_readwrite && dirty && !p_store_idx_match
  wb_arb.io.in(1).bits.ppn := meta_wb_mux.tag
  wb_arb.io.in(1).bits.idx := r_cpu_req_idx(indexmsb,indexlsb)
  wb_arb.io.in(1).bits.way_oh := replaced_way_oh

  // tag update after a miss or a store to an exclusive clean line.
  val clear_valid = tag_miss && r_req_readwrite && meta_wb_mux.valid && (!dirty || wb_rdy)
  val set_dirty   = tag_hit && !meta_resp_mux.dirty && r_req_write
  meta.io.state_req.bits.inner_req.rw := Bool(true)
  meta.io.state_req.bits.inner_req.idx := r_cpu_req_idx(indexmsb,indexlsb)
  meta.io.state_req.bits.inner_req.data.valid := tag_match
  meta.io.state_req.bits.inner_req.data.dirty := tag_match
  meta.io.state_req.valid := clear_valid || set_dirty
  meta.io.state_req.bits.way_en := Mux(clear_valid, replaced_way_oh, hit_way_oh) 
  
  // pending store data, also used for AMO RHS
  val storegen = new StoreDataGen
  val amoalu = new AMOALU
  storegen.io.typ := r_cpu_req_type
  storegen.io.din  := r_cpu_req_data
  when (p_amo) {
    p_store_data <== amoalu.io.out
  }
  when (tag_hit && r_req_write && p_store_rdy || r_replay_amo) {
    p_store_idx    <== r_cpu_req_idx
    p_store_type   <== r_cpu_req_type
    p_store_cmd    <== r_cpu_req_cmd
    p_store_way_oh <== Mux(r_replay_amo, replayer.io.way_oh, hit_way_oh)
    p_store_data   <== storegen.io.dout
  }

  // miss handling
  val mshr = new MSHRFile()
  mshr.io.req_val := tag_miss && r_req_readwrite && (!dirty || wb_rdy) && (!r_req_write || replayer.io.sdq_enq.ready)
  mshr.io.req_ppn := io.cpu.req_ppn
  mshr.io.req_idx := r_cpu_req_idx(indexmsb,indexlsb)
  mshr.io.req_tag := r_cpu_req_tag
  mshr.io.req_offset := r_cpu_req_idx(offsetmsb,0)
  mshr.io.req_cmd := r_cpu_req_cmd
  mshr.io.req_type := r_cpu_req_type
  mshr.io.req_sdq_id := replayer.io.sdq_id
  mshr.io.req_way_oh := replaced_way_oh
  mshr.io.mem_resp_val := io.mem.resp_val && (~rr_count === UFix(0))
  mshr.io.mem_resp_tag := io.mem.resp_tag
  mshr.io.mem_req <> wb.io.refill_req
  mshr.io.meta_req <> meta_arb.io.in(1)
  mshr.io.replay <> replayer.io.replay
  replayer.io.sdq_enq.valid := tag_miss && r_req_write && (!dirty || wb_rdy) && mshr.io.req_rdy
  replayer.io.sdq_enq.bits := storegen.io.dout
  data_arb.io.in(0).bits.inner_req.idx := mshr.io.mem_resp_idx
  data_arb.io.in(0).bits.way_en := mshr.io.mem_resp_way_oh
  replacer.io.pick_new_way := !io.cpu.req_kill && mshr.io.req_val && mshr.io.req_rdy 

  // replays
  val replay = replayer.io.data_req.bits
  val stall_replay = r_replay_amo || p_amo || p_store_valid
  val replay_val = replayer.io.data_req.valid && !stall_replay
  val replay_rdy = data_arb.io.in(1).ready
  data_arb.io.in(1).bits.inner_req.offset := replay.offset(offsetmsb,ramindexlsb)
  data_arb.io.in(1).bits.inner_req.idx := replay.idx
  data_arb.io.in(1).bits.inner_req.rw := replay.cmd === M_XWR
  data_arb.io.in(1).valid := replay_val
  data_arb.io.in(1).bits.way_en := replayer.io.way_oh
  replayer.io.data_req.ready := replay_rdy && !stall_replay
  r_replay_amo <== replay_amo_val && replay_rdy && !stall_replay

  // store write mask generation.
  // assumes store replays are higher-priority than pending stores.
  val maskgen = new StoreMaskGen
  val store_offset = Mux(!replay_val, p_store_idx(offsetmsb,0), replay.offset)
  maskgen.io.typ := Mux(!replay_val, p_store_type, replay.typ)
  maskgen.io.addr := store_offset(offsetlsb-1,0)
  val store_wmask_wide = maskgen.io.wmask << Cat(store_offset(ramindexlsb-1,offsetlsb), Bits(0, log2up(CPU_DATA_BITS/8))).toUFix
  val store_data = Mux(!replay_val, p_store_data, replay.data)
  val store_data_wide = Fill(MEM_DATA_BITS/CPU_DATA_BITS, store_data)
  data_arb.io.in(1).bits.inner_req.data := store_data_wide
  data_arb.io.in(1).bits.inner_req.wmask := store_wmask_wide
  data_arb.io.in(2).bits.inner_req.data := store_data_wide
  data_arb.io.in(2).bits.inner_req.wmask := store_wmask_wide

  // load data subword mux/sign extension.
  // subword loads are delayed by one cycle.
  val loadgen = new LoadDataGen
  val loadgen_use_replay = Reg(replay_val && replay_rdy)
  loadgen.io.typ := Mux(loadgen_use_replay, Reg(replay.typ), r_cpu_req_type)
  loadgen.io.addr := Mux(loadgen_use_replay, Reg(replay.offset), r_cpu_req_idx)(ramindexlsb-1,0)
  loadgen.io.din := data_resp_mux

  amoalu.io.cmd := p_store_cmd
  amoalu.io.typ := p_store_type
  amoalu.io.lhs := loadgen.io.r_dout.toUFix
  amoalu.io.rhs := p_store_data.toUFix

  early_nack <== early_tag_nack || early_load_nack || r_cpu_req_val && r_req_amo || replay_amo_val || r_replay_amo

  // reset and flush unit
  val flusher = new FlushUnit(lines)
  val flushed = Reg(resetVal = Bool(true))
  val flush_rdy = mshr.io.fence_rdy && wb_rdy && !p_store_valid
  flushed <== flushed && !r_cpu_req_val || r_cpu_req_val && r_req_flush && flush_rdy && flusher.io.req.ready
  flusher.io.req.valid := r_cpu_req_val && r_req_flush && flush_rdy && !flushed
  flusher.io.wb_req <> wb_arb.io.in(0)
  flusher.io.meta_req <> meta_arb.io.in(0)
  flusher.io.meta_resp <> meta_resp_mux
  flusher.io.resp.ready := Bool(true) // we don't respond to flush requests

  // we usually nack rather than reporting that the cache is not ready.
  // fences and flushes are the exceptions.
  val pending_fence = Reg(resetVal = Bool(false))
  pending_fence <== (r_cpu_req_val && r_req_fence || pending_fence) && !flush_rdy
  val nack_hit   = p_store_match || r_req_write && !p_store_rdy
  val nack_miss  = dirty && !wb_rdy || !mshr.io.req_rdy || r_req_write && !replayer.io.sdq_enq.ready
  val nack_flush = !flush_rdy && (r_req_fence || r_req_flush) ||
                   !flushed && r_req_flush
  val nack = early_nack || r_req_readwrite && Mux(tag_match, nack_hit, nack_miss) || nack_flush

  io.cpu.req_rdy   := flusher.io.req.ready && !(r_cpu_req_val_ && r_req_flush) && !pending_fence
  io.cpu.resp_nack := r_cpu_req_val_ && !io.cpu.req_kill && nack
  io.cpu.resp_val  := (tag_hit && !nack_hit && r_req_read) || replayer.io.cpu_resp_val
  io.cpu.resp_replay := replayer.io.cpu_resp_val
  io.cpu.resp_miss := tag_miss && !nack_miss && r_req_read
  io.cpu.resp_tag  := Mux(replayer.io.cpu_resp_val, replayer.io.cpu_resp_tag, r_cpu_req_tag)
  io.cpu.resp_data := loadgen.io.dout
  io.cpu.resp_data_subword := loadgen.io.r_dout_subword
                      
  wb.io.mem_req.ready := io.mem.req_rdy
  io.mem.req_val   := wb.io.mem_req.valid
  io.mem.req_rw    := wb.io.mem_req.bits.rw
  io.mem.req_wdata := wb.io.mem_req_data
  io.mem.req_tag   := wb.io.mem_req.bits.tag.toUFix
  io.mem.req_addr  := wb.io.mem_req.bits.addr
}
 
}
