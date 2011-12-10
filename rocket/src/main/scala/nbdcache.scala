package Top {

import Chisel._
import Node._;
import Constants._;
import scala.math._;

class rocketNBDCacheStoreGen extends Component {
  val io = new Bundle {
    val req_type      = Bits(3, 'input)
    val req_addr_lsb  = Bits(3, 'input)
    val req_data      = Bits(64, 'input)
    val store_wmask   = Bits(8, 'output)
    val store_data    = Bits(64, 'output)
  }
  
  // generate write mask and store data signals based on store type and address LSBs
  val wmask = Wire { Bits(8) }
  switch (io.req_type(1,0))
  {
    is (MT_B) { wmask <== Bits(  1,1) <<     io.req_addr_lsb(2,0).toUFix }
    is (MT_H) { wmask <== Bits(  3,2) << Cat(io.req_addr_lsb(2,1), Bits(0,1)).toUFix }
    is (MT_W) { wmask <== Bits( 15,4) << Cat(io.req_addr_lsb(2,2), Bits(0,2)).toUFix }
    otherwise { wmask <== Bits(255,8) } // MT_D
  }

  val data = Wire { Bits(64) }
  switch (io.req_type(1,0))
  {
    is (MT_B) { data <== Fill(8, io.req_data( 7,0)) }
    is (MT_H) { data <== Fill(4, io.req_data(15,0)) }
    is (MT_W) { data <== Fill(2, io.req_data(31,0)) }
    otherwise { data <== io.req_data } // MT_D
  }
  
  io.store_wmask := wmask
  io.store_data := data
}

class DataMemCmd extends Bundle {
  val offset = Bits(width = OFFSET_BITS)
  val cmd    = Bits(width = 4)
  val typ    = Bits(width = 3)
}

class RPQEntry extends Bundle {
  val cmd    = new DataMemCmd()
  val sdq_id = UFix(width = ceil(log(NSDQ)/log(2)).toInt)
}

class Replay extends Bundle {
  val idx    = Bits(width = IDX_BITS)
  val cmd    = new DataMemCmd()
  val sdq_id = UFix(width = ceil(log(NSDQ)/log(2)).toInt)
}

class DataReq extends Bundle {
  val idx  = Bits(width = IDX_BITS)
  val cmd  = new DataMemCmd()
  val data = Bits(width = CPU_DATA_BITS)
}

class DataArrayReq extends Bundle {
  val idx    = Bits(width = IDX_BITS)
  val offset = Bits(width = ceil(log(REFILL_CYCLES)/log(2)).toInt)
  val rw     = Bool()
  val wmask  = Bits(width = MEM_DATA_BITS/8)
  val data   = Bits(width = MEM_DATA_BITS)
}

class MemReq extends Bundle {
  val rw   = Bool()
  val addr = Bits(width = PPN_BITS+IDX_BITS)
}

class WritebackReq extends Bundle {
  val ppn = Bits(width = PPN_BITS)
  val idx = Bits(width = IDX_BITS)
}

class MetaData extends Bundle {
  val valid = Bool()
  val dirty = Bool()
  val tag = Bits(width = PPN_BITS)
}

class MetaReq extends Bundle {
  val idx  = Bits(width = IDX_BITS)
  val rw  = Bool()
  val data = new MetaData()
}

class MSHR extends Component {
  val io = new Bundle {
    val req_pri_val    = Bool('input)
    val req_pri_rdy    = Bool('output)
    val req_sec_val    = Bool('input)
    val req_sec_rdy    = Bool('output)
    val req_ppn        = Bits(PPN_BITS, 'input)
    val req_idx        = Bits(IDX_BITS, 'input)
    val req_cmd        = new RPQEntry().asInput
    val req_tag        = Bits(CPU_TAG_BITS, 'input)

    val idx_match      = Bool('output)
    val tag            = Bits(PPN_BITS, 'output)

    val mem_resp_val = Bool('input)
    val mem_req  = (new ioDecoupled) { new MemReq()  }.flip
    val meta_req = (new ioDecoupled) { new MetaReq() }.flip
    val replay   = (new ioDecoupled) { new Replay()   }.flip
  }

  val valid = Reg(resetVal = Bool(false))
  val dirty = Reg { Bool() }
  val requested = Reg { Bool() }
  val refilled = Reg { Bool() }
  val ppn = Reg { Bits() }
  val idx = Reg { Bits() }

  val req_load = (io.req_cmd.cmd.cmd === M_XRD) || (io.req_cmd.cmd.cmd === M_PRD) || (io.req_cmd.cmd.cmd === M_PFR)
  val req_use_rpq = (io.req_cmd.cmd.cmd != M_PFR) && (io.req_cmd.cmd.cmd != M_PFW)
  val next_dirty = io.req_pri_val && io.req_pri_rdy && !req_load || io.req_sec_val && io.req_sec_rdy && (!req_load || dirty)
  val sec_rdy = io.idx_match && !refilled && (dirty || !requested || req_load)

  val rpq = (new queueSimplePF(NRPQ)) { new RPQEntry() }
  rpq.io.q_reset := Bool(false)
  rpq.io.enq.valid := (io.req_pri_val && io.req_pri_rdy || io.req_sec_val && sec_rdy) && req_use_rpq
  io.req_cmd ^^ rpq.io.enq.bits
  rpq.io.deq.ready := io.replay.ready && refilled

  when (io.req_pri_val && io.req_pri_rdy) {
    valid <== Bool(true)
    requested <== Bool(false)
    refilled <== Bool(false)
    ppn <== io.req_ppn
    idx <== io.req_idx
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
  dirty <== next_dirty

  io.idx_match := valid && (idx === io.req_idx)
  io.tag := ppn
  io.req_pri_rdy := !valid
  io.req_sec_rdy := sec_rdy && rpq.io.enq.ready

  io.meta_req.valid := valid && refilled && !rpq.io.deq.valid
  io.meta_req.bits.rw := Bool(true)
  io.meta_req.bits.idx := idx
  io.meta_req.bits.data.valid := Bool(true)
  io.meta_req.bits.data.dirty := dirty
  io.meta_req.bits.data.tag := ppn

  io.mem_req.valid := valid && !requested
  //io.mem_req.bits.itm := next_dirty
  io.mem_req.bits.rw := Bool(false)
  io.mem_req.bits.addr := Cat(ppn, idx)

  io.replay.valid := rpq.io.deq.valid && refilled
  io.replay.bits.idx := idx
  rpq.io.deq.bits.cmd ^^ io.replay.bits.cmd
  io.replay.bits.sdq_id := rpq.io.deq.bits.sdq_id
}

class MSHRFile extends Component {
  val io = new Bundle {
    val req_val       = Bool('input)
    val req_rdy       = Bool('output)
    val req_cmd       = (new DataMemCmd).asInput
    val req_ppn       = Bits(PADDR_BITS, 'input)
    val req_idx       = Bits(IDX_BITS, 'input)
    val req_data      = Bits(64, 'input)
    val req_tag       = Bits(CPU_TAG_BITS, 'input)

    val mem_resp_val  = Bool('input)
    val mem_resp_tag  = Bits(DMEM_TAG_BITS, 'input)

    val mem_req  = (new ioDecoupled) { new MemReq()  }.flip()
    val meta_req = (new ioDecoupled) { new MetaReq() }.flip()
    val replay   = (new ioDecoupled) { new Replay()   }.flip()
  }

  val idx_match = Wire { Bool() }
  val pri_rdy = Wire { Bool() }
  val sec_rdy = Wire { Bool() }

  val tag_mux = new Mux1H(NMSHR, PPN_BITS)
  val meta_req_arb = (new Arbiter(NMSHR)) { new MetaReq() }
  val mem_req_arb = (new Arbiter(NMSHR)) { new MemReq() }
  val replay_arb = (new Arbiter(NMSHR)) { new RPQEntry() }
  val alloc_arb = (new Arbiter(NMSHR)) { Bool() }

  val tag_match = tag_mux.io.out === io.req_ppn

  for (i <- 0 to NMSHR-1) {
    val mshr = new MSHR()

    val rpqe = new RPQEntry().asInput
    rpqe.cmd.offset <== io.req_cmd.offset
    rpqe.cmd.cmd <== Mux(io.req_cmd.cmd === M_PRD, M_XRD, io.req_cmd.cmd)
    rpqe.cmd.typ <== io.req_cmd.typ
    rpqe.sdq_id <== UFix(0)

    tag_mux.io.sel(i) := mshr.io.idx_match
    tag_mux.io.in(i) := mshr.io.tag

    alloc_arb.io.in(i).valid := mshr.io.req_pri_rdy
    mshr.io.req_pri_val := io.req_val && !idx_match && alloc_arb.io.in(i).ready

    mshr.io.req_sec_val := io.req_val && tag_match
    mshr.io.req_ppn := io.req_ppn
    mshr.io.req_idx := io.req_idx
    mshr.io.req_tag := io.req_tag
    rpqe ^^ mshr.io.req_cmd

    mshr.io.meta_req <> meta_req_arb.io.in(i)
    mshr.io.mem_req <> mem_req_arb.io.in(i)
    mshr.io.replay <> replay_arb.io.in(i)

    mshr.io.mem_resp_val := io.mem_resp_val && (UFix(i) === io.mem_resp_tag)

    when (mshr.io.req_pri_rdy) { pri_rdy <== Bool(true) }
    when (mshr.io.req_sec_rdy) { sec_rdy <== Bool(true) }
    when (mshr.io.idx_match) { idx_match <== Bool(true) }
  }
  pri_rdy <== Bool(false)
  sec_rdy <== Bool(false)
  idx_match <== Bool(false)

  meta_req_arb.io.out ^^ io.meta_req
  mem_req_arb.io.out ^^ io.mem_req
  replay_arb.io.out ^^ io.replay

  io.req_rdy := Mux(idx_match, tag_match && sec_rdy, pri_rdy)
}

class StoreDataUnit extends Component {
  val io = new Bundle {
    val sdq_enq    = (new ioDecoupled) { Bits(width = CPU_DATA_BITS) }
    val sdq_id     = UFix(width = ceil(log(NSDQ)/log(2)).toInt, dir = 'output)
    val replay     = (new ioDecoupled) { new Replay() }
    val data_req   = (new ioDecoupled) { new DataReq() }.flip()
  }

  val cmdq = (new queueSimplePF(2)) { new Replay() }
  val dataq = (new queueSimplePF(2)) { Bits(width = CPU_DATA_BITS) }

  val next_dataq_enq_rdy = !dataq.io.deq.valid || dataq.io.enq.ready && (!dataq.io.enq.valid || dataq.io.deq.ready)
  val next_dataq_enq_val = io.replay.valid && next_dataq_enq_rdy && (io.replay.bits.cmd.cmd != M_XRD) && cmdq.io.enq.ready
  dataq.io.enq.valid := Reg(next_dataq_enq_val, resetVal = Bool(false))
  dataq.io.enq.bits := sdq_dout
  dataq.io.deq.ready := io.data_req.ready && (cmdq.io.deq.bits.cmd.cmd != M_XRD)

  cmdq.io.enq.valid := io.replay.valid && ((io.replay.bits.cmd.cmd === M_XRD) || next_dataq_enq_rdy)
  io.replay.bits ^^ cmdq.io.enq.bits
  cmdq.io.deq.ready := io.data_req.ready && ((cmdq.io.deq.bits.cmd.cmd === M_XRD) || dataq.io.deq.valid)

  val sdq = Mem4(NSDQ, io.sdq_enq.bits);
  sdq.setReadLatency(1);
  sdq.setTarget('inst);
  val sdq_addr = Mux(next_dataq_enq_val, io.replay.bits.sdq_id, io.sdq_id)
  val sdq_wen = io.sdq_enq.valid && io.sdq_enq.ready
  val sdq_dout = sdq.rw(sdq_addr, io.sdq_enq.bits, sdq_wen, cs = next_dataq_enq_val || sdq_wen);
  val sdq_val = Reg(resetVal = Bits(0, ceil(log(NSDQ)/log(2)).toInt))
  when (next_dataq_enq_val) { sdq_val <== sdq_val.bitSet(io.replay.bits.sdq_id, Bool(false)) }
  when (sdq_wen) { sdq_val <== sdq_val.bitSet(io.sdq_id, Bool(true)) }

  def priority_enc(in: Bits, n: Int = 0): Bits = if (in.width == n-1) UFix(n-1) else if(in(n) == Bool(true)) UFix(n) else priority_enc(in, n+1)
  io.sdq_id := priority_enc(~sdq_val)
  io.sdq_enq.ready := ((~sdq_val) != UFix(0)) && !next_dataq_enq_val
  io.replay.ready := cmdq.io.enq.ready && next_dataq_enq_rdy
  io.data_req.valid := cmdq.io.deq.valid && ((cmdq.io.deq.bits.cmd.cmd === M_XRD) || dataq.io.deq.valid)
  io.data_req.bits.idx := cmdq.io.deq.bits.idx
  cmdq.io.deq.bits.cmd ^^ io.data_req.bits.cmd
  io.data_req.bits.data := dataq.io.deq.bits
}

class WritebackUnit extends Component {
  val io = new Bundle {
    val wb_req    = (new ioDecoupled) { new WritebackReq() }
    val data_req  = (new ioDecoupled) { new DataReq() }.flip()
    val data_resp = Bits(width = MEM_DATA_BITS, dir = 'input)
    val mem_req   = (new ioDecoupled) { new MemReq() }.flip()
  }

  val wbq = (new queueSimplePF(REFILL_CYCLES)) { Bits(width = MEM_DATA_BITS) }
  val valid = Reg(resetVal = Bool(false))
  val cnt = Reg() { UFix(ceil(log(REFILL_CYCLES)/log(2)).toInt) }
  val addr = Reg() { new WritebackReq() }

  wbq.io.enq.valid := valid && Reg(io.data_req.valid && io.data_req.ready)
  wbq.io.enq.bits := io.data_resp
  wbq.io.deq.ready := io.mem_req.ready && (~cnt === UFix(0))

  when (io.wb_req.valid && io.wb_req.ready) { valid <== Bool(true); cnt <== UFix(0); addr <== io.wb_req.bits }
  when (io.data_req.valid && io.data_req.ready) { cnt <== cnt + UFix(1) }
  when ((~cnt === UFix(0)) && !wbq.io.deq.valid) { valid <== Bool(false) }

  io.wb_req.ready := !valid
  io.data_req.valid := valid && wbq.io.enq.ready
  io.data_req.bits.idx := addr.idx
  io.data_req.bits.cmd.offset := cnt * UFix(MEM_DATA_BITS/8)
  io.data_req.bits.cmd.cmd := M_XRD
  io.data_req.bits.cmd.typ := UFix(0)
  io.data_req.bits.data := wbq.io.deq.bits
  io.mem_req.valid := wbq.io.deq.valid && (~cnt === UFix(0))
  io.mem_req.bits.rw := Bool(true)
  io.mem_req.bits.addr := Cat(addr.ppn, addr.idx)
}

class FlushUnit extends Component {
  val io = new Bundle {
    val flush_req  = (new ioDecoupled) { Bits(width = CPU_TAG_BITS) }
    val flush_resp = (new ioDecoupled) { Bits(width = CPU_TAG_BITS) }.flip()
    val meta_req   = (new ioDecoupled) { new MetaReq() }.flip()
    val meta_resp  = (new MetaData).asInput()

    val wb_req_val = Bool(dir = 'output)
    val wb_req_rdy = Bool(dir = 'input)
  }
  
  val s_reset :: s_ready :: s_meta_read :: s_meta_wait :: s_writeback :: s_meta_write :: s_done :: Nil = Enum(7) { UFix() }
  val state = Reg(resetVal = s_reset)
  val tag = Reg() { Bits(width = CPU_TAG_BITS) }
  val cnt = Reg() { UFix(ceil(log(REFILL_CYCLES)/log(2)).toInt) }
  val next_cnt = cnt + UFix(1)

  switch (state) {
    is(s_reset) { when (io.meta_req.ready) { state <== Mux(~cnt === UFix(0), s_ready, s_reset); cnt <== next_cnt } }
    is(s_ready) { when (io.flush_req.valid) { state <== s_meta_read; tag <== io.flush_req.bits } }
    is(s_meta_read) { when (io.meta_req.ready) { state <== s_meta_wait } }
    is(s_meta_wait) { state <== Mux(io.meta_resp.valid && io.meta_resp.dirty, s_writeback, s_meta_write) }
    is(s_writeback) { when (io.wb_req_rdy) { state <== s_meta_write } }
    is(s_meta_write) { when (io.meta_req.ready) { state <== Mux(~cnt === UFix(0), s_done, s_meta_read); cnt <== next_cnt } }
    is(s_done) { when (io.flush_resp.ready) { state <== s_ready } }
  }

  io.flush_req.ready := state === s_ready
  io.flush_resp.valid := state === s_done
  io.flush_resp.bits := tag
  io.meta_req.valid := (state === s_meta_read) || (state === s_meta_write) || (state === s_reset)
  io.meta_req.bits.idx := cnt
  io.meta_req.bits.rw := (state === s_meta_write) || (state === s_reset)
  io.meta_req.bits.data.valid := Bool(false)
  io.meta_req.bits.data.dirty := Bool(false)
  io.meta_req.bits.data.tag := UFix(0)
  io.wb_req_val := state === s_writeback
}

class MetaDataArray(lines: Int) extends Component {
  val io = new Bundle {
    val req  = (new ioDecoupled) { new MetaReq() }
    val resp = (new MetaData).asOutput()
  }

  val array = Mem4(lines, io.resp)
  array.setReadLatency(1)
  array.setTarget('inst)
  val rdata = array.rw(io.req.bits.idx, io.req.bits.data, io.req.valid && io.req.bits.rw, cs = io.req.valid)
  rdata ^^ io.resp
  io.req.ready := Bool(true)
}

class DataArray(lines: Int) extends Component {
  val io = new Bundle {
    val req  = (new ioDecoupled) { new DataArrayReq() }
    val resp = Bits(width = MEM_DATA_BITS, dir = 'output)
  }

  val wmask_array = Vec(MEM_DATA_BITS/8) { Wire() { Bits(width = MEM_DATA_BITS) } }
  wmask_array(0) <== Fill(8, io.req.bits.wmask(0))
  for (i <- 1 to MEM_DATA_BITS/8-1) {
    wmask_array(i) <== Cat(Fill(8, io.req.bits.wmask(i)), wmask_array(i-1)(8*(i+1)-1, 8*i))
  }
  val wmask = wmask_array(MEM_DATA_BITS/8-1)

  val array = Mem4(lines*REFILL_CYCLES, io.resp)
  array.setReadLatency(1)
  array.setTarget('inst)
  val addr = Cat(io.req.bits.idx, io.req.bits.offset)
  val rdata = array.rw(addr, io.req.bits.data, io.req.valid && io.req.bits.rw, wmask, cs = io.req.valid)
  rdata ^^ io.resp
  io.req.ready := Bool(true)
}

// state machine to flush (write back dirty lines, invalidate clean ones) the D$
class rocketNBDCacheDM_flush(lines: Int) extends Component {
  val io = new ioDCacheDM();
  val dcache = new rocketNBDCacheDM(lines);
  
  val addrbits = PADDR_BITS;
  val indexbits = ceil(log10(lines)/log10(2)).toInt;
  val offsetbits = 6;
  val tagmsb    = addrbits - 1;
  val taglsb    = indexbits+offsetbits;
  val tagbits   = tagmsb-taglsb+1;
  val indexmsb  = taglsb-1;
  val indexlsb  = offsetbits;
  val offsetmsb = indexlsb-1;
  val offsetlsb = 3;
  
  val flush_count = Reg(resetVal = UFix(0, indexbits));
  val flush_resp_count = Reg(resetVal = UFix(0, indexbits));
  val flushing = Reg(resetVal = Bool(false));
  val flush_waiting = Reg(resetVal = Bool(false));
  val r_cpu_req_tag = Reg(resetVal = Bits(0, 5));

  when (io.cpu.req_val && io.cpu.req_rdy && (io.cpu.req_cmd === M_FLA)) 
  { 
    r_cpu_req_tag <== io.cpu.req_tag;
    flushing <== Bool(true);
    flush_waiting <== Bool(true);
  }
  
  when (dcache.io.cpu.req_rdy && (flush_count === ~Bits(0, indexbits))) {
    flushing <== Bool(false);
  }
  when (dcache.io.cpu.resp_val && (dcache.io.cpu.resp_tag === r_cpu_req_tag) && (flush_resp_count === ~Bits(0, indexbits))) {
    flush_waiting <== Bool(false);
  }
  
  when (flushing && dcache.io.cpu.req_rdy) {
    flush_count <== flush_count + UFix(1,1);
  }
  when (flush_waiting && dcache.io.cpu.resp_val && (dcache.io.cpu.resp_tag(5,0) === r_cpu_req_tag)) {
    flush_resp_count <== flush_resp_count + UFix(1,1);
  }
  
  dcache.io.cpu.req_val   := (io.cpu.req_val && (io.cpu.req_cmd != M_FLA) && !flush_waiting) || flushing;
  dcache.io.cpu.req_cmd   := Mux(flushing, M_FLA, io.cpu.req_cmd);
  dcache.io.cpu.req_idx   := Mux(flushing, Cat(flush_count, Bits(0,offsetbits)), io.cpu.req_idx);
  dcache.io.cpu.req_ppn   := Mux(flushing, UFix(0,PPN_BITS), io.cpu.req_ppn);
  dcache.io.cpu.req_tag   := Mux(flushing, r_cpu_req_tag, io.cpu.req_tag);
  dcache.io.cpu.req_type  := io.cpu.req_type;
  dcache.io.cpu.req_data  ^^ io.cpu.req_data;
  dcache.io.cpu.req_nack := io.cpu.req_nack;
  dcache.io.mem           ^^ io.mem;

  io.cpu.xcpt_ma_ld   := dcache.io.cpu.xcpt_ma_ld;
  io.cpu.xcpt_ma_st   := dcache.io.cpu.xcpt_ma_st;
  io.cpu.req_rdy   := dcache.io.cpu.req_rdy && !flush_waiting;
  io.cpu.resp_miss := dcache.io.cpu.resp_miss;
  io.cpu.resp_data := dcache.io.cpu.resp_data;
  io.cpu.resp_tag  := dcache.io.cpu.resp_tag;
  io.cpu.resp_val  := dcache.io.cpu.resp_val & 
    !(flush_waiting && (io.cpu.resp_tag === r_cpu_req_tag) && (flush_count != ~Bits(0, addrbits)));
  
}

class rocketNBDCacheAMOALU extends Component {
  val io = new Bundle {
    val cmd    = Bits(4, 'input)
    val wmask  = Bits(64/8, 'input)
    val lhs    = UFix(64, 'input)
    val rhs    = UFix(64, 'input)
    val result = UFix(64, 'output)
  }
  
  val signed = (io.cmd === M_XA_MIN) || (io.cmd === M_XA_MAX)
  val sub = (io.cmd === M_XA_MIN) || (io.cmd === M_XA_MINU) || (io.cmd === M_XA_MAX) || (io.cmd === M_XA_MAXU)
  val min = (io.cmd === M_XA_MIN) || (io.cmd === M_XA_MINU)

  val addsub_rhs = Mux(sub, ~io.rhs, io.rhs)
  val adder_lhs = Cat(io.lhs(63,32), io.wmask(3) & io.lhs(31), io.lhs(30,0)).toUFix;
  val adder_rhs = Cat(addsub_rhs(63,32), io.wmask(3) & addsub_rhs(31), addsub_rhs(30,0)).toUFix;
  val adder_out = adder_lhs + adder_rhs + sub.toUFix

  val cmp_lhs  = Mux(io.wmask(7), io.lhs(63), io.lhs(31))
  val cmp_rhs  = Mux(io.wmask(7), io.rhs(63), io.rhs(31))
  val cmp_diff = Mux(io.wmask(7), adder_out(63), adder_out(31))
  val less = Mux(cmp_lhs === cmp_rhs, cmp_diff, Mux(signed, cmp_lhs, cmp_rhs))
  val cmp_out = Mux(min === less, io.lhs, io.rhs)

  val alu_out = Wire() { UFix() };
  switch (io.cmd) {
    is (M_XA_ADD)  { alu_out <== adder_out }
    is (M_XA_SWAP) { alu_out <== io.rhs }
    is (M_XA_AND)  { alu_out <== io.lhs & io.rhs }
    is (M_XA_OR)   { alu_out <== io.lhs | io.rhs }
  }
  alu_out <== cmp_out

  io.result := alu_out
}

// XXX broken for CPU_DATA_WIDTH != 64
class AMOUnit extends Component {
  val io = new Bundle {
    val req      = (new ioDecoupled) { new DataReq() }
    val lhs      = Bits(width = CPU_DATA_BITS)
    val rhs      = Bits(width = CPU_DATA_BITS)
    val wmask    = Bits(width = CPU_DATA_BITS/8, dir = 'input)
    val data_req = (new ioDecoupled) { new DataReq() }.flip()
  }

  val valid = Reg(resetVal = Bool(false))
  val r_cmd = Reg() { new DataMemCmd() }
  val r_idx = Reg() { Bits(width = IDX_BITS) }
  val r_lhs = Reg() { Bits(width = 64) }
  val r_rhs = Reg() { Bits(width = 64) }
  val r_wmask = Reg() { Bits(width = 64/8) }
  when (io.req.valid && io.req.ready) {
    valid <== Bool(true);
    r_idx <== io.req.bits.idx
    r_lhs <== io.lhs;
    r_rhs <== io.rhs;
    r_cmd <== io.req.bits.cmd;
    r_wmask <== io.wmask
  }
  when (io.data_req.valid && io.data_req.ready) {
    valid <== Bool(false)
  }

  val alu = new rocketNBDCacheAMOALU
  alu.io.cmd := r_cmd.cmd
  alu.io.wmask := r_wmask
  alu.io.lhs := r_lhs
  alu.io.rhs := r_rhs

  io.req.ready := !valid
  io.data_req.valid := valid
  io.data_req.bits.idx := r_idx
  r_cmd ^^ io.data_req.bits.cmd
  io.data_req.bits.data := alu.io.result
}

class rocketNBDCacheDM(lines: Int) extends Component {
  val io = new ioDCacheDM();
  
  val addrbits = PADDR_BITS;
  val indexbits = ceil(log(lines)/log(2)).toInt;
  val offsetbits = OFFSET_BITS;
  val tagmsb    = PADDR_BITS-1;
  val taglsb    = indexbits+offsetbits;
  val tagbits   = tagmsb-taglsb+1;
  val indexmsb  = taglsb-1;
  val indexlsb  = offsetbits;
  val offsetmsb = indexlsb-1;
  val offsetlsb = ceil(log(CPU_DATA_BITS/8)/log(2)).toInt;
  
  val s_reset :: s_ready :: s_replay_load :: s_write_amo :: s_start_writeback :: s_writeback :: s_req_refill :: s_refill :: s_resolve_miss :: Nil = Enum(9) { UFix() };
  val state = Reg(resetVal = s_reset);
  
  // idx arrives one clock cycle prior to ppn b/c of DTLB
  val r_cpu_req_idx    = Reg(resetVal = Bits(0, PGIDX_BITS)); 
  val r_cpu_req_ppn    = Reg(resetVal = Bits(0, PPN_BITS)); 
  val r_cpu_req_val    = Reg(resetVal = Bool(false));
  val r_cpu_req_cmd    = Reg(resetVal = Bits(0,4));
  val r_cpu_req_type   = Reg(resetVal = Bits(0,3));
  val r_cpu_req_tag    = Reg(resetVal = Bits(0,5));
  val r_cpu_resp_val   = Reg(resetVal = Bool(false));
  val r_amo_data       = Reg(resetVal = Bits(0,64));

  val p_store_data      = Reg(resetVal = Bits(0,64));
  val p_store_idx       = Reg(resetVal = Bits(0,PGIDX_BITS));
  val p_store_type      = Reg(resetVal = Bits(0,3));
  val p_store_valid     = Reg(resetVal = Bool(false));

  val req_store   = (io.cpu.req_cmd === M_XWR);
  val req_load    = (io.cpu.req_cmd === M_XRD) || (io.cpu.req_cmd === M_PRD);
  val req_flush   = (io.cpu.req_cmd === M_FLA);
  val req_amo     = io.cpu.req_cmd(3).toBool;
  val r_req_load  = (r_cpu_req_cmd === M_XRD) || (r_cpu_req_cmd === M_PRD);
  val r_req_store = (r_cpu_req_cmd === M_XWR);
  val r_req_flush = (r_cpu_req_cmd === M_FLA);
  val r_req_ptw_load = (r_cpu_req_cmd === M_PRD);
  val r_req_amo     = r_cpu_req_cmd(3).toBool;
  
  when (io.cpu.req_val && io.cpu.req_rdy) {
    r_cpu_req_idx   <== io.cpu.req_idx;
    r_cpu_req_cmd   <== io.cpu.req_cmd;
    r_cpu_req_type  <== io.cpu.req_type;
    r_cpu_req_tag   <== io.cpu.req_tag;
  }
  
  when ((state === s_ready) && r_cpu_req_val && !io.cpu.req_nack) {
    r_cpu_req_ppn <== io.cpu.req_ppn;
  }
  when (io.cpu.req_rdy) {
    r_cpu_req_val <== io.cpu.req_val; 
  }
  otherwise {
    r_cpu_req_val <== Bool(false);
  }
  when (((state === s_resolve_miss) && (r_req_load || r_req_amo)) || (state === s_replay_load)) {
    r_cpu_resp_val <== Bool(true);
  }
  otherwise {
    r_cpu_resp_val <== Bool(false);
  }
  
  // refill counter
  val rr_count = Reg(resetVal = UFix(0,2));
  val rr_count_next = rr_count + UFix(1);
  when (((state === s_refill) && io.mem.resp_val) || ((state === s_writeback) && io.mem.req_rdy)) { 
    rr_count <== rr_count_next;
  }

  // tag array
  val tag_addr = 
    Mux((state === s_ready), io.cpu.req_idx(PGIDX_BITS-1,offsetbits),
      r_cpu_req_idx(PGIDX_BITS-1,offsetbits)).toUFix;  
  val tag_we = 
    ((state === s_refill) && io.mem.resp_val && (rr_count === UFix(3,2))) ||
    ((state === s_resolve_miss) && r_req_flush);

  val tag_array = Mem4(lines, r_cpu_req_ppn);
  tag_array.setReadLatency(SRAM_READ_LATENCY);
//   tag_array.setTarget('inst);
  val tag_rdata = tag_array.rw(tag_addr, r_cpu_req_ppn, tag_we);

  // valid bit array
  val vb_array = Reg(resetVal = Bits(0, lines));
  when (tag_we && !r_req_flush) { 
    vb_array <== vb_array.bitSet(r_cpu_req_idx(PGIDX_BITS-1,offsetbits).toUFix, UFix(1,1));
  }
  when (tag_we && r_req_flush) {
    vb_array <== vb_array.bitSet(r_cpu_req_idx(PGIDX_BITS-1,offsetbits).toUFix, UFix(0,1));
  }
  val vb_rdata = vb_array(r_cpu_req_idx(PGIDX_BITS-1,offsetbits).toUFix).toBool;
  val tag_valid = r_cpu_req_val && vb_rdata;
  val tag_match = (tag_rdata === io.cpu.req_ppn);
  val tag_hit  = tag_valid && tag_match;
  val miss = r_cpu_req_val && (!vb_rdata || !tag_match);

  // load/store addresses conflict if they are to any part of the same 64 bit word
  val addr_match    = (r_cpu_req_idx(PGIDX_BITS-1,offsetlsb) === p_store_idx(PGIDX_BITS-1,offsetlsb));
  val ldst_conflict = tag_valid && tag_match && (r_req_load || r_req_amo) && p_store_valid && addr_match;
  val store_hit = r_cpu_req_val && !io.cpu.req_nack && tag_hit && r_req_store ;

  // write the pending store data when the cache is idle, when the next command isn't a load
  // or when there's a load to the same address (in which case there's a 2 cycle delay:
  // once cycle to write the store data and another to read the data back)
  val drain_store = 
    ((store_hit || p_store_valid) && (!io.cpu.req_val || req_store || req_flush)) ||
    (p_store_valid && (miss || ldst_conflict));

  // write pending store data from a store which missed
  // after the cache line refill has completed
  val resolve_store = (state === s_resolve_miss) && r_req_store;
  
  // pending store data
  when (io.cpu.req_val && io.cpu.req_rdy && req_store) { 
    p_store_idx   <== io.cpu.req_idx;
    p_store_data  <== io.cpu.req_data;
    p_store_type  <== io.cpu.req_type;
  }
  when (store_hit && !drain_store) {
    p_store_valid <== Bool(true);
  }
  when (drain_store)  {
    p_store_valid <== Bool(false);
  }
  
  // AMO operand
  when (io.cpu.req_val && io.cpu.req_rdy && req_amo) {
    r_amo_data <== io.cpu.req_data;
  }

  // dirty bit array
  val db_array  = Reg(resetVal = Bits(0, lines));
  val tag_dirty = db_array(r_cpu_req_idx(PGIDX_BITS-1,offsetbits).toUFix).toBool;  
  when ((r_cpu_req_val && !io.cpu.req_nack && tag_hit && r_req_store) || resolve_store)  {
    db_array <== db_array.bitSet(p_store_idx(PGIDX_BITS-1,offsetbits).toUFix, UFix(1,1));
  }
  when (state === s_write_amo) {
    db_array <== db_array.bitSet(r_cpu_req_idx(PGIDX_BITS-1,offsetbits).toUFix, UFix(1,1));
  }
  when (tag_we) {
    db_array <== db_array.bitSet(r_cpu_req_idx(PGIDX_BITS-1,offsetbits).toUFix, UFix(0,1));
  }

  val mshr = new MSHRFile()
  mshr.io.req_val := r_cpu_req_val
  mshr.io.req_ppn := r_cpu_req_ppn
  mshr.io.req_idx := r_cpu_req_idx(PGIDX_BITS-1, offsetbits)
  mshr.io.req_cmd.offset := r_cpu_req_idx(offsetbits-1, 0)
  mshr.io.req_cmd.cmd := r_cpu_req_cmd
  mshr.io.req_cmd.typ := r_cpu_req_type
  
  // generate write mask and data signals for stores and amos
  val storegen = new rocketDCacheStoreGen();
  storegen.io.req_addr_lsb := p_store_idx(2,0);
  storegen.io.req_data := p_store_data;
  storegen.io.req_type := p_store_type;
  val store_data = Fill(2, storegen.io.store_data);
  val store_wmask_d = storegen.io.store_wmask;
  val store_wmask = Mux(p_store_idx(offsetlsb).toBool, Cat(store_wmask_d, Bits(0,64)), Cat(Bits(0,64), store_wmask_d)); 

  // ALU for AMOs
  val amo_alu = new rocketNBDCacheAMOALU();
  val amo_alu_out = Cat(amo_alu.io.result,amo_alu.io.result);
  val amo_wmask = 
    Mux(r_cpu_req_type === MT_D, ~Bits(0,8),
    Mux(r_cpu_req_idx(2).toBool, Cat(~Bits(0,4), Bits(0,4)),
      Cat(Bits(0,4), ~Bits(0,4))));

  val amo_store_wmask_d = Cat(Fill(8, amo_wmask(7)),
  		Fill(8, amo_wmask(6)),
  		Fill(8, amo_wmask(5)),
  		Fill(8, amo_wmask(4)),
  		Fill(8, amo_wmask(3)),
  		Fill(8, amo_wmask(2)),
  		Fill(8, amo_wmask(1)),
  		Fill(8, amo_wmask(0)));

  val amo_store_wmask = Mux(r_cpu_req_idx(offsetlsb).toBool, Cat(amo_store_wmask_d, Bits(0,64)), Cat(Bits(0,64), amo_store_wmask_d)); 
  
  // data array
  val data_addr = 
    Mux(drain_store || resolve_store, p_store_idx(PGIDX_BITS-1, offsetmsb-1),
    Mux((state === s_writeback) && io.mem.req_rdy, Cat(r_cpu_req_idx(PGIDX_BITS-1, offsetbits), rr_count_next),
    Mux((state === s_start_writeback) || (state === s_writeback) || (state === s_refill), Cat(r_cpu_req_idx(PGIDX_BITS-1, offsetbits), rr_count),
    Mux((state === s_resolve_miss) || (state === s_replay_load) || (state === s_write_amo),  r_cpu_req_idx(PGIDX_BITS-1, offsetmsb-1),
      io.cpu.req_idx(PGIDX_BITS-1, offsetmsb-1))))).toUFix;
      
  val data_wdata = 
    Mux((state === s_refill), io.mem.resp_data, 
    Mux((state === s_write_amo), amo_alu_out,
      store_data));
     
  val data_we =
    ((state === s_refill) && io.mem.resp_val) ||
    (state === s_write_amo) ||
     drain_store || resolve_store;
     
  val data_wmask =
    Mux((state === s_refill), ~Bits(0,128),
    Mux((state === s_write_amo), amo_store_wmask,
      store_wmask));

  val data_array = Mem4(lines*4, data_wdata);
  data_array.setReadLatency(SRAM_READ_LATENCY);
//   data_array.setTarget('inst);
  val data_array_rdata = data_array.rw(data_addr, data_wdata, data_we, data_wmask);
  val resp_data = Mux(r_cpu_req_idx(offsetlsb).toBool, data_array_rdata(127, 64), data_array_rdata(63,0));
  val r_resp_data = Reg(resp_data);

  amo_alu.io.cmd := r_cpu_req_cmd;
  amo_alu.io.wmask := amo_wmask;
  amo_alu.io.lhs := Mux(r_cpu_resp_val, resp_data, r_resp_data).toUFix;
  amo_alu.io.rhs := r_amo_data.toUFix;

  // signal a load miss when the data isn't present in the cache and when it's in the pending store data register
  // (causes the cache to block for 2 cycles and the load or amo instruction is replayed)
  val load_miss = 
    !io.cpu.req_nack && 
    (state === s_ready) && r_cpu_req_val && (r_req_load || r_req_amo) && (!tag_hit || (p_store_valid && addr_match));

  // output signals
  // busy when there's a load to the same address as a pending store, or on a cache miss, or when executing a flush
  io.cpu.req_rdy   := mshr.io.req_rdy && (state === s_ready) && !io.cpu.req_nack && !ldst_conflict && (!r_cpu_req_val || (tag_hit && !(r_req_flush || r_req_amo)));
  io.cpu.resp_val  := !io.cpu.req_nack && 
                      ((state === s_ready) &&  tag_hit && (r_req_load || r_req_amo) && !(p_store_valid && addr_match)) || 
                      ((state === s_resolve_miss) && r_req_flush) ||
                      r_cpu_resp_val;
                      
  val misaligned =
    (((r_cpu_req_type === MT_H) || (r_cpu_req_type === MT_HU)) && r_cpu_req_idx(0).toBool) ||
    (((r_cpu_req_type === MT_W) || (r_cpu_req_type === MT_WU)) && (r_cpu_req_idx(1,0) != Bits(0,2))) ||
    ((r_cpu_req_type === MT_D) && (r_cpu_req_idx(2,0) != Bits(0,3)));
    
  io.cpu.xcpt_ma_ld := r_cpu_req_val && (r_req_load  || r_req_amo) && misaligned;
  io.cpu.xcpt_ma_st := r_cpu_req_val && (r_req_store || r_req_amo) && misaligned;
    
  io.cpu.resp_miss := load_miss;
  // tag MSB distinguishes between loads destined for the PTW and CPU
  io.cpu.resp_tag  := Cat(r_req_ptw_load, r_cpu_req_type, r_cpu_req_idx(2,0), r_cpu_req_tag);
  io.cpu.resp_data := resp_data;

  io.mem.req_val   := (state === s_req_refill) || (state === s_writeback);
  io.mem.req_rw    := (state === s_writeback);
  io.mem.req_wdata := data_array_rdata;
  io.mem.req_tag   := UFix(0);
  io.mem.req_addr  := 
    Mux(state === s_writeback, Cat(tag_rdata, r_cpu_req_idx(PGIDX_BITS-1, offsetbits), rr_count), 
      Cat(r_cpu_req_ppn, r_cpu_req_idx(PGIDX_BITS-1, offsetbits), Bits(0,2))).toUFix;

  // control state machine
  switch (state) {
    is (s_reset) {
      state <== s_ready;
    }
    is (s_ready) {
      when (io.cpu.req_nack) {
        state <== s_ready;
      }
      when (ldst_conflict) {
        state <== s_replay_load;
      }
      when (!r_cpu_req_val || (tag_hit && !(r_req_flush || r_req_amo))) {
        state <== s_ready;
      }
      when (tag_hit && r_req_amo) {
        state <== s_write_amo;
      }
      when (tag_valid & tag_dirty) {
        state <== s_start_writeback; 
      }
      when (r_req_flush) {
        state <== s_resolve_miss;
      }
      otherwise {
        state <== s_req_refill;
      }
    }
    is (s_replay_load) {
      state <== s_ready;
    }
    is (s_write_amo) {
      state <== s_ready;
    }
    is (s_start_writeback) {
      state <== s_writeback;
    }
    is (s_writeback) {
      when (io.mem.req_rdy && (rr_count === UFix(3,2))) { 
        when (r_req_flush) {
          state <== s_resolve_miss;
        } 
        otherwise {
          state <== s_req_refill;
        }
      }
    }
    is (s_req_refill)
    {
      when (io.mem.req_rdy) { state <== s_refill; }
    }
    is (s_refill) {
      when (io.mem.resp_val && (rr_count === UFix(3,2))) { state <== s_resolve_miss; }
    }
    is (s_resolve_miss) {
      when (r_req_amo) {
        state <== s_write_amo;
      }
      state <== s_ready;
    }
  }  
}
 
  
}
