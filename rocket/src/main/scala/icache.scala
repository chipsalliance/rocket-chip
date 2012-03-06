package rocket

import Chisel._;
import Node._;
import Constants._;
import scala.math._;

// interface between I$ and pipeline/ITLB (32 bits wide)
class ioImem(view: List[String] = null) extends Bundle (view)
{
  val invalidate = Bool(INPUT);
  val itlb_miss  = Bool(INPUT);
  val req_val   = Bool(INPUT);
  val req_idx   = Bits(PGIDX_BITS, INPUT);
  val req_ppn   = Bits(PPN_BITS, INPUT);
  val resp_data = Bits(32, OUTPUT);
  val resp_val  = Bool(OUTPUT);
}

class ioRocketICache extends Bundle()
{
  val cpu = new ioImem();
  val mem = new ioUncachedRequestor
}

// basic direct mapped instruction cache
// 32 bit wide cpu port, 128 bit wide memory port, 64 byte cachelines
// parameters :
//    lines = # cache lines
class rocketICache(sets: Int, assoc: Int) extends Component {
  val io = new ioRocketICache();

  val lines = sets * assoc;
  val addrbits = PADDR_BITS;
  val indexbits = log2up(sets);
  val offsetbits = OFFSET_BITS;
  val tagmsb    = addrbits - 1;
  val taglsb    = indexbits+offsetbits;
  val tagbits   = addrbits-taglsb;
  val indexmsb  = taglsb-1;
  val indexlsb  = offsetbits;
  val offsetmsb = indexlsb-1;
  val databits = 32;
  val offsetlsb = log2up(databits/8);
  val rf_cnt_bits = log2up(REFILL_CYCLES);

  require(PGIDX_BITS >= taglsb); // virtually-indexed, physically-tagged constraint
  require(ispow2(sets) && ispow2(assoc));
  
  val s_reset :: s_ready :: s_request :: s_refill_wait :: s_refill :: Nil = Enum(5) { UFix() };
  val state = Reg(resetVal = s_reset);
  val invalidated = Reg() { Bool() }
  
  val r_cpu_req_idx    = Reg { Bits() }
  val r_cpu_req_ppn    = Reg { Bits() }
  val r_cpu_req_val    = Reg(resetVal = Bool(false));

  val rdy = Wire() { Bool() }
  val tag_hit = Wire() { Bool() }
  
  when (io.cpu.req_val && rdy) {
    r_cpu_req_val   := Bool(true)
    r_cpu_req_idx   := io.cpu.req_idx
  }
  .otherwise {
    r_cpu_req_val   := Bool(false)
  }
  when (state === s_ready && r_cpu_req_val && !io.cpu.itlb_miss) {
    r_cpu_req_ppn := io.cpu.req_ppn
  }

  val r_cpu_hit_addr = Cat(io.cpu.req_ppn, r_cpu_req_idx)
  val r_cpu_hit_tag = r_cpu_hit_addr(tagmsb,taglsb)
  val r_cpu_miss_addr = Cat(r_cpu_req_ppn, r_cpu_req_idx)
  val r_cpu_miss_tag = r_cpu_miss_addr(tagmsb,taglsb)

  // refill counter
  val refill_count = Reg(resetVal = UFix(0, rf_cnt_bits));
  when (io.mem.xact_rep.valid) {
    refill_count := refill_count + UFix(1);
  }
  val refill_done = io.mem.xact_rep.valid && refill_count.andR

  val repl_way = LFSR16(state === s_ready && r_cpu_req_val && !io.cpu.itlb_miss && !tag_hit)(log2up(assoc)-1,0)
  val word_shift = Cat(r_cpu_req_idx(offsetmsb-rf_cnt_bits,offsetlsb), UFix(0, log2up(databits))).toUFix
  val tag_we = refill_done
  val tag_addr = 
    Mux((state === s_refill), r_cpu_req_idx(indexmsb,indexlsb),
      io.cpu.req_idx(indexmsb,indexlsb)).toUFix;
  val data_addr = 
    Mux((state === s_refill_wait) || (state === s_refill),  Cat(r_cpu_req_idx(indexmsb,offsetbits), refill_count),
      io.cpu.req_idx(indexmsb, offsetbits-rf_cnt_bits)).toUFix;

  val data_mux = (new Mux1H(assoc)){Bits(width = MEM_DATA_BITS)}
  var any_hit = Bool(false)
  for (i <- 0 until assoc)
  {
    val repl_me = (repl_way === UFix(i))
    val tag_array = Mem(sets){ r_cpu_miss_tag }
    tag_array.setReadLatency(1);
    tag_array.setTarget('inst);
    val tag_rdata = tag_array.rw(tag_addr, r_cpu_miss_tag, tag_we && repl_me);

    // valid bit array
    val vb_array = Reg(resetVal = Bits(0, sets));
    when (io.cpu.invalidate) {
      vb_array := Bits(0)
    }
    .elsewhen (tag_we && repl_me) {
      vb_array := vb_array.bitSet(r_cpu_req_idx(indexmsb,indexlsb).toUFix, !invalidated)
    }

    val valid = vb_array(r_cpu_req_idx(indexmsb,indexlsb)).toBool;
    val hit = valid && (tag_rdata === r_cpu_hit_addr(tagmsb,taglsb))
    
    // data array
    val data_array = Mem(sets*REFILL_CYCLES){ io.mem.xact_rep.bits.data }
    data_array.setReadLatency(1);
    data_array.setTarget('inst);
    val data_out = data_array.rw(data_addr, io.mem.xact_rep.bits.data, io.mem.xact_rep.valid && repl_me)

    data_mux.io.sel(i) := hit
    data_mux.io.in(i) := (data_out >> word_shift)(databits-1,0);

    any_hit = any_hit || hit
  }
  tag_hit := any_hit

  val finish_q = (new queue(1)) { new TransactionFinish }
  finish_q.io.enq.valid := refill_done
  finish_q.io.enq.bits.global_xact_id := io.mem.xact_rep.bits.global_xact_id

  // output signals
  io.cpu.resp_val := !io.cpu.itlb_miss && (state === s_ready) && r_cpu_req_val && tag_hit;
  rdy := !io.cpu.itlb_miss && (state === s_ready) && (!r_cpu_req_val || tag_hit);
  io.cpu.resp_data := data_mux.io.out
  io.mem.xact_init.valid := (state === s_request) && finish_q.io.enq.ready
  io.mem.xact_init.bits.t_type := X_INIT_READ_UNCACHED
  io.mem.xact_init.bits.address := r_cpu_miss_addr(tagmsb,indexlsb).toUFix
  io.mem.xact_finish <> finish_q.io.deq

  // control state machine
  when (io.cpu.invalidate) {
    invalidated := Bool(true)
  }
  switch (state) {
    is (s_reset) {
      state := s_ready;
    }
    is (s_ready) {
      when (r_cpu_req_val && !tag_hit && !io.cpu.itlb_miss) {
        state := s_request;
      }
      invalidated := Bool(false)
    }
    is (s_request)
    {
      when (io.mem.xact_init.ready && finish_q.io.enq.ready) {
        state := s_refill_wait;
      }
    }
    is (s_refill_wait) {
      when (io.mem.xact_abort.valid) {
        state := s_request
      }
      when (io.mem.xact_rep.valid) {
        state := s_refill;
      }
    }
    is (s_refill) {
      when (refill_done) {
        state := s_ready;
      }
    }
  } 
}
