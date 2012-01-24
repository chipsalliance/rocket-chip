package Top {

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

// interface between I$ and memory (128 bits wide)
class ioIcache(view: List[String] = null) extends Bundle (view)
{
  val req_addr  = UFix(PADDR_BITS - OFFSET_BITS, INPUT);
  val req_val   = Bool(INPUT);
  val req_rdy   = Bool(OUTPUT);
  val resp_data = Bits(MEM_DATA_BITS, OUTPUT);
  val resp_val  = Bool(OUTPUT);
}

class ioICacheDM extends Bundle()
{
  val cpu = new ioImem();
  val mem = new ioIcache().flip();
}

// basic direct mapped instruction cache
// 32 bit wide cpu port, 128 bit wide memory port, 64 byte cachelines
// parameters :
//    lines = # cache lines
class rocketICacheDM(lines: Int) extends Component {
  val io = new ioICacheDM();

  val addrbits = PADDR_BITS;
  val indexbits = log2up(lines);
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
  
  val s_reset :: s_ready :: s_request :: s_refill_wait :: s_refill :: Nil = Enum(5) { UFix() };
  val state = Reg(resetVal = s_reset);
  
  val r_cpu_req_idx    = Reg { Bits(width = PGIDX_BITS) }
  val r_cpu_req_ppn    = Reg { Bits(width = PPN_BITS) }
  val r_cpu_req_val    = Reg(resetVal = Bool(false));

  val rdy = Wire() { Bool() }
  
  when (io.cpu.req_val && rdy) {
    r_cpu_req_val   <== Bool(true)
    r_cpu_req_idx   <== io.cpu.req_idx
  }
  otherwise {
    r_cpu_req_val   <== Bool(false)
  }
  when (state === s_ready && r_cpu_req_val && !io.cpu.itlb_miss) {
    r_cpu_req_ppn <== io.cpu.req_ppn
  }

  val r_cpu_hit_addr = Cat(io.cpu.req_ppn, r_cpu_req_idx)
  val r_cpu_hit_tag = r_cpu_hit_addr(tagmsb,taglsb)
  val r_cpu_miss_addr = Cat(r_cpu_req_ppn, r_cpu_req_idx)
  val r_cpu_miss_tag = r_cpu_miss_addr(tagmsb,taglsb)

  // refill counter
  val refill_count = Reg(resetVal = UFix(0, rf_cnt_bits));
  when (io.mem.resp_val) {
    refill_count <== refill_count + UFix(1);
  }
  val tag_addr = 
    Mux((state === s_refill_wait), r_cpu_req_idx(indexmsb,indexlsb),
      io.cpu.req_idx(indexmsb,indexlsb)).toUFix;
  val tag_we = (state === s_refill_wait) && io.mem.resp_val;

  val tag_array = Mem4(lines, r_cpu_miss_tag);
  tag_array.setReadLatency(1);
  tag_array.setTarget('inst);
  val tag_rdata = tag_array.rw(tag_addr, r_cpu_miss_tag, tag_we);

  // valid bit array
  val vb_array = Reg(resetVal = Bits(0, lines));
  when (io.cpu.invalidate) {
    vb_array <== Bits(0,lines);
  }
  when (tag_we) {
    vb_array <== vb_array.bitSet(r_cpu_req_idx(indexmsb,indexlsb).toUFix, UFix(1,1));
  }

  val tag_valid = vb_array(r_cpu_req_idx(indexmsb,indexlsb)).toBool;
  val tag_hit = tag_valid && (tag_rdata === r_cpu_hit_addr(tagmsb,taglsb))
  
  // data array
  val data_addr = 
    Mux((state === s_refill_wait) || (state === s_refill),  Cat(r_cpu_req_idx(indexmsb,offsetbits), refill_count),
      io.cpu.req_idx(indexmsb, offsetbits-rf_cnt_bits)).toUFix;
  val data_array = Mem4(lines*REFILL_CYCLES, io.mem.resp_data);
  data_array.setReadLatency(1);
  data_array.setTarget('inst);
  val data_array_rdata = data_array.rw(data_addr, io.mem.resp_data, io.mem.resp_val);

  // output signals
  io.cpu.resp_val := !io.cpu.itlb_miss && (state === s_ready) && r_cpu_req_val && tag_hit;
  rdy <== !io.cpu.itlb_miss && (state === s_ready) && (!r_cpu_req_val || tag_hit);
  io.cpu.resp_data := data_array_rdata >> Cat(r_cpu_req_idx(offsetmsb-rf_cnt_bits,offsetlsb), UFix(0, log2up(databits))).toUFix
  io.mem.req_val := (state === s_request);
  io.mem.req_addr := r_cpu_miss_addr(tagmsb,indexlsb).toUFix

  // control state machine
  switch (state) {
    is (s_reset) {
      state <== s_ready;
    }
    is (s_ready) {
      when (io.cpu.itlb_miss) {
        state <== s_ready;
      }
      when (r_cpu_req_val && !tag_hit) {
        state <== s_request;
      }
    }
    is (s_request)
    {
      when (io.mem.req_rdy) {
        state <== s_refill_wait;
      }
    }
    is (s_refill_wait) {
      when (io.mem.resp_val) {
        state <== s_refill;
      }
    }
    is (s_refill) {
      when (io.mem.resp_val && (~refill_count === UFix(0))) {
        state <== s_ready;
      }
    }
  }  
}

}
