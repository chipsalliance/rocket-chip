package Top {

import Chisel._;
import Node._;
import Constants._;
import scala.math._;

class ioIPrefetcherMem(view: List[String] = null) extends Bundle (view)
{
  val req_addr  = UFix(PADDR_BITS - OFFSET_BITS, OUTPUT);
  val req_val   = Bool(OUTPUT);
  val req_rdy   = Bool(INPUT);
  val req_tag   = Bits(IMEM_TAG_BITS, OUTPUT);
  val resp_data = Bits(MEM_DATA_BITS, INPUT);
  val resp_val  = Bool(INPUT);
  val resp_tag  = Bits(IMEM_TAG_BITS, INPUT);
}

class ioIPrefetcher extends Bundle() {
  val icache = new ioIcache();
  val mem = new ioIPrefetcherMem();
}

class rocketIPrefetcher extends Component() {
  val io = new ioIPrefetcher();
  val pdq = (new queueSimplePF(REFILL_CYCLES)) { Bits(width = MEM_DATA_BITS) };

  val s_invalid :: s_valid :: s_refilling :: s_req_wait :: s_resp_wait :: s_bad_resp_wait :: Nil = Enum(6) { UFix() };
  val state = Reg(resetVal = s_invalid);

  val demand_miss = io.icache.req_val & io.icache.req_rdy;
  val prefetch_addr = Reg() { UFix(width = io.icache.req_addr.width) };
  when (demand_miss) { prefetch_addr <== io.icache.req_addr + UFix(1); }
  
  val addr_match = (prefetch_addr === io.icache.req_addr);
  val hit = (state != s_invalid) & (state != s_req_wait) & addr_match;
  
  io.icache.req_rdy := io.mem.req_rdy;
  val ip_mem_req_rdy  = io.mem.req_rdy & ~(io.icache.req_val & ~hit);
  val ip_mem_resp_val = io.mem.resp_val && io.mem.resp_tag(0).toBool; 
  
  io.mem.req_val  := io.icache.req_val & ~hit | (state === s_req_wait);
  io.mem.req_tag  := !(io.icache.req_val && !hit);
  io.mem.req_addr := Mux(io.mem.req_tag(0).toBool, prefetch_addr, io.icache.req_addr);
  
  val pdq_reset = Reg(resetVal = Bool(true));
  pdq_reset <== demand_miss & ~hit | (state === s_bad_resp_wait);
  
  val fill_cnt = Reg(resetVal = UFix(0, ceil(log(REFILL_CYCLES)/log(2)).toInt));
  when (ip_mem_resp_val.toBool) { fill_cnt <== fill_cnt + UFix(1); }
  val fill_done = (~fill_cnt === UFix(0)) & ip_mem_resp_val;
  
  val forward = Reg(resetVal = Bool(false));
  val forward_cnt = Reg(resetVal = UFix(0, ceil(log(REFILL_CYCLES)/log(2)).toInt));
  when (forward & pdq.io.deq.valid) { forward_cnt <== forward_cnt + UFix(1); }
  val forward_done = (~forward_cnt === UFix(0)) & pdq.io.deq.valid;
  forward <== (demand_miss & hit | forward & ~forward_done);  

  io.icache.resp_val  := (io.mem.resp_val && !io.mem.resp_tag(0).toBool) || (forward && pdq.io.deq.valid);
  io.icache.resp_data := Mux(forward, pdq.io.deq.bits, io.mem.resp_data);
  
  pdq.io.q_reset  := pdq_reset;
  pdq.io.enq.bits := io.mem.resp_data;
  pdq.io.enq.valid  := ip_mem_resp_val.toBool;
  pdq.io.deq.ready  := forward;
  
  switch (state) {
    is (s_invalid) {
      when (demand_miss) { state <== s_req_wait; }
    }
    is (s_valid) {
      when (demand_miss | (forward & forward_done)) { state <== s_req_wait; }
    }
    is (s_refilling) {
      when (demand_miss & ~addr_match & fill_done.toBool) { state <== s_req_wait; }
      when (demand_miss & ~addr_match) { state <== s_bad_resp_wait; }
      when (fill_done.toBool) { state <== s_valid; }
    }
    is (s_req_wait) {
      when (ip_mem_req_rdy) { state <== s_resp_wait; }
    }
    is (s_resp_wait) {
      when (demand_miss & ~addr_match) { state <== s_bad_resp_wait; }
      when (ip_mem_resp_val.toBool) { state <== s_refilling; }
    }
    is (s_bad_resp_wait) {
      when (fill_done.toBool & ip_mem_resp_val.toBool) { state <== s_req_wait; }
    }
  }
}

}
