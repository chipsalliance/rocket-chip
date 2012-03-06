package rocket

import Chisel._;
import Node._;
import Constants._;
import scala.math._;

class ioIPrefetcher extends Bundle() {
  val icache = new ioTileLink().flip
  val mem = new ioTileLink
  val invalidate = Bool(INPUT)
}

class rocketIPrefetcher extends Component() {
  val io = new ioIPrefetcher();
  val pdq = (new queue(REFILL_CYCLES, flushable = true)) { Bits(width = MEM_DATA_BITS) };

  val s_invalid :: s_valid :: s_refilling :: s_req_wait :: s_resp_wait :: s_bad_resp_wait :: Nil = Enum(6) { UFix() };
  val state = Reg(resetVal = s_invalid);

  val ip_mem_resp_abort = io.mem.xact_abort.valid && io.mem.xact_abort.bits.tile_xact_id(0)
  val demand_miss = io.icache.xact_init.valid && io.icache.xact_init.ready
  val prefetch_addr = Reg() { UFix(width = io.icache.xact_init.bits.address.width) };
  val addr_match = (prefetch_addr === io.icache.xact_init.bits.address);
  val hit = (state != s_invalid) && (state != s_req_wait) && addr_match && !ip_mem_resp_abort
  val prefetch_miss = io.icache.xact_init.valid && !hit
  when (demand_miss) { prefetch_addr := io.icache.xact_init.bits.address + UFix(1); }

  io.icache.xact_init.ready := io.mem.xact_init.ready
  val ip_mem_resp_val = io.mem.xact_rep.valid && io.mem.xact_rep.bits.tile_xact_id(0)
  val ip_mem_req_rdy  = io.mem.xact_init.ready && !prefetch_miss
 
  io.mem.xact_abort.ready := Bool(true)
  io.mem.xact_init.valid  := prefetch_miss || (state === s_req_wait)
  io.mem.xact_init.bits.t_type := X_INIT_READ_UNCACHED
  io.mem.xact_init.bits.tile_xact_id  := Mux(prefetch_miss, UFix(0), UFix(1))
  io.mem.xact_init.bits.address := Mux(prefetch_miss, io.icache.xact_init.bits.address, prefetch_addr);
  
  val fill_cnt = Reg(resetVal = UFix(0, ceil(log(REFILL_CYCLES)/log(2)).toInt));
  when (ip_mem_resp_val.toBool) { fill_cnt := fill_cnt + UFix(1); }
  val fill_done = (~fill_cnt === UFix(0)) & ip_mem_resp_val;
  
  val forward = Reg(resetVal = Bool(false));
  val forward_cnt = Reg(resetVal = UFix(0, ceil(log(REFILL_CYCLES)/log(2)).toInt));
  when (forward & pdq.io.deq.valid) { forward_cnt := forward_cnt + UFix(1); }
  val forward_done = (~forward_cnt === UFix(0)) & pdq.io.deq.valid;
  forward := demand_miss && hit || forward && !forward_done

  io.icache.xact_abort.valid := io.mem.xact_abort.valid && !io.mem.xact_abort.bits.tile_xact_id(0) ||
                                forward && ip_mem_resp_abort
  io.icache.xact_rep.valid  := io.mem.xact_rep.valid && !io.mem.xact_rep.bits.tile_xact_id(0) || (forward && pdq.io.deq.valid)
  io.icache.xact_rep.bits.data := Mux(forward, pdq.io.deq.bits, io.mem.xact_rep.bits.data)
  
  pdq.io.flush := Reg(demand_miss && !hit || (state === s_bad_resp_wait), resetVal = Bool(false))
  pdq.io.enq.bits := io.mem.xact_rep.bits.data
  pdq.io.enq.valid  := ip_mem_resp_val.toBool;
  pdq.io.deq.ready  := forward;
  
  switch (state) {
    is (s_invalid) {
      when (demand_miss) { state := s_req_wait; }
    }
    is (s_valid) {
      when (demand_miss | (forward & forward_done)) { state := s_req_wait; }
    }
    is (s_refilling) {
      when (demand_miss & ~addr_match & fill_done.toBool) { state := s_req_wait; }
      .elsewhen (demand_miss & ~addr_match) { state := s_bad_resp_wait; }
      .elsewhen (fill_done.toBool) { state := s_valid; }
    }
    is (s_req_wait) {
      when (ip_mem_req_rdy) { state := s_resp_wait; }
    }
    is (s_resp_wait) {
      when (ip_mem_resp_abort) {
        state := s_invalid
      }
      .elsewhen (demand_miss && !addr_match) { state := s_bad_resp_wait }
      .elsewhen (ip_mem_resp_val) { state := s_refilling }
    }
    is (s_bad_resp_wait) {
      when (fill_done || ip_mem_resp_abort) { state := s_req_wait }
    }
  }

  when (io.invalidate) {
    state := s_invalid
  }
}
