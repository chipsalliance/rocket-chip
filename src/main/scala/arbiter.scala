package Top {

import Chisel._;
import Node._;
import Constants._;

class ioMem() extends Bundle
{
  val req_val = Bool('output);
  val req_rdy = Bool('input);
  val req_rw  = Bool('output);
  val req_addr = UFix(32, 'output);
  val req_wdata = Bits(128, 'output);
  val req_tag = Bits(4, 'output);
  
  val resp_val = Bool('input);
  val resp_tag = Bits(4, 'input);
  val resp_data = Bits(128, 'input);
}

class ioArbiter extends Bundle() {
  val mem = new ioMem();
  val dcache = new ioDcache();
//   val icache = new ioIcache();
  val icache = new ioIPrefetcherMem().flip();
}

class rocketMemArbiter extends Component {
  val io = new ioArbiter();

  // *****************************
  // Interface to memory
  // *****************************

  // Memory request is valid if either icache or dcache have a valid request
  io.mem.req_val := (io.icache.req_val || io.dcache.req_val);

  // Set read/write bit.  Icache always reads
  io.mem.req_rw := Mux(io.icache.req_val,Bool(false),io.dcache.req_rw);

  // Give priority to Icache
  io.mem.req_addr := Mux(io.icache.req_val,io.icache.req_addr,io.dcache.req_addr);

  // high bit of tag=0 for I$, tag=0 for D$
//   io.mem.req_tag := Mux(io.icache.req_val,Bits(0,4),Bits(1,4));
  io.mem.req_tag := Mux(io.icache.req_val, 
                        Cat(Bits(0,1), io.icache.req_tag),
                        Cat(Bits(1,1), io.dcache.req_tag));

  // Just pass through write data (only D$ will write)
  io.mem.req_wdata := io.dcache.req_wdata;

  // *****************************
  // Interface to caches
  // *****************************

  // Read for request from cache if the memory is ready.  Give priority to I$
  io.icache.req_rdy := io.mem.req_rdy;
  io.dcache.req_rdy := io.mem.req_rdy && !io.icache.req_val;

  // Response will only be valid for D$ or I$ not both because of tag bits
  io.icache.resp_val := io.mem.resp_val && !io.mem.resp_tag(3).toBool;
  io.dcache.resp_val := io.mem.resp_val &&  io.mem.resp_tag(3).toBool;

  // Feed through data to both 
  io.icache.resp_data := io.mem.resp_data;
  io.dcache.resp_data := io.mem.resp_data;
  
  io.icache.resp_tag := io.mem.resp_tag(2,0);
  io.dcache.resp_tag := io.mem.resp_tag(2,0);

}

}
