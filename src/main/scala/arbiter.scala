package Top {

import Chisel._;
import Node._;
import Constants._;

class ioMem() extends Bundle
{
  val req_val = Bool('output);
  val req_rdy = Bool('input);
  val req_rw  = Bool('output);
  val req_addr = UFix(PADDR_BITS, 'output);
  val req_wdata = Bits(MEM_DATA_BITS, 'output);
  val req_tag = Bits(MEM_TAG_BITS, 'output);
  
  val resp_val = Bool('input);
  val resp_tag = Bits(MEM_TAG_BITS, 'input);
  val resp_data = Bits(MEM_DATA_BITS, 'input);
}

class ioMemArbiter extends Bundle() {
  val mem = new ioMem();
  val dcache = new ioDcache();
//   val icache = new ioIcache();
  val icache = new ioIPrefetcherMem().flip();
}

class rocketMemArbiter extends Component {
  val io = new ioMemArbiter();

  // *****************************
  // Interface to memory
  // *****************************

  // Memory request is valid if either icache or dcache have a valid request
  io.mem.req_val := (io.icache.req_val || io.dcache.req_val);

  // Set read/write bit.  Icache always reads
  io.mem.req_rw := Mux(io.icache.req_val,Bool(false),io.dcache.req_rw);

  // Give priority to Icache
  io.mem.req_addr := Mux(io.icache.req_val,io.icache.req_addr,io.dcache.req_addr);

  // low bit of tag=0 for I$, 1 for D$
  io.mem.req_tag := Cat(Mux(io.icache.req_val, io.icache.req_tag, io.dcache.req_tag), !io.icache.req_val)

  // Just pass through write data (only D$ will write)
  io.mem.req_wdata := io.dcache.req_wdata;

  // *****************************
  // Interface to caches
  // *****************************

  // Read for request from cache if the memory is ready.  Give priority to I$
  io.icache.req_rdy := io.mem.req_rdy;
  io.dcache.req_rdy := io.mem.req_rdy && !io.icache.req_val;

  // Response will only be valid for D$ or I$ not both because of tag bits
  io.icache.resp_val := io.mem.resp_val && !io.mem.resp_tag(0).toBool;
  io.dcache.resp_val := io.mem.resp_val &&  io.mem.resp_tag(0).toBool;

  // Feed through data to both 
  io.icache.resp_data := io.mem.resp_data;
  io.dcache.resp_data := io.mem.resp_data;
  
  io.icache.resp_tag := io.mem.resp_tag >> UFix(1)
  io.dcache.resp_tag := io.mem.resp_tag >> UFix(1)

}

}
