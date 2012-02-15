package Top {

import Chisel._;
import Node._;
import Constants._;

class ioMem() extends Bundle
{
  val req_val = Bool(OUTPUT);
  val req_rdy = Bool(INPUT);
  val req_rw  = Bool(OUTPUT);
  val req_addr = UFix(PADDR_BITS - OFFSET_BITS, OUTPUT);
  val req_wdata = Bits(MEM_DATA_BITS, OUTPUT);
  val req_tag = Bits(MEM_TAG_BITS, OUTPUT);
  
  val resp_val = Bool(INPUT);
  val resp_tag = Bits(MEM_TAG_BITS, INPUT);
  val resp_data = Bits(MEM_DATA_BITS, INPUT);
}

class ioMemArbiter extends Bundle() {
  val mem = new ioMem();
  val dcache = new ioDCache();
//   val icache = new ioICache();
  val icache = new ioIPrefetcherMem().flip();
  val vicache = new ioICache();
}

class rocketMemArbiter extends Component {
  val io = new ioMemArbiter();

  // *****************************
  // Interface to memory
  // *****************************

  // Memory request is valid if either icache or dcache have a valid request
  io.mem.req_val := (io.icache.req_val || io.vicache.req_val || io.dcache.req_val);

  // Set read/write bit.  ICache always reads
  io.mem.req_rw := Mux(io.dcache.req_val, io.dcache.req_rw, Bool(false));

  // Give priority to ICache
  io.mem.req_addr :=
    Mux(io.dcache.req_val, io.dcache.req_addr,
    Mux(io.icache.req_val, io.icache.req_addr,
        io.vicache.req_addr))

  // low bit of tag to indicate D$, I$, and VI$
  val t_dcache :: t_icache :: t_vicache :: Nil = Enum(3){ UFix() }
  io.mem.req_tag :=
    Mux(io.dcache.req_val, Cat(io.dcache.req_tag, t_dcache),
    Mux(io.icache.req_val, Cat(io.icache.req_tag, t_icache),
        Cat(Bits(0, MEM_TAG_BITS-2), t_vicache)))

  // Just pass through write data (only D$ will write)
  io.mem.req_wdata := io.dcache.req_wdata;

  // *****************************
  // Interface to caches
  // *****************************

  // Read for request from cache if the memory is ready.  Give priority to D$.
  // This way, writebacks will never be interrupted by I$ refills.
  io.dcache.req_rdy := io.mem.req_rdy;
  io.icache.req_rdy := io.mem.req_rdy && !io.dcache.req_val;
  io.vicache.req_rdy := io.mem.req_rdy && !io.dcache.req_val && !io.icache.req_val

  // Response will only be valid for D$ or I$ not both because of tag bits
  io.dcache.resp_val := io.mem.resp_val && (io.mem.resp_tag(1,0) === t_dcache)
  io.icache.resp_val := io.mem.resp_val && (io.mem.resp_tag(1,0) === t_icache)
  io.vicache.resp_val := io.mem.resp_val && (io.mem.resp_tag(1,0) === t_vicache)

  // Feed through data to both 
  io.dcache.resp_data := io.mem.resp_data;
  io.icache.resp_data := io.mem.resp_data;
  io.vicache.resp_data := io.mem.resp_data
  
  io.dcache.resp_tag := io.mem.resp_tag >> UFix(2)
  io.icache.resp_tag := io.mem.resp_tag >> UFix(2)
}

}
