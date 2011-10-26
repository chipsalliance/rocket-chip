package Top {

import Chisel._
import Node._;
import scala.math._;

// interface between I$ and processor (32 bits wide)
class ioImem(view: List[String] = null) extends Bundle (view)
{
  val req_addr  = UFix(32, 'input);
  val req_val   = Bool('input);
  val req_rdy   = Bool('output);
  val resp_data = Bits(32, 'output);
  val resp_val  = Bool('output);
}

// interface between I$ and memory (128 bits wide)
class ioIcache(view: List[String] = null) extends Bundle (view)
{
  val req_addr  = UFix(32, 'input);
  val req_val   = Bool('input);
  val req_rdy   = Bool('output);
  val resp_data = Bits(128, 'output);
  val resp_val  = Bool('output);
}

class ioICacheDM extends Bundle() {
  val cpu = new ioImem();
  val mem = new ioIcache().flip();
}

// basic direct mapped instruction cache
// parameters :
//    lines = # cache lines
//    addr_bits = address width (word addressable) bits
//    32 bit wide cpu port, 128 bit wide memory port, 64 byte cachelines

class rocketICacheDM(lines: Int, addrbits : Int) extends Component {
  val io = new ioICacheDM();
  
  val indexbits = ceil(log10(lines)/log10(2)).toInt;
  val offsetbits = 6;
  val tagmsb    = addrbits - 1;
  val taglsb    = indexbits+offsetbits;
  val indexmsb  = taglsb-1;
  val indexlsb  = offsetbits;
  val offsetmsb = indexlsb-1;
  val offsetlsb = 2;
  
  val s_reset :: s_ready :: s_request :: s_refill_wait :: s_refill :: s_resolve_miss :: Nil = Enum(6) { UFix() };
  val state = Reg(resetVal = s_reset);
  
  val r_cpu_req_addr = Reg(Bits(0, addrbits));
  when (io.cpu.req_val && ((state === s_ready) || (state === s_resolve_miss))) { r_cpu_req_addr <== io.cpu.req_addr; }
  
  val r_cpu_req_val = Reg(Bool(false));
  when ((state === s_ready) || (state === s_resolve_miss)) { r_cpu_req_val <== io.cpu.req_val; }
  otherwise { r_cpu_req_val <== Bool(false); }

  val refill_count = Reg(resetVal = UFix(0,2));
  when (io.mem.resp_val) { refill_count <== refill_count + UFix(1); }
  
  // tag array
  val tag_wdata = r_cpu_req_addr(tagmsb, taglsb);
  val tag_waddr = r_cpu_req_addr(indexmsb, indexlsb).toUFix;
  val tag_we    = (state === s_refill_wait) && io.mem.resp_val;
  val tag_array = Mem(lines, tag_we, tag_waddr, tag_wdata);
  val tag_raddr = io.cpu.req_addr(indexmsb, indexlsb);;
  val tag_lookup = Reg(tag_array.read(tag_raddr));
  
  // valid bit array
  val vb_array = Reg(resetVal = Bits(0, lines));
  val vb_rdata = Reg(vb_array(io.cpu.req_addr(indexmsb, indexlsb)));
  
  when ((state === s_refill_wait) && io.mem.resp_val) { vb_array <== vb_array.bitSet(r_cpu_req_addr(indexmsb, indexlsb).toUFix, UFix(1,1)); }

  val tag_match = vb_rdata.toBool && (tag_lookup === r_cpu_req_addr(tagmsb, taglsb));

  // data array
  val data_array_waddr = Cat(r_cpu_req_addr(indexmsb, indexlsb), refill_count).toUFix;
  val data_array = Mem(lines*4, io.mem.resp_val, data_array_waddr, io.mem.resp_data);
  val data_array_raddr = Cat(io.cpu.req_addr(indexmsb, indexlsb), io.cpu.req_addr(offsetmsb, offsetmsb-1));
  val data_array_read  = data_array(data_array_raddr);
  val data_array_rdata = Reg(data_array_read);
  
  io.cpu.resp_val := (r_cpu_req_val && tag_match && (state === s_ready)); //  || (state === s_resolve_miss);
  io.cpu.req_rdy  := ((state === s_ready) && (!r_cpu_req_val || (r_cpu_req_val && tag_match))); // || (state === s_resolve_miss);
  io.cpu.resp_data := MuxLookup(r_cpu_req_addr(offsetmsb-2, offsetlsb).toUFix, data_array_rdata(127, 96), 
                                Array(UFix(2) -> data_array_rdata(95,64),
                                      UFix(1) -> data_array_rdata(63,32),
                                      UFix(0) -> data_array_rdata(31,0)));

  io.mem.req_val := (state === s_request);
  io.mem.req_addr := Cat(r_cpu_req_addr(tagmsb, indexlsb), Bits(0,2)).toUFix;  

  // control state machine
  switch (state) {
    is (s_reset) {
      state <== s_ready;
    }
    is (s_ready) {
      when (r_cpu_req_val && !tag_match) { state <== s_request; }
    }
    is (s_request)
    {
      when (io.mem.req_rdy) { state <== s_refill_wait; }
    }
    is (s_refill_wait) {
      when (io.mem.resp_val)  { state <== s_refill; }
    }
    is (s_refill) {
      when (io.mem.resp_val && (refill_count === UFix(3,2))) { state <== s_resolve_miss; }
    }
    is (s_resolve_miss) {
      state <== s_ready;
    }
  }  
}

}
