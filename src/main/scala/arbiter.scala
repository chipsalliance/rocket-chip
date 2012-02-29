package rocket

import Chisel._;
import Node._;
import Constants._;

class ioMem() extends Bundle
{
  val req_val = Bool(OUTPUT);
  val req_rdy = Bool(INPUT);
  val req_rw  = Bool(OUTPUT);
  val req_addr = UFix(PADDR_BITS - OFFSET_BITS, OUTPUT);
  val req_tag = Bits(MEM_TAG_BITS, OUTPUT);

  val req_data_val = Bool(OUTPUT);
  val req_data_rdy = Bool(INPUT);
  val req_data_bits = Bits(MEM_DATA_BITS, OUTPUT);
  
  val resp_val = Bool(INPUT);
  val resp_tag = Bits(MEM_TAG_BITS, INPUT);
  val resp_data = Bits(MEM_DATA_BITS, INPUT);
}

class ioMemArbiter(n: Int) extends Bundle() {
  val mem = new ioMem();
  val requestor = Vec(n) { new ioMem().flip() }
}

class rocketMemArbiter(n: Int) extends Component {
  val io = new ioMemArbiter(n);

  var req_val = Bool(false)
  var req_rdy = io.mem.req_rdy
  for (i <- 0 until n)
  {
    io.requestor(i).req_rdy := req_rdy
    req_val = req_val || io.requestor(i).req_val
    req_rdy = req_rdy && !io.requestor(i).req_val
  }

  var req_data_val = Bool(false)
  var req_data_rdy = io.mem.req_data_rdy
  for (i <- 0 until n)
  {
    io.requestor(i).req_data_rdy := req_data_rdy
    req_data_val = req_data_val || io.requestor(i).req_data_val
    req_data_rdy = req_data_rdy && !io.requestor(i).req_data_val
  }

  var req_rw = io.requestor(n-1).req_rw
  var req_addr = io.requestor(n-1).req_addr
  var req_tag = Cat(io.requestor(n-1).req_tag, UFix(n-1, log2up(n)))
  for (i <- n-1 to 0 by -1)
  {
    req_rw = Mux(io.requestor(i).req_val, io.requestor(i).req_rw, req_rw)
    req_addr = Mux(io.requestor(i).req_val, io.requestor(i).req_addr, req_addr)
    req_tag = Mux(io.requestor(i).req_val, Cat(io.requestor(i).req_tag, UFix(i, log2up(n))), req_tag)
  }

  var req_data_bits = io.requestor(n-1).req_data_bits
  for (i <- n-1 to 0 by -1)
    req_data_bits = Mux(io.requestor(i).req_data_val, io.requestor(i).req_data_bits, req_data_bits)

  io.mem.req_val := req_val
  io.mem.req_rw := req_rw
  io.mem.req_addr := req_addr
  io.mem.req_tag := req_tag

  io.mem.req_data_val := req_data_val
  io.mem.req_data_bits := req_data_bits

  for (i <- 0 until n)
  {
    io.requestor(i).resp_val := io.mem.resp_val && io.mem.resp_tag(log2up(n)-1,0) === UFix(i)
    io.requestor(i).resp_data := io.mem.resp_data
    io.requestor(i).resp_tag := io.mem.resp_tag >> UFix(log2up(n))
  }
}
