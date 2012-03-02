package rocket

import Chisel._;
import Node._;
import Constants._;

class rocketMemArbiter(n: Int) extends Component {
  val io = new Bundle {
    val mem = new ioTileLink
    val requestor = Vec(n) { new ioTileLink().flip }
  }

  var req_val = Bool(false)
  var req_rdy = io.mem.xact_init.ready
  for (i <- 0 until n)
  {
    io.requestor(i).xact_init.ready := req_rdy
    req_val = req_val || io.requestor(i).xact_init.valid
    req_rdy = req_rdy && !io.requestor(i).xact_init.valid
  }

  // if more than one requestor at a time can write back, the data
  // arbiter needs to be made stateful: one xact's write data must
  // be sent to the memory system contiguously.
  var req_data_val = Bool(false)
  var req_data_rdy = io.mem.xact_init_data.ready
  for (i <- 0 until n)
  {
    io.requestor(i).xact_init_data.ready := req_data_rdy
    req_data_val = req_data_val || io.requestor(i).xact_init_data.valid
    req_data_rdy = req_data_rdy && !io.requestor(i).xact_init_data.valid
  }

  var req_bits = Wire() { new TransactionInit }
  req_bits := io.requestor(n-1).xact_init.bits
  req_bits.tile_xact_id := Cat(io.requestor(n-1).xact_init.bits.tile_xact_id, UFix(n-1, log2up(n)))
  for (i <- n-2 to 0 by -1)
  {
    var my_req_bits = Wire() { new TransactionInit }
    my_req_bits := io.requestor(i).xact_init.bits
    my_req_bits.tile_xact_id := Cat(io.requestor(i).xact_init.bits.tile_xact_id, UFix(i, log2up(n)))

    req_bits = Mux(io.requestor(i).xact_init.valid, my_req_bits, req_bits)
  }

  var req_data_bits = io.requestor(n-1).xact_init_data.bits
  for (i <- n-2 to 0 by -1)
    req_data_bits = Mux(io.requestor(i).xact_init_data.valid, io.requestor(i).xact_init_data.bits, req_data_bits)

  io.mem.xact_init.valid := req_val
  io.mem.xact_init.bits := req_bits

  io.mem.xact_init_data.valid := req_data_val
  io.mem.xact_init_data.bits := req_data_bits

  for (i <- 0 until n)
  {
    val tag = io.mem.xact_rep.bits.tile_xact_id
    io.requestor(i).xact_rep.valid := io.mem.xact_rep.valid && tag(log2up(n)-1,0) === UFix(i)
    io.requestor(i).xact_rep.bits.data := io.mem.xact_rep.bits.data
    io.requestor(i).xact_rep.bits.t_type := io.mem.xact_rep.bits.t_type
    io.requestor(i).xact_rep.bits.tile_xact_id := tag >> UFix(log2up(n))
    io.requestor(i).xact_rep.bits.global_xact_id := io.mem.xact_rep.bits.global_xact_id
  }
}
