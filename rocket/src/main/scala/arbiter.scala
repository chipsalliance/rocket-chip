package rocket

import Chisel._;
import Node._;
import Constants._;
import uncore._

class ioUncachedRequestor extends Bundle {
  val xact_init      = (new FIFOIO) { new TransactionInit }
  val xact_abort     = (new FIFOIO) { new TransactionAbort }.flip
  val xact_rep       = (new FIFOIO)      { new TransactionReply }.flip
  val xact_finish    = (new FIFOIO) { new TransactionFinish }
}

class rocketMemArbiter(n: Int) extends Component {
  val io = new Bundle {
    val mem = new ioUncachedRequestor
    val requestor = Vec(n) { new ioUncachedRequestor }.flip
  }

  var xi_val = Bool(false)
  var xi_rdy = io.mem.xact_init.ready
  for (i <- 0 until n)
  {
    io.requestor(i).xact_init.ready := xi_rdy
    xi_val = xi_val || io.requestor(i).xact_init.valid
    xi_rdy = xi_rdy && !io.requestor(i).xact_init.valid
  }

  var xi_bits = new TransactionInit
  xi_bits := io.requestor(n-1).xact_init.bits
  xi_bits.tile_xact_id := Cat(io.requestor(n-1).xact_init.bits.tile_xact_id, UFix(n-1, log2Up(n)))
  for (i <- n-2 to 0 by -1)
  {
    var my_xi_bits = new TransactionInit
    my_xi_bits := io.requestor(i).xact_init.bits
    my_xi_bits.tile_xact_id := Cat(io.requestor(i).xact_init.bits.tile_xact_id, UFix(i, log2Up(n)))

    xi_bits = Mux(io.requestor(i).xact_init.valid, my_xi_bits, xi_bits)
  }

  io.mem.xact_init.valid := xi_val
  io.mem.xact_init.bits := xi_bits

  var xf_val = Bool(false)
  var xf_rdy = io.mem.xact_finish.ready
  for (i <- 0 until n)
  {
    io.requestor(i).xact_finish.ready := xf_rdy
    xf_val = xf_val || io.requestor(i).xact_finish.valid
    xf_rdy = xf_rdy && !io.requestor(i).xact_finish.valid
  }

  var xf_bits = io.requestor(n-1).xact_finish.bits
  for (i <- n-2 to 0 by -1)
    xf_bits = Mux(io.requestor(i).xact_finish.valid, io.requestor(i).xact_finish.bits, xf_bits)

  io.mem.xact_finish.valid := xf_val
  io.mem.xact_finish.bits := xf_bits

  for (i <- 0 until n)
  {
    val tag = io.mem.xact_rep.bits.tile_xact_id
    io.requestor(i).xact_rep.valid := io.mem.xact_rep.valid && tag(log2Up(n)-1,0) === UFix(i)
    io.requestor(i).xact_rep.bits := io.mem.xact_rep.bits
    io.requestor(i).xact_rep.bits.tile_xact_id := tag >> UFix(log2Up(n))
  }

  for (i <- 0 until n)
  {
    val tag = io.mem.xact_abort.bits.tile_xact_id
    io.requestor(i).xact_abort.valid := io.mem.xact_abort.valid && tag(log2Up(n)-1,0) === UFix(i)
    io.requestor(i).xact_abort.bits := io.mem.xact_abort.bits
    io.requestor(i).xact_abort.bits.tile_xact_id := tag >> UFix(log2Up(n))
  }

  io.mem.xact_abort.ready := Bool(true)
  io.mem.xact_rep.ready := Bool(true)
}
