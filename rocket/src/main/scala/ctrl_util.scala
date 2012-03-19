package rocket

import Chisel._
import Node._;

class rocketCtrlSboard(entries: Int, nread: Int, nwrite: Int) extends Component
{
  class read_port extends Bundle {
    val addr = UFix(log2up(entries), INPUT)
    val data = Bool(OUTPUT)
  }
  class write_port extends Bundle {
    val en = Bool(INPUT)
    val addr = UFix(log2up(entries), INPUT)
    val data = Bool(INPUT)
  }

  val io = new Bundle {
    val r = Vec(nread) { new read_port() }
    val w = Vec(nwrite) { new write_port() }
  }

  val busybits = Vec(entries) { Reg(resetVal = Bool(false)) }

  for (i <- 0 until nread)
    io.r(i).data := busybits(io.r(i).addr)

  for (i <- 0 until nwrite) {
    when (io.w(i).en) {
      busybits(io.w(i).addr) := io.w(i).data
    }
  }
}
