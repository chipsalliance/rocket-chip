package Top

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

  val busybits = Reg(resetVal = Bits(0, entries));

  for (i <- 0 until nread)
    io.r(i).data := busybits(io.r(i).addr)

  var wdata = busybits
  for (i <- 0 until nwrite)
    wdata = wdata.bitSet(io.w(i).addr, Mux(io.w(i).en, io.w(i).data, wdata(io.w(i).addr)))
  busybits := wdata
}
