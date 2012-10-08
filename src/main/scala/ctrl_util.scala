package rocket

import Chisel._
import Node._

class rocketCtrlSboard(entries: Int, nread: Int, nwrite: Int) extends Component
{
  class read_port extends Bundle {
    val addr = UFix(INPUT, log2Up(entries))
    val data = Bool(OUTPUT)
  }
  class write_port extends Bundle {
    val en = Bool(INPUT)
    val addr = UFix(INPUT, log2Up(entries))
    val data = Bool(INPUT)
  }

  val io = new Bundle {
    val r = Vec(nread) { new read_port() }
    val w = Vec(nwrite) { new write_port() }
  }

  val busybits = Reg(resetVal = Bits(0, entries))

  val wmasks = (0 until nwrite).map(i => Fill(entries, io.w(i).en) & (UFix(1) << io.w(i).addr))
  val wdatas = (0 until nwrite).map(i => Mux(io.w(i).data, wmasks(i), UFix(0)))
  var next = busybits & ~wmasks.reduceLeft(_|_) | wdatas.reduceLeft(_|_)
  busybits := next

  for (i <- 0 until nread)
    io.r(i).data := busybits(io.r(i).addr)
}
