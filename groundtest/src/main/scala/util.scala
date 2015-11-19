package groundtest

import Chisel._

class Timer(initCount: Int) extends Module {
  val io = new Bundle {
    val start = Bool(INPUT)
    val stop = Bool(INPUT)
    val timeout = Bool(OUTPUT)
  }

  val countdown = Reg(UInt(width = log2Up(initCount)))
  val active = Reg(init = Bool(false))

  when (io.start) {
    countdown := UInt(initCount - 1)
    active := Bool(true)
  }

  when (io.stop) {
    active := Bool(false)
  }

  when (active) {
    countdown := countdown - UInt(1)
  }

  io.timeout := countdown === UInt(0)
}

object Timer {
  def apply(initCount: Int, start: Bool, stop: Bool): Bool = {
    val timer = Module(new Timer(initCount))
    timer.io.start := start
    timer.io.stop  := stop
    timer.io.timeout
  }
}
