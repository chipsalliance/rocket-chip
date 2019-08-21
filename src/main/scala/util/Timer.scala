// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.util

import Chisel._

/** Timer with a statically-specified period.
  * Can take multiple inflight start-stop events with ID
  * Will continue to count down as long as at least one event is inflight
  */
class Timer(initCount: Int, maxInflight: Int) extends Module {
  val io = new Bundle {
    val start = Valid(UInt(width = log2Up(maxInflight))).flip
    val stop = Valid(UInt(width = log2Up(maxInflight))).flip
    val timeout = Valid(UInt(width = log2Up(maxInflight)))
  }

  val inflight = Reg(init = Vec.fill(maxInflight) { Bool(false) })
  val countdown = Reg(UInt(width = log2Up(initCount)))
  val active = inflight.reduce(_ || _)

  when (active) { countdown := countdown - UInt(1) }

  when (io.start.valid) {
    inflight(io.start.bits) := Bool(true)
    countdown := UInt(initCount - 1)
  }

  when (io.stop.valid) { inflight(io.stop.bits) := Bool(false) }

  io.timeout.valid := countdown === UInt(0) && active
  io.timeout.bits := PriorityEncoder(inflight)

  assert(!io.stop.valid || inflight(io.stop.bits),
         "Timer stop for transaction that's not inflight")
}

/** Simplified Timer with a statically-specified period.
  * Can be stopped repeatedly, even when not active.
  */
class SimpleTimer(initCount: Int) extends Module {
  val io = new Bundle {
    val start = Bool(INPUT)
    val stop = Bool(INPUT)
    val timeout = Bool(OUTPUT)
  }

  val countdown = Reg(UInt(width = log2Up(initCount)))
  val active = Reg(init = Bool(false))

  when (active) { countdown := countdown - UInt(1) }

  when (io.start) {
    active := Bool(true)
    countdown := UInt(initCount - 1)
  }

  when (io.stop) { active := Bool(false) }

  io.timeout := countdown === UInt(0) && active
}

object SimpleTimer {
  def apply(initCount: Int, start: Bool, stop: Bool): Bool = {
    val timer = Module(new SimpleTimer(initCount))
    timer.io.start := start
    timer.io.stop  := stop
    timer.io.timeout
  }
}

/** Timer with a dynamically-specified period.  */
class DynamicTimer(w: Int) extends Module {
  val io = new Bundle {
    val start   = Bool(INPUT)
    val period  = UInt(INPUT, w)
    val stop    = Bool(INPUT)
    val timeout = Bool(OUTPUT)
  }

  val countdown = Reg(init = UInt(0, w))
  val active = Reg(init = Bool(false))

  when (io.start) {
    countdown := io.period
    active := Bool(true)
  } .elsewhen (io.stop || countdown === UInt(0)) {
    active := Bool(false)
  } .elsewhen (active) {
    countdown := countdown - UInt(1)
  }

  io.timeout := countdown === UInt(0) && active
}
