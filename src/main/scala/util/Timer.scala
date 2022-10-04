// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util.{PriorityEncoder, Valid, log2Up}

/** Timer with a statically-specified period.
  * Can take multiple inflight start-stop events with ID
  * Will continue to count down as long as at least one event is inflight
  */
class Timer(initCount: Int, maxInflight: Int) extends Module {
  val io = IO(new Bundle {
    val start = Flipped(Valid(UInt(log2Up(maxInflight).W)))
    val stop = Flipped(Valid(UInt(log2Up(maxInflight).W)))
    val timeout = Valid(UInt(log2Up(maxInflight).W))
  })

  val inflight = RegInit(VecInit(Seq.fill(maxInflight) { false.B }))
  val countdown = Reg(UInt(log2Up(initCount).W))
  val active = inflight.reduce(_ || _)

  when (active) { countdown := countdown - 1.U }

  when (io.start.valid) {
    inflight(io.start.bits) := true.B
    countdown := (initCount - 1).U
  }

  when (io.stop.valid) { inflight(io.stop.bits) := false.B }

  io.timeout.valid := countdown === 0.U && active
  io.timeout.bits := PriorityEncoder(inflight)

  assert(!io.stop.valid || inflight(io.stop.bits),
         "Timer stop for transaction that's not inflight")
}

/** Simplified Timer with a statically-specified period.
  * Can be stopped repeatedly, even when not active.
  */
class SimpleTimer(initCount: Int) extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val stop = Input(Bool())
    val timeout = Output(Bool())
  })

  val countdown = Reg(UInt(log2Up(initCount).W))
  val active = RegInit(false.B)

  when (active) { countdown := countdown - 1.U }

  when (io.start) {
    active := true.B
    countdown := (initCount - 1).U
  }

  when (io.stop) { active := false.B }

  io.timeout := countdown === 0.U && active
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
  val io = IO(new Bundle {
    val start   = Input(Bool())
    val period  = Input(UInt(w.W))
    val stop    = Input(Bool())
    val timeout = Output(Bool())
  })

  val countdown = RegInit(0.U(w.W))
  val active = RegInit(false.B)

  when (io.start) {
    countdown := io.period
    active := true.B
  } .elsewhen (io.stop || countdown === 0.U) {
    active := false.B
  } .elsewhen (active) {
    countdown := countdown - 1.U
  }

  io.timeout := countdown === 0.U && active
}
