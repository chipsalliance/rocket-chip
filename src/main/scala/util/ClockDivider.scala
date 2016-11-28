// See LICENSE.Berkeley for license details.

package util

import Chisel._

/** Divide the clock by 2 */
class ClockDivider2 extends Module {
  val io = new Bundle {
    val clock_out = Clock(OUTPUT)
  }

  val clock_reg = Reg(Bool())
  clock_reg := !clock_reg

  io.clock_out := clock_reg.asClock
}

/** Divide the clock by power of 2 times.
 *  @param pow2 divides the clock 2 ^ pow2 times
 *  WARNING: This is meant for simulation use only. */
class Pow2ClockDivider(pow2: Int) extends Module {
  val io = new Bundle {
    val clock_out = Clock(OUTPUT)
  }

  if (pow2 == 0) {
    io.clock_out := clock
  } else {
    val dividers = Seq.fill(pow2) { Module(new ClockDivider2) }

    dividers.init.zip(dividers.tail).map { case (last, next) =>
      next.clock := last.io.clock_out
    }

    io.clock_out := dividers.last.io.clock_out
  }
}
