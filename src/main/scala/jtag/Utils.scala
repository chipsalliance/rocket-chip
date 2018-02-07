// See LICENSE.jtag for license details.

package freechips.rocketchip.jtag

import Chisel._
import chisel3.core.{Input, Output}
import chisel3.experimental.withClock

/** Bundle representing a tristate pin.
  */
class Tristate extends Bundle {
  val data = Bool()
  val driven = Bool()  // active high, pin is hi-Z when driven is low
}

/** Generates a register that updates on the falling edge of the input clock signal.
  */
object NegEdgeReg {
  def apply[T <: Data](clock: Clock, next: T, enable: Bool=true.B, name: Option[String] = None): T = {
    // TODO pass in initial value as well
    withClock((!clock.asUInt).asClock) {
      val reg = RegEnable(next = next, enable = enable)
      name.foreach{reg.suggestName(_)}
      reg
    }
  }
}

/** A module that counts transitions on the input clock line, used as a basic sanity check and
  * debug indicator clock-crossing designs.
  */
class ClockedCounter(counts: BigInt, init: Option[BigInt]) extends Module {
  require(counts > 0, "really?")

  val width = log2Ceil(counts)
  class CountIO extends Bundle {
    val count = Output(UInt(width.W))
  }
  val io = IO(new CountIO)

  val count = init match {
    case Some(init) => RegInit(init.U(width.W))
    case None => Reg(UInt(width.W))
  }

  when (count === (counts - 1).asUInt) {
    count := 0.U
  } .otherwise {
    count := count + 1.U
  }
 io.count := count
}

/** Count transitions on the input bit by specifying it as a clock to a counter.
  */
object ClockedCounter {
  def apply (data: Bool, counts: BigInt, init: BigInt): UInt = {
    withClock(data.asClock) {
      val counter = Module(new ClockedCounter(counts, Some(init)))
      counter.io.count
    }
  }

  def apply (data: Bool, counts: BigInt): UInt = {
    withClock(data.asClock) {
      val counter = Module(new ClockedCounter(counts, None))
      counter.io.count
    }
  }
}
