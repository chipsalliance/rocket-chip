// See LICENSE.jtag for license details.

package freechips.rocketchip.jtag

import chisel3._
import chisel3.util._

/** Bundle representing a tristate pin.
  */
class Tristate extends Bundle {
  val data = Bool()
  val driven = Bool()  // active high, pin is hi-Z when driven is low
}

class NegativeEdgeLatch[T <: Data](clock: Clock, dataType: T)
    extends Module(override_clock=Some(clock)) {
  class IoClass extends Bundle {
    val next = Input(dataType)
    val enable = Input(Bool())
    val output = Output(dataType)
  }
  val io = IO(new IoClass)

  val reg = Reg(dataType)
  when (io.enable) {
    reg := io.next
  }
  io.output := reg
}

/** Generates a register that updates on the falling edge of the input clock signal.
  */
object NegativeEdgeLatch {
  def apply[T <: Data](clock: Clock, next: T, enable: Bool=true.B, name: Option[String] = None): T = {
    // TODO better init passing once in-module multiclock support improves
    val latch_module = Module(new NegativeEdgeLatch((!clock.asUInt).asClock, next.cloneType))
    name.foreach(latch_module.suggestName(_))
    latch_module.io.next := next
    latch_module.io.enable := enable
    latch_module.io.output
  }
}

/** A module that counts transitions on the input clock line, used as a basic sanity check and
  * debug indicator clock-crossing designs.
  */
class ClockedCounter(modClock: Clock, counts: BigInt, init: Option[BigInt])
    extends Module(override_clock=Some(modClock)) {
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
    val counter = Module(new ClockedCounter(data.asClock, counts, Some(init)))
    counter.io.count
  }
  def apply (data: Bool, counts: BigInt): UInt = {
    val counter = Module(new ClockedCounter(data.asClock, counts, None))
    counter.io.count
  }
}
