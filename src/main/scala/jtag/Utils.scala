// See LICENSE.jtag for license details.

package freechips.rocketchip.jtag

import Chisel._
import chisel3.core.{Input, Output}
import chisel3.experimental.withClock
import chisel3.util.HasBlackBoxInline

/** Bundle representing a tristate pin.
  */
class Tristate extends Bundle {
  val data = Bool()
  val driven = Bool()  // active high, pin is hi-Z when driven is low
}

class NegEdgeReg(w: Int) extends HasBlackBoxInline {


  require( w > 0, s"NegEdgeReg must be at least 1 bit wide, not ${w}")
  val width_str = if (w > 1) s"[${w-1}:0]" else ""

  override def desiredName = s"NegEdgeReg_$w"

  val io = IO(new Bundle{
    val clock = Input(Clock())
    val enable = Input(Bool())
    val d = Input(UInt(w.W))
    val q = Output(UInt(w.W))
  })

  //scalastyle:off regex
  setInline(s"NegEdgeReg_${w}.v",
    s"""
      |module NegEdgeReg_${w}(
      |    input      clock,
      |    input      enable,
      |    input      ${width_str} d,
      |    output reg ${width_str} q
      |);
      |  always @(negedge clock) begin
      |    if (enable) begin
      |        q <= d;
      |    end
      |  end
      |endmodule
    """.stripMargin)
}

  /** Generates a non-reset register that updates on the falling edge of the input clock signal.
  */
object NegEdgeReg {
  def apply[T <: Data](clock: Clock, next: T, enable: Bool=true.B, name: Option[String] = None): T = {
    val reg = Module(new NegEdgeReg(w = next.getWidth))
    reg.io.clock := clock
    reg.io.d := next.asUInt
    reg.io.enable := enable
    name.foreach(reg.suggestName(_))
    next.chiselCloneType.fromBits(reg.io.q)
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
