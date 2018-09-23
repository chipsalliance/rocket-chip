// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import freechips.rocketchip.config.{Field, Parameters}

case object ClockGateImpl extends Field[() => ClockGate](() => new EICG_wrapper)

abstract class ClockGate extends BlackBox {
  val io = IO(new Bundle{
    val in = Input(Clock())
    val en = Input(Bool())
    val out = Output(Clock())
  })
}

object ClockGate {
  def apply[T <: ClockGate](
      in: Clock,
      en: Bool,
      name: Option[String] = None)(implicit p: Parameters): Clock = {
    val cg = Module(p(ClockGateImpl)())
    name.foreach(cg.suggestName(_))
    cg.io.in := in
    cg.io.en := en
    cg.io.out
  }

  def apply[T <: ClockGate](
      in: Clock,
      en: Bool,
      name: String)(implicit p: Parameters): Clock =
    apply(in, en, Some(name))
}

// behavioral model of Integrated Clock Gating cell
class EICG_wrapper extends ClockGate
