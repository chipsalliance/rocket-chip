// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util.{HasBlackBoxResource, HasBlackBoxPath}
import org.chipsalliance.cde.config.{Field, Parameters}

import java.nio.file.{Files, Paths}

case object ClockGateImpl extends Field[() => ClockGate](() => new EICG_wrapper)
case object ClockGateModelFile extends Field[Option[String]](None)

abstract class ClockGate extends BlackBox
  with HasBlackBoxResource with HasBlackBoxPath {
  val io = IO(new Bundle{
    val in = Input(Clock())
    val test_en = Input(Bool())
    val en = Input(Bool())
    val out = Output(Clock())
  })

  def addVerilogResource(vsrc: String): Unit = {
    if (Files.exists(Paths.get(vsrc)))
      addPath(vsrc)
    else
      addResource(vsrc)
  }
}

object ClockGate {
  def apply[T <: ClockGate](
      in: Clock,
      en: Bool,
      name: Option[String] = None)(implicit p: Parameters): Clock = {
    val cg = Module(p(ClockGateImpl)())
    name.foreach(cg.suggestName(_))
    p(ClockGateModelFile).map(cg.addVerilogResource(_))

    cg.io.in := in
    cg.io.test_en := false.B
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
