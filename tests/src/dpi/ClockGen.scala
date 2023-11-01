package org.chipsalliance.rocketchip.internal.tests.dpi

import chisel3._
import chisel3.experimental.ExtModule
import chisel3.probe._
import chisel3.util.HasExtModuleInline

case class ClockGenParameter(clockRate: Int)

class ClockGen(val parameter: ClockGenParameter) extends ExtModule with HasExtModuleInline with HasExtModuleDefine {
  setInline(
    s"$desiredName.sv",
    s"""module $desiredName;
       |  reg clock = 1'b0;
       |  always #(${parameter.clockRate}) clock = ~clock;
       |  reg reset = 1'b1;
       |  initial #(${2 * parameter.clockRate + 1}) reset = 0;
       |endmodule
       |""".stripMargin
  )
  val clock = define(RWProbe(Bool()), Seq("ClockGen", "ClockGen", "clock"))
  val reset = define(RWProbe(Bool()), Seq("ClockGen", "ClockGen", "reset"))
}
