package freechips.rocketchip.prci

import chisel3._
import chisel3.util.HasBlackBoxInline
import chisel3.experimental.DoubleParam
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._

class ClockSourceIO extends Bundle {
  val power = Input(Bool())
  val gate = Input(Bool())
  val clk = Output(Clock())
}

/** This clock source is only intended to be used in test harnesses, and does not work correctly in verilator. */
class ClockSourceAtFreq(val freqMHz: Double) extends BlackBox(Map(
  "PERIOD_PS" -> DoubleParam(1000000/freqMHz)
)) with HasBlackBoxInline {
  val io = IO(new ClockSourceIO)

  setInline("ClockSourceAtFreq.v",
    s"""
      |module ClockSourceAtFreq #(parameter PERIOD_PS="") (
      |    input power,
      |    input gate,
      |    output clk);
      |  timeunit 1ps/1ps;
      |
      |  reg clk_i;
      |  initial
      |    clk_i = 1'b0;
      |  always
      |    clk_i = #(PERIOD_PS/2.0) ~clk_i & (power & ~gate);
      |  assign
      |    clk = clk_i;
      |endmodule
      |""".stripMargin)
}

/** This clock source is only intended to be used in test harnesses, and does not work correctly in verilator. */
class ClockSourceAtFreqFromPlusArg(val plusArgName: String) extends BlackBox
    with HasBlackBoxInline {
  val io = IO(new ClockSourceIO)

  override def desiredName = s"ClockSourceAtFreqFromPlusArg$plusArgName"

  setInline(s"$desiredName.v",
    s"""
      |module $desiredName (
      |    input power,
      |    input gate,
      |    output clk);
      |  timeunit 1ps/1ps;
      |
      |  reg clk_i;
      |  real FREQ_MHZ;
      |  real PERIOD_PS;
      |  initial begin
      |    clk_i = 1'b0;
      |    if (!$$value$$plusargs("$plusArgName=%d", FREQ_MHZ)) begin
      |      FREQ_MHZ = 100.0;
      |    end
      |    PERIOD_PS = 1000000.0 / FREQ_MHZ;
      |    forever #(PERIOD_PS/2.0) clk_i = ~clk_i & (power & ~gate);
      |  end
      |  assign clk = clk_i;
      |endmodule
      |""".stripMargin)
}

/** This clock source is only intended to be used in test harnesses, and does not work correctly in verilator. */
class TestClockSource(freqs: Seq[Option[Double]])(implicit p: Parameters) extends LazyModule {

  val node = ClockSourceNode(freqs.map(f =>
    ClockSourceParameters(give = f.map(ff => ClockParameters(freqMHz = ff)))))

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    node.out.zipWithIndex.foreach { case ((bundle, edge), i) =>
      val source = edge.source.give.map(f =>
        Module(new ClockSourceAtFreq(f.freqMHz)).io
      ).getOrElse(Module(new ClockSourceAtFreqFromPlusArg(s"CLOCKFREQMHZ$i")).io)
      source.power := true.B
      source.gate  := false.B
      bundle.clock := source.clk
      bundle.reset := reset
    }
  }
}
