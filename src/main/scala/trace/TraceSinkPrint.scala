package freechips.rocketchip.trace


import chisel3._
import chisel3.util._
import chisel3.experimental.StringParam
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci._
import org.chipsalliance.cde.config.{Parameters}

class TraceSinkPrint(blackbox_file_name: String)(implicit p: Parameters) extends LazyModule {
  override lazy val module = new TraceSinkPrintImpl(this)
  val blackbox_name = blackbox_file_name
  class TraceSinkPrintImpl(outer: TraceSinkPrint) extends LazyModuleImp(outer) with HasTraceSinkIO {
    withClockAndReset(clock, reset) {
      io.trace_in.ready := true.B
      val byte_printer = Module(new BytePrinter(outer.blackbox_name))
      byte_printer.io.clk := clock
      byte_printer.io.reset := reset
      byte_printer.io.in_valid := io.trace_in.valid
      byte_printer.io.in_byte := io.trace_in.bits
    }
  }
}

class BytePrinter(name: String) extends BlackBox(
  Map(
    "FILE_NAME" -> StringParam(s"tacit_${name}_trace.out")
  )
) with HasBlackBoxResource {
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val reset = Input(Reset())
    val in_valid = Input(Bool())
    val in_byte = Input(UInt(8.W))
  })
  addResource("/vsrc/BytePrinter.v")
}