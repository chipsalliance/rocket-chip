package freechips.rocketchip.trace

import chisel3._
import chisel3.util._
import chisel3.experimental.StringParam

class TraceSinkMonitor(name: String) extends BlackBox(
  Map(
    "FILE_NAME" -> StringParam(s"${name}_trace.out")
  )
) with HasBlackBoxResource {
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val reset = Input(Reset())
    val in_fire = Input(Bool())
    val in_byte = Input(UInt(8.W))
  })
  addResource("/vsrc/TraceSinkMonitor.v")
}