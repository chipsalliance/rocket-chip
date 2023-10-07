// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket

import chisel3._
import chisel3.util._
import chisel3.experimental.{IntParam}
import org.chipsalliance.cde.config.{Parameters}
import freechips.rocketchip.tile.{HasCoreParameters}


class WidenedTracedInstruction extends Bundle {
  val valid = Bool()
  val iaddr = UInt(64.W)
  val insn = UInt(64.W)
  val priv = UInt(3.W)
  val exception = Bool()
  val interrupt = Bool()
  val cause = UInt(64.W)
  val tval = UInt(64.W)
  val wdata = UInt(512.W)
}

// "DebugROB" classes provide an in-order commit log
// with wdata populated.
// These is not synthesizable, they use a C++ blackbox to implement the
// write-back reordering
class DebugROBPushTrace(implicit val p: Parameters) extends BlackBox with HasBlackBoxResource with HasCoreParameters {
  require(traceHasWdata && (vLen max xLen) <= 512)
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val hartid = Input(UInt(32.W))
    val should_wb = Input(Bool())
    val has_wb = Input(Bool())
    val wb_tag = Input(UInt(64.W))
    val trace = Input(new WidenedTracedInstruction)
  })
  addResource("/csrc/debug_rob.cc")
  addResource("/vsrc/debug_rob.v")
}

class DebugROBPushWb(implicit val p: Parameters) extends BlackBox
    with HasBlackBoxResource with HasCoreParameters {
  require(traceHasWdata && (vLen max xLen) <= 512)
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val hartid = Input(UInt(32.W))
    val valid = Input(Bool())
    val wb_tag = Input(UInt(64.W))
    val wb_data = Input(UInt(512.W))
  })
  addResource("/csrc/debug_rob.cc")
  addResource("/vsrc/debug_rob.v")
}

class DebugROBPopTrace(implicit val p: Parameters) extends BlackBox with HasBlackBoxResource with HasCoreParameters {
  require(traceHasWdata && (vLen max xLen) <= 512)
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val hartid = Input(UInt(32.W))
    val trace = Output(new WidenedTracedInstruction)
  })
  addResource("/csrc/debug_rob.cc")
  addResource("/vsrc/debug_rob.v")
}

object DebugROB {
  def pushTrace(clock: Clock, reset: Reset,
    hartid: UInt, trace: TracedInstruction,
    should_wb: Bool, has_wb: Bool, wb_tag: UInt)(implicit p: Parameters) = {
    val debug_rob_push_trace = Module(new DebugROBPushTrace)
    debug_rob_push_trace.io.clock := clock
    debug_rob_push_trace.io.reset := reset
    debug_rob_push_trace.io.hartid := hartid
    debug_rob_push_trace.io.should_wb := should_wb
    debug_rob_push_trace.io.has_wb := has_wb
    debug_rob_push_trace.io.wb_tag := wb_tag
    debug_rob_push_trace.io.trace := trace
  }
  def popTrace(clock: Clock, reset: Reset,
    hartid: UInt)(implicit p: Parameters): TracedInstruction = {
    val debug_rob_pop_trace = Module(new DebugROBPopTrace)
    debug_rob_pop_trace.io.clock := clock
    debug_rob_pop_trace.io.reset := reset
    debug_rob_pop_trace.io.hartid := hartid
    val trace = Wire(new TracedInstruction)
    trace := debug_rob_pop_trace.io.trace
    trace
  }
  def pushWb(clock: Clock, reset: Reset,
    hartid: UInt, valid: Bool, tag: UInt, data: UInt)(implicit p: Parameters) {
    val debug_rob_push_wb = Module(new DebugROBPushWb)
    debug_rob_push_wb.io.clock := clock
    debug_rob_push_wb.io.reset := reset
    debug_rob_push_wb.io.hartid := hartid
    debug_rob_push_wb.io.valid := valid
    debug_rob_push_wb.io.wb_tag := tag
    debug_rob_push_wb.io.wb_data := data
  }
}
