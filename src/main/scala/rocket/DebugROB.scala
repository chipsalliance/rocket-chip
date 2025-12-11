// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.{Parameters}
import freechips.rocketchip.tile.{HasCoreParameters}
import freechips.rocketchip.util.DecoupledHelper

case class DebugROBParams(size: Int)

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
    hartid: UInt, valid: Bool, tag: UInt, data: UInt)(implicit p: Parameters): Unit = {
    val debug_rob_push_wb = Module(new DebugROBPushWb)
    debug_rob_push_wb.io.clock := clock
    debug_rob_push_wb.io.reset := reset
    debug_rob_push_wb.io.hartid := hartid
    debug_rob_push_wb.io.valid := valid
    debug_rob_push_wb.io.wb_tag := tag
    debug_rob_push_wb.io.wb_data := data
  }
}

class TaggedInstruction(val nXPR: Int)(implicit val p: Parameters) extends Bundle {
  val insn = new TracedInstruction
  val tag = UInt(log2Up(nXPR + 1).W)
  val waiting = Bool()
}

class TaggedWbData(implicit val p: Parameters) extends Bundle with HasCoreParameters {
  val valid = Bool()
  val data  = UInt(xLen.W)
}

class HardDebugROB(val traceRatio: Int, val nXPR: Int)(implicit val p: Parameters)
  extends Module with HasCoreParameters
{
  val io = IO(new Bundle {
    val i_insn    = Input(new TracedInstruction)
    val should_wb = Input(Bool())
    val has_wb    = Input(Bool())
    val tag       = Input(UInt(log2Up(nXPR + 1).W))

    val wb_val    = Input(Bool())
    val wb_tag    = Input(UInt(log2Up(nXPR + 1).W))
    val wb_data   = Input(UInt(xLen.W))

    val o_insn   = Output(new TracedInstruction)
  })

  val iq = Module(new Queue(new TaggedInstruction(nXPR), traceRatio * nXPR, flow = true))

  // No backpressure
  assert(iq.io.enq.ready)

  iq.io.enq.valid := io.i_insn.valid || io.i_insn.exception || io.i_insn.interrupt
  iq.io.enq.bits.insn := io.i_insn
  iq.io.enq.bits.tag := io.tag
  iq.io.enq.bits.waiting := io.should_wb && !io.has_wb

  val wb_q = Seq.fill(nXPR)(Reg(new TaggedWbData))

  for (i <- 0 until nXPR) {
    when (io.wb_val && i.U === io.wb_tag) {
      assert(wb_q(i).valid === false.B)
      wb_q(i).valid := true.B
      wb_q(i).data := io.wb_data
    }
  }

  val tag_matches = Seq.fill(nXPR)(Wire(Bool()))
  for (i <- 0 until nXPR) {
    val is_match = iq.io.deq.bits.waiting &&
                   (iq.io.deq.bits.tag === i.U) &&
                   wb_q(i).valid
    when (is_match) {
      tag_matches(i) := true.B
    } .otherwise {
      tag_matches(i) := false.B
    }
  }

  val tag_match = tag_matches.reduce(_ || _)
  val vinsn_rdy = !iq.io.deq.bits.waiting ||
                  (iq.io.deq.bits.tag >= nXPR.U) || // not integer instruction
                  tag_match
  val maybeFire = Mux(iq.io.deq.bits.insn.valid, vinsn_rdy, true.B)
  val fireTrace = DecoupledHelper(
    iq.io.deq.valid,
    maybeFire)

  iq.io.deq.ready := fireTrace.fire(iq.io.deq.valid)
  io.o_insn := iq.io.deq.bits.insn
  io.o_insn.valid := iq.io.deq.fire && iq.io.deq.bits.insn.valid

  for (i <- 0 until nXPR) {
    when (tag_match && fireTrace.fire() && i.U === iq.io.deq.bits.tag) {
      io.o_insn.wdata.get := wb_q(i).data
      wb_q(i).valid := false.B
    }
  }

  val qcnt = RegInit(0.U(64.W))
  when (iq.io.enq.fire && !iq.io.deq.fire) {
    qcnt := qcnt + 1.U
  } .elsewhen (!iq.io.enq.fire && iq.io.deq.fire) {
    qcnt := qcnt - 1.U
  } .otherwise {
    qcnt := qcnt
  }
}
