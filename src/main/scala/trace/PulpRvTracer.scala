// See LICENSE.SiFive for license details.

package freechips.rocketchip.trace

import chisel3._
import chisel3.experimental.IntParam
import chisel3.util.{Cat, Decoupled, HasBlackBoxPath}
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule._
import java.nio.file.Paths

/** Parameters for the optional PULP rv_tracer backend. */
case class PulpRvTracerParams(tracerBaseAddr: BigInt, coreParams: TraceCoreParams)

class PulpRvTracerBlackBox(params: TraceCoreParams) extends BlackBox(Map(
  "N" -> IntParam(params.nGroups)
)) with HasBlackBoxPath {
  val io = IO(new Bundle {
    val clk_i = Input(Clock())
    val rst_ni = Input(Bool())
    val enable_i = Input(Bool())
    val config_valid_i = Input(Bool())
    val config_addr_i = Input(UInt(8.W))
    val config_data_i = Input(UInt(32.W))
    val config_ready_o = Output(Bool())
    val valid_i = Input(UInt(params.nGroups.W))
    val itype_i = Input(Vec(params.nGroups, UInt(4.W)))
    val cause_i = Input(UInt(params.xlen.W))
    val tval_i = Input(UInt(params.xlen.W))
    val priv_i = Input(UInt(2.W))
    val iaddr_i = Input(Vec(params.nGroups, UInt(params.xlen.W)))
    val iretire_i = Input(Vec(params.nGroups, UInt(32.W)))
    val ilastsize_i = Input(UInt(params.nGroups.W))
    val time_i = Input(UInt(64.W))
    val encapsulator_ready_i = Input(Bool())
    val external_enable_i = Input(Bool())
    val packet_valid_o = Output(UInt(params.nGroups.W))
    val packet_type_o = Output(Vec(params.nGroups, UInt(4.W)))
    val packet_length_o = Output(Vec(params.nGroups, UInt(5.W)))
    val packet_payload_o = Output(Vec(params.nGroups, UInt(248.W)))
    val stall_o = Output(Bool())
  })
  private val rvTracerRoot = Paths.get("../rv_tracer").toAbsolutePath.normalize.toString
  private def rv(path: String): Unit = addPath(Paths.get(rvTracerRoot, path).toString)
  rv("rtl/rv_tracer_wrapper.sv")
  rv("rtl/rv_tracer.sv")
  rv("include/te_pkg.sv")
  rv("rtl/te_branch_map.sv")
  rv("rtl/te_filter.sv")
  rv("rtl/te_packet_emitter.sv")
  rv("rtl/te_priority.sv")
  rv("rtl/te_reg.sv")
  rv("rtl/te_resync_counter.sv")
  rv("rtl/rv_tracer_math_compat.sv")
  rv("rtl/lzc.sv")
  rv("rtl/rv_tracer_compat.sv")
}

/**
  * Adapts the RISC-V Processor Trace core interface to PULP rv_tracer.
  * The first integration is intentionally constrained to one retirement group:
  * rv_tracer may emit one packet per group while the existing debug sink is a
  * single byte stream.
  */
class LazyPulpRvTracer(val coreParams: TraceCoreParams)(implicit p: Parameters)
    extends LazyModule {
  require(coreParams.nGroups == 1,
    "PULP rv_tracer backend currently supports a single retirement group")
  require(coreParams.xlen == 64,
    "PULP rv_tracer wrapper currently targets RV64")
  override lazy val module = new LazyPulpRvTracerModule(this)
}

class LazyPulpRvTracerModule(outer: LazyPulpRvTracer) extends LazyModuleImp(outer) {
    val io = IO(new Bundle {
      val enable = Input(Bool())
      val config = Flipped(Decoupled(new PulpTraceConfigRequest))
      val in = Input(new TraceCoreInterface(outer.coreParams))
      val stall = Output(Bool())
      val out = Decoupled(UInt(8.W))
    })
    val bb = Module(new PulpRvTracerBlackBox(outer.coreParams))
    val g = io.in.group.head
    bb.io.clk_i := clock
    bb.io.rst_ni := !reset.asBool
    bb.io.enable_i := io.enable
    bb.io.external_enable_i := io.enable
    bb.io.config_valid_i := io.config.valid
    bb.io.config_addr_i := io.config.bits.addr
    bb.io.config_data_i := io.config.bits.data
    io.config.ready := bb.io.config_ready_o
    // The PULP filter has only the two architectural privilege bits. Rocket
    // encodes Debug Mode separately in priv(2), so do not alias Debug-ROM
    // retirements to M-mode when a M-only filter is selected.
    bb.io.valid_i := (io.enable && !io.in.priv(2) && (g.iretire =/= 0.U)).asUInt
    bb.io.itype_i(0) := g.itype.asUInt
    bb.io.cause_i := io.in.cause
    bb.io.tval_i := io.in.tval
    bb.io.priv_i := io.in.priv(1, 0)
    bb.io.iaddr_i(0) := g.iaddr
    bb.io.iretire_i(0) := g.iretire.pad(32)
    bb.io.ilastsize_i := g.ilastsize
    bb.io.time_i := io.in.time.pad(64)

    val payload = RegInit(0.U(248.W))
    val bytes = Wire(Vec(31, UInt(8.W)))
    bytes := payload.asTypeOf(bytes)
    val queue = Module(new TraceByteFifo(64))
    queue.io.clear := !io.enable
    val emit = RegInit(false.B)
    val index = RegInit(0.U(6.W))
    val length = Reg(UInt(5.W))
    when (!io.enable) {
      emit := false.B
      index := 0.U
    }.elsewhen (!emit && bb.io.packet_valid_o(0)) {
      emit := true.B
      index := 0.U
      length := bb.io.packet_length_o.head
      payload := bb.io.packet_payload_o.head
    }.elsewhen (emit && queue.io.enq.fire) {
      when (index === (length +& 1.U)) { emit := false.B }
      index := index + 1.U
    }
    queue.io.enq.valid := emit
    // Every trace packet starts with 0xA5, followed by the Normal
    // Encapsulation length byte and the little-endian rv_tracer payload.
    queue.io.enq.bits := Mux(index === 0.U,
      "hA5".U(8.W),
      Mux(index === 1.U,
        Cat(0.U(3.W), length),
        bytes((index - 2.U)(4, 0))))
    io.out.valid := queue.io.deq.valid && io.enable
    io.out.bits := queue.io.deq.bits
    queue.io.deq.ready := io.out.ready && io.enable
    // The downstream FIFO can accept bytes while a packet is being drained.
    // Keeping ready low for the whole drain interval deadlocks lossless
    // rv_tracer mode: it stalls retirement before the packet can complete.
    bb.io.encapsulator_ready_i := queue.io.enq.ready && io.enable
    io.stall := emit && !queue.io.enq.ready && io.enable
}
