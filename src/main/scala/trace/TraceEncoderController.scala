// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.trace

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule._
import org.chipsalliance.diplomacy.bundlebridge._
import freechips.rocketchip.diplomacy.{AddressSet}
import freechips.rocketchip.resources.{SimpleDevice}
import freechips.rocketchip.tilelink.TLRegisterNode
import freechips.rocketchip.regmapper.{RegField, RegFieldDesc}

object TraceSinkTarget {
  def width = 8
}

class TraceEncoderControlInterface() extends Bundle {
  val enable = Bool()
  val target = UInt(TraceSinkTarget.width.W)
  val bp_mode = UInt(32.W)
  val sync_max = UInt(32.W)
  val clear = Bool()
}

class PulpTraceConfigRequest extends Bundle {
  val addr = UInt(8.W)
  val data = UInt(32.W)
}

class TraceSPIStream extends Bundle {
  val enable = Output(Bool())
  val tx = Decoupled(UInt(8.W))
}

/** Small byte FIFO used as the memory-mapped/JTAG trace sink. */
class TraceByteFifo(depth: Int) extends Module {
  private val ptrWidth = log2Ceil(depth)
  private val countWidth = log2Ceil(depth + 1)
  val io = IO(new Bundle {
    val enq = Flipped(Decoupled(UInt(8.W)))
    val deq = Decoupled(UInt(8.W))
    val clear = Input(Bool())
    val count = Output(UInt(countWidth.W))
  })

  val mem = Mem(depth, UInt(8.W))
  val rptr = RegInit(0.U(ptrWidth.W))
  val wptr = RegInit(0.U(ptrWidth.W))
  val count = RegInit(0.U(countWidth.W))

  io.enq.ready := (count =/= depth.U) || io.deq.fire
  io.deq.valid := count =/= 0.U
  io.deq.bits := mem(rptr)
  io.count := count

  when (io.clear) {
    rptr := 0.U
    wptr := 0.U
    count := 0.U
  }.otherwise {
    when (io.enq.fire) {
      mem(wptr) := io.enq.bits
      wptr := wptr + 1.U
    }
    when (io.deq.fire) {
      rptr := rptr + 1.U
    }
    when (io.enq.fire =/= io.deq.fire) {
      count := count + Mux(io.enq.fire, 1.U, (-1).S(countWidth.W).asUInt)
    }
  }
}
class TraceEncoderController(addr: BigInt, beatBytes: Int, hartId: Int,
  pulpConfig: Boolean = false)(implicit p: Parameters) extends LazyModule {
  val traceSpiNode = BundleBridgeSource(() => new TraceSPIStream)

  val device = new SimpleDevice(s"trace-encoder-controller$hartId", Seq("ucbbar,trace"))
  val node = TLRegisterNode(
    address = Seq(AddressSet(addr, if (pulpConfig) 0x7ff else 0xff)),
    device = device,
    beatBytes = beatBytes
  )
  override lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val traceSpi = TraceEncoderController.this.traceSpiNode.bundle
    val io = IO(new Bundle {
      val control = Output(new TraceEncoderControlInterface())
      val trace_in = Flipped(Decoupled(UInt(8.W)))
      val pulp_config = if (pulpConfig) Some(Decoupled(new PulpTraceConfigRequest)) else None
    })

    val pulpPending = if (pulpConfig) Some(RegInit(false.B)) else None
    val pulpAddr = if (pulpConfig) Some(Reg(UInt(8.W))) else None
    val pulpData = if (pulpConfig) Some(Reg(UInt(32.W))) else None
    io.pulp_config.foreach { c =>
      c.valid := pulpPending.get
      c.bits.addr := pulpAddr.get
      c.bits.data := pulpData.get
      when (c.fire) { pulpPending.get := false.B }
    }

    val control_reg_write_valid = Wire(Bool())
    // Trace is opt-in. For tapeout the PULP backend owns spi_0 while enabled.
    val control_reg_bits = RegInit(1.U(2.W))
    val enable = control_reg_bits(1)
    val active = control_reg_bits(0)
    io.control.enable := enable
    traceSpi.enable := enable

    val trace_encoder_impl = RegInit(0.U(32.W))

    val trace_sink_target = RegInit(0.U(TraceSinkTarget.width.W))
    io.control.target := trace_sink_target.asUInt

    val trace_bp_mode = RegInit(0.U(32.W))
    io.control.bp_mode := trace_bp_mode

    val trace_sync_max = RegInit(1024.U(32.W))
    io.control.sync_max := trace_sync_max
    val trace_clear = WireDefault(false.B)
    io.control.clear := trace_clear

    val trace_read = WireDefault(false.B)
    val trace_fifo = if (pulpConfig) None else Some(Module(new TraceByteFifo(1024)))
    if (pulpConfig) {
      traceSpi.tx.valid := io.trace_in.valid
      traceSpi.tx.bits := io.trace_in.bits
      io.trace_in.ready := traceSpi.tx.ready
    } else {
      trace_fifo.get.io.enq <> io.trace_in
      trace_fifo.get.io.clear := trace_clear
      trace_fifo.get.io.deq.ready := trace_read && trace_fifo.get.io.deq.valid
      traceSpi.tx.valid := false.B
      traceSpi.tx.bits := 0.U
    }

    def traceEncoderControlRegWrite(valid: Bool, bits: UInt): Bool = {
      control_reg_write_valid := valid
      when (control_reg_write_valid) {
        control_reg_bits := bits
      }
      true.B
    }

    def traceEncoderControlRegRead(ready: Bool): (Bool, UInt) = {
      (true.B, control_reg_bits)
    }

    val pulpShadow = if (pulpConfig) Some(RegInit(VecInit(Seq.fill(64)(0.U(32.W))))) else None
    val pulpFields = if (pulpConfig) {
      (0 until 64).map { word =>
        val offset = 0x40 + 4 * word
        offset -> Seq(RegField(32, { (_: Bool) => (true.B, pulpShadow.get(word)) }, { (valid: Bool, bits: UInt) =>
          when (valid && !pulpPending.get) {
            // The TL register slot is word-spaced, but rv_tracer's APB
            // address is the byte offset encoded by te_pkg (e.g. 0x03 and
            // 0x15). Preserve the APB offset rather than multiplying it.
            pulpAddr.get := word.U
            pulpData.get := bits
            pulpPending.get := true.B
            pulpShadow.get(word) := bits
          }
          !pulpPending.get
        }, RegFieldDesc(f"pulp_apb_${word * 4}%02x",
          f"PULP rv_tracer APB register 0x${word * 4}%02x")))
      }
    } else Seq.empty

    val regmap = node.regmap(
      (Seq(
        0x00 -> Seq(
          RegField(2, traceEncoderControlRegRead(_), traceEncoderControlRegWrite(_, _),
            RegFieldDesc("control", "Control trace encoder"))
        ),
        0x04 -> Seq(
          RegField.r(32, trace_encoder_impl,
            RegFieldDesc("impl", "Trace encoder implementation"))
        ),
        0x20 -> Seq(
          RegField(8, trace_sink_target,
            RegFieldDesc("target", "Trace transport sink target; independent of E-Trace encapsulation flow"))
        ),
        0x24 -> Seq(
          RegField(32, trace_bp_mode,
            RegFieldDesc("bp_mode", "Trace branch predictor mode"))
        ),
        0x28 -> Seq(
          RegField(32, trace_sync_max,
            RegFieldDesc("sync_max", "Maximum instructions between synchronization packets"))
        ),
        0x2c -> Seq(
          RegField.r(13, if (pulpConfig) Cat(0.U(11.W), enable, active)
            else Cat(trace_fifo.get.io.deq.valid, trace_fifo.get.io.enq.ready, trace_fifo.get.io.count),
            RegFieldDesc("status", "Trace transport status"))
        )) ++ (if (pulpConfig) Seq.empty else Seq(
          0x30 -> Seq(
            RegField.r(8, { ready: Bool =>
              trace_read := ready
              (true.B, trace_fifo.get.io.deq.bits)
            }, RegFieldDesc("data", "Next trace byte; read pops the FIFO"))
          ))) ++ Seq(
        0x34 -> Seq(
          RegField(1, { valid: Bool => (true.B, 0.U(1.W)) }, { (valid: Bool, bits: UInt) =>
            when (valid) { trace_clear := bits(0) }
            true.B
          },
            RegFieldDesc("clear", "Clear all queued trace bytes"))
        ) ) ++ pulpFields):_*
    )
  }
}
