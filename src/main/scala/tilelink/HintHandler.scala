// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util.Repeater
import scala.math.min

// Acks Hints for managers that don't support them or Acks all Hints if !passthrough
class TLHintHandler(passthrough: Boolean = true)(implicit p: Parameters) extends LazyModule
{
  val node = TLAdapterNode(
    clientFn = { cp =>
      cp.copy(clients = cp.clients.map { c => c.copy(
        sourceId = IdRange(c.sourceId.start*2, c.sourceId.end*2))})},
    managerFn = { mp =>
      mp.copy(managers = mp.managers.map { m => m.copy(
        supportsHint = if (m.supportsHint && passthrough) m.supportsHint else m.supportsPutPartial)})})

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out <> in

      // Does the HintHandler help with this message?
      val help = in.a.bits.opcode === TLMessages.Hint && (!passthrough.B ||
        edgeOut.manager.fastProperty(in.a.bits.address, _.supportsHint.none, (b:Boolean) => b.B))
      val mapPP = WireInit(help)

      // To handle multi-beat Hints, we need to extend the Hint when creating a PutPartial
      // However, when used between a device (SRAM/RegisterRouter/etc) and a Fragmenter, this is not needed
      val needRepeater = (edgeIn.manager.managers zip edgeOut.manager.managers) exists { case (in, out) =>
        !(out.supportsHint && passthrough) && out.supportsPutPartial.max > edgeOut.manager.beatBytes
      }

      val a = if (!needRepeater) in.a else {
        val repeater = Module(new Repeater(in.a.bits))
        val mux = Wire(chiselTypeOf(in.a))

        repeater.io.repeat := mapPP && !edgeIn.last(out.a)
        repeater.io.enq <> in.a
        // Work-around broken chisel3 <>
        out.a.bits := mux.bits
        out.a.valid := mux.valid
        mux.ready := out.a.ready

        // Only some signals need to be repeated
        mux.bits.opcode  := in.a.bits.opcode  // ignored when full
        mux.bits.param   := in.a.bits.param   // ignored when full
        mux.bits.size    := repeater.io.deq.bits.size
        mux.bits.source  := repeater.io.deq.bits.source
        mux.bits.address := repeater.io.deq.bits.address
        mux.bits.data    := in.a.bits.data    // irrelevant when full (mask = 0)
        mux.bits.mask    := in.a.bits.mask    // ignored when full
        mux.bits.corrupt := in.a.bits.corrupt // irrelevant when full (mask = 0)

        mux.valid := repeater.io.deq.valid
        repeater.io.deq.ready := mux.ready

        mapPP := repeater.io.full || help
        mux
      }

      // Transform Hint to PutPartialData
      out.a.bits.opcode := Mux(mapPP, TLMessages.PutPartialData, a.bits.opcode)
      out.a.bits.param  := Mux(mapPP, 0.U, a.bits.param)
      out.a.bits.mask   := Mux(mapPP, 0.U, a.bits.mask)
      out.a.bits.source := a.bits.source << 1 | mapPP

      // Transform AccessAck to HintAck
      in.d.bits.source := out.d.bits.source >> 1
      in.d.bits.opcode := Mux(out.d.bits.source(0), TLMessages.HintAck, out.d.bits.opcode)

      if (edgeOut.manager.anySupportAcquireB && edgeIn.client.anySupportProbe) {
        in.b.bits.source := out.b.bits.source >> 1
        out.c.bits.source := in.c.bits.source << 1
      }
    }
  }
}

object TLHintHandler
{
  def apply(passthrough: Boolean = true)(implicit p: Parameters): TLNode =
  {
    val hints = LazyModule(new TLHintHandler(passthrough))
    hints.node
  }
}

/** Synthesizeable unit tests */
import freechips.rocketchip.unittest._

//TODO ensure handler will pass through hints to clients that can handle them themselves

class TLRAMHintHandler(txns: Int)(implicit p: Parameters) extends LazyModule {
  val fuzz = LazyModule(new TLFuzzer(txns))
  val model = LazyModule(new TLRAMModel("HintHandler"))
  val ram  = LazyModule(new TLRAM(AddressSet(0x0, 0x3ff)))

  (ram.node
    := TLFragmenter(4, 256)
    := TLDelayer(0.1)
    := TLHintHandler()
    := TLDelayer(0.1)
    := model.node
    := fuzz.node)

  lazy val module = new LazyModuleImp(this) with UnitTestModule {
    io.finished := fuzz.module.io.finished
  }
}

class TLRAMHintHandlerTest(txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  val dut = Module(LazyModule(new TLRAMHintHandler(txns)).module)
  io <> dut.io
}
