// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util.Repeater
import freechips.rocketchip.devices.tilelink.TLROM
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
        supportsHint =
          if (m.supportsHint && passthrough) m.supportsHint
          else if (m.supportsPutPartial) m.supportsPutPartial
          else if (m.regionType != RegionType.GET_EFFECTS) m.supportsGet
          else TransferSizes.none)})})

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out <> in

      // Confirm we have added Hint support
      edgeIn.manager.managers.foreach { m =>
        require (m.supportsHint, s"No legal way to implement Hints for ${m.name}")
      }

      val isHint = in.a.bits.opcode === TLMessages.Hint
      def usePP (m: TLManagerParameters) = !(passthrough && m.supportsHint) && m.supportsPutPartial
      def useGet(m: TLManagerParameters) = !(passthrough && m.supportsHint) && !m.supportsPutPartial

      // Does the HintHandler help using PutPartial with this message?
      val helpPP = isHint && edgeOut.manager.fastProperty(in.a.bits.address, usePP, (b:Boolean) => b.B)
      val mapPP = WireInit(helpPP)

      // What about Get?
      val mapGet = isHint && edgeOut.manager.fastProperty(in.a.bits.address, useGet, (b:Boolean) => b.B)

      // To handle multi-beat Hints using PutPartial, we need to extend the Hint when transforming A
      // However, when used between a device (SRAM/RegisterRouter/etc) and a Fragmenter, this is not needed
      val needRepeater = edgeOut.manager.managers.exists { m =>
        !(passthrough && m.supportsHint) && m.supportsPutPartial.max > edgeOut.manager.beatBytes
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

        mapPP := repeater.io.full || helpPP
        mux
      }

      // Transform Hint to PutPartialData
      out.a.bits.opcode := Mux(mapPP, TLMessages.PutPartialData, Mux(mapGet, TLMessages.Get, a.bits.opcode))
      out.a.bits.param  := Mux(mapPP | mapGet, 0.U, a.bits.param)
      out.a.bits.mask   := Mux(mapPP, 0.U, a.bits.mask)
      out.a.bits.source := a.bits.source << 1 | (mapPP|mapGet)

      // To handle multi-beat Hints using Get, we need to drop the AccessAckData when transforming D
      val needsDrop = edgeOut.manager.managers.exists { m =>
        !(passthrough && m.supportsHint) && !m.supportsPutPartial &&
        m.supportsGet.max > edgeOut.manager.beatBytes
      }

      val transform = out.d.bits.source(0)
      val drop = if (!needsDrop) false.B else {
        // We don't need to care about if it was a Get or PP; last works for both
        val last = edgeOut.last(out.d)
        !last && transform
      }

      // Transform AccessAck[Data] to HintAck
      in.d.bits.source := out.d.bits.source >> 1
      in.d.bits.opcode := Mux(transform, TLMessages.HintAck, out.d.bits.opcode)
      in.d.valid := out.d.valid && !drop
      out.d.ready := in.d.ready ||  drop

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
  val ram1  = LazyModule(new TLRAM(AddressSet(0x0,   0x3ff)))
  val ram2  = LazyModule(new TLRAM(AddressSet(0x400, 0x3ff)))
  val rom   = LazyModule(new TLROM(0x800, 0x400, Seq.fill(128) { 0 }))
  val xbar  = LazyModule(new TLXbar)

  (ram1.node
    := TLDelayer(0.1)
    := TLHintHandler() // should have no state (not multi-beat)
    := TLDelayer(0.1)
    := TLHintHandler() // should have no logic
    := TLDelayer(0.1)
    := TLFragmenter(4, 64)
    := xbar.node)
  (ram2.node
    := TLFragmenter(4, 64) // should cause HintHandler to use multi-beat Put
    := TLDelayer(0.1)
    := xbar.node)
  (rom.node
    := TLFragmenter(4, 64) // should cause HintHandler to use multi-beat Get
    := xbar.node)
  (xbar.node
    := TLDelayer(0.1)
    := TLHintHandler() // multi-beat with Get, PutPartial, and passthrough
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
