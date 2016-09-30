// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo

// Acks Hints for managers that don't support them or Acks all Hints if !passthrough
class TLHintHandler(supportManagers: Boolean = true, supportClients: Boolean = false, passthrough: Boolean = true) extends LazyModule
{
  // HintAcks can come back combinationally => minLatency=0
  val node = TLAdapterNode(
    clientFn  = { case Seq(c) => if (!supportClients)  c else c.copy(minLatency = 0, clients  = c.clients .map(_.copy(supportsHint = TransferSizes(1, c.maxTransfer)))) },
    managerFn = { case Seq(m) => if (!supportManagers) m else m.copy(minLatency = 0, managers = m.managers.map(_.copy(supportsHint = TransferSizes(1, m.maxTransfer)))) })

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in  = node.bundleIn
      val out = node.bundleOut
    }

    val in  = io.in(0)
    val out = io.out(0)
    val edgeIn  = node.edgesIn(0)
    val edgeOut = node.edgesOut(0)

    // Don't add support for clients if there is no BCE channel
    val bce = edgeOut.manager.anySupportAcquire && edgeIn.client.anySupportProbe
    require (!supportClients || bce)

    // Does it even make sense to add the HintHandler?
    val smartClients = edgeIn.client.clients.map(_.supportsHint.max == edgeIn.client.maxTransfer).reduce(_&&_)
    val smartManagers = edgeOut.manager.managers.map(_.supportsHint.max == edgeOut.manager.maxTransfer).reduce(_&&_)

    if (supportManagers && !smartManagers) {
      // State of the Hint bypass
      val counter = RegInit(UInt(0, width = log2Up(edgeOut.manager.maxTransfer/edgeOut.manager.beatBytes)))
      val hintHoldsD = RegInit(Bool(false))
      val outerHoldsD = counter =/= UInt(0)
      // Only one of them can hold it
      assert (!hintHoldsD || !outerHoldsD)

      // Count outer D beats
      val beats1 = edgeOut.numBeats1(out.d.bits)
      when (out.d.fire()) { counter := Mux(outerHoldsD, counter - UInt(1), beats1) }

      // Who wants what?
      val address = edgeIn.address(in.a.bits)
      val handleA = if (passthrough) !edgeOut.manager.supportsHintFast(address, edgeIn.size(in.a.bits)) else Bool(true)
      val hintBitsAtA = handleA && in.a.bits.opcode === TLMessages.Hint
      val hintWantsD = in.a.valid && hintBitsAtA
      val outerWantsD = out.d.valid

      // Prioritize existing D traffic over HintAck (and finish multibeat xfers)
      val hintWinsD = hintHoldsD || (!outerHoldsD && !outerWantsD)
      hintHoldsD := hintWantsD && hintWinsD && !in.d.ready
      // Hint can only hold D b/c it still wants it from last cycle
      assert (!hintHoldsD || hintWantsD)

      in.d.valid  := Mux(hintWinsD, hintWantsD, outerWantsD)
      in.d.bits   := Mux(hintWinsD, edgeIn.HintAck(in.a.bits, edgeOut.manager.findIdStartFast(address)), out.d.bits)
      out.d.ready := in.d.ready && !hintHoldsD

      in.a.ready  := Mux(hintBitsAtA, hintWinsD && in.d.ready, out.a.ready)
      out.a.valid := in.a.valid && !hintBitsAtA
      out.a.bits  := in.a.bits
    } else {
      out.a.valid := in.a.valid
      in.a.ready := out.a.ready
      out.a.bits := in.a.bits

      in.d.valid := out.d.valid
      out.d.ready := in.d.ready
      in.d.bits := out.d.bits
    }

    if (supportClients && !smartClients) {
      // State of the Hint bypass
      val counter = RegInit(UInt(0, width = log2Up(edgeIn.client.maxTransfer/edgeIn.manager.beatBytes)))
      val hintHoldsC = RegInit(Bool(false))
      val innerHoldsC = counter =/= UInt(0)
      // Only one of them can hold it
      assert (!hintHoldsC || !innerHoldsC)

      // Count inner C beats
      val beats1 = edgeIn.numBeats1(in.c.bits)
      when (in.c.fire()) { counter := Mux(innerHoldsC, counter - UInt(1), beats1) }

      // Who wants what?
      val handleB = if (passthrough) !edgeIn.client.supportsHint(out.b.bits.source, edgeOut.size(out.b.bits)) else Bool(true)
      val hintBitsAtB = handleB && out.b.bits.opcode === TLMessages.Hint
      val hintWantsC = out.b.valid && hintBitsAtB
      val innerWantsC = in.c.valid

      // Prioritize existing C traffic over HintAck (and finish multibeat xfers)
      val hintWinsC = hintHoldsC || (!innerHoldsC && !innerWantsC)
      hintHoldsC := hintWantsC && hintWinsC && !out.c.ready
      // Hint can only hold C b/c it still wants it from last cycle
      assert (!hintHoldsC || hintWantsC)

      out.c.valid := Mux(hintWinsC, hintWantsC, innerWantsC)
      out.c.bits  := Mux(hintWinsC, edgeOut.HintAck(out.b.bits), in.c.bits)
      in.c.ready  := out.c.ready && !hintHoldsC

      out.b.ready := Mux(hintBitsAtB, hintWinsC && out.c.ready, in.b.ready)
      in.b.valid  := out.b.valid && !hintBitsAtB
      in.b.bits   := out.b.bits
    } else if (bce) {
      in.b.valid := out.b.valid
      out.b.ready := in.b.ready
      in.b.bits := out.b.bits

      out.c.valid := in.c.valid
      in.c.ready := out.c.ready
      out.c.bits := in.c.bits
    } else {
      in.b.valid := Bool(false)
      in.c.ready := Bool(true)
      out.b.ready := Bool(true)
      out.c.valid := Bool(false)
    }

    if (bce) {
      // Pass E through unchanged
      out.e.valid := in.e.valid
      in.e.ready := out.e.ready
      out.e.bits := in.e.bits
    } else {
      in.e.ready := Bool(true)
      out.e.valid := Bool(false)
    }
  }
}

object TLHintHandler
{
  // applied to the TL source node; y.node := TLHintHandler(x.node)
  def apply(supportManagers: Boolean = true, supportClients: Boolean = false, passthrough: Boolean = true)(x: TLOutwardNode)(implicit sourceInfo: SourceInfo): TLOutwardNode = {
    val hints = LazyModule(new TLHintHandler(supportManagers, supportClients, passthrough))
    hints.node := x
    hints.node
  }
}

/** Synthesizeable unit tests */
import unittest._

//TODO ensure handler will pass through hints to clients that can handle them themselves

class TLRAMHintHandler() extends LazyModule {
  val fuzz = LazyModule(new TLFuzzer(5000))
  val model = LazyModule(new TLRAMModel)
  val ram  = LazyModule(new TLRAM(AddressSet(0x0, 0x3ff)))

  model.node := fuzz.node
  ram.node := TLFragmenter(4, 256)(TLHintHandler()(model.node))

  lazy val module = new LazyModuleImp(this) with HasUnitTestIO {
    io.finished := fuzz.module.io.finished
  }
}

class TLRAMHintHandlerTest extends UnitTest(timeout = 500000) {
  io.finished := Module(LazyModule(new TLRAMHintHandler).module).io.finished
}
