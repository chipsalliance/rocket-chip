// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import diplomacy._

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

    if (supportManagers && !(passthrough && smartManagers)) {
      val address = edgeIn.address(in.a.bits)
      val handleA = if (passthrough) !edgeOut.manager.supportsHintFast(address, edgeIn.size(in.a.bits)) else Bool(true)
      val hintBitsAtA = handleA && in.a.bits.opcode === TLMessages.Hint
      val hint = Wire(out.d)

      hint.valid  := in.a.valid &&  hintBitsAtA
      out.a.valid := in.a.valid && !hintBitsAtA
      in.a.ready := Mux(hintBitsAtA, hint.ready, out.a.ready)

      hint.bits := edgeIn.HintAck(in.a.bits, edgeOut.manager.findIdStartFast(address))
      out.a.bits := in.a.bits

      TLArbiter(TLArbiter.lowestIndexFirst)(in.d, (edgeOut.numBeats(out.d.bits), out.d), (UInt(1), hint))
    } else {
      out.a.valid := in.a.valid
      in.a.ready := out.a.ready
      out.a.bits := in.a.bits

      in.d.valid := out.d.valid
      out.d.ready := in.d.ready
      in.d.bits := out.d.bits
    }

    if (supportClients && !(passthrough && smartClients)) {
      val handleB = if (passthrough) !edgeIn.client.supportsHint(out.b.bits.source, edgeOut.size(out.b.bits)) else Bool(true)
      val hintBitsAtB = handleB && out.b.bits.opcode === TLMessages.Hint
      val hint = Wire(in.c)

      hint.valid := out.b.valid &&  hintBitsAtB
      in.b.valid := out.b.valid && !hintBitsAtB
      out.b.ready := Mux(hintBitsAtB, hint.ready, in.b.ready)

      hint.bits := edgeOut.HintAck(out.b.bits)
      in.b.bits := out.b.bits

      TLArbiter(TLArbiter.lowestIndexFirst)(out.c, (edgeIn.numBeats(in.c.bits), in.c), (UInt(1), hint))
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
