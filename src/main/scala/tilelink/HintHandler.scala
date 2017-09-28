// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import scala.math.min

// Acks Hints for managers that don't support them or Acks all Hints if !passthrough
class TLHintHandler(supportManagers: Boolean = true, supportClients: Boolean = false, passthrough: Boolean = true)(implicit p: Parameters) extends LazyModule
{
  val node = TLAdapterNode(
    clientFn  = { c => if (!supportClients)  c else c.copy(minLatency = min(1, c.minLatency), clients  = c.clients .map(_.copy(supportsHint = TransferSizes(1, c.maxTransfer)))) },
    managerFn = { m => if (!supportManagers) m else m.copy(minLatency = min(1, m.minLatency), managers = m.managers.map(_.copy(supportsHint = TransferSizes(1, m.maxTransfer)))) })

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      // Don't add support for clients if there is no BCE channel
      val bce = edgeOut.manager.anySupportAcquireB && edgeIn.client.anySupportProbe
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

        hint.bits := edgeIn.HintAck(in.a.bits)
        out.a.bits := in.a.bits

        TLArbiter(TLArbiter.lowestIndexFirst)(in.d, (edgeOut.numBeats1(out.d.bits), out.d), (UInt(0), Queue(hint, 1)))
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

        TLArbiter(TLArbiter.lowestIndexFirst)(out.c, (edgeIn.numBeats1(in.c.bits), in.c), (UInt(0), Queue(hint, 1)))
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
}

object TLHintHandler
{
  // applied to the TL source node; y.node := TLHintHandler(x.node)
  def apply(supportManagers: Boolean = true, supportClients: Boolean = false, passthrough: Boolean = true)(x: TLOutwardNode)(implicit p: Parameters, sourceInfo: SourceInfo): TLOutwardNode = {
    val hints = LazyModule(new TLHintHandler(supportManagers, supportClients, passthrough))
    hints.node :=? x
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

  model.node := fuzz.node
  ram.node := TLFragmenter(4, 256)(TLDelayer(0.1)(TLHintHandler()(TLDelayer(0.1)(model.node))))

  lazy val module = new LazyModuleImp(this) with UnitTestModule {
    io.finished := fuzz.module.io.finished
  }
}

class TLRAMHintHandlerTest(txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  io.finished := Module(LazyModule(new TLRAMHintHandler(txns)).module).io.finished
}
