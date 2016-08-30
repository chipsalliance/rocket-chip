// See LICENSE for license details.

package uncore.tilelink2

import Chisel._

// Acks Hints for managers that don't support them or Acks all Hints if !passthrough
class TLHintHandler(passthrough: Boolean = true) extends TLSimpleFactory
{
  val node = TLAdapterNode(
    clientFn  = { case Seq(c) => c.copy(clients  = c.clients .map(_.copy(supportsHint = true))) },
    managerFn = { case Seq(m) => m.copy(managers = m.managers.map(_.copy(supportsHint = true))) })

  lazy val module = Module(new TLModule(this) {
    val io = new Bundle {
      val in  = node.bundleIn
      val out = node.bundleOut
    }

    val in  = io.in(0)
    val out = io.out(0)
    val edgeIn  = node.edgesIn(0)
    val edgeOut = node.edgesOut(0)

    val handleA = if (passthrough) !edgeOut.manager.supportsHint(in.a.bits.address) else Bool(true)
    val bypassD = handleA && in.a.bits.opcode === TLMessages.Hint

    // Prioritize existing D traffic over HintAck
    in.d.valid  := out.d.valid || (bypassD && in.a.valid)
    out.d.ready := in.d.ready
    in.d.bits   := Mux(out.d.valid, out.d.bits, edgeIn.HintAck(in.a.bits.source, in.a.bits.size))

    in.a.ready  := Mux(bypassD, in.d.ready && !out.d.valid, out.a.ready)
    out.a.valid := in.a.valid && !bypassD
    out.a.bits  := in.a.bits
    
    val handleB = if (passthrough) !edgeIn.client.supportsHint(out.b.bits.source) else Bool(true)
    val bypassC = handleB && out.b.bits.opcode === TLMessages.Hint

    // Prioritize existing C traffic over HintAck
    out.c.valid := in.c.valid || (bypassC && in.b.valid)
    in.c.ready  := out.c.ready
    out.c.bits  := Mux(in.c.valid, in.c.bits, edgeOut.HintAck(out.b.bits.address, out.b.bits.size))
    
    out.b.ready := Mux(bypassC, out.c.ready && !in.c.valid, in.b.ready)
    in.b.valid  := out.b.valid && !bypassC
    in.b.bits   := out.b.bits

    // Pass E through unchanged
    out.e.valid := in.e.valid
    in.e.ready := out.e.ready
    out.e.bits := in.e.bits
  })
}
