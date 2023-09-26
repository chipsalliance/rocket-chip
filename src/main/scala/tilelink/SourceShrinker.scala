// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class TLSourceShrinker(maxInFlight: Int)(implicit p: Parameters) extends LazyModule
{
  require (maxInFlight > 0)
  private def noShrinkRequired(client: TLClientPortParameters) = maxInFlight >= client.endSourceId

  // The SourceShrinker completely destroys all FIFO property guarantees
  private val client = TLMasterParameters.v1(
    name     = "TLSourceShrinker",
    sourceId = IdRange(0, maxInFlight))
  val node = (new TLAdapterNode(
    clientFn  = { cp => if (noShrinkRequired(cp)) { cp } else {
      // We erase all client information since we crush the source Ids
      TLMasterPortParameters.v1(
        clients = Seq(client.v1copy(requestFifo = cp.clients.exists(_.requestFifo))),
        echoFields = cp.echoFields,
        requestFields = cp.requestFields,
        responseKeys = cp.responseKeys)
    }},
    managerFn = { mp => mp.v1copy(managers = mp.managers.map(m => m.v1copy(fifoId = if (maxInFlight==1) Some(0) else m.fifoId)))
    }) {
    override def circuitIdentity = edges.in.map(_.client).forall(noShrinkRequired)
})

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    node.in.zip(node.out).foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out.b.ready := true.B
      out.c.valid := false.B
      out.e.valid := false.B
      in.b.valid := false.B
      in.c.ready := true.B
      in.e.ready := true.B

      if (noShrinkRequired(edgeIn.client)) {
        out.a <> in.a
        in.d <> out.d
      } else {
        // Acquires cannot pass this adapter; it makes Probes impossible
        require (!edgeIn.client.anySupportProbe ||
                 !edgeOut.manager.anySupportAcquireB)

        // State tracking
        val sourceIdMap = Mem(maxInFlight, UInt(edgeIn.bundle.sourceBits.W))
        val allocated = RegInit(0.U(maxInFlight.W))
        val nextFreeOH = ~(leftOR(~allocated) << 1) & ~allocated
        val nextFree = OHToUInt(nextFreeOH)
        val full = allocated.andR

        val a_first = edgeIn.first(in.a)
        val d_last  = edgeIn.last(in.d)

        val block = a_first && full
        in.a.ready := out.a.ready && !block
        out.a.valid := in.a.valid && !block
        out.a.bits := in.a.bits
        out.a.bits.source := nextFree holdUnless a_first

        val bypass = (edgeOut.manager.minLatency == 0).B && in.a.valid && !full && a_first && nextFree === out.d.bits.source
        in.d <> out.d
        in.d.bits.source := Mux(bypass, in.a.bits.source, sourceIdMap(out.d.bits.source))

        when (a_first && in.a.fire) {
          sourceIdMap(nextFree) := in.a.bits.source
        }

        val alloc = a_first && in.a.fire
        val free = d_last && in.d.fire
        val alloc_id = Mux(alloc, nextFreeOH, 0.U)
        val free_id = Mux(free, UIntToOH(out.d.bits.source), 0.U)
        allocated := (allocated | alloc_id) & ~free_id
      }
    }
  }
}

object TLSourceShrinker
{
  def apply(maxInFlight: Int)(implicit p: Parameters): TLNode =
  {
    val shrinker = LazyModule(new TLSourceShrinker(maxInFlight))
    shrinker.node
  }
}
