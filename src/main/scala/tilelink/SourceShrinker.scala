// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import scala.math.{min,max}

class TLSourceShrinker(maxInFlight: Int)(implicit p: Parameters) extends LazyModule
{
  require (maxInFlight > 0)

  // The SourceShrinker completely destroys all FIFO property guarantees
  private val client = TLClientParameters(
    name     = "TLSourceShrinker",
    sourceId = IdRange(0, maxInFlight))
  val node = TLAdapterNode(
    // We erase all client information since we crush the source Ids
    clientFn  = { cp => TLClientPortParameters(clients = Seq(client.copy(requestFifo = cp.clients.exists(_.requestFifo)))) },
    managerFn = { mp => mp.copy(managers = mp.managers.map(m => m.copy(fifoId = if (maxInFlight==1) Some(0) else m.fifoId)))  })

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      // Acquires cannot pass this adapter; it makes Probes impossible
      require (!edgeIn.client.anySupportProbe || 
               !edgeOut.manager.anySupportAcquireB)

      out.b.ready := Bool(true)
      out.c.valid := Bool(false)
      out.e.valid := Bool(false)
      in.b.valid := Bool(false)
      in.c.ready := Bool(true)
      in.e.ready := Bool(true)

      if (maxInFlight >= edgeIn.client.endSourceId) {
        out.a <> in.a
        in.d <> out.d
      } else {
        // State tracking
        val sourceIdMap = Mem(maxInFlight, in.a.bits.source)
        val allocated = RegInit(UInt(0, width = maxInFlight))
        val nextFreeOH = ~(leftOR(~allocated) << 1) & ~allocated
        val nextFree = OHToUInt(nextFreeOH)
        val full = allocated.andR()

        val a_first = edgeIn.first(in.a)
        val d_last  = edgeIn.last(in.d)

        val block = a_first && full
        in.a.ready := out.a.ready && !block
        out.a.valid := in.a.valid && !block
        out.a.bits := in.a.bits
        out.a.bits.source := nextFree holdUnless a_first

        val bypass = Bool(edgeOut.manager.minLatency == 0) && in.a.valid && !full && a_first && nextFree === out.d.bits.source
        in.d <> out.d
        in.d.bits.source := Mux(bypass, in.a.bits.source, sourceIdMap(out.d.bits.source))

        when (a_first && in.a.fire()) {
          sourceIdMap(nextFree) := in.a.bits.source
        }

        val alloc = a_first && in.a.fire()
        val free = d_last && in.d.fire()
        val alloc_id = Mux(alloc, nextFreeOH, UInt(0))
        val free_id = Mux(free, UIntToOH(out.d.bits.source), UInt(0))
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
