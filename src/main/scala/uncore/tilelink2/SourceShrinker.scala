// See LICENSE.SiFive for license details.

package uncore.tilelink2

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import config._
import diplomacy._
import util._
import scala.math.{min,max}

class TLSourceShrinker(maxInFlight: Int)(implicit p: Parameters) extends LazyModule
{
  require (maxInFlight > 0)

  private val client = TLClientParameters(sourceId = IdRange(0, maxInFlight))
  val node = TLAdapterNode(
    // We erase all client information since we crush the source Ids
    clientFn  = { _ => TLClientPortParameters(clients = Seq(client)) },
    managerFn = { mp => mp })

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in  = node.bundleIn
      val out = node.bundleOut
    }

    ((io.in zip io.out) zip (node.edgesIn zip node.edgesOut)) foreach { case ((in, out), (edgeIn, edgeOut)) =>
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

        in.d <> out.d
        in.d.bits.source := sourceIdMap(out.d.bits.source)

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
  // applied to the TL source node; y.node := TLSourceShrinker(n)(x.node)
  def apply(maxInFlight: Int)(x: TLOutwardNode)(implicit p: Parameters, sourceInfo: SourceInfo): TLOutwardNode = {
    val shrinker = LazyModule(new TLSourceShrinker(maxInFlight))
    shrinker.node := x
    shrinker.node
  }
}
