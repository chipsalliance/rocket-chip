// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import diplomacy._
import scala.math.{min,max}

class TLSourceShrinker(maxInFlight: Int) extends LazyModule
{
  private val client = TLClientParameters(sourceId = IdRange(0, maxInFlight))
  val node = TLAdapterNode(
    // We erase all client information since we crush the source Ids
    clientFn  = { case _ => TLClientPortParameters(clients = Seq(client)) },
    managerFn = { case Seq(mp) => mp })

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in  = node.bundleIn
      val out = node.bundleOut
    }

    val edgeIn = node.edgesIn(0)
    val edgeOut = node.edgesOut(0)
    val in = io.in(0)
    val out = io.out(0)

    // Acquires cannot pass this adapter; it makes Probes impossible
    require (!edgeIn.client.anySupportProbe || 
             !edgeOut.manager.anySupportAcquire)

    out.b.ready := Bool(true)
    out.c.valid := Bool(false)
    out.e.valid := Bool(false)
    in.b.valid := Bool(false)
    in.c.ready := Bool(true)
    in.e.ready := Bool(true)

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
    out.a.bits.source := holdUnless(nextFree, a_first)

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

object TLSourceShrinker
{
  // applied to the TL source node; y.node := TLSourceShrinker(n)(x.node)
  def apply(maxInFlight: Int)(x: TLOutwardNode)(implicit sourceInfo: SourceInfo): TLOutwardNode = {
    val shrinker = LazyModule(new TLSourceShrinker(maxInFlight))
    shrinker.node := x
    shrinker.node
  }
}
