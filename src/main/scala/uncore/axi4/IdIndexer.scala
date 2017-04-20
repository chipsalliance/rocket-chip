// See LICENSE.SiFive for license details.

package uncore.axi4

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import config._
import diplomacy._
import scala.math.{min,max}

class AXI4IdIndexer(idBits: Int)(implicit p: Parameters) extends LazyModule
{
  require (idBits >= 0)

  val node = AXI4AdapterNode(
    masterFn = { mp => mp.copy(
      userBits = mp.userBits + max(0, log2Ceil(mp.endId) - idBits),
      masters  = Seq(AXI4MasterParameters(
        id      = IdRange(0, min(mp.endId, 1 << idBits)),
        aligned = mp.masters.map(_.aligned).reduce(_ && _))))
    },
    slaveFn = { sp => sp.copy(
      slaves = sp.slaves.map(s => s.copy(
        interleavedId = if (idBits == 0) Some(0) else s.interleavedId)))
    })

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in  = node.bundleIn
      val out = node.bundleOut
    }

    ((io.in zip io.out) zip (node.edgesIn zip node.edgesOut)) foreach { case ((in, out), (edgeIn, edgeOut)) =>

      // Leave everything mostly untouched
      out.ar <> in.ar
      out.aw <> in.aw
      out.w <> in.w
      in.b <> out.b
      in.r <> out.r

      val bits = log2Ceil(edgeIn.master.endId) - idBits
      if (bits > 0) {
       out.ar.bits.user.get := Cat(in.ar.bits.user.toList ++ Seq(in.ar.bits.id >> idBits))
       out.aw.bits.user.get := Cat(in.aw.bits.user.toList ++ Seq(in.aw.bits.id >> idBits))
       in.r.bits.user.foreach { _ := out.r.bits.user.get >> bits }
       in.b.bits.user.foreach { _ := out.b.bits.user.get >> bits }
       in.r.bits.id := Cat(out.r.bits.user.get, out.r.bits.id)
       in.b.bits.id := Cat(out.b.bits.user.get, out.b.bits.id)
      }
    }
  }
}

object AXI4IdIndexer
{
  // applied to the AXI4 source node; y.node := AXI4IdIndexer(idBits)(x.node)
  def apply(idBits: Int)(x: AXI4OutwardNode)(implicit p: Parameters, sourceInfo: SourceInfo): AXI4OutwardNode = {
    val indexer = LazyModule(new AXI4IdIndexer(idBits))
    indexer.node := x
    indexer.node
  }
}
