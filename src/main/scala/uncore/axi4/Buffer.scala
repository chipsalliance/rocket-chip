// See LICENSE.SiFive for license details.

package uncore.axi4

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import diplomacy._
import scala.math.max

// pipe is only used if a queue has depth = 1
class AXI4Buffer(aw: Int = 2, w: Int = 2, b: Int = 2, ar: Int = 2, r: Int = 2, pipe: Boolean = true) extends LazyModule
{
  require (aw >= 0)
  require (w  >= 0)
  require (b  >= 0)
  require (ar >= 0)
  require (r  >= 0)

  val node = AXI4IdentityNode()

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in  = node.bundleIn
      val out = node.bundleOut
    }

    ((io.in zip io.out) zip (node.edgesIn zip node.edgesOut)) foreach { case ((in, out), (edgeIn, edgeOut)) =>
      if (aw>0) { out.aw <> Queue(in .aw, aw, pipe && aw<2) } else { out.aw <> in .aw }
      if (w >0) { out.w  <> Queue(in .w,  w,  pipe && w <2) } else { out.w  <> in .w  }
      if (b >0) { in .b  <> Queue(out.b,  b,  pipe && b <2) } else { in .b  <> out.b  }
      if (ar>0) { out.ar <> Queue(in .ar, ar, pipe && ar<2) } else { out.ar <> in .ar }
      if (r >0) { in .r  <> Queue(out.r,  r,  pipe && r <2) } else { in .r  <> out.r  }
    }
  }
}

object AXI4Buffer
{
  // applied to the AXI4 source node; y.node := AXI4Buffer(x.node)
  def apply()                               (x: AXI4OutwardNode)(implicit sourceInfo: SourceInfo): AXI4OutwardNode = apply(2)(x)
  def apply(entries: Int)                   (x: AXI4OutwardNode)(implicit sourceInfo: SourceInfo): AXI4OutwardNode = apply(entries, true)(x)
  def apply(entries: Int, pipe: Boolean)    (x: AXI4OutwardNode)(implicit sourceInfo: SourceInfo): AXI4OutwardNode = apply(entries, entries, pipe)(x)
  def apply(aw: Int, br: Int)               (x: AXI4OutwardNode)(implicit sourceInfo: SourceInfo): AXI4OutwardNode = apply(aw, br, true)(x)
  def apply(aw: Int, br: Int, pipe: Boolean)(x: AXI4OutwardNode)(implicit sourceInfo: SourceInfo): AXI4OutwardNode = apply(aw, aw, br, aw, br, pipe)(x)
  def apply(aw: Int, w: Int, b: Int, ar: Int, r: Int, pipe: Boolean = true)(x: AXI4OutwardNode)(implicit sourceInfo: SourceInfo): AXI4OutwardNode = {
    val buffer = LazyModule(new AXI4Buffer(aw, w, b, ar, r, pipe))
    buffer.node := x
    buffer.node
  }
}
