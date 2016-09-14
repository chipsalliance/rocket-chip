// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo

class TLBuffer(entries: Int = 2, pipe: Boolean = false) extends LazyModule
{
  val node = TLIdentityNode()

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in  = node.bundleIn
      val out = node.bundleOut
    }
    
    ((io.in zip io.out) zip (node.edgesIn zip node.edgesOut)) foreach { case ((in, out), (edgeIn, edgeOut)) =>
      out.a <> Queue(in .a, entries, pipe)
      in .d <> Queue(out.d, entries, pipe)

      if (edgeOut.manager.anySupportAcquire && edgeOut.client.anySupportProbe) {
        in .b <> Queue(out.b, entries, pipe)
        out.c <> Queue(in .c, entries, pipe)
        out.e <> Queue(in .e, entries, pipe)
      } else {
        in.b.valid := Bool(false)
        in.c.ready := Bool(true)
        in.e.ready := Bool(true)
        out.b.ready := Bool(true)
        out.c.valid := Bool(false)
        out.e.valid := Bool(false)
      }
    }
  }
}

object TLBuffer
{
  // applied to the TL source node; y.node := TLBuffer(x.node)
  def apply(x: TLBaseNode, entries: Int = 2, pipe: Boolean = false)(implicit sourceInfo: SourceInfo): TLBaseNode = {
    val buffer = LazyModule(new TLBuffer(entries, pipe))
    buffer.node := x
    buffer.node
  }
}
