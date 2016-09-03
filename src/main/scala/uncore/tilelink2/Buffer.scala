// See LICENSE for license details.

package uncore.tilelink2

import Chisel._

class TLBuffer(entries: Int = 2, pipe: Boolean = false) extends LazyModule
{
  val node = TLIdentityNode()

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in  = node.bundleIn
      val out = node.bundleOut
    }

    val in  = io.in(0)
    val out = io.out(0)

    out.a <> Queue(in .a, entries, pipe)
    in .d <> Queue(out.d, entries, pipe)
    
    val edge = node.edgesOut(0) // same as edgeIn(0)
    if (edge.manager.anySupportAcquire && edge.client.anySupportProbe) {
      in .b <> Queue(out.b, entries, pipe)
      out.c <> Queue(in .c, entries, pipe)
      out.e <> Queue(out.e, entries, pipe)
    }
  }
}

object TLBuffer
{
  // applied to the TL source node; connect (TLBuffer(x.node) -> y.node)
  def apply(x: TLBaseNode, entries: Int = 2, pipe: Boolean = false)(implicit lazyModule: LazyModule): TLBaseNode = {
    val buffer = LazyModule(new TLBuffer(entries, pipe))
    lazyModule.connect(x -> buffer.node)
    buffer.node
  }
}
