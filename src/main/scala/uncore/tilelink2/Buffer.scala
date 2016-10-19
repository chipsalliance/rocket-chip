// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import diplomacy._
import scala.math.{min,max}

// pipe is only used if a queue has depth = 1
class TLBuffer(a: Int = 2, b: Int = 2, c: Int = 2, d: Int = 2, e: Int = 2, pipe: Boolean = true) extends LazyModule
{
  require (a >= 0)
  require (b >= 0)
  require (c >= 0)
  require (d >= 0)
  require (e >= 0)

  val node = TLAdapterNode(
    clientFn  = { seq => seq(0).copy(minLatency = seq(0).minLatency + min(1,b) + min(1,c)) },
    managerFn = { seq => seq(0).copy(minLatency = seq(0).minLatency + min(1,a) + min(1,d)) })

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in  = node.bundleIn
      val out = node.bundleOut
    }
    
    ((io.in zip io.out) zip (node.edgesIn zip node.edgesOut)) foreach { case ((in, out), (edgeIn, edgeOut)) =>
      if (a>0) { out.a <> Queue(in .a, a, pipe && a<2) } else { out.a <> in.a }
      if (d>0) { in .d <> Queue(out.d, d, pipe && d<2) } else { in.d <> out.d }

      if (edgeOut.manager.anySupportAcquire && edgeOut.client.anySupportProbe) {
        if (b>0) { in .b <> Queue(out.b, b, pipe && b<2) } else { in.b <> out.b }
        if (c>0) { out.c <> Queue(in .c, c, pipe && c<2) } else { out.c <> in.c }
        if (e>0) { out.e <> Queue(in .e, e, pipe && e<2) } else { out.e <> in.e }
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
  def apply()                                (x: TLOutwardNode)(implicit sourceInfo: SourceInfo): TLOutwardNode = apply(2)(x)
  def apply(entries: Int)                    (x: TLOutwardNode)(implicit sourceInfo: SourceInfo): TLOutwardNode = apply(entries, true)(x)
  def apply(entries: Int, pipe: Boolean)     (x: TLOutwardNode)(implicit sourceInfo: SourceInfo): TLOutwardNode = apply(entries, entries, pipe)(x)
  def apply(ace: Int, bd: Int)               (x: TLOutwardNode)(implicit sourceInfo: SourceInfo): TLOutwardNode = apply(ace, bd, true)(x)
  def apply(ace: Int, bd: Int, pipe: Boolean)(x: TLOutwardNode)(implicit sourceInfo: SourceInfo): TLOutwardNode = apply(ace, bd, ace, bd, ace, pipe)(x)
  def apply(a: Int, b: Int, c: Int, d: Int, e: Int, pipe: Boolean = true)(x: TLOutwardNode)(implicit sourceInfo: SourceInfo): TLOutwardNode = {
    val buffer = LazyModule(new TLBuffer(a, b, c, d, e, pipe))
    buffer.node := x
    buffer.node
  }
}
