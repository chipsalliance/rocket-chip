// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import diplomacy._

class TLIsolation(f: (Bool, UInt) => UInt) extends LazyModule
{
  val node = TLAsyncIdentityNode()

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in  = node.bundleIn
      val out = node.bundleOut
      val iso = Bool(INPUT)
    }

    def ISO[T <: Data](x: T): T = x.fromBits(f(io.iso, x.asUInt))

    ((io.in zip io.out) zip (node.edgesIn zip node.edgesOut)) foreach { case ((in, out), (edgeIn, edgeOut)) =>

      out.a.mem := ISO(in.a.mem)
      out.a.widx := ISO(in.a.widx)
      in.a.ridx := ISO(out.a.ridx)
      out.d.ridx := ISO(in.d.ridx)
      in.d.widx := ISO(out.d.widx)
      in.d.mem := ISO(out.d.mem)

      if (edgeOut.manager.base.anySupportAcquire && edgeOut.client.base.anySupportProbe) {
        in.b.widx := ISO(out.b.widx)
        in.c.ridx := ISO(out.c.ridx)
        in.e.ridx := ISO(out.e.ridx)
        out.b.ridx := ISO(in.b.ridx)
        out.c.widx := ISO(in.c.widx)
        out.e.widx := ISO(in.e.widx)
        in.b.mem := ISO(out.b.mem)
        out.c.mem := ISO(in.c.mem)
        out.e.mem := ISO(in.e.mem)
      } else {
        in.b.widx := UInt(0)
        in.c.ridx := UInt(0)
        in.e.ridx := UInt(0)
        out.b.ridx := UInt(0)
        out.c.widx := UInt(0)
        out.e.widx := UInt(0)
      }
    }
  }
}

object TLIsolation
{
  // applied to the TL source node; y.node := TLIsolation()(x.node)
  // f should insert an isolation gate between the input UInt and its result
  def apply(f: (Bool, UInt) => UInt)(x: TLAsyncOutwardNode)(implicit sourceInfo: SourceInfo): (TLAsyncOutwardNode, () => Bool) = {
    val iso = LazyModule(new TLIsolation(f))
    iso.node := x
    (iso.node, () => iso.module.io.iso)
  }
}
