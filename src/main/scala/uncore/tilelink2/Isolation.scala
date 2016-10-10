// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import diplomacy._

class TLIsolation(fOut: (Bool, UInt) => UInt, fIn: (Bool, UInt) => UInt) extends LazyModule
{
  val node = TLAsyncIdentityNode()

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in  = node.bundleIn
      val out = node.bundleOut
      val iso_out = Bool(INPUT) // Isolate from client to manager
      val iso_in  = Bool(INPUT) // Isolate from manager to client
    }

    def ISOo[T <: Data](x: T): T = x.fromBits(fOut(io.iso_out, x.asUInt))
    def ISOi[T <: Data](x: T): T = x.fromBits(fIn (io.iso_in,  x.asUInt))

    ((io.in zip io.out) zip (node.edgesIn zip node.edgesOut)) foreach { case ((in, out), (edgeIn, edgeOut)) =>

      out.a.mem  := ISOo(in .a.mem)
      out.a.widx := ISOo(in .a.widx)
      in .a.ridx := ISOi(out.a.ridx)
      out.d.ridx := ISOo(in .d.ridx)
      in .d.widx := ISOi(out.d.widx)
      in .d.mem  := ISOi(out.d.mem)

      if (edgeOut.manager.base.anySupportAcquire && edgeOut.client.base.anySupportProbe) {
        in .b.widx := ISOi(out.b.widx)
        in .c.ridx := ISOi(out.c.ridx)
        in .e.ridx := ISOi(out.e.ridx)
        out.b.ridx := ISOo(in .b.ridx)
        out.c.widx := ISOo(in .c.widx)
        out.e.widx := ISOo(in .e.widx)
        in .b.mem  := ISOi(out.b.mem)
        out.c.mem  := ISOo(in .c.mem)
        out.e.mem  := ISOo(in .e.mem)
      } else {
        in .b.widx := UInt(0)
        in .c.ridx := UInt(0)
        in .e.ridx := UInt(0)
        out.b.ridx := UInt(0)
        out.c.widx := UInt(0)
        out.e.widx := UInt(0)
      }
    }
  }
}

object TLIsolation
{
  // applied to the TL source node; y.node := TLIsolation(fOut, fIn)(x.node)
  // f should insert an isolation gate between the input UInt and its result
  // fOut is applied for data flowing from client to manager
  // fIn  is applied for data flowing from manager to client
  def apply(fOut: (Bool, UInt) => UInt, fIn: (Bool, UInt) => UInt)(x: TLAsyncOutwardNode)(implicit sourceInfo: SourceInfo): (TLAsyncOutwardNode, () => (Bool, Bool)) = {
    val iso = LazyModule(new TLIsolation(fOut, fIn))
    iso.node := x
    (iso.node, () => (iso.module.io.iso_out, iso.module.io.iso_in))
  }
}
