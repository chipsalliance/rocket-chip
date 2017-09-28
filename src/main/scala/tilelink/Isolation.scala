// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util.AsyncBundle

// READ the comments in the TLIsolation object before you instantiate this module
class TLIsolation(fOut: (Bool, UInt) => UInt, fIn: (Bool, UInt) => UInt)(implicit p: Parameters) extends LazyModule
{
  val node = TLAsyncAdapterNode()

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val iso_out = Bool(INPUT) // Isolate from client to manager
      val iso_in  = Bool(INPUT) // Isolate from manager to client
    })

    def ISOo[T <: Data](x: T): T = x.fromBits(fOut(io.iso_out, x.asUInt))
    def ISOi[T <: Data](x: T): T = x.fromBits(fIn (io.iso_in,  x.asUInt))

    def ABo[T <: Data](x: AsyncBundle[T], y: AsyncBundle[T]) {
      x.mem            := ISOo(y.mem)
      x.widx           := ISOo(y.widx)
      x.widx_valid     := ISOo(y.widx_valid)
      x.source_reset_n := ISOo(y.source_reset_n)
      y.ridx           := ISOi(x.ridx)
      y.ridx_valid     := ISOi(x.ridx_valid)
      y.sink_reset_n   := ISOi(x.sink_reset_n)
    }

    def ABi[T <: Data](x: AsyncBundle[T], y: AsyncBundle[T]) {
      x.mem            := ISOi(y.mem)
      x.widx           := ISOi(y.widx)
      x.widx_valid     := ISOi(y.widx_valid)
      x.source_reset_n := ISOi(y.source_reset_n)
      y.ridx           := ISOo(x.ridx)
      y.ridx_valid     := ISOo(x.ridx_valid)
      y.sink_reset_n   := ISOo(x.sink_reset_n)
    }

    def ABz[T <: Data](x: AsyncBundle[T], y: AsyncBundle[T]) {
      x.widx           := UInt(0)
      x.widx_valid     := Bool(false)
      x.source_reset_n := Bool(false)
      y.ridx           := UInt(0)
      y.ridx_valid     := Bool(false)
      y.sink_reset_n   := Bool(false)
    }

    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      ABo(out.a, in .a)
      ABi(in .d, out.d)

      if (edgeOut.manager.base.anySupportAcquireB && edgeOut.client.base.anySupportProbe) {
        ABi(in .b, out.b)
        ABo(out.c, in .c)
        ABo(out.e, in .e)
      } else {
        ABz(in .b, out.b)
        ABz(out.c, in .c)
        ABz(out.e, in .e)
      }
    }
  }
}

object TLIsolation
{
  // applied to the TL source node; y.node := TLIsolation(fOut, fIn)(x.node)
  // f* should insert an isolation gate between the input UInt and its result
  // fOut is applied to data flowing from client to manager
  // fIn  is applied to data flowing from manager to client
  // **** WARNING: the isolation functions must bring the values to 0 ****
  def apply(fOut: (Bool, UInt) => UInt, fIn: (Bool, UInt) => UInt)(x: TLAsyncOutwardNode)(implicit p: Parameters, sourceInfo: SourceInfo): (TLAsyncOutwardNode, () => (Bool, Bool)) = {
    val iso = LazyModule(new TLIsolation(fOut, fIn))
    iso.node :=? x
    (iso.node, () => (iso.module.io.iso_out, iso.module.io.iso_in))
  }
}
