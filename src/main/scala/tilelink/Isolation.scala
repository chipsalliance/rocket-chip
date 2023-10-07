// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util.AsyncBundle

// READ the comments in the TLIsolation object before you instantiate this module
class TLIsolation(fOut: (Bool, UInt) => UInt, fIn: (Bool, UInt) => UInt)(implicit p: Parameters) extends LazyModule
{
  val node = TLAsyncAdapterNode()

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val iso_out = Input(Bool()) // Isolate from client to manager
      val iso_in  = Input(Bool()) // Isolate from manager to client
    })

    def ISOo[T <: Data](x: T): T = fOut(io.iso_out, x.asUInt).asTypeOf(x)
    def ISOi[T <: Data](x: T): T = fIn (io.iso_in,  x.asUInt).asTypeOf(x)

    def ABo[T <: Data](x: AsyncBundle[T], y: AsyncBundle[T]): Unit = {
      x.mem            := ISOo(y.mem)
      x.widx           := ISOo(y.widx)
      y.ridx           := ISOi(x.ridx)
      (x.index zip y.index) foreach { case (x, y) => y := ISOi(x) }
      (x.safe zip y.safe) foreach { case (x, y) =>
        x.widx_valid     := ISOo(y.widx_valid)
        x.source_reset_n := ISOo(y.source_reset_n)
        y.ridx_valid     := ISOi(x.ridx_valid)
        y.sink_reset_n   := ISOi(x.sink_reset_n)
      }
    }

    def ABi[T <: Data](x: AsyncBundle[T], y: AsyncBundle[T]): Unit = {
      x.mem            := ISOi(y.mem)
      x.widx           := ISOi(y.widx)
      y.ridx           := ISOo(x.ridx)
      (x.index zip y.index) foreach { case (x, y) => y := ISOo(x) }
      (x.safe zip y.safe) foreach { case (x, y) =>
        x.widx_valid     := ISOi(y.widx_valid)
        x.source_reset_n := ISOi(y.source_reset_n)
        y.ridx_valid     := ISOo(x.ridx_valid)
        y.sink_reset_n   := ISOo(x.sink_reset_n)
      }
    }

    def ABz[T <: Data](x: AsyncBundle[T], y: AsyncBundle[T]): Unit = {
      x.widx           := 0.U
      y.ridx           := 0.U
      (x.index zip y.index) foreach { case (_, y) => y := 0.U }
      (x.safe zip y.safe) foreach { case (x, y) =>
        x.widx_valid     := false.B
        x.source_reset_n := false.B
        y.ridx_valid     := false.B
        y.sink_reset_n   := false.B
      }
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
