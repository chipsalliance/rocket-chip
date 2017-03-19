// See LICENSE.SiFive for license details.

package uncore.tilelink2

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import config._
import diplomacy._

// q is the probability to delay a request
class TLDelayer(q: Double)(implicit p: Parameters) extends LazyModule
{
  val node = TLIdentityNode()
  require (0.0 <= q && q < 1)

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in  = node.bundleIn
      val out = node.bundleOut
    }

    def feed[T <: Data](sink: DecoupledIO[T], source: DecoupledIO[T]) {
      val allow = UInt((q * 65535.0).toInt) <= LFSR16(source.valid)
      sink.valid := source.valid && allow
      source.ready := sink.ready && allow
      sink.bits := source.bits
    }

    (io.in zip io.out) foreach { case (in, out) =>
      feed(out.a, in.a)
      feed(out.c, in.c)
      feed(out.e, in.e)
      feed(in.b, out.b)
      feed(in.d, out.d)
    }
  }
}

object TLDelayer
{
  // applied to the TL source node; y.node := TLDelayer(0.01)(x.node)
  def apply(q: Double)(x: TLOutwardNode)(implicit p: Parameters, sourceInfo: SourceInfo): TLOutwardNode = {
    val delayer = LazyModule(new TLDelayer(q))
    delayer.node := x
    delayer.node
  }
}
