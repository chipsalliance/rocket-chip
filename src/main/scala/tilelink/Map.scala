// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import scala.math.{min,max}

// Moves the AddressSets of slave devices around
// Combine with TLFilter to remove slaves or reduce their size
class TLMap(fn: AddressSet => BigInt)(implicit p: Parameters) extends LazyModule
{
  val node = TLAdapterNode(
    clientFn = { cp => cp },
    managerFn = { mp =>
      mp.copy(managers = mp.managers.map(m =>
        m.copy(address = m.address.map(a =>
          AddressSet(fn(a), a.mask)))))})

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in = node.bundleIn
      val out = node.bundleOut
    }

    io.out <> io.in

    ((io.in zip io.out) zip (node.edgesIn zip node.edgesOut)) foreach { case ((in, out), (edgeIn, edgeOut)) =>
      val convert = edgeIn.manager.managers.flatMap(_.address) zip edgeOut.manager.managers.flatMap(_.address)
      def forward(x: UInt) =
        convert.map { case (i, o) => Mux(i.contains(x), UInt(o.base) | (x & UInt(o.mask)), UInt(0)) }.reduce(_ | _)
      def backward(x: UInt) =
        convert.map { case (i, o) => Mux(o.contains(x), UInt(i.base) | (x & UInt(i.mask)), UInt(0)) }.reduce(_ | _)

      out.a.bits.address := forward(in.a.bits.address)
      if (edgeOut.manager.anySupportAcquireB && edgeOut.client.anySupportProbe) {
        out.c.bits.address := forward(in.c.bits.address)
        in.b.bits.address := backward(out.b.bits.address)
      }
    }
  }
}

object TLMap
{
  // applied to the TL source node; y.node := TLMap(fn)(x.node)
  def apply(fn: AddressSet => BigInt)(x: TLOutwardNode)(implicit p: Parameters, sourceInfo: SourceInfo): TLOutwardNode = {
    val map = LazyModule(new TLMap(fn))
    map.node :=? x
    map.node
  }
}
