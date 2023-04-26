// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._

// Moves the AddressSets of slave devices around
// Combine with TLFilter to remove slaves or reduce their size
class TLMap(fn: AddressSet => BigInt)(implicit p: Parameters) extends LazyModule
{
  val node = TLAdapterNode(
    clientFn = { cp => cp },
    managerFn = { mp =>
      mp.v1copy(managers = mp.managers.map(m =>
        m.v1copy(address = m.address.map(a =>
          AddressSet(fn(a), a.mask)))))})

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out <> in
      val convert = edgeIn.manager.managers.flatMap(_.address) zip edgeOut.manager.managers.flatMap(_.address)
      def forward(x: UInt) =
        convert.map { case (i, o) => Mux(i.contains(x), o.base.U | (x & o.mask.U), 0.U) }.reduce(_ | _)
      def backward(x: UInt) =
        convert.map { case (i, o) => Mux(o.contains(x), i.base.U | (x & i.mask.U), 0.U) }.reduce(_ | _)

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
  def apply(fn: AddressSet => BigInt)(implicit p: Parameters): TLNode =
  {
    val map = LazyModule(new TLMap(fn))
    map.node
  }
}
