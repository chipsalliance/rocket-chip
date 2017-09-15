// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

case class TLNodeNumbererNode(nodeAddressOffset: Option[Int] = None)(implicit valName: ValName) extends TLCustomNode(0 to 999, 0 to 999)
{
  def resolveStar(iKnown: Int, oKnown: Int, iStars: Int, oStars: Int): (Int, Int) = {
    require (oStars + iStars <= 1, s"${name} (a custom adapter) appears left of a :*= ${iStars} times and right of a :=* ${oStars} times; at most once is allowed${lazyModule.line}")
    if (oStars > 0) {
      require (iKnown >= oKnown, s"${name} (a custom adapter) has ${oKnown} outputs and ${iKnown} inputs; cannot assign ${iKnown-oKnown} edges to resolve :=*${lazyModule.line}")
      (0, iKnown - oKnown)
    } else {
      require (oKnown >= iKnown, s"${name} (a custom adapter) has ${oKnown} outputs and ${iKnown} inputs; cannot assign ${oKnown-iKnown} edges to resolve :*=${lazyModule.line}")
      (oKnown - iKnown, 0)
    }
  }

  def mapParamsD(n: Int, p: Seq[TLClientPortParameters]): Seq[TLClientPortParameters] = {
    require(n == p.size, s"${name} has ${p.size} inputs and ${n} outputs; they must match${lazyModule.line}")
    p
  }

  def mapParamsU(n: Int, p: Seq[TLManagerPortParameters]): Seq[TLManagerPortParameters] = {
    require(n == p.size, s"${name} has ${n} inputs and ${p.size} outputs; they must match${lazyModule.line}")
    val minNodeOffset = log2Ceil(p.map(_.maxAddress).max)
    val nodeOffset = nodeAddressOffset.getOrElse(minNodeOffset)
    require (nodeOffset >= minNodeOffset)

    p.zipWithIndex.map { case (mp, i) =>
      val nodeIndex = BigInt(i+1) << nodeOffset
      mp.copy(managers = mp.managers.map(m => m.copy(address = m.address.map(a => a.copy(base = a.base | nodeIndex)))))
    }
  }
}

class TLNodeNumberer(nodeAddressOffset: Option[Int] = None)(implicit p: Parameters) extends LazyModule
{
  val node = TLNodeNumbererNode(nodeAddressOffset)

  lazy val module = new LazyModuleImp(this) {
    val minNodeOffset = log2Ceil(node.edges.out.map(_.manager.maxAddress).max)
    val nodeOffset = nodeAddressOffset.getOrElse(minNodeOffset)

    (node.in zip node.out).zipWithIndex foreach { case (((in, _), (out, _)), i) =>
      out <> in
      // a&c address already get truncated
      in.b.bits.address := (UInt(i+1) << nodeOffset) | out.b.bits.address
    }
  }
}

object TLNodeNumberer
{
  // applied to the TL source node; y.node := TLBuffer(x.node)
  def apply(nodeAddressOffset: Option[Int] = None)(x: TLOutwardNode)(implicit p: Parameters, sourceInfo: SourceInfo): TLOutwardNode = {
    val numberer = LazyModule(new TLNodeNumberer(nodeAddressOffset))
    numberer.node :=? x
    numberer.node
  }
}
