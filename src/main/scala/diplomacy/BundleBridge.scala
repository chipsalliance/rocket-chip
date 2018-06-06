// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomacy

import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.{Parameters,Field}

case class BundleBridgeParams[T <: Data](gen: () => T)
case class BundleBridgeNull()

class BundleBridgeImp[T <: Data]() extends SimpleNodeImp[BundleBridgeParams[T], BundleBridgeNull, BundleBridgeParams[T], T]
{
  def edge(pd: BundleBridgeParams[T], pu: BundleBridgeNull, p: Parameters, sourceInfo: SourceInfo) = pd
  def bundle(e: BundleBridgeParams[T]) = e.gen()
  def render(e: BundleBridgeParams[T]) = RenderedEdge(colour = "#cccc00" /* yellow */)
}

case class BundleBridgeSource[T <: Data](gen: () => T)(implicit valName: ValName) extends SourceNode(new BundleBridgeImp[T])(Seq(BundleBridgeParams(gen)))
case class BundleBridgeSink[T<: Data]()(implicit valName: ValName) extends SinkNode(new BundleBridgeImp[T])(Seq(BundleBridgeNull()))
