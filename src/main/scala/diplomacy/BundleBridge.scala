// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomacy

import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.experimental.IO
import freechips.rocketchip.config.{Parameters,Field}

case class BundleBridgeParams[T <: Data](gen: () => T)
case class BundleBridgeNull()

class BundleBridgeImp[T <: Data]() extends SimpleNodeImp[BundleBridgeParams[T], BundleBridgeNull, BundleBridgeParams[T], T]
{
  def edge(pd: BundleBridgeParams[T], pu: BundleBridgeNull, p: Parameters, sourceInfo: SourceInfo) = pd
  def bundle(e: BundleBridgeParams[T]) = e.gen()
  def render(e: BundleBridgeParams[T]) = RenderedEdge(colour = "#cccc00" /* yellow */)
}

case class BundleBridgeSink[T <: Data]()(implicit valName: ValName) extends SinkNode(new BundleBridgeImp[T])(Seq(BundleBridgeNull()))
{
  def bundle: T = in(0)._1

  def makeIO()(implicit valName: ValName): T = {
    val io = IO(bundle.cloneType)
    io.suggestName(valName.name)
    io <> bundle
    io
  }
}

case class BundleBridgeSource[T <: Data](gen: () => T)(implicit valName: ValName) extends SourceNode(new BundleBridgeImp[T])(Seq(BundleBridgeParams(gen)))
{
  def bundle: T = out(0)._1

  def makeIO()(implicit valName: ValName): T = {
    val io = IO(Flipped(bundle.cloneType))
    io.suggestName(valName.name)
    bundle <> io
    io
  }

  private var doneSink = false
  def makeSink()(implicit p: Parameters) = {
    require (!doneSink, "Can only call makeSink() once")
    doneSink = true
    val sink = BundleBridgeSink[T]()
    sink := this
    sink
  }
}

// BundleBridgeIdentityNode can apply a tranform to the bundle being bridged
class BundleBridgeIdentityNode[T <: Data](f: T => T = identity[T](_))(implicit valName: ValName) extends IdentityNode(new BundleBridgeImp[T])()(valName) {
  override protected[diplomacy] def instantiate() = {
    val dangles = super.instantiate()
    (out zip in) map { case ((o, _), (i, _)) => o <> f(i) }
    dangles
  }
}
