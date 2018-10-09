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
    require (!doneSink, "Can only call makeSink() once; use a BundleBridgeBroadcast node")
    doneSink = true
    val sink = BundleBridgeSink[T]()
    sink := this
    sink
  }
}

// BundleBridgeTransform can apply a transform to the bundle being bridged
class BundleBridgeTransform[T <: Data](f: T => T = identity[T](_))(implicit valName: ValName) extends IdentityNode(new BundleBridgeImp[T])()(valName) {
  override protected[diplomacy] def instantiate() = {
    val dangles = super.instantiate()
    (out zip in) map { case ((o, _), (i, _)) => o <> f(i) }
    dangles
  }
}

// BundleBridgeTap allows the bridged wire to be tapped
class BundleBridgeTap[T <: Data]()(implicit valName: ValName) extends BundleBridgeTransform[T]()(valName) {
  def bundle: T = out(0)._1
}

class BundleBridgeBroadcast[T <: Data](
    inputRequiresOutput: Boolean = true,
    outputRequiresInput: Boolean = true)(
    implicit valName: ValName)
  extends CustomNode(new BundleBridgeImp[T])
{
  def resolveStar(iKnown: Int, oKnown: Int, iStars: Int, oStars: Int): (Int, Int) = {
    require (!outputRequiresInput || oKnown == 0 || iStars + iKnown != 0, s"$name $location has $oKnown required outputs and no possible inputs")
    require (!inputRequiresOutput || iKnown == 0 || oStars + oKnown != 0, s"$name $location has $iKnown required inputs and no possible outputs")
    require (iKnown == 0 || iKnown == 1, s"$name $location must have 0 or 1 input")
    if (iKnown == 0 && oKnown == 0) (0, 0) else (1, 1)
  }
  def mapParamsD(n: Int, p: Seq[BundleBridgeParams[T]]): Seq[BundleBridgeParams[T]] = { if (n > 0) { Seq.fill(n)(p.head) } else Nil }
  def mapParamsU(n: Int, p: Seq[BundleBridgeNull]): Seq[BundleBridgeNull] = { if (n > 0) { Seq(BundleBridgeNull()) } else Nil }

  override protected[diplomacy] def instantiate() = {
    val dangles = super.instantiate()
    out.map { case (o, _) => o := in.head._1 }
    dangles
  }
}
