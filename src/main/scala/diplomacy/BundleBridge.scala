// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomacy

import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.core.{DataMirror,ActualDirection}
import chisel3.experimental.IO
import freechips.rocketchip.config.{Parameters,Field}

class BundleBridgeNull
case class BundleBridgeParams[T <: Data](gen: () => T) extends BundleBridgeNull

object BundleBridgeNull {
  def apply(): BundleBridgeNull = new BundleBridgeNull
}

class BundleBridgeImp[T <: Data]() extends SimpleNodeImp[BundleBridgeNull, BundleBridgeNull, BundleBridgeParams[T], T]
{
  def edge(pd: BundleBridgeNull, pu: BundleBridgeNull, p: Parameters, sourceInfo: SourceInfo) = (pd, pu) match {
    case (p: BundleBridgeParams[T], _: BundleBridgeNull) => p
    case (_: BundleBridgeNull, p: BundleBridgeParams[T]) => p
    case (p1: BundleBridgeParams[T], p2: BundleBridgeParams[T]) =>
      throw new Exception("Too many params found")
    case (_: BundleBridgeNull, _: BundleBridgeNull) =>
      throw new Exception("No params found")
  }
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

trait BundleBridgeBaseSource[T <: Data] extends SourceNode[BundleBridgeNull, BundleBridgeNull, BundleBridgeParams[T], BundleBridgeParams[T], T]
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

case class BundleBridgeNullSource[T <: Data]()(implicit valName: ValName) extends SourceNode(new BundleBridgeImp[T])(Seq(BundleBridgeNull())) with BundleBridgeBaseSource[T]

case class BundleBridgeSource[T <: Data](gen: () => T)(implicit valName: ValName) extends SourceNode(new BundleBridgeImp[T])(Seq(BundleBridgeParams(gen))) with BundleBridgeBaseSource[T]

case class BundleBridgeNexus[T <: Data]()(implicit valName: ValName) extends NexusNode(new BundleBridgeImp[T])(
  dFn = seq => seq.head,
  uFn = _ => BundleBridgeNull(),
  inputRequiresOutput = false)

class BundleBroadcast[T <: Data]()(implicit p: Parameters) extends LazyModule
{
  val node = BundleBridgeNexus[T]()

  lazy val module = new LazyModuleImp(this) {
    require (node.in.size == 1)
    val (in, _) = node.in.head
    def getElements(x: Data): Seq[Element] = x match {
      case e: Element => Seq(e)
      case a: Aggregate => a.getElements.flatMap(getElements)
    }
    getElements(in).foreach { elt => DataMirror.directionOf(elt) match {
      case ActualDirection.Output => ()
      case ActualDirection.Unspecified => ()
      case _ => require(false, "BundleBroadcast can only be used with Output-directed Bundles")
    } }
    node.out.foreach { case (out, _) => out := in }
  }
}

object BundleBroadcast
{
  def apply[T <: Data](name: Option[String] = None)(implicit p: Parameters): BundleBridgeNexus[T] = {
    val broadcast = LazyModule(new BundleBroadcast[T])
    name.map(broadcast.suggestName)
    broadcast.node
  }
}
