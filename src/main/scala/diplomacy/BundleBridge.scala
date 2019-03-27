// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomacy

import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.core.{DataMirror,ActualDirection}
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

case class BundleBridgeIdentityNode[T <: Data]()(implicit valName: ValName) extends IdentityNode(new BundleBridgeImp[T])()
case class BundleBridgeEphemeralNode[T <: Data]()(implicit valName: ValName) extends EphemeralNode(new BundleBridgeImp[T])()

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
