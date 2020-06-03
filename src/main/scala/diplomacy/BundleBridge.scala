// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomacy

import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.experimental.{DataMirror,IO}
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

  def makeIO()(implicit valName: ValName): T = makeIOs()(valName).head
}

case class BundleBridgeSource[T <: Data](gen: () => T)(implicit valName: ValName) extends SourceNode(new BundleBridgeImp[T])(Seq(BundleBridgeParams(gen)))
{
  def bundle: T = out(0)._1

  def makeIO()(implicit valName: ValName): T = makeIOs()(valName).head

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

case class BundleBridgeNexusNode[T <: Data](default: Option[() => T] = None)
                                           (implicit valName: ValName)
  extends NexusNode(new BundleBridgeImp[T])(
    dFn = seq => seq.headOption.orElse(default.map(BundleBridgeParams(_))).get,
    uFn = _ => BundleBridgeNull(),
    inputRequiresOutput = false,
    outputRequiresInput = !default.isDefined)

class BundleBridgeNexus[T <: Data](
  inputFn: Seq[T] => T,
  outputFn: (T, Int) => Seq[T],
  default: Option[() => T] = None
) (implicit p: Parameters) extends LazyModule
{
  val node = BundleBridgeNexusNode[T](default)

  lazy val module = new LazyModuleImp(this) {
    val defaultWireOpt = default.map(_())
    val inputs: Seq[T] = defaultWireOpt.toList ++ node.in.map(_._1)
    require(inputs.size >= 1, "BundleBridgeNexus requires at least one input or default.")
    inputs.foreach { i => require(DataMirror.checkTypeEquivalence(i, inputs.head),
      "BundleBridgeNexus requires all inputs have equivalent Chisel Data types")
    }
    def getElements(x: Data): Seq[Element] = x match {
      case e: Element => Seq(e)
      case a: Aggregate => a.getElements.flatMap(getElements)
    }
    inputs.flatMap(getElements).foreach { elt => DataMirror.directionOf(elt) match {
      case ActualDirection.Output => ()
      case ActualDirection.Unspecified => ()
      case _ => require(false, "BundleBridgeNexus can only be used with Output-directed Bundles")
    } }

    val broadcast: T = inputFn(inputs)
    val outputs: Seq[T] = outputFn(broadcast, node.out.size)
    node.out.map(_._1).foreach { o => require(DataMirror.checkTypeEquivalence(o, outputs.head),
      s"BundleBridgeNexus requires all outputs have equivalent Chisel Data types, but got\n$o\nvs\n${outputs.head}")
    }
    require(outputs.size == node.out.size,
      s"BundleBridgeNexus outputFn must generate one output wire per edgeOut, but got ${outputs.size} vs ${node.out.size}")

    node.out.zip(outputs).foreach { case ((out, _), bcast) => out := bcast }
  }
}

object BundleBridgeNexus {
  def requireOne[T <: Data](registered: Boolean)(seq: Seq[T]): T = {
    require(seq.size == 1, "BundleBroadcast default requires one input")
    if (registered) RegNext(seq.head) else seq.head
  }

  def orReduction[T <: Data](registered: Boolean)(seq: Seq[T]): T = {
    val x = seq.reduce((a,b) => (a.asUInt | b.asUInt).asTypeOf(seq.head))
    if (registered) RegNext(x) else x
  }

  def fillN[T <: Data](registered: Boolean)(x: T, n: Int): Seq[T] = Seq.fill(n) {
    if (registered) RegNext(x) else x
  }

  def apply[T <: Data](
    inputFn: Seq[T] => T = orReduction[T](false) _,
    outputFn: (T, Int) => Seq[T] = fillN[T](false) _,
    default: Option[() => T] = None
  )(implicit p: Parameters, valName: ValName): BundleBridgeNexusNode[T] = {
    val broadcast = LazyModule(new BundleBridgeNexus[T](inputFn, outputFn, default))
    broadcast.node
  }
}

object BundleBroadcast {
  def apply[T <: Data](
    name: Option[String] = None,
    registered: Boolean = false
  )(implicit p: Parameters, valName: ValName): BundleBridgeNexusNode[T] = {
    val finalName = name.map(ValName(_)).getOrElse(valName)
    BundleBridgeNexus.apply[T](
      inputFn = BundleBridgeNexus.requireOne[T](registered) _,
      outputFn = BundleBridgeNexus.fillN[T](registered) _)(
      p, finalName)
  }
}
