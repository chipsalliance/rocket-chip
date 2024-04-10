// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomacy

import chisel3._
import chisel3.experimental.SourceInfo
import chisel3.reflect.DataMirror
import chisel3.reflect.DataMirror.internal.chiselTypeClone
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util.DataToAugmentedData

case class BundleBridgeParams[T <: Data](genOpt: Option[() => T])

case object BundleBridgeParams {
  def apply[T <: Data](gen: () => T): BundleBridgeParams[T] = BundleBridgeParams(Some(gen))
}

case class BundleBridgeEdgeParams[T <: Data](source: BundleBridgeParams[T], sink: BundleBridgeParams[T])

class BundleBridgeImp[T <: Data]() extends SimpleNodeImp[BundleBridgeParams[T], BundleBridgeParams[T], BundleBridgeEdgeParams[T], T]
{
  def edge(pd: BundleBridgeParams[T], pu: BundleBridgeParams[T], p: Parameters, sourceInfo: SourceInfo) = BundleBridgeEdgeParams(pd, pu)
  def bundle(e: BundleBridgeEdgeParams[T]): T = {
    val sourceOpt = e.source.genOpt.map(_())
    val sinkOpt = e.sink.genOpt.map(_())
    (sourceOpt, sinkOpt) match {
      case (None,    None)    =>
        throw new Exception("BundleBridge needs source or sink to provide bundle generator function")
      case (Some(a), None)    => chiselTypeClone(a)
      case (None,    Some(b)) => chiselTypeClone(b)
      case (Some(a), Some(b)) => {
        require(DataMirror.checkTypeEquivalence(a, b),
          s"BundleBridge requires doubly-specified source and sink generators to have equivalent Chisel Data types, but got \n$a\n vs\n$b")
        chiselTypeClone(a)
      }
    }
  }
  def render(e: BundleBridgeEdgeParams[T]) = RenderedEdge(colour = "#cccc00" /* yellow */)
}

case class BundleBridgeSink[T <: Data](genOpt: Option[() => T] = None)
                                      (implicit valName: ValName)
  extends SinkNode(new BundleBridgeImp[T])(Seq(BundleBridgeParams(genOpt)))
{
  def bundle: T = in(0)._1

  private def inferOutput = bundle.getElements.forall { elt =>
    DataMirror.directionOf(elt) == ActualDirection.Unspecified
  }

  def makeIO()(implicit valName: ValName): T = {
    val io: T = IO(if (inferOutput) Output(chiselTypeOf(bundle)) else chiselTypeClone(bundle))
    io.suggestName(valName.name)
    io <> bundle
    io
  }
  def makeIO(name: String): T = makeIO()(ValName(name))
}

object BundleBridgeSink {
  def apply[T <: Data]()(implicit valName: ValName): BundleBridgeSink[T] = {
    BundleBridgeSink(None)
  }
}

case class BundleBridgeSource[T <: Data](genOpt: Option[() => T] = None)(implicit valName: ValName) extends SourceNode(new BundleBridgeImp[T])(Seq(BundleBridgeParams(genOpt)))
{
  def bundle: T = out(0)._1

  private def inferInput = bundle.getElements.forall { elt =>
    DataMirror.directionOf(elt) == ActualDirection.Unspecified
  }

  def makeIO()(implicit valName: ValName): T = {
    val io: T = IO(if (inferInput) Input(chiselTypeOf(bundle)) else Flipped(chiselTypeClone(bundle)))
    io.suggestName(valName.name)
    bundle <> io
    io
  }
  def makeIO(name: String): T = makeIO()(ValName(name))

  private var doneSink = false
  def makeSink()(implicit p: Parameters) = {
    require (!doneSink, "Can only call makeSink() once")
    doneSink = true
    val sink = BundleBridgeSink[T]()
    sink := this
    sink
  }
}

object BundleBridgeSource {
  def apply[T <: Data]()(implicit valName: ValName): BundleBridgeSource[T] = {
    BundleBridgeSource(None)
  }
  def apply[T <: Data](gen: () => T)(implicit valName: ValName): BundleBridgeSource[T] = {
    BundleBridgeSource(Some(gen))
  }
}

case class BundleBridgeIdentityNode[T <: Data]()(implicit valName: ValName) extends IdentityNode(new BundleBridgeImp[T])()
case class BundleBridgeEphemeralNode[T <: Data]()(implicit valName: ValName) extends EphemeralNode(new BundleBridgeImp[T])()

object BundleBridgeNameNode {
  def apply[T <: Data](name: String) = BundleBridgeIdentityNode[T]()(ValName(name))
}

case class BundleBridgeNexusNode[T <: Data](default: Option[() => T] = None,
                                            inputRequiresOutput: Boolean = false) // when false, connecting a source does not mandate connecting a sink
                                           (implicit valName: ValName)
  extends NexusNode(new BundleBridgeImp[T])(
    dFn = seq => seq.headOption.getOrElse(BundleBridgeParams(default)),
    uFn = seq => seq.headOption.getOrElse(BundleBridgeParams(None)),
    inputRequiresOutput = inputRequiresOutput,
    outputRequiresInput = !default.isDefined)

class BundleBridgeNexus[T <: Data](
  inputFn: Seq[T] => T,
  outputFn: (T, Int) => Seq[T],
  default: Option[() => T] = None,
  inputRequiresOutput: Boolean = false,
  override val shouldBeInlined: Boolean = true
) (implicit p: Parameters) extends LazyModule
{
  val node = BundleBridgeNexusNode[T](default, inputRequiresOutput)

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val defaultWireOpt = default.map(_())
    val inputs: Seq[T] = node.in.map(_._1)
    inputs.foreach { i => require(DataMirror.checkTypeEquivalence(i, inputs.head),
      s"${node.context} requires all inputs have equivalent Chisel Data types, but got\n$i\nvs\n${inputs.head}")
    }
    inputs.flatMap(_.getElements).foreach { elt => DataMirror.directionOf(elt) match {
      case ActualDirection.Output => ()
      case ActualDirection.Unspecified => ()
      case _ => require(false, s"${node.context} can only be used with Output-directed Bundles")
    } }

    val outputs: Seq[T] = if (node.out.size > 0) {
      val broadcast: T = if (inputs.size >= 1) inputFn(inputs) else defaultWireOpt.get
      outputFn(broadcast, node.out.size)
    } else { Nil }

    node.out.map(_._1).foreach { o => require(DataMirror.checkTypeEquivalence(o, outputs.head),
      s"${node.context} requires all outputs have equivalent Chisel Data types, but got\n$o\nvs\n${outputs.head}")
    }

    require(outputs.size == node.out.size,
      s"${node.context} outputFn must generate one output wire per edgeOut, but got ${outputs.size} vs ${node.out.size}")

    node.out.zip(outputs).foreach { case ((out, _), bcast) => out := bcast }
  }
}

object BundleBridgeNexus {
  def safeRegNext[T <: Data](x: T): T = {
    val reg = Reg(chiselTypeOf(x))
    reg := x
    reg
  }

  def requireOne[T <: Data](registered: Boolean)(seq: Seq[T]): T = {
    require(seq.size == 1, "BundleBroadcast default requires one input")
    if (registered) safeRegNext(seq.head) else seq.head
  }

  def orReduction[T <: Data](registered: Boolean)(seq: Seq[T]): T = {
    val x = seq.reduce((a,b) => (a.asUInt | b.asUInt).asTypeOf(seq.head))
    if (registered) safeRegNext(x) else x
  }

  def fillN[T <: Data](registered: Boolean)(x: T, n: Int): Seq[T] = Seq.fill(n) {
    if (registered) safeRegNext(x) else x
  }

  def apply[T <: Data](
    inputFn: Seq[T] => T = orReduction[T](false) _,
    outputFn: (T, Int) => Seq[T] = fillN[T](false) _,
    default: Option[() => T] = None,
    inputRequiresOutput: Boolean = false,
    shouldBeInlined: Boolean = true
  )(implicit p: Parameters): BundleBridgeNexusNode[T] = {
    val nexus = LazyModule(new BundleBridgeNexus[T](inputFn, outputFn, default, inputRequiresOutput, shouldBeInlined))
    nexus.node
  }
}

object BundleBroadcast {
  def apply[T <: Data](
    name: Option[String] = None,
    registered: Boolean = false,
    default: Option[() => T] = None,
    inputRequiresOutput: Boolean = false, // when false, connecting a source does not mandate connecting a sink
    shouldBeInlined: Boolean = true
  )(implicit p: Parameters): BundleBridgeNexusNode[T] = {
    val broadcast = LazyModule(new BundleBridgeNexus[T](
      inputFn = BundleBridgeNexus.requireOne[T](registered) _,
      outputFn = BundleBridgeNexus.fillN[T](registered) _,
      default = default,
      inputRequiresOutput = inputRequiresOutput,
      shouldBeInlined = shouldBeInlined))
    name.foreach(broadcast.suggestName(_))
    broadcast.node
  }
}
