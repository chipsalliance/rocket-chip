// See LICENSE.SiFive for license details.

package uncore.tilelink2

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import config._
import diplomacy._
import scala.collection.mutable.ListBuffer
import scala.math.max

// A potentially empty half-open range; [start, end)
case class IntRange(start: Int, end: Int)
{
  require (start >= 0)
  require (start <= end)
  def size = end - start
  def overlaps(x: IntRange) = start < x.end && x.start < end
  def offset(x: Int) = IntRange(x+start, x+end)
}

object IntRange
{
  implicit def apply(end: Int): IntRange = apply(0, end)
}

case class IntSourceParameters(
  range:     IntRange,
  resources: Seq[Resource] = Seq(),
  nodePath:  Seq[BaseNode] = Seq())
{
  val name = nodePath.lastOption.map(_.lazyModule.name).getOrElse("disconnected")
}

case class IntSinkParameters(
  nodePath:  Seq[BaseNode] = Seq())
{
  val name = nodePath.lastOption.map(_.lazyModule.name).getOrElse("disconnected")
}

case class IntSourcePortParameters(sources: Seq[IntSourceParameters])
{
  val num = sources.map(_.range.size).sum
  // The interrupts mapping must not overlap
  sources.map(_.range).combinations(2).foreach { case Seq(a, b) => require (!a.overlaps(b)) }
  // The interrupts must perfectly cover the range
  require (sources.isEmpty || sources.map(_.range.end).max == num)
}
object IntSourcePortSimple
{
  def apply(num: Int = 1, ports: Int = 1, sources: Int = 1, resources: Seq[Resource] = Nil) =
    if (num == 0) Nil else
    Seq.fill(ports)(IntSourcePortParameters(Seq.fill(sources)(IntSourceParameters(range = IntRange(0, num), resources = resources))))
}

case class IntSinkPortParameters(sinks: Seq[IntSinkParameters])
object IntSinkPortSimple
{
  def apply(ports: Int = 1, sinks: Int = 1) =
    Seq.fill(ports)(IntSinkPortParameters(Seq.fill(sinks)(IntSinkParameters())))
}

case class IntEdge(source: IntSourcePortParameters, sink: IntSinkPortParameters)

object IntImp extends NodeImp[IntSourcePortParameters, IntSinkPortParameters, IntEdge, IntEdge, Vec[Bool]]
{
  def edgeO(pd: IntSourcePortParameters, pu: IntSinkPortParameters): IntEdge = IntEdge(pd, pu)
  def edgeI(pd: IntSourcePortParameters, pu: IntSinkPortParameters): IntEdge = IntEdge(pd, pu)
  def bundleO(eo: IntEdge): Vec[Bool] = Vec(eo.source.num, Bool())
  def bundleI(ei: IntEdge): Vec[Bool] = Vec(ei.source.num, Bool())

  def colour = "#0000ff" // blue
  override def labelI(ei: IntEdge) = ei.source.sources.map(_.range.size).sum.toString
  override def labelO(eo: IntEdge) = eo.source.sources.map(_.range.size).sum.toString

  def connect(bo: => Vec[Bool], bi: => Vec[Bool], ei: => IntEdge)(implicit p: Parameters, sourceInfo: SourceInfo): (Option[LazyModule], () => Unit) = {
    (None, () => {
      // Cannot use bulk connect, because the widths could differ
      (bo zip bi) foreach { case (o, i) => i := o }
    })
  }

  override def mixO(pd: IntSourcePortParameters, node: OutwardNode[IntSourcePortParameters, IntSinkPortParameters, Vec[Bool]]): IntSourcePortParameters =
   pd.copy(sources = pd.sources.map  { s => s.copy (nodePath = node +: s.nodePath) })
  override def mixI(pu: IntSinkPortParameters, node: InwardNode[IntSourcePortParameters, IntSinkPortParameters, Vec[Bool]]): IntSinkPortParameters =
   pu.copy(sinks   = pu.sinks.map    { s => s.copy (nodePath = node +: s.nodePath) })
}

case class IntIdentityNode() extends IdentityNode(IntImp)
case class IntSourceNode(portParams: Seq[IntSourcePortParameters]) extends SourceNode(IntImp)(portParams)
case class IntSinkNode(portParams: Seq[IntSinkPortParameters]) extends SinkNode(IntImp)(portParams)

case class IntNexusNode(
  sourceFn:       Seq[IntSourcePortParameters] => IntSourcePortParameters,
  sinkFn:         Seq[IntSinkPortParameters]   => IntSinkPortParameters,
  numSourcePorts: Range.Inclusive = 0 to 128,
  numSinkPorts:   Range.Inclusive = 0 to 128)
  extends NexusNode(IntImp)(sourceFn, sinkFn, numSourcePorts, numSinkPorts)

case class IntOutputNode() extends OutputNode(IntImp)
case class IntInputNode() extends InputNode(IntImp)

case class IntBlindOutputNode(portParams: Seq[IntSinkPortParameters]) extends BlindOutputNode(IntImp)(portParams)
case class IntBlindInputNode(portParams: Seq[IntSourcePortParameters]) extends BlindInputNode(IntImp)(portParams)

case class IntInternalOutputNode(portParams: Seq[IntSinkPortParameters]) extends InternalOutputNode(IntImp)(portParams)
case class IntInternalInputNode(portParams: Seq[IntSourcePortParameters]) extends InternalInputNode(IntImp)(portParams)

class IntXbar()(implicit p: Parameters) extends LazyModule
{
  val intnode = IntNexusNode(
    sinkFn         = { _ => IntSinkPortParameters(Seq(IntSinkParameters())) },
    sourceFn       = { seq =>
      IntSourcePortParameters((seq zip seq.map(_.num).scanLeft(0)(_+_).init).map {
        case (s, o) => s.sources.map(z => z.copy(range = z.range.offset(o)))
      }.flatten)
    })

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in  = intnode.bundleIn
      val out = intnode.bundleOut
    }

    val cat = (intnode.edgesIn zip io.in).map{ case (e, i) => i.take(e.source.num) }.flatten
    io.out.foreach { _ := cat }
  }
}

class IntXing(sync: Int = 3)(implicit p: Parameters) extends LazyModule
{
  val intnode = IntIdentityNode()

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in = intnode.bundleIn
      val out = intnode.bundleOut
    }

    (io.in zip io.out) foreach { case (in, out) =>
      out := (0 to sync).foldLeft(in) { case (a, _) => RegNext(a) }
    }
  }
}
