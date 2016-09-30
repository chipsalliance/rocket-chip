// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import scala.collection.mutable.ListBuffer
import scala.math.max
import chisel3.internal.sourceinfo.SourceInfo

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
  range:    IntRange,
  nodePath: Seq[BaseNode] = Seq())
{
  val name = nodePath.lastOption.map(_.lazyModule.name).getOrElse("disconnected")
}

case class IntSinkParameters(
  nodePath: Seq[BaseNode] = Seq())
{
  val name = nodePath.lastOption.map(_.lazyModule.name).getOrElse("disconnected")
}

case class IntSourcePortParameters(sources: Seq[IntSourceParameters])
{
  val num = sources.map(_.range.size).sum
  // The interrupts mapping must not overlap
  sources.map(_.range).combinations(2).foreach { case Seq(a, b) => require (!a.overlaps(b)) }
  // The interrupts must perfectly cover the range
  require (sources.map(_.range.end).max == num)
}

case class IntSinkPortParameters(sinks: Seq[IntSinkParameters])

case class IntEdge(source: IntSourcePortParameters, sink: IntSinkPortParameters)

object IntImp extends NodeImp[IntSourcePortParameters, IntSinkPortParameters, IntEdge, IntEdge, Vec[Bool]]
{
  def edgeO(pd: IntSourcePortParameters, pu: IntSinkPortParameters): IntEdge = IntEdge(pd, pu)
  def edgeI(pd: IntSourcePortParameters, pu: IntSinkPortParameters): IntEdge = IntEdge(pd, pu)
  def bundleO(eo: Seq[IntEdge]): Vec[Vec[Bool]] = {
    if (eo.isEmpty) Vec(0, Vec(0, Bool())) else
    Vec(eo.size, Vec(eo.map(_.source.num).max, Bool()))
  }
  def bundleI(ei: Seq[IntEdge]): Vec[Vec[Bool]] = {
    require (!ei.isEmpty)
    Vec(ei.size, Vec(ei.map(_.source.num).max, Bool())).flip
  }

  def connect(bo: => Vec[Bool], bi: => Vec[Bool], ei: => IntEdge)(implicit sourceInfo: SourceInfo): (Option[LazyModule], () => Unit) = {
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
case class IntOutputNode() extends OutputNode(IntImp)
case class IntInputNode() extends InputNode(IntImp)

case class IntSourceNode(num: Int) extends SourceNode(IntImp)(
  IntSourcePortParameters(Seq(IntSourceParameters(num))), (if (num == 0) 0 else 1) to 1)
case class IntSinkNode() extends SinkNode(IntImp)(
  IntSinkPortParameters(Seq(IntSinkParameters())))

case class IntAdapterNode(
  sourceFn:       Seq[IntSourcePortParameters] => IntSourcePortParameters,
  sinkFn:         Seq[IntSinkPortParameters]   => IntSinkPortParameters,
  numSourcePorts: Range.Inclusive = 1 to 1,
  numSinkPorts:   Range.Inclusive = 1 to 1)
  extends InteriorNode(IntImp)(sourceFn, sinkFn, numSourcePorts, numSinkPorts)

class IntXbar extends LazyModule
{
  val intnode = IntAdapterNode(
    numSourcePorts = 1 to 1, // does it make sense to have more than one interrupt sink?
    numSinkPorts   = 1 to 128,
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
