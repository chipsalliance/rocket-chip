// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util.SynchronizerShiftReg
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

case class IntEdge(source: IntSourcePortParameters, sink: IntSinkPortParameters, params: Parameters, sourceInfo: SourceInfo)

object IntImp extends SimpleNodeImp[IntSourcePortParameters, IntSinkPortParameters, IntEdge, Vec[Bool]]
{
  def edge(pd: IntSourcePortParameters, pu: IntSinkPortParameters, p: Parameters, sourceInfo: SourceInfo) = IntEdge(pd, pu, p, sourceInfo)
  def bundle(e: IntEdge) = Vec(e.source.num, Bool())
  def render(e: IntEdge) = RenderedEdge(colour = "#0000ff" /* blue */, label = e.source.sources.map(_.range.size).sum.toString, flipped = true)

  override def mixO(pd: IntSourcePortParameters, node: OutwardNode[IntSourcePortParameters, IntSinkPortParameters, Vec[Bool]]): IntSourcePortParameters =
   pd.copy(sources = pd.sources.map  { s => s.copy (nodePath = node +: s.nodePath) })
  override def mixI(pu: IntSinkPortParameters, node: InwardNode[IntSourcePortParameters, IntSinkPortParameters, Vec[Bool]]): IntSinkPortParameters =
   pu.copy(sinks   = pu.sinks.map    { s => s.copy (nodePath = node +: s.nodePath) })
}

case class IntSourceNode(portParams: Seq[IntSourcePortParameters])(implicit valName: ValName) extends SourceNode(IntImp)(portParams)
case class IntSinkNode(portParams: Seq[IntSinkPortParameters])(implicit valName: ValName) extends SinkNode(IntImp)(portParams)
case class IntAdapterNode(
  sourceFn: IntSourcePortParameters => IntSourcePortParameters = { s => s },
  sinkFn:   IntSinkPortParameters   => IntSinkPortParameters   = { s => s },
  num:      Range.Inclusive = 0 to 999)(
  implicit valName: ValName)
  extends AdapterNode(IntImp)(sourceFn, sinkFn, num)
case class IntIdentityNode()(implicit valName: ValName) extends IdentityNode(IntImp)()

case class IntNexusNode(
  sourceFn:       Seq[IntSourcePortParameters] => IntSourcePortParameters,
  sinkFn:         Seq[IntSinkPortParameters]   => IntSinkPortParameters,
  numSourcePorts: Range.Inclusive = 0 to 128,
  numSinkPorts:   Range.Inclusive = 0 to 128)(
  implicit valName: ValName)
  extends NexusNode(IntImp)(sourceFn, sinkFn, numSourcePorts, numSinkPorts)

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
    val cat = intnode.in.map { case (i, e) => i.take(e.source.num) }.flatten
    intnode.out.foreach { case (o, _) => o := cat }
  }
}

class IntXing(sync: Int = 3)(implicit p: Parameters) extends LazyModule
{
  val intnode = IntAdapterNode()

  lazy val module = new LazyModuleImp(this) {
    (intnode.in zip intnode.out) foreach { case ((in, _), (out, _)) =>
      out := SynchronizerShiftReg(in, sync)
    }
  }
}
