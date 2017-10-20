// See LICENSE.SiFive for license details.

package freechips.rocketchip.interrupts

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

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
