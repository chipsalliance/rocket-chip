// See LICENSE.SiFive for license details.

package freechips.rocketchip.interrupts

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
  def indexes = start until end
}

object IntRange
{
  implicit def apply(end: Int): IntRange = apply(0, end)
}

case class IntSourceParameters(
  rangeOpt:  Option[IntRange],
  resources: Seq[Resource] = Seq(),
  nodePath:  Seq[BaseNode] = Seq())
{
  val name: String = nodePath.lastOption.map(_.lazyModule.name).getOrElse("disconnected")
  val description: String = rangeOpt.map { range =>
    s"[${range.start}, ${range.end}) => $name"
  }.getOrElse { s"[unresolved range) => $name" }
}

object IntSourceParameters {
  def apply(range: Int): IntSourceParameters =
    new IntSourceParameters(Some(range), Nil, Nil)
  def apply(range: Int, resources: Seq[Resource]): IntSourceParameters =
    new IntSourceParameters(Some(range), resources, Nil)
}

case class IntSinkParameters(
  rangeOpt:  Option[IntRange] = None,
  nodePath:  Seq[BaseNode] = Seq())
{
  val name = nodePath.lastOption.map(_.lazyModule.name).getOrElse("disconnected")
}

object IntSinkParameters {
  def apply(range: Int): IntSinkParameters =
    new IntSinkParameters(Some(range), Nil)
}

case class IntSourcePortParameters(sources: Seq[IntSourceParameters])
{
  def num: Int = {
    require(sources.forall(_.rangeOpt.isDefined), "Cannot use IntSourcePortParameters.num for sink-defined ranges. Use IntEdge.num instead.")
    sources.flatMap(_.rangeOpt).map(_.size).sum
  }
}

object IntSourcePortSimple
{
  def apply(num: Int = 1, ports: Int = 1, sources: Int = 1, resources: Seq[Resource] = Nil) =
    if (num == 0) Nil
    else Seq.fill(ports)(IntSourcePortParameters(
      sources = Seq.tabulate(sources)(idx => IntSourceParameters(
        rangeOpt = Some(IntRange(idx*num, idx*num+num)), resources = resources))))
}

case class IntSinkPortParameters(sinks: Seq[IntSinkParameters])

object IntSinkPortSimple
{
  def apply(ports: Int = 1, sinks: Int = 1) =
    Seq.fill(ports)(IntSinkPortParameters(Seq.fill(sinks)(IntSinkParameters(None))))
}

case class IntEdge(source: IntSourcePortParameters, sink: IntSinkPortParameters, params: Parameters, sourceInfo: SourceInfo)
{
  val hasSinkDefinedRanges = sink.sinks.exists(_.rangeOpt.isDefined)
  require (!hasSinkDefinedRanges || source.sources.size == sink.sinks.size,
    "Interrupt sinks are only allowed to define ranges in situations where there is an exact match between the numbers of sinks and sources.")
  val resolvedSources: Seq[IntSourceParameters] = if (hasSinkDefinedRanges) {
    source.sources.zip(sink.sinks).map { case (source, sink) =>
      (source.rangeOpt, sink.rangeOpt) match {
        case (Some(sourceRange), Some(sinkRange)) =>
          require(sourceRange.size == sinkRange.size,
            s"If source and sink both specify the number of interrupts, they must agree, but these do not.\nSource: $source\nSink: $sink")
          source
        case (Some(_), None) => source // original behavior
        case (None, sinkRangeOpt) => source.copy(rangeOpt = sinkRangeOpt) // sink chooses
        case (None, None) => require(false, s"Neither source nor sink specified the number of interrupts!\nSource: $source\nSink: $sink"); source
      }
    }
  } else { source.sources }

  val ranges = resolvedSources.flatMap(_.rangeOpt)
  val num = ranges.map(_.size).sum
  val max = ranges.map(_.end).max

  ranges.combinations(2).foreach { case Seq(a, b) =>
    require (!a.overlaps(b), s"Interrupt ranges cannot overlap, but got $a and $b")
  }

  require (ranges.isEmpty || max == num,
    s"Interrupt range coverage must be complete, but max $max did not equal size $num")

  /** Bind a device (e.g. an interrupt controller) to all the source resources registered on this edge */
  def bindDevice(device: Device, label: Int => Option[Int]): Unit = {
    resolvedSources.foreach { s =>
      s.resources.foreach { r =>
        s.rangeOpt.get.indexes.map(label).flatten.foreach { i =>
          r.bind(device, ResourceInt(i))
        }
      }
    }
  }
}
