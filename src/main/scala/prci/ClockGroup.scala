// See LICENSE.SiFive for license details.
package freechips.rocketchip.prci

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._

case class ClockGroupNode(groupName: String)(implicit valName: ValName)
  extends MixedNexusNode(ClockGroupImp, ClockImp)(
    dFn = { _ => ClockSourceParameters() },
    uFn = { seq => ClockGroupSinkParameters(name = groupName, members = seq) })
{
  override def circuitIdentity = outputs.size == 1
}

class ClockGroup(groupName: String)(implicit p: Parameters) extends LazyModule
{
  val node = ClockGroupNode(groupName)

  lazy val module = new Impl
  class Impl extends LazyRawModuleImp(this) {
    val (in, _) = node.in(0)
    val (out, _) = node.out.unzip

    require (node.in.size == 1)
    require (in.member.size == out.size)

    (in.member.data zip out) foreach { case (i, o) => o := i }
  }
}

object ClockGroup
{
  def apply()(implicit p: Parameters, valName: ValName) = LazyModule(new ClockGroup(valName.name)).node
}

case class ClockGroupAggregateNode(groupName: String)(implicit valName: ValName)
  extends NexusNode(ClockGroupImp)(
    dFn = { _ => ClockGroupSourceParameters() },
    uFn = { seq => ClockGroupSinkParameters(name = groupName, members = seq.flatMap(_.members))})
{
  override def circuitIdentity = outputs.size == 1
}

class ClockGroupAggregator(groupName: String)(implicit p: Parameters) extends LazyModule
{
  val node = ClockGroupAggregateNode(groupName)

  lazy val module = new Impl
  class Impl extends LazyRawModuleImp(this) {
    val (in, _) = node.in.unzip
    val (out, _) = node.out.unzip
    val outputs = out.flatMap(_.member.data)

    require (node.in.size == 1, s"Aggregator for groupName: ${groupName} had ${node.in.size} inward edges instead of 1")
    require (in.head.member.size == outputs.size)
    in.head.member.data.zip(outputs).foreach { case (i, o) => o := i }
  }
}

object ClockGroupAggregator
{
  def apply()(implicit p: Parameters, valName: ValName) = LazyModule(new ClockGroupAggregator(valName.name)).node
}

class SimpleClockGroupSource(numSources: Int = 1)(implicit p: Parameters) extends LazyModule
{
  val node = ClockGroupSourceNode(List.fill(numSources) { ClockGroupSourceParameters() })

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {

    val (out, _) = node.out.unzip
    out.map { out: ClockGroupBundle =>
      out.member.data.foreach { o =>
        o.clock := clock; o.reset := reset }
    }
  }
}

object SimpleClockGroupSource
{
  def apply(num: Int = 1)(implicit p: Parameters, valName: ValName) = LazyModule(new SimpleClockGroupSource(num)).node
}

case class FixedClockBroadcastNode(fixedClockOpt: Option[ClockParameters])(implicit valName: ValName)
  extends NexusNode(ClockImp)(
    dFn = { seq => fixedClockOpt.map(_ => ClockSourceParameters(give = fixedClockOpt)).orElse(seq.headOption).getOrElse(ClockSourceParameters()) },
    uFn = { seq => fixedClockOpt.map(_ =>   ClockSinkParameters(take = fixedClockOpt)).orElse(seq.headOption).getOrElse(ClockSinkParameters()) },
    inputRequiresOutput = false) {
  def fixedClockResources(name: String, prefix: String = "soc/"): Seq[Option[FixedClockResource]] = Seq(fixedClockOpt.map(t => new FixedClockResource(name, t.freqMHz, prefix)))
}

class FixedClockBroadcast(fixedClockOpt: Option[ClockParameters])(implicit p: Parameters) extends LazyModule
{
  val node = new FixedClockBroadcastNode(fixedClockOpt) {
    override def circuitIdentity = outputs.size == 1
  }

  lazy val module = new Impl
  class Impl extends LazyRawModuleImp(this) {
    val (in, _) = node.in(0)
    val (out, _) = node.out.unzip
    require (node.in.size == 1, "FixedClockBroadcast can only broadcast a single clock")
    out.foreach { _ := in }
  }
}

object FixedClockBroadcast
{
  def apply(fixedClockOpt: Option[ClockParameters])(implicit p: Parameters, valName: ValName) = LazyModule(new FixedClockBroadcast(fixedClockOpt)).node
}

case class PRCIClockGroupNode()(implicit valName: ValName)
  extends NexusNode(ClockGroupImp)(
    dFn = { _ => ClockGroupSourceParameters() },
    uFn = { _ => ClockGroupSinkParameters("prci", Nil) },
    outputRequiresInput = false)
