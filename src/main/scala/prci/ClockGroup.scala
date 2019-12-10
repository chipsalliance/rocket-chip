// See LICENSE.SiFive for license details.
package freechips.rocketchip.prci

import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

case class ClockGroupNode(groupName: String)(implicit valName: ValName)
  extends MixedNexusNode(ClockGroupImp, ClockImp)(
    dFn = { _ => ClockSourceParameters() },
    uFn = { seq => ClockGroupSinkParameters(name = groupName, members = seq) })

class ClockGroup(groupName: String)(implicit p: Parameters) extends LazyModule
{
  val node = ClockGroupNode(groupName)

  lazy val module = new LazyModuleImp(this) {
    val (in, _) = node.in(0)
    val (out, _) = node.out.unzip

    require (node.in.size == 1)
    require (in.member.size == out.size)

    (in.member zip out) foreach { case (i, o) => o := i }
  }
}

object ClockGroup
{
  def apply()(implicit p: Parameters, valName: ValName) =
    LazyModule(new ClockGroup(valName.name)).node

  def pickGroupFromCrossingType(sync: ClockGroupNode, async: ClockGroupNode)(xType: ClockCrossingType) = {
    xType match {
      case _: AsynchronousCrossing => async
      case _ => sync
    }
  }
}

case class ClockGroupBroadcastNode(groupName: String)(implicit valName: ValName)
  extends NexusNode(ClockGroupImp)(
    dFn = { _ => ClockGroupSourceParameters() },
    uFn = { seq => ClockGroupSinkParameters(name = groupName, members = seq.flatMap(_.members))},
    outputRequiresInput = false)

class ClockGroupBroadcast(groupName: String)(implicit p: Parameters) extends LazyModule
{
  val node = ClockGroupBroadcastNode(groupName)

  lazy val module = new LazyModuleImp(this) {
    val (in, _) = node.in.unzip
    val (out, _) = node.out.unzip
    val outputs = out.flatMap(_.member)

    require (node.in.size <= 1)
    if (node.in.size == 0) {
      println(s"Diplomacy found the following clocks:\n${node.out.unzip._2.mkString("\n")}")
      outputs.foreach { o =>
        o.clock := clock
        o.reset := reset
      }
    } else {
      require (in.head.member.size == outputs.size)
      in.head.member.zip(outputs).foreach { case (i, o) => o := i }
    }
  }
}

object ClockGroupBroadcast
{
  def apply()(implicit p: Parameters, valName: ValName) =
    LazyModule(new ClockGroupBroadcast(valName.name)).node

  def pickGroupFromCrossingType(sync: ClockGroupBroadcastNode, async: ClockGroupBroadcastNode)(xType: ClockCrossingType) = {
    xType match {
      case _: AsynchronousCrossing => async
      case _ => sync
    }
  }
}
