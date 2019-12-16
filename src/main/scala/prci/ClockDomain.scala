package freechips.rocketchip.prci

import chisel3._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

abstract class ClockDomain(implicit p: Parameters) extends LazyModule
  with LazyScope
  with HasClockDomainCrossing
{
  def clockBundle: ClockBundle

  lazy val module = new LazyRawModuleImp(this) {
    childClock := clockBundle.clock
    childReset := clockBundle.reset
    val clock = clockBundle.clock
    val reset = clockBundle.reset
  }
}

class ClockSinkDomain(take: Option[ClockParameters] = None)(implicit p: Parameters) extends ClockDomain
{
  val clockNode = ClockSinkNode(Seq(ClockSinkParameters(take = take)))
  def clockBundle = clockNode.in.head._1
}

class ClockSourceDomain(give: Option[ClockParameters] = None)(implicit p: Parameters) extends ClockDomain
{
  val clockNode = ClockSourceNode(Seq(ClockSourceParameters(give = give)))
  def clockBundle = clockNode.out.head._1
}
