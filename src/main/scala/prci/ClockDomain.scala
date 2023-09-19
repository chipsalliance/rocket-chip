package freechips.rocketchip.prci

import chisel3._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._

abstract class Domain(implicit p: Parameters) extends LazyModule with HasDomainCrossing {
  def clockBundle: ClockBundle

  lazy val module = new Impl
  class Impl extends LazyRawModuleImp(this) {
    childClock := clockBundle.clock
    childReset := clockBundle.reset

    // these are just for backwards compatibility with external devices
    // that were manually wiring themselves to the domain's clock/reset input:
    val clock = IO(Output(chiselTypeOf(clockBundle.clock)))
    val reset = IO(Output(chiselTypeOf(clockBundle.reset)))
    clock := clockBundle.clock
    reset := clockBundle.reset
  }
}

abstract class ClockDomain(implicit p: Parameters) extends Domain with HasClockDomainCrossing

class ClockSinkDomain(val clockSinkParams: ClockSinkParameters)(implicit p: Parameters) extends ClockDomain
{
  def this(take: Option[ClockParameters] = None, name: Option[String] = None)(implicit p: Parameters) = this(ClockSinkParameters(take = take, name = name))
  val clockNode = ClockSinkNode(Seq(clockSinkParams))
  def clockBundle = clockNode.in.head._1
}

class ClockSourceDomain(val clockSourceParams: ClockSourceParameters)(implicit p: Parameters) extends ClockDomain
{
  def this(give: Option[ClockParameters] = None, name: Option[String] = None)(implicit p: Parameters) = this(ClockSourceParameters(give = give, name = name))
  val clockNode = ClockSourceNode(Seq(clockSourceParams))
  def clockBundle = clockNode.out.head._1
}

abstract class ResetDomain(implicit p: Parameters) extends Domain with HasResetDomainCrossing
