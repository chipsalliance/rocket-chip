package freechips.rocketchip.prci

import chisel3._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

class ClockSinkDomain(take: Option[ClockParameters] = None)(implicit p: Parameters) extends LazyModule
    with LazyScope
    with HasClockDomainCrossing {

  val clockNode = ClockSinkNode(Seq(ClockSinkParameters(take = take)))

  lazy val module = new LazyRawModuleImp(this) {
    val (bundle, _) = clockNode.in.head
    childClock := bundle.clock
    childReset := bundle.reset
  }
}

class ClockSourceDomain(give: Option[ClockParameters] = None)(implicit p: Parameters) extends LazyModule
    with LazyScope
    with HasClockDomainCrossing {

  val clockNode = ClockSourceNode(Seq(ClockSourceParameters(give = give)))

  lazy val module = new LazyRawModuleImp(this) {
    val (bundle, _) = clockNode.out.head
    childClock := bundle.clock
    childReset := bundle.reset
  }
}
