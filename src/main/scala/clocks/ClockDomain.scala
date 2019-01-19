package freechips.rocketchip.clocks

import chisel3._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

class ClockSinkDomain(take: Option[ClockParameters] = None)(implicit p: Parameters) extends LazyModule
    with LazyScope
    with HasClockDomainCrossing {

  val node = ClockSinkNode(Seq(ClockSinkParameters(take = take)))

  lazy val module = new LazyRawModuleImp(this) {
    val (bundle, edge) = node.in.head
    childClock := bundle.clock
    childReset := bundle.reset
  }
}

class ClockSourceDomain(give: Option[ClockParameters] = None)(implicit p: Parameters) extends LazyModule
    with LazyScope
    with HasClockDomainCrossing {

  val node = ClockSourceNode(Seq(ClockSourceParameters(give = give)))

  lazy val module = new LazyRawModuleImp(this) {
    val (bundle, edge) = node.out.head
    childClock := bundle.clock
    childReset := bundle.reset
  }
}
