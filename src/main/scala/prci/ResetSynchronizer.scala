// See LICENSE for license details.
package freechips.rocketchip.prci

import org.chipsalliance.cde.config.{Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util.{ResetCatchAndSync}

/**
  * Synchronizes the reset of a diplomatic clock-reset pair to its accompanying clock.
  */
class ResetSynchronizer(implicit p: Parameters) extends LazyModule {
  val node = ClockAdapterNode()
  lazy val module = new Impl
  class Impl extends LazyRawModuleImp(this) {
    (node.out zip node.in).map { case ((o, _), (i, _)) =>
      o.clock := i.clock
      o.reset := ResetCatchAndSync(i.clock, i.reset.asBool)
    }
  }
}

object ResetSynchronizer {
  def apply()(implicit p: Parameters, valName: ValName) = LazyModule(new ResetSynchronizer()).node
}


/**
  * Instantiates a reset synchronizer on all clock-reset pairs in a clock group.
  */
class ClockGroupResetSynchronizer(implicit p: Parameters) extends LazyModule {
  val node = ClockGroupAdapterNode()
  lazy val module = new Impl
  class Impl extends LazyRawModuleImp(this) {
    (node.out zip node.in).map { case ((oG, _), (iG, _)) =>
      (oG.member.data zip iG.member.data).foreach { case (o, i) =>
        o.clock := i.clock
        o.reset := ResetCatchAndSync(i.clock, i.reset.asBool)
      }
    }
  }
}

object ClockGroupResetSynchronizer {
  def apply()(implicit p: Parameters, valName: ValName) = LazyModule(new ClockGroupResetSynchronizer()).node
}
