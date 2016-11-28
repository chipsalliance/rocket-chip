// See LICENSE.SiFive for license details.

package rocketchip

import Chisel._
import config._
import diplomacy._
import uncore.tilelink2._
import uncore.devices._
import util._
import coreplex._

trait RocketPlexMaster extends L2Crossbar {
  val module: RocketPlexMasterModule
  val mem: Seq[TLInwardNode]

  val coreplex = LazyModule(new DefaultCoreplex)

  coreplex.l2in := l2.node
  socBus.node := coreplex.mmio
  coreplex.mmioInt := intBus.intnode
  (mem zip coreplex.mem) foreach { case (m, c) => m := c }
}

trait RocketPlexMasterBundle extends L2CrossbarBundle {
  val outer: RocketPlexMaster
}

trait RocketPlexMasterModule extends L2CrossbarModule {
  val outer: RocketPlexMaster
  val io: RocketPlexMasterBundle
}
