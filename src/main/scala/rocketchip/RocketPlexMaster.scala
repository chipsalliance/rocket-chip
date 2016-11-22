// See LICENSE for license details.

package rocketchip

import Chisel._
import config._
import diplomacy._
import uncore.tilelink2._
import uncore.devices._
import util._
import coreplex._

trait RocketPlexMaster extends TopNetwork {
  val module: RocketPlexMasterModule
  val mem: Seq[TLInwardNode]

  val coreplex = LazyModule(new DefaultCoreplex)

  socBus.node := coreplex.mmio
  coreplex.mmioInt := intBus.intnode
  (mem zip coreplex.mem) foreach { case (m, c) => m := c }
}

trait RocketPlexMasterBundle extends TopNetworkBundle {
  val outer: RocketPlexMaster
}

trait RocketPlexMasterModule extends TopNetworkModule {
  val outer: RocketPlexMaster
  val io: RocketPlexMasterBundle
}
