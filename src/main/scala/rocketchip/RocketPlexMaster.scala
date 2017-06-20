// See LICENSE.SiFive for license details.

package rocketchip

import Chisel._
import coreplex.RocketPlex
import diplomacy.{LazyModule, LazyMultiIOModuleImp}

/** Add a RocketPlex to the system */
trait HasRocketPlexMaster extends HasSystemNetworks with HasCoreplexRISCVPlatform {
  val module: HasRocketPlexMasterModuleImp

  val coreplex = LazyModule(new RocketPlex)

  coreplex.l2in :=* fsb.node
  bsb.node :*= coreplex.l2out
  socBus.node := coreplex.mmio
  coreplex.mmioInt := intBus.intnode

  require (mem.size == coreplex.mem.size)
  (mem zip coreplex.mem) foreach { case (xbar, channel) => xbar.node :=* channel }
}


trait HasRocketPlexMasterModuleImp extends LazyMultiIOModuleImp {
  val outer: HasRocketPlexMaster

  outer.coreplex.module.io.tcrs.foreach { case tcr =>
    tcr.clock := clock
    tcr.reset := reset
  }
}
