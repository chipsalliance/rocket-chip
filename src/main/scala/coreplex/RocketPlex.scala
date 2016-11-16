package coreplex

import Chisel._
import cde.{Parameters, Field}
import diplomacy._
import uncore.tilelink2._
import uncore.coherence._
import rocket._
import uncore.devices.NTiles

trait RocketPlex extends CoreplexRISCVPlatform {
  val module: RocketPlexModule

  val rocketTiles = List.tabulate(p(NTiles)) { i => LazyModule(new RocketTile(i)) }
  val tileIntNodes = rocketTiles.map { _ => IntInternalOutputNode() }

  tileIntNodes.foreach { _ := plic.intnode }
  rocketTiles.foreach { r =>
    r.slaveNode.foreach { _ := cbus.node }
    l1tol2.node := r.cachedOut
    l1tol2.node := r.uncachedOut
  }
}

trait RocketPlexBundle extends CoreplexRISCVPlatformBundle {
  val outer: CoreplexRISCVPlatform
}

trait RocketPlexModule extends CoreplexRISCVPlatformModule {
  val outer: RocketPlex
  val io: RocketPlexBundle

  outer.rocketTiles.map(_.module).zipWithIndex.foreach { case (tile, i) =>
    tile.io.hartid := UInt(i)
    tile.io.resetVector := io.resetVector
    tile.io.interrupts := outer.clint.module.io.tiles(i)
    tile.io.interrupts.debug := outer.debug.module.io.debugInterrupts(i)
    tile.io.interrupts.meip := outer.tileIntNodes(i).bundleOut(0)(0)
    tile.io.interrupts.seip.foreach(_ := outer.tileIntNodes(i).bundleOut(0)(1))
  }
}
