// See LICENSE.SiFive for license details.

package coreplex

import Chisel._
import config._
import diplomacy._
import rocket._
import uncore.tilelink2._

case object RocketConfigs extends Field[Seq[RocketConfig]]

trait RocketTiles extends CoreplexRISCVPlatform {
  val module: RocketTilesModule

  val rocketTiles = p(RocketConfigs).map { c =>
    LazyModule(new RocketTile(c)(p.alterPartial {
      case SharedMemoryTLEdge => l1tol2.node.edgesIn(0)
      case PAddrBits => l1tol2.node.edgesIn(0).bundle.addressBits
  }))}

  rocketTiles.foreach { r =>
    r.masterNodes.foreach { l1tol2.node := _ }
    r.slaveNode.foreach { _ := cbus.node }
  }

  val tileIntNodes = rocketTiles.map { _ => IntInternalOutputNode() }
  tileIntNodes.foreach { _ := plic.intnode }
}

trait RocketTilesBundle extends CoreplexRISCVPlatformBundle {
  val outer: RocketTiles
}

trait RocketTilesModule extends CoreplexRISCVPlatformModule {
  val outer: RocketTiles
  val io: RocketTilesBundle

  outer.rocketTiles.map(_.module).zipWithIndex.foreach { case (tile, i) =>
    tile.io.hartid := UInt(i)
    tile.io.resetVector := io.resetVector
    tile.io.interrupts := outer.clint.module.io.tiles(i)
    tile.io.interrupts.debug := outer.debug.module.io.debugInterrupts(i)
    tile.io.interrupts.meip := outer.tileIntNodes(i).bundleOut(0)(0)
    tile.io.interrupts.seip.foreach(_ := outer.tileIntNodes(i).bundleOut(0)(1))
  }
}

trait AsyncRocketTiles extends CoreplexRISCVPlatform {
  val module: AsyncRocketTilesModule

  val rocketTiles = p(RocketConfigs).map { c =>
    LazyModule(new AsyncRocketTile(c)(p.alterPartial {
      case SharedMemoryTLEdge => l1tol2.node.edgesIn(0)
      case PAddrBits => l1tol2.node.edgesIn(0).bundle.addressBits
  }))}

  rocketTiles.foreach { r =>
    r.masterNodes.foreach { l1tol2.node := TLAsyncCrossingSink()(_) }
    r.slaveNode.foreach { _ := TLAsyncCrossingSource()(cbus.node) }
  }

  val tileIntNodes = rocketTiles.map { _ => IntInternalOutputNode() }
  tileIntNodes.foreach { _ := plic.intnode }
}

trait AsyncRocketTilesBundle extends CoreplexRISCVPlatformBundle {
  val outer: AsyncRocketTiles

  val tcrs = Vec(nTiles, new Bundle {
    val clock = Clock(INPUT)
    val reset = Bool(INPUT)
  })
}

trait AsyncRocketTilesModule extends CoreplexRISCVPlatformModule {
  val outer: AsyncRocketTiles
  val io: AsyncRocketTilesBundle

  outer.rocketTiles.map(_.module).zipWithIndex.foreach { case (tile, i) =>
    tile.clock := io.tcrs(i).clock
    tile.reset := io.tcrs(i).reset
    tile.io.hartid := UInt(i)
    tile.io.resetVector := io.resetVector
    tile.io.interrupts := outer.clint.module.io.tiles(i)
    tile.io.interrupts.debug := outer.debug.module.io.debugInterrupts(i)
    tile.io.interrupts.meip := outer.tileIntNodes(i).bundleOut(0)(0)
    tile.io.interrupts.seip.foreach(_ := outer.tileIntNodes(i).bundleOut(0)(1))
  }
}
