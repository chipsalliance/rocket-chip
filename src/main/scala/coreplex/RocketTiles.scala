// See LICENSE.SiFive for license details.

package coreplex

import Chisel._
import config._
import diplomacy._
import rocket._
import uncore.tilelink2._

case object RocketConfigs extends Field[Seq[RocketConfig]]

trait HasSynchronousRocketTiles extends CoreplexRISCVPlatform {
  val module: HasSynchronousRocketTilesModule

  val rocketTiles: Seq[RocketTile] = p(RocketConfigs).map { c =>
    LazyModule(new RocketTile(c)(p.alterPartial {
      case SharedMemoryTLEdge => l1tol2.node.edgesIn(0)
      case PAddrBits => l1tol2.node.edgesIn(0).bundle.addressBits
  }))}

  rocketTiles.foreach { r =>
    r.masterNodes.foreach { l1tol2.node := TLBuffer()(_) }
    r.slaveNode.foreach { _ := cbus.node }
  }

  val rocketTileIntNodes = rocketTiles.map { _ => IntInternalOutputNode() }
  rocketTileIntNodes.foreach { _ := plic.intnode }
}

trait HasSynchronousRocketTilesBundle extends CoreplexRISCVPlatformBundle {
  val outer: HasSynchronousRocketTiles
}

trait HasSynchronousRocketTilesModule extends CoreplexRISCVPlatformModule {
  val outer: HasSynchronousRocketTiles
  val io: HasSynchronousRocketTilesBundle

  outer.rocketTiles.map(_.module).zipWithIndex.foreach { case (tile, i) =>
    tile.io.hartid := UInt(i)
    tile.io.resetVector := io.resetVector
    tile.io.interrupts := outer.clint.module.io.tiles(i)
    tile.io.interrupts.debug := outer.debug.module.io.debugInterrupts(i)
    tile.io.interrupts.meip := outer.rocketTileIntNodes(i).bundleOut(0)(0)
    tile.io.interrupts.seip.foreach(_ := outer.rocketTileIntNodes(i).bundleOut(0)(1))
  }
}

trait HasAsynchronousRocketTiles extends CoreplexRISCVPlatform {
  val module: HasAsynchronousRocketTilesModule

  val rocketTiles: Seq[AsyncRocketTile] = p(RocketConfigs).map { c =>
    LazyModule(new AsyncRocketTile(c)(p.alterPartial {
      case SharedMemoryTLEdge => l1tol2.node.edgesIn(0)
      case PAddrBits => l1tol2.node.edgesIn(0).bundle.addressBits
  }))}

  rocketTiles.foreach { r =>
    r.masterNodes.foreach { l1tol2.node := TLAsyncCrossingSink()(_) }
    r.slaveNode.foreach { _ := TLAsyncCrossingSource()(cbus.node) }
  }

  val rocketTileIntNodes = rocketTiles.map { _ => IntInternalOutputNode() }
  rocketTileIntNodes.foreach { _ := plic.intnode }
}

trait HasAsynchronousRocketTilesBundle extends CoreplexRISCVPlatformBundle {
  val outer: HasAsynchronousRocketTiles

  val tcrs = Vec(nTiles, new Bundle {
    val clock = Clock(INPUT)
    val reset = Bool(INPUT)
  })
}

trait HasAsynchronousRocketTilesModule extends CoreplexRISCVPlatformModule {
  val outer: HasAsynchronousRocketTiles
  val io: HasAsynchronousRocketTilesBundle

  outer.rocketTiles.map(_.module).zipWithIndex.foreach { case (tile, i) =>
    tile.clock := io.tcrs(i).clock
    tile.reset := io.tcrs(i).reset
    tile.io.hartid := UInt(i)
    tile.io.resetVector := io.resetVector
    tile.io.interrupts := outer.clint.module.io.tiles(i)
    tile.io.interrupts.debug := outer.debug.module.io.debugInterrupts(i)
    tile.io.interrupts.meip := outer.rocketTileIntNodes(i).bundleOut(0)(0)
    tile.io.interrupts.seip.foreach(_ := outer.rocketTileIntNodes(i).bundleOut(0)(1))
  }
}
