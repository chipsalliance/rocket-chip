package coreplex

import Chisel._
import config._
import diplomacy._
import uncore.tilelink2._
import uncore.coherence._
import rocket._
import uncore.devices.NTiles

trait RocketTiles extends CoreplexRISCVPlatform {
  val module: RocketTilesModule

  val rocketTiles = List.tabulate(p(NTiles)) { i => LazyModule(new RocketTile(i)) }
  val tileIntNodes = rocketTiles.map { _ => IntInternalOutputNode() }

  tileIntNodes.foreach { _ := plic.intnode }
  rocketTiles.foreach { r =>
    r.slaveNode.foreach { _ := cbus.node }
    l1tol2.node := r.cachedOut
    l1tol2.node := r.uncachedOut
  }
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

class AsyncRocketTile(tileId: Int)(implicit p: Parameters) extends LazyModule {
  val rocket = LazyModule(new RocketTile(tileId))

  val cachedOut = TLAsyncOutputNode()
  val uncachedOut = TLAsyncOutputNode()
  val slaveNode = rocket.slaveNode.map(_ => TLAsyncInputNode())

  cachedOut := TLAsyncCrossingSource()(rocket.cachedOut)
  uncachedOut := TLAsyncCrossingSource()(rocket.uncachedOut)
  (rocket.slaveNode zip slaveNode) foreach { case (r,n) => r := TLAsyncCrossingSink()(n) }

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val cached = cachedOut.bundleOut
      val uncached = uncachedOut.bundleOut
      val slave = slaveNode.map(_.bundleIn)
      val hartid = UInt(INPUT, p(XLen))
      val interrupts = new TileInterrupts()(rocket.coreParams).asInput
      val resetVector = UInt(INPUT, p(XLen))
    }
    rocket.module.io.interrupts := ShiftRegister(io.interrupts, 3)
    // signals that do not change:
    rocket.module.io.hartid := io.hartid
    rocket.module.io.resetVector := io.resetVector
  }
}

trait AsyncRocketTiles extends CoreplexRISCVPlatform {
  val module: AsyncRocketTilesModule

  val rocketTiles = List.tabulate(p(NTiles)) { i => LazyModule(new AsyncRocketTile(i)) }
  val tileIntNodes = rocketTiles.map { _ => IntInternalOutputNode() }

  tileIntNodes.foreach { _ := plic.intnode }
  rocketTiles.foreach { r =>
    r.slaveNode.foreach { _ := TLAsyncCrossingSource()(cbus.node) }
    l1tol2.node := TLAsyncCrossingSink()(r.cachedOut)
    l1tol2.node := TLAsyncCrossingSink()(r.uncachedOut)
  }
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
