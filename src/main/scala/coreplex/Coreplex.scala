package coreplex

import Chisel._
import cde.{Parameters, Field}
import junctions._
import diplomacy._
import uncore.tilelink._
import uncore.tilelink2._
import uncore.util._
import util._
import rocket._

trait BroadcastL2 {
    this: CoreplexNetwork =>
  def l2ManagerFactory() = {
    val bh = LazyModule(new TLBroadcast(l1tol2_lineBytes, nTrackersPerBank))
    (bh.node, bh.node)
  }
}

/////

trait DirectConnection {
    this: CoreplexNetwork with CoreplexRISCVPlatform =>

  lazyTiles foreach { t =>
    t.slaveNode.foreach { _ := cbus.node }
    l1tol2.node := TLBuffer(1,1,2,2,0)(TLHintHandler()(t.cachedOut))
    l1tol2.node := TLBuffer(1,0,0,2,0)(TLHintHandler()(t.uncachedOut))
  }
}

trait DirectConnectionModule {
  this: CoreplexNetworkModule with CoreplexRISCVPlatformModule {
    val outer: CoreplexNetwork with CoreplexRISCVPlatform
    val io: CoreplexRISCVPlatformBundle
  } =>

  // connect coreplex-internal interrupts to tiles
  tiles.zipWithIndex.foreach { case (tile, i) =>
    tile.io.hartid := UInt(i)
    tile.io.resetVector := io.resetVector
    tile.io.interrupts := outer.clint.module.io.tiles(i)
    tile.io.interrupts.debug := outer.debug.module.io.debugInterrupts(i)
    tile.io.interrupts.meip := outer.tileIntNodes(i).bundleOut(0)(0)
    tile.io.interrupts.seip.foreach(_ := outer.tileIntNodes(i).bundleOut(0)(1))
  }
}

class DefaultCoreplex(implicit p: Parameters) extends BaseCoreplex
    with BroadcastL2
    with DirectConnection {
  override lazy val module = new DefaultCoreplexModule(this, () => new DefaultCoreplexBundle(this))
}

class DefaultCoreplexBundle[+L <: DefaultCoreplex](_outer: L) extends BaseCoreplexBundle(_outer)

class DefaultCoreplexModule[+L <: DefaultCoreplex, +B <: DefaultCoreplexBundle[L]](_outer: L, _io: () => B) extends BaseCoreplexModule(_outer, _io)
    with DirectConnectionModule

/////

trait AsyncConnection {
    this: CoreplexNetwork with CoreplexRISCVPlatform =>

  val masterCrossings = lazyTiles.map { t =>
    t.masterNodes map { m =>
      val crossing = LazyModule(new TLAsyncCrossing)
      crossing.node := m
      val monitor = (cbus.node := crossing.node)
      (crossing, monitor)
    }
  }

  val slaveCrossings = lazyTiles.map { t =>
    t.slaveNode map { s =>
      val crossing = LazyModule(new TLAsyncCrossing)
      crossing.node := cbus.node
      val monitor = (s := crossing.node)
      (crossing, monitor)
    }
  }
}

trait AsyncConnectionBundle {
    this: CoreplexNetworkBundle with CoreplexRISCVPlatformBundle =>
  val tcrs = Vec(nTiles, new Bundle {
    val clock = Clock(INPUT)
    val reset = Bool(INPUT)
  })
}

trait AsyncConnectionModule {
  this: Module with CoreplexNetworkModule with CoreplexRISCVPlatformModule {
    val outer: AsyncConnection with CoreplexNetwork with CoreplexRISCVPlatform
    val io: AsyncConnectionBundle with CoreplexNetworkBundle with CoreplexRISCVPlatformBundle
  } =>

  (outer.masterCrossings zip io.tcrs) foreach { case (masters, tcr) =>
    masters.foreach { case (crossing, monitor) =>
      crossing.module.io.out_clock  := clock
      crossing.module.io.out_reset  := reset
      crossing.module.io.in_clock := tcr.clock
      crossing.module.io.in_reset := tcr.reset
      monitor.foreach { m =>
        m.module.clock := clock
        m.module.reset := reset
      }
    }
  }

  (outer.slaveCrossings zip io.tcrs) foreach { case (slaves, tcr) =>
    slaves.foreach { case (crossing, monitor) =>
      crossing.module.io.in_clock  := clock
      crossing.module.io.in_reset  := reset
      crossing.module.io.out_clock := tcr.clock
      crossing.module.io.out_reset := tcr.reset
      monitor.foreach { m =>
        m.module.clock := tcr.clock
        m.module.reset := tcr.reset
      }
    }
  }

  (tiles.zipWithIndex, io.tcrs).zipped.foreach { case ((tile, i), tcr) =>
    tile.clock := tcr.clock
    tile.reset := tcr.reset

    val ti = tile.io.interrupts
    ti.debug := LevelSyncTo(tcr.clock, outer.debug.module.io.debugInterrupts(i))
    ti.mtip := LevelSyncTo(tcr.clock, outer.clint.module.io.tiles(i).mtip)
    ti.msip := LevelSyncTo(tcr.clock, outer.clint.module.io.tiles(i).msip)
    ti.meip := LevelSyncTo(tcr.clock, outer.tileIntNodes(i).bundleOut(0)(0))
    ti.seip.foreach { _ := LevelSyncTo(tcr.clock, outer.tileIntNodes(i).bundleOut(0)(1)) }

    tile.io.hartid := UInt(i)
    tile.io.resetVector := io.resetVector 
  }
}

class MultiClockCoreplex(implicit p: Parameters) extends BaseCoreplex
    with BroadcastL2
    with AsyncConnection {
  override lazy val module = new MultiClockCoreplexModule(this, () => new MultiClockCoreplexBundle(this))
}

class MultiClockCoreplexBundle[+L <: MultiClockCoreplex](_outer: L) extends BaseCoreplexBundle(_outer)
    with AsyncConnectionBundle

class MultiClockCoreplexModule[+L <: MultiClockCoreplex, +B <: MultiClockCoreplexBundle[L]](_outer: L, _io: () => B) extends BaseCoreplexModule(_outer, _io)
    with AsyncConnectionModule
