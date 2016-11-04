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
  lazyTiles.map(_.slave).flatten.foreach { scratch => scratch := cbus.node }
}

trait DirectConnectionModule {
    this: CoreplexNetworkModule with CoreplexRISCVPlatformModule =>

  val tlBuffering = TileLinkDepths(1,1,2,2,0)
  val ultBuffering = UncachedTileLinkDepths(1,2)

  (tiles zip uncoreTileIOs) foreach { case (tile, uncore) =>
    (uncore.cached zip tile.io.cached) foreach { case (u, t) => u <> TileLinkEnqueuer(t, tlBuffering) }
    (uncore.uncached zip tile.io.uncached) foreach { case (u, t) => u <> TileLinkEnqueuer(t, ultBuffering) }

    tile.io.interrupts <> uncore.interrupts

    tile.io.hartid := uncore.hartid
    tile.io.resetVector := uncore.resetVector
  }
}

class DefaultCoreplex(implicit p: Parameters) extends BaseCoreplex
    with DirectConnection {
  override lazy val module = new DefaultCoreplexModule(this, () => new DefaultCoreplexBundle(this))
}

class DefaultCoreplexBundle[+L <: DefaultCoreplex](_outer: L) extends BaseCoreplexBundle(_outer)

class DefaultCoreplexModule[+L <: DefaultCoreplex, +B <: DefaultCoreplexBundle[L]](_outer: L, _io: () => B) extends BaseCoreplexModule(_outer, _io)
    with DirectConnectionModule

/////

trait AsyncConnection {
    this: CoreplexNetwork with CoreplexRISCVPlatform =>
  val crossings = lazyTiles.map(_.slave).map(_.map { scratch =>
    val crossing = LazyModule(new TLAsyncCrossing)
    crossing.node := cbus.node
    val monitor = (scratch := crossing.node)
    (crossing, monitor)
  })
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
    val outer: AsyncConnection
    val io: AsyncConnectionBundle
  } =>

  (outer.crossings zip io.tcrs) foreach { case (slaves, tcr) =>
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

  (tiles, uncoreTileIOs, io.tcrs).zipped foreach { case (tile, uncore, tcr) =>
    tile.clock := tcr.clock
    tile.reset := tcr.reset

    (uncore.cached zip tile.io.cached) foreach { case (u, t) => u <> AsyncTileLinkFrom(tcr.clock, tcr.reset, t) }
    (uncore.uncached zip tile.io.uncached) foreach { case (u, t) => u <> AsyncUTileLinkFrom(tcr.clock, tcr.reset, t) }

    val ti = tile.io.interrupts
    val ui = uncore.interrupts
    ti.debug := LevelSyncTo(tcr.clock, ui.debug)
    ti.mtip := LevelSyncTo(tcr.clock, ui.mtip)
    ti.msip := LevelSyncTo(tcr.clock, ui.msip)
    ti.meip := LevelSyncTo(tcr.clock, ui.meip)
    ti.seip.foreach { _ := LevelSyncTo(tcr.clock, ui.seip.get) }

    tile.io.hartid := uncore.hartid
    tile.io.resetVector := uncore.resetVector
  }
}

class MultiClockCoreplex(implicit p: Parameters) extends BaseCoreplex
    with AsyncConnection {
  override lazy val module = new MultiClockCoreplexModule(this, () => new MultiClockCoreplexBundle(this))
}

class MultiClockCoreplexBundle[+L <: MultiClockCoreplex](_outer: L) extends BaseCoreplexBundle(_outer)
    with AsyncConnectionBundle

class MultiClockCoreplexModule[+L <: MultiClockCoreplex, +B <: MultiClockCoreplexBundle[L]](_outer: L, _io: () => B) extends BaseCoreplexModule(_outer, _io)
    with AsyncConnectionModule
