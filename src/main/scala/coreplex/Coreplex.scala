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

trait DirectConnection {
  implicit val p: Parameters
  val lazyTiles: Seq[LazyTile]
  val legacy: TLLegacy
  val cbus: TLXbar

  lazyTiles.map(_.slave).flatten.foreach { scratch => scratch := cbus.node }
}

trait DirectConnectionModule {
  val tiles: Seq[TileImp]
  val uncoreTileIOs: Seq[TileIO]

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
  override lazy val module = new DefaultCoreplexModule(this, new DefaultCoreplexBundle(this))
}

class DefaultCoreplexBundle[+L <: DefaultCoreplex](outer: L) extends BaseCoreplexBundle(outer)

class DefaultCoreplexModule[+L <: DefaultCoreplex, +B <: DefaultCoreplexBundle[L]](outer: L, io: B) extends BaseCoreplexModule(outer, io)
    with DirectConnectionModule

/////

trait TileClockResetBundle extends Bundle with HasCoreplexParameters {
  val tcrs = Vec(nTiles, new Bundle {
    val clock = Clock(INPUT)
    val reset = Bool(INPUT)
  })
}

trait AsyncConnection {
  implicit val p: Parameters
  val lazyTiles: Seq[LazyTile]
  val legacy: TLLegacy
  val cbus: TLXbar

  val crossings = lazyTiles.map(_.slave).map(_.map { scratch =>
    val crossing = LazyModule(new TLAsyncCrossing)
    crossing.node := cbus.node
    val monitor = (scratch := crossing.node)
    (crossing, monitor)
  })
}

trait AsyncConnectionModule extends Module {
  val io: TileClockResetBundle
  val tiles: Seq[TileImp]
  val uncoreTileIOs: Seq[TileIO]
  val outer: AsyncConnection

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
  override lazy val module = new MultiClockCoreplexModule(this, new MultiClockCoreplexBundle(this))
}

class MultiClockCoreplexBundle[+L <: MultiClockCoreplex](outer: L) extends BaseCoreplexBundle(outer)
    with TileClockResetBundle

class MultiClockCoreplexModule[+L <: MultiClockCoreplex, +B <: MultiClockCoreplexBundle[L]](outer: L, io: B) extends BaseCoreplexModule(outer, io)
    with AsyncConnectionModule
