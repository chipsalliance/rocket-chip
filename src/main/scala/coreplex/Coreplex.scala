package coreplex

import Chisel._
import cde.{Parameters, Field}
import junctions._
import uncore.tilelink._
import uncore.util._
import util._
import rocket._

trait DirectConnection {
  val tiles: Seq[Tile]
  val uncoreTileIOs: Seq[TileIO]

  val tlBuffering = TileLinkDepths(1,1,2,2,0)
  val utlBuffering = UncachedTileLinkDepths(2,2)

  (tiles zip uncoreTileIOs) foreach { case (tile, uncore) =>
    (uncore.cached zip tile.io.cached) foreach { case (u, t) => u <> TileLinkEnqueuer(t, tlBuffering) }
    (uncore.uncached zip tile.io.uncached) foreach { case (u, t) => u <> TileLinkEnqueuer(t, utlBuffering) }
    tile.io.slave.foreach { _ <> TileLinkEnqueuer(uncore.slave.get, 1) }

    tile.io.interrupts <> uncore.interrupts

    tile.io.hartid := uncore.hartid
    tile.io.resetVector := uncore.resetVector
  }
}

class DefaultCoreplex(c: CoreplexConfig)(implicit p: Parameters) extends BaseCoreplex(c)(p) {
  override lazy val module = Module(new DefaultCoreplexModule(c, this, new DefaultCoreplexBundle(c)(p))(p))
}

class DefaultCoreplexBundle(c: CoreplexConfig)(implicit p: Parameters) extends BaseCoreplexBundle(c)(p)

class DefaultCoreplexModule[+L <: DefaultCoreplex, +B <: DefaultCoreplexBundle](
    c: CoreplexConfig, l: L, b: => B)(implicit p: Parameters) extends BaseCoreplexModule(c, l, b)(p)
    with DirectConnection

/////

trait TileClockResetBundle {
  val c: CoreplexConfig
  val tcrs = Vec(c.nTiles, new Bundle {
    val clock = Clock(INPUT)
    val reset = Bool(INPUT)
  })
}

trait AsyncConnection {
  val io: TileClockResetBundle
  val tiles: Seq[Tile]
  val uncoreTileIOs: Seq[TileIO]

  (tiles, uncoreTileIOs, io.tcrs).zipped foreach { case (tile, uncore, tcr) =>
    tile.clock := tcr.clock
    tile.reset := tcr.reset

    (uncore.cached zip tile.io.cached) foreach { case (u, t) => u <> AsyncTileLinkFrom(tcr.clock, tcr.reset, t) }
    (uncore.uncached zip tile.io.uncached) foreach { case (u, t) => u <> AsyncUTileLinkFrom(tcr.clock, tcr.reset, t) }
    tile.io.slave.foreach { _ <> AsyncUTileLinkTo(tcr.clock, tcr.reset, uncore.slave.get)}

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

class MultiClockCoreplex(c: CoreplexConfig)(implicit p: Parameters) extends BaseCoreplex(c)(p) {
  override lazy val module = Module(new MultiClockCoreplexModule(c, this, new MultiClockCoreplexBundle(c)(p))(p))
}

class MultiClockCoreplexBundle(c: CoreplexConfig)(implicit p: Parameters) extends BaseCoreplexBundle(c)(p)
    with TileClockResetBundle

class MultiClockCoreplexModule[+L <: MultiClockCoreplex, +B <: MultiClockCoreplexBundle](
    c: CoreplexConfig, l: L, b: => B)(implicit p: Parameters) extends BaseCoreplexModule(c, l, b)(p)
    with AsyncConnection
