package coreplex

import Chisel._
import cde.{Parameters, Field}
import junctions._
import uncore.tilelink._
import uncore.util._
import uncore.devices.{DebugBusIO, AsyncDebugBusFrom}
import rocket._

trait DirectConnection {
  val tiles: Seq[Tile]
  val uncoreTileIOs: Seq[TileIO]

  val io: BaseCoreplexBundle
  val uncoreExtMem: Vec[ClientUncachedTileLinkIO]
  val uncoreExtMMIO: ClientUncachedTileLinkIO
  val uncoreExtSlave: Vec[ClientUncachedTileLinkIO]
  val uncoreExtDebug: DebugBusIO

  val tlBuffering = TileLinkDepths(1,1,2,2,0)
  val ultBuffering = UncachedTileLinkDepths(1,2)

  (tiles zip uncoreTileIOs) foreach { case (tile, uncore) =>
    (uncore.cached zip tile.io.cached) foreach { case (u, t) => u <> TileLinkEnqueuer(t, tlBuffering)(t.p) }
    (uncore.uncached zip tile.io.uncached) foreach { case (u, t) => u <> TileLinkEnqueuer(t, ultBuffering)(t.p) }
    tile.io.slave.foreach { _ <> TileLinkEnqueuer(uncore.slave.get, 1)(uncore.slave.get.p) }

    tile.io.interrupts <> uncore.interrupts

    tile.io.hartid := uncore.hartid
    tile.io.resetVector := uncore.resetVector
  }

  io.master.mem <> uncoreExtMem
  io.master.mmio <> uncoreExtMMIO
  uncoreExtSlave <> io.slave
  uncoreExtDebug <> io.debug
}

class DefaultCoreplex(c: CoreplexConfig)(implicit p: Parameters) extends BaseCoreplex(c)(p) {
  override lazy val module = Module(new DefaultCoreplexModule(c, this, new DefaultCoreplexBundle(c)(p))(p))
}

class DefaultCoreplexBundle(c: CoreplexConfig)(implicit p: Parameters) extends BaseCoreplexBundle(c)(p)

class DefaultCoreplexModule[+L <: DefaultCoreplex, +B <: DefaultCoreplexBundle]
  (c: CoreplexConfig, l: L, b: => B)(implicit p: Parameters)
  extends BaseCoreplexModule(c, l, b)(p) with DirectConnection

/////

trait HasExtraClockResets {
  val c: CoreplexConfig
  val tcrs = Vec(c.nTiles, new Bundle {
    val clock = Clock(INPUT)
    val reset = Bool(INPUT)
  })
  val extcr = new Bundle {
    val clock = Clock(INPUT)
    val reset = Bool(INPUT)
  }
}

trait AsyncConnection {
  val io: BaseCoreplexBundle
  val cio = io.asInstanceOf[HasExtraClockResets]

  val tiles: Seq[Tile]
  val uncoreTileIOs: Seq[TileIO]
  val uncoreExtMem: Vec[ClientUncachedTileLinkIO]
  val uncoreExtMMIO: ClientUncachedTileLinkIO
  val uncoreExtSlave: Vec[ClientUncachedTileLinkIO]
  val uncoreExtDebug: DebugBusIO

  (tiles, uncoreTileIOs, cio.tcrs).zipped foreach { case (tile, uncore, tcr) =>
    tile.clock := tcr.clock
    tile.reset := tcr.reset

    (uncore.cached zip tile.io.cached) foreach { case (u, t) => u <> AsyncTileLinkFrom(tcr.clock, tcr.reset, t) }
    (uncore.uncached zip tile.io.uncached) foreach { case (u, t) => u <> AsyncUTileLinkFrom(tcr.clock, tcr.reset, t) }
    tile.io.slave.foreach { _ <> AsyncUTileLinkTo(tcr.clock, tcr.reset, uncore.slave.get)}

    val ti = tile.io.interrupts
    val ui = uncore.interrupts
    // These two come from outside the coreplex
    ti.mtip := LevelSyncCrossing(cio.extcr.clock, tcr.clock, ui.mtip)
    ti.msip := LevelSyncCrossing(cio.extcr.clock, tcr.clock, ui.msip)
    // These come from inside the coreplex
    ti.meip := LevelSyncTo(tcr.clock, ui.meip)
    ti.debug := LevelSyncTo(tcr.clock, ui.debug)
    ti.seip.foreach { _ := LevelSyncTo(tcr.clock, ui.seip.get) }

    tile.io.hartid := uncore.hartid
    tile.io.resetVector := uncore.resetVector
  }


  io.master.mem.zip(uncoreExtMem).foreach { case (ext, uncore) =>
    ext <> AsyncUTileLinkTo(cio.extcr.clock, cio.extcr.reset, uncore)
  }
  io.master.mmio <> AsyncUTileLinkTo(cio.extcr.clock, cio.extcr.reset, uncoreExtMMIO)
  uncoreExtSlave.zip(io.slave).foreach { case (uncore, ext) =>
    uncore <> AsyncUTileLinkFrom(cio.extcr.clock, cio.extcr.reset, ext)
  }
  uncoreExtDebug <> AsyncDebugBusFrom(cio.extcr.clock, cio.extcr.reset, io.debug)
}

class MultiClockCoreplex(c: CoreplexConfig)(implicit p: Parameters) extends BaseCoreplex(c)(p) {
  override lazy val module = Module(new MultiClockCoreplexModule(c, this, new MultiClockCoreplexBundle(c)(p))(p))
}

class MultiClockCoreplexBundle(c: CoreplexConfig)(implicit p: Parameters)
  extends BaseCoreplexBundle(c)(p) with HasExtraClockResets

class MultiClockCoreplexModule[+L <: MultiClockCoreplex, +B <: MultiClockCoreplexBundle]
  (c: CoreplexConfig, l: L, b: => B)(implicit p: Parameters)
  extends BaseCoreplexModule(c, l, b)(p) with AsyncConnection
