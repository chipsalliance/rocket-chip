package coreplex

import Chisel._
import cde.{Parameters, Field}
import junctions._
import uncore.tilelink._
import uncore.coherence._
import uncore.agents._
import uncore.devices._
import uncore.util._
import uncore.converters._
import rocket._
import rocket.Util._

/** Number of memory channels */
case object NMemoryChannels extends Field[Int]
/** Number of banks per memory channel */
case object NBanksPerMemoryChannel extends Field[Int]
/** Least significant bit of address used for bank partitioning */
case object BankIdLSB extends Field[Int]
/** Function for building some kind of coherence manager agent */
case object BuildL2CoherenceManager extends Field[(Int, Parameters) => CoherenceAgent]
/** Function for building some kind of tile connected to a reset signal */
case object BuildTiles extends Field[Seq[(Bool, Parameters) => Tile]]
/** The file to read the BootROM contents from */
case object BootROMFile extends Field[String]
/** Separate clocks for various coreplex domains? */
case object CoreplexMultiClock extends Field[CoreplexClockConfig]

trait HasCoreplexParameters {
  implicit val p: Parameters
  lazy val nBanksPerMemChannel = p(NBanksPerMemoryChannel)
  lazy val lsb = p(BankIdLSB)
  lazy val innerParams = p.alterPartial({ case TLId => "L1toL2" })
  lazy val outermostParams = p.alterPartial({ case TLId => "Outermost" })
  lazy val outermostMMIOParams = p.alterPartial({ case TLId => "MMIO_Outermost" })
  lazy val globalAddrMap = p(rocketchip.GlobalAddrMap)
  lazy val multiclockL2 = p(CoreplexMultiClock).l2agent
  lazy val multiclockTiles = p(CoreplexMultiClock).tiles
}

case class CoreplexClockConfig(l2agent: Boolean, tiles: Boolean)

case class CoreplexConfig(
    nTiles: Int,
    nExtInterrupts: Int,
    nSlaves: Int,
    nMemChannels: Int,
    hasSupervisor: Boolean,
    hasExtMMIOPort: Boolean)
{
  val plicKey = PLICConfig(nTiles, hasSupervisor, nExtInterrupts, 0)
}

class MultiClockBundle(clockConfig: CoreplexClockConfig, coreplexConfig: CoreplexConfig) extends Bundle {
  val l2Clock = clockConfig.l2agent.option(Clock())
  val l2Reset = clockConfig.l2agent.option(Bool())
  val tileClocks = clockConfig.tiles.option(Vec(coreplexConfig.nTiles, Clock()))

  override def cloneType = new MultiClockBundle(clockConfig, coreplexConfig).asInstanceOf[this.type]
}

abstract class Coreplex(implicit val p: Parameters, implicit val c: CoreplexConfig) extends Module
    with HasCoreplexParameters {
  class CoreplexIO(implicit val p: Parameters, implicit val c: CoreplexConfig) extends Bundle {
    val master = new Bundle {
      val mem = Vec(c.nMemChannels, new ClientUncachedTileLinkIO()(outermostParams))
      val mmio = c.hasExtMMIOPort.option(new ClientUncachedTileLinkIO()(outermostMMIOParams))
    }
    val slave = Vec(c.nSlaves, new ClientUncachedTileLinkIO()(innerParams)).flip
    val clocks = new MultiClockBundle(p(CoreplexMultiClock), c).asInput
    val interrupts = Vec(c.nExtInterrupts, Bool()).asInput
    val debug = new DebugBusIO()(p).flip
    val prci = Vec(c.nTiles, new PRCITileIO).flip
    val success: Option[Bool] = hasSuccessFlag.option(Bool(OUTPUT))
  }

  def hasSuccessFlag: Boolean = false
  val io = new CoreplexIO
}

class DefaultCoreplex(tp: Parameters, tc: CoreplexConfig) extends Coreplex()(tp, tc) {
  class TileWrapper(tileBuilder: (Bool, Parameters) => Tile, _clock: Clock, _reset: Bool)
      (implicit p: Parameters) extends Module(Some(_clock), Some(_reset)) {

    val wrappedTile = tileBuilder(reset, p)
    val io = new Bundle {
      val tile = wrappedTile.io.cloneType
      val ext_clock = Clock(INPUT)
      val ext_reset = Bool(INPUT)
    }

    io.tile.uncached <> wrappedTile.io.uncached.map(AsyncClientUncachedTileLinkTo(io.ext_clock, io.ext_reset, _))
    io.tile.cached <> wrappedTile.io.cached.map(AsyncClientTileLinkTo(io.ext_clock, io.ext_reset, _))
    wrappedTile.io.interrupts := LevelSyncFrom(io.ext_clock, io.tile.interrupts)
    wrappedTile.io.hartid := LevelSyncFrom(io.ext_clock, io.tile.hartid)
    wrappedTile.io.slave.zip(io.tile.slave).foreach { case (ts, os) =>
      ts <> AsyncClientUncachedTileLinkFrom(io.ext_clock, io.ext_reset, os)
    }
    io.tile.elements.get("success").zip(wrappedTile.io.elements.get("success")).foreach {
      case (os, is) => os := LevelSyncTo(io.ext_clock, is)
    }
  }

  class L2Wrapper(l2Builder: Parameters => CoherenceAgent, _clock: Option[Clock], _reset: Option[Bool])
      (implicit p: Parameters) extends Module(_clock, _reset) {

    val innerParams = p.alterPartial({ case TLId => "L1toL2" })
    val outerParams = p.alterPartial({ case TLId => "L2toMC" })
    val outermostParams = p.alterPartial({ case TLId => "Outermost" })

    val io = new Bundle {
      val inner = new ManagerTileLinkIO()(innerParams)
      val outer = new ClientUncachedTileLinkIO()(outermostParams)
      val ext_clock = Clock(INPUT)
      val ext_reset = Bool(INPUT)
    }

    val backendBuffering = TileLinkDepths(0,0,0,0,0)

    val wrappedL2 = l2Builder(p)
    val unwrap = Module(new ClientTileLinkIOUnwrapper()(outerParams))
    wrappedL2.incoherent.foreach(_ := Bool(false))
    unwrap.io.in <> TileLinkEnqueuer(wrappedL2.outerTL, backendBuffering)(outerParams)
    val outermostPort = TileLinkWidthAdapter(unwrap.io.out, "Outermost")

    if (multiclockL2) {
      io.inner <> AsyncManagerTileLinkTo(io.ext_clock, io.ext_reset, wrappedL2.innerTL)
      io.outer <> AsyncClientUncachedTileLinkTo(io.ext_clock, io.ext_reset, outermostPort)
    } else {
      io.inner <> wrappedL2.innerTL
      io.outer <> outermostPort
    }
  }

  // Build a set of Tiles
  val tileResets = Wire(Vec(tc.nTiles, Bool()))
  val tileList = p(BuildTiles).zip(tileResets).zipWithIndex.map {
    case ((tile, rst), idx) =>
      if (multiclockTiles) {
        val tileClock = io.clocks.tileClocks.get(idx)
        val tileReset = LevelSyncTo(tileClock, rst)
        val wrapper = Module(new TileWrapper(tile, tileClock, tileReset))
        wrapper.io.ext_clock := clock
        wrapper.io.ext_reset := rst
        wrapper.io.tile
      } else tile(rst, p).io
  }
  val tileCachedPorts = tileList.flatMap(tile => tile.cached)
  val tileUncachedPorts = tileList.flatMap(tile => tile.uncached)
  val nBanks = tc.nMemChannels * nBanksPerMemChannel

  // Build an uncore backing the Tiles
  buildUncore(p.alterPartial({
    case HastiId => "TL"
    case TLId => "L1toL2"
    case NCachedTileLinkPorts => tileCachedPorts.size
    case NUncachedTileLinkPorts => tileUncachedPorts.size
  }))

  def buildUncore(implicit p: Parameters) = {
    // Create a simple L1toL2 NoC between the tiles and the banks of outer memory
    // Cached ports are first in client list, making sharerToClientId just an indentity function
    // addrToBank is sed to hash physical addresses (of cache blocks) to banks (and thereby memory channels)
    def sharerToClientId(sharerId: UInt) = sharerId
    def addrToBank(addr: UInt): UInt = if (nBanks == 0) UInt(0) else {
      val isMemory = globalAddrMap.isInRegion("mem", addr << log2Up(p(CacheBlockBytes)))
      Mux(isMemory, addr.extract(lsb + log2Ceil(nBanks) - 1, lsb), UInt(nBanks))
    }
    val preBuffering = TileLinkDepths(1,1,2,2,0)
    val l1tol2net = Module(new PortedTileLinkCrossbar(addrToBank, sharerToClientId, preBuffering))

    // Create point(s) of coherence serialization
    val managerEndpoints = List.tabulate(nBanks) { id =>
      val wrapper = Module(new L2Wrapper(p(BuildL2CoherenceManager)(id, _), io.clocks.l2Clock, io.clocks.l2Reset))
      wrapper.io.ext_clock := clock
      wrapper.io.ext_reset := reset
      wrapper.io
    }

    val mmioManager = Module(new MMIOTileLinkManager()(p.alterPartial({
        case TLId => "L1toL2"
        case InnerTLId => "L1toL2"
        case OuterTLId => "L2toMMIO"
      })))

    // Wire the tiles to the TileLink client ports of the L1toL2 network,
    // and coherence manager(s) to the other side
    l1tol2net.io.clients_cached <> tileCachedPorts
    l1tol2net.io.clients_uncached <> tileUncachedPorts ++ io.slave
    l1tol2net.io.managers <> managerEndpoints.map(_.inner) :+ mmioManager.io.inner

    val mem_ic = Module(new TileLinkMemoryInterconnect(nBanksPerMemChannel, tc.nMemChannels)(outermostParams))
    mem_ic.io.in <> managerEndpoints.map(_.outer)
    io.master.mem <> mem_ic.io.out

    buildMMIONetwork(TileLinkEnqueuer(mmioManager.io.outer, 1))(
        p.alterPartial({case TLId => "L2toMMIO"}))
  }

  def buildMMIONetwork(mmio: ClientUncachedTileLinkIO)(implicit p: Parameters) = {
    val ioAddrMap = globalAddrMap.subMap("io")

    val mmioNetwork = Module(new TileLinkRecursiveInterconnect(1, ioAddrMap))
    mmioNetwork.io.in.head <> mmio

    val plic = Module(new PLIC(c.plicKey))
    plic.io.tl <> mmioNetwork.port("int:plic")
    for (i <- 0 until io.interrupts.size) {
      val gateway = Module(new LevelGateway)
      gateway.io.interrupt := io.interrupts(i)
      plic.io.devices(i) <> gateway.io.plic
    }

    val debugModule = Module(new DebugModule)
    debugModule.io.tl <> mmioNetwork.port("int:debug")
    debugModule.io.db <> io.debug

    // connect coreplex-internal interrupts to tiles
    for (((tile, tileReset), i) <- (tileList zip tileResets) zipWithIndex) {
      tileReset := io.prci(i).reset
      tile.interrupts := io.prci(i).interrupts
      tile.interrupts.meip := plic.io.harts(plic.cfg.context(i, 'M'))
      tile.interrupts.seip.foreach(_ := plic.io.harts(plic.cfg.context(i, 'S')))
      tile.interrupts.debug := debugModule.io.debugInterrupts(i)
      tile.hartid := i
    }

    val tileSlavePorts = (0 until tc.nTiles) map (i => s"int:dmem$i") filter (ioAddrMap contains _)
    for ((t, m) <- (tileList.map(_.slave).flatten) zip (tileSlavePorts map (mmioNetwork port _)))
      t <> TileLinkEnqueuer(m, 1)

    io.master.mmio.foreach { _ <> mmioNetwork.port("ext") }
  }
}

class GroundTestCoreplex(tp: Parameters, tc: CoreplexConfig) extends DefaultCoreplex(tp, tc) {
  override def hasSuccessFlag = true
  io.success.get := tileList.flatMap(_.elements get "success").map(_.asInstanceOf[Bool]).reduce(_&&_)
}
