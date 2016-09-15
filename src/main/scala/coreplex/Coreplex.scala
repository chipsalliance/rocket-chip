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

trait HasCoreplexParameters {
  implicit val p: Parameters
  lazy val nBanksPerMemChannel = p(NBanksPerMemoryChannel)
  lazy val lsb = p(BankIdLSB)
  lazy val innerParams = p.alterPartial({ case TLId => "L1toL2" })
  lazy val outermostParams = p.alterPartial({ case TLId => "Outermost" })
  lazy val outermostMMIOParams = p.alterPartial({ case TLId => "MMIO_Outermost" })
  lazy val globalAddrMap = p(rocketchip.GlobalAddrMap).get
}

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

abstract class Coreplex(implicit val p: Parameters, implicit val c: CoreplexConfig) extends Module
    with HasCoreplexParameters {
  class CoreplexIO(implicit val p: Parameters, implicit val c: CoreplexConfig) extends Bundle {
    val master = new Bundle {
      val mem = Vec(c.nMemChannels, new ClientUncachedTileLinkIO()(outermostParams))
      val mmio = c.hasExtMMIOPort.option(new ClientUncachedTileLinkIO()(outermostMMIOParams))
    }
    val slave = Vec(c.nSlaves, new ClientUncachedTileLinkIO()(innerParams)).flip
    val interrupts = Vec(c.nExtInterrupts, Bool()).asInput
    val debug = new DebugBusIO()(p).flip
    val prci = Vec(c.nTiles, new PRCITileIO).flip
    val success = Bool(OUTPUT)
  }

  val io = new CoreplexIO
}

class DefaultCoreplex(tp: Parameters, tc: CoreplexConfig) extends Coreplex()(tp, tc) {
  // Coreplex doesn't know when to stop running
  io.success := Bool(false)

  // Build a set of Tiles
  val tileResets = Wire(Vec(tc.nTiles, Bool()))
  val tileList = p(BuildTiles).zip(tileResets).map {
    case (tile, rst) => tile(rst, p)
  }
  val nCachedPorts = tileList.map(tile => tile.io.cached.size).reduce(_ + _)
  val nUncachedPorts = tileList.map(tile => tile.io.uncached.size).reduce(_ + _)
  val nBanks = tc.nMemChannels * nBanksPerMemChannel

  // Build an uncore backing the Tiles
  buildUncore(p.alterPartial({
    case HastiId => "TL"
    case TLId => "L1toL2"
    case NCachedTileLinkPorts => nCachedPorts
    case NUncachedTileLinkPorts => nUncachedPorts
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
    val managerEndpoints = List.tabulate(nBanks){id => p(BuildL2CoherenceManager)(id, p)}
    managerEndpoints.flatMap(_.incoherent).foreach(_ := Bool(false))

    val mmioManager = Module(new MMIOTileLinkManager()(p.alterPartial({
        case TLId => "L1toL2"
        case InnerTLId => "L1toL2"
        case OuterTLId => "L2toMMIO"
      })))

    // Wire the tiles to the TileLink client ports of the L1toL2 network,
    // and coherence manager(s) to the other side
    l1tol2net.io.clients_cached <> tileList.map(_.io.cached).flatten
    l1tol2net.io.clients_uncached <> tileList.map(_.io.uncached).flatten ++ io.slave
    l1tol2net.io.managers <> managerEndpoints.map(_.innerTL) :+ mmioManager.io.inner

    // Create a converter between TileLinkIO and MemIO for each channel
    val mem_ic = Module(new TileLinkMemoryInterconnect(nBanksPerMemChannel, tc.nMemChannels)(outermostParams))

    val outerTLParams = p.alterPartial({ case TLId => "L2toMC" })
    val backendBuffering = TileLinkDepths(0,0,0,0,0)
    for ((bank, icPort) <- managerEndpoints zip mem_ic.io.in) {
      val unwrap = Module(new ClientTileLinkIOUnwrapper()(outerTLParams))
      unwrap.io.in <> TileLinkEnqueuer(bank.outerTL, backendBuffering)(outerTLParams)
      TileLinkWidthAdapter(icPort, unwrap.io.out)
    }

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
      tile.io.interrupts := io.prci(i).interrupts
      tile.io.interrupts.meip := plic.io.harts(plic.cfg.context(i, 'M'))
      tile.io.interrupts.seip.foreach(_ := plic.io.harts(plic.cfg.context(i, 'S')))
      tile.io.interrupts.debug := debugModule.io.debugInterrupts(i)
      tile.io.hartid := i
    }

    val tileSlavePorts = (0 until tc.nTiles) map (i => s"int:dmem$i") filter (ioAddrMap contains _)
    for ((t, m) <- (tileList.map(_.io.slave).flatten) zip (tileSlavePorts map (mmioNetwork port _)))
      t <> TileLinkEnqueuer(m, 1)

    io.master.mmio.foreach { _ <> mmioNetwork.port("ext") }
  }
}

class GroundTestCoreplex(tp: Parameters, tc: CoreplexConfig) extends DefaultCoreplex(tp, tc) {
  io.success := tileList.flatMap(_.io.elements get "success").map(_.asInstanceOf[Bool]).reduce(_&&_)
}
