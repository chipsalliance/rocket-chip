package coreplex

import Chisel._
import cde.{Parameters, Field}
import junctions._
import diplomacy._
import uncore.tilelink._
import uncore.tilelink2._
import uncore.coherence._
import uncore.agents._
import uncore.devices._
import uncore.util._
import uncore.converters._
import rocket._
import util._

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
  lazy val outerMemParams = p.alterPartial({ case TLId => "L2toMC" })
  lazy val outerMMIOParams = p.alterPartial({ case TLId => "L2toMMIO" })
  lazy val globalAddrMap = p(rocketchip.GlobalAddrMap)
}

case class CoreplexConfig(
    nTiles: Int,
    nExtInterrupts: Int,
    nSlaves: Int,
    nMemChannels: Int,
    hasSupervisor: Boolean)
{
  val nInterruptPriorities = if (nExtInterrupts <= 1) 0 else (nExtInterrupts min 7)
  val plicKey = PLICConfig(nTiles, hasSupervisor, nExtInterrupts, nInterruptPriorities)
}

abstract class BaseCoreplex(c: CoreplexConfig)(implicit val p: Parameters) extends LazyModule with HasCoreplexParameters {

  val debugLegacy = LazyModule(new TLLegacy()(outerMMIOParams))
  val debugModule = LazyModule(new TLDebugModule(p(XLen)/8))
  debugModule.node :=
  TLHintHandler()(
    TLBuffer()(
      TLFragmenter(p(XLen)/8, debugLegacy.tlDataBeats * debugLegacy.tlDataBytes)(
        TLWidthWidget(debugLegacy.tlDataBytes)(debugLegacy.node))))

}

abstract class BaseCoreplexBundle(val c: CoreplexConfig)(implicit val p: Parameters) extends Bundle with HasCoreplexParameters {
  val master = new Bundle {
    val mem = Vec(c.nMemChannels, new ClientUncachedTileLinkIO()(outerMemParams))
    val mmio = new ClientUncachedTileLinkIO()(outerMMIOParams)
  }
  val slave = Vec(c.nSlaves, new ClientUncachedTileLinkIO()(innerParams)).flip
  val interrupts = Vec(c.nExtInterrupts, Bool()).asInput
  val debug = new DebugBusIO()(p).flip
  val clint = Vec(c.nTiles, new CoreplexLocalInterrupts).asInput
  val resetVector = UInt(INPUT, p(XLen))
  val success = Bool(OUTPUT) // used for testing

  override def cloneType = this.getClass.getConstructors.head.newInstance(c, p).asInstanceOf[this.type]
}

abstract class BaseCoreplexModule[+L <: BaseCoreplex, +B <: BaseCoreplexBundle](
    c: CoreplexConfig, l: L, b: B)(implicit val p: Parameters) extends LazyModuleImp(l) with HasCoreplexParameters {
  val outer: L = l
  val io: B = b

  // Build a set of Tiles
  val tiles = p(BuildTiles) map { _(reset, p) }
  val uncoreTileIOs = (tiles zipWithIndex) map { case (tile, i) => Wire(tile.io) }

  val nCachedPorts = tiles.map(tile => tile.io.cached.size).reduce(_ + _)
  val nUncachedPorts = tiles.map(tile => tile.io.uncached.size).reduce(_ + _)
  val nBanks = c.nMemChannels * nBanksPerMemChannel

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
    val l1tol2net = Module(new PortedTileLinkCrossbar(addrToBank, sharerToClientId))

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
    l1tol2net.io.clients_cached <> uncoreTileIOs.map(_.cached).flatten
    l1tol2net.io.clients_uncached <> uncoreTileIOs.map(_.uncached).flatten ++ io.slave
    l1tol2net.io.managers <> managerEndpoints.map(_.innerTL) :+ mmioManager.io.inner

    val mem_ic = Module(new TileLinkMemoryInterconnect(nBanksPerMemChannel, c.nMemChannels)(outerMemParams))

    val backendBuffering = TileLinkDepths(0,0,0,0,0)
    for ((bank, icPort) <- managerEndpoints zip mem_ic.io.in) {
      val enqueued = TileLinkEnqueuer(bank.outerTL, backendBuffering)
      icPort <> TileLinkIOUnwrapper(enqueued)
    }

    io.master.mem <> mem_ic.io.out

    buildMMIONetwork(TileLinkEnqueuer(mmioManager.io.outer, 1))(outerMMIOParams)
  }

  def buildMMIONetwork(mmio: ClientUncachedTileLinkIO)(implicit p: Parameters) = {
    val ioAddrMap = globalAddrMap.subMap("io")

    val cBus = Module(new TileLinkRecursiveInterconnect(1, ioAddrMap))
    cBus.io.in.head <> mmio

    val plic = Module(new PLIC(c.plicKey))
    plic.io.tl <> cBus.port("cbus:plic")
    for (i <- 0 until io.interrupts.size) {
      val gateway = Module(new LevelGateway)
      gateway.io.interrupt := io.interrupts(i)
      plic.io.devices(i) <> gateway.io.plic
    }

    outer.debugLegacy.module.io.legacy <> cBus.port("cbus:debug")
    outer.debugModule.module.io.db <> io.debug

    // connect coreplex-internal interrupts to tiles
    for ((tile, i) <- (uncoreTileIOs zipWithIndex)) {
      tile.interrupts <> io.clint(i)
      tile.interrupts.meip := plic.io.harts(plic.cfg.context(i, 'M'))
      tile.interrupts.seip.foreach(_ := plic.io.harts(plic.cfg.context(i, 'S')))
      tile.interrupts.debug := outer.debugModule.module.io.debugInterrupts(i)
      tile.hartid := UInt(i)
      tile.resetVector := io.resetVector
    }

    val tileSlavePorts = (0 until c.nTiles) map (i => s"cbus:dmem$i") filter (ioAddrMap contains _)
    for ((t, m) <- (uncoreTileIOs.map(_.slave).flatten) zip (tileSlavePorts map (cBus port _)))
      t <> m

    io.master.mmio <> cBus.port("pbus")
  }

  // Coreplex doesn't know when to stop running
  io.success := Bool(false)
}
