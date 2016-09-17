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
  lazy val nTileCached = p(NCachedTileLinkPorts)
  lazy val nTileUncached = p(NUncachedTileLinkPorts)
  lazy val innerParams = p.alterPartial({ case TLId => "L1toL2" })
  lazy val outerParams = p.alterPartial({ case TLId => "L2toMC" })
  lazy val outerMMIOParams = p.alterPartial({ case TLId => "L2toMMIO" })
  lazy val outermostParams = p.alterPartial({ case TLId => "Outermost" })
  lazy val outermostMMIOParams = p.alterPartial({ case TLId => "MMIO_Outermost" })
  lazy val globalAddrMap = p(rocketchip.GlobalAddrMap)
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

class CoreplexIO(implicit val p: Parameters, implicit val c: CoreplexConfig)
    extends Bundle with HasCoreplexParameters {
  val master = new Bundle {
    val mem = Vec(c.nMemChannels, new ClientUncachedTileLinkIO()(outermostParams))
    val mmio = c.hasExtMMIOPort.option(new ClientUncachedTileLinkIO()(outermostMMIOParams))
  }
  val slave = Vec(c.nSlaves, new ClientUncachedTileLinkIO()(innerParams)).flip
  val interrupts = Vec(c.nExtInterrupts, Bool()).asInput
  val debug = new DebugBusIO()(p).flip
  val clint = Vec(c.nTiles, new CoreplexLocalInterrupts).asInput
  val success = Bool(OUTPUT)

  override def cloneType = new CoreplexIO()(p, c).asInstanceOf[this.type]
}

abstract class Coreplex(implicit val p: Parameters, implicit val c: CoreplexConfig)
    extends Module with HasCoreplexParameters {
  val io: CoreplexIO
}

class L2Block(c: CoreplexConfig)(implicit val p: Parameters)
    extends Module with HasCoreplexParameters {
  val io = new Bundle {
    val tiles_cached = Vec(nTileCached, new ClientTileLinkIO()(innerParams)).flip
    val tiles_uncached = Vec(nTileUncached, new ClientUncachedTileLinkIO()(innerParams)).flip
    val slave = Vec(c.nSlaves, new ClientUncachedTileLinkIO()(innerParams)).flip
    val mem = Vec(c.nMemChannels, new ClientUncachedTileLinkIO()(outermostParams))
    val mmio = new ClientUncachedTileLinkIO()(outerMMIOParams)
  }

  val nBanks = c.nMemChannels * nBanksPerMemChannel

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
  l1tol2net.io.clients_cached <> io.tiles_cached
  l1tol2net.io.clients_uncached <> io.tiles_uncached ++ io.slave
  l1tol2net.io.managers <> managerEndpoints.map(_.innerTL) :+ mmioManager.io.inner

  // Create a converter between TileLinkIO and MemIO for each channel
  val mem_ic = Module(new TileLinkMemoryInterconnect(nBanksPerMemChannel, c.nMemChannels)(outermostParams))

  val backendBuffering = TileLinkDepths(0,0,0,0,0)
  for ((bank, icPort) <- managerEndpoints zip mem_ic.io.in) {
    val unwrap = Module(new ClientTileLinkIOUnwrapper()(outerParams))
    unwrap.io.in <> TileLinkEnqueuer(bank.outerTL, backendBuffering)(outerParams)
    TileLinkWidthAdapter(icPort, unwrap.io.out)
  }

  io.mem <> mem_ic.io.out
  io.mmio <> mmioManager.io.outer
}

class MMIOBlock(nTileSlaves: Int, c: CoreplexConfig)(implicit val p: Parameters)
    extends Module with HasCoreplexParameters {
  val io = new Bundle {
    val slave = new ClientUncachedTileLinkIO()(outerMMIOParams).flip
    val master = c.hasExtMMIOPort.option(
      new ClientUncachedTileLinkIO()(outermostMMIOParams))
    val interrupts = Vec(c.nExtInterrupts, Bool()).asInput
    val debug = new DebugBusIO()(p).flip
    val clint = Vec(c.nTiles, new CoreplexLocalInterrupts).asInput
    val tileInterrupts = Vec(c.nTiles, new TileInterrupts).asOutput
    val tileResets = Vec(c.nTiles, Bool(OUTPUT))
    val tileSlaves = Vec(nTileSlaves, new ClientUncachedTileLinkIO()(innerParams))
  }

  val ioAddrMap = globalAddrMap.subMap("io")

  val mmioNetwork = Module(new TileLinkRecursiveInterconnect(1, ioAddrMap))
  mmioNetwork.io.in.head <> io.slave

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
  for (((int, rst), i) <- (io.tileInterrupts zip io.tileResets) zipWithIndex) {
    rst := reset // TODO should tiles be reset separately from coreplex?
    int := io.clint(i)
    int.meip := plic.io.harts(plic.cfg.context(i, 'M'))
    int.seip.foreach(_ := plic.io.harts(plic.cfg.context(i, 'S')))
    int.debug := debugModule.io.debugInterrupts(i)
  }

  (0 until c.nTiles)
    .map(i => s"int:dmem$i")
    .filter(ioAddrMap contains _)
    .map(name => mmioNetwork.port(name))
    .zip(io.tileSlaves)
    .foreach { case (m, t) => t <> m }

  io.master.foreach { _ <> mmioNetwork.port("ext") }
}

abstract class BaseCoreplex(tp: Parameters, tc: CoreplexConfig)
    extends Coreplex()(tp, tc) {

  def buildTiles(clocks: Seq[Clock], resets: Seq[Bool]): Seq[Tile] = {
    (p(BuildTiles).zipWithIndex, clocks, resets).zipped.map {
      case ((tileBuilder, i), clk, rst) =>
        val tile = tileBuilder(rst, p)
        tile.clock := clk
        tile.io.hartid := UInt(i)
        tile
    }
  }

  def buildL2Block(_clock: Clock, _reset: Bool,
                   nCachedPorts: Int, nUncachedPorts: Int): L2Block = {
    val l2block = Module(new L2Block(c)(p.alterPartial({
      case TLId => "L1toL2"
      case NCachedTileLinkPorts => nCachedPorts
      case NUncachedTileLinkPorts => nUncachedPorts
    })))
    l2block.clock := _clock
    l2block.reset := _reset
    l2block
  }

  def buildMMIONetwork(nTileSlaves: Int): MMIOBlock = {
    val mmioBlock = Module(new MMIOBlock(nTileSlaves, c)(p.alterPartial({
      case TileId => 0 // It's annoying, but TileInterrupts requires this
      case TLId => "L2toMMIO"
    })))
    io.master.mmio.map { _ <> mmioBlock.io.master.get }
    mmioBlock.io.debug <> io.debug
    mmioBlock.io.interrupts <> io.interrupts
    mmioBlock.io.clint <> io.clint
    mmioBlock
  }
}

class DefaultCoreplex(tp: Parameters, tc: CoreplexConfig) extends BaseCoreplex(tp, tc) {
  val io = new CoreplexIO
  // Build a set of Tiles
  val tileResets = Wire(Vec(tc.nTiles, Bool()))
  val tileList = buildTiles(Seq.fill(tc.nTiles){clock}, tileResets)
  val tileCachedPorts = tileList.flatMap(tile => tile.io.cached)
  val tileUncachedPorts = tileList.flatMap(tile => tile.io.uncached)

  val l2block = buildL2Block(clock, reset,
    tileCachedPorts.size, tileUncachedPorts.size)
  l2block.io.tiles_cached <> tileCachedPorts
  l2block.io.tiles_uncached <> tileUncachedPorts
  l2block.io.slave <> io.slave
  io.master.mem <> l2block.io.mem

  val nTileSlaves = tileList.flatMap(_.io.slave).size
  val mmioBlock = buildMMIONetwork(nTileSlaves)

  mmioBlock.io.slave <> TileLinkEnqueuer(l2block.io.mmio, 1)(outerMMIOParams)

  tileList.flatMap(_.io.slave).zip(mmioBlock.io.tileSlaves).map {
    case (tileSlave, mmioSlave) =>
      tileSlave <> TileLinkEnqueuer(mmioSlave, 1)(innerParams)
  }

  tileList.zip(mmioBlock.io.tileInterrupts).map {
    case (tile, int) => tile.io.interrupts := int
  }

  tileResets := mmioBlock.io.tileResets

  io.success := Bool(false)
}

class GroundTestCoreplex(tp: Parameters, tc: CoreplexConfig) extends DefaultCoreplex(tp, tc) {
  io.success := tileList.flatMap(_.io.elements get "success").map(_.asInstanceOf[Bool]).reduce(_&&_)
}
