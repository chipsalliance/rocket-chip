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
case object BuildTiles extends Field[Seq[Parameters => LazyTile]]
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
  lazy val nTiles = p(uncore.devices.NTiles)
  lazy val nExtInterrupts = p(rocketchip.NExtInterrupts)
  lazy val nSlaves = p(rocketchip.NCoreplexExtClients)
  lazy val nMemChannels = p(NMemoryChannels)
  lazy val hasSupervisor = p(rocket.UseVM)

  lazy val nInterruptPriorities = if (nExtInterrupts <= 1) 0 else (nExtInterrupts min 7)
  lazy val plicKey = PLICConfig(nTiles, hasSupervisor, nExtInterrupts, nInterruptPriorities)
  lazy val clintKey = CoreplexLocalInterrupterConfig()
}

case class CoreplexParameters(implicit val p: Parameters) extends HasCoreplexParameters

abstract class BareCoreplex(implicit val p: Parameters) extends LazyModule with HasCoreplexParameters {
  val l1tol2 = LazyModule(new TLXbar)
  val mmio = TLOutputNode()
  val lazyTiles = p(BuildTiles) map { _(p) }
  val legacy = LazyModule(new TLLegacy()(outerMMIOParams))

  mmio :=
    TLBuffer()(
    TLWidthWidget(legacy.tlDataBytes)(
    l1tol2.node))

  // Kill this once we move TL2 into rocket
  l1tol2.node :=
    TLHintHandler()(
    legacy.node)
}

abstract class BareCoreplexBundle[+L <: BareCoreplex](val outer: L) extends Bundle with HasCoreplexParameters {
  implicit val p = outer.p

  val mem = Vec(nMemChannels, new ClientUncachedTileLinkIO()(outerMemParams))
  val mmio = outer.mmio.bundleOut
  val slave = Vec(nSlaves, new ClientUncachedTileLinkIO()(innerParams)).flip
  val resetVector = UInt(INPUT, p(XLen))
  val success = Bool(OUTPUT) // used for testing
}

abstract class BareCoreplexModule[+L <: BareCoreplex, +B <: BareCoreplexBundle[L]](val outer: L, val io: B) extends LazyModuleImp(outer) with HasCoreplexParameters {
  implicit val p = outer.p

  // Build a set of Tiles
  val tiles = outer.lazyTiles.map(_.module)
  val uncoreTileIOs = (tiles zipWithIndex) map { case (tile, i) => Wire(tile.io) }

  val nCachedPorts = tiles.map(tile => tile.io.cached.size).reduce(_ + _)
  val nUncachedPorts = tiles.map(tile => tile.io.uncached.size).reduce(_ + _)
  val nBanks = nMemChannels * nBanksPerMemChannel
  
  buildUncore(p.alterPartial({
    case HastiId => "TL"
    case TLId => "L1toL2"
    case NCachedTileLinkPorts => nCachedPorts
    case NUncachedTileLinkPorts => nUncachedPorts
  }))

  def buildUncore(implicit p: Parameters) {
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
    outer.legacy.module.io.legacy <>  mmioManager.io.outer 

    val mem_ic = Module(new TileLinkMemoryInterconnect(nBanksPerMemChannel, nMemChannels)(outerMemParams))

    val backendBuffering = TileLinkDepths(0,0,0,0,0)
    for ((bank, icPort) <- managerEndpoints zip mem_ic.io.in) {
      val enqueued = TileLinkEnqueuer(bank.outerTL, backendBuffering)
      icPort <> TileLinkIOUnwrapper(enqueued)
    }

    io.mem <> mem_ic.io.out
  }

  for ((tile, i) <- (uncoreTileIOs zipWithIndex)) {
    tile.hartid := UInt(i)
    tile.resetVector := io.resetVector
  }

  // Coreplex doesn't know when to stop running
  io.success := Bool(false)
}

trait CoreplexPeripherals extends HasCoreplexParameters {
  val module: CoreplexPeripheralsModule
  val l1tol2: TLXbar
  val legacy: TLLegacy

  val cbus  = LazyModule(new TLXbar)
  val debug = LazyModule(new TLDebugModule())
  val plic  = LazyModule(new TLPLIC(() => plicKey))
  val clint = LazyModule(new CoreplexLocalInterrupter(clintKey))

  cbus.node :=
    TLAtomicAutomata(arithmetic = true)( // disable once TLB uses TL2 metadata
    TLWidthWidget(legacy.tlDataBytes)(
    TLBuffer()(
    l1tol2.node)))

  debug.node := TLFragmenter(p(XLen)/8, legacy.tlDataBeats * legacy.tlDataBytes)(cbus.node)
  plic.node  := TLFragmenter(p(XLen)/8, legacy.tlDataBeats * legacy.tlDataBytes)(cbus.node)
  clint.node := TLFragmenter(p(XLen)/8, legacy.tlDataBeats * legacy.tlDataBytes)(cbus.node)
}

trait CoreplexPeripheralsBundle extends HasCoreplexParameters {
  val outer: CoreplexPeripherals

  val debug = new DebugBusIO().flip
  val interrupts = Vec(nExtInterrupts, Bool()).asInput
}

trait CoreplexPeripheralsModule extends HasCoreplexParameters {
  val outer: CoreplexPeripherals
  val io: CoreplexPeripheralsBundle
  val uncoreTileIOs: Seq[TileIO]

  for (i <- 0 until io.interrupts.size) {
    val gateway = Module(new LevelGateway)
    gateway.io.interrupt := io.interrupts(i)
    outer.plic.module.io.devices(i) <> gateway.io.plic
  }

  outer.debug.module.io.db <> io.debug
  outer.clint.module.io.rtcTick := Counter(p(rocketchip.RTCPeriod)).inc()

  // connect coreplex-internal interrupts to tiles
  for ((tile, i) <- (uncoreTileIOs zipWithIndex)) {
    tile.interrupts <> outer.clint.module.io.tiles(i)
    tile.interrupts.meip := outer.plic.module.io.harts(plicKey.context(i, 'M'))
    tile.interrupts.seip.foreach(_ := outer.plic.module.io.harts(plicKey.context(i, 'S')))
    tile.interrupts.debug := outer.debug.module.io.debugInterrupts(i)
  }
}

class BaseCoreplex(implicit p: Parameters) extends BareCoreplex
    with CoreplexPeripherals {
  override lazy val module = new BaseCoreplexModule(this, new BaseCoreplexBundle(this))
}

class BaseCoreplexBundle[+L <: BaseCoreplex](outer: L) extends BareCoreplexBundle(outer)
    with CoreplexPeripheralsBundle

class BaseCoreplexModule[+L <: BaseCoreplex, +B <: BaseCoreplexBundle[L]](outer: L, io: B) extends BareCoreplexModule(outer, io)
    with CoreplexPeripheralsModule
