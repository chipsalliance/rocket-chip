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
/** Number of tracker per bank */
case object NTrackersPerBank extends Field[Int]
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
  lazy val nSlaves = p(rocketchip.NCoreplexExtClients)
  lazy val nMemChannels = p(NMemoryChannels)
  lazy val hasSupervisor = p(rocket.UseVM)
  lazy val nTrackersPerBank = p(NTrackersPerBank)
}

case class CoreplexParameters(implicit val p: Parameters) extends HasCoreplexParameters

abstract class BareCoreplex(implicit val p: Parameters) extends LazyModule
abstract class BareCoreplexBundle[+L <: BareCoreplex](_outer: L) extends Bundle {
  val outer = _outer
}
abstract class BareCoreplexModule[+L <: BareCoreplex, +B <: BareCoreplexBundle[L]](_outer: L, _io: () => B) extends LazyModuleImp(_outer) {
  val outer = _outer
  val io = _io ()
}

trait CoreplexNetwork extends HasCoreplexParameters {
  this: BareCoreplex =>

  val l1tol2 = LazyModule(new TLXbar)
  val l1tol2_beatBytes = p(TLKey("L2toMMIO")).dataBitsPerBeat/8
  val l1tol2_lineBytes = p(CacheBlockBytes)

  val cbus = LazyModule(new TLXbar)
  val cbus_beatBytes = p(XLen)/8
  val cbus_lineBytes = l1tol2_lineBytes

  val mmio = TLOutputNode()
  val mmioInt = IntInputNode()

  cbus.node :=
    TLAtomicAutomata(arithmetic = true)( // disable once TLB uses TL2 metadata
    TLWidthWidget(l1tol2_beatBytes)(
    TLBuffer()(
    l1tol2.node)))

  mmio :=
    TLBuffer()(
    TLWidthWidget(l1tol2_beatBytes)(
    l1tol2.node))
}

trait CoreplexNetworkBundle extends HasCoreplexParameters {
  this: {
    val outer: CoreplexNetwork
  } =>

  implicit val p = outer.p
  val mmio = outer.mmio.bundleOut
  val interrupts = outer.mmioInt.bundleIn
}

trait CoreplexNetworkModule extends HasCoreplexParameters {
    this: BareCoreplexModule[BareCoreplex, BareCoreplexBundle[BareCoreplex]] =>
  implicit val p = outer.p
}

trait BankedL2CoherenceManagers {
    this: CoreplexNetwork =>
  require (isPow2(nBanksPerMemChannel))
  require (isPow2(l1tol2_lineBytes))

  def l2ManagerFactory(): (TLInwardNode, TLOutwardNode)

  val l2Channels = Seq.fill(nMemChannels) {
    val bankBar = LazyModule(new TLXbar)
    val output = TLOutputNode()

    output := bankBar.node
    val mask = ~BigInt((nBanksPerMemChannel-1) * l1tol2_lineBytes)
    for (i <- 0 until nBanksPerMemChannel) {
      val (in, out) = l2ManagerFactory()
      in := TLFilter(AddressSet(i * l1tol2_lineBytes, mask))(l1tol2.node)
      bankBar.node := out
    }

    output
  }
}

trait BankedL2CoherenceManagersBundle {
  this: CoreplexNetworkBundle {
    val outer: BankedL2CoherenceManagers
  } =>

  require (nMemChannels == 1, "Seq in Chisel Bundle needed to support > 1") // !!!
  val mem = outer.l2Channels.map(_.bundleOut).toList.head // .head should be removed !!!
}

trait BankedL2CoherenceManagersModule {
  this: CoreplexNetworkModule {
    val outer: BankedL2CoherenceManagers
    val io: BankedL2CoherenceManagersBundle
  } =>
}

trait CoreplexRISCVPlatform {
    this: CoreplexNetwork =>

  // Build a set of Tiles
  val lazyTiles = p(BuildTiles) map { _(p) }
  val legacy = LazyModule(new TLLegacy()(outerMMIOParams))
  val tileIntNodes = lazyTiles.map { _ => IntInternalOutputNode() } // this should be moved into the Tile...

  val debug = LazyModule(new TLDebugModule())
  val plic  = LazyModule(new TLPLIC(hasSupervisor, maxPriorities = 7))
  val clint = LazyModule(new CoreplexLocalInterrupter)

  // Kill this once we move TL2 into rocket
  l1tol2.node :=
    TLHintHandler()(
    legacy.node)

  debug.node := TLFragmenter(cbus_beatBytes, cbus_lineBytes)(cbus.node)
  plic.node  := TLFragmenter(cbus_beatBytes, cbus_lineBytes)(cbus.node)
  clint.node := TLFragmenter(cbus_beatBytes, cbus_lineBytes)(cbus.node)

  plic.intnode := mmioInt
  tileIntNodes.foreach { _ := plic.intnode }
}

trait CoreplexRISCVPlatformBundle {
  this: CoreplexNetworkBundle {
    val outer: CoreplexRISCVPlatform
  } =>

  val mem = Vec(nMemChannels, new ClientUncachedTileLinkIO()(outerMemParams))
  val slave = Vec(nSlaves, new ClientUncachedTileLinkIO()(innerParams)).flip
  val debug = new DebugBusIO().flip
  val rtcTick = Bool(INPUT)
  val resetVector = UInt(INPUT, p(XLen))
  val success = Bool(OUTPUT) // used for testing
}

trait CoreplexRISCVPlatformModule {
  this: CoreplexNetworkModule {
    val outer: CoreplexNetwork with CoreplexRISCVPlatform
    val io: CoreplexRISCVPlatformBundle
  } =>

  val tiles = outer.lazyTiles.map(_.module)
  val uncoreTileIOs = (tiles zipWithIndex) map { case (tile, i) => Wire(tile.io) }

  println("\nGenerated Address Map")
  for (entry <- p(rocketchip.GlobalAddrMap).flatten) {
    val name = entry.name
    val start = entry.region.start
    val end = entry.region.start + entry.region.size - 1
    val prot = entry.region.attr.prot
    val protStr = (if ((prot & AddrMapProt.R) > 0) "R" else "") +
                  (if ((prot & AddrMapProt.W) > 0) "W" else "") +
                  (if ((prot & AddrMapProt.X) > 0) "X" else "")
    val cacheable = if (entry.region.attr.cacheable) " [C]" else ""
    println(f"\t$name%s $start%x - $end%x, $protStr$cacheable")
  }

  // Create and export the ConfigString
  val managers = outer.l1tol2.node.edgesIn(0).manager.managers
  val configString = rocketchip.GenerateConfigString(p, outer.clint, outer.plic, managers)
  // Allow something else to have override the config string
  if (!ConfigStringOutput.contents.isDefined) {
    ConfigStringOutput.contents = Some(configString)
  }
  println(s"\nGenerated Configuration String\n${ConfigStringOutput.contents.get}")

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

  // connect coreplex-internal interrupts to tiles
  for ((tile, i) <- (uncoreTileIOs zipWithIndex)) {
    tile.hartid := UInt(i)
    tile.resetVector := io.resetVector
    tile.interrupts := outer.clint.module.io.tiles(i)
    tile.interrupts.debug := outer.debug.module.io.debugInterrupts(i)
    tile.interrupts.meip := outer.tileIntNodes(i).bundleOut(0)(0)
    tile.interrupts.seip.foreach(_ := outer.tileIntNodes(i).bundleOut(0)(1))
  }

  outer.debug.module.io.db <> io.debug
  outer.clint.module.io.rtcTick := io.rtcTick

  // Coreplex doesn't know when to stop running
  io.success := Bool(false)
}

class BaseCoreplex(implicit p: Parameters) extends BareCoreplex
    with CoreplexNetwork
    with CoreplexRISCVPlatform {
  override lazy val module = new BaseCoreplexModule(this, () => new BaseCoreplexBundle(this))
}

class BaseCoreplexBundle[+L <: BaseCoreplex](_outer: L) extends BareCoreplexBundle(_outer)
    with CoreplexNetworkBundle
    with CoreplexRISCVPlatformBundle

class BaseCoreplexModule[+L <: BaseCoreplex, +B <: BaseCoreplexBundle[L]](_outer: L, _io: () => B) extends BareCoreplexModule(_outer, _io)
    with CoreplexNetworkModule
    with CoreplexRISCVPlatformModule
