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
/** The file to read the BootROM contents from */
case object BootROMFile extends Field[String]

trait HasCoreplexParameters {
  implicit val p: Parameters
  lazy val nBanksPerMemChannel = p(NBanksPerMemoryChannel)
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

  val mem = Seq.fill(nMemChannels) {
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

  require (nMemChannels <= 1, "Seq in Chisel Bundle needed to support > 1") // !!!
  val mem = outer.mem.map(_.bundleOut).toList.headOption // .headOption should be removed !!!
}

trait BankedL2CoherenceManagersModule {
  this: CoreplexNetworkModule {
    val outer: BankedL2CoherenceManagers
    val io: BankedL2CoherenceManagersBundle
  } =>
}

trait CoreplexRISCVPlatform {
    this: CoreplexNetwork =>

  val lazyTiles = List.tabulate(p(NTiles)){ i => LazyModule(new RocketTile(i)) }
  val debug = LazyModule(new TLDebugModule())
  val plic  = LazyModule(new TLPLIC(hasSupervisor, maxPriorities = 7))
  val clint = LazyModule(new CoreplexLocalInterrupter)
  val tileIntNodes = lazyTiles.map { _ => IntInternalOutputNode() } // this should be moved into the Tile...

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

  // Remaining external coreplex signals
  outer.debug.module.io.db <> io.debug
  outer.clint.module.io.rtcTick := io.rtcTick
  io.success := Bool(false) // Coreplex doesn't know when to stop running

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
}

abstract class BaseCoreplex(implicit p: Parameters) extends BareCoreplex
    with CoreplexNetwork
    with BankedL2CoherenceManagers
    with CoreplexRISCVPlatform {
  override lazy val module = new BaseCoreplexModule(this, () => new BaseCoreplexBundle(this))
}

class BaseCoreplexBundle[+L <: BaseCoreplex](_outer: L) extends BareCoreplexBundle(_outer)
    with CoreplexNetworkBundle
    with BankedL2CoherenceManagersBundle
    with CoreplexRISCVPlatformBundle

class BaseCoreplexModule[+L <: BaseCoreplex, +B <: BaseCoreplexBundle[L]](_outer: L, _io: () => B) extends BareCoreplexModule(_outer, _io)
    with CoreplexNetworkModule
    with BankedL2CoherenceManagersModule
    with CoreplexRISCVPlatformModule
