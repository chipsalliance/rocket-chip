// See LICENSE.SiFive for license details.

package coreplex

import Chisel._
import config._
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

/** Widths of various points in the SoC */
case class TLBusConfig(beatBytes: Int)
case object CBusConfig extends Field[TLBusConfig]
case object L1toL2Config extends Field[TLBusConfig]

/** L2 Broadcast Hub configuration */
case class BroadcastConfig(
  nTrackers:  Int     = 4,
  bufferless: Boolean = false)
case object BroadcastConfig extends Field[BroadcastConfig]

/** L2 memory subsystem configuration */
case class BankedL2Config(
  nMemoryChannels:  Int = 1,
  nBanksPerChannel: Int = 1,
  coherenceManager: Parameters => (TLInwardNode, TLOutwardNode) = { case q =>
    implicit val p = q
    val BroadcastConfig(nTrackers, bufferless) = p(BroadcastConfig)
    val bh = LazyModule(new TLBroadcast(p(CacheBlockBytes), nTrackers, bufferless))
    (bh.node, TLWidthWidget(p(L1toL2Config).beatBytes)(bh.node))
  }) {
  val nBanks = nMemoryChannels*nBanksPerChannel
}
case object BankedL2Config extends Field[BankedL2Config]

/** The file to read the BootROM contents from */
case object BootROMFile extends Field[String]

trait HasCoreplexParameters {
  implicit val p: Parameters
  lazy val cbusConfig = p(CBusConfig)
  lazy val l1tol2Config = p(L1toL2Config)
  lazy val nTiles = p(uncore.devices.NTiles)
  lazy val hasSupervisor = p(rocket.UseVM)
  lazy val l2Config = p(BankedL2Config)
}

case class CoreplexParameters(implicit val p: Parameters) extends HasCoreplexParameters

abstract class BareCoreplex(implicit p: Parameters) extends LazyModule
abstract class BareCoreplexBundle[+L <: BareCoreplex](_outer: L) extends GenericParameterizedBundle(_outer) {
  val outer = _outer
  implicit val p = outer.p
}
abstract class BareCoreplexModule[+L <: BareCoreplex, +B <: BareCoreplexBundle[L]](_outer: L, _io: () => B) extends LazyModuleImp(_outer) {
  val outer = _outer
  val io = _io ()
}

trait CoreplexNetwork extends HasCoreplexParameters {
  val module: CoreplexNetworkModule

  val l1tol2 = LazyModule(new TLXbar)
  val l1tol2_beatBytes = l1tol2Config.beatBytes
  val l1tol2_lineBytes = p(CacheBlockBytes)

  val cbus = LazyModule(new TLXbar)
  val cbus_beatBytes = cbusConfig.beatBytes
  val cbus_lineBytes = l1tol2_lineBytes

  val intBar = LazyModule(new IntXbar)

  val mmio = TLOutputNode()
  val mmioInt = IntInputNode()

  intBar.intnode := mmioInt

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
  val outer: CoreplexNetwork

  val mmio = outer.mmio.bundleOut
  val interrupts = outer.mmioInt.bundleIn
}

trait CoreplexNetworkModule extends HasCoreplexParameters {
  val outer: CoreplexNetwork
  val io: CoreplexNetworkBundle

  println("\nGenerated Address Map")
  for (manager <- outer.l1tol2.node.edgesIn(0).manager.managers) {
    val prot = (if (manager.supportsGet)     "R" else "") +
               (if (manager.supportsPutFull) "W" else "") +
               (if (manager.executable)      "X" else "") +
               (if (manager.supportsAcquire) " [C]" else "")
    manager.address.foreach { a =>
      println(f"\t${manager.name}%s ${a.base}%x - ${a.base+a.mask+1}%x, $prot")
    }
  }
}

trait BankedL2CoherenceManagers extends CoreplexNetwork {
  val module: BankedL2CoherenceManagersModule

  require (isPow2(l2Config.nBanksPerChannel))
  require (isPow2(l1tol2_lineBytes))

  val mem = Seq.fill(l2Config.nMemoryChannels) {
    val bankBar = LazyModule(new TLXbar)
    val output = TLOutputNode()

    output := bankBar.node
    val mask = ~BigInt((l2Config.nBanksPerChannel-1) * l1tol2_lineBytes)
    for (i <- 0 until l2Config.nBanksPerChannel) {
      val (in, out) = l2Config.coherenceManager(p)
      in := TLFilter(AddressSet(i * l1tol2_lineBytes, mask))(l1tol2.node)
      bankBar.node := out
    }

    output
  }
}

trait BankedL2CoherenceManagersBundle extends CoreplexNetworkBundle {
  val outer: BankedL2CoherenceManagers

  require (l2Config.nMemoryChannels <= 1, "Seq in Chisel Bundle needed to support > 1") // !!!
  val mem = outer.mem.map(_.bundleOut).toList.headOption // .headOption should be removed !!!
}

trait BankedL2CoherenceManagersModule extends CoreplexNetworkModule {
  val outer: BankedL2CoherenceManagers
  val io: BankedL2CoherenceManagersBundle
}

abstract class BaseCoreplex(implicit p: Parameters) extends BareCoreplex
    with CoreplexNetwork
    with BankedL2CoherenceManagers {
  override lazy val module = new BaseCoreplexModule(this, () => new BaseCoreplexBundle(this))
}

class BaseCoreplexBundle[+L <: BaseCoreplex](_outer: L) extends BareCoreplexBundle(_outer)
    with CoreplexNetworkBundle
    with BankedL2CoherenceManagersBundle

class BaseCoreplexModule[+L <: BaseCoreplex, +B <: BaseCoreplexBundle[L]](_outer: L, _io: () => B) extends BareCoreplexModule(_outer, _io)
    with CoreplexNetworkModule
    with BankedL2CoherenceManagersModule
