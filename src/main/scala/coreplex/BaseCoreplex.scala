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
  val module: CoreplexNetworkModule

  val l1tol2 = LazyModule(new TLXbar)
  val l1tol2_beatBytes = p(TLKey("L2toMMIO")).dataBitsPerBeat/8
  val l1tol2_lineBytes = p(CacheBlockBytes)

  val cbus = LazyModule(new TLXbar)
  val cbus_beatBytes = p(XLen)/8
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

  implicit val p = outer.p
  val mmio = outer.mmio.bundleOut
  val interrupts = outer.mmioInt.bundleIn
}

trait CoreplexNetworkModule extends HasCoreplexParameters {
  val outer: CoreplexNetwork
  val io: CoreplexNetworkBundle

  implicit val p = outer.p
}

trait BankedL2CoherenceManagers extends CoreplexNetwork {
  val module: BankedL2CoherenceManagersModule

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

trait BankedL2CoherenceManagersBundle extends CoreplexNetworkBundle {
  val outer: BankedL2CoherenceManagers

  require (nMemChannels <= 1, "Seq in Chisel Bundle needed to support > 1") // !!!
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
