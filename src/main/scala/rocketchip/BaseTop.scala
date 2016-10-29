// See LICENSE for license details.

package rocketchip

import Chisel._
import cde.{Parameters, Field}
import junctions._
import diplomacy._
import uncore.tilelink._
import uncore.tilelink2._
import uncore.devices._
import util._
import rocket._
import coreplex._

// the following parameters will be refactored properly with TL2
case object GlobalAddrMap extends Field[AddrMap]
case object NCoreplexExtClients extends Field[Int]
/** Enable or disable monitoring of Diplomatic buses */
case object TLEmitMonitors extends Field[Bool]

abstract class BareTop[+C <: BaseCoreplex](buildCoreplex: Parameters => C)(implicit val q: Parameters) extends LazyModule {
  // Fill in the TL1 legacy parameters; remove these once rocket/groundtest/unittest are TL2
  val pBusMasters = new RangeManager
  lazy val legacyAddrMap = GenerateGlobalAddrMap(q, coreplex.l1tol2.node.edgesIn(0).manager.managers)
  val coreplex : C = LazyModule(buildCoreplex(q.alterPartial {
    case NCoreplexExtClients => pBusMasters.sum
    case GlobalAddrMap => legacyAddrMap
  }))

  TopModule.contents = Some(this)
}

abstract class BareTopBundle[+L <: BareTop[BaseCoreplex]](val outer: L) extends Bundle
abstract class BareTopModule[+B <: BareTopBundle[BareTop[BaseCoreplex]]](val io: B) extends LazyModuleImp(io.outer) {
  val outer = io.outer.asInstanceOf[io.outer.type]
}

/** Base Top with no Periphery */
trait TopNetwork extends HasPeripheryParameters {
  this: BareTop[BaseCoreplex] =>
  implicit val p = q
  TLImp.emitMonitors = p(TLEmitMonitors)

  // Add a SoC and peripheral bus
  val socBus = LazyModule(new TLXbar)
  val peripheryBus = LazyModule(new TLXbar)
  val intBus = LazyModule(new IntXbar)

  peripheryBus.node :=
    TLWidthWidget(p(SOCBusKey).beatBytes)(
    TLAtomicAutomata(arithmetic = p(PeripheryBusKey).arithAMO)(
    socBus.node))
}

trait TopNetworkBundle extends HasPeripheryParameters {
  this: BareTopBundle[BareTop[BaseCoreplex]] =>
  implicit val p = outer.q
  val success = Bool(OUTPUT)
}

trait TopNetworkModule extends HasPeripheryParameters {
  this: {
    val outer: BareTop[BaseCoreplex] with TopNetwork
    val io: TopNetworkBundle
  } =>
  implicit val p = outer.p

  val coreplexMem  : Vec[ClientUncachedTileLinkIO] = Wire(outer.coreplex.module.io.mem)
  val coreplexSlave: Vec[ClientUncachedTileLinkIO] = Wire(outer.coreplex.module.io.slave)
  val coreplexDebug: DebugBusIO                    = Wire(outer.coreplex.module.io.debug)

  io.success := outer.coreplex.module.io.success
}

/** Base Top with no Periphery */
class BaseTop[+C <: BaseCoreplex](buildCoreplex: Parameters => C)(implicit p: Parameters) extends BareTop(buildCoreplex)
    with TopNetwork {
  override lazy val module = new BaseTopModule(new BaseTopBundle(this))
}

class BaseTopBundle[+L <: BaseTop[BaseCoreplex]](outer: L) extends BareTopBundle(outer)
    with TopNetworkBundle

class BaseTopModule[+B <: BaseTopBundle[BaseTop[BaseCoreplex]]](io: B) extends BareTopModule(io)
    with TopNetworkModule

trait DirectConnection {
  this: BareTop[BaseCoreplex] with TopNetwork =>

  socBus.node := coreplex.mmio
  coreplex.mmioInt := intBus.intnode
}

trait DirectConnectionModule {
  this: TopNetworkModule {
    val outer: BaseTop[BaseCoreplex]
  } =>

  coreplexMem <> outer.coreplex.module.io.mem
  outer.coreplex.module.io.slave <> coreplexSlave
  outer.coreplex.module.io.debug <> coreplexDebug
}
