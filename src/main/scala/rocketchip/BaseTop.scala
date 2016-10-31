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

abstract class BareTop[+C <: BaseCoreplex](_coreplex: Parameters => C)(implicit val q: Parameters) extends LazyModule {
  // Fill in the TL1 legacy parameters; remove these once rocket/groundtest/unittest are TL2
  val pBusMasters = new RangeManager
  lazy val legacyAddrMap = GenerateGlobalAddrMap(q, coreplex.l1tol2.node.edgesIn(0).manager.managers)
  val coreplex : C = LazyModule(_coreplex(q.alterPartial {
    case NCoreplexExtClients => pBusMasters.sum
    case GlobalAddrMap => legacyAddrMap
  }))

  TopModule.contents = Some(this)
}

abstract class BareTopBundle[+L <: BareTop[BaseCoreplex]](_outer: L) extends Bundle {
  val outer = _outer
}

abstract class BareTopModule[+L <: BareTop[BaseCoreplex], +B <: BareTopBundle[L]](_outer: L, _io: () => B) extends LazyModuleImp(_outer) {
  val outer = _outer
  val io = _io ()
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
  val coreplexRtc  : Bool                          = Wire(outer.coreplex.module.io.rtcTick)

  io.success := outer.coreplex.module.io.success

  outer.coreplex.module.io.rtcTick := coreplexRtc
  coreplexRtc := Counter(p(rocketchip.RTCPeriod)).inc()
}

/** Base Top with no Periphery */
class BaseTop[+C <: BaseCoreplex](_coreplex: Parameters => C)(implicit p: Parameters) extends BareTop(_coreplex)
    with TopNetwork {
  override lazy val module = new BaseTopModule(this, () => new BaseTopBundle(this))
}

class BaseTopBundle[+L <: BaseTop[BaseCoreplex]](_outer: L) extends BareTopBundle(_outer)
    with TopNetworkBundle

class BaseTopModule[+L <: BaseTop[BaseCoreplex], +B <: BaseTopBundle[L]](_outer: L, _io: () => B) extends BareTopModule(_outer, _io)
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
