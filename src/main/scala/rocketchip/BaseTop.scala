// See LICENSE for license details.

package rocketchip

import Chisel._
import config._
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
/** Enable or disable monitoring of Diplomatic buses */
case object TLEmitMonitors extends Field[Boolean]

abstract class BareTop[+C <: BaseCoreplex](_coreplex: Parameters => C)(implicit val p: Parameters) extends LazyModule {
  // Fill in the TL1 legacy parameters; remove these once rocket/groundtest/unittest are TL2
  lazy val legacyAddrMap = GenerateGlobalAddrMap(p, coreplex.l1tol2.node.edgesIn(0).manager.managers)
  val coreplex : C = LazyModule(_coreplex(p.alterPartial {
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
  val module: TopNetworkModule

  TLImp.emitMonitors = p(TLEmitMonitors)

  // Add a SoC and peripheral bus
  val socBus = LazyModule(new TLXbar)
  val peripheryBus = LazyModule(new TLXbar)
  val intBus = LazyModule(new IntXbar)

  peripheryBus.node :=
    TLWidthWidget(socBusConfig.beatBytes)(
    TLAtomicAutomata(arithmetic = peripheryBusArithmetic)(
    socBus.node))

  var coreplexMem = Seq[TLOutwardNode]()
}

trait TopNetworkBundle extends HasPeripheryParameters {
  val outer: TopNetwork
  implicit val p = outer.p
}

trait TopNetworkModule extends HasPeripheryParameters {
  val io: TopNetworkBundle
  val outer: TopNetwork
  implicit val p = outer.p
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

trait DirectConnection extends TopNetwork {
  val coreplex: BaseCoreplex

  socBus.node := coreplex.mmio
  coreplex.mmioInt := intBus.intnode

  coreplexMem = coreplex.mem
}
