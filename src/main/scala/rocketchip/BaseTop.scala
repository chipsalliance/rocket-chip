// See LICENSE.SiFive for license details.

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

/** Enable or disable monitoring of Diplomatic buses */
case object TLEmitMonitors extends Field[Boolean]

abstract class BareTop(implicit val p: Parameters) extends LazyModule {
  TopModule.contents = Some(this)
}

abstract class BareTopBundle[+L <: BareTop](_outer: L) extends Bundle {
  val outer = _outer
}

abstract class BareTopModule[+L <: BareTop, +B <: BareTopBundle[L]](_outer: L, _io: () => B) extends LazyModuleImp(_outer) {
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
class BaseTop(implicit p: Parameters) extends BareTop
    with TopNetwork {
  override lazy val module = new BaseTopModule(this, () => new BaseTopBundle(this))
}

class BaseTopBundle[+L <: BaseTop](_outer: L) extends BareTopBundle(_outer)
    with TopNetworkBundle

class BaseTopModule[+L <: BaseTop, +B <: BaseTopBundle[L]](_outer: L, _io: () => B) extends BareTopModule(_outer, _io)
    with TopNetworkModule

trait L2Crossbar extends TopNetwork {
  val l2 = LazyModule(new TLXbar)
}

trait L2CrossbarBundle extends TopNetworkBundle {
}

trait L2CrossbarModule extends TopNetworkModule {
}
