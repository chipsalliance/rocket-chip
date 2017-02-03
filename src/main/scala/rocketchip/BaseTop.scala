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

abstract class BareTop(implicit p: Parameters) extends LazyModule {
  ElaborationArtefacts.add("graphml", graphML)
}

abstract class BareTopBundle[+L <: BareTop](_outer: L) extends GenericParameterizedBundle(_outer) {
  val outer = _outer
  implicit val p = outer.p
}

abstract class BareTopModule[+L <: BareTop, +B <: BareTopBundle[L]](_outer: L, _io: () => B) extends LazyModuleImp(_outer) {
  val outer = _outer
  val io = _io ()
}

/** Base Top with no Periphery */
trait TopNetwork extends HasPeripheryParameters {
  val module: TopNetworkModule

  // Add a SoC and peripheral bus
  val socBus = LazyModule(new TLXbar)
  val peripheryBus = LazyModule(new TLXbar)
  val intBus = LazyModule(new IntXbar)
  val l2 = LazyModule(new TLBuffer)

  peripheryBus.node :=
    TLBuffer()(
    TLWidthWidget(socBusConfig.beatBytes)(
    TLAtomicAutomata(arithmetic = peripheryBusArithmetic)(
    socBus.node)))
}

trait TopNetworkBundle extends HasPeripheryParameters {
  val outer: TopNetwork
}

trait TopNetworkModule extends HasPeripheryParameters {
  val io: TopNetworkBundle
  val outer: TopNetwork
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
