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

/** BareTop is the root class for creating a top-level RTL module */
abstract class BareTop(implicit p: Parameters) extends LazyModule {
  ElaborationArtefacts.add("graphml", graphML)
}

abstract class BareTopBundle[+L <: BareTop](_outer: L) extends GenericParameterizedBundle(_outer) {
  val outer = _outer
  implicit val p = outer.p
}

abstract class BareTopModule[+L <: BareTop, +B <: BareTopBundle[L]](_outer: L, _io: () => B) extends LazyMultiIOModuleImp(_outer) {
  val outer = _outer
  val io = IO(_io())
}

/** HasTopLevelNetworks provides buses that will serve as attachment points,
  * for use in sub-traits that connect individual agents or external ports.
  */
trait HasTopLevelNetworks extends HasPeripheryParameters {
  val module: HasTopLevelNetworksModule

  val socBus = LazyModule(new TLXbar)          // Wide or unordered-access slave devices (TL-UH)
  val peripheryBus = LazyModule(new TLXbar)    // Narrow and ordered-access slave devices (TL-UL)
  val intBus = LazyModule(new IntXbar)         // Device and global external interrupts
  val fsb = LazyModule(new TLBuffer(BufferParams.none))          // Master devices talking to the frontside of the L2
  val bsb = LazyModule(new TLBuffer(BufferParams.none))          // Slave devices talking to the backside of the L2
  val mem = Seq.fill(nMemoryChannels) { LazyModule(new TLXbar) } // Ports out to DRAM

  // The peripheryBus hangs off of socBus;
  // here we convert TL-UH -> TL-UL
  peripheryBus.node :=
    TLBuffer()(
    TLWidthWidget(socBusConfig.beatBytes)(
    TLAtomicAutomata(arithmetic = peripheryBusArithmetic)(
    socBus.node)))
}

trait HasTopLevelNetworksBundle extends HasPeripheryParameters {
  val outer: HasTopLevelNetworks
}

trait HasTopLevelNetworksModule extends HasPeripheryParameters {
  val outer: HasTopLevelNetworks
  val io: HasTopLevelNetworksBundle
}

/** Base Top class with no peripheral devices or ports added */
abstract class BaseTop(implicit p: Parameters) extends BareTop
    with HasTopLevelNetworks {
  override val module: BaseTopModule[BaseTop, BaseTopBundle[BaseTop]]
}

abstract class BaseTopBundle[+L <: BaseTop](_outer: L) extends BareTopBundle(_outer)
    with HasTopLevelNetworksBundle

abstract class BaseTopModule[+L <: BaseTop, +B <: BaseTopBundle[L]](_outer: L, _io: () => B) extends BareTopModule(_outer, _io)
    with HasTopLevelNetworksModule
