// See LICENSE.SiFive for license details.

package rocketchip

import Chisel._
import config._
import junctions._
import rocketchip._

/** Example system with periphery devices (w/o coreplex) */
abstract class ExampleSystem(implicit p: Parameters) extends BaseSystem
    with HasPeripheryAsyncExtInterrupts
    with HasPeripheryMasterAXI4MemPort
    with HasPeripheryMasterAXI4MMIOPort
    with HasPeripherySlaveAXI4Port
    with HasPeripheryErrorSlave
    with HasPeripheryZeroSlave {
  override lazy val module = new ExampleSystemModule(this)
}

class ExampleSystemModule[+L <: ExampleSystem](_outer: L) extends BaseSystemModule(_outer)
    with HasPeripheryExtInterruptsModuleImp
    with HasPeripheryMasterAXI4MemPortModuleImp
    with HasPeripheryMasterAXI4MMIOPortModuleImp
    with HasPeripherySlaveAXI4PortModuleImp

/** Example Top with periphery and a Rocket coreplex */
class ExampleRocketTop(implicit p: Parameters) extends ExampleSystem
    with HasPeripheryBootROM
    with HasPeripheryDebug
    with HasPeripheryRTCCounter
    with HasRocketPlexMaster {
  override lazy val module = new ExampleRocketTopModule(this)
}

class ExampleRocketTopModule[+L <: ExampleRocketTop](_outer: L) extends ExampleSystemModule(_outer)
    with HasPeripheryBootROMModuleImp
    with HasPeripheryDebugModuleImp
    with HasPeripheryRTCCounterModuleImp
    with HasRocketPlexMasterModuleImp
