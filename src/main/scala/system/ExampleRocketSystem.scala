// See LICENSE.SiFive for license details.

package freechips.rocketchip.system

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.coreplex._
import freechips.rocketchip.devices.tilelink._

/** Example Top with periphery devices and ports, and a Rocket coreplex */
class ExampleRocketSystem(implicit p: Parameters) extends RocketCoreplex
    with HasAsyncExtInterrupts
    with HasMasterAXI4MemPort
    with HasMasterAXI4MMIOPort
    with HasSlaveAXI4Port
    with HasPeripheryBootROM
    with HasSystemErrorSlave {
  override lazy val module = new ExampleRocketSystemModule(this)
}

class ExampleRocketSystemModule[+L <: ExampleRocketSystem](_outer: L) extends RocketCoreplexModule(_outer)
    with HasRTCModuleImp
    with HasExtInterruptsModuleImp
    with HasMasterAXI4MemPortModuleImp
    with HasMasterAXI4MMIOPortModuleImp
    with HasSlaveAXI4PortModuleImp
    with HasPeripheryBootROMModuleImp
