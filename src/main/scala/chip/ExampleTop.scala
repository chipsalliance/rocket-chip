// See LICENSE.SiFive for license details.

package freechips.rocketchip.chip

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.coreplex._
import freechips.rocketchip.devices.tilelink._

/** Example Top with periphery devices and ports, and a Rocket coreplex */
class ExampleRocketTop(implicit p: Parameters) extends RocketCoreplex
    with HasAsyncExtInterrupts
    with HasMasterAXI4MemPort
    with HasMasterAXI4MMIOPort
    with HasSlaveAXI4Port
    with HasPeripheryBootROM
    with HasPeripheryErrorSlave
    with HasMemoryZeroSlave {
  override lazy val module = new ExampleRocketTopModule(this)
}

class ExampleRocketTopModule[+L <: ExampleRocketTop](_outer: L) extends RocketCoreplexModule(_outer)
    with HasExtInterruptsModuleImp
    with HasMasterAXI4MemPortModuleImp
    with HasMasterAXI4MMIOPortModuleImp
    with HasSlaveAXI4PortModuleImp
