// See LICENSE.SiFive for license details.

package rocketchip

import Chisel._
import config._
import junctions._
import rocketchip._

/** Example Top with Periphery (w/o coreplex) */
abstract class ExampleTop(implicit p: Parameters) extends BaseTop
    with PeripheryExtInterrupts
    with PeripheryMasterAXI4Mem
    with PeripheryMasterAXI4MMIO
    with PeripherySlaveAXI4{
  override lazy val module = new ExampleTopModule(this, () => new ExampleTopBundle(this))
}

class ExampleTopBundle[+L <: ExampleTop](_outer: L) extends BaseTopBundle(_outer)
    with PeripheryExtInterruptsBundle
    with PeripheryMasterAXI4MemBundle
    with PeripheryMasterAXI4MMIOBundle
    with PeripherySlaveAXI4Bundle

class ExampleTopModule[+L <: ExampleTop, +B <: ExampleTopBundle[L]](_outer: L, _io: () => B) extends BaseTopModule(_outer, _io)
    with PeripheryExtInterruptsModule
    with PeripheryMasterAXI4MemModule
    with PeripheryMasterAXI4MMIOModule
    with PeripherySlaveAXI4Module

class ExampleRocketTop(implicit p: Parameters) extends ExampleTop
    with PeripheryBootROM
    with PeripheryZero
    with PeripheryDebug
    with PeripheryCounter
    with HardwiredResetVector
    with RocketPlexMaster {
  override lazy val module = new ExampleRocketTopModule(this, () => new ExampleRocketTopBundle(this))
}

class ExampleRocketTopBundle[+L <: ExampleRocketTop](_outer: L) extends ExampleTopBundle(_outer)
    with PeripheryBootROMBundle
    with PeripheryZeroBundle
    with PeripheryDebugBundle
    with PeripheryCounterBundle
    with HardwiredResetVectorBundle
    with RocketPlexMasterBundle

class ExampleRocketTopModule[+L <: ExampleRocketTop, +B <: ExampleRocketTopBundle[L]](_outer: L, _io: () => B) extends ExampleTopModule(_outer, _io)
    with PeripheryBootROMModule
    with PeripheryZeroModule
    with PeripheryDebugModule
    with PeripheryCounterModule
    with HardwiredResetVectorModule
    with RocketPlexMasterModule
