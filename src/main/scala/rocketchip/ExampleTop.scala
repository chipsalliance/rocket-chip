// See LICENSE for license details.

package rocketchip

import Chisel._
import cde.{Parameters, Field}
import junctions._
import coreplex._
import rocketchip._

/** Example Top with Periphery */
class ExampleTop(q: Parameters) extends BaseTop(q)
    with PeripheryBootROM
    with PeripheryDebug
    with PeripheryExtInterrupts
    with PeripheryCoreplexLocalInterrupter
    with PeripheryMasterMem
    with PeripheryMasterAXI4MMIO
    with PeripherySlave {
  override lazy val module = Module(new ExampleTopModule(p, this, new ExampleTopBundle(p, this)))
}

class ExampleTopBundle[+L <: ExampleTop](p: Parameters, l: L) extends BaseTopBundle(p, l)
    with PeripheryBootROMBundle
    with PeripheryDebugBundle
    with PeripheryExtInterruptsBundle
    with PeripheryCoreplexLocalInterrupterBundle
    with PeripheryMasterMemBundle
    with PeripheryMasterAXI4MMIOBundle
    with PeripherySlaveBundle

class ExampleTopModule[+L <: ExampleTop, +B <: ExampleTopBundle[L]](p: Parameters, l: L, b: B) extends BaseTopModule(p, l, b)
    with PeripheryBootROMModule
    with PeripheryDebugModule
    with PeripheryExtInterruptsModule
    with PeripheryCoreplexLocalInterrupterModule
    with PeripheryMasterMemModule
    with PeripheryMasterAXI4MMIOModule
    with PeripherySlaveModule
    with HardwiredResetVector
    with DirectConnection

/** Example Top with TestRAM */
class ExampleTopWithTestRAM(q: Parameters) extends ExampleTop(q)
    with PeripheryTestRAM {
  override lazy val module = Module(new ExampleTopWithTestRAMModule(p, this, new ExampleTopWithTestRAMBundle(p, this)))
}

class ExampleTopWithTestRAMBundle[+L <: ExampleTopWithTestRAM](p: Parameters, l: L) extends ExampleTopBundle(p, l)
    with PeripheryTestRAMBundle

class ExampleTopWithTestRAMModule[+L <: ExampleTopWithTestRAM, +B <: ExampleTopWithTestRAMBundle[L]](p: Parameters, l: L, b: B) extends ExampleTopModule(p, l, b)
    with PeripheryTestRAMModule
