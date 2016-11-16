// See LICENSE for license details.

package rocketchip

import Chisel._
import cde.{Parameters, Field}
import junctions._
import coreplex._
import rocketchip._

/** Example Top with Periphery */
class ExampleTop[+C <: BaseCoreplex](_coreplex: Parameters => C)(implicit p: Parameters) extends BaseTop(_coreplex)
    with DirectConnection
    with PeripheryBootROM
    with PeripheryExtInterrupts
    with PeripheryMasterAXI4Mem
    with PeripheryMasterAXI4MMIO {
  override lazy val module = new ExampleTopModule(this, () => new ExampleTopBundle(this))
}

class ExampleTopBundle[+L <: ExampleTop[BaseCoreplex]](_outer: L) extends BaseTopBundle(_outer)
    with PeripheryBootROMBundle
    with PeripheryExtInterruptsBundle
    with PeripheryMasterAXI4MemBundle
    with PeripheryMasterAXI4MMIOBundle

class ExampleTopModule[+L <: ExampleTop[BaseCoreplex], +B <: ExampleTopBundle[L]](_outer: L, _io: () => B) extends BaseTopModule(_outer, _io)
    with PeripheryBootROMModule
    with PeripheryExtInterruptsModule
    with PeripheryMasterAXI4MemModule
    with PeripheryMasterAXI4MMIOModule

class ExampleRocketTop[+C <: DefaultCoreplex](_coreplex: Parameters => C)(implicit p: Parameters) extends ExampleTop(_coreplex)
    with PeripheryDTM
    with PeripheryCounter
    with HardwiredResetVector {
  override lazy val module = new ExampleRocketTopModule(this, () => new ExampleRocketTopBundle(this))
}

class ExampleRocketTopBundle[+L <: ExampleRocketTop[DefaultCoreplex]](_outer: L) extends ExampleTopBundle(_outer)
    with PeripheryDTMBundle
    with PeripheryCounterBundle
    with HardwiredResetVectorBundle

class ExampleRocketTopModule[+L <: ExampleRocketTop[DefaultCoreplex], +B <: ExampleRocketTopBundle[L]](_outer: L, _io: () => B) extends ExampleTopModule(_outer, _io)
    with PeripheryDTMModule
    with PeripheryCounterModule
    with HardwiredResetVectorModule

/** Example Top with TestRAM */
class ExampleTopWithTestRAM[+C <: BaseCoreplex](_coreplex: Parameters => C)(implicit p: Parameters) extends ExampleTop(_coreplex)
    with PeripheryTestRAM {
  override lazy val module = new ExampleTopWithTestRAMModule(this, () => new ExampleTopWithTestRAMBundle(this))
}

class ExampleTopWithTestRAMBundle[+L <: ExampleTopWithTestRAM[BaseCoreplex]](_outer: L) extends ExampleTopBundle(_outer)
    with PeripheryTestRAMBundle

class ExampleTopWithTestRAMModule[+L <: ExampleTopWithTestRAM[BaseCoreplex], +B <: ExampleTopWithTestRAMBundle[L]](_outer: L, _io: () => B) extends ExampleTopModule(_outer, _io)
    with PeripheryTestRAMModule
