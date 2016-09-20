// See LICENSE for license details.

package rocketchip

import Chisel._
import cde.{Parameters, Field}
import coreplex.Coreplex
import rocketchip._

/** Example Top with Periphery */
class ExampleTop(q: Parameters) extends BaseTop(q)
    with PeripheryBootROM with PeripheryDebug with PeripheryExtInterrupts with PeripheryCoreplexLocalInterrupter
    with PeripheryMasterMem with PeripheryMasterMMIO with PeripherySlave {
  override lazy val module = Module(new ExampleTopModule(p, this, new ExampleTopBundle(p, _)))
}

class ExampleTopBundle(p: Parameters, c: Coreplex) extends BaseTopBundle(p, c)
    with PeripheryBootROMBundle with PeripheryDebugBundle with PeripheryExtInterruptsBundle with PeripheryCoreplexLocalInterrupterBundle
    with PeripheryMasterMemBundle with PeripheryMasterMMIOBundle with PeripherySlaveBundle

class ExampleTopModule[+L <: ExampleTop, +B <: ExampleTopBundle](p: Parameters, l: L, b: Coreplex => B) extends BaseTopModule(p, l, b)
    with PeripheryBootROMModule with PeripheryDebugModule with PeripheryExtInterruptsModule with PeripheryCoreplexLocalInterrupterModule
    with PeripheryMasterMemModule with PeripheryMasterMMIOModule with PeripherySlaveModule
    with HardwiredResetVector

/** Example Top with TestRAM */
class ExampleTopWithTestRAM(q: Parameters) extends ExampleTop(q)
    with PeripheryTestRAM {
  override lazy val module = Module(new ExampleTopWithTestRAMModule(p, this, new ExampleTopWithTestRAMBundle(p, _)))
}

class ExampleTopWithTestRAMBundle(p: Parameters, c: Coreplex) extends ExampleTopBundle(p, c)
    with PeripheryTestRAMBundle

class ExampleTopWithTestRAMModule[+L <: ExampleTopWithTestRAM, +B <: ExampleTopWithTestRAMBundle](p: Parameters, l: L, b: Coreplex => B) extends ExampleTopModule(p, l, b)
    with PeripheryTestRAMModule
