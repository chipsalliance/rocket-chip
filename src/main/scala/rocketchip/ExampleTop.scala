// See LICENSE for license details.

package rocketchip

import Chisel._
import cde.{Parameters, Field}
import junctions._
import coreplex._
import rocketchip._
import util.Pow2ClockDivider

abstract class BaseExampleTop(q: Parameters) extends BaseTop(q)
    with PeripheryBootROM
    with PeripheryDebug
    with PeripheryExtInterrupts
    with PeripheryCoreplexLocalInterrupter
    with PeripheryMasterMem
    with PeripheryMasterMMIO
    with PeripherySlave

abstract class BaseExampleTopBundle(p: Parameters) extends BaseTopBundle(p)
    with PeripheryBootROMBundle
    with PeripheryDebugBundle
    with PeripheryExtInterruptsBundle
    with PeripheryCoreplexLocalInterrupterBundle
    with PeripheryMasterMemBundle
    with PeripheryMasterMMIOBundle
    with PeripherySlaveBundle

abstract class BaseExampleTopModule[+L <: BaseExampleTop, +B <: BaseExampleTopBundle]
    (p: Parameters, l: L, b: => B) extends BaseTopModule(p, l, b)
    with PeripheryBootROMModule
    with PeripheryDebugModule
    with PeripheryExtInterruptsModule
    with PeripheryCoreplexLocalInterrupterModule
    with PeripheryMasterMemModule
    with PeripheryMasterMMIOModule
    with PeripherySlaveModule
    with HardwiredResetVector

/** Example Top with Periphery */
class ExampleTop(q: Parameters) extends BaseExampleTop(q) {
  override lazy val module = Module(new ExampleTopModule(p, this, new ExampleTopBundle(p)))
}

class ExampleTopBundle(p: Parameters) extends BaseExampleTopBundle(p)

class ExampleTopModule[+L <: ExampleTop, +B <: ExampleTopBundle]
    (p: Parameters, l: L, b: => B) extends BaseExampleTopModule(p, l, b)
    with DirectDebugConnection
    with DirectCoreplexLocalInterruptConnection
    with DirectMemoryConnection
    with DirectMMIOConnection
    with DirectSlaveConnection

/** Example Top with TestRAM */
class ExampleTopWithTestRAM(q: Parameters) extends ExampleTop(q)
    with PeripheryTestRAM {
  override lazy val module = Module(new ExampleTopWithTestRAMModule(p, this, new ExampleTopWithTestRAMBundle(p)))
}

class ExampleTopWithTestRAMBundle(p: Parameters) extends ExampleTopBundle(p)
    with PeripheryTestRAMBundle

class ExampleTopWithTestRAMModule[+L <: ExampleTopWithTestRAM, +B <: ExampleTopWithTestRAMBundle](p: Parameters, l: L, b: => B) extends ExampleTopModule(p, l, b)
    with PeripheryTestRAMModule

trait HasMultiClockCoreplex {
  val coreplex: Module
  val coreplexIO: BaseCoreplexBundle
  val multiClockCoreplexIO = coreplexIO.asInstanceOf[TileClockResetBundle]
  val tileClocks = multiClockCoreplexIO.tcrs.map(_.clock)
  val reset: Bool

  multiClockCoreplexIO.tcrs.foreach { tcr =>
    val tileDivider = Module(new Pow2ClockDivider(1))
    tcr.clock := tileDivider.io.clock_out
    tcr.reset := reset
  }

  val coreplexDivider = Module(new Pow2ClockDivider(2))
  coreplex.clock := coreplexDivider.io.clock_out
}

/** Example Top with Multi Clock */
class ExampleMultiClockTop(q: Parameters) extends BaseExampleTop(q) {
  override lazy val module = Module(new ExampleMultiClockTopModule(p, this, new ExampleMultiClockTopBundle(p)))
}

class ExampleMultiClockTopBundle(p: Parameters) extends BaseExampleTopBundle(p)

class ExampleMultiClockTopModule[+L <: ExampleMultiClockTop, +B <: ExampleMultiClockTopBundle]
    (p: Parameters, l: L, b: => B) extends BaseExampleTopModule(p, l, b)
    with HasMultiClockCoreplex
    with AsyncDebugConnection
    with AsyncCoreplexLocalInterruptConnection
    with AsyncMemoryConnection
    with AsyncMMIOConnection
    with AsyncSlaveConnection
