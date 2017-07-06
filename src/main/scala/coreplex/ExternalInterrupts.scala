// See LICENSE.SiFive for license details.

package freechips.rocketchip.coreplex

import Chisel._
import freechips.rocketchip.config.Field
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

/** Specifies the number of external interrupts */
case object NExtTopInterrupts extends Field[Int]

/** This trait adds externally driven interrupts to the system. 
  * However, it should not be used directly; instead one of the below
  * synchronization wiring child traits should be used.
  */
abstract trait HasExtInterrupts extends HasInterruptBus {
  private val device = new Device with DeviceInterrupts {
    def describe(resources: ResourceBindings): Description = {
      Description("soc/external-interrupts", describeInterrupts(resources))
    }
  }

  val nExtInterrupts = p(NExtTopInterrupts)
  val extInterrupts = IntInternalInputNode(IntSourcePortSimple(num = nExtInterrupts, resources = device.int))
}

/** This trait should be used if the External Interrupts have NOT
  * already been synchronized to the Periphery (PLIC) Clock.
  */
trait HasAsyncExtInterrupts extends HasExtInterrupts {
  if (nExtInterrupts > 0) {
    val extInterruptXing = LazyModule(new IntXing)
    interruptBusNode := extInterruptXing.intnode
    extInterruptXing.intnode := extInterrupts
  }
}

/** This trait can be used if the External Interrupts have already been synchronized
  * to the Periphery (PLIC) Clock.
  */
trait HasSyncExtInterrupts extends HasExtInterrupts {
  if (nExtInterrupts > 0) {
    interruptBusNode := extInterrupts
  }
}

/** Common io name and methods for propagating or tying off the port bundle */
trait HasExtInterruptsBundle {
  val interrupts: UInt

  def tieOffInterrupts(dummy: Int = 1) {
    interrupts := UInt(0)
  }
}

/** This trait performs the translation from a UInt IO into Diplomatic Interrupts.
  * The wiring must be done in the concrete LazyModuleImp. 
  */
trait HasExtInterruptsModuleImp extends LazyMultiIOModuleImp with HasExtInterruptsBundle {
  val outer: HasExtInterrupts
  val interrupts = IO(UInt(INPUT, width = outer.nExtInterrupts))

  outer.extInterrupts.bundleIn.flatten.zipWithIndex.foreach { case(o, i) => o := interrupts(i) }
}
