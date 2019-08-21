// See LICENSE.SiFive for license details.

package freechips.rocketchip.interrupts

import chisel3._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._

/** Mix this trait into a RegisterRouter to be able to attach its interrupt sources to an interrupt bus */
trait HasInterruptSources { this: RegisterRouter[_] =>
  def nInterrupts: Int
  protected val intnode = IntSourceNode(IntSourcePortSimple(num = nInterrupts, resources = Seq(Resource(device, "int"))))

  // Externally, this helper should be used to connect the interrupts to a bus
  val intXing: IntOutwardCrossingHelper = this.crossOut(intnode)

  // Internally, this wire should be used to drive interrupt values
  val interrupts: ModuleValue[Vec[Bool]] = InModuleBody { if (intnode.out.isEmpty) Vec(0, Bool()) else intnode.out(0)._1 }
}
