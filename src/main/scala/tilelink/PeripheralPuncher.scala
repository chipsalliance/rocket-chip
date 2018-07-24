// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.regmapper.RegField
import freechips.rocketchip.tilelink._

trait HasCrossableTLControlRegMap extends HasClockDomainCrossing { this: PeripheralPuncher[_] =>
  protected val node = TLRegisterNode(
    address = address,
    device = device,
    deviceKey = "reg/control",
    concurrency = devParams.concurrency,
    beatBytes = devParams.beatBytes,
    undefZero = devParams.undefZero,
    executable = devParams.executable)
  protected def regmap(mapping: RegField.Map*) { InModuleBody { node.regmap(mapping:_*) } }
  val controlXing = this.crossIn(node)
}

trait HasCrossableInterrupts extends HasClockDomainCrossing { this: PeripheralPuncher[_] =>
  def nInterrupts: Int = 0
  protected val intnode = IntSourceNode(IntSourcePortSimple(num = nInterrupts, resources = Seq(Resource(device, "int"))))
  val interrupts = InModuleBody { if (intnode.out.isEmpty) Vec(0, Bool()) else intnode.out(0)._1 }
  val intXing = this.crossOut(intnode)
}

case class PeripheralPuncherParams(
  name: String,
  compat: Seq[String],
  base: BigInt,
  size: BigInt = 4096,
  concurrency: Int = 0,
  beatBytes: Int = 4,
  undefZero: Boolean = true,
  executable: Boolean = false)

abstract class PeripheralPuncher[T <: Data](val devParams: PeripheralPuncherParams, portBundle: => T)
                                           (implicit p: Parameters)
    extends LazyModule
    with HasClockDomainCrossing {

  val address = Seq(AddressSet(devParams.base, devParams.size-1))
  val device = new SimpleDevice(devParams.name, devParams.compat) {
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      Description(name, mapping ++ extraResources(resources))
    }
  }

  // Allow devices to extend the DTS mapping
  def extraResources(resources: ResourceBindings) = Map[String, Seq[ResourceValue]]()

  val ioNode = BundleBridgeSource(() => portBundle.cloneType)
  val port = InModuleBody { ioNode.bundle }

  protected def regmap(mapping: RegField.Map*): Unit
}
