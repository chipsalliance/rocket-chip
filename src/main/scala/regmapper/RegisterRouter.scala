// See LICENSE.SiFive for license details.

package freechips.rocketchip.regmapper

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tilelink._

case class RegisterRouterParams(
  name: String,
  compat: Seq[String],
  base: BigInt,
  size: BigInt = 4096,
  concurrency: Int = 0,
  beatBytes: Int = 4,
  undefZero: Boolean = true,
  executable: Boolean = false)

abstract class RegisterRouter[T <: Data](devParams: RegisterRouterParams)(implicit p: Parameters)
    extends LazyModule
    with HasClockDomainCrossing {

  require (isPow2(devParams.size))
  val address = Seq(AddressSet(devParams.base, devParams.size-1))
  val concurrency = devParams.concurrency
  val beatBytes = devParams.beatBytes
  val undefZero = devParams.undefZero
  val executable = devParams.executable
  val device = new SimpleDevice(devParams.name, devParams.compat) {
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      Description(name, mapping ++ extraResources(resources))
    }
  }
  // Allow devices to extend the DTS mapping
  def extraResources(resources: ResourceBindings) = Map[String, Seq[ResourceValue]]()

  protected def regmap(mapping: RegField.Map*): Unit
}

abstract class IORegisterRouter[T <: Data](devParams: RegisterRouterParams, portBundle: => T)(implicit p: Parameters)
    extends RegisterRouter(devParams) {
  val ioNode = BundleBridgeSource(() => portBundle.cloneType)
  val port = InModuleBody { ioNode.bundle }
}
