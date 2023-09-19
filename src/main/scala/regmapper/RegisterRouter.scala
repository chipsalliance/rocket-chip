// See LICENSE.SiFive for license details.

package freechips.rocketchip.regmapper

import chisel3._
import chisel3.util.{isPow2}
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._

/** Parameters which apply to any RegisterRouter. */
case class RegisterRouterParams(
  name: String,
  compat: Seq[String],
  base: BigInt,
  size: BigInt = 4096,
  concurrency: Int = 0,
  beatBytes: Int = 4,
  undefZero: Boolean = true,
  executable: Boolean = false)

/** Subclasses of RegisterRouter are LazyModules comprising software-visible devices that contain a set of MMIO registers. */
abstract class RegisterRouter(devParams: RegisterRouterParams)(implicit p: Parameters)
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

/** Subclasses of IORegisterRouter are RegisterRouters that also contain an external IO port that is encapsulated as a BundleBridgeSource.
  * - Type parameter T is the Data subclass represention the IO's ports wire representation.
  */
abstract class IORegisterRouter[T <: Data](devParams: RegisterRouterParams, portBundle: => T)(implicit p: Parameters)
    extends RegisterRouter(devParams) {
  val ioNode = BundleBridgeSource(() => portBundle.cloneType)
  val port = InModuleBody { ioNode.bundle }
}
