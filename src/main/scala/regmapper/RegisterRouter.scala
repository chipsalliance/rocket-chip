// See LICENSE.SiFive for license details.

package freechips.rocketchip.regmapper

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{ResourceBindings, _}
import freechips.rocketchip.diplomaticobjectmodel.DiplomaticObjectModelAddressing
import freechips.rocketchip.diplomaticobjectmodel.model._
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

  var resourceBindings: Option[ResourceBindings] = None
  var omComponent: Option[OMComponent] = None

  val device = new SimpleDevice(devParams.name, devParams.compat) {
    override def describe(resources: ResourceBindings): Description = {
      resourceBindings = Some(resources)

      val Description(name, mapping) = super.describe(resources)
      Description(name, mapping ++ extraResources(resources))
    }

    def getInterrupts(): Seq[OMInterrupt] = {
      DiplomaticObjectModelAddressing.describeInterrupts(describe(resourceBindings.get).name, resourceBindings.get)
    }

    def setOMComponent(omc: OMComponent): Unit = {
      omComponent = Some(omc)
    }

    override def getOMComponents(resourceBindingsMap: ResourceBindingsMap): Seq[OMComponent] = {
      DiplomaticObjectModelAddressing.getOMComponentHelper(this, resourceBindingsMap, get)
    }

    /**
      * Spoofing the function that the  DiplomaticObjectModelAddressing.getOMComponentHelper expects
      * @param resourceBindings
      * @return
      */
    def get(resourceBindings: ResourceBindings): Seq[OMComponent] = {
      Seq(omComponent.get)
    }
  }

  // Allow devices to extend the DTS mapping
  def extraResources(resources: ResourceBindings) = Map[String, Seq[ResourceValue]]()

  protected def regmap(mapping: RegField.Map*): OMRegisterMap
}

abstract class IORegisterRouter[T <: Data](devParams: RegisterRouterParams, portBundle: => T)(implicit p: Parameters)
    extends RegisterRouter(devParams) {
  val ioNode = BundleBridgeSource(() => portBundle.cloneType)
  val port = InModuleBody { ioNode.bundle }
}
