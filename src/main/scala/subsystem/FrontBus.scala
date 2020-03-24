// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{Location}

case class FrontBusParams(
    beatBytes: Int,
    blockBytes: Int,
    dtsFrequency: Option[BigInt] = None,
    zeroDevice: Option[AddressSet] = None,
    errorDevice: Option[DevNullParams] = None)
  extends HasTLBusParams
  with HasBuiltInDeviceParams
  with TLBusWrapperInstantiationLike
{
  def instantiate(context: HasLocations, loc: Location[TLBusWrapper])(implicit p: Parameters): FrontBus = {
    val fbus = LazyModule(new FrontBus(this, loc.name))
    context.tlBusWrapperLocationMap += (loc -> fbus)
    fbus
  }
}

class FrontBus(params: FrontBusParams, name: String = "front_bus")(implicit p: Parameters)
    extends TLBusWrapper(params, name)
    with HasTLXbarPhy {
  val builtInDevices: BuiltInDevices = BuiltInDevices.attach(params, outwardNode)
}
