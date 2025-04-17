// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy.lazymodule._

import freechips.rocketchip.devices.tilelink.{BuiltInErrorDeviceParams, BuiltInZeroDeviceParams, BuiltInDevices, HasBuiltInDeviceParams}
import freechips.rocketchip.tilelink.{HasTLBusParams, TLBusWrapper, TLBusWrapperInstantiationLike, HasTLXbarPhy}
import freechips.rocketchip.util.{Location}

case class FrontBusParams(
    beatBytes: Int,
    blockBytes: Int,
    dtsFrequency: Option[BigInt] = None,
    zeroDevice: Option[BuiltInZeroDeviceParams] = None,
    errorDevice: Option[BuiltInErrorDeviceParams] = None)
  extends HasTLBusParams
  with HasBuiltInDeviceParams
  with TLBusWrapperInstantiationLike
{
  def instantiate(context: HasTileLinkLocations with HasPRCILocations with LazyModule, loc: Location[TLBusWrapper])(implicit p: Parameters): FrontBus = {
    val fbus = LazyModule(new FrontBus(this, loc.name))
    fbus.suggestName(loc.name)
    context.tlBusWrapperLocationMap += (loc -> fbus)
    fbus
  }
}

class FrontBus(params: FrontBusParams, name: String = "front_bus")(implicit p: Parameters)
    extends TLBusWrapper(params, name)
    with HasTLXbarPhy {
  val builtInDevices: BuiltInDevices = BuiltInDevices.attach(params, outwardNode)
  val prefixNode = None
}
