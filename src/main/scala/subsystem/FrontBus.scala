// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

case class FrontBusParams(
    beatBytes: Int,
    blockBytes: Int,
    zeroDevice: Option[AddressSet] = None,
    errorDevice: Option[DevNullParams] = None)
  extends HasTLBusParams with HasBuiltInDeviceParams

class FrontBus(params: FrontBusParams)(implicit p: Parameters)
    extends TLBusWrapper(params, "front_bus")
    with CanHaveBuiltInDevices
    with CanAttachTLMasters
    with HasTLXbarPhy {
  attachBuiltInDevices(params)
}
