// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.tilelink._

case class FrontBusParams(
  beatBytes: Int,
  blockBytes: Int) extends HasTLBusParams

class FrontBus(params: FrontBusParams)(implicit p: Parameters)
    extends TLBusWrapper(params, "front_bus")
    with CanAttachTLMasters
    with HasTLXbarPhy
