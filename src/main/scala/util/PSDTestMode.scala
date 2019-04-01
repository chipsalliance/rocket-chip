// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy.BundleBridgeNexus

case object IncludePSDTest extends Field[Boolean](false)
//Note: one should never have `IncludePSDTest without
// PSDTestModeBroadcastKey defined everywhere one wants to use it.
// One could just use the PSDTestModeBroadcastKey.isDefined directly,
// but it's a more error prone process to instantiate it and pass it in an
// altered Parameters so the API is to have both.
case object PSDTestModeBroadcastKey extends Field[Option[BundleBridgeNexus[PSDTestMode]]](None)

class PSDTestMode extends Bundle {
  val test_mode       = Bool()
  val test_mode_reset = Bool()
  // TODO: Clocks?
}

trait CanHavePSDTestModeIO {
  implicit val p: Parameters
  val psd = p(IncludePSDTest).option(new PSDTestMode().asInput)
}
