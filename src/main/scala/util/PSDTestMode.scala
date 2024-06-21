// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._

import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy._
import org.chipsalliance.diplomacy.bundlebridge._

@deprecated("moved to standalone rocketutils library", "rocketchip 2.0.0")
case object IncludePSDTest extends Field[Boolean](false)
@deprecated("moved to standalone rocketutils library", "rocketchip 2.0.0")
case object PSDTestModeBroadcastKey extends Field(
  BundleBridgeEphemeralNode[PSDTestMode]()(ValName("global_psd_test_mode"))
)

@deprecated("moved to standalone rocketutils library", "rocketchip 2.0.0")
class PSDTestMode extends Bundle {
  val test_mode       = Bool()
  val test_mode_reset = Bool()
  // TODO: Clocks?
}

@deprecated("moved to standalone rocketutils library", "rocketchip 2.0.0")
trait CanHavePSDTestModeIO {
  implicit val p: Parameters
  val psd = p(IncludePSDTest).option(Input(new PSDTestMode()))
}
