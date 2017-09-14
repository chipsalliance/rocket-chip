// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import Chisel._
import freechips.rocketchip.config._

case object IncludePSDTest extends Field[Boolean](false)

class PSDTestModeIO extends Bundle {
  val test_mode       = Bool(INPUT)
  val test_mode_reset = Bool(INPUT)
  // TODO: Clocks?
}

trait CanHavePSDTestModeIO {
  implicit val p: Parameters
  val psd = p(IncludePSDTest).option(new PSDTestModeIO())
}
