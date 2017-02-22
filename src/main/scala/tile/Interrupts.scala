// See LICENSE.SiFive for license details.

package tile

import Chisel._
import config.Parameters
import util._

class TileInterrupts(implicit p: Parameters) extends CoreBundle()(p) {
  val debug = Bool()
  val mtip = Bool()
  val msip = Bool()
  val meip = Bool()
  val seip = usingVM.option(Bool())
}
