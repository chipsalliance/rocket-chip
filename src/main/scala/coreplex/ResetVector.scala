// See LICENSE for license details.

package freechips.rocketchip.coreplex

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.ResetVectorBits

/** A single place for all tiles to find out the reset vector */
trait HasResetVectorWire {
  implicit val p: Parameters
  val resetVectorBits = p(ResetVectorBits)
  val global_reset_vector = Wire(UInt(width = resetVectorBits))
}
