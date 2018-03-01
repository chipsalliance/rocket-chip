// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import Chisel._

/** A single place for all tiles to find out the reset vector */
trait HasResetVectorWire {
  def resetVectorBits: Int
  val global_reset_vector = Wire(UInt(width = resetVectorBits))
}
