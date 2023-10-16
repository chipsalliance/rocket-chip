// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile

import chisel3.Vec
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.prci._
import freechips.rocketchip.rocket.{TracedInstruction}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{TraceCoreInterface}


/** A wrapper containing all logic necessary to safely place a tile
  * inside of a particular Power/Reset/Clock/Interrupt domain.
  *
  * This adds a layer to the module hierarchy which is a parent of the tile
  * and should contain all logic related to clock crossings, isolation cells,
  * hierarchical P&R boundary buffers, core-local interrupt handling,
  * and any other IOs related to PRCI control.
  */
abstract class TilePRCIDomain[T <: BaseTile](
  clockSinkParams: ClockSinkParameters,
  crossingParams: HierarchicalElementCrossingParamsLike)
  (implicit p: Parameters)
    extends HierarchicalElementPRCIDomain[T](clockSinkParams, crossingParams)
{
  def tile_reset_domain = element_reset_domain
}
