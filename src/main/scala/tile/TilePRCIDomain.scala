// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.prci._
import freechips.rocketchip.tilelink._

/** A wrapper containing all logic necessary to safely place a tile
  * inside of a particular Power/Reset/Clock/Interrupt domain.
  *
  * This adds a layer to the module hierarchy which is a parent of the tile
  * and should contain all logic related to clock crossings, isolation cells,
  * hierarchical P&R boundary buffers, core-local interrupt handling,
  * and any other IOs related to PRCI control.
  */
abstract class TilePRCIDomain[T <: BaseTile](id: Int)(implicit p: Parameters)
    extends ClockDomain
{
  val tile: T

  val clockNode = ClockIdentityNode()
  val clockSinkNode = ClockSinkNode(Seq(ClockSinkParameters(take = None, name = Some(s"core_$id"))))
  lazy val clockBundle = clockSinkNode.in.head._1

  /** External code looking to connect and clock-cross the interrupts driven into this tile can call this. */
  def crossIntIn(crossing: ClockCrossingType):  IntInwardNode = {
    val intInXing = this.crossIn(tile.intInwardNode)
    intInXing(crossing)
  }

  /** External code looking to connect and clock-cross the interrupts raised by devices inside this tile can call this. */
  def crossIntOut(crossing: ClockCrossingType): IntOutwardNode = {
    val intOutXing = this.crossOut(tile.intOutwardNode)
    intOutXing(crossing)
  }

  /** External code looking to connect the ports where this tile is slaved to an interconnect
    * (while also crossing clock domains) can call this.
    */
  def crossSlavePort(crossing: ClockCrossingType): TLInwardNode = { DisableMonitors { implicit p => FlipRendering { implicit p =>
    val tlSlaveXing = this.crossIn({
      tile.slaveNode :*= this { tile.makeSlaveBoundaryBuffers(crossing) }
    })
    tlSlaveXing(crossing)
  } } }

  /** External code looking to connect the ports where this tile masters an interconnect
    * (while also crossing clock domains) can call this.
    */
  def crossMasterPort(crossing: ClockCrossingType): TLOutwardNode = {
    val tlMasterXing = this.crossOut({
      this { tile.makeMasterBoundaryBuffers(crossing) } :=* tile.masterNode
    })
    tlMasterXing(crossing)
  }
}
