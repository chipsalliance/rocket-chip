// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile

import chisel3.Vec
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.prci._
import freechips.rocketchip.rocket.{TracedInstruction}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{BlockDuringReset}

/** A wrapper containing all logic necessary to safely place a tile
  * inside of a particular Power/Reset/Clock/Interrupt domain.
  *
  * This adds a layer to the module hierarchy which is a parent of the tile
  * and should contain all logic related to clock crossings, isolation cells,
  * hierarchical P&R boundary buffers, core-local interrupt handling,
  * and any other IOs related to PRCI control.
  */
abstract class TilePRCIDomain[T <: BaseTile](clockSinkParams: ClockSinkParameters)(implicit p: Parameters)
    extends ClockDomain
{
  val tile: T

  val tapClockNode = ClockIdentityNode()
  val clockNode = FixedClockBroadcast(None) :=* tapClockNode
  val tile_reset_domain = LazyModule(new ClockSinkDomain(clockSinkParams) { override def shouldBeInlined = true })
  lazy val clockBundle = tapClockNode.in.head._1

  /** Node to broadcast legacy "raw" instruction trace while surpressing it during (async) reset. */
  val traceNexusNode = BundleBridgeNexus(
    inputFn = (s: Seq[Vec[TracedInstruction]]) => 
      BlockDuringReset(BundleBridgeNexus.requireOne[Vec[TracedInstruction]](false)(s)))

  /** External code looking to connect and clock-cross the interrupts driven into this tile can call this. */
  def crossIntIn(crossing: ClockCrossingType):IntInwardNode = {
    val intInXing = this.crossIn(tile.intInwardNode)
    intInXing(crossing)
  }

  /** External code looking to connect and clock/reset-cross
    *   - interrupts raised by devices inside this tile
    *   - notifications raise by the cores and caches
    * can call this function to instantiate the required crossing hardware.
    */
  def crossIntOut(crossing: ClockCrossingType, tileNode: IntOutwardNode): IntOutwardNode = {
    val intOutXing = this.crossOut(this { IntBlockDuringReset() :=* tileNode })
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
    val tlMasterXing = this.crossOut(this {
      tile.makeMasterBoundaryBuffers(crossing) :=*
        DisableMonitors { implicit p => TLBlockDuringReset() :=* tile.masterNode } // dont monitor reset domain crossing
    })
    tlMasterXing(crossing)
  }
}
