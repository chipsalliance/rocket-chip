// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile

import chisel3.Vec
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.prci._
import freechips.rocketchip.rocket.{TracedInstruction}
import freechips.rocketchip.subsystem.{TileCrossingParamsLike, CrossesToOnlyOneResetDomain}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{TraceCoreInterface}

/** A wrapper containing all logic within a managed reset domain for a tile.
  *
  * This does not add a layer of the module hierarchy.
  */
class TileResetDomain(clockSinkParams: ClockSinkParameters, resetCrossingType: ResetCrossingType)
                     (implicit p: Parameters)
    extends ResetDomain
    with CrossesToOnlyOneResetDomain
{ 
  def crossing = resetCrossingType
  val clockNode = ClockSinkNode(Seq(clockSinkParams))
  def clockBundle = clockNode.in.head._1
  override def shouldBeInlined = true
}

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
  crossingParams: TileCrossingParamsLike)
  (implicit p: Parameters)
    extends ClockDomain
{
  val tile: T
  val tile_reset_domain = LazyModule(new TileResetDomain(clockSinkParams, crossingParams.resetCrossingType))
  val tapClockNode = ClockIdentityNode()
  val clockNode = FixedClockBroadcast(None) :=* tapClockNode
  lazy val clockBundle = tapClockNode.in.head._1

  private val traceSignalName = "trace"
  private val traceCoreSignalName = "tracecore"
  /** Node to broadcast legacy "raw" instruction trace while surpressing it during (async) reset. */
  val traceNode: BundleBridgeIdentityNode[Vec[TracedInstruction]] = BundleBridgeNameNode(traceSignalName)
  /** Node to broadcast standardized instruction trace while surpressing it during (async) reset. */
  val traceCoreNode: BundleBridgeIdentityNode[TraceCoreInterface] = BundleBridgeNameNode(traceCoreSignalName)

  /** Function to handle all trace crossings when tile is instantiated inside domains */
  def crossTracesOut(): Unit = this {
    val traceNexusNode = BundleBridgeBlockDuringReset[Vec[TracedInstruction]](
      resetCrossingType = crossingParams.resetCrossingType,
      name = Some(traceSignalName))
    traceNode :*= traceNexusNode := tile.traceNode

    val traceCoreNexusNode = BundleBridgeBlockDuringReset[TraceCoreInterface](
      resetCrossingType = crossingParams.resetCrossingType,
      name = Some(traceCoreSignalName))
    traceCoreNode :*= traceCoreNexusNode := tile.traceCoreNode
  }

  /** External code looking to connect and clock-cross the interrupts driven into this tile can call this. */
  def crossIntIn(crossingType: ClockCrossingType): IntInwardNode = {
    // Unlike the other crossing helpers, here nothing is is blocked during reset because we know these are inputs and assume that tile reset is longer than uncore reset
    val intInClockXing = this.crossIn(tile.intInwardNode)
    intInClockXing(crossingType)
  }

  /** External code looking to connect and clock/reset-cross
    *   - interrupts raised by devices inside this tile
    *   - notifications raise by the cores and caches
    * can call this function to instantiate the required crossing hardware.
    * Takes crossingType as an argument because some interrupts are supposed to be synchronous
    * Takes tileNode as an argument because tiles might have multiple outbound interrupt nodes
    */
  def crossIntOut(crossingType: ClockCrossingType, tileNode: IntOutwardNode): IntOutwardNode = {
    val intOutResetXing = this { tile_reset_domain.crossIntOut(tileNode) }
    val intOutClockXing = this.crossOut(intOutResetXing)
    intOutClockXing(crossingType)
  }

  /** External code looking to connect the ports where this tile is slaved to an interconnect
    * (while also crossing clock domains) can call this.
    */
  def crossSlavePort(crossingType: ClockCrossingType): TLInwardNode = { DisableMonitors { implicit p => FlipRendering { implicit p =>
    val tlSlaveResetXing = this {
      tile_reset_domain.crossTLIn(tile.slaveNode) :*=
        tile.makeSlaveBoundaryBuffers(crossingType)
    }
    val tlSlaveClockXing = this.crossIn(tlSlaveResetXing)
    tlSlaveClockXing(crossingType)
  } } }

  /** External code looking to connect the ports where this tile masters an interconnect
    * (while also crossing clock domains) can call this.
    */
  def crossMasterPort(crossingType: ClockCrossingType): TLOutwardNode = {
    val tlMasterResetXing = this { DisableMonitors { implicit p =>
      tile.makeMasterBoundaryBuffers(crossingType) :=*
        tile_reset_domain.crossTLOut(tile.masterNode)
    } }
    val tlMasterClockXing = this.crossOut(tlMasterResetXing)
    tlMasterClockXing(crossingType)
  }
}
