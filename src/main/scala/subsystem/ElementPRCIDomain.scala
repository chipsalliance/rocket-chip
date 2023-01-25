package freechips.rocketchip.subsystem

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.prci._
import freechips.rocketchip.tile.{RocketTile}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.debug.{TLDebugModule}
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.rocket.{TracedInstruction}
import freechips.rocketchip.util.{TraceCoreInterface}


/** A wrapper containing all logic within a managed reset domain for a element.
  *
  * This does not add a layer of the module hierarchy.
  */
class ElementResetDomain(clockSinkParams: ClockSinkParameters, resetCrossingType: ResetCrossingType)
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
abstract class ElementPRCIDomain[T <: BaseElement](
  clockSinkParams: ClockSinkParameters,
  crossingParams: ElementCrossingParamsLike)
  (implicit p: Parameters)
    extends ClockDomain
{
  val element: T
  val element_reset_domain = LazyModule(new ElementResetDomain(clockSinkParams, crossingParams.resetCrossingType))
  val tapClockNode = ClockIdentityNode()
  val clockNode = FixedClockBroadcast(None) :=* tapClockNode
  lazy val clockBundle = tapClockNode.in.head._1

  /** Node to broadcast legacy "raw" instruction trace while surpressing it during (async) reset. */
  val traceNodes: Map[Int, BundleBridgeIdentityNode[Vec[TracedInstruction]]]
  /** Node to broadcast standardized instruction trace while surpressing it during (async) reset. */
  val traceCoreNodes: Map[Int, BundleBridgeIdentityNode[TraceCoreInterface]]

  /** Function to handle all trace crossings when tile is instantiated inside domains */
  def crossTracesOut(): Unit = this {
    require(element.traceNodes.size == traceNodes.size)
    require(element.traceCoreNodes.size == traceCoreNodes.size)
    for (i <- 0 until traceNodes.size) {
      val traceNexusNode = BundleBridgeBlockDuringReset[Vec[TracedInstruction]](
        resetCrossingType = crossingParams.resetCrossingType)
      traceNodes(i) :*= traceNexusNode := element.traceNodes(i)
    }
    for (i <- 0 until traceCoreNodes.size) {
      val traceCoreNexusNode = BundleBridgeBlockDuringReset[TraceCoreInterface](
        resetCrossingType = crossingParams.resetCrossingType)
      traceCoreNodes(i) :*= traceCoreNexusNode := element.traceCoreNodes(i)
    }
  }

  /** External code looking to connect and clock-cross the interrupts driven into this tile can call this. */
  def crossIntIn(crossingType: ClockCrossingType, tileNode: IntInwardNode): IntInwardNode = {
    // Unlike the other crossing helpers, here nothing is is blocked during reset because we know these are inputs and assume that tile reset is longer than uncore reset
    val intInClockXing = this.crossIn(tileNode)
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
    val intOutResetXing = this { element_reset_domain.crossIntOut(tileNode) }
    val intOutClockXing = this.crossOut(intOutResetXing)
    intOutClockXing(crossingType)
  }

  /** External code looking to connect the ports where this tile is slaved to an interconnect
    * (while also crossing clock domains) can call this.
    */
  def crossSlavePort(crossingType: ClockCrossingType): TLInwardNode = { DisableMonitors { implicit p => FlipRendering { implicit p =>
    val tlSlaveResetXing = this {
      element_reset_domain.crossTLIn(element.slaveNode) :*=
        element.makeSlaveBoundaryBuffers(crossingType)
    }
    val tlSlaveClockXing = this.crossIn(tlSlaveResetXing)
    tlSlaveClockXing(crossingType)
  } } }

  /** External code looking to connect the ports where this tile masters an interconnect
    * (while also crossing clock domains) can call this.
    */
  def crossMasterPort(crossingType: ClockCrossingType): TLOutwardNode = {
    val tlMasterResetXing = this { DisableMonitors { implicit p =>
      element.makeMasterBoundaryBuffers(crossingType) :=*
        element_reset_domain.crossTLOut(element.masterNode)
    } }
    val tlMasterClockXing = this.crossOut(tlMasterResetXing)
    tlMasterClockXing(crossingType)
  }
}
