package freechips.rocketchip.subsystem

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.prci._
import freechips.rocketchip.tile.{RocketTile, TraceBundle}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.debug.{TLDebugModule}
import freechips.rocketchip.devices.tilelink._
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
  val traceNodes: Seq[BundleBridgeIdentityNode[TraceBundle]]
  /** Node to broadcast standardized instruction trace while surpressing it during (async) reset. */
  val traceCoreNodes: Seq[BundleBridgeIdentityNode[TraceCoreInterface]]
}
