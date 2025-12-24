package freechips.rocketchip.subsystem

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy.lazymodule._

import freechips.rocketchip.devices.debug.TLDebugModule
import org.chipsalliance.diplomacy.{DisableMonitors, FlipRendering}
import freechips.rocketchip.interrupts.{IntInwardNode, IntOutwardNode}
import freechips.rocketchip.prci.{ClockCrossingType, ResetCrossingType, ResetDomain, ClockSinkNode, ClockSinkParameters, ClockIdentityNode, FixedClockBroadcast, ClockDomain}
import freechips.rocketchip.tile.{RocketTile, TraceBundle}
import freechips.rocketchip.tilelink.{TLInwardNode, TLOutwardNode}
import freechips.rocketchip.trace.TraceCoreInterface

import freechips.rocketchip.tilelink.TLClockDomainCrossing
import freechips.rocketchip.tilelink.TLResetDomainCrossing
import freechips.rocketchip.interrupts.IntClockDomainCrossing
import freechips.rocketchip.interrupts.IntResetDomainCrossing

/** A wrapper containing all logic within a managed reset domain for a element.
  *
  * This does not add a layer of the module hierarchy.
  */
class HierarchicalElementResetDomain(clockSinkParams: ClockSinkParameters, resetCrossingType: ResetCrossingType)
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
abstract class HierarchicalElementPRCIDomain[T <: BaseHierarchicalElement](
  clockSinkParams: ClockSinkParameters,
  crossingParams: HierarchicalElementCrossingParamsLike)
  (implicit p: Parameters)
    extends ClockDomain
{
  val element: T
  val element_reset_domain = LazyModule(new HierarchicalElementResetDomain(clockSinkParams, crossingParams.resetCrossingType))
  val tapClockNode = ClockIdentityNode()
  val clockNode = FixedClockBroadcast() :=* tapClockNode
  lazy val clockBundle = tapClockNode.in.head._1

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
        element { element.makeSlaveBoundaryBuffers(crossingType) }
    }
    val tlSlaveClockXing = this.crossIn(tlSlaveResetXing)
    tlSlaveClockXing(crossingType)
  } } }

  /** External code looking to connect the ports where this tile masters an interconnect
    * (while also crossing clock domains) can call this.
    */
  def crossMasterPort(crossingType: ClockCrossingType): TLOutwardNode = {
    val tlMasterResetXing = this { DisableMonitors { implicit p =>
      element { element.makeMasterBoundaryBuffers(crossingType) } :=*
        element_reset_domain.crossTLOut(element.masterNode)
    } }
    val tlMasterClockXing = this.crossOut(tlMasterResetXing)
    tlMasterClockXing(crossingType)
  }
}
