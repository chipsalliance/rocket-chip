package freechips.rocketchip.subsystem

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.prci._
import freechips.rocketchip.tile.{LookupByHartIdImpl}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import freechips.rocketchip.rocket.{TracedInstruction}
import freechips.rocketchip.devices.debug.{TLDebugModule}
import freechips.rocketchip.devices.tilelink._

trait ElementParams {
  val name: String
}

abstract class InstantiableElementParams[ElementType <: BaseElement] extends ElementParams

/** An interface for describing the parameteization of how Elements are connected to interconnects */
trait ElementCrossingParamsLike {
  /** The type of clock crossing that should be inserted at the element boundary. */
  def crossingType: ClockCrossingType
  /** Parameters describing the contents and behavior of the point where the element is attached as an interconnect master. */
  def master: ElementPortParamsLike
  /** Parameters describing the contents and behavior of the point where the element is attached as an interconnect slave. */
  def slave: ElementPortParamsLike
  /** The subnetwork location of the device selecting the apparent base address of MMIO devices inside the element */
  def mmioBaseAddressPrefixWhere: TLBusWrapperLocation
  /** Inject a reset management subgraph that effects the element child reset only */
  def resetCrossingType: ResetCrossingType
  /** Keep the element clock separate from the interconnect clock (e.g. even if they are synchronous to one another) */
  def forceSeparateClockReset: Boolean
}

/** An interface for describing the parameterization of how a particular element port is connected to an interconnect */
trait ElementPortParamsLike {
  /** The subnetwork location of the interconnect to which this element port should be connected. */
  def where: TLBusWrapperLocation
  /** Allows port-specific adapters to be injected into the interconnect side of the attachment point. */
  def injectNode(context: Attachable)(implicit p: Parameters): TLNode
}

abstract class BaseElement (val crossing: ClockCrossingType)(implicit p: Parameters)
    extends LazyModule()(p)
    with CrossesToOnlyOneClockDomain
{
  def module: BaseElementModuleImp[BaseElement]
  def masterNode: TLOutwardNode
  def slaveNode: TLInwardNode
  def intInwardNode: IntInwardNode    // Interrupts to the core from external devices
  def intOutwardNode: IntOutwardNode  // Interrupts from tile-internal devices (e.g. BEU)
  def haltNode: IntOutwardNode        // Unrecoverable error has occurred; suggest reset
  def ceaseNode: IntOutwardNode       // Tile has ceased to retire instructions
  def wfiNode: IntOutwardNode         // Tile is waiting for an interrupt

  protected val tlOtherMastersNode = TLIdentityNode()
  protected val tlMasterXbar = LazyModule(new TLXbar)
  protected val tlSlaveXbar = LazyModule(new TLXbar)
  protected val intXbar = LazyModule(new IntXbar)

  val traceCoreNodes: Seq[BundleBridgeOutwardNode[TraceCoreInterface]]
  val traceNodes: Seq[BundleBridgeOutwardNode[Vec[TracedInstruction]]]


  /** Helper function to insert additional buffers on master ports at the boundary of the tile.
    *
    * The boundary buffering needed to cut feed-through paths is
    * microarchitecture specific, so this may need to be overridden
    * in subclasses of this class.
    */
  def makeMasterBoundaryBuffers(crossing: ClockCrossingType)(implicit p: Parameters) = TLBuffer(BufferParams.none)

  /** Helper function to insert additional buffers on slave ports at the boundary of the tile.
    *
    * The boundary buffering needed to cut feed-through paths is
    * microarchitecture specific, so this may need to be overridden
    * in subclasses of this class.
    */
  def makeSlaveBoundaryBuffers(crossing: ClockCrossingType)(implicit p: Parameters) = TLBuffer(BufferParams.none)


}

abstract class BaseElementModuleImp[+L <: BaseElement](val outer: L) extends LazyModuleImp(outer)

