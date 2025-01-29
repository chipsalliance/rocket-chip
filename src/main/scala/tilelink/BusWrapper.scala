// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy._
import org.chipsalliance.diplomacy.bundlebridge._
import org.chipsalliance.diplomacy.lazymodule._
import org.chipsalliance.diplomacy.nodes._

import freechips.rocketchip.diplomacy.{AddressSet, NoHandle, NodeHandle, NodeBinding}

// TODO This class should be moved to package subsystem to resolve
//      the dependency awkwardness of the following imports
import freechips.rocketchip.devices.tilelink.{BuiltInDevices, CanHaveBuiltInDevices}
import freechips.rocketchip.prci.{
  ClockParameters, ClockDomain, ClockGroup, ClockGroupAggregator, ClockSinkNode,
  FixedClockBroadcast, ClockGroupEdgeParameters, ClockSinkParameters, ClockSinkDomain,
  ClockGroupEphemeralNode, asyncMux, ClockCrossingType, NoCrossing
}
import freechips.rocketchip.subsystem.{
  HasTileLinkLocations, CanConnectWithinContextThatHasTileLinkLocations,
  CanInstantiateWithinContextThatHasTileLinkLocations
}
import freechips.rocketchip.util.Location

/** Specifies widths of various attachement points in the SoC */
trait HasTLBusParams {
  def beatBytes: Int
  def blockBytes: Int

  def beatBits: Int = beatBytes * 8
  def blockBits: Int = blockBytes * 8
  def blockBeats: Int = blockBytes / beatBytes
  def blockOffset: Int = log2Up(blockBytes)

  def dtsFrequency: Option[BigInt]
  def fixedClockOpt = dtsFrequency.map(f => ClockParameters(freqMHz = f.toDouble / 1000000.0))

  require (isPow2(beatBytes))
  require (isPow2(blockBytes))
}

abstract class TLBusWrapper(params: HasTLBusParams, val busName: String)(implicit p: Parameters)
    extends ClockDomain
    with HasTLBusParams
    with CanHaveBuiltInDevices
{
  private val clockGroupAggregator = LazyModule(new ClockGroupAggregator(busName){ override def shouldBeInlined = true }).suggestName(busName + "_clock_groups")
  private val clockGroup = LazyModule(new ClockGroup(busName){ override def shouldBeInlined = true })
  val clockGroupNode = clockGroupAggregator.node // other bus clock groups attach here
  val clockNode = clockGroup.node
  val fixedClockNode = FixedClockBroadcast(fixedClockOpt) // device clocks attach here
  private val clockSinkNode = ClockSinkNode(List(ClockSinkParameters(take = fixedClockOpt)))

  clockGroup.node := clockGroupAggregator.node
  fixedClockNode := clockGroup.node // first member of group is always domain's own clock
  clockSinkNode := fixedClockNode

  InModuleBody {
    // make sure the above connections work properly because mismatched-by-name signals will just be ignored.
    (clockGroup.node.edges.in zip clockGroupAggregator.node.edges.out).zipWithIndex map { case ((in: ClockGroupEdgeParameters , out: ClockGroupEdgeParameters), i) =>
      require(in.members.keys == out.members.keys, s"clockGroup := clockGroupAggregator not working as you expect for index ${i}, becuase clockGroup has ${in.members.keys} and clockGroupAggregator has ${out.members.keys}")
    }
  }

  def clockBundle = clockSinkNode.in.head._1
  def beatBytes = params.beatBytes
  def blockBytes = params.blockBytes
  def dtsFrequency = params.dtsFrequency
  val dtsClk = fixedClockNode.fixedClockResources(s"${busName}_clock").flatten.headOption

  /* If you violate this requirement, you will have a rough time.
   * The codebase is riddled with the assumption that this is true.
   */
  require(blockBytes >= beatBytes)

  def inwardNode: TLInwardNode
  def outwardNode: TLOutwardNode
  def busView: TLEdge
  def prefixNode: Option[BundleBridgeNode[UInt]]
  def unifyManagers: List[TLManagerParameters] = ManagerUnification(busView.manager.managers)
  def crossOutHelper = this.crossOut(outwardNode)(ValName("bus_xing"))
  def crossInHelper = this.crossIn(inwardNode)(ValName("bus_xing"))
  def generateSynchronousDomain(domainName: String): ClockSinkDomain = {
    val domain = LazyModule(new ClockSinkDomain(take = fixedClockOpt, name = Some(domainName)))
    domain.clockNode := fixedClockNode
    domain
  }
  def generateSynchronousDomain: ClockSinkDomain = generateSynchronousDomain("")

  protected val addressPrefixNexusNode = BundleBroadcast[UInt](registered = false, default = Some(() => 0.U(1.W)))

  def to[T](name: String)(body: => T): T = {
    this { LazyScope(s"coupler_to_${name}", s"TLInterconnectCoupler_${busName}_to_${name}") { body } }
  }

  def from[T](name: String)(body: => T): T = {
    this { LazyScope(s"coupler_from_${name}", s"TLInterconnectCoupler_${busName}_from_${name}") { body } }
  }

  def coupleTo[T](name: String)(gen: TLOutwardNode => T): T =
    to(name) { gen(TLNameNode("tl") :*=* outwardNode) }

  def coupleFrom[T](name: String)(gen: TLInwardNode => T): T =
    from(name) { gen(inwardNode :*=* TLNameNode("tl")) }

  def crossToBus(bus: TLBusWrapper, xType: ClockCrossingType, allClockGroupNode: ClockGroupEphemeralNode): NoHandle = {
    bus.clockGroupNode := asyncMux(xType, allClockGroupNode, this.clockGroupNode)
    coupleTo(s"bus_named_${bus.busName}") {
      bus.crossInHelper(xType) :*= TLWidthWidget(beatBytes) :*= _
    }
  }

  def crossFromBus(bus: TLBusWrapper, xType: ClockCrossingType, allClockGroupNode: ClockGroupEphemeralNode): NoHandle = {
    bus.clockGroupNode := asyncMux(xType, allClockGroupNode, this.clockGroupNode)
    coupleFrom(s"bus_named_${bus.busName}") {
      _ :=* TLWidthWidget(bus.beatBytes) :=* bus.crossOutHelper(xType)
    }
  }
}

trait TLBusWrapperInstantiationLike {
  def instantiate(context: HasTileLinkLocations, loc: Location[TLBusWrapper])(implicit p: Parameters): TLBusWrapper
}

trait TLBusWrapperConnectionLike {
  val xType: ClockCrossingType
  def connect(context: HasTileLinkLocations, client: Location[TLBusWrapper], manager: Location[TLBusWrapper])(implicit p: Parameters): Unit
}

object TLBusWrapperConnection {
  /** Backwards compatibility factory for client driving clock and manager setting cardinality */
  def crossTo(
      xType: ClockCrossingType,
      driveClockFromClient: Option[Boolean] = Some(true),
      nodeBinding: NodeBinding = BIND_STAR,
      flipRendering: Boolean = false) = {
    apply(xType, driveClockFromClient, nodeBinding, flipRendering)(
          managerNodeView  = { case(w, p) => w.crossInHelper(xType)(p) })
  }

  /** Backwards compatibility factory for manager driving clock and client setting cardinality */
  def crossFrom(
      xType: ClockCrossingType,
      driveClockFromClient: Option[Boolean] = Some(false),
      nodeBinding: NodeBinding = BIND_QUERY,
      flipRendering: Boolean = true) = {
    apply(xType, driveClockFromClient, nodeBinding, flipRendering)(
          clientNodeView  = { case(w, p) => w.crossOutHelper(xType)(p) })
  }

  /** Factory for making generic connections between TLBusWrappers */
  def apply
    (xType: ClockCrossingType = NoCrossing,
     driveClockFromClient: Option[Boolean] = None,
     nodeBinding: NodeBinding = BIND_ONCE,
     flipRendering: Boolean = false)(
     managerNodeView: (TLBusWrapper, Parameters) => TLInwardNode = { case(w, _) => w.inwardNode },
     clientNodeView: (TLBusWrapper, Parameters) => TLOutwardNode = { case(w, _) => w.outwardNode },
     inject: Parameters => TLNode = { _ => TLTempNode() }) = {
    new TLBusWrapperConnection(
      xType, driveClockFromClient, nodeBinding, flipRendering)(
      managerNodeView, clientNodeView, inject)
  }
}

/** TLBusWrapperConnection is a parameterization of a connection between two TLBusWrappers.
  * It has the following serializable parameters:
  *   - xType: What type of TL clock crossing adapter to insert between the buses.
  *       The appropriate half of the crossing adapter ends up inside each bus.
  *   - driveClockFromClient: if None, don't bind the bus's diplomatic clockGroupNode,
  *       otherwise have either the client or the manager bus bind the other one's clockGroupNode,
  *       assuming the inserted crossing type is not asynchronous.
  *   - nodeBinding: fine-grained control of multi-edge cardinality resolution for diplomatic bindings within the connection.
  *   - flipRendering: fine-grained control of the graphML rendering of the connection.
  * If has the following non-serializable parameters:
  *   - managerNodeView: programmatic control of the specific attachment point within the manager bus.
  *   - clientNodeView: programmatic control of the specific attachment point within the client bus.
  *   - injectNode: programmatic injection of additional nodes into the middle of the connection.
  * The connect method applies all these parameters to create a diplomatic connection between two Location[TLBusWrapper]s.
  */
class TLBusWrapperConnection
    (val xType: ClockCrossingType,
     val driveClockFromClient: Option[Boolean],
     val nodeBinding: NodeBinding,
     val flipRendering: Boolean)
    (managerNodeView: (TLBusWrapper, Parameters) => TLInwardNode,
     clientNodeView: (TLBusWrapper, Parameters) => TLOutwardNode,
     inject: Parameters => TLNode)
  extends TLBusWrapperConnectionLike
{
  def connect(context: HasTileLinkLocations, client: Location[TLBusWrapper], manager: Location[TLBusWrapper])(implicit p: Parameters): Unit = {
    val clientTLBus = context.locateTLBusWrapper(client)
    val managerTLBus  = context.locateTLBusWrapper(manager)
    def bindClocks(implicit p: Parameters) = driveClockFromClient match {
      case Some(true)  => managerTLBus.clockGroupNode  := asyncMux(xType, context.allClockGroupsNode, clientTLBus.clockGroupNode)
      case Some(false) => clientTLBus.clockGroupNode := asyncMux(xType, context.allClockGroupsNode, managerTLBus.clockGroupNode)
      case None =>
    }
    def bindTLNodes(implicit p: Parameters) = nodeBinding match {
      case BIND_ONCE  => managerNodeView(managerTLBus, p) :=   TLWidthWidget(clientTLBus.beatBytes) :=   inject(p) :=   clientNodeView(clientTLBus, p)
      case BIND_QUERY => managerNodeView(managerTLBus, p) :=*  TLWidthWidget(clientTLBus.beatBytes) :=*  inject(p) :=*  clientNodeView(clientTLBus, p)
      case BIND_STAR  => managerNodeView(managerTLBus, p) :*=  TLWidthWidget(clientTLBus.beatBytes) :*=  inject(p) :*=  clientNodeView(clientTLBus, p)
      case BIND_FLEX  => managerNodeView(managerTLBus, p) :*=* TLWidthWidget(clientTLBus.beatBytes) :*=* inject(p) :*=* clientNodeView(clientTLBus, p)
    }

    if (flipRendering) { FlipRendering { implicit p =>
      bindClocks(implicitly[Parameters])
      managerTLBus.from(s"bus_named_${clientTLBus.busName}") {
        bindTLNodes(implicitly[Parameters])
      }
    } } else {
      bindClocks(implicitly[Parameters])
      clientTLBus.to (s"bus_named_${managerTLBus.busName}")  {
        bindTLNodes(implicitly[Parameters])
      }
    }
  }
}

class TLBusWrapperTopology(
  val instantiations: Seq[(Location[TLBusWrapper], TLBusWrapperInstantiationLike)],
  val connections: Seq[(Location[TLBusWrapper], Location[TLBusWrapper], TLBusWrapperConnectionLike)]
) extends CanInstantiateWithinContextThatHasTileLinkLocations
  with CanConnectWithinContextThatHasTileLinkLocations
{
  def instantiate(context: HasTileLinkLocations)(implicit p: Parameters): Unit = {
    instantiations.foreach { case (loc, params) => context { params.instantiate(context, loc) } }
  }
  def connect(context: HasTileLinkLocations)(implicit p: Parameters): Unit = {
    connections.foreach { case (client, manager, params) => context { params.connect(context, client, manager) } }
  }
}

trait HasTLXbarPhy { this: TLBusWrapper =>
  private val xbar = LazyModule(new TLXbar(nameSuffix = Some(busName))).suggestName(busName + "_xbar")

  override def shouldBeInlined = xbar.node.circuitIdentity
  def inwardNode: TLInwardNode = xbar.node
  def outwardNode: TLOutwardNode = xbar.node
  def busView: TLEdge = xbar.node.edges.in.head
}

case class AddressAdjusterWrapperParams(
  blockBytes: Int,
  beatBytes: Int,
  replication: Option[ReplicatedRegion],
  forceLocal: Seq[AddressSet] = Nil,
  localBaseAddressDefault: Option[BigInt] = None,
  policy: TLFIFOFixer.Policy = TLFIFOFixer.allVolatile,
  ordered: Boolean = true
)
  extends HasTLBusParams
  with TLBusWrapperInstantiationLike
{
  val dtsFrequency = None
  def instantiate(context: HasTileLinkLocations, loc: Location[TLBusWrapper])(implicit p: Parameters): AddressAdjusterWrapper = {
    val aaWrapper = LazyModule(new AddressAdjusterWrapper(this, context.busContextName + "_" + loc.name))
    aaWrapper.suggestName(context.busContextName + "_" + loc.name + "_wrapper")
    context.tlBusWrapperLocationMap += (loc -> aaWrapper)
    aaWrapper
  }
}

class AddressAdjusterWrapper(params: AddressAdjusterWrapperParams, name: String)(implicit p: Parameters) extends TLBusWrapper(params, name) {
  private val address_adjuster = params.replication.map { r => LazyModule(new AddressAdjuster(r, params.forceLocal, params.localBaseAddressDefault, params.ordered)) }
  private val viewNode = TLIdentityNode()
  val inwardNode: TLInwardNode = address_adjuster.map(_.node :*=* TLFIFOFixer(params.policy) :*=* viewNode).getOrElse(viewNode)
  def outwardNode: TLOutwardNode = address_adjuster.map(_.node).getOrElse(viewNode)
  def busView: TLEdge = viewNode.edges.in.head
  val prefixNode = address_adjuster.map { a =>
    a.prefix := addressPrefixNexusNode
    addressPrefixNexusNode
  }
  val builtInDevices = BuiltInDevices.none
  override def shouldBeInlined = !params.replication.isDefined
}

case class TLJBarWrapperParams(
  blockBytes: Int,
  beatBytes: Int
)
  extends HasTLBusParams
  with TLBusWrapperInstantiationLike
{
  val dtsFrequency = None
  def instantiate(context: HasTileLinkLocations, loc: Location[TLBusWrapper])(implicit p: Parameters): TLJBarWrapper = {
    val jbarWrapper = LazyModule(new TLJBarWrapper(this, context.busContextName + "_" + loc.name))
    jbarWrapper.suggestName(context.busContextName + "_" + loc.name + "_wrapper")
    context.tlBusWrapperLocationMap += (loc -> jbarWrapper)
    jbarWrapper
  }
}

class TLJBarWrapper(params: TLJBarWrapperParams, name: String)(implicit p: Parameters) extends TLBusWrapper(params, name) {
  private val jbar = LazyModule(new TLJbar)
  val inwardNode: TLInwardNode = jbar.node
  val outwardNode: TLOutwardNode = jbar.node
  def busView: TLEdge = jbar.node.edges.in.head
  val prefixNode = None
  val builtInDevices = BuiltInDevices.none
  override def shouldBeInlined = jbar.node.circuitIdentity
}
