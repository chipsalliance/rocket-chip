package freechips.rocketchip.subsystem

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.rocket.{TracedInstruction}
import freechips.rocketchip.prci._
import freechips.rocketchip.tile.{RocketTile, NMI}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.debug.{TLDebugModule}
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.util._

case class ClustersLocated(loc: HierarchicalLocation) extends Field[Seq[CanAttachCluster]](Nil)

case class ClusterParams(
  val clusterId: Int,
  val clockSinkParams: ClockSinkParameters = ClockSinkParameters()
) extends ElementParams {
  val name = s"cluster_$clusterId"
  def instantiate(crossing: ElementCrossingParamsLike, lookup: LookupByClusterIdImpl)(implicit p: Parameters): Cluster = {
    new Cluster(this, crossing.crossingType, lookup)
  }
}

class Cluster(
  val thisClusterParams: ClusterParams,
  crossing: ClockCrossingType,
  lookup: LookupByClusterIdImpl)(implicit p: Parameters) extends BaseElement(crossing)(p)
    with Attachable
    with HasConfigurableTLNetworkTopology
    with InstantiatesElements
    with HasElements
{
  lazy val clusterId = thisClusterParams.clusterId
  lazy val location = InCluster(clusterId)

  val clockGroupNode = ClockGroupAggregator()
  println(p(TLNetworkTopologyLocated(InCluster(0))))
  println(p(TLNetworkTopologyLocated(InCluster(1))))
  println(thisClusterParams)
  println(clusterId)
  println(location)
  println(tlBusWrapperLocationMap)
  val csbus = tlBusWrapperLocationMap(CSBUS(clusterId)) // like the sbus in the base subsystem
  val ccbus = tlBusWrapperLocationMap(CCBUS(clusterId)) // like the cbus in the base subsystem

  csbus.clockGroupNode := clockGroupNode
  ccbus.clockGroupNode := clockGroupNode

  val slaveNode = ccbus.inwardNode
  val masterNode = csbus.outwardNode



  val ibus = LazyModule(new InterruptBusWrapper)
  ibus.clockNode := csbus.fixedClockNode
  implicit val asyncClockGroupsNode = p(AsyncClockGroupsKey)()

  lazy val msipNodes = totalHartIdList.map { i => (i, IntIdentityNode()) }.toMap
  lazy val meipNodes = totalHartIdList.map { i => (i, IntIdentityNode()) }.toMap
  lazy val seipNodes = totalHartIdList.map { i => (i, IntIdentityNode()) }.toMap
  lazy val tileToPlicNodes = totalHartIdList.map { i => (i, IntIdentityNode()) }.toMap
  lazy val debugNodes = totalHartIdList.map { i => (i, IntSyncIdentityNode()) }.toMap
  lazy val nmiNodes = totalTiles.filter(_.tileParams.core.useNMI).map { t => (t.hartId, BundleBridgeIdentityNode[NMI]()) }.toMap
  lazy val tileHartIdNodes = totalHartIdList.map { i => (i, BundleBridgeIdentityNode[UInt]()) }.toMap
  lazy val tileResetVectorNodes = totalHartIdList.map { i => (i, BundleBridgeIdentityNode[UInt]()) }.toMap
  lazy val traceCoreNodes = totalHartIdList.map { i => (i, BundleBridgeIdentityNode[TraceCoreInterface]()) }.toMap
  lazy val traceNodes = totalHartIdList.map { i => (i, BundleBridgeIdentityNode[Vec[TracedInstruction]]()) }.toMap

  // TODO fix: shouldn't need to connect dummy notifications
  tileHaltXbarNode := NullIntSource()
  tileWFIXbarNode := NullIntSource()
  tileCeaseXbarNode := NullIntSource()

  override lazy val module = new ClusterModuleImp(this)
}

class ClusterModuleImp(outer: Cluster) extends BaseElementModuleImp[Cluster](outer)

case class InCluster(id: Int) extends HierarchicalLocation(s"Cluster$id")

class ClusterPRCIDomain(
  clockSinkParams: ClockSinkParameters,
  crossingParams: ElementCrossingParamsLike,
  clusterParams: ClusterParams,
  lookup: LookupByClusterIdImpl)
  (implicit p: Parameters) extends ElementPRCIDomain[Cluster](clockSinkParams, crossingParams)
{
  val clockGroupNode = ClockGroupAggregator()
  val element = element_reset_domain {
    LazyModule(clusterParams.instantiate(crossingParams, lookup))
  }

  clockNode := ClockGroup() := clockGroupNode
  element.clockGroupNode := clockGroupNode
}


trait CanAttachCluster {
  type ClusterContextType <: DefaultElementContextType

  def clusterParams: ClusterParams
  def crossingParams: ElementCrossingParamsLike

  def instantiate(allClusterParams: Seq[ClusterParams], instantiatedClusters: Seq[ClusterPRCIDomain])(implicit p: Parameters): ClusterPRCIDomain = {
    val clockSinkParams = clusterParams.clockSinkParams.copy(name = Some(clusterParams.name))
    val cluster_prci_domain = LazyModule(new ClusterPRCIDomain(
      clockSinkParams, crossingParams, clusterParams, PriorityMuxClusterIdFromSeq(allClusterParams)))
    cluster_prci_domain
  }

  def connect(domain: ClusterPRCIDomain, context: ClusterContextType): Unit = {
    connectMasterPorts(domain, context)
    connectSlavePorts(domain, context)
    connectInterrupts(domain, context)
    connectPRC(domain, context)
    connectOutputNotifications(domain, context)
    connectInputConstants(domain, context)
    connectTrace(domain, context)
  }

  def connectMasterPorts(domain: ClusterPRCIDomain, context: Attachable): Unit = {
    implicit val p = context.p
    val dataBus = context.locateTLBusWrapper(crossingParams.master.where)
    dataBus.coupleFrom(clusterParams.name) { bus =>
      bus :=* crossingParams.master.injectNode(context) :=* domain.crossMasterPort(crossingParams.crossingType)
    }
  }
  def connectSlavePorts(domain: ClusterPRCIDomain, context: Attachable): Unit = {
    implicit val p = context.p
    val controlBus = context.locateTLBusWrapper(crossingParams.slave.where)
    controlBus.coupleTo(clusterParams.name) { bus =>
      domain.crossSlavePort(crossingParams.crossingType) :*= crossingParams.slave.injectNode(context) :*= TLWidthWidget(controlBus.beatBytes) :*= bus
    }
  }
  def connectInterrupts(domain: ClusterPRCIDomain, context: ClusterContextType): Unit = {
    implicit val p = context.p

    domain.element.debugNodes.foreach { case (hartid, node) =>
      node := context.debugNodes(hartid)
    }

    domain.element.msipNodes.foreach { case (hartid, node) =>
      domain.crossIntIn(crossingParams.crossingType, node) := context.msipNodes(hartid)
    }

    domain.element.meipNodes.foreach { case (hartid, node) =>
      domain.crossIntIn(crossingParams.crossingType, node) := context.meipNodes(hartid)
    }

    domain.element.seipNodes.foreach { case (hartid, node) =>
      domain.crossIntIn(crossingParams.crossingType, node) := context.seipNodes(hartid)
    }

    domain.element.tileToPlicNodes.foreach { case (hartid, node) =>
      FlipRendering { implicit p =>
        context.tileToPlicNodes(hartid) :=* domain.crossIntOut(crossingParams.crossingType, node) }
    }
    context.ibus.fromSync :=* domain.crossIntOut(crossingParams.crossingType, domain.element.ibus.toPLIC)

    domain.element.nmiNodes.foreach { case (hartid, node) =>
      node := context.nmiNodes(hartid)
    }
  }

  def connectPRC(domain: ClusterPRCIDomain, context: ClusterContextType): Unit = {
    implicit val p = context.p
    domain.clockGroupNode := context.asyncClockGroupsNode
    domain {
      domain.element_reset_domain.clockNode := crossingParams.resetCrossingType.injectClockNode := domain.clockNode
    }
  }

  def connectOutputNotifications(domain: ClusterPRCIDomain, context: ClusterContextType): Unit = {
    implicit val p = context.p
    context.tileHaltXbarNode  :=* domain.crossIntOut(NoCrossing, domain.element.tileHaltXbarNode)
    context.tileWFIXbarNode   :=* domain.crossIntOut(NoCrossing, domain.element.tileWFIXbarNode)
    context.tileCeaseXbarNode :=* domain.crossIntOut(NoCrossing, domain.element.tileCeaseXbarNode)

  }

  def connectInputConstants(domain: ClusterPRCIDomain, context: ClusterContextType): Unit = {
    implicit val p = context.p
    val tlBusToGetPrefixFrom = context.locateTLBusWrapper(crossingParams.mmioBaseAddressPrefixWhere)
    domain.element.tileHartIdNodes.foreach { case (hartid, node) =>
      node := context.tileHartIdNodes(hartid)
    }
    domain.element.tileResetVectorNodes.foreach { case (hartid, node) =>
      node := context.tileResetVectorNodes(hartid)
    }
  }

  def connectTrace(domain: ClusterPRCIDomain, context: ClusterContextType): Unit = {
    implicit val p = context.p
    domain.element.traceNodes.foreach { case (hartid, node) =>
      val traceNexusNode = BundleBridgeBlockDuringReset[Vec[TracedInstruction]](
        resetCrossingType = crossingParams.resetCrossingType)
      context.traceNodes(hartid) := traceNexusNode := node
    }
    domain.element.traceCoreNodes.foreach { case (hartid, node) =>
      val traceCoreNexusNode = BundleBridgeBlockDuringReset[TraceCoreInterface](
        resetCrossingType = crossingParams.resetCrossingType)
      context.traceCoreNodes(hartid) :*= traceCoreNexusNode := node
    }
  }
}

case class ClusterAttachParams(
  clusterParams: ClusterParams,
  crossingParams: ElementCrossingParamsLike
) extends CanAttachCluster
