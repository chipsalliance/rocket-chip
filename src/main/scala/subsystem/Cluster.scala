package freechips.rocketchip.subsystem

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy.bundlebridge._
import org.chipsalliance.diplomacy.lazymodule._

import freechips.rocketchip.devices.debug.{TLDebugModule}
import org.chipsalliance.diplomacy.FlipRendering
import freechips.rocketchip.interrupts.{IntIdentityNode, IntSyncIdentityNode, NullIntSource}
import freechips.rocketchip.prci.{ClockCrossingType, NoCrossing, ClockSinkParameters, ClockGroupIdentityNode, BundleBridgeBlockDuringReset}
import freechips.rocketchip.tile.{RocketTile, NMI, TraceBundle}
import freechips.rocketchip.tilelink.TLWidthWidget
import freechips.rocketchip.trace.TraceCoreInterface

import scala.collection.immutable.SortedMap

case class ClustersLocated(loc: HierarchicalLocation) extends Field[Seq[CanAttachCluster]](Nil)

trait BaseClusterParams extends HierarchicalElementParams {
  val clusterId: Int
}

abstract class InstantiableClusterParams[ClusterType <: Cluster]
    extends HierarchicalElementParams
    with BaseClusterParams {
  def instantiate(crossing: HierarchicalElementCrossingParamsLike, lookup: LookupByClusterIdImpl)(implicit p: Parameters): ClusterType
}

case class ClusterParams(
  val clusterId: Int,
  val clockSinkParams: ClockSinkParameters = ClockSinkParameters()
) extends InstantiableClusterParams[Cluster] {
  val baseName = "cluster"
  val uniqueName = s"${baseName}_$clusterId"
  def instantiate(crossing: HierarchicalElementCrossingParamsLike, lookup: LookupByClusterIdImpl)(implicit p: Parameters): Cluster = {
    new Cluster(this, crossing.crossingType, lookup)
  }
}

class Cluster(
  val thisClusterParams: BaseClusterParams,
  crossing: ClockCrossingType,
  lookup: LookupByClusterIdImpl)(implicit p: Parameters) extends BaseHierarchicalElement(crossing)(p)
    with Attachable
    with HasConfigurableTLNetworkTopology
    with InstantiatesHierarchicalElements
    with HasHierarchicalElements
{
  val busContextName = thisClusterParams.baseName
  lazy val clusterId = thisClusterParams.clusterId
  lazy val location = InCluster(clusterId)

  lazy val allClockGroupsNode = ClockGroupIdentityNode()

  val csbus = tlBusWrapperLocationMap(CSBUS(clusterId)) // like the sbus in the base subsystem
  val ccbus = tlBusWrapperLocationMap(CCBUS(clusterId)) // like the cbus in the base subsystem
  val cmbus = tlBusWrapperLocationMap.lift(CMBUS(clusterId)).getOrElse(csbus)

  csbus.clockGroupNode := allClockGroupsNode
  ccbus.clockGroupNode := allClockGroupsNode

  val slaveNode = ccbus.inwardNode
  val masterNode = cmbus.outwardNode

  lazy val ibus = LazyModule(new InterruptBusWrapper)
  ibus.clockNode := csbus.fixedClockNode

  def msipDomain = this
  def meipDomain = this
  def seipDomain = this
  def toPlicDomain = this
  def hasSupervisorTileIdList: Seq[Int] = totalTiles.filter(_._2.tileParams.core.hasSupervisorMode).keys.toSeq.sorted
  lazy val msipNodes = totalTileIdList.map { i => (i, IntIdentityNode()) }.to(SortedMap)
  lazy val meipNodes = totalTileIdList.map { i => (i, IntIdentityNode()) }.to(SortedMap)
  lazy val seipNodes = hasSupervisorTileIdList.map { i => (i, IntIdentityNode()) }.to(SortedMap)
  lazy val tileToPlicNodes = totalTileIdList.map { i => (i, IntIdentityNode()) }.to(SortedMap)
  lazy val debugNodes = totalTileIdList.map { i => (i, IntSyncIdentityNode()) }.to(SortedMap)
  lazy val nmiNodes = totalTiles.filter { case (i,t) => t.tileParams.core.useNMI }
    .view.mapValues(_ => BundleBridgeIdentityNode[NMI]()).to(SortedMap)
  lazy val tileHartIdNodes = totalTileIdList.map { i => (i, BundleBridgeIdentityNode[UInt]()) }.to(SortedMap)
  lazy val tileResetVectorNodes = totalTileIdList.map { i => (i, BundleBridgeIdentityNode[UInt]()) }.to(SortedMap)
  lazy val traceCoreNodes = totalTileIdList.map { i => (i, BundleBridgeIdentityNode[TraceCoreInterface]()) }.to(SortedMap)
  lazy val traceNodes = totalTileIdList.map { i => (i, BundleBridgeIdentityNode[TraceBundle]()) }.to(SortedMap)

  // TODO fix: shouldn't need to connect dummy notifications
  tileHaltXbarNode := NullIntSource()
  tileWFIXbarNode := NullIntSource()
  // tileCeaseXbarNode := NullIntSource()

  override lazy val module = new ClusterModuleImp(this)
}

class ClusterModuleImp(outer: Cluster) extends BaseHierarchicalElementModuleImp[Cluster](outer)

case class InCluster(id: Int) extends HierarchicalLocation(s"Cluster$id")

abstract class ClusterPRCIDomain[ClusterType <: Cluster](
  clockSinkParams: ClockSinkParameters,
  crossingParams: HierarchicalElementCrossingParamsLike,
  clusterParams: InstantiableClusterParams[ClusterType],
  lookup: LookupByClusterIdImpl)
  (implicit p: Parameters) extends HierarchicalElementPRCIDomain[ClusterType](clockSinkParams, crossingParams)
{
  val element = element_reset_domain {
    LazyModule(clusterParams.instantiate(crossingParams, lookup))
  }
  // Nothing should depend on the clocks coming from clockNode anyways
  clockNode := element.csbus.fixedClockNode
}


trait CanAttachCluster {
  type ClusterType <: Cluster
  type ClusterContextType <: DefaultHierarchicalElementContextType
  def clusterParams: InstantiableClusterParams[ClusterType]
  def crossingParams: HierarchicalElementCrossingParamsLike

  def instantiate(allClusterParams: Seq[BaseClusterParams], instantiatedClusters: SortedMap[Int, ClusterPRCIDomain[_]])(implicit p: Parameters): ClusterPRCIDomain[ClusterType] = {
    val clockSinkParams = clusterParams.clockSinkParams.copy(name = Some(clusterParams.uniqueName))
    val cluster_prci_domain = LazyModule(new ClusterPRCIDomain[ClusterType](
      clockSinkParams, crossingParams, clusterParams, PriorityMuxClusterIdFromSeq(allClusterParams)) {})
    cluster_prci_domain
  }

  def connect(domain: ClusterPRCIDomain[ClusterType], context: ClusterContextType): Unit = {
    connectMasterPorts(domain, context)
    connectSlavePorts(domain, context)
    connectInterrupts(domain, context)
    connectPRC(domain, context)
    connectOutputNotifications(domain, context)
    connectInputConstants(domain, context)
    connectTrace(domain, context)
  }

  def connectMasterPorts(domain: ClusterPRCIDomain[ClusterType], context: Attachable): Unit = {
    implicit val p = context.p
    val dataBus = context.locateTLBusWrapper(crossingParams.master.where)
    dataBus.coupleFrom(clusterParams.baseName) { bus =>
      bus :=* crossingParams.master.injectNode(context) :=* domain.crossMasterPort(crossingParams.crossingType)
    }
  }
  def connectSlavePorts(domain: ClusterPRCIDomain[ClusterType], context: Attachable): Unit = {
    implicit val p = context.p
    val controlBus = context.locateTLBusWrapper(crossingParams.slave.where)
    controlBus.coupleTo(clusterParams.baseName) { bus =>
      domain.crossSlavePort(crossingParams.crossingType) :*= crossingParams.slave.injectNode(context) :*= TLWidthWidget(controlBus.beatBytes) :*= bus
    }
  }
  def connectInterrupts(domain: ClusterPRCIDomain[ClusterType], context: ClusterContextType): Unit = {
    implicit val p = context.p

    domain.element.debugNodes.foreach { case (hartid, node) =>
      node := context.debugNodes(hartid)
    }

    domain.element.msipNodes.foreach { case (hartid, node) => context.msipDomain {
      domain.crossIntIn(crossingParams.crossingType, node) := context.msipNodes(hartid)
    }}

    domain.element.meipNodes.foreach { case (hartid, node) => context.meipDomain {
      domain.crossIntIn(crossingParams.crossingType, node) := context.meipNodes(hartid)
    }}

    domain.element.seipNodes.foreach { case (hartid, node) => context.seipDomain {
      domain.crossIntIn(crossingParams.crossingType, node) := context.seipNodes(hartid)
    }}

    domain.element.tileToPlicNodes.foreach { case (hartid, node) =>
      FlipRendering { implicit p =>
        context.tileToPlicNodes(hartid) :=* domain.crossIntOut(crossingParams.crossingType, node) }
    }
    context.ibus.fromSync :=* domain.crossIntOut(crossingParams.crossingType, domain.element.ibus.toPLIC)

    domain.element.nmiNodes.foreach { case (hartid, node) =>
      node := context.nmiNodes(hartid)
    }
  }

  def connectPRC(domain: ClusterPRCIDomain[ClusterType], context: ClusterContextType): Unit = {
    implicit val p = context.p
    domain.element.allClockGroupsNode :*= context.allClockGroupsNode
    domain {
      domain.element_reset_domain.clockNode := crossingParams.resetCrossingType.injectClockNode := domain.clockNode
    }
  }

  def connectOutputNotifications(domain: ClusterPRCIDomain[ClusterType], context: ClusterContextType): Unit = {
    implicit val p = context.p
    context.tileHaltXbarNode  :=* domain.crossIntOut(NoCrossing, domain.element.tileHaltXbarNode)
    context.tileWFIXbarNode   :=* domain.crossIntOut(NoCrossing, domain.element.tileWFIXbarNode)
    context.tileCeaseXbarNode :=* domain.crossIntOut(NoCrossing, domain.element.tileCeaseXbarNode)

  }

  def connectInputConstants(domain: ClusterPRCIDomain[ClusterType], context: ClusterContextType): Unit = {
    implicit val p = context.p
    val tlBusToGetPrefixFrom = context.locateTLBusWrapper(crossingParams.mmioBaseAddressPrefixWhere)
    domain.element.tileHartIdNodes.foreach { case (hartid, node) =>
      node := context.tileHartIdNodes(hartid)
    }
    domain.element.tileResetVectorNodes.foreach { case (hartid, node) =>
      node := context.tileResetVectorNodes(hartid)
    }
  }

  def connectTrace(domain: ClusterPRCIDomain[ClusterType], context: ClusterContextType): Unit = {
    implicit val p = context.p
    domain.element.traceNodes.foreach { case (hartid, node) =>
      val traceNexusNode = BundleBridgeBlockDuringReset[TraceBundle](
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

case class ClusterAttachParams (
  clusterParams: ClusterParams,
  crossingParams: HierarchicalElementCrossingParamsLike
) extends CanAttachCluster {
  type ClusterType = Cluster
}

case class CloneClusterAttachParams(
  sourceClusterId: Int,
  cloneParams: CanAttachCluster
) extends CanAttachCluster {
  type ClusterType = cloneParams.ClusterType
  def clusterParams = cloneParams.clusterParams
  def crossingParams = cloneParams.crossingParams

  override def instantiate(allClusterParams: Seq[BaseClusterParams], instantiatedClusters: SortedMap[Int, ClusterPRCIDomain[_]])(implicit p: Parameters): ClusterPRCIDomain[ClusterType] = {
    require(instantiatedClusters.contains(sourceClusterId))
    val clockSinkParams = clusterParams.clockSinkParams.copy(name = Some(clusterParams.uniqueName))
    val cluster_prci_domain = CloneLazyModule(
      new ClusterPRCIDomain[ClusterType](clockSinkParams, crossingParams, clusterParams, PriorityMuxClusterIdFromSeq(allClusterParams)) {},
      instantiatedClusters(sourceClusterId)
    )
    cluster_prci_domain
  }
}
