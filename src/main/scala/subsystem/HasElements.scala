// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import chisel3._
import chisel3.dontTouch
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.devices.debug.{TLDebugModule, HasPeripheryDebug}
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.prci.{ClockGroup, ResetCrossingType, ClockGroupNode}
import freechips.rocketchip.util._
import freechips.rocketchip.rocket.{TracedInstruction}

/** A default implementation of parameterizing the connectivity of the port where the tile is the master.
  *   Optional timing buffers and/or an optional CacheCork can be inserted in the interconnect's clock domain.
  */
case class ElementMasterPortParams(
  buffers: Int = 0,
  cork: Option[Boolean] = None,
  where: TLBusWrapperLocation = SBUS
) extends ElementPortParamsLike {
  def injectNode(context: Attachable)(implicit p: Parameters): TLNode = {
    (TLBuffer.chainNode(buffers) :=* cork.map { u => TLCacheCork(unsafe = u) } .getOrElse { TLTempNode() })
  }
}

/** A default implementation of parameterizing the connectivity of the port giving access to slaves inside the tile.
  *   Optional timing buffers and/or an optional BusBlocker adapter can be inserted in the interconnect's clock domain.
  */
case class ElementSlavePortParams(
  buffers: Int = 0,
  blockerCtrlAddr: Option[BigInt] = None,
  blockerCtrlWhere: TLBusWrapperLocation = CBUS,
  where: TLBusWrapperLocation = CBUS
) extends ElementPortParamsLike {
  def injectNode(context: Attachable)(implicit p: Parameters): TLNode = {
    val controlBus = context.locateTLBusWrapper(where)
    val blockerBus = context.locateTLBusWrapper(blockerCtrlWhere)
    blockerCtrlAddr
      .map { BasicBusBlockerParams(_, blockerBus.beatBytes, controlBus.beatBytes) }
      .map { bbbp =>
        val blocker = LazyModule(new BasicBusBlocker(bbbp))
        blockerBus.coupleTo("tile_slave_port_bus_blocker") { blocker.controlNode := TLFragmenter(blockerBus) := _ }
        blocker.node :*= TLBuffer.chainNode(buffers)
      } .getOrElse { TLBuffer.chainNode(buffers) }
  }
}

/** InstantiatesTiles adds a Config-urable sequence of Elements of any type
  *   to the subsystem class into which it is mixed.
  */
trait InstantiatesElements { this: LazyModule with Attachable =>
  val location: HierarchicalLocation

  /** Record the order in which to instantiate all tiles, based on statically-assigned ids.
    *
    * Note that these ids, which are often used as the tiles' default hartid input,
    * may or may not be those actually reflected at runtime in e.g. the $mhartid CSR
    */
  val tileAttachParams: Seq[CanAttachTile] = p(TilesLocated(location)).sortBy(_.tileParams.tileId)
  val tileParams: Seq[TileParams] = tileAttachParams.map(_.tileParams)
  val tileCrossingTypes: Seq[ClockCrossingType] = tileAttachParams.map(_.crossingParams.crossingType)

  /** The actual list of instantiated tiles in this block. */
  val tile_prci_domains: Seq[TilePRCIDomain[_]] = tileAttachParams.foldLeft(Seq[TilePRCIDomain[_]]()) {
    case (instantiated, params) => instantiated :+ params.instantiate(tileParams, instantiated)(p)
  }

  val clusterAttachParams: Seq[CanAttachCluster] = p(ClustersLocated(location)).sortBy(_.clusterParams.clusterId)
  val clusterParams: Seq[ClusterParams] = clusterAttachParams.map(_.clusterParams)
  val clusterCrossingTypes: Seq[ClockCrossingType] = clusterAttachParams.map(_.crossingParams.crossingType)
  val cluster_prci_domains: Seq[ClusterPRCIDomain] = clusterAttachParams.foldLeft(Seq[ClusterPRCIDomain]()) {
    case (instantiated, params) => instantiated :+ params.instantiate(clusterParams, instantiated)(p)
  }

  val element_prci_domains: Seq[ElementPRCIDomain[_]] = tile_prci_domains ++ cluster_prci_domains

  val leafTiles: Seq[BaseTile] = tile_prci_domains.map(_.element.asInstanceOf[BaseTile]).sortBy(_.tileId)
  val totalTiles: Seq[BaseTile] = (leafTiles ++ cluster_prci_domains.map(_.element.asInstanceOf[Cluster].totalTiles).flatten).sortBy(_.tileId)

  // Helper functions for accessing certain parameters that are popular to refer to in subsystem code
  def nLeafTiles: Int = leafTiles.size
  def nTotalTiles: Int = totalTiles.size
  def leafTileIdList: Seq[Int] = leafTiles.map(_.tileId)
  def totalTileIdList: Seq[Int] = totalTiles.map(_.tileId)
  def localIntCounts: Seq[Int] = totalTiles.map(_.tileParams.core.nLocalInterrupts)

  require(totalTileIdList.distinct.size == totalTiles.size, s"Every tile must be statically assigned a unique id, but got:\n${totalTileIdList}")
}

/** HasTiles instantiates and also connects a Config-urable sequence of tiles of any type to subsystem interconnect resources. */
trait HasElements extends DefaultElementContextType
{ this: LazyModule with Attachable with InstantiatesElements =>
  implicit val p: Parameters

  // connect all the tiles to interconnect attachment points made available in this subsystem context
  tileAttachParams.zip(tile_prci_domains).foreach { case (params, td) =>
    params.connect(td.asInstanceOf[TilePRCIDomain[params.TileType]], this.asInstanceOf[params.TileContextType])
  }

  clusterAttachParams.zip(cluster_prci_domains).foreach { case (params, cd) =>
    params.connect(cd.asInstanceOf[ClusterPRCIDomain], this.asInstanceOf[params.ClusterContextType])
  }
}

/** Provides some Chisel connectivity to certain tile IOs
  * This trait is intended for the root subsystem
  */
trait HasElementsRootContextModuleImp extends LazyModuleImp {
  val outer: InstantiatesElements with HasElements with HasElementsRootContext with HasTileInputConstants

  val reset_vector = outer.tileResetVectorIONodes.zipWithIndex.map { case (n, i) => n.makeIO(s"reset_vector_$i") }
  val tile_hartids = outer.tileHartIdIONodes.zipWithIndex.map { case (n, i) => n.makeIO(s"tile_hartids_$i") }

  val meip = if (outer.meipIONode.isDefined) Some(IO(Input(Vec(outer.meipIONode.get.out.size, Bool())))) else None
  meip.foreach { m =>
    m.zipWithIndex.foreach{ case (pin, i) =>
      (outer.meipIONode.get.out(i)._1)(0) := pin
    }
  }
  val seip = if (outer.seipIONode.isDefined) Some(IO(Input(Vec(outer.seipIONode.get.out.size, Bool())))) else None
  seip.foreach { s =>
    s.zipWithIndex.foreach{ case (pin, i) =>
      (outer.seipIONode.get.out(i)._1)(0) := pin
    }
  }
  val nmi = outer.nmiIONodes.map { case (i, node) =>
    node.makeIO(s"nmi_$i")
  }
}

/** Most tile types require only these traits in order for their standardized connect functions to apply.
  *
  *    BaseTiles subtypes with different needs can extend this trait to provide themselves with
  *    additional external connection points.
  */
trait DefaultElementContextType
    extends Attachable
    with HasTileNotificationSinks
{ this: LazyModule with Attachable =>
  val msipNodes: Map[Int, IntNode]
  val meipNodes: Map[Int, IntNode]
  val seipNodes: Map[Int, IntNode]
  val tileToPlicNodes: Map[Int, IntNode]
  val debugNodes: Map[Int, IntSyncNode]
  val nmiNodes: Map[Int, BundleBridgeNode[NMI]]
  val tileHartIdNodes: Map[Int, BundleBridgeNode[UInt]]
  val tileResetVectorNodes: Map[Int, BundleBridgeNode[UInt]]
  val traceCoreNodes: Map[Int, BundleBridgeNode[TraceCoreInterface]]
  val traceNodes: Map[Int, BundleBridgeNode[Vec[TracedInstruction]]]
}

/** This trait provides the tile attachment context for the root (outermost) subsystem */
trait HasElementsRootContext
{ this: HasElements
    with HasTileNotificationSinks
    with InstantiatesElements =>

  val clintOpt: Option[CLINT]
  val plicOpt: Option[TLPLIC]
  val debugOpt: Option[TLDebugModule]

  val msipNodes: Map[Int, IntNode] = (0 until nTotalTiles).map { i =>
    (i, IntEphemeralNode() := clintOpt.map(_.intnode).getOrElse(NullIntSource(sources = CLINTConsts.ints)))
  }.toMap

  val meipIONode = Option.when(plicOpt.isEmpty)(IntNexusNode(
    sourceFn = { _ => IntSourcePortParameters(Seq(IntSourceParameters(1))) },
    sinkFn   = { _ => IntSinkPortParameters(Seq(IntSinkParameters())) },
    outputRequiresInput = false,
    inputRequiresOutput = false))
  val meipNodes: Map[Int, IntNode] = (0 until nTotalTiles).map { i =>
    (i, IntEphemeralNode() := plicOpt.map(_.intnode).getOrElse(meipIONode.get))
  }.toMap

  val seipIONode = Option.when(plicOpt.isEmpty)(IntNexusNode(
    sourceFn = { _ => IntSourcePortParameters(Seq(IntSourceParameters(1))) },
    sinkFn   = { _ => IntSinkPortParameters(Seq(IntSinkParameters())) },
    outputRequiresInput = false,
    inputRequiresOutput = false))
  val seipNodes: Map[Int, IntNode] = (0 until nTotalTiles).map { i =>
    (i, IntEphemeralNode() := plicOpt.map(_.intnode).getOrElse(seipIONode.get))
  }.toMap

  val tileToPlicNodes: Map[Int, IntNode] = (0 until nTotalTiles).map { i =>
    plicOpt.map(o => (i, o.intnode :=* IntEphemeralNode()))
  }.flatten.toMap

  val debugNodes: Map[Int, IntSyncNode] = (0 until nTotalTiles).map { i =>
    (i, IntSyncIdentityNode())
  }.toMap

  debugNodes.foreach { case (hartid, node) =>
    node := debugOpt.map(_.intnode).getOrElse(IntSyncCrossingSource() := NullIntSource())
  }

  val nmiHarts = totalTiles.filter(_.tileParams.core.useNMI).map(_.tileId)
  val nmiIONodes = nmiHarts.map { i => (i, BundleBridgeSource[NMI]()) }.toMap
  val nmiNodes: Map[Int, BundleBridgeNode[NMI]] = nmiIONodes.map { case (i, n) =>
    (i, BundleBridgeEphemeralNode[NMI]() := n)
  }.toMap

  val traceCoreNodes: Map[Int, BundleBridgeSink[TraceCoreInterface]] = (0 until nTotalTiles).map { i => (i, BundleBridgeSink[TraceCoreInterface]()) }.toMap
  val traceNodes: Map[Int, BundleBridgeSink[Vec[TracedInstruction]]] = (0 until nTotalTiles).map { i => (i, BundleBridgeSink[Vec[TracedInstruction]]()) }.toMap
}
