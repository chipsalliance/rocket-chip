// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import chisel3._
import chisel3.dontTouch
import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.devices.debug.{TLDebugModule, HasPeripheryDebug}
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.prci.{ClockGroup, ResetCrossingType, ClockGroupNode}
import freechips.rocketchip.util._


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
  val tileAttachParams: Seq[CanAttachTile] = p(TilesLocated(location)).sortBy(_.tileParams.hartId)
  val tileParams: Seq[TileParams] = tileAttachParams.map(_.tileParams)
  val tileCrossingTypes: Seq[ClockCrossingType] = tileAttachParams.map(_.crossingParams.crossingType)

  /** The actual list of instantiated tiles in this block. */
  val tile_prci_domains: Seq[TilePRCIDomain[_]] = tileAttachParams.foldLeft(Seq[TilePRCIDomain[_]]()) {
    case (instantiated, params) => instantiated :+ params.instantiate(tileParams, instantiated)(p)
  }

  val leafTiles: Seq[BaseTile] = tile_prci_domains.map(_.element.asInstanceOf[BaseTile])
  val totalTiles: Seq[BaseTile] = tile_prci_domains.map(_.element.asInstanceOf[BaseTile])

  val element_prci_domains: Seq[ElementPRCIDomain[_]] = tile_prci_domains

  // Helper functions for accessing certain parameters that are popular to refer to in subsystem code
  def nLeafTiles: Int = tileAttachParams.size
  def nTotalTiles: Int = tileAttachParams.size
  def leafHartIdList: Seq[Int] = tileParams.map(_.hartId)
  def totalHartIdList: Seq[Int] = tileParams.map(_.hartId)
  def localIntCounts: Seq[Int] = tileParams.map(_.core.nLocalInterrupts)

  require(totalHartIdList.distinct.size == totalTiles.size, s"Every tile must be statically assigned a unique id, but got:\n${totalHartIdList}")
}

/** HasTiles instantiates and also connects a Config-urable sequence of tiles of any type to subsystem interconnect resources. */
trait HasElements extends DefaultElementContextType
{ this: LazyModule with Attachable with InstantiatesElements =>
  implicit val p: Parameters

  // connect all the tiles to interconnect attachment points made available in this subsystem context
  tileAttachParams.zip(tile_prci_domains).foreach { case (params, td) =>
    params.connect(td.asInstanceOf[TilePRCIDomain[params.TileType]], this.asInstanceOf[params.TileContextType])
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
  val plicNodes: Map[Int, IntNode]
  val debugNodes: Map[Int, IntSyncNode]
  val nmiNodes: Map[Int, BundleBridgeNode[NMI]]
  val tileHartIdNodes: Map[Int, BundleBridgeNode[UInt]]
  val tileResetVectorNodes: Map[Int, BundleBridgeNode[UInt]]
}

/** This trait provides the tile attachment context for the root (outermost) subsystem */
trait HasElementsRootContext
{ this: HasElements
    with HasPeripheryDebug
    with CanHavePeripheryCLINT
    with CanHavePeripheryPLIC
    with HasTileNotificationSinks
    with InstantiatesElements =>
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

  val plicNodes: Map[Int, IntNode] = (0 until nTotalTiles).map { i =>
    plicOpt.map(o => (i, o.intnode :=* IntEphemeralNode()))
  }.flatten.toMap

  val debugNodes: Map[Int, IntSyncNode] = (0 until nTotalTiles).map { i =>
    (i, IntSyncIdentityNode())
  }.toMap

  debugNodes.foreach { case (hartid, node) =>
    node := debugOpt.map(_.intnode).getOrElse(IntSyncCrossingSource() := NullIntSource())
  }

  val nmiHarts = tileParams.filter(_.core.useNMI).map(_.hartId)
  val nmiIONodes = nmiHarts.map { i => (i, BundleBridgeSource[NMI]()) }.toMap
  val nmiNodes: Map[Int, BundleBridgeNode[NMI]] = nmiIONodes.map { case (i, n) =>
    (i, BundleBridgeEphemeralNode[NMI]() := n)
  }.toMap
}
