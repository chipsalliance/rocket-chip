// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import Chisel._
import chisel3.dontTouch
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.devices.debug.{HasPeripheryDebug, HasPeripheryDebugModuleImp}
import freechips.rocketchip.devices.tilelink.{BasicBusBlocker, BasicBusBlockerParams, CLINTConsts, PLICKey, CanHavePeripheryPLIC, CanHavePeripheryCLINT}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomaticobjectmodel.logicaltree.{LogicalModuleTree}
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tile.{BaseTile, LookupByHartIdImpl, TileParams, HasExternallyDrivenTileConstants, InstantiatableTileParams, PriorityMuxHartIdFromSeq}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

/** Entry point for Config-uring the presence of Tiles */
case class TilesLocated(loc: HierarchicalLocation) extends Field[Seq[CanAttachTile]](Nil)

/** An interface for describing the parameteization of how Tiles are connected to interconnects */
trait TileCrossingParamsLike {
  val crossingType: ClockCrossingType
  val master: TilePortParamsLike
  val slave: TilePortParamsLike
}

/** An interface for describing the parameterization of how a particular tile port is connected to an interconnect */
trait TilePortParamsLike {
  // the subnetwork location of the interconnect to which this tile port should be connected
  def where: TLBusWrapperLocation
  // allows port-specific adapters to be injected into the interconnect side of the attachment point
  def injectNode(context: Attachable)(implicit p: Parameters): TLNode
}

/** A default implementation of parameterizing the connectivity of the port where the tile is the master.
  *   Optional timing buffers and/or an optional CacheCork can be inserted in the interconnect's clock domain.
  */
case class TileMasterPortParams(
  buffers: Int = 0,
  cork: Option[Boolean] = None,
  where: TLBusWrapperLocation = SBUS
) extends TilePortParamsLike {
  def injectNode(context: Attachable)(implicit p: Parameters): TLNode = {
    (TLBuffer(buffers) :=* cork.map { u => TLCacheCork(unsafe = u) } .getOrElse { TLTempNode() })
  }
}

/** A default implementation of parameterizing the connectivity of the port giving access to slaves inside the tile.
  *   Optional timing buffers and/or an optional BusBlocker adapter can be inserted in the interconnect's clock domain.
  */
case class TileSlavePortParams(
  buffers: Int = 0,
  blockerCtrlAddr: Option[BigInt] = None,
  blockerCtrlWhere: TLBusWrapperLocation = CBUS,
  where: TLBusWrapperLocation = CBUS
) extends TilePortParamsLike {
  def injectNode(context: Attachable)(implicit p: Parameters): TLNode = {
    val controlBus = context.locateTLBusWrapper(where)
    val blockerBus = context.locateTLBusWrapper(blockerCtrlWhere)
    blockerCtrlAddr
      .map { BasicBusBlockerParams(_, blockerBus.beatBytes, controlBus.beatBytes) }
      .map { bbbp =>
        val blocker = LazyModule(new BasicBusBlocker(bbbp))
        blockerBus.coupleTo("tile_slave_port_bus_blocker") { blocker.controlNode := TLFragmenter(blockerBus) := _ }
        blocker.node :*= TLBuffer(buffers)
      } .getOrElse { TLBuffer(buffers) }
  }
}

/** These are sources of interrupts that are driven into the tile.
  * They need to be instantiated before tiles are attached to the subsystem containing them.
  */
trait HasTileInterruptSources
  extends CanHavePeripheryPLIC
  with CanHavePeripheryCLINT
  with HasPeripheryDebug
{ this: BaseSubsystem => // TODO ideally this bound would be softened to LazyModule
  // meipNode is used to create a single bit subsystem input in Configs without a PLIC
  val meipNode = p(PLICKey) match {
    case Some(_) => None
    case None    => Some(IntNexusNode(
      sourceFn = { _ => IntSourcePortParameters(Seq(IntSourceParameters(1))) },
      sinkFn   = { _ => IntSinkPortParameters(Seq(IntSinkParameters())) },
      outputRequiresInput = false,
      inputRequiresOutput = false))
  }
}

/** These are sinks of notifications that are driven out from the tile.
  * They need to be instantiated before tiles are attached to the subsystem containing them.
  */
trait HasTileNotificationSinks { this: LazyModule =>
  val tileHaltXbarNode = IntXbar(p)
  val tileHaltSinkNode = IntSinkNode(IntSinkPortSimple())
  tileHaltSinkNode := tileHaltXbarNode

  val tileWFIXbarNode = IntXbar(p)
  val tileWFISinkNode = IntSinkNode(IntSinkPortSimple())
  tileWFISinkNode := tileWFIXbarNode

  val tileCeaseXbarNode = IntXbar(p)
  val tileCeaseSinkNode = IntSinkNode(IntSinkPortSimple())
  tileCeaseSinkNode := tileCeaseXbarNode
}

/** HasTiles adds a Config-urable sequence of tiles of any type
  *   to the subsystem class into which it is mixed.
  */
trait HasTiles extends HasCoreMonitorBundles with DefaultTileContextType
{ this: BaseSubsystem => // TODO: ideally this bound would be softened to Attachable
  implicit val p: Parameters

  // Actually instantiate all tiles, in order based on statically-assigned hartids
  val tileAttachParams: Seq[CanAttachTile] = p(TilesLocated(location)).sortBy(_.tileParams.hartId)
  val tiles: Seq[BaseTile] = tileAttachParams.map(_.instantiate(p))

  // Helper functions for accessing certain parameters the are popular to refer to in subsystem code
  val tileParams: Seq[TileParams] = tileAttachParams.map(_.tileParams)
  val tileCrossingTypes = tileAttachParams.map(_.crossingParams.crossingType)
  def nTiles: Int = tileAttachParams.size
  def hartIdList: Seq[Int] = tileParams.map(_.hartId)
  def localIntCounts: Seq[Int] = tileParams.map(_.core.nLocalInterrupts)

  require(hartIdList.distinct.size == tiles.size, s"Every tile must be statically assigned a unique id, but got:\n${hartIdList}")

  // connect all the tiles to interconnect attachment points made available in this subsystem context
  tileAttachParams.zip(tiles).foreach { case (params, t) =>
    params.connect(t.asInstanceOf[params.TileType], this.asInstanceOf[params.TileContextType])
  }
}

/** Most tile types require only these traits in order for their standardized connect functions to apply.
  *    BaseTiles subtypes with different needs can extend this trait to provide themselves with
  *    additional external connection points.
  */
trait DefaultTileContextType
  extends Attachable
  with HasTileInterruptSources
  with HasTileNotificationSinks
{ this: BaseSubsystem => } // TODO: ideally this bound would be softened to LazyModule

/** Standardized interface by which parameterized tiles can be attached to contexts containing interconnect resources.
  *   Sub-classes of this trait can optionally override the individual connect functions in order to specialize
  *   their attachment behaviors, but most use cases should be be handled simply by changing the implementation
  *   of the injectNode functions in crossingParams.
  */
trait CanAttachTile {
  type TileType <: BaseTile
  type TileContextType <: DefaultTileContextType
  def tileParams: InstantiatableTileParams[TileType]
  def crossingParams: TileCrossingParamsLike
  def lookup: LookupByHartIdImpl

  // narrow waist through which all tiles are intended to pass while being instantiated
  def instantiate(implicit p: Parameters): TileType = {
    val tile = LazyModule(tileParams.instantiate(crossingParams, lookup))
    tile
  }

  // a default set of connections that need to occur for most tile types
  def connect(tile: TileType, context: TileContextType): Unit = {
    connectMasterPorts(tile, context)
    connectSlavePorts(tile, context)
    connectInterrupts(tile, context)
    LogicalModuleTree.add(context.logicalTreeNode, tile.logicalTreeNode)
  }

  // connect the port where the tile is the master to a TileLink interconnect
  def connectMasterPorts(tile: TileType, context: Attachable): Unit = {
    implicit val p = context.p
    val dataBus = context.locateTLBusWrapper(crossingParams.master.where)
    dataBus.coupleFrom(tileParams.name.getOrElse("tile")) { bus =>
      bus :=* crossingParams.master.injectNode(context) :=* tile.crossMasterPort()
    }
  }

  // connect the port where the tile is the slave to a TileLink interconnect
  def connectSlavePorts(tile: TileType, context: Attachable): Unit = {
    implicit val p = context.p
    DisableMonitors { implicit p =>
      val controlBus = context.locateTLBusWrapper(crossingParams.slave.where)
      controlBus.coupleTo(tileParams.name.getOrElse("tile")) { bus =>
        tile.crossSlavePort() :*= crossingParams.slave.injectNode(context) :*= TLWidthWidget(controlBus.beatBytes) :*= bus
      }
    }
  }

  // connect the various interrupt and notification wires going to and from the tile
  def connectInterrupts(tile: TileType, context: TileContextType): Unit = {
    implicit val p = context.p
    // NOTE: The order of calls to := matters! They must match how interrupts
    //       are decoded from tile.intInwardNode inside the tile. For this reason,
    //       we stub out missing interrupts with constant sources here.

    // 1. Debug interrupt is definitely asynchronous in all cases.
    tile.intInwardNode :=
      context.debugOpt
        .map { tile { IntSyncAsyncCrossingSink(3) } := _.intnode }
        .getOrElse { NullIntSource() }

    // 2. The CLINT and PLIC output interrupts are synchronous to the TileLink bus clock,
    //    so might need to be synchronized depending on the Tile's crossing type.

    //    From CLINT: "msip" and "mtip"
    tile.crossIntIn() :=
      context.clintOpt.map { _.intnode }
        .getOrElse { NullIntSource(sources = CLINTConsts.ints) }

    //    From PLIC: "meip"
    tile.crossIntIn() :=
      context.plicOpt .map { _.intnode }
        .getOrElse { context.meipNode.get }

    //    From PLIC: "seip" (only if supervisor mode is enabled)
    if (tile.tileParams.core.hasSupervisorMode) {
      tile.crossIntIn() :=
        context.plicOpt .map { _.intnode }
          .getOrElse { NullIntSource() }
    }

    // 3. Local Interrupts ("lip") are required to already be synchronous to the Tile's clock.
    // (they are connected to tile.intInwardNode in a seperate trait)

    // 4. Interrupts coming out of the tile are sent to the PLIC,
    //    so might need to be synchronized depending on the Tile's crossing type.
    context.plicOpt.foreach { plic =>
      FlipRendering { implicit p =>
        plic.intnode :=* tile.crossIntOut()
      }
    }

    // 5. Notifications of tile status are collected without needing to be clock-crossed
    context.tileHaltXbarNode :=* tile.haltNode
    context.tileWFIXbarNode :=* tile.wfiNode
    context.tileCeaseXbarNode :=* tile.ceaseNode
  }
}

/** Provides some Chisel connectivity to certain tile IOs */
trait HasTilesModuleImp extends LazyModuleImp with HasPeripheryDebugModuleImp {
  val outer: HasTiles with HasTileInterruptSources

  def resetVectorBits: Int = {
    // Consider using the minimum over all widths, rather than enforcing homogeneity
    val vectors = outer.tiles.map(_.module.constants.reset_vector)
    require(vectors.tail.forall(_.getWidth == vectors.head.getWidth))
    vectors.head.getWidth
  }

  val tile_inputs = outer.tiles.map(_.module.constants)

  val meip = if(outer.meipNode.isDefined) Some(IO(Vec(outer.meipNode.get.out.size, Bool()).asInput)) else None
  meip.foreach { m =>
    m.zipWithIndex.foreach{ case (pin, i) =>
      (outer.meipNode.get.out(i)._1)(0) := pin
    }
  }
}
