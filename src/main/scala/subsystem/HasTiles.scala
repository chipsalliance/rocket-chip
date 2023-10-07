// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import chisel3._
import chisel3.dontTouch
import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.devices.tilelink.{BasicBusBlocker, BasicBusBlockerParams, CLINTConsts, PLICKey, CanHavePeripheryPLIC, CanHavePeripheryCLINT}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.prci.{ClockGroup, ResetCrossingType, ClockGroupNode}
import freechips.rocketchip.util._

/** Entry point for Config-uring the presence of Tiles */
case class TilesLocated(loc: HierarchicalLocation) extends Field[Seq[CanAttachTile]](Nil)

/** Whether to add timing-closure registers along the path of the hart id
  * as it propagates through the subsystem and into the tile.
  *
  * These are typically only desirable when a dynamically programmable prefix is being combined
  * with the static hart id via [[freechips.rocketchip.subsystem.HasTiles.tileHartIdNexusNode]].
  */
case object InsertTimingClosureRegistersOnHartIds extends Field[Boolean](false)

/** Whether per-tile hart ids are going to be driven as inputs into the subsystem,
  * and if so, what their width should be.
  */
case object SubsystemExternalHartIdWidthKey extends Field[Option[Int]](None)

/** Whether per-tile reset vectors are going to be driven as inputs into the subsystem.
  *
  * Unlike the hart ids, the reset vector width is determined by the sinks within the tiles,
  * based on the size of the address map visible to the tiles.
  */
case object SubsystemExternalResetVectorKey extends Field[Boolean](true)

/** An interface for describing the parameteization of how Tiles are connected to interconnects */
trait TileCrossingParamsLike {
  /** The type of clock crossing that should be inserted at the tile boundary. */
  def crossingType: ClockCrossingType
  /** Parameters describing the contents and behavior of the point where the tile is attached as an interconnect master. */
  def master: TilePortParamsLike
  /** Parameters describing the contents and behavior of the point where the tile is attached as an interconnect slave. */
  def slave: TilePortParamsLike
  /** The subnetwork location of the device selecting the apparent base address of MMIO devices inside the tile */
  def mmioBaseAddressPrefixWhere: TLBusWrapperLocation
  /** Inject a reset management subgraph that effects the tile child reset only */
  def resetCrossingType: ResetCrossingType
  /** Keep the tile clock separate from the interconnect clock (e.g. even if they are synchronous to one another) */
  def forceSeparateClockReset: Boolean
}

/** An interface for describing the parameterization of how a particular tile port is connected to an interconnect */
trait TilePortParamsLike {
  /** The subnetwork location of the interconnect to which this tile port should be connected. */
  def where: TLBusWrapperLocation
  /** Allows port-specific adapters to be injected into the interconnect side of the attachment point. */
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
    (TLBuffer.chainNode(buffers) :=* cork.map { u => TLCacheCork(unsafe = u) } .getOrElse { TLTempNode() })
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
        blocker.node :*= TLBuffer.chainNode(buffers)
      } .getOrElse { TLBuffer.chainNode(buffers) }
  }
}

/** These are sources of interrupts that are driven into the tile.
  * They need to be instantiated before tiles are attached to the subsystem containing them.
  */
trait HasTileInterruptSources
  extends CanHavePeripheryPLIC
  with CanHavePeripheryCLINT
  with InstantiatesTiles
{ this: BaseSubsystem => // TODO ideally this bound would be softened to LazyModule
  /** meipNode is used to create a single bit subsystem input in Configs without a PLIC */
  val meipNode = p(PLICKey) match {
    case Some(_) => None
    case None    => Some(IntNexusNode(
      sourceFn = { _ => IntSourcePortParameters(Seq(IntSourceParameters(1))) },
      sinkFn   = { _ => IntSinkPortParameters(Seq(IntSinkParameters())) },
      outputRequiresInput = false,
      inputRequiresOutput = false))
  }
  val seipNode = p(PLICKey) match {
    case Some(_) => None
    case None    => Some(IntNexusNode(
      sourceFn = { _ => IntSourcePortParameters(Seq(IntSourceParameters(1))) },
      sinkFn   = { _ => IntSinkPortParameters(Seq(IntSinkParameters())) },
      outputRequiresInput = false,
      inputRequiresOutput = false))
  }
  /** Source of Non-maskable Interrupt (NMI) input bundle to each tile. */
  val tileNMINode = BundleBridgeEphemeralNode[NMI]()
  val tileNMIIONodes: Seq[BundleBridgeSource[NMI]] = {
    Seq.fill(tiles.size) {
      val nmiSource = BundleBridgeSource[NMI]()
      tileNMINode := nmiSource
      nmiSource
    }
  }
}

/** These are sources of "constants" that are driven into the tile.
  * 
  * While they are not expected to change dyanmically while the tile is executing code,
  * they may be either tied to a contant value or programmed during boot or reset.
  * They need to be instantiated before tiles are attached within the subsystem containing them.
  */
trait HasTileInputConstants extends InstantiatesTiles { this: BaseSubsystem =>
  /** tileHartIdNode is used to collect publishers and subscribers of hartids. */
  val tileHartIdNode = BundleBridgeEphemeralNode[UInt]()

  /** tileHartIdNexusNode is a BundleBridgeNexus that collects dynamic hart prefixes.
    *
    *   Each "prefix" input is actually the same full width as the outer hart id; the expected usage
    *   is that each prefix source would set only some non-overlapping portion of the bits to non-zero values.
    *   This node orReduces them, and further combines the reduction with the static ids assigned to each tile,
    *   producing a unique, dynamic hart id for each tile.
    *
    *   If p(InsertTimingClosureRegistersOnHartIds) is set, the input and output values are registered.
    *
    *   The output values are [[dontTouch]]'d to prevent constant propagation from pulling the values into
    *   the tiles if they are constant, which would ruin deduplication of tiles that are otherwise homogeneous.
    */
  val tileHartIdNexusNode = LazyModule(new BundleBridgeNexus[UInt](
    inputFn = BundleBridgeNexus.orReduction[UInt](registered = p(InsertTimingClosureRegistersOnHartIds)) _,
    outputFn = (prefix: UInt, n: Int) =>  Seq.tabulate(n) { i =>
      val y = dontTouch(prefix | hartIdList(i).U(p(MaxHartIdBits).W)) // dontTouch to keep constant prop from breaking tile dedup
      if (p(InsertTimingClosureRegistersOnHartIds)) BundleBridgeNexus.safeRegNext(y) else y
    },
    default = Some(() => 0.U(p(MaxHartIdBits).W)),
    inputRequiresOutput = true, // guard against this being driven but then ignored in tileHartIdIONodes below
    shouldBeInlined = false // can't inline something whose output we are are dontTouching
  )).node
  // TODO: Replace the DebugModuleHartSelFuncs config key with logic to consume the dynamic hart IDs

  /** tileResetVectorNode is used to collect publishers and subscribers of tile reset vector addresses. */
  val tileResetVectorNode = BundleBridgeEphemeralNode[UInt]()

  /** tileResetVectorNexusNode is a BundleBridgeNexus that accepts a single reset vector source, and broadcasts it to all tiles. */
  val tileResetVectorNexusNode = BundleBroadcast[UInt](
    inputRequiresOutput = true // guard against this being driven but ignored in tileResetVectorIONodes below
  )

  /** tileHartIdIONodes may generate subsystem IOs, one per tile, allowing the parent to assign unique hart ids.
    *
    *   Or, if such IOs are not configured to exist, tileHartIdNexusNode is used to supply an id to each tile.
    */
  val tileHartIdIONodes: Seq[BundleBridgeSource[UInt]] = p(SubsystemExternalHartIdWidthKey) match {
    case Some(w) => Seq.fill(tiles.size) {
      val hartIdSource = BundleBridgeSource(() => UInt(w.W))
      tileHartIdNode := hartIdSource
      hartIdSource
    }
    case None => { tileHartIdNode :*= tileHartIdNexusNode; Nil }
  }

  /** tileResetVectorIONodes may generate subsystem IOs, one per tile, allowing the parent to assign unique reset vectors.
    *
    *   Or, if such IOs are not configured to exist, tileResetVectorNexusNode is used to supply a single reset vector to every tile.
    */
  val tileResetVectorIONodes: Seq[BundleBridgeSource[UInt]] = p(SubsystemExternalResetVectorKey) match {
    case true => Seq.fill(tiles.size) {
      val resetVectorSource = BundleBridgeSource[UInt]()
      tileResetVectorNode := resetVectorSource
      resetVectorSource
    }
    case false => { tileResetVectorNode :*= tileResetVectorNexusNode; Nil }
  }
}

/** These are sinks of notifications that are driven out from the tile.
  *
  * They need to be instantiated before tiles are attached to the subsystem containing them.
  */
trait HasTileNotificationSinks { this: LazyModule =>
  val tileHaltXbarNode = IntXbar()
  val tileHaltSinkNode = IntSinkNode(IntSinkPortSimple())
  tileHaltSinkNode := tileHaltXbarNode

  val tileWFIXbarNode = IntXbar()
  val tileWFISinkNode = IntSinkNode(IntSinkPortSimple())
  tileWFISinkNode := tileWFIXbarNode

  val tileCeaseXbarNode = IntXbar()
  val tileCeaseSinkNode = IntSinkNode(IntSinkPortSimple())
  tileCeaseSinkNode := tileCeaseXbarNode
}

/** Most tile types require only these traits in order for their standardized connect functions to apply.
  *
  *    BaseTiles subtypes with different needs can extend this trait to provide themselves with
  *    additional external connection points.
  */
trait DefaultTileContextType
  extends Attachable
  with HasTileInterruptSources
  with HasTileNotificationSinks
  with HasTileInputConstants
{ this: BaseSubsystem =>
  val debugNode: IntSyncOutwardNode
} // TODO: ideally this bound would be softened to LazyModule

/** Standardized interface by which parameterized tiles can be attached to contexts containing interconnect resources.
  *
  *   Sub-classes of this trait can optionally override the individual connect functions in order to specialize
  *   their attachment behaviors, but most use cases should be be handled simply by changing the implementation
  *   of the injectNode functions in crossingParams.
  */
trait CanAttachTile {
  type TileType <: BaseTile
  type TileContextType <: DefaultTileContextType
  def tileParams: InstantiableTileParams[TileType]
  def crossingParams: TileCrossingParamsLike

  /** Narrow waist through which all tiles are intended to pass while being instantiated. */
  def instantiate(allTileParams: Seq[TileParams], instantiatedTiles: Seq[TilePRCIDomain[_]])(implicit p: Parameters): TilePRCIDomain[TileType] = {
    val clockSinkParams = tileParams.clockSinkParams.copy(name = Some(s"${tileParams.name.getOrElse("core")}_${tileParams.hartId}"))
    val tile_prci_domain = LazyModule(new TilePRCIDomain[TileType](clockSinkParams, crossingParams) { self =>
      val tile = self.tile_reset_domain { LazyModule(tileParams.instantiate(crossingParams, PriorityMuxHartIdFromSeq(allTileParams))) }
    })
    tile_prci_domain
  }

  /** A default set of connections that need to occur for most tile types */
  def connect(domain: TilePRCIDomain[TileType], context: TileContextType): Unit = {
    connectMasterPorts(domain, context)
    connectSlavePorts(domain, context)
    connectInterrupts(domain, context)
    connectPRC(domain, context)
    connectOutputNotifications(domain, context)
    connectInputConstants(domain, context)
  }

  /** Connect the port where the tile is the master to a TileLink interconnect. */
  def connectMasterPorts(domain: TilePRCIDomain[TileType], context: Attachable): Unit = {
    implicit val p = context.p
    val dataBus = context.locateTLBusWrapper(crossingParams.master.where)
    dataBus.coupleFrom(tileParams.name.getOrElse("tile")) { bus =>
      bus :=* crossingParams.master.injectNode(context) :=* domain.crossMasterPort(crossingParams.crossingType)
    }
  }

  /** Connect the port where the tile is the slave to a TileLink interconnect. */
  def connectSlavePorts(domain: TilePRCIDomain[TileType], context: Attachable): Unit = {
    implicit val p = context.p
    DisableMonitors { implicit p =>
      val controlBus = context.locateTLBusWrapper(crossingParams.slave.where)
      controlBus.coupleTo(tileParams.name.getOrElse("tile")) { bus =>
        domain.crossSlavePort(crossingParams.crossingType) :*= crossingParams.slave.injectNode(context) :*= TLWidthWidget(controlBus.beatBytes) :*= bus
      }
    }
  }

  /** Connect the various interrupts sent to and and raised by the tile. */
  def connectInterrupts(domain: TilePRCIDomain[TileType], context: TileContextType): Unit = {
    implicit val p = context.p
    // NOTE: The order of calls to := matters! They must match how interrupts
    //       are decoded from tile.intInwardNode inside the tile. For this reason,
    //       we stub out missing interrupts with constant sources here.

    // 1. Debug interrupt is definitely asynchronous in all cases.
    domain.tile.intInwardNode := domain { IntSyncAsyncCrossingSink(3) } := context.debugNode

    // 2. The CLINT and PLIC output interrupts are synchronous to the TileLink bus clock,
    //    so might need to be synchronized depending on the Tile's crossing type.

    //    From CLINT: "msip" and "mtip"
    domain.crossIntIn(crossingParams.crossingType) :=
      context.clintOpt.map { _.intnode }
        .getOrElse { NullIntSource(sources = CLINTConsts.ints) }

    //    From PLIC: "meip"
    domain.crossIntIn(crossingParams.crossingType) :=
      context.plicOpt .map { _.intnode }
        .getOrElse { context.meipNode.get }

    //    From PLIC: "seip" (only if supervisor mode is enabled)
    if (domain.tile.tileParams.core.hasSupervisorMode) {
      domain.crossIntIn(crossingParams.crossingType) :=
        context.plicOpt .map { _.intnode }
          .getOrElse { context.seipNode.get }
    }

    // 3. Local Interrupts ("lip") are required to already be synchronous to the Tile's clock.
    // (they are connected to domain.tile.intInwardNode in a seperate trait)

    // 4. Interrupts coming out of the tile are sent to the PLIC,
    //    so might need to be synchronized depending on the Tile's crossing type.
    context.plicOpt.foreach { plic =>
      FlipRendering { implicit p =>
        plic.intnode :=* domain.crossIntOut(crossingParams.crossingType, domain.tile.intOutwardNode)
      }
    }

    // 5. Connect NMI inputs to the tile. These inputs are synchronous to the respective core_clock.
    domain.tile.nmiNode := context.tileNMINode
  }

  /** Notifications of tile status are connected to be broadcast without needing to be clock-crossed. */
  def connectOutputNotifications(domain: TilePRCIDomain[TileType], context: TileContextType): Unit = {
    implicit val p = context.p
    context.tileHaltXbarNode  :=* domain.crossIntOut(NoCrossing, domain.tile.haltNode)
    context.tileWFIXbarNode   :=* domain.crossIntOut(NoCrossing, domain.tile.wfiNode)
    context.tileCeaseXbarNode :=* domain.crossIntOut(NoCrossing, domain.tile.ceaseNode)
    // TODO should context be forced to have a trace sink connected here?
    //      for now this just ensures domain.trace[Core]Node has been crossed without connecting it externally
    domain.crossTracesOut()
  }

  /** Connect inputs to the tile that are assumed to be constant during normal operation, and so are not clock-crossed. */
  def connectInputConstants(domain: TilePRCIDomain[TileType], context: TileContextType): Unit = {
    implicit val p = context.p
    val tlBusToGetPrefixFrom = context.locateTLBusWrapper(crossingParams.mmioBaseAddressPrefixWhere)
    domain.tile.hartIdNode := context.tileHartIdNode
    domain.tile.resetVectorNode := context.tileResetVectorNode
    tlBusToGetPrefixFrom.prefixNode.foreach { domain.tile.mmioAddressPrefixNode := _ }
  }

  /** Connect power/reset/clock resources. */
  def connectPRC(domain: TilePRCIDomain[TileType], context: TileContextType): Unit = {
    implicit val p = context.p
    val tlBusToGetClockDriverFrom = context.locateTLBusWrapper(crossingParams.master.where)
    (crossingParams.crossingType match {
      case _: SynchronousCrossing | _: CreditedCrossing =>
        if (crossingParams.forceSeparateClockReset) {
          domain.clockNode := tlBusToGetClockDriverFrom.clockNode
        } else {
          domain.clockNode := tlBusToGetClockDriverFrom.fixedClockNode
        }
      case _: RationalCrossing => domain.clockNode := tlBusToGetClockDriverFrom.clockNode
      case _: AsynchronousCrossing => {
        val tileClockGroup = ClockGroup()
        tileClockGroup := context.asyncClockGroupsNode
        domain.clockNode := tileClockGroup
      }
    })

    domain {
      domain.tile_reset_domain.clockNode := crossingParams.resetCrossingType.injectClockNode := domain.clockNode
    }
  }
}

case class CloneTileAttachParams(
  sourceHart: Int,
  cloneParams: CanAttachTile
) extends CanAttachTile {
  type TileType = cloneParams.TileType
  type TileContextType = cloneParams.TileContextType

  def tileParams = cloneParams.tileParams
  def crossingParams = cloneParams.crossingParams
  require(sourceHart < tileParams.hartId)

  override def instantiate(allTileParams: Seq[TileParams], instantiatedTiles: Seq[TilePRCIDomain[_]])(implicit p: Parameters): TilePRCIDomain[TileType] = {
    val clockSinkParams = tileParams.clockSinkParams.copy(name = Some(s"${tileParams.name.getOrElse("core")}_${tileParams.hartId}"))
    val tile_prci_domain = CloneLazyModule(
      new TilePRCIDomain[TileType](clockSinkParams, crossingParams) { self =>
        val tile = self.tile_reset_domain { LazyModule(tileParams.instantiate(crossingParams, PriorityMuxHartIdFromSeq(allTileParams))) }
      },
      instantiatedTiles(sourceHart).asInstanceOf[TilePRCIDomain[TileType]]
    )
    tile_prci_domain 
  }
}


/** InstantiatesTiles adds a Config-urable sequence of tiles of any type
  *   to the subsystem class into which it is mixed.
  */
trait InstantiatesTiles { this: BaseSubsystem =>
  /** Record the order in which to instantiate all tiles, based on statically-assigned ids.
    *
    * Note that these ids, which are often used as the tiles' default hartid input,
    * may or may not be those actually reflected at runtime in e.g. the $mhartid CSR
    */
  val tileAttachParams: Seq[CanAttachTile] = p(TilesLocated(location)).sortBy(_.tileParams.hartId)
  val tileParams: Seq[TileParams] = tileAttachParams.map(_.tileParams)
  val tileCrossingTypes: Seq[ClockCrossingType] = tileAttachParams.map(_.crossingParams.crossingType)

  /** The actual list of instantiated tiles in this subsystem. */
  val tile_prci_domains: Seq[TilePRCIDomain[_]] = tileAttachParams.foldLeft(Seq[TilePRCIDomain[_]]()) {
    case (instantiated, params) => instantiated :+ params.instantiate(tileParams, instantiated)(p)
  }

  val tiles: Seq[BaseTile] = tile_prci_domains.map(_.tile.asInstanceOf[BaseTile])

  // Helper functions for accessing certain parameters that are popular to refer to in subsystem code
  def nTiles: Int = tileAttachParams.size
  def hartIdList: Seq[Int] = tileParams.map(_.hartId)
  def localIntCounts: Seq[Int] = tileParams.map(_.core.nLocalInterrupts)

  require(hartIdList.distinct.size == tiles.size, s"Every tile must be statically assigned a unique id, but got:\n${hartIdList}")
}

/** HasTiles instantiates and also connects a Config-urable sequence of tiles of any type to subsystem interconnect resources. */
trait HasTiles extends InstantiatesTiles with HasCoreMonitorBundles with DefaultTileContextType
{ this: BaseSubsystem => // TODO: ideally this bound would be softened to Attachable
  implicit val p: Parameters

  // connect all the tiles to interconnect attachment points made available in this subsystem context
  tileAttachParams.zip(tile_prci_domains).foreach { case (params, td) =>
    params.connect(td.asInstanceOf[TilePRCIDomain[params.TileType]], this.asInstanceOf[params.TileContextType])
  }
}

/** Provides some Chisel connectivity to certain tile IOs */
trait HasTilesModuleImp extends LazyModuleImp {
  val outer: HasTiles with HasTileInterruptSources with HasTileInputConstants

  val reset_vector = outer.tileResetVectorIONodes.zipWithIndex.map { case (n, i) => n.makeIO(s"reset_vector_$i") }
  val tile_hartids = outer.tileHartIdIONodes.zipWithIndex.map { case (n, i) => n.makeIO(s"tile_hartids_$i") }

  val meip = if(outer.meipNode.isDefined) Some(IO(Input(Vec(outer.meipNode.get.out.size, Bool())))) else None
  meip.foreach { m =>
    m.zipWithIndex.foreach{ case (pin, i) =>
      (outer.meipNode.get.out(i)._1)(0) := pin
    }
  }
  val seip = if(outer.seipNode.isDefined) Some(IO(Input(Vec(outer.seipNode.get.out.size, Bool())))) else None
  seip.foreach { s =>
    s.zipWithIndex.foreach{ case (pin, i) =>
      (outer.seipNode.get.out(i)._1)(0) := pin
    }
  }
  val nmi = outer.tiles.zip(outer.tileNMIIONodes).zipWithIndex.map { case ((tile, n), i) => tile.tileParams.core.useNMI.option(n.makeIO(s"nmi_$i")) }
}
