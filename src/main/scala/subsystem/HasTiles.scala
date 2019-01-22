// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import Chisel._
import chisel3.experimental.dontTouch
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.devices.debug.TLDebugModule
import freechips.rocketchip.devices.tilelink.{BasicBusBlocker, BasicBusBlockerParams, CLINT, CLINTConsts, TLPLIC, PLICKey}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tile.{BaseTile, LookupByHartId, LookupByHartIdImpl, TileKey, TileParams, SharedMemoryTLEdge, HasExternallyDrivenTileConstants}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

class ClockedTileInputs(implicit val p: Parameters) extends ParameterizedBundle
    with HasExternallyDrivenTileConstants
    with Clocked

trait HasTiles extends HasCoreMonitorBundles { this: BaseSubsystem =>
  implicit val p: Parameters
  val tiles: Seq[BaseTile]
  protected def tileParams: Seq[TileParams] = tiles.map(_.tileParams)
  def nTiles: Int = tileParams.size
  def hartIdList: Seq[Int] = tileParams.map(_.hartId)
  def localIntCounts: Seq[Int] = tileParams.map(_.core.nLocalInterrupts)
  def sharedMemoryTLEdge = sbus.busView

  // define some nodes that are useful for collecting or driving tile interrupts
  val meipNode = p(PLICKey) match {
    case Some(_) => None
    case None    => Some(IntNexusNode(
      sourceFn = { _ => IntSourcePortParameters(Seq(IntSourceParameters(1))) },
      sinkFn   = { _ => IntSinkPortParameters(Seq(IntSinkParameters())) },
      outputRequiresInput = false,
      inputRequiresOutput = false))
  }

  val tileHaltXbarNode = IntXbar(p)
  val tileHaltSinkNode = IntSinkNode(IntSinkPortSimple())
  tileHaltSinkNode := tileHaltXbarNode

  val tileWFIXbarNode = IntXbar(p)
  val tileWFISinkNode = IntSinkNode(IntSinkPortSimple())
  tileWFISinkNode := tileWFIXbarNode

  val tileCeaseXbarNode = IntXbar(p)
  val tileCeaseSinkNode = IntSinkNode(IntSinkPortSimple())
  tileCeaseSinkNode := tileCeaseXbarNode

  private val lookupByHartId = new LookupByHartIdImpl {
    def apply[T <: Data](f: TileParams => Option[T], hartId: UInt): T =
      PriorityMux(tileParams.collect { case t if f(t).isDefined => (t.hartId.U === hartId) -> f(t).get })
  }

  protected def augmentedTileParameters(tp: TileParams): Parameters = p.alterPartial {
    // For legacy reasons, it is convenient to store some state
    // in the global Parameters about the specific tile being built now
    case TileKey => tp
    case SharedMemoryTLEdge => sharedMemoryTLEdge
    case LookupByHartId => lookupByHartId
  }

  protected def connectMasterPortsToSBus(tile: BaseTile, crossing: RocketCrossingParams) {
    sbus.fromTile(tile.tileParams.name, crossing.master.buffers) {
        crossing.master.cork
          .map { u => TLCacheCork(unsafe = u) }
          .map { _ :=* tile.crossMasterPort() }
          .getOrElse { tile.crossMasterPort() }
    }
  }

  protected def connectSlavePortsToCBus(tile: BaseTile, crossing: RocketCrossingParams)(implicit valName: ValName) {

    DisableMonitors { implicit p =>
      cbus.toTile(tile.tileParams.name) {
        crossing.slave.blockerCtrlAddr
          .map { BasicBusBlockerParams(_, pbus.beatBytes, sbus.beatBytes) }
          .map { bbbp => LazyModule(new BasicBusBlocker(bbbp)) }
          .map { bbb =>
            cbus.coupleTo("bus_blocker") { bbb.controlNode := _ }
            tile.crossSlavePort() :*= bbb.node
          } .getOrElse { tile.crossSlavePort() }
      }
    }
  }

  protected def connectInterrupts(tile: BaseTile, debugOpt: Option[TLDebugModule], clintOpt: Option[CLINT], plicOpt: Option[TLPLIC]) {
    // Handle all the different types of interrupts crossing to or from the tile:
    // NOTE: The order of calls to := matters! They must match how interrupts
    //       are decoded from tile.intInwardNode inside the tile. For this reason,
    //       we stub out missing interrupts with constant sources here.

    // 1. Debug interrupt is definitely asynchronous in all cases.
    tile.intInwardNode :=
      debugOpt
        .map { tile { IntSyncCrossingSink(3) } := _.intnode }
        .getOrElse { NullIntSource() }

    // 2. The CLINT and PLIC output interrupts are synchronous to the TileLink bus clock,
    //    so might need to be synchronized depending on the Tile's crossing type.

    //    From CLINT: "msip" and "mtip"
    tile.crossIntIn() :=
      clintOpt.map { _.intnode }
        .getOrElse { NullIntSource(sources = CLINTConsts.ints) }

    //    From PLIC: "meip"
    tile.crossIntIn() :=
      plicOpt .map { _.intnode }
        .getOrElse { meipNode.get }

    //    From PLIC: "seip" (only if vm/supervisor mode is enabled)
    if (tile.tileParams.core.useVM) {
      tile.crossIntIn() :=
        plicOpt .map { _.intnode }
          .getOrElse { NullIntSource() }
    }

    // 3. Local Interrupts ("lip") are required to already be synchronous to the Tile's clock.
    // (they are connected to tile.intInwardNode in a seperate trait)

    // 4. Interrupts coming out of the tile are sent to the PLIC,
    //    so might need to be synchronized depending on the Tile's crossing type.
    plicOpt.foreach { plic =>
      FlipRendering { implicit p =>
        plic.intnode :=* tile.crossIntOut()
      }
    }

    // 5. Reports of tile status are collected without needing to be clock-crossed
    tileHaltXbarNode := tile.haltNode
    tileWFIXbarNode := tile.wfiNode
    tileCeaseXbarNode := tile.ceaseNode
  }

  protected def perTileOrGlobalSetting[T](in: Seq[T], n: Int): Seq[T] = in.size match {
    case 1 => List.fill(n)(in.head)
    case x if x == n => in
    case _ => throw new Exception("must provide exactly 1 or #tiles of this key")
  }
}

trait HasTilesBundle {
  val tile_inputs: Vec[ClockedTileInputs]
}

trait HasTilesModuleImp extends LazyModuleImp
    with HasTilesBundle
    with HasResetVectorWire {
  val outer: HasTiles

  def resetVectorBits: Int = {
    // Consider using the minimum over all widths, rather than enforcing homogeneity
    val vectors = outer.tiles.map(_.module.constants.reset_vector)
    require(vectors.tail.forall(_.getWidth == vectors.head.getWidth))
    vectors.head.getWidth
  }

  val tile_inputs = dontTouch(Wire(Vec(outer.nTiles, new ClockedTileInputs()(p.alterPartial {
    case SharedMemoryTLEdge => outer.sharedMemoryTLEdge
  })))) // dontTouch keeps constant prop from sucking these signals into the tile

  // Unconditionally wire up the non-diplomatic tile inputs
  outer.tiles.map(_.module).zip(tile_inputs).foreach { case(tile, wire) =>
    tile.clock := wire.clock
    tile.reset := wire.reset
    tile.constants.hartid := wire.hartid
    tile.constants.reset_vector := wire.reset_vector
  }

  val meip = if(outer.meipNode.isDefined) Some(IO(Vec(outer.meipNode.get.out.size, Bool()).asInput)) else None
  meip.foreach { m =>
    m.zipWithIndex.foreach{ case (pin, i) =>
      (outer.meipNode.get.out(i)._1)(0) := pin
    }
  }
}
