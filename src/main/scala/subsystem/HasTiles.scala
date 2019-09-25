// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import Chisel._
import chisel3.experimental.dontTouch
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.devices.debug.TLDebugModule
import freechips.rocketchip.devices.tilelink.{BasicBusBlocker, BasicBusBlockerParams, CLINT, CLINTConsts, TLPLIC, PLICKey}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tile.{BaseTile, LookupByHartId, LookupByHartIdImpl, TileParams, HasExternallyDrivenTileConstants}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

trait HasTiles extends HasCoreMonitorBundles { this: BaseSubsystem =>
  implicit val p: Parameters
  val tiles: Seq[BaseTile]
  protected def tileParams: Seq[TileParams] = tiles.map(_.tileParams)
  def nTiles: Int = tileParams.size
  def hartIdList: Seq[Int] = tileParams.map(_.hartId)
  def localIntCounts: Seq[Int] = tileParams.map(_.core.nLocalInterrupts)

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
            cbus.coupleTo("bus_blocker") { bbb.controlNode := TLFragmenter(cbus) := _ }
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

trait HasTilesModuleImp extends LazyModuleImp {
  val outer: HasTiles

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
