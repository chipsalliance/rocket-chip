// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import Chisel._
import chisel3.experimental.dontTouch
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.devices.debug.TLDebugModule
import freechips.rocketchip.devices.tilelink.{BasicBusBlocker, BasicBusBlockerParams, CLINT, TLPLIC}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tile.{BaseTile, TileParams, SharedMemoryTLEdge, HasExternallyDrivenTileConstants}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

class ClockedTileInputs(implicit val p: Parameters) extends ParameterizedBundle
    with HasExternallyDrivenTileConstants
    with Clocked

trait HasTiles { this: BaseSubsystem =>
  implicit val p: Parameters
  val tiles: Seq[BaseTile]
  protected def tileParams: Seq[TileParams] = tiles.map(_.tileParams)
  def nTiles: Int = tileParams.size
  def hartIdList: Seq[Int] = tileParams.map(_.hartId)
  def localIntCounts: Seq[Int] = tileParams.map(_.core.nLocalInterrupts)
  def sharedMemoryTLEdge = sbus.busView

  protected def connectMasterPortsToSBus(tile: BaseTile, crossing: RocketCrossingParams) {
    def tileMasterBuffering: TLOutwardNode = tile {
      crossing.crossingType match {
        case _: AsynchronousCrossing => tile.masterNode
        case SynchronousCrossing(b) =>
          tile.masterNode
        case RationalCrossing(dir) =>
          require (dir != SlowToFast, "Misconfiguration? Core slower than fabric")
          tile.makeMasterBoundaryBuffers :=* tile.masterNode
      }
    }

    sbus.fromTile(tile.tileParams.name, crossing.master.buffers) {
        crossing.master.cork
          .map { u => TLCacheCork(unsafe = u) }
          .map { _ :=* tile.crossTLOut }
          .getOrElse { tile.crossTLOut }
    } :=* tileMasterBuffering
  }

  protected def connectSlavePortsToPBus(tile: BaseTile, crossing: RocketCrossingParams)(implicit valName: ValName) {
    def tileSlaveBuffering: TLInwardNode = tile {
      crossing.crossingType match {
        case RationalCrossing(_) => tile.slaveNode :*= tile.makeSlaveBoundaryBuffers
        case _ => tile.slaveNode
      }
    }

    DisableMonitors { implicit p =>
      tileSlaveBuffering :*= pbus.toTile(tile.tileParams.name) {
        crossing.slave.blockerCtrlAddr
          .map { BasicBusBlockerParams(_, pbus.beatBytes, sbus.beatBytes) }
          .map { bbbp => LazyModule(new BasicBusBlocker(bbbp)) }
          .map { bbb =>
            pbus.toVariableWidthSlave(Some("bus_blocker")) { bbb.controlNode }
            tile.crossTLIn :*= bbb.node
          } .getOrElse { tile.crossTLIn }
      }
    }
  }

  protected def connectInterrupts(tile: BaseTile, debugOpt: Option[TLDebugModule], clintOpt: Option[CLINT], plicOpt: Option[TLPLIC]) {
    // Handle all the different types of interrupts crossing to or from the tile:
    // 1. Debug interrupt is definitely asynchronous in all cases.
    // 2. The CLINT and PLIC output interrupts are synchronous to the periphery clock,
    //    so might need to be synchronized depending on the Tile's crossing type.
    // 3. Local Interrupts are required to already be synchronous to the tile clock.
    // 4. Interrupts coming out of the tile are sent to the PLIC,
    //    so might need to be synchronized depending on the Tile's crossing type.
    // NOTE: The order of calls to := matters! They must match how interrupts
    //       are decoded from tile.intNode inside the tile.

    // 1. always async crossing for debug
    debugOpt.foreach { debug =>
      tile.intInwardNode := tile { IntSyncCrossingSink(3) } := debug.intnode
    }

    // 2. clint+plic conditionally crossing
    val periphIntNode = tile.intInwardNode :=* tile.crossIntIn
    clintOpt.foreach { periphIntNode := _.intnode }    // msip+mtip
    plicOpt.foreach { plic =>
      periphIntNode := plic.intnode                    // meip
      if (tile.tileParams.core.useVM)
        periphIntNode := plic.intnode                  // seip
    }

    // 3. local interrupts  never cross
    // tile.intInwardNode is wired up externally       // lip

    // 4. conditional crossing from core to PLIC
    plicOpt.foreach { plic =>
      FlipRendering { implicit p =>
        plic.intnode :=* tile.crossIntOut :=* tile.intOutwardNode
      }
    }
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
}

