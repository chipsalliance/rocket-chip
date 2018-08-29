// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.devices.debug.{HasPeripheryDebug, HasPeripheryDebugModuleImp}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._

// TODO: how specific are these to RocketTiles?
case class TileMasterPortParams(buffers: Int = 0, cork: Option[Boolean] = None)
case class TileSlavePortParams(buffers: Int = 0, blockerCtrlAddr: Option[BigInt] = None)

case class RocketCrossingParams(
    crossingType: ClockCrossingType = SynchronousCrossing(),
    master: TileMasterPortParams = TileMasterPortParams(),
    slave: TileSlavePortParams = TileSlavePortParams()) {
  def knownRatio: Option[Int] = crossingType match {
    case RationalCrossing(_) => Some(2)
    case _ => None
  }
}

case object RocketTilesKey extends Field[Seq[RocketTileParams]](Nil)
case object RocketCrossingKey extends Field[Seq[RocketCrossingParams]](List(RocketCrossingParams()))

trait HasRocketTiles extends HasTiles
    with CanHavePeripheryPLIC
    with CanHavePeripheryCLINT
    with HasPeripheryDebug { this: BaseSubsystem =>
  val module: HasRocketTilesModuleImp

  protected val rocketTileParams = p(RocketTilesKey)
  private val crossings = perTileOrGlobalSetting(p(RocketCrossingKey), rocketTileParams.size)

  // Make a tile and wire its nodes into the system,
  // according to the specified type of clock crossing.
  // Note that we also inject new nodes into the tile itself,
  // also based on the crossing type.
  val rocketTiles = rocketTileParams.zip(crossings).map { case (tp, crossing) =>
    val rocket = LazyModule(new RocketTile(tp, crossing.crossingType)(augmentedTileParameters(tp)))
      .suggestName(tp.name)

    connectMasterPortsToSBus(rocket, crossing)
    connectSlavePortsToCBus(rocket, crossing)
    connectInterrupts(rocket, Some(debug), clintOpt, plicOpt)

    rocket
  }
}

trait HasRocketTilesModuleImp extends HasTilesModuleImp
    with HasPeripheryDebugModuleImp {
  val outer: HasRocketTiles
}

class RocketSubsystem(implicit p: Parameters) extends BaseSubsystem
    with HasRocketTiles {
  val tiles = rocketTiles
  override lazy val module = new RocketSubsystemModuleImp(this)
}

class RocketSubsystemModuleImp[+L <: RocketSubsystem](_outer: L) extends BaseSubsystemModuleImp(_outer)
    with HasRocketTilesModuleImp {
  tile_inputs.zip(outer.hartIdList).foreach { case(wire, i) =>
    wire.clock := clock
    wire.reset := reset
    wire.hartid := UInt(i)
    wire.reset_vector := global_reset_vector
  }
}
