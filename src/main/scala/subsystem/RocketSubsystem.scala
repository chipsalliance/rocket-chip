// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci.{ResetCrossingType, NoResetCrossing}
import freechips.rocketchip.tile._
import freechips.rocketchip.devices.debug.{HasPeripheryDebug, HasPeripheryDebugModuleImp}
import freechips.rocketchip.devices.tilelink.{CanHavePeripheryCLINT, CanHavePeripheryPLIC}

case class RocketCrossingParams(
  crossingType: ClockCrossingType = SynchronousCrossing(),
  master: ElementPortParamsLike = ElementMasterPortParams(),
  slave: ElementSlavePortParams = ElementSlavePortParams(),
  mmioBaseAddressPrefixWhere: TLBusWrapperLocation = CBUS,
  resetCrossingType: ResetCrossingType = NoResetCrossing(),
  forceSeparateClockReset: Boolean = false
) extends ElementCrossingParamsLike

case class RocketTileAttachParams(
  tileParams: RocketTileParams,
  crossingParams: RocketCrossingParams
) extends CanAttachTile { type TileType = RocketTile }

trait HasRocketTiles extends HasTiles { this: BaseSubsystem =>
  val rocketTiles = totalTiles.collect { case r: RocketTile => r }

  def coreMonitorBundles = (rocketTiles map { t =>
    t.module.core.rocketImpl.coreMonitorBundle
  }).toList
}

class RocketSubsystem(implicit p: Parameters) extends BaseSubsystem
    with HasPeripheryDebug
    with CanHavePeripheryCLINT
    with CanHavePeripheryPLIC
    with HasRocketTiles
{
  override lazy val module = new RocketSubsystemModuleImp(this)
}

class RocketSubsystemModuleImp[+L <: RocketSubsystem](_outer: L) extends BaseSubsystemModuleImp(_outer)
    with HasPeripheryDebugModuleImp
    with HasTilesRootModuleImp

