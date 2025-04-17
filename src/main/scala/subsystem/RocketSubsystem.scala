// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import org.chipsalliance.cde.config._

import freechips.rocketchip.devices.debug.HasPeripheryDebug
import freechips.rocketchip.devices.tilelink.{CanHavePeripheryCLINT, CanHavePeripheryPLIC}
import freechips.rocketchip.prci.{ResetCrossingType, NoResetCrossing, SynchronousCrossing, ClockCrossingType}
import freechips.rocketchip.tile.{RocketTile, RocketTileParams}
import freechips.rocketchip.util.HasCoreMonitorBundles

case class RocketCrossingParams(
  crossingType: ClockCrossingType = SynchronousCrossing(),
  master: HierarchicalElementPortParamsLike = HierarchicalElementMasterPortParams(),
  slave: HierarchicalElementSlavePortParams = HierarchicalElementSlavePortParams(),
  mmioBaseAddressPrefixWhere: TLBusWrapperLocation = CBUS,
  resetCrossingType: ResetCrossingType = NoResetCrossing(),
  forceSeparateClockReset: Boolean = false,
  forceMergedCreditedTLCrossings: Boolean = false
) extends HierarchicalElementCrossingParamsLike

case class RocketTileAttachParams(
  tileParams: RocketTileParams,
  crossingParams: RocketCrossingParams
) extends CanAttachTile { type TileType = RocketTile }

trait HasRocketTiles {
  this: BaseSubsystem with InstantiatesHierarchicalElements =>
  val rocketTiles = totalTiles.values.collect { case r: RocketTile => r }

  def coreMonitorBundles = (rocketTiles map { t =>
    t.module.core.rocketImpl.coreMonitorBundle
  }).toList
}

class RocketSubsystem(implicit p: Parameters) extends BaseSubsystem
    with InstantiatesHierarchicalElements
    with HasTileNotificationSinks
    with HasTileInputConstants
    with CanHavePeripheryCLINT
    with CanHavePeripheryPLIC
    with HasPeripheryDebug
    with HasHierarchicalElementsRootContext
    with HasHierarchicalElements
    with HasCoreMonitorBundles
    with HasRocketTiles
{
  override lazy val module = new RocketSubsystemModuleImp(this)
}

class RocketSubsystemModuleImp[+L <: RocketSubsystem](_outer: L) extends BaseSubsystemModuleImp(_outer)
    with HasHierarchicalElementsRootContextModuleImp {
  override lazy val outer = _outer
}

