// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci.{ResetCrossingType, NoResetCrossing}
import freechips.rocketchip.tile._

case class RocketCrossingParams(
  crossingType: ClockCrossingType = SynchronousCrossing(),
  master: TilePortParamsLike = TileMasterPortParams(),
  slave: TileSlavePortParams = TileSlavePortParams(),
  mmioBaseAddressPrefixWhere: TLBusWrapperLocation = CBUS,
  resetCrossingType: ResetCrossingType = NoResetCrossing()
) extends TileCrossingParamsLike {
  def forceSeparateClockReset: Boolean = false
}

case class RocketTileAttachParams(
  tileParams: RocketTileParams,
  crossingParams: RocketCrossingParams,
  lookup: LookupByHartIdImpl
) extends CanAttachTile { type TileType = RocketTile }

case object RocketTilesKey extends Field[Seq[RocketTileParams]](Nil)
case object RocketCrossingKey extends Field[Seq[RocketCrossingParams]](List(RocketCrossingParams()))

trait HasRocketTiles extends HasTiles { this: BaseSubsystem =>
  val rocketTiles = tiles.collect { case r: RocketTile => r }

  def coreMonitorBundles = (rocketTiles map { t =>
    t.module.core.rocketImpl.coreMonitorBundle
  }).toList
}

class RocketSubsystem(implicit p: Parameters) extends BaseSubsystem with HasRocketTiles {
  override lazy val module = new RocketSubsystemModuleImp(this)
}

class RocketSubsystemModuleImp[+L <: RocketSubsystem](_outer: L) extends BaseSubsystemModuleImp(_outer)
    with HasTilesModuleImp
