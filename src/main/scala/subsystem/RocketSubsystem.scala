// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.devices.debug.{HasPeripheryDebug, HasPeripheryDebugModuleImp}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomaticobjectmodel.logicaltree._
import freechips.rocketchip.diplomaticobjectmodel.model._
import freechips.rocketchip.tile._

case object HartPrefixKey extends Field[Boolean](false)

case class RocketCrossingParams(
  crossingType: ClockCrossingType = SynchronousCrossing(),
  master: TileMasterPortParams = TileMasterPortParams(),
  slave: TileSlavePortParams = TileSlavePortParams()
) extends TileCrossingParamsLike

case class RocketTileAttachParams(
  tileParams: RocketTileParams,
  crossingParams: RocketCrossingParams,
  lookup: LookupByHartIdImpl
) extends CanAttachTile {
  type TileType = RocketTile
  type TileContextType = DefaultTileContextType
}

case object RocketTilesKey extends Field[Seq[RocketTileParams]](Nil)
case object RocketCrossingKey extends Field[Seq[RocketCrossingParams]](List(RocketCrossingParams()))

trait HasRocketTiles extends HasTiles { this: BaseSubsystem =>
  val rocketTiles = tiles.collect { case r: RocketTile => r }

  def coreMonitorBundles = (rocketTiles map { t =>
    t.module.core.rocketImpl.coreMonitorBundle
  }).toList
}

// Field for specifying MaskROM addition to subsystem
case object PeripheryMaskROMKey extends Field[Seq[MaskROMParams]](Nil)

class RocketSubsystem(implicit p: Parameters) extends BaseSubsystem
    with HasRocketTiles {
        
  // add Mask ROM devices
  val maskROMs = p(PeripheryMaskROMKey).map { MaskROM.attach(_, cbus) }

  val hartPrefixNode = if (p(HartPrefixKey)) {
    Some(BundleBroadcast[UInt](registered = true))
  } else {
    None
  }

  val hartPrefixes = hartPrefixNode.map { hpn => Seq.fill(tiles.size) {
   val hps = BundleBridgeSink[UInt]()
   hps := hpn
   hps
  } }.getOrElse(Nil)

  override lazy val module = new RocketSubsystemModuleImp(this)
}

class RocketSubsystemModuleImp[+L <: RocketSubsystem](_outer: L) extends BaseSubsystemModuleImp(_outer)
    with HasResetVectorWire
    with HasTilesModuleImp {

  for (i <- 0 until outer.tiles.size) {
    val wire = tile_inputs(i)
    val prefix = outer.hartPrefixes.lift(i).map(_.bundle).getOrElse(UInt(0))
    wire.hartid := prefix | UInt(outer.hartIdList(i))
    wire.reset_vector := global_reset_vector
  }
}
