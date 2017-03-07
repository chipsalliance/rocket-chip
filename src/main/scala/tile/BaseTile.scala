// See LICENSE.SiFive for license details.

package tile

import Chisel._
import config._
import diplomacy._
import rocket._
import uncore.tilelink2._
import util._

case object SharedMemoryTLEdge extends Field[TLEdgeOut]
case object TileKey extends Field[TileParams]

trait TileParams {
  val core: CoreParams
  val icache: Option[ICacheParams]
  val dcache: Option[DCacheParams]
  val rocc: Seq[RoCCParams]
  val btb: Option[BTBParams]
}

trait HasTileParameters {
  implicit val p: Parameters
  val tileParams: TileParams = p(TileKey)

  val usingVM = tileParams.core.useVM
  val usingUser = tileParams.core.useUser || usingVM
  val usingDebug = tileParams.core.useDebug
  val usingRoCC = !tileParams.rocc.isEmpty
  val usingBTB = tileParams.btb.isDefined && tileParams.btb.get.nEntries > 0
  val usingPTW = usingVM
  val usingDataScratchpad = tileParams.dcache.flatMap(_.scratch).isDefined

  def dcacheArbPorts = 1 + usingVM.toInt + usingDataScratchpad.toInt + tileParams.rocc.size
}

abstract class BareTile(implicit p: Parameters) extends LazyModule

abstract class BareTileBundle[+L <: BareTile](_outer: L) extends GenericParameterizedBundle(_outer) {
  val outer = _outer
  implicit val p = outer.p
}

abstract class BareTileModule[+L <: BareTile, +B <: BareTileBundle[L]](_outer: L, _io: () => B) extends LazyModuleImp(_outer) {
  val outer = _outer
  val io = _io ()
}

// Uses TileLink master port to connect caches and accelerators to the coreplex
trait HasTileLinkMasterPort extends HasTileParameters {
  implicit val p: Parameters
  val module: HasTileLinkMasterPortModule
  val masterNode = TLOutputNode()
  val intNode = IntSinkNode(IntSinkPortSimple())
}

trait HasTileLinkMasterPortBundle {
  val outer: HasTileLinkMasterPort
  val master = outer.masterNode.bundleOut
  val interrupts = outer.intNode.bundleIn
}

trait HasTileLinkMasterPortModule {
  val outer: HasTileLinkMasterPort
  val io: HasTileLinkMasterPortBundle
}

abstract class BaseTile(tileParams: TileParams)(implicit p: Parameters) extends BareTile
    with HasTileLinkMasterPort {
  override lazy val module = new BaseTileModule(this, () => new BaseTileBundle(this))
}

class BaseTileBundle[+L <: BaseTile](_outer: L) extends BareTileBundle(_outer)
    with HasTileLinkMasterPortBundle {
  val hartid = UInt(INPUT, p(XLen))
  val resetVector = UInt(INPUT, p(XLen))
}

class BaseTileModule[+L <: BaseTile, +B <: BaseTileBundle[L]](_outer: L, _io: () => B) extends BareTileModule(_outer, _io)
    with HasTileLinkMasterPortModule
