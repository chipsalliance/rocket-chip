// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile

import Chisel._

import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

case object SharedMemoryTLEdge extends Field[TLEdgeOut]
case object TileKey extends Field[TileParams]
case object ResetVectorBits extends Field[Int]
case object MaxHartIdBits extends Field[Int]

trait TileParams {
  val core: CoreParams
  val icache: Option[ICacheParams]
  val dcache: Option[DCacheParams]
  val rocc: Seq[RoCCParams]
  val btb: Option[BTBParams]
  val trace: Boolean
}

trait HasTileParameters {
  implicit val p: Parameters
  def tileParams: TileParams = p(TileKey)

  def usingVM: Boolean = tileParams.core.useVM
  def usingUser: Boolean = tileParams.core.useUser || usingVM
  def usingDebug: Boolean = tileParams.core.useDebug
  def usingRoCC: Boolean = !tileParams.rocc.isEmpty
  def usingBTB: Boolean = tileParams.btb.isDefined && tileParams.btb.get.nEntries > 0
  def usingPTW: Boolean = usingVM
  def usingDataScratchpad: Boolean = tileParams.dcache.flatMap(_.scratch).isDefined

  def xLen: Int = p(XLen)
  def xBytes: Int = xLen / 8
  def iLen: Int = 32
  def pgIdxBits: Int = 12
  def pgLevelBits: Int = 10 - log2Ceil(xLen / 32)
  def vaddrBits: Int = pgIdxBits + pgLevels * pgLevelBits
  def paddrBits: Int = p(SharedMemoryTLEdge).bundle.addressBits
  def vpnBits: Int = vaddrBits - pgIdxBits
  def ppnBits: Int = paddrBits - pgIdxBits
  def pgLevels: Int = p(PgLevels)
  def asIdBits: Int = p(ASIdBits)
  def vpnBitsExtended: Int = vpnBits + (vaddrBits < xLen).toInt
  def vaddrBitsExtended: Int = vpnBitsExtended + pgIdxBits
  def maxPAddrBits: Int = xLen match { case 32 => 34; case 64 => 56 }

  def hartIdLen: Int = p(MaxHartIdBits)
  def resetVectorLen: Int = paddrBits min vaddrBitsExtended

  def dcacheArbPorts = 1 + usingVM.toInt + usingDataScratchpad.toInt + tileParams.rocc.size
}

abstract class BareTile(implicit p: Parameters) extends LazyModule

abstract class BareTileBundle[+L <: BareTile](_outer: L) extends GenericParameterizedBundle(_outer) {
  val outer = _outer
  implicit val p = outer.p
}

abstract class BareTileModule[+L <: BareTile, +B <: BareTileBundle[L]](_outer: L, _io: () => B) extends LazyModuleImp(_outer) {
  val outer = _outer
  val io = IO(_io ())
}

/** Uses TileLink master port to connect caches and accelerators to the coreplex */
trait HasTileLinkMasterPort {
  implicit val p: Parameters
  val module: HasTileLinkMasterPortModule
  val masterNode = TLIdentityNode()
  val tileBus = LazyModule(new TLXbar) // TileBus xbar for cache backends to connect to
  masterNode := tileBus.node
}

trait HasTileLinkMasterPortBundle {
  val outer: HasTileLinkMasterPort
}

trait HasTileLinkMasterPortModule {
  val outer: HasTileLinkMasterPort
  val io: HasTileLinkMasterPortBundle
}

/** Some other standard inputs */
trait HasExternallyDrivenTileConstants extends Bundle with HasTileParameters {
  val hartid = UInt(INPUT, hartIdLen)
  val reset_vector = UInt(INPUT, resetVectorLen)
}

trait CanHaveInstructionTracePort extends Bundle with HasTileParameters {
  val trace = tileParams.trace.option(Vec(tileParams.core.retireWidth, new TracedInstruction).asOutput)
}

/** Base class for all Tiles that use TileLink */
abstract class BaseTile(tileParams: TileParams)(implicit p: Parameters) extends BareTile
    with HasTileParameters
    with HasTileLinkMasterPort {
  override lazy val module = new BaseTileModule(this, () => new BaseTileBundle(this))
}

class BaseTileBundle[+L <: BaseTile](_outer: L) extends BareTileBundle(_outer)
    with HasTileLinkMasterPortBundle
    with HasExternallyDrivenTileConstants
    with CanHaveInstructionTracePort

class BaseTileModule[+L <: BaseTile, +B <: BaseTileBundle[L]](_outer: L, _io: () => B) extends BareTileModule(_outer, _io)
    with HasTileParameters
    with HasTileLinkMasterPortModule {
  require(xLen == 32 || xLen == 64)
  require(paddrBits <= maxPAddrBits)
  require(resetVectorLen <= xLen)
  require(resetVectorLen <= vaddrBitsExtended)
}
