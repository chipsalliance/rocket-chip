// See LICENSE.SiFive for license details.

package rocket

import Chisel._
import config._
import uncore.tilelink2.TLEdgeOut
import uncore.util.{CacheName, CacheBlockBytes}
import util._

case object BuildCore extends Field[(RocketConfig, Parameters) => CoreModule with HasCoreIO]

// These parameters apply to all cores, for now
case object XLen extends Field[Int]
case object UseVM extends Field[Boolean]
case object UseUser extends Field[Boolean]
case object UseDebug extends Field[Boolean]

// These parameters can be varied per-core
trait CoreConfig {
  useAtomics: Bool
  useCompressed: Bool
  fpuConfig: Option[FPUConfig]
  mulDivConfig: Option[MulDivConfig]
  fetchWidth: Int
  decodeWidth: Int
  retireWidth: Int
  instBits: Int
}

trait HasCoreParameters {
  implicit val p: Parameters
  implicit val tileConfig: TileConfig = p(TileKey)
  implicit val coreConfig = tileConfig.core

  val xLen = p(XLen)
  val fLen = xLen // TODO relax this
  require(xLen == 32 || xLen == 64)

  val usingVM = p(UseVM)
  val usingUser = p(UseUser) || usingVM
  val usingDebug = p(UseDebug)

  val usingMulDiv = coreConfig.mulDivConfig.nonEmpty
  val usingFPU = coreConfig.fpuConfig.nonEmpty
  val usingAtomics = coreConfig.useAtomics
  val usingCompressed = coreConfig.useCompressed

  val retireWidth = coreConfig.retireWidth
  val fetchWidth = coreConfig.fetchWidth
  val decodeWidth = coreConfig.decodeWidth

  val coreInstBits = coreConfig.instBits
  val coreInstBytes = coreInstBits/8
  val coreDataBits = xLen
  val coreDataBytes = coreDataBits/8

  val dcacheArbPorts = 1 + usingVM.toInt + (p(DataScratchpadSize) > 0).toInt + p(BuildRoCC).size
  val coreDCacheReqTagBits = 6
  val dcacheReqTagBits = coreDCacheReqTagBits + log2Ceil(dcacheArbPorts)

  def pgIdxBits = 12
  def pgLevelBits = 10 - log2Ceil(xLen / 32)
  def vaddrBits = pgIdxBits + pgLevels * pgLevelBits
  val paddrBits = p(PAddrBits)
  def ppnBits = paddrBits - pgIdxBits
  def vpnBits = vaddrBits - pgIdxBits
  val pgLevels = p(PgLevels)
  val asIdBits = p(ASIdBits)
  val vpnBitsExtended = vpnBits + (vaddrBits < xLen).toInt
  val vaddrBitsExtended = vpnBitsExtended + pgIdxBits
  val coreMaxAddrBits = paddrBits max vaddrBitsExtended
  val maxPAddrBits = xLen match { case 32 => 34; case 64 => 50 }
  require(paddrBits <= maxPAddrBits)

  // Print out log of committed instructions and their writeback values.
  // Requires post-processing due to out-of-order writebacks.
  val enableCommitLog = false
}

abstract class CoreModule(implicit val coreConfig: CoreConfig, val p: Parameters) extends Module
  with HasCoreParameters

abstract class CoreBundle(implicit val p: Parameters) extends ParameterizedBundle()(p)
  with HasCoreParameters

trait HasCoreIO {
  implicit val p: Parameters
  implicit val t: TileConfig 
  val io = new Bundle {
    val interrupts = new TileInterrupts().asInput
    val hartid = UInt(INPUT, p(XLen))
    val imem  = new FrontendIO()(p)
    val dmem = new HellaCacheIO()(p)
    val ptw = new DatapathPTWIO().flip
    val fpu = new FPUCoreIO().flip
    val rocc = new RoCCCoreIO().flip
  }
}
