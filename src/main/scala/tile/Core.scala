// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile

import Chisel._

import freechips.rocketchip.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._

case object BuildCore extends Field[Parameters => CoreModule with HasCoreIO]
case object XLen extends Field[Int]

// These parameters can be varied per-core
trait CoreParams {
  val useVM: Boolean
  val useUser: Boolean
  val useDebug: Boolean
  val useAtomics: Boolean
  val useCompressed: Boolean
  val mulDiv: Option[MulDivParams]
  val fpu: Option[FPUParams]
  val fetchWidth: Int
  val decodeWidth: Int
  val retireWidth: Int
  val instBits: Int
  val nLocalInterrupts: Int
  val nL2TLBEntries: Int
  val jumpInFrontend: Boolean

  def instBytes: Int = instBits / 8
  def fetchBytes: Int = fetchWidth * instBytes
}

trait HasCoreParameters extends HasTileParameters {
  val coreParams: CoreParams = tileParams.core

  val xLen = p(XLen)
  val fLen = xLen // TODO relax this
  require(xLen == 32 || xLen == 64)

  val usingMulDiv = coreParams.mulDiv.nonEmpty
  val usingFPU = coreParams.fpu.nonEmpty
  val usingAtomics = coreParams.useAtomics
  val usingCompressed = coreParams.useCompressed

  val retireWidth = coreParams.retireWidth
  val fetchWidth = coreParams.fetchWidth
  val decodeWidth = coreParams.decodeWidth

  val coreInstBits = coreParams.instBits
  val coreInstBytes = coreInstBits/8
  val coreDataBits = xLen max fLen
  val coreDataBytes = coreDataBits/8

  val coreDCacheReqTagBits = 6
  val dcacheReqTagBits = coreDCacheReqTagBits + log2Ceil(dcacheArbPorts)

  def pgIdxBits = 12
  def pgLevelBits = 10 - log2Ceil(xLen / 32)
  def vaddrBits = pgIdxBits + pgLevels * pgLevelBits
  def paddrBits: Int = p(SharedMemoryTLEdge).bundle.addressBits
  def ppnBits = paddrBits - pgIdxBits
  def vpnBits = vaddrBits - pgIdxBits
  val pgLevels = p(PgLevels)
  val asIdBits = p(ASIdBits)
  val vpnBitsExtended = vpnBits + (vaddrBits < xLen).toInt
  val vaddrBitsExtended = vpnBitsExtended + pgIdxBits
  def coreMaxAddrBits = paddrBits max vaddrBitsExtended
  val maxPAddrBits = xLen match { case 32 => 34; case 64 => 56 }
  require(paddrBits <= maxPAddrBits)

  // Print out log of committed instructions and their writeback values.
  // Requires post-processing due to out-of-order writebacks.
  val enableCommitLog = false
}

abstract class CoreModule(implicit val p: Parameters) extends Module
  with HasCoreParameters

abstract class CoreBundle(implicit val p: Parameters) extends ParameterizedBundle()(p)
  with HasCoreParameters

trait HasCoreIO extends HasTileParameters {
  implicit val p: Parameters
  val io = new CoreBundle()(p) with HasExternallyDrivenTileConstants {
    val interrupts = new TileInterrupts().asInput
    val imem  = new FrontendIO
    val dmem = new HellaCacheIO
    val ptw = new DatapathPTWIO().flip
    val fpu = new FPUCoreIO().flip
    val rocc = new RoCCCoreIO().flip
  }
}
