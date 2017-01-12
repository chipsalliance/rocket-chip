// See LICENSE.SiFive for license details.

package rocket

import Chisel._
import config._
import uncore.tilelink2.TLEdgeOut
import uncore.util.{CacheName, CacheBlockBytes}
import util._

case object BuildCore extends Field[(RocketConfig, Parameters) => CoreModule with HasCoreIO]
case object SharedMemoryTLEdge extends Field[TLEdgeOut]

trait HasCoreParameters {
  implicit val p: Parameters
  val xLen = p(XLen)
  val fLen = xLen // TODO relax this

  val usingVM = p(UseVM)
  val usingUser = p(UseUser) || usingVM
  val usingDebug = p(UseDebug)
  val usingMulDiv = p(MulDivKey).nonEmpty
  val usingFPU = p(FPUKey).nonEmpty
  val usingAtomics = p(UseAtomics)
  val usingCompressed = p(UseCompressed)
  val usingRoCC = !p(BuildRoCC).isEmpty
  val fastLoadWord = p(FastLoadWord)
  val fastLoadByte = p(FastLoadByte)
  val fastJAL = p(FastJAL)
  val nBreakpoints = p(NBreakpoints)
  val nPerfCounters = p(NPerfCounters)
  val nPerfEvents = p(NPerfEvents)
  val usingDataScratchpad = p(DataScratchpadSize) > 0

  val retireWidth = p(RetireWidth)
  val fetchWidth = p(FetchWidth)
  val coreInstBits = p(CoreInstBits)
  val coreInstBytes = coreInstBits/8
  val coreDataBits = xLen
  val coreDataBytes = coreDataBits/8

  val dcacheArbPorts = 1 + usingVM.toInt + usingDataScratchpad.toInt + p(BuildRoCC).size
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
  val nCustomMrwCsrs = p(NCustomMRWCSRs)

  // fetchWidth doubled, but coreInstBytes halved, for RVC
  val decodeWidth = fetchWidth / (if (usingCompressed) 2 else 1)

  // Print out log of committed instructions and their writeback values.
  // Requires post-processing due to out-of-order writebacks.
  val enableCommitLog = false

  val maxPAddrBits = xLen match {
    case 32 => 34
    case 64 => 50
  }

  require(paddrBits <= maxPAddrBits)
  require(!fastLoadByte || fastLoadWord)
}

abstract class CoreModule(implicit val p: Parameters) extends Module
  with HasCoreParameters

abstract class CoreBundle(implicit val p: Parameters) extends ParameterizedBundle()(p)
  with HasCoreParameters

trait HasCoreIO {
  implicit val p: Parameters
  val io = new Bundle {
    val interrupts = new TileInterrupts().asInput
    val hartid = UInt(INPUT, p(XLen))
    val imem  = new FrontendIO()(p.alterPartial({case CacheName => CacheName("L1I") }))
    val dmem = new HellaCacheIO()(p.alterPartial({ case CacheName => CacheName("L1D") }))
    val ptw = new DatapathPTWIO().flip
    val fpu = new FPUCoreIO().flip
    val rocc = new RoCCCoreIO().flip
  }
}

