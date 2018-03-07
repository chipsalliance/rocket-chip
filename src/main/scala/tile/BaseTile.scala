// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile

import Chisel._

import freechips.rocketchip.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
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
  val hartId: Int
  val blockerCtrlAddr: Option[BigInt]
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
  def vaddrBits: Int =
    if (usingVM) {
      val v = pgIdxBits + pgLevels * pgLevelBits
      require(v == xLen || xLen > v && v > paddrBits)
      v
    } else {
      // since virtual addresses sign-extend but physical addresses
      // zero-extend, make room for a zero sign bit for physical addresses
      (paddrBits + 1) min xLen
    }
  def paddrBits: Int = p(SharedMemoryTLEdge).bundle.addressBits
  def vpnBits: Int = vaddrBits - pgIdxBits
  def ppnBits: Int = paddrBits - pgIdxBits
  def pgLevels: Int = p(PgLevels)
  def asIdBits: Int = p(ASIdBits)
  def vpnBitsExtended: Int = vpnBits + (vaddrBits < xLen).toInt
  def vaddrBitsExtended: Int = vpnBitsExtended + pgIdxBits
  def maxPAddrBits: Int = xLen match { case 32 => 34; case 64 => 56 }

  def hartId: Int = tileParams.hartId
  def hartIdLen: Int = p(MaxHartIdBits)
  def resetVectorLen: Int = paddrBits

  def cacheBlockBytes = p(CacheBlockBytes)
  def lgCacheBlockBytes = log2Up(cacheBlockBytes)
  def masterPortBeatBytes = p(SystemBusKey).beatBytes

  def dcacheArbPorts = 1 + usingVM.toInt + usingDataScratchpad.toInt + tileParams.rocc.size

  // TODO merge with isaString in CSR.scala
  def isaDTS: String = {
    val m = if (tileParams.core.mulDiv.nonEmpty) "m" else ""
    val a = if (tileParams.core.useAtomics) "a" else ""
    val f = if (tileParams.core.fpu.nonEmpty) "f" else ""
    val d = if (tileParams.core.fpu.nonEmpty && p(XLen) > 32) "d" else ""
    val c = if (tileParams.core.useCompressed) "c" else ""
    s"rv${p(XLen)}i$m$a$f$d$c"
  }

  def tileProperties: PropertyMap = {
    val dcache = tileParams.dcache.filter(!_.scratch.isDefined).map(d => Map(
      "d-cache-block-size"   -> cacheBlockBytes.asProperty,
      "d-cache-sets"         -> d.nSets.asProperty,
      "d-cache-size"         -> (d.nSets * d.nWays * cacheBlockBytes).asProperty)
    ).getOrElse(Nil)

    val incoherent = if (!tileParams.core.useAtomicsOnlyForIO) Nil else Map(
      "sifive,d-cache-incoherent" -> Nil)

    val icache = tileParams.icache.map(i => Map(
      "i-cache-block-size"   -> cacheBlockBytes.asProperty,
      "i-cache-sets"         -> i.nSets.asProperty,
      "i-cache-size"         -> (i.nSets * i.nWays * cacheBlockBytes).asProperty)
    ).getOrElse(Nil)

    val dtlb = tileParams.dcache.filter(_ => tileParams.core.useVM).map(d => Map(
      "d-tlb-size"           -> d.nTLBEntries.asProperty,
      "d-tlb-sets"           -> 1.asProperty)).getOrElse(Nil)

    val itlb = tileParams.icache.filter(_ => tileParams.core.useVM).map(i => Map(
      "i-tlb-size"           -> i.nTLBEntries.asProperty,
      "i-tlb-sets"           -> 1.asProperty)).getOrElse(Nil)

    val mmu = if (!tileParams.core.useVM) Nil else Map(
        "tlb-split" -> Nil,
        "mmu-type"  -> (p(PgLevels) match {
          case 2 => "riscv,sv32"
          case 3 => "riscv,sv39"
          case 4 => "riscv,sv48"
        }).asProperty)

    dcache ++ icache ++ dtlb ++ itlb ++ mmu ++ incoherent
  }

}

/** Base class for all Tiles that use TileLink */
abstract class BaseTile(tileParams: TileParams, val crossing: SubsystemClockCrossing)
                       (implicit p: Parameters) extends LazyModule with HasTileParameters with HasCrossing
{
  def module: BaseTileModuleImp[BaseTile]
  def masterNode: TLOutwardNode
  def slaveNode: TLInwardNode
  def intInwardNode: IntInwardNode
  def intOutwardNode: IntOutwardNode

  protected val tlOtherMastersNode = TLIdentityNode()
  protected val tlMasterXbar = LazyModule(new TLXbar)
  protected val tlSlaveXbar = LazyModule(new TLXbar)
  protected val intXbar = LazyModule(new IntXbar)

  def connectTLSlave(node: TLNode, bytes: Int) {
    DisableMonitors { implicit p =>
      (Seq(node, TLFragmenter(bytes, cacheBlockBytes, earlyAck=EarlyAck.PutFulls))
        ++ (xBytes != bytes).option(TLWidthWidget(xBytes)))
        .foldRight(tlSlaveXbar.node:TLOutwardNode)(_ :*= _)
    }
  }

  // Find resource labels for all the outward caches
  def nextLevelCacheProperty: PropertyOption = {
    val outer = tlMasterXbar.node.edges.out
      .flatMap(_.manager.managers)
      .filter(_.supportsAcquireB)
      .flatMap(_.resources.headOption)
      .map(_.owner.label)
      .distinct
    if (outer.isEmpty) None
    else Some("next-level-cache" -> outer.map(l => ResourceReference(l)).toList)
  }

  def toDescription(resources: ResourceBindings)(compat: String, extraProperties: PropertyMap = Nil): Description = {
    val cpuProperties: PropertyMap = Map(
        "reg"                  -> resources("reg").map(_.value),
        "device_type"          -> "cpu".asProperty,
        "compatible"           -> Seq(ResourceString(compat), ResourceString("riscv")),
        "status"               -> "okay".asProperty,
        "clock-frequency"      -> tileParams.core.bootFreqHz.asProperty,
        "riscv,isa"            -> isaDTS.asProperty,
        "timebase-frequency"   -> p(DTSTimebase).asProperty)

    Description(s"cpus/cpu@${hartId}", (cpuProperties ++ nextLevelCacheProperty ++ tileProperties ++ extraProperties).toMap)
  }
}

class BaseTileModuleImp[+L <: BaseTile](val outer: L) extends LazyModuleImp(outer) with HasTileParameters {

  require(xLen == 32 || xLen == 64)
  require(paddrBits <= maxPAddrBits)
  require(resetVectorLen <= xLen)
  require(resetVectorLen <= vaddrBitsExtended)
  require (log2Up(hartId + 1) <= hartIdLen, s"p(MaxHartIdBits) of $hartIdLen is not enough for hartid $hartId")

  val trace = tileParams.trace.option(IO(Vec(tileParams.core.retireWidth, new TracedInstruction).asOutput))
  val constants = IO(new TileInputConstants)

  val fpuOpt = outer.tileParams.core.fpu.map(params => Module(new FPU(params)(outer.p)))
}

/** Some other non-tilelink but still standard inputs */
trait HasExternallyDrivenTileConstants extends Bundle with HasTileParameters {
  val hartid = UInt(INPUT, hartIdLen)
  val reset_vector = UInt(INPUT, resetVectorLen)
}

class TileInputConstants(implicit val p: Parameters) extends ParameterizedBundle with HasExternallyDrivenTileConstants
