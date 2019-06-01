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

case object TileVisibilityNodeKey extends Field[TLEphemeralNode]
case object TileKey extends Field[TileParams]
case object ResetVectorBits extends Field[Int]
case object MaxHartIdBits extends Field[Int]
case object LookupByHartId extends Field[LookupByHartIdImpl]

trait TileParams {
  val core: CoreParams
  val icache: Option[ICacheParams]
  val dcache: Option[DCacheParams]
  val btb: Option[BTBParams]
  val hartId: Int
  val beuAddr: Option[BigInt]
  val blockerCtrlAddr: Option[BigInt]
  val name: Option[String]
}

/** These parameters values are not computed based on diplomacy negotiation
  * and so are safe to use while diplomacy itself is running.
  */
trait HasNonDiplomaticTileParameters {
  implicit val p: Parameters
  def tileParams: TileParams = p(TileKey)

  def usingVM: Boolean = tileParams.core.useVM
  def usingUser: Boolean = tileParams.core.useUser || usingVM
  def usingDebug: Boolean = tileParams.core.useDebug
  def usingRoCC: Boolean = !p(BuildRoCC).isEmpty
  def usingBTB: Boolean = tileParams.btb.isDefined && tileParams.btb.get.nEntries > 0
  def usingPTW: Boolean = usingVM
  def usingDataScratchpad: Boolean = tileParams.dcache.flatMap(_.scratch).isDefined

  def xLen: Int = p(XLen)
  def xBytes: Int = xLen / 8
  def iLen: Int = 32
  def pgIdxBits: Int = 12
  def pgLevelBits: Int = 10 - log2Ceil(xLen / 32)
  def pgLevels: Int = p(PgLevels)
  def maxSVAddrBits: Int = pgIdxBits + pgLevels * pgLevelBits
  def minPgLevels: Int = {
    val res = xLen match { case 32 => 2; case 64 => 3 }
    require(pgLevels >= res)
    res
  }
  def asIdBits: Int = p(ASIdBits)
  def maxPAddrBits: Int = xLen match { case 32 => 34; case 64 => 56 }

  def hartId: Int = tileParams.hartId
  def hartIdLen: Int = p(MaxHartIdBits)

  def cacheBlockBytes = p(CacheBlockBytes)
  def lgCacheBlockBytes = log2Up(cacheBlockBytes)
  def masterPortBeatBytes = p(SystemBusKey).beatBytes

  // TODO make HellaCacheIO diplomatic and remove this brittle collection of hacks
  //                  Core   PTW                DTIM                    coprocessors           
  def dcacheArbPorts = 1 + usingVM.toInt + usingDataScratchpad.toInt + p(BuildRoCC).size

  // TODO merge with isaString in CSR.scala
  def isaDTS: String = {
    val ie = if (tileParams.core.useRVE) "e" else "i"
    val m = if (tileParams.core.mulDiv.nonEmpty) "m" else ""
    val a = if (tileParams.core.useAtomics) "a" else ""
    val f = if (tileParams.core.fpu.nonEmpty) "f" else ""
    val d = if (tileParams.core.fpu.nonEmpty && tileParams.core.fpu.get.fLen > 32) "d" else ""
    val c = if (tileParams.core.useCompressed) "c" else ""
    s"rv${p(XLen)}$ie$m$a$f$d$c"
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
        "mmu-type"  -> s"riscv,sv$maxSVAddrBits".asProperty)

    val pmp = if (tileParams.core.nPMPs > 0) Map("riscv,pmpregions" -> tileParams.core.nPMPs.asProperty) else Nil

    dcache ++ icache ++ dtlb ++ itlb ++ mmu ++ pmp ++ incoherent
  }

}

/** These parameters values are computed based on diplomacy negotiations
  * and so are NOT safe to use while diplomacy itself is running.
  * Only mix this trait into LazyModuleImps, Modules, Bundles, Data, etc.
  */
trait HasTileParameters extends HasNonDiplomaticTileParameters {
  protected def tlBundleParams = p(TileVisibilityNodeKey).edges.out.head.bundle
  def paddrBits: Int = tlBundleParams.addressBits
  def vaddrBits: Int =
    if (usingVM) {
      val v = maxSVAddrBits
      require(v == xLen || xLen > v && v > paddrBits)
      v
    } else {
      // since virtual addresses sign-extend but physical addresses
      // zero-extend, make room for a zero sign bit for physical addresses
      (paddrBits + 1) min xLen
    }
  def vpnBits: Int = vaddrBits - pgIdxBits
  def ppnBits: Int = paddrBits - pgIdxBits
  def vpnBitsExtended: Int = vpnBits + (vaddrBits < xLen).toInt
  def vaddrBitsExtended: Int = vpnBitsExtended + pgIdxBits

  def resetVectorLen: Int = paddrBits
}

/** Base class for all Tiles that use TileLink */
abstract class BaseTile private (val crossing: ClockCrossingType, q: Parameters)
    extends LazyModule()(q)
    with CrossesToOnlyOneClockDomain
    with HasNonDiplomaticTileParameters
{
  // Public constructor alters Parameters to supply some legacy compatibility keys
  def this(tileParams: TileParams, crossing: ClockCrossingType, lookup: LookupByHartIdImpl, p: Parameters) = {
    this(crossing, p.alterMap(Map(
      TileKey -> tileParams,
      TileVisibilityNodeKey -> TLEphemeralNode()(ValName("tile_master")),
      LookupByHartId -> lookup
    )))
  }

  def module: BaseTileModuleImp[BaseTile]
  def masterNode: TLOutwardNode
  def slaveNode: TLInwardNode
  def intInwardNode: IntInwardNode    // Interrupts to the core from external devices
  def intOutwardNode: IntOutwardNode  // Interrupts from tile-internal devices (e.g. BEU)
  def haltNode: IntOutwardNode        // Unrecoverable error has occurred; suggest reset
  def ceaseNode: IntOutwardNode       // Tile has ceased to retire instructions
  def wfiNode: IntOutwardNode         // Tile is waiting for an interrupt

  protected val tlOtherMastersNode = TLIdentityNode()
  protected val tlMasterXbar = LazyModule(new TLXbar)
  protected val tlSlaveXbar = LazyModule(new TLXbar)
  protected val intXbar = LazyModule(new IntXbar)

  val traceSourceNode = BundleBridgeSource(() => Vec(tileParams.core.retireWidth, new TracedInstruction()))
  val traceNode = BundleBroadcast[Vec[TracedInstruction]](Some("trace"))
  traceNode := traceSourceNode

  val bpwatchSourceNode = BundleBridgeSource(() => Vec(tileParams.core.nBreakpoints, new BPWatch()))
  val bpwatchNode = BundleBroadcast[Vec[BPWatch]](Some("bpwatch"))
  bpwatchNode := bpwatchSourceNode

  def connectTLSlave(xbarNode: TLOutwardNode, node: TLNode, bytes: Int) {
    DisableMonitors { implicit p =>
      (Seq(node, TLFragmenter(bytes, cacheBlockBytes, earlyAck=EarlyAck.PutFulls))
        ++ (xBytes != bytes).option(TLWidthWidget(xBytes)))
        .foldRight(xbarNode)(_ :*= _)
    }
  }
  def connectTLSlave(node: TLNode, bytes: Int) { connectTLSlave(tlSlaveXbar.node, node, bytes) }

  val visibilityNode = p(TileVisibilityNodeKey)
  protected def visibleManagers = visibilityNode.edges.out.flatMap(_.manager.managers)
  def unifyManagers: List[TLManagerParameters] = ManagerUnification(visibleManagers)

  // Find resource labels for all the outward caches
  def nextLevelCacheProperty: PropertyOption = {
    val outer = visibleManagers
      .filter(_.supportsAcquireB)
      .flatMap(_.resources.headOption)
      .map(_.owner.label)
      .distinct
    if (outer.isEmpty) None
    else Some("next-level-cache" -> outer.map(l => ResourceReference(l)).toList)
  }

  def cpuProperties: PropertyMap = Map(
      "device_type"          -> "cpu".asProperty,
      "status"               -> "okay".asProperty,
      "clock-frequency"      -> tileParams.core.bootFreqHz.asProperty,
      "riscv,isa"            -> isaDTS.asProperty,
      "timebase-frequency"   -> p(DTSTimebase).asProperty,
      "hardware-exec-breakpoint-count" -> tileParams.core.nBreakpoints.asProperty
  )

  // The boundary buffering needed to cut feed-through paths is
  // microarchitecture specific, so these may need to be overridden
  protected def makeMasterBoundaryBuffers(implicit p: Parameters) = TLBuffer(BufferParams.none)
  def crossMasterPort(): TLOutwardNode = {
    val tlMasterXing = this.crossOut(crossing match {
      case RationalCrossing(_) => this { makeMasterBoundaryBuffers } :=* masterNode
      case _ => masterNode
    })
    tlMasterXing(crossing)
  }

  protected def makeSlaveBoundaryBuffers(implicit p: Parameters) = TLBuffer(BufferParams.none)
  def crossSlavePort(): TLInwardNode = { DisableMonitors { implicit p => FlipRendering { implicit p =>
    val tlSlaveXing = this.crossIn(crossing match {
      case RationalCrossing(_) => slaveNode :*= this { makeSlaveBoundaryBuffers }
      case _ => slaveNode
    })
    tlSlaveXing(crossing)
  } } }

  def crossIntIn(): IntInwardNode = crossIntIn(intInwardNode)
  def crossIntOut(): IntOutwardNode = crossIntOut(intOutwardNode)

  this.suggestName(tileParams.name)
}

abstract class BaseTileModuleImp[+L <: BaseTile](val outer: L) extends LazyModuleImp(outer) with HasTileParameters {

  require(xLen == 32 || xLen == 64)
  require(paddrBits <= maxPAddrBits)
  require(resetVectorLen <= xLen)
  require(resetVectorLen <= vaddrBitsExtended)
  require (log2Up(hartId + 1) <= hartIdLen, s"p(MaxHartIdBits) of $hartIdLen is not enough for hartid $hartId")

  val constants = IO(new TileInputConstants)
}

/** Some other non-tilelink but still standard inputs */
trait HasExternallyDrivenTileConstants extends Bundle with HasTileParameters {
  val hartid = UInt(INPUT, hartIdLen)
  val reset_vector = UInt(INPUT, resetVectorLen)
}

class TileInputConstants(implicit val p: Parameters) extends Bundle with HasExternallyDrivenTileConstants
