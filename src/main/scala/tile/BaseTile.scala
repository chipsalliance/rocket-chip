// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile

import chisel3._
import chisel3.util.{log2Ceil, log2Up}
import org.chipsalliance.cde.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._

import freechips.rocketchip.interrupts._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import freechips.rocketchip.prci.{ClockSinkParameters}

case object TileVisibilityNodeKey extends Field[TLEphemeralNode]
case object TileKey extends Field[TileParams]
case object LookupByHartId extends Field[LookupByHartIdImpl]

trait TileParams extends HierarchicalElementParams {
  val core: CoreParams
  val icache: Option[ICacheParams]
  val dcache: Option[DCacheParams]
  val btb: Option[BTBParams]
  val tileId: Int // may not be hartid
  val blockerCtrlAddr: Option[BigInt]
}

abstract class InstantiableTileParams[TileType <: BaseTile]
    extends InstantiableHierarchicalElementParams[TileType]
    with TileParams {
  def instantiate(crossing: HierarchicalElementCrossingParamsLike, lookup: LookupByHartIdImpl)
                 (implicit p: Parameters): TileType
}

/** These parameters values are not computed based on diplomacy negotiation
  * and so are safe to use while diplomacy itself is running.
  */
trait HasNonDiplomaticTileParameters {
  implicit val p: Parameters
  def tileParams: TileParams = p(TileKey)

  def usingVM: Boolean = tileParams.core.useVM
  def usingUser: Boolean = tileParams.core.useUser || usingSupervisor
  def usingSupervisor: Boolean = tileParams.core.hasSupervisorMode
  def usingHypervisor: Boolean = usingVM && tileParams.core.useHypervisor
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
  def maxHypervisorExtraAddrBits: Int = 2
  def hypervisorExtraAddrBits: Int = {
    if (usingHypervisor) maxHypervisorExtraAddrBits
    else 0
  }
  def maxHVAddrBits: Int = maxSVAddrBits + hypervisorExtraAddrBits
  def minPgLevels: Int = {
    val res = xLen match { case 32 => 2; case 64 => 3 }
    require(pgLevels >= res)
    res
  }
  def asIdBits: Int = p(ASIdBits)
  def vmIdBits: Int = p(VMIdBits)
  lazy val maxPAddrBits: Int = {
    require(xLen == 32 || xLen == 64, s"Only XLENs of 32 or 64 are supported, but got $xLen")
    xLen match { case 32 => 34; case 64 => 56 }
  }

  def tileId: Int = tileParams.tileId

  def cacheBlockBytes = p(CacheBlockBytes)
  def lgCacheBlockBytes = log2Up(cacheBlockBytes)
  def masterPortBeatBytes = p(SystemBusKey).beatBytes

  // TODO make HellaCacheIO diplomatic and remove this brittle collection of hacks
  //                  Core   PTW                DTIM                    coprocessors           
  def dcacheArbPorts = 1 + usingVM.toInt + usingDataScratchpad.toInt + p(BuildRoCC).size + (tileParams.core.useVector && tileParams.core.vectorUseDCache).toInt

  // TODO merge with isaString in CSR.scala
  def isaDTS: String = {
    val ie = if (tileParams.core.useRVE) "e" else "i"
    val m = if (tileParams.core.mulDiv.nonEmpty) "m" else ""
    val a = if (tileParams.core.useAtomics) "a" else ""
    val f = if (tileParams.core.fpu.nonEmpty) "f" else ""
    val d = if (tileParams.core.fpu.nonEmpty && tileParams.core.fpu.get.fLen > 32) "d" else ""
    val c = if (tileParams.core.useCompressed) "c" else ""
    val v = if (tileParams.core.useVector) "v" else ""
    val h = if (usingHypervisor) "h" else ""
    val multiLetterExt = (
      // rdcycle[h], rdinstret[h] is implemented
      // rdtime[h] is not implemented, and could be provided by software emulation
      // see https://github.com/chipsalliance/rocket-chip/issues/3207
      //Some(Seq("zicntr")) ++
      Option.when(tileParams.core.useConditionalZero)(Seq("zicond")) ++
      Some(Seq("zicsr", "zifencei", "zihpm")) ++
      Option.when(tileParams.core.fpu.nonEmpty && tileParams.core.fpu.get.fLen >= 16 && tileParams.core.fpu.get.minFLen <= 16)(Seq("zfh")) ++
      tileParams.core.customIsaExt.map(Seq(_))
    ).flatten
    val multiLetterString = multiLetterExt.mkString("_")
    s"rv${p(XLen)}$ie$m$a$f$d$c$v$h$multiLetterString"
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
      "d-tlb-size"           -> (d.nTLBWays * d.nTLBSets).asProperty,
      "d-tlb-sets"           -> d.nTLBSets.asProperty)).getOrElse(Nil)

    val itlb = tileParams.icache.filter(_ => tileParams.core.useVM).map(i => Map(
      "i-tlb-size"           -> (i.nTLBWays * i.nTLBSets).asProperty,
      "i-tlb-sets"           -> i.nTLBSets.asProperty)).getOrElse(Nil)

    val mmu =
      if (tileParams.core.useVM) {
        if (tileParams.core.useHypervisor) {
          Map("tlb-split" -> Nil, "mmu-type" -> s"riscv,sv${maxSVAddrBits},sv${maxSVAddrBits}x4".asProperty)
        } else {
          Map("tlb-split" -> Nil, "mmu-type" -> s"riscv,sv$maxSVAddrBits".asProperty)
        }
      } else {
        Nil
      }

    val pmp = if (tileParams.core.nPMPs > 0) Map(
      "riscv,pmpregions" -> tileParams.core.nPMPs.asProperty,
      "riscv,pmpgranularity" -> tileParams.core.pmpGranularity.asProperty) else Nil

    dcache ++ icache ++ dtlb ++ itlb ++ mmu ++ pmp ++ incoherent
  }

}

/** These parameters values are computed based on diplomacy negotiations
  * and so are NOT safe to use while diplomacy itself is running.
  * Only mix this trait into LazyModuleImps, Modules, Bundles, Data, etc.
  */
trait HasTileParameters extends HasNonDiplomaticTileParameters {
  protected def tlBundleParams = p(TileVisibilityNodeKey).edges.out.head.bundle
  lazy val paddrBits: Int = {
    val bits = tlBundleParams.addressBits
    require(bits <= maxPAddrBits, s"Requested $bits paddr bits, but since xLen is $xLen only $maxPAddrBits will fit")
    bits
  }
  def vaddrBits: Int =
    if (usingVM) {
      val v = maxHVAddrBits
      require(v == xLen || xLen > v && v > paddrBits)
      v
    } else {
      // since virtual addresses sign-extend but physical addresses
      // zero-extend, make room for a zero sign bit for physical addresses
      (paddrBits + 1) min xLen
    }
  def vpnBits: Int = vaddrBits - pgIdxBits
  def ppnBits: Int = paddrBits - pgIdxBits
  def vpnBitsExtended: Int = vpnBits + (if (vaddrBits < xLen) 1 + usingHypervisor.toInt else 0)
  def vaddrBitsExtended: Int = vpnBitsExtended + pgIdxBits
}

/** Base class for all Tiles that use TileLink */
abstract class BaseTile private (crossing: ClockCrossingType, q: Parameters)
    extends BaseHierarchicalElement(crossing)(q)
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

  def intInwardNode: IntInwardNode            // Interrupts to the core from external devices
  def intOutwardNode: Option[IntOutwardNode]  // Interrupts from tile-internal devices (e.g. BEU)
  def haltNode: IntOutwardNode                // Unrecoverable error has occurred; suggest reset
  def ceaseNode: IntOutwardNode               // Tile has ceased to retire instructions
  def wfiNode: IntOutwardNode                 // Tile is waiting for an interrupt
  def module: BaseTileModuleImp[BaseTile]

  /** Node for broadcasting a hart id to diplomatic consumers within the tile. */
  val hartIdNexusNode: BundleBridgeNode[UInt] = BundleBroadcast[UInt](registered = p(InsertTimingClosureRegistersOnHartIds))

  /** Node for consuming the hart id input in tile-layer Chisel logic. */
  val hartIdSinkNode = BundleBridgeSink[UInt]()

  /** Node for driving a hart id input, which is to be broadcast to units within the tile.
    *
    * Making this id value an IO and then using it to do lookups of information
    * that would make otherwise-homogeneous tiles heterogeneous is a useful trick
    * to enable deduplication of tiles for hierarchical P&R flows.
    */
  val hartIdNode: BundleBridgeInwardNode[UInt] =
    hartIdSinkNode := hartIdNexusNode := BundleBridgeNameNode("hartid")

  /** Node for broadcasting a reset vector to diplomatic consumers within the tile. */
  val resetVectorNexusNode: BundleBridgeNode[UInt] = BundleBroadcast[UInt]()

  /** Node for consuming the reset vector input in tile-layer Chisel logic.
    *
    * Its width is sized by looking at the size of the address space visible
    * on the tile's master ports, but this lookup is not evaluated until
    * diplomacy has completed and Chisel elaboration has begun.
    */
  val resetVectorSinkNode = BundleBridgeSink[UInt](Some(() => UInt(visiblePhysAddrBits.W)))

  /** Node for supplying a reset vector that processors in this tile might begin fetching instructions from as they come out of reset. */
  val resetVectorNode: BundleBridgeInwardNode[UInt] =
    resetVectorSinkNode := resetVectorNexusNode := BundleBridgeNameNode("reset_vector")

  /** Nodes for connecting NMI interrupt sources and vectors into the tile */
  val nmiSinkNode = Option.when(tileParams.core.useNMI) {
    BundleBridgeSink[NMI](Some(() => new NMI(visiblePhysAddrBits)))
  }
  val nmiNode: Option[BundleBridgeInwardNode[NMI]] = nmiSinkNode.map(_ := BundleBridgeNameNode("nmi"))

  /** Node for broadcasting an address prefix to diplomatic consumers within the tile.
    *
    * The prefix should be applied by consumers by or-ing ouputs of this node
    * with a static base address (which is looked up based on the driven hartid value).
    */
  val mmioAddressPrefixNexusNode = BundleBridgeNexus[UInt](
    inputFn = BundleBridgeNexus.orReduction[UInt](registered = false) _,
    outputFn = BundleBridgeNexus.fillN[UInt](registered = false) _,
    default = Some(() => 0.U(1.W))
  )

  /** Node for external drivers to prefix base addresses of MMIO devices to which the core has a direct access path. */
  val mmioAddressPrefixNode: BundleBridgeInwardNode[UInt] =
    mmioAddressPrefixNexusNode :=* BundleBridgeNameNode("mmio_address_prefix")

  // TODO: Any node marked "consumed by the core" or "driven by the core"
  //       should be moved to either be: a member of a specific BaseTile subclass,
  //       or actually just a member of the core's LazyModule itself,
  //       assuming the core itself is diplomatic.
  //       Then these nodes should just become IdentityNodes of their respective type

  protected def traceRetireWidth = tileParams.core.retireWidth
  /** Node for the core to drive legacy "raw" instruction trace. */
  val traceSourceNode = BundleBridgeSource(() => new TraceBundle)
  /** Node for external consumers to source a legacy instruction trace from the core. */
  val traceNode = traceSourceNode

  def traceCoreParams = new TraceCoreParams()
  /** Node for core to drive instruction trace conforming to RISC-V Processor Trace spec V1.0 */
  val traceCoreSourceNode = BundleBridgeSource(() => new TraceCoreInterface(traceCoreParams))
  /** Node for external consumers to source  a V1.0 instruction trace from the core. */
  val traceCoreNode = traceCoreSourceNode

  /** Node to broadcast collected trace sideband signals into the tile. */
  val traceAuxNexusNode = BundleBridgeNexus[TraceAux](default = Some(() => {
    val aux = Wire(new TraceAux)
    aux.stall  := false.B
    aux.enable := false.B
    aux
  }))
  /** Trace sideband signals to be consumed by the core. */
  val traceAuxSinkNode = BundleBridgeSink[TraceAux]()
  /** Trace sideband signals collected here to be driven into the tile. */
  val traceAuxNode: BundleBridgeInwardNode[TraceAux] =
    traceAuxSinkNode := traceAuxNexusNode :=* BundleBridgeNameNode("trace_aux")

  /** Node for watchpoints to control trace driven by core. */
  val bpwatchSourceNode = BundleBridgeSource(() => Vec(tileParams.core.nBreakpoints, new BPWatch(traceRetireWidth)))
  /** Node to broadcast watchpoints to control trace. */
  val bpwatchNexusNode = BundleBroadcast[Vec[BPWatch]]()
  /** Node for external consumers to source watchpoints to control trace. */
  val bpwatchNode: BundleBridgeOutwardNode[Vec[BPWatch]] =
    BundleBridgeNameNode("bpwatch") :*= bpwatchNexusNode := bpwatchSourceNode

  /** Helper function for connecting MMIO devices inside the tile to an xbar that will make them visible to external masters. */
  def connectTLSlave(xbarNode: TLOutwardNode, node: TLNode, bytes: Int): Unit = {
    DisableMonitors { implicit p =>
      (Seq(node, TLFragmenter(bytes, cacheBlockBytes, earlyAck=EarlyAck.PutFulls))
        ++ (xBytes != bytes).option(TLWidthWidget(xBytes)))
        .foldRight(xbarNode)(_ :*= _)
    }
  }
  def connectTLSlave(node: TLNode, bytes: Int): Unit = { connectTLSlave(tlSlaveXbar.node, node, bytes) }

  /** TileLink node which represents the view that the intra-tile masters have of the rest of the system. */
  val visibilityNode = p(TileVisibilityNodeKey)
  protected def visibleManagers = visibilityNode.edges.out.flatMap(_.manager.managers)
  protected def visiblePhysAddrBits = visibilityNode.edges.out.head.bundle.addressBits
  def unifyManagers: List[TLManagerParameters] = ManagerUnification(visibleManagers)

  /** Finds resource labels for all the outward caches. */
  def nextLevelCacheProperty: PropertyOption = {
    val outer = visibleManagers
      .filter(_.supportsAcquireB)
      .flatMap(_.resources.headOption)
      .map(_.owner.label)
      .distinct
    if (outer.isEmpty) None
    else Some("next-level-cache" -> outer.map(l => ResourceReference(l)).toList)
  }

  /** Create a DTS representation of this "cpu". */
  def cpuProperties: PropertyMap = Map(
      "device_type"          -> "cpu".asProperty,
      "status"               -> "okay".asProperty,
      "clock-frequency"      -> tileParams.core.bootFreqHz.asProperty,
      "riscv,isa"            -> isaDTS.asProperty,
      "timebase-frequency"   -> p(DTSTimebase).asProperty,
      "hardware-exec-breakpoint-count" -> tileParams.core.nBreakpoints.asProperty
  )

  /** Can be used to access derived params calculated by HasCoreParameters
    *
    * However, callers must ensure they do not access a diplomatically-determined parameter
    * before the graph in question has been fully connected.
    */
  protected lazy val lazyCoreParamsView: HasCoreParameters = {
    class C(implicit val p: Parameters) extends HasCoreParameters
    new C
  }

  this.suggestName(tileParams.baseName)
}

abstract class BaseTileModuleImp[+L <: BaseTile](outer: L) extends BaseHierarchicalElementModuleImp[L](outer)
