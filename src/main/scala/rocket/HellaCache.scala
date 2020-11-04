// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.rocket

import Chisel._
import chisel3.dontTouch
import freechips.rocketchip.amba._
import freechips.rocketchip.config.{Parameters, Field}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomaticobjectmodel.model.OMSRAM
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import scala.collection.mutable.ListBuffer

case class DCacheParams(
    nSets: Int = 64,
    nWays: Int = 4,
    rowBits: Int = 64,
    subWordBits: Option[Int] = None,
    replacementPolicy: String = "random",
    nTLBSets: Int = 1,
    nTLBWays: Int = 32,
    nTLBBasePageSectors: Int = 4,
    nTLBSuperpages: Int = 4,
    tagECC: Option[String] = None,
    dataECC: Option[String] = None,
    dataECCBytes: Int = 1,
    nMSHRs: Int = 1,
    nSDQ: Int = 17,
    nRPQ: Int = 16,
    nMMIOs: Int = 1,
    blockBytes: Int = 64,
    separateUncachedResp: Boolean = false,
    acquireBeforeRelease: Boolean = false,
    pipelineWayMux: Boolean = false,
    clockGate: Boolean = false,
    scratch: Option[BigInt] = None) extends L1CacheParams {

  def tagCode: Code = Code.fromString(tagECC)
  def dataCode: Code = Code.fromString(dataECC)

  def dataScratchpadBytes: Int = scratch.map(_ => nSets*blockBytes).getOrElse(0)

  def replacement = new RandomReplacement(nWays)

  def silentDrop: Boolean = !acquireBeforeRelease

  require((!scratch.isDefined || nWays == 1),
    "Scratchpad only allowed in direct-mapped cache.")
  require((!scratch.isDefined || nMSHRs == 0),
    "Scratchpad only allowed in blocking cache.")
  if (scratch.isEmpty)
    require(isPow2(nSets), s"nSets($nSets) must be pow2")
}

trait HasL1HellaCacheParameters extends HasL1CacheParameters with HasCoreParameters {
  val cacheParams = tileParams.dcache.get
  val cfg = cacheParams

  def wordBits = coreDataBits
  def wordBytes = coreDataBytes
  def subWordBits = cacheParams.subWordBits.getOrElse(wordBits)
  def subWordBytes = subWordBits / 8
  def wordOffBits = log2Up(wordBytes)
  def beatBytes = cacheBlockBytes / cacheDataBeats
  def beatWords = beatBytes / wordBytes
  def beatOffBits = log2Up(beatBytes)
  def idxMSB = untagBits-1
  def idxLSB = blockOffBits
  def offsetmsb = idxLSB-1
  def offsetlsb = wordOffBits
  def rowWords = rowBits/wordBits
  def doNarrowRead = coreDataBits * nWays % rowBits == 0
  def eccBytes = cacheParams.dataECCBytes
  val eccBits = cacheParams.dataECCBytes * 8
  val encBits = cacheParams.dataCode.width(eccBits)
  val encWordBits = encBits * (wordBits / eccBits)
  def encDataBits = cacheParams.dataCode.width(coreDataBits) // NBDCache only
  def encRowBits = encDataBits*rowWords
  def lrscCycles = coreParams.lrscCycles // ISA requires 16-insn LRSC sequences to succeed
  def lrscBackoff = 3 // disallow LRSC reacquisition briefly
  def blockProbeAfterGrantCycles = 8 // give the processor some time to issue a request after a grant
  def nIOMSHRs = cacheParams.nMMIOs
  def maxUncachedInFlight = cacheParams.nMMIOs
  def dataScratchpadSize = cacheParams.dataScratchpadBytes

  require(rowBits >= coreDataBits, s"rowBits($rowBits) < coreDataBits($coreDataBits)")
  if (!usingDataScratchpad)
    require(rowBits == cacheDataBits, s"rowBits($rowBits) != cacheDataBits($cacheDataBits)")
  // would need offset addr for puts if data width < xlen
  require(xLen <= cacheDataBits, s"xLen($xLen) > cacheDataBits($cacheDataBits)")
}

abstract class L1HellaCacheModule(implicit val p: Parameters) extends Module
  with HasL1HellaCacheParameters

abstract class L1HellaCacheBundle(implicit val p: Parameters) extends ParameterizedBundle()(p)
  with HasL1HellaCacheParameters

/** Bundle definitions for HellaCache interfaces */

trait HasCoreMemOp extends HasL1HellaCacheParameters {
  val addr = UInt(width = coreMaxAddrBits)
  val idx  = (usingVM && untagBits > pgIdxBits).option(UInt(coreMaxAddrBits.W))
  val tag  = Bits(width = coreParams.dcacheReqTagBits + log2Ceil(dcacheArbPorts))
  val cmd  = Bits(width = M_SZ)
  val size = Bits(width = log2Ceil(coreDataBytes.log2 + 1))
  val signed = Bool()
  val dprv = UInt(width = PRV.SZ)
}

trait HasCoreData extends HasCoreParameters {
  val data = Bits(width = coreDataBits)
  val mask = UInt(width = coreDataBytes)
}

class HellaCacheReqInternal(implicit p: Parameters) extends CoreBundle()(p) with HasCoreMemOp {
  val phys = Bool()
  val no_alloc = Bool()
  val no_xcpt = Bool()
}

class HellaCacheReq(implicit p: Parameters) extends HellaCacheReqInternal()(p) with HasCoreData

class HellaCacheResp(implicit p: Parameters) extends CoreBundle()(p)
    with HasCoreMemOp
    with HasCoreData {
  val replay = Bool()
  val has_data = Bool()
  val data_word_bypass = Bits(width = coreDataBits)
  val data_raw = Bits(width = coreDataBits)
  val store_data = Bits(width = coreDataBits)
}

class AlignmentExceptions extends Bundle {
  val ld = Bool()
  val st = Bool()
}

class HellaCacheExceptions extends Bundle {
  val ma = new AlignmentExceptions
  val pf = new AlignmentExceptions
  val ae = new AlignmentExceptions
}

class HellaCacheWriteData(implicit p: Parameters) extends CoreBundle()(p) with HasCoreData

class HellaCachePerfEvents extends Bundle {
  val acquire = Bool()
  val release = Bool()
  val grant = Bool()
  val tlbMiss = Bool()
  val blocked = Bool()
  val canAcceptStoreThenLoad = Bool()
  val canAcceptStoreThenRMW = Bool()
  val canAcceptLoadThenLoad = Bool()
  val storeBufferEmptyAfterLoad = Bool()
  val storeBufferEmptyAfterStore = Bool()
}

// interface between D$ and processor/DTLB
class HellaCacheIO(implicit p: Parameters) extends CoreBundle()(p) {
  val req = Decoupled(new HellaCacheReq)
  val s1_kill = Bool(OUTPUT) // kill previous cycle's req
  val s1_data = new HellaCacheWriteData().asOutput // data for previous cycle's req
  val s2_nack = Bool(INPUT) // req from two cycles ago is rejected
  val s2_nack_cause_raw = Bool(INPUT) // reason for nack is store-load RAW hazard (performance hint)
  val s2_kill = Bool(OUTPUT) // kill req from two cycles ago
  val s2_uncached = Bool(INPUT) // advisory signal that the access is MMIO
  val s2_paddr = UInt(INPUT, paddrBits) // translated address

  val resp = Valid(new HellaCacheResp).flip
  val replay_next = Bool(INPUT)
  val s2_xcpt = (new HellaCacheExceptions).asInput
  val uncached_resp = tileParams.dcache.get.separateUncachedResp.option(Decoupled(new HellaCacheResp).flip)
  val ordered = Bool(INPUT)
  val perf = new HellaCachePerfEvents().asInput

  val keep_clock_enabled = Bool(OUTPUT) // should D$ avoid clock-gating itself?
  val clock_enabled = Bool(INPUT) // is D$ currently being clocked?
}

/** Base classes for Diplomatic TL2 HellaCaches */

abstract class HellaCache(staticIdForMetadataUseOnly: Int)(implicit p: Parameters) extends LazyModule
    with HasNonDiplomaticTileParameters {
  protected val cfg = tileParams.dcache.get

  protected def cacheClientParameters = cfg.scratch.map(x => Seq()).getOrElse(Seq(TLMasterParameters.v2(
    name          = s"Core ${staticIdForMetadataUseOnly} DCache",
    sourceId      = IdRange(0, 1 max cfg.nMSHRs),
    emits = TLMasterToSlaveTransferSizes(
      acquireB = TransferSizes(cfg.blockBytes, cfg.blockBytes)
    ),
    supports = TLSlaveToMasterTransferSizes(
      probe = TransferSizes(cfg.blockBytes, cfg.blockBytes)
    ))))

  val fLen = tileParams.core.fpu.map(_.fLen).getOrElse(0)
  val vMemDataBits = tileParams.core.vMemDataBits
  val maxTransferSizeBits = p(XLen) max fLen max vMemDataBits
  val maxTransferSizeBytes = maxTransferSizeBits / 8

  protected def mmioClientParameters = Seq(TLMasterParameters.v2(
    name          = s"Core ${staticIdForMetadataUseOnly} DCache MMIO",
    sourceId      = IdRange(firstMMIO, firstMMIO + cfg.nMMIOs),
    requestFifo   = true,
    emits = TLMasterToSlaveTransferSizes(
      arithmetic = TransferSizes(4, maxTransferSizeBytes),
      logical = TransferSizes(4, maxTransferSizeBytes),
      get = TransferSizes(1, maxTransferSizeBytes),
      putFull = TransferSizes(1, maxTransferSizeBytes)
    )))

  def firstMMIO = (cacheClientParameters.map(_.sourceId.end) :+ 0).max

  val node = TLClientNode(Seq(TLMasterPortParameters.v1(
    clients = cacheClientParameters ++ mmioClientParameters,
    minLatency = 1,
    requestFields = tileParams.core.useVM.option(Seq()).getOrElse(Seq(AMBAProtField())))))

  val hartIdSinkNodeOpt = cfg.scratch.map(_ => BundleBridgeSink[UInt]())
  val mmioAddressPrefixSinkNodeOpt = cfg.scratch.map(_ => BundleBridgeSink[UInt]())

  val module: HellaCacheModule

  def flushOnFenceI = cfg.scratch.isEmpty && !node.edges.out(0).manager.managers.forall(m => !m.supportsAcquireT || !m.executable || m.regionType >= RegionType.TRACKED || m.regionType <= RegionType.IDEMPOTENT)

  def canSupportCFlushLine = !usingVM || cfg.blockBytes * cfg.nSets <= (1 << pgIdxBits)

  require(!tileParams.core.haveCFlush || cfg.scratch.isEmpty, "CFLUSH_D_L1 instruction requires a D$")

  def getOMSRAMs(): Seq[OMSRAM]
}

class HellaCacheBundle(val outer: HellaCache)(implicit p: Parameters) extends CoreBundle()(p) {
  val cpu = (new HellaCacheIO).flip
  val ptw = new TLBPTWIO()
  val errors = new DCacheErrors
}

class HellaCacheModule(outer: HellaCache) extends LazyModuleImp(outer)
    with HasL1HellaCacheParameters {
  implicit val edge = outer.node.edges.out(0)
  val (tl_out, _) = outer.node.out(0)
  val io = IO(new HellaCacheBundle(outer))
  val io_hartid = outer.hartIdSinkNodeOpt.map(_.bundle)
  val io_mmio_address_prefix = outer.mmioAddressPrefixSinkNodeOpt.map(_.bundle)
  dontTouch(io.cpu.resp) // Users like to monitor these fields even if the core ignores some signals
  dontTouch(io.cpu.s1_data)

  private val fifoManagers = edge.manager.managers.filter(TLFIFOFixer.allVolatile)
  fifoManagers.foreach { m =>
    require (m.fifoId == fifoManagers.head.fifoId,
      s"IOMSHRs must be FIFO for all regions with effects, but HellaCache sees\n"+
      s"${m.nodePath.map(_.name)}\nversus\n${fifoManagers.head.nodePath.map(_.name)}")
  }
}

/** Support overriding which HellaCache is instantiated */

case object BuildHellaCache extends Field[BaseTile => Parameters => HellaCache](HellaCacheFactory.apply)

object HellaCacheFactory {
  def apply(tile: BaseTile)(p: Parameters): HellaCache = {
    if (tile.tileParams.dcache.get.nMSHRs == 0)
      new DCache(tile.staticIdForMetadataUseOnly, tile.crossing)(p)
    else
      new NonBlockingDCache(tile.staticIdForMetadataUseOnly)(p)
  }
}

/** Mix-ins for constructing tiles that have a HellaCache */

trait HasHellaCache { this: BaseTile =>
  val module: HasHellaCacheModule
  implicit val p: Parameters
  var nDCachePorts = 0
  lazy val dcache: HellaCache = LazyModule(p(BuildHellaCache)(this)(p))

  tlMasterXbar.node := dcache.node
  dcache.hartIdSinkNodeOpt.map { _ := hartIdNexusNode }
  dcache.mmioAddressPrefixSinkNodeOpt.map { _ := mmioAddressPrefixNexusNode }
}

trait HasHellaCacheModule {
  val outer: HasHellaCache with HasTileParameters
  implicit val p: Parameters
  val dcachePorts = ListBuffer[HellaCacheIO]()
  val dcacheArb = Module(new HellaCacheArbiter(outer.nDCachePorts)(outer.p))
  outer.dcache.module.io.cpu <> dcacheArb.io.mem
}

/** Metadata array used for all HellaCaches */

class L1Metadata(implicit p: Parameters) extends L1HellaCacheBundle()(p) {
  val coh = new ClientMetadata
  val tag = UInt(width = tagBits)
}

object L1Metadata {
  def apply(tag: Bits, coh: ClientMetadata)(implicit p: Parameters) = {
    val meta = Wire(new L1Metadata)
    meta.tag := tag
    meta.coh := coh
    meta
  }
}

class L1MetaReadReq(implicit p: Parameters) extends L1HellaCacheBundle()(p) {
  val idx    = UInt(width = idxBits)
  val way_en = UInt(width = nWays)
  val tag    = UInt(width = tagBits)
}

class L1MetaWriteReq(implicit p: Parameters) extends L1MetaReadReq()(p) {
  val data = new L1Metadata
}

class L1MetadataArray[T <: L1Metadata](onReset: () => T)(implicit p: Parameters) extends L1HellaCacheModule()(p) {
  val rstVal = onReset()
  val io = new Bundle {
    val read = Decoupled(new L1MetaReadReq).flip
    val write = Decoupled(new L1MetaWriteReq).flip
    val resp = Vec(nWays, rstVal.cloneType).asOutput
  }
  val rst_cnt = Reg(init=UInt(0, log2Up(nSets+1)))
  val rst = rst_cnt < UInt(nSets)
  val waddr = Mux(rst, rst_cnt, io.write.bits.idx)
  val wdata = Mux(rst, rstVal, io.write.bits.data).asUInt
  val wmask = Mux(rst || Bool(nWays == 1), SInt(-1), io.write.bits.way_en.asSInt).asBools
  val rmask = Mux(rst || Bool(nWays == 1), SInt(-1), io.read.bits.way_en.asSInt).asBools
  when (rst) { rst_cnt := rst_cnt+UInt(1) }

  val metabits = rstVal.getWidth
  val tag_array = SeqMem(nSets, Vec(nWays, UInt(width = metabits)))
  val wen = rst || io.write.valid
  when (wen) {
    tag_array.write(waddr, Vec.fill(nWays)(wdata), wmask)
  }
  io.resp := tag_array.read(io.read.bits.idx, io.read.fire()).map(rstVal.fromBits(_))

  io.read.ready := !wen // so really this could be a 6T RAM
  io.write.ready := !rst
}
