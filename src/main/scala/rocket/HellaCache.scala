// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package rocket

import Chisel._
import config.{Parameters, Field}
import coreplex._
import diplomacy._
import tile._
import uncore.constants._
import uncore.tilelink2._
import uncore.util.Code
import util.{ParameterizedBundle, RandomReplacement}
import scala.collection.mutable.ListBuffer

case class DCacheParams(
    nSets: Int = 64,
    nWays: Int = 4,
    rowBits: Int = 64,
    nTLBEntries: Int = 8,
    splitMetadata: Boolean = false,
    ecc: Option[Code] = None,
    nMSHRs: Int = 1,
    nSDQ: Int = 17,
    nRPQ: Int = 16,
    nMMIOs: Int = 1,
    blockBytes: Int = 64,
    scratch: Option[BigInt] = None) extends L1CacheParams {

  def dataScratchpadBytes: Int = scratch.map(_ => nSets*blockBytes).getOrElse(0)

  def replacement = new RandomReplacement(nWays)

  require((!scratch.isDefined || nWays == 1),
    "Scratchpad only allowed in direct-mapped cache.")
  require((!scratch.isDefined || nMSHRs == 0),
    "Scratchpad only allowed in blocking cache.")
  require(isPow2(nSets), s"nSets($nSets) must be pow2")
}

trait HasL1HellaCacheParameters extends HasL1CacheParameters with HasCoreParameters {
  val cacheParams = tileParams.dcache.get
  val cfg = cacheParams

  def wordBits = xLen // really, xLen max 
  def wordBytes = wordBits/8
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
  def encDataBits = code.width(coreDataBits)
  def encRowBits = encDataBits*rowWords
  def lrscCycles = 32 // ISA requires 16-insn LRSC sequences to succeed
  def nIOMSHRs = cacheParams.nMMIOs
  def maxUncachedInFlight = cacheParams.nMMIOs
  def dataScratchpadSize = cacheParams.dataScratchpadBytes

  require(rowBits >= coreDataBits, s"rowBits($rowBits) < coreDataBits($coreDataBits)")
  // TODO should rowBits even be seperably specifiable?
  require(rowBits == cacheDataBits, s"rowBits($rowBits) != cacheDataBits($cacheDataBits)") 
  // would need offset addr for puts if data width < xlen
  require(xLen <= cacheDataBits, s"xLen($xLen) > cacheDataBits($cacheDataBits)")
  require(!usingVM || untagBits <= pgIdxBits, s"untagBits($untagBits) > pgIdxBits($pgIdxBits)")
}

abstract class L1HellaCacheModule(implicit val p: Parameters) extends Module
  with HasL1HellaCacheParameters

abstract class L1HellaCacheBundle(implicit val p: Parameters) extends ParameterizedBundle()(p)
  with HasL1HellaCacheParameters

/** Bundle definitions for HellaCache interfaces */

trait HasCoreMemOp extends HasCoreParameters {
  val addr = UInt(width = coreMaxAddrBits)
  val tag  = Bits(width = dcacheReqTagBits)
  val cmd  = Bits(width = M_SZ)
  val typ  = Bits(width = MT_SZ)
}

trait HasCoreData extends HasCoreParameters {
  val data = Bits(width = coreDataBits)
}

class HellaCacheReqInternal(implicit p: Parameters) extends CoreBundle()(p) with HasCoreMemOp {
  val phys = Bool()
}

class HellaCacheReq(implicit p: Parameters) extends HellaCacheReqInternal()(p) with HasCoreData

class HellaCacheResp(implicit p: Parameters) extends CoreBundle()(p)
    with HasCoreMemOp
    with HasCoreData {
  val replay = Bool()
  val has_data = Bool()
  val data_word_bypass = Bits(width = coreDataBits)
  val store_data = Bits(width = coreDataBits)
}

class AlignmentExceptions extends Bundle {
  val ld = Bool()
  val st = Bool()
}

class HellaCacheExceptions extends Bundle {
  val ma = new AlignmentExceptions
  val pf = new AlignmentExceptions
}

// interface between D$ and processor/DTLB
class HellaCacheIO(implicit p: Parameters) extends CoreBundle()(p) {
  val req = Decoupled(new HellaCacheReq)
  val s1_kill = Bool(OUTPUT) // kill previous cycle's req
  val s1_data = Bits(OUTPUT, coreDataBits) // data for previous cycle's req
  val s2_nack = Bool(INPUT) // req from two cycles ago is rejected

  // performance events
  val acquire = Bool(INPUT)
  val release = Bool(INPUT)

  val resp = Valid(new HellaCacheResp).flip
  val replay_next = Bool(INPUT)
  val xcpt = (new HellaCacheExceptions).asInput
  val invalidate_lr = Bool(OUTPUT)
  val ordered = Bool(INPUT)
}

/** Base classes for Diplomatic TL2 HellaCaches */

abstract class HellaCache(implicit p: Parameters) extends LazyModule {
  private val cfg = p(TileKey).dcache.get
  val node = TLClientNode(cfg.scratch.map { _ =>
      TLClientParameters(sourceId = IdRange(0, cfg.nMMIOs))
    } getOrElse {
      TLClientParameters(
        sourceId = IdRange(0, cfg.nMSHRs+cfg.nMMIOs),
        supportsProbe = TransferSizes(1, cfg.blockBytes))
    })
  val module: HellaCacheModule
}

class HellaCacheBundle(outer: HellaCache) extends Bundle {
  implicit val p = outer.p
  val cpu = (new HellaCacheIO).flip
  val ptw = new TLBPTWIO()
  val mem = outer.node.bundleOut
}

class HellaCacheModule(outer: HellaCache) extends LazyModuleImp(outer)
    with HasL1HellaCacheParameters {
  implicit val edge = outer.node.edgesOut(0)
  val io = new HellaCacheBundle(outer)
  val tl_out = io.mem(0)
}

object HellaCache {
  def apply(blocking: Boolean, scratch: () => Option[AddressSet] = () => None)(implicit p: Parameters) = {
    if (blocking) LazyModule(new DCache(scratch))
    else LazyModule(new NonBlockingDCache)
  }
}

/** Mix-ins for constructing tiles that have a HellaCache */

trait HasHellaCache extends HasTileLinkMasterPort {
  val module: HasHellaCacheModule
  implicit val p: Parameters
  def findScratchpadFromICache: Option[AddressSet]
  var nDCachePorts = 0
  val dcache = HellaCache(tileParams.dcache.get.nMSHRs == 0, findScratchpadFromICache _)
  masterNode := dcache.node
}

trait HasHellaCacheBundle extends HasTileLinkMasterPortBundle {
  val outer: HasHellaCache
}

trait HasHellaCacheModule extends HasTileLinkMasterPortModule {
  val outer: HasHellaCache
  //val io: HasHellaCacheBundle
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
  val wmask = Mux(rst || Bool(nWays == 1), SInt(-1), io.write.bits.way_en.asSInt).toBools
  val rmask = Mux(rst || Bool(nWays == 1), SInt(-1), io.read.bits.way_en.asSInt).toBools
  when (rst) { rst_cnt := rst_cnt+UInt(1) }

  val metabits = rstVal.getWidth

  if (hasSplitMetadata) {
    val tag_arrs = List.fill(nWays){ SeqMem(nSets, UInt(width = metabits)) }
    val tag_readout = Wire(Vec(nWays,rstVal.cloneType))
    (0 until nWays).foreach { (i) =>
      when (rst || (io.write.valid && wmask(i))) {
        tag_arrs(i).write(waddr, wdata)
      }
      io.resp(i) := rstVal.fromBits(tag_arrs(i).read(io.read.bits.idx, io.read.valid && rmask(i)))
    }
  } else {
    val tag_arr = SeqMem(nSets, Vec(nWays, UInt(width = metabits)))
    when (rst || io.write.valid) {
      tag_arr.write(waddr, Vec.fill(nWays)(wdata), wmask)
    }
    io.resp := tag_arr.read(io.read.bits.idx, io.read.valid).map(rstVal.fromBits(_))
  }

  io.read.ready := !rst && !io.write.valid // so really this could be a 6T RAM
  io.write.ready := !rst
}
