// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package rocket

import Chisel._
import config.{Parameters, Field}
import coreplex._
import diplomacy._
import uncore.constants._
import uncore.tilelink2._
import uncore.util._
import util.ParameterizedBundle
import scala.collection.mutable.ListBuffer

case class DCacheParameters(
    nSets: Int = 64,
    nWays: Int = 4,
    rowBits: Int = 128,
    nTLBEntries: Int = 8,
    splitMetadata: Boolean = false,
    ecc: Option[Code] = None,
    nMSHRs: Int = 1,
    nSDQ: Int = 17,
    nRPQ: Int = 16,
    nMMIOs: Int = 1) extends CacheParameters {
  val replacement = new RandomReplacement(nWays)
}

trait HasL1HellaCacheParameters extends HasL1CacheParameters {
  override val cacheParameters = p(TileKey).dcache

  val wordBits = xLen // really, xLen max 
  val wordBytes = wordBits/8
  val wordOffBits = log2Up(wordBytes)
  val beatBytes = cacheBlockBytes / cacheDataBeats
  val beatWords = beatBytes / wordBytes
  val beatOffBits = log2Up(beatBytes)
  val idxMSB = untagBits-1
  val idxLSB = blockOffBits
  val offsetmsb = idxLSB-1
  val offsetlsb = wordOffBits
  val rowWords = rowBits/wordBits
  val doNarrowRead = coreDataBits * nWays % rowBits == 0
  val encDataBits = code.width(coreDataBits)
  val encRowBits = encDataBits*rowWords
  val nIOMSHRs = 1
  val lrscCycles = 32 // ISA requires 16-insn LRSC sequences to succeed

  require(isPow2(nSets))
  require(rowBits >= coreDataBits)
  require(rowBits == cacheDataBits) // TODO should rowBits even be seperably specifiable?
  require(xLen <= cacheDataBits) // would need offset addr for puts if data width < xlen
  require(!usingVM || untagBits <= pgIdxBits)
}

abstract class L1HellaCacheModule(implicit val p: Parameters) extends Module
  with HasL1HellaCacheParameters

abstract class L1HellaCacheBundle(implicit val p: Parameters) extends ParameterizedBundle()(p)
  with HasL1HellaCacheParameters

class L1Metadata(implicit p: Parameters) extends Metadata()(p) with HasL1HellaCacheParameters {
  val coh = new ClientMetadata
}
object L1Metadata {
  def apply(tag: Bits, coh: ClientMetadata)(implicit p: Parameters) = {
    val meta = Wire(new L1Metadata)
    meta.tag := tag
    meta.coh := coh
    meta
  }
}

class L1MetaReadReq(implicit p: Parameters) extends MetaReadReq {
  val tag = Bits(width = tagBits)
  override def cloneType = new L1MetaReadReq()(p).asInstanceOf[this.type] //TODO remove
}

class L1MetaWriteReq(implicit p: Parameters) extends 
  MetaWriteReq[L1Metadata](new L1Metadata)

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

  val resp = Valid(new HellaCacheResp).flip
  val replay_next = Bool(INPUT)
  val xcpt = (new HellaCacheExceptions).asInput
  val invalidate_lr = Bool(OUTPUT)
  val ordered = Bool(INPUT)
}

abstract class HellaCache(implicit p: Parameters) extends LazyModule {
  val node = TLClientNode(TLClientParameters(
    sourceId = IdRange(0, cfg.nMSHRs + cfg.nMMIOs),
    supportsProbe = TransferSizes(p(CacheBlockBytes))))
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
  implicit val cfg = outer.cfg
  implicit val edge = outer.node.edgesOut(0)
  val io = new HellaCacheBundle(outer)
  val tl_out = io.mem(0)
}

object HellaCache {
  def apply(cfg: DCacheConfig, scratch: () => Option[AddressSet] = () => None)(implicit p: Parameters) = {
    if (cfg.nMSHRs == 0) LazyModule(new DCache(cfg, scratch))
    else LazyModule(new NonBlockingDCache(cfg))
  }
}

/** Mix-ins for constructing tiles that have a HellaCache */
trait HasHellaCache extends TileNetwork {
  val module: HasHellaCacheModule
  implicit val p: Parameters
  def findScratchpadFromICache: Option[AddressSet]
  var nDCachePorts = 0
  val dcacheParams = p.alterPartial({ case CacheName => CacheName("L1D") })
  val dcache = HellaCache(p(DCacheKey), findScratchpadFromICache _)(dcacheParams)
  l1backend.node := dcache.node
}

trait HasHellaCacheBundle extends TileNetworkBundle {
  val outer: HasHellaCache
}

trait HasHellaCacheModule extends TileNetworkModule {
  val outer: HasHellaCache
  //val io: HasHellaCacheBundle
  val dcachePorts = ListBuffer[HellaCacheIO]()
  val dcacheArb = Module(new HellaCacheArbiter(outer.nDCachePorts)(outer.dcacheParams))
  outer.dcache.module.io.cpu <> dcacheArb.io.mem
}
