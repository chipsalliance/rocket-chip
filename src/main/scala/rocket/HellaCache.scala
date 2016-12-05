// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package rocket

import Chisel._
import config.{Parameters, Field}
import diplomacy._
import uncore.tilelink2._
import uncore.util._
import uncore.constants._
import uncore.tilelink.{TLKey, TLId}
import util.ParameterizedBundle

case class DCacheConfig(
  nMSHRs: Int = 1,
  nSDQ: Int = 17,
  nRPQ: Int = 16,
  nMMIOs: Int = 1)

case object DCacheKey extends Field[DCacheConfig]

trait HasL1HellaCacheParameters extends HasCacheParameters with HasCoreParameters {
  val outerDataBeats = p(TLKey(p(TLId))).dataBeats
  val outerDataBits = p(TLKey(p(TLId))).dataBitsPerBeat

  val refillCyclesPerBeat = outerDataBits/rowBits
  require(refillCyclesPerBeat == 1)

  val refillCycles = refillCyclesPerBeat*outerDataBeats

  val cacheBlockBytes = p(CacheBlockBytes)
  val lgCacheBlockBytes = log2Up(cacheBlockBytes)

  val wordBits = xLen // really, xLen max 
  val wordBytes = wordBits/8
  val wordOffBits = log2Up(wordBytes)
  val beatBytes = cacheBlockBytes / outerDataBeats
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
  require(rowBits <= outerDataBits)
  require(xLen <= outerDataBits) // would need offset addr for puts if data width < xlen
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

abstract class HellaCache(val cfg: DCacheConfig)(implicit val p: Parameters) extends LazyModule {
  val node = TLClientNode(TLClientParameters(
    sourceId = IdRange(0, cfg.nMSHRs + cfg.nMMIOs),
    supportsProbe = TransferSizes(p(CacheBlockBytes))))
  val module: HellaCacheModule
}

class HellaCacheBundle(outer: HellaCache)(implicit p: Parameters) extends Bundle {
  val cpu = (new HellaCacheIO).flip
  val ptw = new TLBPTWIO()
  val mem = outer.node.bundleOut
}

class HellaCacheModule(outer: HellaCache)(implicit val p: Parameters) extends LazyModuleImp(outer)
    with HasL1HellaCacheParameters {
  implicit val cfg = outer.cfg
  val io = new HellaCacheBundle(outer)
  val tl_out = io.mem(0)

  /* TODO
  edge.manager.managers.foreach { m =>
    if (m.supportsGet) {
      require (m.supportsGet.contains(TransferSizes(1, tlDataBytes)))
    ....etc
    }
  }
  */

}

object HellaCache {
  def apply(cfg: DCacheConfig, scratch: () => Option[AddressSet] = () => None)(implicit p: Parameters) = {
    if (cfg.nMSHRs == 0) LazyModule(new DCache(cfg, scratch))
    else LazyModule(new NonBlockingDCache(cfg))
  }
}
