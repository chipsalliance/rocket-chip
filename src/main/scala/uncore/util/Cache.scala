// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package uncore.util

import Chisel._
import config.{Parameters, Field}
import rocket.PAddrBits
import util.ParameterizedBundle
import uncore.constants._

// These parameters apply to all caches, for now
case object CacheBlockBytes extends Field[Int]

trait CacheConfig {
  val nSets:         Int
  val nWays:         Int
  val rowBits:       Int
  val nTLBEntries:   Int
  val cacheIdBits:   Int
  val splitMetadata: Boolean
  val ecc:           Option[Code]
}
case class CacheName(id: String) extends Field[CacheConfig]
case object CacheName extends Field[CacheName]

trait HasCacheParameters {
  implicit val p: Parameters
  implicit val cacheConfig: CacheConfig
  val nSets = cacheConfig.nSets
  val blockOffBits = log2Up(p(CacheBlockBytes))
  val cacheIdBits = cacheConfig.cacheIdBits
  val idxBits = log2Up(cacheConfig.nSets)
  val untagBits = blockOffBits + cacheIdBits + idxBits
  val tagBits = p(PAddrBits) - untagBits
  val nWays = cacheConfig.nWays
  val wayBits = log2Up(nWays)
  val isDM = nWays == 1
  val rowBits = cacheConfig.rowBits
  val rowBytes = rowBits/8
  val rowOffBits = log2Up(rowBytes)
  val code = cacheConfig.ecc
  val hasSplitMetadata = cacheConfig.splitMetadata
}

abstract class CacheModule(implicit val p: Parameters) extends Module
  with HasCacheParameters

abstract class CacheBundle(implicit val p: Parameters) extends ParameterizedBundle()(p)
  with HasCacheParameters

abstract class ReplacementPolicy {
  def way: UInt
  def miss: Unit
  def hit: Unit
}

class RandomReplacement(ways: Int) extends ReplacementPolicy {
  private val replace = Wire(Bool())
  replace := Bool(false)
  val lfsr = LFSR16(replace)

  def way = if(ways == 1) UInt(0) else lfsr(log2Up(ways)-1,0)
  def miss = replace := Bool(true)
  def hit = {}
}

abstract class SeqReplacementPolicy {
  def access(set: UInt): Unit
  def update(valid: Bool, hit: Bool, set: UInt, way: UInt): Unit
  def way: UInt
}

class SeqRandom(n_ways: Int) extends SeqReplacementPolicy {
  val logic = new RandomReplacement(n_ways)
  def access(set: UInt) = { }
  def update(valid: Bool, hit: Bool, set: UInt, way: UInt) = {
    when (valid && !hit) { logic.miss }
  }
  def way = logic.way
}

class PseudoLRU(n: Int)
{
  require(isPow2(n))
  val state_reg = Reg(Bits(width = n))
  def access(way: UInt) {
    state_reg := get_next_state(state_reg,way)
  }
  def get_next_state(state: UInt, way: UInt) = {
    var next_state = state
    var idx = UInt(1,1)
    for (i <- log2Up(n)-1 to 0 by -1) {
      val bit = way(i)
      next_state = next_state.bitSet(idx, !bit)
      idx = Cat(idx, bit)
    }
    next_state
  }
  def replace = get_replace_way(state_reg)
  def get_replace_way(state: Bits) = {
    var idx = UInt(1,1)
    for (i <- 0 until log2Up(n))
      idx = Cat(idx, state(idx))
    idx(log2Up(n)-1,0)
  }
}

class SeqPLRU(n_sets: Int, n_ways: Int) extends SeqReplacementPolicy {
  val state = SeqMem(n_sets, Bits(width = n_ways-1))
  val logic = new PseudoLRU(n_ways)
  val current_state = Wire(Bits())
  val plru_way = logic.get_replace_way(current_state)
  val next_state = Wire(Bits())

  def access(set: UInt) = {
    current_state := Cat(state.read(set), Bits(0, width = 1))
  }

  def update(valid: Bool, hit: Bool, set: UInt, way: UInt) = {
    val update_way = Mux(hit, way, plru_way)
    next_state := logic.get_next_state(current_state, update_way)
    when (valid) { state.write(set, next_state(n_ways-1,1)) }
  }

  def way = plru_way
}

abstract class Metadata(implicit p: Parameters) extends CacheBundle()(p) {
  val tag = Bits(width = tagBits)
}

class MetaReadReq(implicit p: Parameters) extends CacheBundle()(p) {
  val idx  = Bits(width = idxBits)
  val way_en = Bits(width = nWays)
}

class MetaWriteReq[T <: Metadata](gen: T)(implicit p: Parameters) extends MetaReadReq()(p) {
  val data = gen.cloneType
  override def cloneType = new MetaWriteReq(gen)(p).asInstanceOf[this.type]
}

class MetadataArray[T <: Metadata](onReset: () => T)(implicit p: Parameters) extends CacheModule()(p) {
  val rstVal = onReset()
  val io = new Bundle {
    val read = Decoupled(new MetaReadReq).flip
    val write = Decoupled(new MetaWriteReq(rstVal)).flip
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

