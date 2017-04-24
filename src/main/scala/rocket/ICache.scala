// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package rocket

import Chisel._
import config._
import diplomacy._
import tile._
import uncore.tilelink2._
import uncore.util.Code
import util._
import Chisel.ImplicitConversions._

case class ICacheParams(
    nSets: Int = 64,
    nWays: Int = 4,
    rowBits: Int = 128,
    nTLBEntries: Int = 32,
    cacheIdBits: Int = 0,
    ecc: Option[Code] = None,
    blockBytes: Int = 64) extends L1CacheParams {
  def replacement = new RandomReplacement(nWays)
}

trait HasL1ICacheParameters extends HasL1CacheParameters with HasCoreParameters {
  val cacheParams = tileParams.icache.get
}

class ICacheReq(implicit p: Parameters) extends CoreBundle()(p) with HasL1ICacheParameters {
  val addr = UInt(width = vaddrBits)
}

class ICacheResp(implicit p: Parameters) extends CoreBundle()(p) with HasL1ICacheParameters {
  val data = Bits(width = coreInstBits)
  val datablock = Bits(width = rowBits)
}

class ICache(val latency: Int)(implicit p: Parameters) extends LazyModule {
  lazy val module = new ICacheModule(this)
  val node = TLClientNode(TLClientParameters(sourceId = IdRange(0,1)))
}

class ICacheBundle(outer: ICache) extends CoreBundle()(outer.p) {
  val req = Valid(new ICacheReq).flip
  val s1_paddr = UInt(INPUT, paddrBits) // delayed one cycle w.r.t. req
  val s1_kill = Bool(INPUT) // delayed one cycle w.r.t. req
  val s2_kill = Bool(INPUT) // delayed two cycles; prevents I$ miss emission

  val resp = Valid(UInt(width = coreInstBits * fetchWidth))
  val invalidate = Bool(INPUT)
  val mem = outer.node.bundleOut
}

class ICacheModule(outer: ICache) extends LazyModuleImp(outer)
    with HasL1ICacheParameters {
  val io = new ICacheBundle(outer)
  val edge = outer.node.edgesOut(0)
  val tl_out = io.mem(0)

  require(isPow2(nSets) && isPow2(nWays))
  require(isPow2(coreInstBytes))
  require(!usingVM || pgIdxBits >= untagBits)

  val s_ready :: s_request :: s_refill :: Nil = Enum(UInt(), 3)
  val state = Reg(init=s_ready)
  val invalidated = Reg(Bool())

  val refill_addr = Reg(UInt(width = paddrBits))
  val s1_tag_hit = Wire(Vec(nWays, Bool()))
  val s1_any_tag_hit = s1_tag_hit.reduce(_||_)

  val s1_valid = Reg(init=Bool(false))
  val out_valid = s1_valid && state === s_ready && !io.s1_kill
  val s1_idx = io.s1_paddr(untagBits-1,blockOffBits)
  val s1_tag = io.s1_paddr(tagBits+untagBits-1,untagBits)
  val s1_hit = out_valid && s1_any_tag_hit
  val s1_miss = s1_valid && state === s_ready && !s1_any_tag_hit

  val s0_valid = io.req.valid && state === s_ready
  val s0_vaddr = io.req.bits.addr

  s1_valid := s0_valid

  when (s1_miss) { refill_addr := io.s1_paddr }
  val refill_tag = refill_addr(tagBits+untagBits-1,untagBits)
  val refill_idx = refill_addr(untagBits-1,blockOffBits)
  val (_, _, refill_done, refill_cnt) = edge.count(tl_out.d)
  tl_out.d.ready := Bool(true)
  require (edge.manager.minLatency > 0)

  val repl_way = if (isDM) UInt(0) else LFSR16(s1_miss)(log2Up(nWays)-1,0)
  val entagbits = code.width(tagBits)
  val tag_array = SeqMem(nSets, Vec(nWays, Bits(width = entagbits)))
  val tag_rdata = tag_array.read(s0_vaddr(untagBits-1,blockOffBits), !refill_done && s0_valid)
  when (refill_done) {
    val tag = code.encode(refill_tag)
    tag_array.write(refill_idx, Vec.fill(nWays)(tag), Vec.tabulate(nWays)(repl_way === _))
  }

  val vb_array = Reg(init=Bits(0, nSets*nWays))
  when (refill_done && !invalidated) {
    vb_array := vb_array.bitSet(Cat(repl_way, refill_idx), Bool(true))
  }
  val invalidate = Wire(init = io.invalidate)
  when (invalidate) {
    vb_array := Bits(0)
    invalidated := Bool(true)
  }

  val s1_tag_disparity = Wire(Vec(nWays, Bool()))
  val wordBits = coreInstBits * fetchWidth
  val s1_dout = Wire(Vec(nWays, UInt(width = code.width(wordBits))))
  val s1_dout_valid = RegNext(s0_valid)

  for (i <- 0 until nWays) {
    val s1_vb = vb_array(Cat(UInt(i), io.s1_paddr(untagBits-1,blockOffBits))).toBool
    s1_tag_disparity(i) := code.decode(tag_rdata(i)).error
    s1_tag_hit(i) := s1_vb && code.decode(tag_rdata(i)).uncorrected === s1_tag
  }

  require(rowBits % wordBits == 0)
  val data_arrays = Seq.fill(rowBits / wordBits) { SeqMem(nSets * refillCycles, Vec(nWays, UInt(width = code.width(wordBits)))) }
  for ((data_array, i) <- data_arrays zipWithIndex) {
    val wen = tl_out.d.valid
    when (wen) {
      val idx = (refill_idx << log2Ceil(refillCycles)) | refill_cnt
      val data = tl_out.d.bits.data(wordBits*(i+1)-1, wordBits*i)
      data_array.write(idx, Vec.fill(nWays)(code.encode(data)), (0 until nWays).map(repl_way === _))
    }
    def wordMatch(addr: UInt) = addr.extract(log2Ceil(rowBytes)-1, log2Ceil(wordBits/8)) === i
    val s0_raddr = s0_vaddr(untagBits-1,blockOffBits-log2Ceil(refillCycles))
    val dout = data_array.read(s0_raddr, !wen && (s0_valid && wordMatch(s0_vaddr)))
    when (wordMatch(io.s1_paddr)) {
      s1_dout := dout
    }
  }

/*
  for ((data_array, i) <- data_arrays zipWithIndex) {
    val wen = tl_out.d.valid && repl_way === UInt(i)
    when (wen) {
      val e_d = code.encode(tl_out.d.bits.data)
      data_array.write((refill_idx << log2Ceil(refillCycles)) | refill_cnt, e_d)
    }
    val s0_raddr = s0_vaddr(untagBits-1,blockOffBits-log2Ceil(refillCycles))
    s1_dout(i) := data_array.read(s0_raddr, !wen && s0_valid)
  }
*/

  // output signals
  outer.latency match {
    case 1 =>
      require(code.width(rowBits) == rowBits) // no ECC
      io.resp.bits := Mux1H(s1_tag_hit, s1_dout)
      io.resp.valid := s1_hit
    case 2 =>
      val s2_valid = RegNext(out_valid, Bool(false))
      val s2_hit = RegNext(s1_hit, Bool(false))
      val s2_tag_hit = RegEnable(s1_tag_hit, s1_valid)
      val s2_dout = RegEnable(s1_dout, s1_valid)
      val s2_way_mux = Mux1H(s2_tag_hit, s2_dout)

      val s2_tag_disparity = RegEnable(s1_tag_disparity, s1_valid).asUInt.orR
      val s2_data_disparity = code.decode(s2_way_mux).error
      val s2_disparity = s2_tag_disparity || s2_data_disparity
      when (s2_valid && s2_disparity) { invalidate := true }

      io.resp.bits := code.decode(s2_way_mux).uncorrected
      io.resp.valid := s2_hit && !s2_disparity
  }
  tl_out.a.valid := state === s_request && !io.s2_kill
  tl_out.a.bits := edge.Get(
                    fromSource = UInt(0),
                    toAddress = (refill_addr >> blockOffBits) << blockOffBits,
                    lgSize = lgCacheBlockBytes)._2
  tl_out.b.ready := Bool(true)
  tl_out.c.valid := Bool(false)
  tl_out.e.valid := Bool(false)

  // control state machine
  switch (state) {
    is (s_ready) {
      when (s1_miss && !io.s1_kill) { state := s_request }
      invalidated := Bool(false)
    }
    is (s_request) {
      when (tl_out.a.ready) { state := s_refill }
      when (io.s2_kill) { state := s_ready }
    }
  }
  when (refill_done) {
    assert(state === s_refill)
    state := s_ready
  }
}
