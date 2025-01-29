// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket

import chisel3._
import chisel3.util._
import chisel3.experimental.SourceInfo

import org.chipsalliance.cde.config._

import freechips.rocketchip.amba.AMBAProt
import freechips.rocketchip.diplomacy.{BufferParams}
import freechips.rocketchip.prci.{ClockCrossingType, RationalCrossing, SynchronousCrossing, AsynchronousCrossing, CreditedCrossing}
import freechips.rocketchip.tile.{CoreBundle, LookupByHartId}
import freechips.rocketchip.tilelink.{TLFIFOFixer,ClientMetadata, TLBundleA, TLAtomics, TLBundleB, TLPermissions}
import freechips.rocketchip.tilelink.TLMessages.{AccessAck, HintAck, AccessAckData, Grant, GrantData, ReleaseAck}
import freechips.rocketchip.util.{CanHaveErrors, ClockGate, IdentityCode, ReplacementPolicy, DescribedSRAM, property}

import freechips.rocketchip.util.BooleanToAugmentedBoolean
import freechips.rocketchip.util.UIntToAugmentedUInt
import freechips.rocketchip.util.UIntIsOneOf
import freechips.rocketchip.util.IntToAugmentedInt
import freechips.rocketchip.util.SeqToAugmentedSeq
import freechips.rocketchip.util.SeqBoolBitwiseOps

// TODO: delete this trait once deduplication is smart enough to avoid globally inlining matching circuits
trait InlineInstance { self: chisel3.experimental.BaseModule =>
  chisel3.experimental.annotate(
    new chisel3.experimental.ChiselAnnotation {
      def toFirrtl: firrtl.annotations.Annotation = firrtl.passes.InlineAnnotation(self.toNamed) } )
}

class DCacheErrors(implicit p: Parameters) extends L1HellaCacheBundle()(p)
    with CanHaveErrors {
  val correctable = (cacheParams.tagCode.canCorrect || cacheParams.dataCode.canCorrect).option(Valid(UInt(paddrBits.W)))
  val uncorrectable = (cacheParams.tagCode.canDetect || cacheParams.dataCode.canDetect).option(Valid(UInt(paddrBits.W)))
  val bus = Valid(UInt(paddrBits.W))
}

class DCacheDataReq(implicit p: Parameters) extends L1HellaCacheBundle()(p) {
  val addr = UInt(untagBits.W)
  val write = Bool()
  val wdata = UInt((encBits * rowBytes / eccBytes).W)
  val wordMask = UInt((rowBytes / subWordBytes).W)
  val eccMask = UInt((wordBytes / eccBytes).W)
  val way_en = UInt(nWays.W)
}

class DCacheDataArray(implicit p: Parameters) extends L1HellaCacheModule()(p) {
  val io = IO(new Bundle {
    val req = Flipped(Valid(new DCacheDataReq))
    val resp = Output(Vec(nWays, UInt((req.bits.wdata.getWidth).W)))
  })

  require(rowBits % subWordBits == 0, "rowBits must be a multiple of subWordBits")
  val eccMask = if (eccBits == subWordBits) Seq(true.B) else io.req.bits.eccMask.asBools
  val wMask = if (nWays == 1) eccMask else (0 until nWays).flatMap(i => eccMask.map(_ && io.req.bits.way_en(i)))
  val wWords = io.req.bits.wdata.grouped(encBits * (subWordBits / eccBits))
  val addr = io.req.bits.addr >> rowOffBits
  val data_arrays = Seq.tabulate(rowBits / subWordBits) {
    i =>
      DescribedSRAM(
        name = s"${tileParams.baseName}_dcache_data_arrays_${i}",
        desc = "DCache Data Array",
        size = nSets * cacheBlockBytes / rowBytes,
        data = Vec(nWays * (subWordBits / eccBits), UInt(encBits.W))
      )
  }

  val rdata = for ((array , i) <- data_arrays.zipWithIndex) yield {
    val valid = io.req.valid && ((data_arrays.size == 1).B || io.req.bits.wordMask(i))
    when (valid && io.req.bits.write) {
      val wMaskSlice = (0 until wMask.size).filter(j => i % (wordBits/subWordBits) == (j % (wordBytes/eccBytes)) / (subWordBytes/eccBytes)).map(wMask(_))
      val wData = wWords(i).grouped(encBits)
      array.write(addr, VecInit((0 until nWays).flatMap(i => wData)), wMaskSlice)
    }
    val data = array.read(addr, valid && !io.req.bits.write)
    data.grouped(subWordBits / eccBits).map(_.asUInt).toSeq
  }
  (io.resp zip rdata.transpose).foreach { case (resp, data) => resp := data.asUInt }
}

class DCacheMetadataReq(implicit p: Parameters) extends L1HellaCacheBundle()(p) {
  val write = Bool()
  val addr = UInt(vaddrBitsExtended.W)
  val idx = UInt(idxBits.W)
  val way_en = UInt(nWays.W)
  val data = UInt(cacheParams.tagCode.width(new L1Metadata().getWidth).W)
}

class DCache(staticIdForMetadataUseOnly: Int, val crossing: ClockCrossingType)(implicit p: Parameters) extends HellaCache(staticIdForMetadataUseOnly)(p) {
  override lazy val module = new DCacheModule(this)
}

class DCacheTLBPort(implicit p: Parameters) extends CoreBundle()(p) {
  val req = Flipped(Decoupled(new TLBReq(coreDataBytes.log2)))
  val s1_resp = Output(new TLBResp(coreDataBytes.log2))
  val s2_kill = Input(Bool())
}

class DCacheModule(outer: DCache) extends HellaCacheModule(outer) {
  val tECC = cacheParams.tagCode
  val dECC = cacheParams.dataCode
  require(subWordBits % eccBits == 0, "subWordBits must be a multiple of eccBits")
  require(eccBytes == 1 || !dECC.isInstanceOf[IdentityCode])
  require(cacheParams.silentDrop || cacheParams.acquireBeforeRelease, "!silentDrop requires acquireBeforeRelease")
  val usingRMW = eccBytes > 1 || usingAtomicsInCache
  val mmioOffset = outer.firstMMIO
  edge.manager.requireFifo(TLFIFOFixer.allVolatile)  // TileLink pipelining MMIO requests

  val clock_en_reg = Reg(Bool())
  io.cpu.clock_enabled := clock_en_reg

  val gated_clock =
    if (!cacheParams.clockGate) clock
    else ClockGate(clock, clock_en_reg, "dcache_clock_gate")
  class DCacheModuleImpl { // entering gated-clock domain

  val tlb = Module(new TLB(false, log2Ceil(coreDataBytes), TLBConfig(nTLBSets, nTLBWays, cacheParams.nTLBBasePageSectors, cacheParams.nTLBSuperpages)))
  val pma_checker = Module(new TLB(false, log2Ceil(coreDataBytes), TLBConfig(nTLBSets, nTLBWays, cacheParams.nTLBBasePageSectors, cacheParams.nTLBSuperpages)) with InlineInstance)

  // tags
  val replacer = ReplacementPolicy.fromString(cacheParams.replacementPolicy, nWays)

  /** Metadata Arbiter:
    * 0: Tag update on reset
    * 1: Tag update on ECC error
    * 2: Tag update on hit
    * 3: Tag update on refill
    * 4: Tag update on release
    * 5: Tag update on flush
    * 6: Tag update on probe
    * 7: Tag update on CPU request
    */
  val metaArb = Module(new Arbiter(new DCacheMetadataReq, 8) with InlineInstance)

  val tag_array = DescribedSRAM(
    name = s"${tileParams.baseName}_dcache_tag_array",
    desc = "DCache Tag Array",
    size = nSets,
    data = Vec(nWays, chiselTypeOf(metaArb.io.out.bits.data))
  )

  // data
  val data = Module(new DCacheDataArray)
  /** Data Arbiter
    * 0: data from pending store buffer
    * 1: data from TL-D refill
    * 2: release to TL-A
    * 3: hit path to CPU
    */
  val dataArb = Module(new Arbiter(new DCacheDataReq, 4) with InlineInstance)
  dataArb.io.in.tail.foreach(_.bits.wdata := dataArb.io.in.head.bits.wdata) // tie off write ports by default
  data.io.req.bits <> dataArb.io.out.bits
  data.io.req.valid := dataArb.io.out.valid
  dataArb.io.out.ready := true.B
  metaArb.io.out.ready := clock_en_reg

  val tl_out_a = Wire(chiselTypeOf(tl_out.a))
  tl_out.a <> {
    val a_queue_depth = outer.crossing match {
      case RationalCrossing(_) => // TODO make this depend on the actual ratio?
        if (cacheParams.separateUncachedResp) (maxUncachedInFlight + 1) / 2
        else 2 min maxUncachedInFlight-1
      case SynchronousCrossing(BufferParams.none) => 1 // Need some buffering to guarantee livelock freedom
      case SynchronousCrossing(_)  => 0 // Adequate buffering within the crossing
      case _: AsynchronousCrossing => 0 // Adequate buffering within the crossing
      case _: CreditedCrossing     => 0 // Adequate buffering within the crossing
    }
    Queue(tl_out_a, a_queue_depth, flow = true)
  }

  val (tl_out_c, release_queue_empty) =
    if (cacheParams.acquireBeforeRelease) {
      val q = Module(new Queue(chiselTypeOf(tl_out.c.bits), cacheDataBeats, flow = true))
      tl_out.c <> q.io.deq
      (q.io.enq, q.io.count === 0.U)
    } else {
      (tl_out.c, true.B)
    }

  val s1_valid = RegNext(io.cpu.req.fire, false.B)
  val s1_probe = RegNext(tl_out.b.fire, false.B)
  val probe_bits = RegEnable(tl_out.b.bits, tl_out.b.fire) // TODO has data now :(
  val s1_nack = WireDefault(false.B)
  val s1_valid_masked = s1_valid && !io.cpu.s1_kill
  val s1_valid_not_nacked = s1_valid && !s1_nack
  val s1_tlb_req_valid = RegNext(io.tlb_port.req.fire, false.B)
  val s2_tlb_req_valid = RegNext(s1_tlb_req_valid, false.B)
  val s0_clk_en = metaArb.io.out.valid && !metaArb.io.out.bits.write

  val s0_req = WireInit(io.cpu.req.bits)
  s0_req.addr := Cat(metaArb.io.out.bits.addr >> blockOffBits, io.cpu.req.bits.addr(blockOffBits-1,0))
  s0_req.idx.foreach(_ := Cat(metaArb.io.out.bits.idx, s0_req.addr(blockOffBits-1, 0)))
  when (!metaArb.io.in(7).ready) { s0_req.phys := true.B }
  val s1_req = RegEnable(s0_req, s0_clk_en)
  val s1_vaddr = Cat(s1_req.idx.getOrElse(s1_req.addr) >> tagLSB, s1_req.addr(tagLSB-1, 0))

  val s0_tlb_req = WireInit(io.tlb_port.req.bits)
  when (!io.tlb_port.req.fire) {
    s0_tlb_req.passthrough := s0_req.phys
    s0_tlb_req.vaddr := s0_req.addr
    s0_tlb_req.size := s0_req.size
    s0_tlb_req.cmd := s0_req.cmd
    s0_tlb_req.prv := s0_req.dprv
    s0_tlb_req.v := s0_req.dv
  }
  val s1_tlb_req = RegEnable(s0_tlb_req, s0_clk_en || io.tlb_port.req.valid)

  val s1_read = isRead(s1_req.cmd)
  val s1_write = isWrite(s1_req.cmd)
  val s1_readwrite = s1_read || s1_write
  val s1_sfence = s1_req.cmd === M_SFENCE || s1_req.cmd === M_HFENCEV || s1_req.cmd === M_HFENCEG
  val s1_flush_line = s1_req.cmd === M_FLUSH_ALL && s1_req.size(0)
  val s1_flush_valid = Reg(Bool())
  val s1_waw_hazard = Wire(Bool())

  val s_ready :: s_voluntary_writeback :: s_probe_rep_dirty :: s_probe_rep_clean :: s_probe_retry :: s_probe_rep_miss :: s_voluntary_write_meta :: s_probe_write_meta :: s_dummy :: s_voluntary_release :: Nil = Enum(10)
  val supports_flush = outer.flushOnFenceI || coreParams.haveCFlush
  val flushed = RegInit(true.B)
  val flushing = RegInit(false.B)
  val flushing_req = Reg(chiselTypeOf(s1_req))
  val cached_grant_wait = RegInit(false.B)
  val resetting = RegInit(false.B)
  val flushCounter = RegInit((nSets * (nWays-1)).U(log2Ceil(nSets * nWays).W))
  val release_ack_wait = RegInit(false.B)
  val release_ack_addr = Reg(UInt(paddrBits.W))
  val release_state = RegInit(s_ready)
  val refill_way = Reg(UInt())
  val any_pstore_valid = Wire(Bool())
  val inWriteback = release_state.isOneOf(s_voluntary_writeback, s_probe_rep_dirty)
  val releaseWay = Wire(UInt())
  io.cpu.req.ready := (release_state === s_ready) && !cached_grant_wait && !s1_nack

  // I/O MSHRs
  val uncachedInFlight = RegInit(VecInit(Seq.fill(maxUncachedInFlight)(false.B)))
  val uncachedReqs = Reg(Vec(maxUncachedInFlight, new HellaCacheReq))
  val uncachedResp = WireInit(new HellaCacheReq, DontCare)

  // hit initiation path
  val s0_read = isRead(io.cpu.req.bits.cmd)
  dataArb.io.in(3).valid := io.cpu.req.valid && likelyNeedsRead(io.cpu.req.bits)
  dataArb.io.in(3).bits := dataArb.io.in(1).bits
  dataArb.io.in(3).bits.write := false.B
  dataArb.io.in(3).bits.addr := Cat(io.cpu.req.bits.idx.getOrElse(io.cpu.req.bits.addr) >> tagLSB, io.cpu.req.bits.addr(tagLSB-1, 0))
  dataArb.io.in(3).bits.wordMask := {
    val mask = (subWordBytes.log2 until rowOffBits).foldLeft(1.U) { case (in, i) =>
      val upper_mask = Mux((i >= wordBytes.log2).B || io.cpu.req.bits.size <= i.U, 0.U,
        ((BigInt(1) << (1 << (i - subWordBytes.log2)))-1).U)
      val upper = Mux(io.cpu.req.bits.addr(i), in, 0.U) | upper_mask
      val lower = Mux(io.cpu.req.bits.addr(i), 0.U, in)
      upper ## lower
    }
    Fill(subWordBytes / eccBytes, mask)
  }
  dataArb.io.in(3).bits.eccMask := ~0.U((wordBytes / eccBytes).W)
  dataArb.io.in(3).bits.way_en := ~0.U(nWays.W)
  when (!dataArb.io.in(3).ready && s0_read) { io.cpu.req.ready := false.B }
  val s1_did_read = RegEnable(dataArb.io.in(3).ready && (io.cpu.req.valid && needsRead(io.cpu.req.bits)), s0_clk_en)
  val s1_read_mask = RegEnable(dataArb.io.in(3).bits.wordMask, s0_clk_en)
  metaArb.io.in(7).valid := io.cpu.req.valid
  metaArb.io.in(7).bits.write := false.B
  metaArb.io.in(7).bits.idx := dataArb.io.in(3).bits.addr(idxMSB, idxLSB)
  metaArb.io.in(7).bits.addr := io.cpu.req.bits.addr
  metaArb.io.in(7).bits.way_en := metaArb.io.in(4).bits.way_en
  metaArb.io.in(7).bits.data := metaArb.io.in(4).bits.data
  when (!metaArb.io.in(7).ready) { io.cpu.req.ready := false.B }

  // address translation
  val s1_cmd_uses_tlb = s1_readwrite || s1_flush_line || s1_req.cmd === M_WOK
  io.ptw <> tlb.io.ptw
  tlb.io.kill := io.cpu.s2_kill || s2_tlb_req_valid && io.tlb_port.s2_kill
  tlb.io.req.valid := s1_tlb_req_valid || s1_valid && !io.cpu.s1_kill && s1_cmd_uses_tlb
  tlb.io.req.bits := s1_tlb_req
  when (!tlb.io.req.ready && !tlb.io.ptw.resp.valid && !io.cpu.req.bits.phys) { io.cpu.req.ready := false.B }
  when (!s1_tlb_req_valid && s1_valid && s1_cmd_uses_tlb && tlb.io.resp.miss) { s1_nack := true.B }

  tlb.io.sfence.valid := s1_valid && !io.cpu.s1_kill && s1_sfence
  tlb.io.sfence.bits.rs1 := s1_req.size(0)
  tlb.io.sfence.bits.rs2 := s1_req.size(1)
  tlb.io.sfence.bits.asid := io.cpu.s1_data.data
  tlb.io.sfence.bits.addr := s1_req.addr
  tlb.io.sfence.bits.hv := s1_req.cmd === M_HFENCEV
  tlb.io.sfence.bits.hg := s1_req.cmd === M_HFENCEG

  io.tlb_port.req.ready := clock_en_reg
  io.tlb_port.s1_resp := tlb.io.resp
  when (s1_tlb_req_valid && s1_valid && !(s1_req.phys && s1_req.no_xcpt)) { s1_nack := true.B }

  pma_checker.io <> DontCare
  pma_checker.io.req.bits.passthrough := true.B
  pma_checker.io.req.bits.vaddr := s1_req.addr
  pma_checker.io.req.bits.size := s1_req.size
  pma_checker.io.req.bits.cmd := s1_req.cmd
  pma_checker.io.req.bits.prv := s1_req.dprv
  pma_checker.io.req.bits.v := s1_req.dv

  val s1_paddr = Cat(Mux(s1_tlb_req_valid, s1_req.addr(paddrBits-1, pgIdxBits), tlb.io.resp.paddr >> pgIdxBits), s1_req.addr(pgIdxBits-1, 0))
  val s1_victim_way = Wire(UInt())
  val (s1_hit_way, s1_hit_state, s1_meta) =
    if (usingDataScratchpad) {
      val baseAddr = p(LookupByHartId)(_.dcache.flatMap(_.scratch.map(_.U)), io_hartid.get) | io_mmio_address_prefix.get
      val inScratchpad = s1_paddr >= baseAddr && s1_paddr < baseAddr + (nSets * cacheBlockBytes).U
      val hitState = Mux(inScratchpad, ClientMetadata.maximum, ClientMetadata.onReset)
      val dummyMeta = L1Metadata(0.U, ClientMetadata.onReset)
      (inScratchpad, hitState, Seq(tECC.encode(dummyMeta.asUInt)))
    } else {
      val metaReq = metaArb.io.out
      val metaIdx = metaReq.bits.idx
      when (metaReq.valid && metaReq.bits.write) {
        val wmask = if (nWays == 1) Seq(true.B) else metaReq.bits.way_en.asBools
        tag_array.write(metaIdx, VecInit(Seq.fill(nWays)(metaReq.bits.data)), wmask)
      }
      val s1_meta = tag_array.read(metaIdx, metaReq.valid && !metaReq.bits.write)
      val s1_meta_uncorrected = s1_meta.map(tECC.decode(_).uncorrected.asTypeOf(new L1Metadata))
      val s1_tag = s1_paddr >> tagLSB
      val s1_meta_hit_way = s1_meta_uncorrected.map(r => r.coh.isValid() && r.tag === s1_tag).asUInt
      val s1_meta_hit_state = (
        s1_meta_uncorrected.map(r => Mux(r.tag === s1_tag && !s1_flush_valid, r.coh.asUInt, 0.U))
        .reduce (_|_)).asTypeOf(chiselTypeOf(ClientMetadata.onReset))
      (s1_meta_hit_way, s1_meta_hit_state, s1_meta)
    }
  val s1_data_way = WireDefault(if (nWays == 1) 1.U else Mux(inWriteback, releaseWay, s1_hit_way))
  val tl_d_data_encoded = Wire(chiselTypeOf(encodeData(tl_out.d.bits.data, false.B)))
  val s1_all_data_ways = VecInit(data.io.resp ++ (!cacheParams.separateUncachedResp).option(tl_d_data_encoded))
  val s1_mask_xwr = new StoreGen(s1_req.size, s1_req.addr, 0.U, wordBytes).mask
  val s1_mask = Mux(s1_req.cmd === M_PWR, io.cpu.s1_data.mask, s1_mask_xwr)
  // for partial writes, s1_data.mask must be a subset of s1_mask_xwr
  assert(!(s1_valid_masked && s1_req.cmd === M_PWR) || (s1_mask_xwr | ~io.cpu.s1_data.mask).andR)

  val s2_valid = RegNext(s1_valid_masked && !s1_sfence, init=false.B)
  val s2_valid_no_xcpt = s2_valid && !io.cpu.s2_xcpt.asUInt.orR
  val s2_probe = RegNext(s1_probe, init=false.B)
  val releaseInFlight = s1_probe || s2_probe || release_state =/= s_ready
  val s2_not_nacked_in_s1 = RegNext(!s1_nack)
  val s2_valid_not_nacked_in_s1 = s2_valid && s2_not_nacked_in_s1
  val s2_valid_masked = s2_valid_no_xcpt && s2_not_nacked_in_s1
  val s2_valid_not_killed = s2_valid_masked && !io.cpu.s2_kill
  val s2_req = Reg(chiselTypeOf(io.cpu.req.bits))
  val s2_cmd_flush_all = s2_req.cmd === M_FLUSH_ALL && !s2_req.size(0)
  val s2_cmd_flush_line = s2_req.cmd === M_FLUSH_ALL && s2_req.size(0)
  val s2_tlb_xcpt = Reg(chiselTypeOf(tlb.io.resp))
  val s2_pma = Reg(chiselTypeOf(tlb.io.resp))
  val s2_uncached_resp_addr = Reg(chiselTypeOf(s2_req.addr)) // should be DCE'd in synthesis
  when (s1_valid_not_nacked || s1_flush_valid) {
    s2_req := s1_req
    s2_req.addr := s1_paddr
    s2_tlb_xcpt := tlb.io.resp
    s2_pma := Mux(s1_tlb_req_valid, pma_checker.io.resp, tlb.io.resp)
  }
  val s2_vaddr = Cat(RegEnable(s1_vaddr, s1_valid_not_nacked || s1_flush_valid) >> tagLSB, s2_req.addr(tagLSB-1, 0))
  val s2_read = isRead(s2_req.cmd)
  val s2_write = isWrite(s2_req.cmd)
  val s2_readwrite = s2_read || s2_write
  val s2_flush_valid_pre_tag_ecc = RegNext(s1_flush_valid)
  val s1_meta_decoded = s1_meta.map(tECC.decode(_))
  val s1_meta_clk_en = s1_valid_not_nacked || s1_flush_valid || s1_probe
  val s2_meta_correctable_errors = s1_meta_decoded.map(m => RegEnable(m.correctable, s1_meta_clk_en)).asUInt
  val s2_meta_uncorrectable_errors = s1_meta_decoded.map(m => RegEnable(m.uncorrectable, s1_meta_clk_en)).asUInt
  val s2_meta_error_uncorrectable = s2_meta_uncorrectable_errors.orR
  val s2_meta_corrected = s1_meta_decoded.map(m => RegEnable(m.corrected, s1_meta_clk_en).asTypeOf(new L1Metadata))
  val s2_meta_error = (s2_meta_uncorrectable_errors | s2_meta_correctable_errors).orR
  val s2_flush_valid = s2_flush_valid_pre_tag_ecc && !s2_meta_error
  val s2_data = {
    val wordsPerRow = rowBits / subWordBits
    val en = s1_valid || inWriteback || io.cpu.replay_next
    val word_en = Mux(inWriteback, Fill(wordsPerRow, 1.U), Mux(s1_did_read, s1_read_mask, 0.U))
    val s1_way_words = s1_all_data_ways.map(_.grouped(dECC.width(eccBits) * (subWordBits / eccBits)))
    if (cacheParams.pipelineWayMux) {
      val s1_word_en = Mux(io.cpu.replay_next, 0.U, word_en)
      (for (i <- 0 until wordsPerRow) yield {
        val s2_way_en = RegEnable(Mux(s1_word_en(i), s1_data_way, 0.U), en)
        val s2_way_words = (0 until nWays).map(j => RegEnable(s1_way_words(j)(i), en && word_en(i)))
        (0 until nWays).map(j => Mux(s2_way_en(j), s2_way_words(j), 0.U)).reduce(_|_)
      }).asUInt
    } else {
      val s1_word_en = Mux(!io.cpu.replay_next, word_en, UIntToOH(uncachedResp.addr.extract(log2Up(rowBits/8)-1, log2Up(wordBytes)), wordsPerRow))
      (for (i <- 0 until wordsPerRow) yield {
        RegEnable(Mux1H(Mux(s1_word_en(i), s1_data_way, 0.U), s1_way_words.map(_(i))), en)
      }).asUInt
    }
  }
  val s2_probe_way = RegEnable(s1_hit_way, s1_probe)
  val s2_probe_state = RegEnable(s1_hit_state, s1_probe)
  val s2_hit_way = RegEnable(s1_hit_way, s1_valid_not_nacked)
  val s2_hit_state = RegEnable(s1_hit_state, s1_valid_not_nacked || s1_flush_valid)
  val s2_waw_hazard = RegEnable(s1_waw_hazard, s1_valid_not_nacked)
  val s2_store_merge = Wire(Bool())
  val s2_hit_valid = s2_hit_state.isValid()
  val (s2_hit, s2_grow_param, s2_new_hit_state) = s2_hit_state.onAccess(s2_req.cmd)
  val s2_data_decoded = decodeData(s2_data)
  val s2_word_idx = s2_req.addr.extract(log2Up(rowBits/8)-1, log2Up(wordBytes))
  val s2_data_error = s2_data_decoded.map(_.error).orR
  val s2_data_error_uncorrectable = s2_data_decoded.map(_.uncorrectable).orR
  val s2_data_corrected = (s2_data_decoded.map(_.corrected): Seq[UInt]).asUInt
  val s2_data_uncorrected = (s2_data_decoded.map(_.uncorrected): Seq[UInt]).asUInt
  val s2_valid_hit_maybe_flush_pre_data_ecc_and_waw = s2_valid_masked && !s2_meta_error && s2_hit
  val s2_no_alloc_hazard = if (!usingVM || pgIdxBits >= untagBits) false.B else {
    // make sure that any in-flight non-allocating accesses are ordered before
    // any allocating accesses.  this can only happen if aliasing is possible.
    val any_no_alloc_in_flight = Reg(Bool())
    when (!uncachedInFlight.asUInt.orR) { any_no_alloc_in_flight := false.B }
    when (s2_valid && s2_req.no_alloc) { any_no_alloc_in_flight := true.B }
    val s1_need_check = any_no_alloc_in_flight || s2_valid && s2_req.no_alloc

    val concerns = (uncachedInFlight zip uncachedReqs) :+ (s2_valid && s2_req.no_alloc, s2_req)
    val s1_uncached_hits = concerns.map { c =>
      val concern_wmask = new StoreGen(c._2.size, c._2.addr, 0.U, wordBytes).mask
      val addr_match = (c._2.addr ^ s1_paddr)(pgIdxBits+pgLevelBits-1, wordBytes.log2) === 0.U
      val mask_match = (concern_wmask & s1_mask_xwr).orR || c._2.cmd === M_PWR || s1_req.cmd === M_PWR
      val cmd_match = isWrite(c._2.cmd) || isWrite(s1_req.cmd)
      c._1 && s1_need_check && cmd_match && addr_match && mask_match
    }

    val s2_uncached_hits = RegEnable(s1_uncached_hits.asUInt, s1_valid_not_nacked)
    s2_uncached_hits.orR
  }
  val s2_valid_hit_pre_data_ecc_and_waw = s2_valid_hit_maybe_flush_pre_data_ecc_and_waw && s2_readwrite && !s2_no_alloc_hazard
  val s2_valid_flush_line = s2_valid_hit_maybe_flush_pre_data_ecc_and_waw && s2_cmd_flush_line
  val s2_valid_hit_pre_data_ecc = s2_valid_hit_pre_data_ecc_and_waw && (!s2_waw_hazard || s2_store_merge)
  val s2_valid_data_error = s2_valid_hit_pre_data_ecc_and_waw && s2_data_error
  val s2_valid_hit = s2_valid_hit_pre_data_ecc && !s2_data_error
  val s2_valid_miss = s2_valid_masked && s2_readwrite && !s2_meta_error && !s2_hit
  val s2_uncached = !s2_pma.cacheable || s2_req.no_alloc && !s2_pma.must_alloc && !s2_hit_valid
  val s2_valid_cached_miss = s2_valid_miss && !s2_uncached && !uncachedInFlight.asUInt.orR
  dontTouch(s2_valid_cached_miss)
  val s2_want_victimize = (!usingDataScratchpad).B && (s2_valid_cached_miss || s2_valid_flush_line || s2_valid_data_error || s2_flush_valid)
  val s2_cannot_victimize = !s2_flush_valid && io.cpu.s2_kill
  val s2_victimize = s2_want_victimize && !s2_cannot_victimize
  val s2_valid_uncached_pending = s2_valid_miss && s2_uncached && !uncachedInFlight.asUInt.andR
  val s2_victim_way = UIntToOH(RegEnable(s1_victim_way, s1_valid_not_nacked || s1_flush_valid))
  val s2_victim_or_hit_way = Mux(s2_hit_valid, s2_hit_way, s2_victim_way)
  val s2_victim_tag = Mux(s2_valid_data_error || s2_valid_flush_line, s2_req.addr(paddrBits-1, tagLSB), Mux1H(s2_victim_way, s2_meta_corrected).tag)
  val s2_victim_state = Mux(s2_hit_valid, s2_hit_state, Mux1H(s2_victim_way, s2_meta_corrected).coh)

  val (s2_prb_ack_data, s2_report_param, probeNewCoh)= s2_probe_state.onProbe(probe_bits.param)
  val (s2_victim_dirty, s2_shrink_param, voluntaryNewCoh) = s2_victim_state.onCacheControl(M_FLUSH)
  dontTouch(s2_victim_dirty)
  val s2_update_meta = s2_hit_state =/= s2_new_hit_state
  val s2_dont_nack_uncached = s2_valid_uncached_pending && tl_out_a.ready
  val s2_dont_nack_misc = s2_valid_masked && !s2_meta_error &&
    (supports_flush.B && s2_cmd_flush_all && flushed && !flushing ||
     supports_flush.B && s2_cmd_flush_line && !s2_hit ||
     s2_req.cmd === M_WOK)
  io.cpu.s2_nack := s2_valid_no_xcpt && !s2_dont_nack_uncached && !s2_dont_nack_misc && !s2_valid_hit
  when (io.cpu.s2_nack || (s2_valid_hit_pre_data_ecc_and_waw && s2_update_meta)) { s1_nack := true.B }

  // tag updates on ECC errors
  val s2_first_meta_corrected = PriorityMux(s2_meta_correctable_errors, s2_meta_corrected)
  metaArb.io.in(1).valid := s2_meta_error && (s2_valid_masked || s2_flush_valid_pre_tag_ecc || s2_probe)
  metaArb.io.in(1).bits.write := true.B
  metaArb.io.in(1).bits.way_en := s2_meta_uncorrectable_errors | Mux(s2_meta_error_uncorrectable, 0.U, PriorityEncoderOH(s2_meta_correctable_errors))
  metaArb.io.in(1).bits.idx := Mux(s2_probe, probeIdx(probe_bits), s2_vaddr(idxMSB, idxLSB))
  metaArb.io.in(1).bits.addr := Cat(io.cpu.req.bits.addr >> untagBits, metaArb.io.in(1).bits.idx << blockOffBits)
  metaArb.io.in(1).bits.data := tECC.encode {
    val new_meta = WireDefault(s2_first_meta_corrected)
    when (s2_meta_error_uncorrectable) { new_meta.coh := ClientMetadata.onReset }
    new_meta.asUInt
  }

  // tag updates on hit
  metaArb.io.in(2).valid := s2_valid_hit_pre_data_ecc_and_waw && s2_update_meta
  metaArb.io.in(2).bits.write := !io.cpu.s2_kill
  metaArb.io.in(2).bits.way_en := s2_victim_or_hit_way
  metaArb.io.in(2).bits.idx := s2_vaddr(idxMSB, idxLSB)
  metaArb.io.in(2).bits.addr := Cat(io.cpu.req.bits.addr >> untagBits, s2_vaddr(idxMSB, 0))
  metaArb.io.in(2).bits.data := tECC.encode(L1Metadata(s2_req.addr >> tagLSB, s2_new_hit_state).asUInt)

  // load reservations and TL error reporting
  val s2_lr = (usingAtomics && !usingDataScratchpad).B && s2_req.cmd === M_XLR
  val s2_sc = (usingAtomics && !usingDataScratchpad).B && s2_req.cmd === M_XSC
  val lrscCount = RegInit(0.U)
  val lrscValid = lrscCount > lrscBackoff.U
  val lrscBackingOff = lrscCount > 0.U && !lrscValid
  val lrscAddr = Reg(UInt())
  val lrscAddrMatch = lrscAddr === (s2_req.addr >> blockOffBits)
  val s2_sc_fail = s2_sc && !(lrscValid && lrscAddrMatch)
  when ((s2_valid_hit && s2_lr && !cached_grant_wait || s2_valid_cached_miss) && !io.cpu.s2_kill) {
    lrscCount := Mux(s2_hit, (lrscCycles - 1).U, 0.U)
    lrscAddr := s2_req.addr >> blockOffBits
  }
  when (lrscCount > 0.U) { lrscCount := lrscCount - 1.U }
  when (s2_valid_not_killed && lrscValid) { lrscCount := lrscBackoff.U }
  when (s1_probe) { lrscCount := 0.U }

  // don't perform data correction if it might clobber a recent store
  val s2_correct = s2_data_error && !any_pstore_valid && !RegNext(any_pstore_valid || s2_valid) && usingDataScratchpad.B
  // pending store buffer
  val s2_valid_correct = s2_valid_hit_pre_data_ecc_and_waw && s2_correct && !io.cpu.s2_kill
  def s2_store_valid_pre_kill = s2_valid_hit && s2_write && !s2_sc_fail
  def s2_store_valid = s2_store_valid_pre_kill && !io.cpu.s2_kill
  val pstore1_cmd = RegEnable(s1_req.cmd, s1_valid_not_nacked && s1_write)
  val pstore1_addr = RegEnable(s1_vaddr, s1_valid_not_nacked && s1_write)
  val pstore1_data = RegEnable(io.cpu.s1_data.data, s1_valid_not_nacked && s1_write)
  val pstore1_way = RegEnable(s1_hit_way, s1_valid_not_nacked && s1_write)
  val pstore1_mask = RegEnable(s1_mask, s1_valid_not_nacked && s1_write)
  val pstore1_storegen_data = WireDefault(pstore1_data)
  val pstore1_rmw = usingRMW.B && RegEnable(needsRead(s1_req), s1_valid_not_nacked && s1_write)
  val pstore1_merge_likely = s2_valid_not_nacked_in_s1 && s2_write && s2_store_merge
  val pstore1_merge = s2_store_valid && s2_store_merge
  val pstore2_valid = RegInit(false.B)
  val pstore_drain_opportunistic = !(io.cpu.req.valid && likelyNeedsRead(io.cpu.req.bits)) && !(s1_valid && s1_waw_hazard)
  val pstore_drain_on_miss = releaseInFlight || RegNext(io.cpu.s2_nack)
  val pstore1_held = RegInit(false.B)
  val pstore1_valid_likely = s2_valid && s2_write || pstore1_held
  def pstore1_valid_not_rmw(s2_kill: Bool) = s2_valid_hit_pre_data_ecc && s2_write && !s2_kill || pstore1_held
  val pstore1_valid = s2_store_valid || pstore1_held
  any_pstore_valid := pstore1_held || pstore2_valid
  val pstore_drain_structural = pstore1_valid_likely && pstore2_valid && ((s1_valid && s1_write) || pstore1_rmw)
  assert(pstore1_rmw || pstore1_valid_not_rmw(io.cpu.s2_kill) === pstore1_valid)
  ccover(pstore_drain_structural, "STORE_STRUCTURAL_HAZARD", "D$ read-modify-write structural hazard")
  ccover(pstore1_valid && pstore_drain_on_miss, "STORE_DRAIN_ON_MISS", "D$ store buffer drain on miss")
  ccover(s1_valid_not_nacked && s1_waw_hazard, "WAW_HAZARD", "D$ write-after-write hazard")
  def should_pstore_drain(truly: Bool) = {
    val s2_kill = truly && io.cpu.s2_kill
    !pstore1_merge_likely &&
    (usingRMW.B && pstore_drain_structural ||
      (((pstore1_valid_not_rmw(s2_kill) && !pstore1_rmw) || pstore2_valid) && (pstore_drain_opportunistic || pstore_drain_on_miss)))
  }
  val pstore_drain = should_pstore_drain(true.B)
  pstore1_held := (s2_store_valid && !s2_store_merge || pstore1_held) && pstore2_valid && !pstore_drain
  val advance_pstore1 = (pstore1_valid || s2_valid_correct) && (pstore2_valid === pstore_drain)
  pstore2_valid := pstore2_valid && !pstore_drain || advance_pstore1
  val pstore2_addr = RegEnable(Mux(s2_correct, s2_vaddr, pstore1_addr), advance_pstore1)
  val pstore2_way = RegEnable(Mux(s2_correct, s2_hit_way, pstore1_way), advance_pstore1)
  val pstore2_storegen_data = {
    for (i <- 0 until wordBytes)
      yield RegEnable(pstore1_storegen_data(8*(i+1)-1, 8*i), advance_pstore1 || pstore1_merge && pstore1_mask(i))
  }.asUInt
  val pstore2_storegen_mask = {
    val mask = Reg(UInt(wordBytes.W))
    when (advance_pstore1 || pstore1_merge) {
      val mergedMask = pstore1_mask | Mux(pstore1_merge, mask, 0.U)
      mask := ~Mux(s2_correct, 0.U, ~mergedMask)
    }
    mask
  }
  s2_store_merge := (if (eccBytes == 1) false.B else {
    ccover(pstore1_merge, "STORE_MERGED", "D$ store merged")
    // only merge stores to ECC granules that are already stored-to, to avoid
    // WAW hazards
    val wordMatch = (eccMask(pstore2_storegen_mask) | ~eccMask(pstore1_mask)).andR
    val idxMatch = s2_vaddr(untagBits-1, log2Ceil(wordBytes)) === pstore2_addr(untagBits-1, log2Ceil(wordBytes))
    val tagMatch = (s2_hit_way & pstore2_way).orR
    pstore2_valid && wordMatch && idxMatch && tagMatch
  })
  dataArb.io.in(0).valid := should_pstore_drain(false.B)
  dataArb.io.in(0).bits.write := pstore_drain
  dataArb.io.in(0).bits.addr := Mux(pstore2_valid, pstore2_addr, pstore1_addr)
  dataArb.io.in(0).bits.way_en := Mux(pstore2_valid, pstore2_way, pstore1_way)
  dataArb.io.in(0).bits.wdata := encodeData(Fill(rowWords, Mux(pstore2_valid, pstore2_storegen_data, pstore1_data)), false.B)
  dataArb.io.in(0).bits.wordMask := {
    val eccMask = dataArb.io.in(0).bits.eccMask.asBools.grouped(subWordBytes/eccBytes).map(_.orR).toSeq.asUInt
    val wordMask = UIntToOH(Mux(pstore2_valid, pstore2_addr, pstore1_addr).extract(rowOffBits-1, wordBytes.log2))
    FillInterleaved(wordBytes/subWordBytes, wordMask) & Fill(rowBytes/wordBytes, eccMask)
  }
  dataArb.io.in(0).bits.eccMask := eccMask(Mux(pstore2_valid, pstore2_storegen_mask, pstore1_mask))

  // store->load RAW hazard detection
  def s1Depends(addr: UInt, mask: UInt) =
    addr(idxMSB, wordOffBits) === s1_vaddr(idxMSB, wordOffBits) &&
    Mux(s1_write, (eccByteMask(mask) & eccByteMask(s1_mask_xwr)).orR, (mask & s1_mask_xwr).orR)
  val s1_hazard =
    (pstore1_valid_likely && s1Depends(pstore1_addr, pstore1_mask)) ||
     (pstore2_valid && s1Depends(pstore2_addr, pstore2_storegen_mask))
  val s1_raw_hazard = s1_read && s1_hazard
  s1_waw_hazard := (if (eccBytes == 1) false.B else {
    ccover(s1_valid_not_nacked && s1_waw_hazard, "WAW_HAZARD", "D$ write-after-write hazard")
    s1_write && (s1_hazard || needsRead(s1_req) && !s1_did_read)
  })
  when (s1_valid && s1_raw_hazard) { s1_nack := true.B }

  // performance hints to processor
  io.cpu.s2_nack_cause_raw := RegNext(s1_raw_hazard) || !(!s2_waw_hazard || s2_store_merge)

  // Prepare a TileLink request message that initiates a transaction
  val a_source = PriorityEncoder(~uncachedInFlight.asUInt << mmioOffset) // skip the MSHR
  val acquire_address = (s2_req.addr >> idxLSB) << idxLSB
  val access_address = s2_req.addr
  val a_size = s2_req.size
  val a_data = Fill(beatWords, pstore1_data)
  val a_mask = pstore1_mask << (access_address.extract(beatBytes.log2-1, wordBytes.log2) << 3)
  val get     = edge.Get(a_source, access_address, a_size)._2
  val put     = edge.Put(a_source, access_address, a_size, a_data)._2
  val putpartial = edge.Put(a_source, access_address, a_size, a_data, a_mask)._2
  val atomics = if (edge.manager.anySupportLogical) {
    MuxLookup(s2_req.cmd, WireDefault(0.U.asTypeOf(new TLBundleA(edge.bundle))))(Array(
      M_XA_SWAP -> edge.Logical(a_source, access_address, a_size, a_data, TLAtomics.SWAP)._2,
      M_XA_XOR  -> edge.Logical(a_source, access_address, a_size, a_data, TLAtomics.XOR) ._2,
      M_XA_OR   -> edge.Logical(a_source, access_address, a_size, a_data, TLAtomics.OR)  ._2,
      M_XA_AND  -> edge.Logical(a_source, access_address, a_size, a_data, TLAtomics.AND) ._2,
      M_XA_ADD  -> edge.Arithmetic(a_source, access_address, a_size, a_data, TLAtomics.ADD)._2,
      M_XA_MIN  -> edge.Arithmetic(a_source, access_address, a_size, a_data, TLAtomics.MIN)._2,
      M_XA_MAX  -> edge.Arithmetic(a_source, access_address, a_size, a_data, TLAtomics.MAX)._2,
      M_XA_MINU -> edge.Arithmetic(a_source, access_address, a_size, a_data, TLAtomics.MINU)._2,
      M_XA_MAXU -> edge.Arithmetic(a_source, access_address, a_size, a_data, TLAtomics.MAXU)._2))
  } else {
    // If no managers support atomics, assert fail if processor asks for them
    assert (!(tl_out_a.valid && s2_read && s2_write && s2_uncached))
    WireDefault(new TLBundleA(edge.bundle), DontCare)
  }

  tl_out_a.valid := !io.cpu.s2_kill &&
    (s2_valid_uncached_pending ||
      (s2_valid_cached_miss &&
       !(release_ack_wait && (s2_req.addr ^ release_ack_addr)(((pgIdxBits + pgLevelBits) min paddrBits) - 1, idxLSB) === 0.U) &&
       (cacheParams.acquireBeforeRelease.B && !release_ack_wait && release_queue_empty || !s2_victim_dirty)))
  tl_out_a.bits := Mux(!s2_uncached, acquire(s2_vaddr, s2_req.addr, s2_grow_param),
    Mux(!s2_write, get,
    Mux(s2_req.cmd === M_PWR, putpartial,
    Mux(!s2_read, put, atomics))))

  // Drive APROT Bits
  tl_out_a.bits.user.lift(AMBAProt).foreach { x =>
    val user_bit_cacheable = s2_pma.cacheable

    x.privileged  := s2_req.dprv === PRV.M.U || user_bit_cacheable
    // if the address is cacheable, enable outer caches
    x.bufferable  := user_bit_cacheable
    x.modifiable  := user_bit_cacheable
    x.readalloc   := user_bit_cacheable
    x.writealloc  := user_bit_cacheable

    // Following are always tied off
    x.fetch       := false.B
    x.secure      := true.B
  }

  // Set pending bits for outstanding TileLink transaction
  val a_sel = UIntToOH(a_source, maxUncachedInFlight+mmioOffset) >> mmioOffset
  when (tl_out_a.fire) {
    when (s2_uncached) {
      (a_sel.asBools zip (uncachedInFlight zip uncachedReqs)) foreach { case (s, (f, r)) =>
        when (s) {
          f := true.B
          r := s2_req
          r.cmd := Mux(s2_write, Mux(s2_req.cmd === M_PWR, M_PWR, M_XWR), M_XRD)
        }
      }
    }.otherwise {
      cached_grant_wait := true.B
      refill_way := s2_victim_or_hit_way
    }
  }

  // grant
  val (d_first, d_last, d_done, d_address_inc) = edge.addr_inc(tl_out.d)
  val (d_opc, grantIsUncached, grantIsUncachedData) = {
    val uncachedGrantOpcodesSansData = Seq(AccessAck, HintAck)
    val uncachedGrantOpcodesWithData = Seq(AccessAckData)
    val uncachedGrantOpcodes = uncachedGrantOpcodesWithData ++ uncachedGrantOpcodesSansData
    val whole_opc = tl_out.d.bits.opcode
    if (usingDataScratchpad) {
      assert(!tl_out.d.valid || whole_opc.isOneOf(uncachedGrantOpcodes))
      // the only valid TL-D messages are uncached, so we can do some pruning
      val opc = whole_opc(uncachedGrantOpcodes.map(_.getWidth).max - 1, 0)
      val data = DecodeLogic(opc, uncachedGrantOpcodesWithData, uncachedGrantOpcodesSansData)
      (opc, true.B, data)
    } else {
      (whole_opc, whole_opc.isOneOf(uncachedGrantOpcodes), whole_opc.isOneOf(uncachedGrantOpcodesWithData))
    }
  }
  tl_d_data_encoded := encodeData(tl_out.d.bits.data, tl_out.d.bits.corrupt && !io.ptw.customCSRs.suppressCorruptOnGrantData && !grantIsUncached)
  val grantIsCached = d_opc.isOneOf(Grant, GrantData)
  val grantIsVoluntary = d_opc === ReleaseAck // Clears a different pending bit
  val grantIsRefill = d_opc === GrantData     // Writes the data array
  val grantInProgress = RegInit(false.B)
  val blockProbeAfterGrantCount = RegInit(0.U)
  when (blockProbeAfterGrantCount > 0.U) { blockProbeAfterGrantCount := blockProbeAfterGrantCount - 1.U }
  val canAcceptCachedGrant = !release_state.isOneOf(s_voluntary_writeback, s_voluntary_write_meta, s_voluntary_release)
  tl_out.d.ready := Mux(grantIsCached, (!d_first || tl_out.e.ready) && canAcceptCachedGrant, true.B)
  val uncachedRespIdxOH = UIntToOH(tl_out.d.bits.source, maxUncachedInFlight+mmioOffset) >> mmioOffset
  uncachedResp := Mux1H(uncachedRespIdxOH, uncachedReqs)
  when (tl_out.d.fire) {
    when (grantIsCached) {
      grantInProgress := true.B
      assert(cached_grant_wait, "A GrantData was unexpected by the dcache.")
      when(d_last) {
        cached_grant_wait := false.B
        grantInProgress := false.B
        blockProbeAfterGrantCount := (blockProbeAfterGrantCycles - 1).U
        replacer.miss
      }
    } .elsewhen (grantIsUncached) {
      (uncachedRespIdxOH.asBools zip uncachedInFlight) foreach { case (s, f) =>
        when (s && d_last) {
          assert(f, "An AccessAck was unexpected by the dcache.") // TODO must handle Ack coming back on same cycle!
          f := false.B
        }
      }
      when (grantIsUncachedData) {
        if (!cacheParams.separateUncachedResp) {
          if (!cacheParams.pipelineWayMux)
            s1_data_way := 1.U << nWays
          s2_req.cmd := M_XRD
          s2_req.size := uncachedResp.size
          s2_req.signed := uncachedResp.signed
          s2_req.tag := uncachedResp.tag
          s2_req.addr := {
            require(rowOffBits >= beatOffBits)
            val dontCareBits = s1_paddr >> rowOffBits << rowOffBits
            dontCareBits | uncachedResp.addr(beatOffBits-1, 0)
          }
          s2_uncached_resp_addr := uncachedResp.addr
        }
      }
    } .elsewhen (grantIsVoluntary) {
      assert(release_ack_wait, "A ReleaseAck was unexpected by the dcache.") // TODO should handle Ack coming back on same cycle!
      release_ack_wait := false.B
    }
  }

  // Finish TileLink transaction by issuing a GrantAck
  tl_out.e.valid := tl_out.d.valid && d_first && grantIsCached && canAcceptCachedGrant
  tl_out.e.bits := edge.GrantAck(tl_out.d.bits)
  assert(tl_out.e.fire === (tl_out.d.fire && d_first && grantIsCached))

  // data refill
  // note this ready-valid signaling ignores E-channel backpressure, which
  // benignly means the data RAM might occasionally be redundantly written
  dataArb.io.in(1).valid := tl_out.d.valid && grantIsRefill && canAcceptCachedGrant
  when (grantIsRefill && !dataArb.io.in(1).ready) {
    tl_out.e.valid := false.B
    tl_out.d.ready := false.B
  }
  if (!usingDataScratchpad) {
    dataArb.io.in(1).bits.write := true.B
    dataArb.io.in(1).bits.addr :=  (s2_vaddr >> idxLSB) << idxLSB | d_address_inc
    dataArb.io.in(1).bits.way_en := refill_way
    dataArb.io.in(1).bits.wdata := tl_d_data_encoded
    dataArb.io.in(1).bits.wordMask := ~0.U((rowBytes / subWordBytes).W)
    dataArb.io.in(1).bits.eccMask := ~0.U((wordBytes / eccBytes).W)
  } else {
    dataArb.io.in(1).bits := dataArb.io.in(0).bits
  }

  // tag updates on refill
  // ignore backpressure from metaArb, which can only be caused by tag ECC
  // errors on hit-under-miss.  failing to write the new tag will leave the
  // line invalid, so we'll simply request the line again later.
  metaArb.io.in(3).valid := grantIsCached && d_done && !tl_out.d.bits.denied
  metaArb.io.in(3).bits.write := true.B
  metaArb.io.in(3).bits.way_en := refill_way
  metaArb.io.in(3).bits.idx := s2_vaddr(idxMSB, idxLSB)
  metaArb.io.in(3).bits.addr := Cat(io.cpu.req.bits.addr >> untagBits, s2_vaddr(idxMSB, 0))
  metaArb.io.in(3).bits.data := tECC.encode(L1Metadata(s2_req.addr >> tagLSB, s2_hit_state.onGrant(s2_req.cmd, tl_out.d.bits.param)).asUInt)

  if (!cacheParams.separateUncachedResp) {
    // don't accept uncached grants if there's a structural hazard on s2_data...
    val blockUncachedGrant = Reg(Bool())
    blockUncachedGrant := dataArb.io.out.valid
    when (grantIsUncachedData && (blockUncachedGrant || s1_valid)) {
      tl_out.d.ready := false.B
      // ...but insert bubble to guarantee grant's eventual forward progress
      when (tl_out.d.valid) {
        io.cpu.req.ready := false.B
        dataArb.io.in(1).valid := true.B
        dataArb.io.in(1).bits.write := false.B
        blockUncachedGrant := !dataArb.io.in(1).ready
      }
    }
  }
  ccover(tl_out.d.valid && !tl_out.d.ready, "BLOCK_D", "D$ D-channel blocked")

  // Handle an incoming TileLink Probe message
  val block_probe_for_core_progress = blockProbeAfterGrantCount > 0.U || lrscValid
  val block_probe_for_pending_release_ack = release_ack_wait && (tl_out.b.bits.address ^ release_ack_addr)(((pgIdxBits + pgLevelBits) min paddrBits) - 1, idxLSB) === 0.U
  val block_probe_for_ordering = releaseInFlight || block_probe_for_pending_release_ack || grantInProgress
  metaArb.io.in(6).valid := tl_out.b.valid && (!block_probe_for_core_progress || lrscBackingOff)
  tl_out.b.ready := metaArb.io.in(6).ready && !(block_probe_for_core_progress || block_probe_for_ordering || s1_valid || s2_valid)
  metaArb.io.in(6).bits.write := false.B
  metaArb.io.in(6).bits.idx := probeIdx(tl_out.b.bits)
  metaArb.io.in(6).bits.addr := Cat(io.cpu.req.bits.addr >> paddrBits, tl_out.b.bits.address)
  metaArb.io.in(6).bits.way_en := metaArb.io.in(4).bits.way_en
  metaArb.io.in(6).bits.data := metaArb.io.in(4).bits.data

  // replacement policy
  s1_victim_way := (if (replacer.perSet && nWays > 1) {
    val repl_array = Mem(nSets, UInt(replacer.nBits.W))
    val s1_repl_idx = s1_req.addr(idxBits+blockOffBits-1, blockOffBits)
    val s2_repl_idx = s2_vaddr(idxBits+blockOffBits-1, blockOffBits)
    val s2_repl_state = Reg(UInt(replacer.nBits.W))
    val s2_new_repl_state = replacer.get_next_state(s2_repl_state, OHToUInt(s2_hit_way))
    val s2_repl_wen = s2_valid_masked && s2_hit_way.orR && s2_repl_state =/= s2_new_repl_state
    val s1_repl_state = Mux(s2_repl_wen && s2_repl_idx === s1_repl_idx, s2_new_repl_state, repl_array(s1_repl_idx))
    when (s1_valid_not_nacked) { s2_repl_state := s1_repl_state }

    val waddr = Mux(resetting, flushCounter(idxBits-1, 0), s2_repl_idx)
    val wdata = Mux(resetting, 0.U, s2_new_repl_state)
    val wen = resetting || s2_repl_wen
    when (wen) { repl_array(waddr) := wdata }

    replacer.get_replace_way(s1_repl_state)
  } else {
    replacer.way
  })

  // release
  val (c_first, c_last, releaseDone, c_count) = edge.count(tl_out_c)
  val releaseRejected = Wire(Bool())
  val s1_release_data_valid = RegNext(dataArb.io.in(2).fire)
  val s2_release_data_valid = RegNext(s1_release_data_valid && !releaseRejected)
  releaseRejected := s2_release_data_valid && !tl_out_c.fire
  val releaseDataBeat = Cat(0.U, c_count) + Mux(releaseRejected, 0.U, s1_release_data_valid + Cat(0.U, s2_release_data_valid))

  val nackResponseMessage = edge.ProbeAck(b = probe_bits, reportPermissions = TLPermissions.NtoN)
  val cleanReleaseMessage = edge.ProbeAck(b = probe_bits, reportPermissions = s2_report_param)
  val dirtyReleaseMessage = edge.ProbeAck(b = probe_bits, reportPermissions = s2_report_param, data = 0.U)

  tl_out_c.valid := (s2_release_data_valid || (!cacheParams.silentDrop.B && release_state === s_voluntary_release)) && !(c_first && release_ack_wait)
  tl_out_c.bits := nackResponseMessage
  val newCoh = WireDefault(probeNewCoh)
  releaseWay := s2_probe_way

  if (!usingDataScratchpad) {
    when (s2_victimize) {
      assert(s2_valid_flush_line || s2_flush_valid || io.cpu.s2_nack)
      val discard_line = s2_valid_flush_line && s2_req.size(1) || s2_flush_valid && flushing_req.size(1)
      release_state := Mux(s2_victim_dirty && !discard_line, s_voluntary_writeback,
                       Mux(!cacheParams.silentDrop.B && !release_ack_wait && release_queue_empty && s2_victim_state.isValid() && (s2_valid_flush_line || s2_flush_valid || s2_readwrite && !s2_hit_valid), s_voluntary_release,
                       s_voluntary_write_meta))
      probe_bits := addressToProbe(s2_vaddr, Cat(s2_victim_tag, s2_req.addr(tagLSB-1, idxLSB)) << idxLSB)
    }
    when (s2_probe) {
      val probeNack = WireDefault(true.B)
      when (s2_meta_error) {
        release_state := s_probe_retry
      }.elsewhen (s2_prb_ack_data) {
        release_state := s_probe_rep_dirty
      }.elsewhen (s2_probe_state.isValid()) {
        tl_out_c.valid := true.B
        tl_out_c.bits := cleanReleaseMessage
        release_state := Mux(releaseDone, s_probe_write_meta, s_probe_rep_clean)
      }.otherwise {
        tl_out_c.valid := true.B
        probeNack := !releaseDone
        release_state := Mux(releaseDone, s_ready, s_probe_rep_miss)
      }
      when (probeNack) { s1_nack := true.B }
    }
    when (release_state === s_probe_retry) {
      metaArb.io.in(6).valid := true.B
      metaArb.io.in(6).bits.idx := probeIdx(probe_bits)
      metaArb.io.in(6).bits.addr := Cat(io.cpu.req.bits.addr >> paddrBits, probe_bits.address)
      when (metaArb.io.in(6).ready) {
        release_state := s_ready
        s1_probe := true.B
      }
    }
    when (release_state === s_probe_rep_miss) {
      tl_out_c.valid := true.B
      when (releaseDone) { release_state := s_ready }
    }
    when (release_state === s_probe_rep_clean) {
      tl_out_c.valid := true.B
      tl_out_c.bits := cleanReleaseMessage
      when (releaseDone) { release_state := s_probe_write_meta }
    }
    when (release_state === s_probe_rep_dirty) {
      tl_out_c.bits := dirtyReleaseMessage
      when (releaseDone) { release_state := s_probe_write_meta }
    }
    when (release_state.isOneOf(s_voluntary_writeback, s_voluntary_write_meta, s_voluntary_release)) {
      when (release_state === s_voluntary_release) {
        tl_out_c.bits := edge.Release(fromSource = 0.U,
                                      toAddress = 0.U,
                                      lgSize = lgCacheBlockBytes.U,
                                      shrinkPermissions = s2_shrink_param)._2
      }.otherwise {
        tl_out_c.bits := edge.Release(fromSource = 0.U,
                                      toAddress = 0.U,
                                      lgSize = lgCacheBlockBytes.U,
                                      shrinkPermissions = s2_shrink_param,
                                      data = 0.U)._2
      }
      newCoh := voluntaryNewCoh
      releaseWay := s2_victim_or_hit_way
      when (releaseDone) { release_state := s_voluntary_write_meta }
      when (tl_out_c.fire && c_first) {
        release_ack_wait := true.B
        release_ack_addr := probe_bits.address
      }
    }
    tl_out_c.bits.source := probe_bits.source
    tl_out_c.bits.address := probe_bits.address
    tl_out_c.bits.data := s2_data_corrected
    tl_out_c.bits.corrupt := inWriteback && s2_data_error_uncorrectable
  }

  tl_out_c.bits.user.lift(AMBAProt).foreach { x =>
    x.fetch       := false.B
    x.secure      := true.B
    x.privileged  := true.B
    x.bufferable  := true.B
    x.modifiable  := true.B
    x.readalloc   := true.B
    x.writealloc  := true.B
  }

  dataArb.io.in(2).valid := inWriteback && releaseDataBeat < refillCycles.U
  dataArb.io.in(2).bits := dataArb.io.in(1).bits
  dataArb.io.in(2).bits.write := false.B
  dataArb.io.in(2).bits.addr := (probeIdx(probe_bits) << blockOffBits) | (releaseDataBeat(log2Up(refillCycles)-1,0) << rowOffBits)
  dataArb.io.in(2).bits.wordMask := ~0.U((rowBytes / subWordBytes).W)
  dataArb.io.in(2).bits.eccMask := ~0.U((wordBytes / eccBytes).W)
  dataArb.io.in(2).bits.way_en := ~0.U(nWays.W)

  metaArb.io.in(4).valid := release_state.isOneOf(s_voluntary_write_meta, s_probe_write_meta)
  metaArb.io.in(4).bits.write := true.B
  metaArb.io.in(4).bits.way_en := releaseWay
  metaArb.io.in(4).bits.idx := probeIdx(probe_bits)
  metaArb.io.in(4).bits.addr := Cat(io.cpu.req.bits.addr >> untagBits, probe_bits.address(idxMSB, 0))
  metaArb.io.in(4).bits.data := tECC.encode(L1Metadata(tl_out_c.bits.address >> tagLSB, newCoh).asUInt)
  when (metaArb.io.in(4).fire) { release_state := s_ready }

  // cached response
  (io.cpu.resp.bits: Data).waiveAll :<>= (s2_req: Data).waiveAll
  io.cpu.resp.bits.has_data := s2_read
  io.cpu.resp.bits.replay := false.B
  io.cpu.s2_uncached := s2_uncached && !s2_hit
  io.cpu.s2_paddr := s2_req.addr
  io.cpu.s2_gpa := s2_tlb_xcpt.gpa
  io.cpu.s2_gpa_is_pte := s2_tlb_xcpt.gpa_is_pte

  // report whether there are any outstanding accesses.  disregard any
  // manager-port accesses, since they don't affect local memory ordering.
  val s1_isManagerPortAccess = s1_req.no_xcpt
  val s2_isManagerPortAccess = s2_req.no_xcpt
  io.cpu.ordered := !(s1_valid && !s1_isManagerPortAccess || s2_valid && !s2_isManagerPortAccess || cached_grant_wait || uncachedInFlight.asUInt.orR)
  io.cpu.store_pending := (cached_grant_wait && isWrite(s2_req.cmd)) || uncachedInFlight.asUInt.orR

  val s1_xcpt_valid = tlb.io.req.valid && !s1_isManagerPortAccess && !s1_nack
  io.cpu.s2_xcpt := Mux(RegNext(s1_xcpt_valid), s2_tlb_xcpt, 0.U.asTypeOf(s2_tlb_xcpt))

  if (usingDataScratchpad) {
    assert(!(s2_valid_masked && s2_req.cmd.isOneOf(M_XLR, M_XSC)))
  } else {
    ccover(tl_out.b.valid && !tl_out.b.ready, "BLOCK_B", "D$ B-channel blocked")
  }

  // uncached response
  val s1_uncached_data_word = {
    val word_idx = uncachedResp.addr.extract(log2Up(rowBits/8)-1, log2Up(wordBytes))
    val words = tl_out.d.bits.data.grouped(wordBits)
    words(word_idx)
  }
  val s2_uncached_data_word = RegEnable(s1_uncached_data_word, io.cpu.replay_next)
  val doUncachedResp = RegNext(io.cpu.replay_next)
  io.cpu.resp.valid := (s2_valid_hit_pre_data_ecc || doUncachedResp) && !s2_data_error
  io.cpu.replay_next := tl_out.d.fire && grantIsUncachedData && !cacheParams.separateUncachedResp.B
  when (doUncachedResp) {
    assert(!s2_valid_hit)
    io.cpu.resp.bits.replay := true.B
    io.cpu.resp.bits.addr := s2_uncached_resp_addr
  }

  io.cpu.uncached_resp.map { resp =>
    resp.valid := tl_out.d.valid && grantIsUncachedData
    resp.bits.tag := uncachedResp.tag
    resp.bits.size := uncachedResp.size
    resp.bits.signed := uncachedResp.signed
    resp.bits.data := new LoadGen(uncachedResp.size, uncachedResp.signed, uncachedResp.addr, s1_uncached_data_word, false.B, wordBytes).data
    resp.bits.data_raw := s1_uncached_data_word
    when (grantIsUncachedData && !resp.ready) {
      tl_out.d.ready := false.B
    }
  }

  // load data subword mux/sign extension
  val s2_data_word = (0 until rowBits by wordBits).map(i => s2_data_uncorrected(wordBits+i-1,i)).reduce(_|_)
  val s2_data_word_corrected = (0 until rowBits by wordBits).map(i => s2_data_corrected(wordBits+i-1,i)).reduce(_|_)
  val s2_data_word_possibly_uncached = Mux(cacheParams.pipelineWayMux.B && doUncachedResp, s2_uncached_data_word, 0.U) | s2_data_word
  val loadgen = new LoadGen(s2_req.size, s2_req.signed, s2_req.addr, s2_data_word_possibly_uncached, s2_sc, wordBytes)
  io.cpu.resp.bits.data := loadgen.data | s2_sc_fail
  io.cpu.resp.bits.data_word_bypass := loadgen.wordData
  io.cpu.resp.bits.data_raw := s2_data_word
  io.cpu.resp.bits.store_data := pstore1_data

  // AMOs
  if (usingRMW) {
    val amoalus = (0 until coreDataBits / xLen).map { i =>
      val amoalu = Module(new AMOALU(xLen))
      amoalu.io.mask := pstore1_mask >> (i * xBytes)
      amoalu.io.cmd := (if (usingAtomicsInCache) pstore1_cmd else M_XWR)
      amoalu.io.lhs := s2_data_word >> (i * xLen)
      amoalu.io.rhs := pstore1_data >> (i * xLen)
      amoalu
    }
    pstore1_storegen_data := (if (!usingDataScratchpad) amoalus.map(_.io.out).asUInt else {
      val mask = FillInterleaved(8, Mux(s2_correct, 0.U, pstore1_mask))
      amoalus.map(_.io.out_unmasked).asUInt & mask | s2_data_word_corrected & ~mask
    })
  } else if (!usingAtomics) {
    assert(!(s1_valid_masked && s1_read && s1_write), "unsupported D$ operation")
  }

  if (coreParams.useVector) {
    edge.manager.managers.foreach { m =>
      // Statically ensure that no-allocate accesses are permitted.
      // We could consider turning some of these into dynamic PMA checks.
      require(!m.supportsAcquireB || m.supportsGet, "With a vector unit, cacheable memory must support Get")
      require(!m.supportsAcquireT || m.supportsPutPartial, "With a vector unit, cacheable memory must support PutPartial")
    }
  }

  // flushes
  if (!usingDataScratchpad)
    when (RegNext(reset.asBool)) { resetting := true.B }
  val flushCounterNext = flushCounter +& 1.U
  val flushDone = (flushCounterNext >> log2Ceil(nSets)) === nWays.U
  val flushCounterWrap = flushCounterNext(log2Ceil(nSets)-1, 0)
  ccover(s2_valid_masked && s2_cmd_flush_all && s2_meta_error, "TAG_ECC_ERROR_DURING_FENCE_I", "D$ ECC error in tag array during cache flush")
  ccover(s2_valid_masked && s2_cmd_flush_all && s2_data_error, "DATA_ECC_ERROR_DURING_FENCE_I", "D$ ECC error in data array during cache flush")
  s1_flush_valid := metaArb.io.in(5).fire && !s1_flush_valid && !s2_flush_valid_pre_tag_ecc && release_state === s_ready && !release_ack_wait
  metaArb.io.in(5).valid := flushing && !flushed
  metaArb.io.in(5).bits.write := false.B
  metaArb.io.in(5).bits.idx := flushCounter(idxBits-1, 0)
  metaArb.io.in(5).bits.addr := Cat(io.cpu.req.bits.addr >> untagBits, metaArb.io.in(5).bits.idx << blockOffBits)
  metaArb.io.in(5).bits.way_en := metaArb.io.in(4).bits.way_en
  metaArb.io.in(5).bits.data := metaArb.io.in(4).bits.data

  // Only flush D$ on FENCE.I if some cached executable regions are untracked.
  if (supports_flush) {
    when (s2_valid_masked && s2_cmd_flush_all) {
      when (!flushed && !io.cpu.s2_kill && !release_ack_wait && !uncachedInFlight.asUInt.orR) {
        flushing := true.B
        flushing_req := s2_req
      }
    }

    when (tl_out_a.fire && !s2_uncached) { flushed := false.B }
    when (flushing) {
      s1_victim_way := flushCounter >> log2Up(nSets)
      when (s2_flush_valid) {
        flushCounter := flushCounterNext
        when (flushDone) {
          flushed := true.B
          if (!isPow2(nWays)) flushCounter := flushCounterWrap
        }
      }
      when (flushed && release_state === s_ready && !release_ack_wait) {
        flushing := false.B
      }
    }
  }
  metaArb.io.in(0).valid := resetting
  metaArb.io.in(0).bits := metaArb.io.in(5).bits
  metaArb.io.in(0).bits.write := true.B
  metaArb.io.in(0).bits.way_en := ~0.U(nWays.W)
  metaArb.io.in(0).bits.data := tECC.encode(L1Metadata(0.U, ClientMetadata.onReset).asUInt)
  when (resetting) {
    flushCounter := flushCounterNext
    when (flushDone) {
      resetting := false.B
      if (!isPow2(nWays)) flushCounter := flushCounterWrap
    }
  }

  // gate the clock
  clock_en_reg := !cacheParams.clockGate.B ||
    io.ptw.customCSRs.disableDCacheClockGate ||
    io.cpu.keep_clock_enabled ||
    metaArb.io.out.valid || // subsumes resetting || flushing
    s1_probe || s2_probe ||
    s1_valid || s2_valid ||
    io.tlb_port.req.valid ||
    s1_tlb_req_valid || s2_tlb_req_valid ||
    pstore1_held || pstore2_valid ||
    release_state =/= s_ready ||
    release_ack_wait || !release_queue_empty ||
    !tlb.io.req.ready ||
    cached_grant_wait || uncachedInFlight.asUInt.orR ||
    lrscCount > 0.U || blockProbeAfterGrantCount > 0.U

  // performance events
  io.cpu.perf.acquire := edge.done(tl_out_a)
  io.cpu.perf.release := edge.done(tl_out_c)
  io.cpu.perf.grant := tl_out.d.valid && d_last
  io.cpu.perf.tlbMiss := io.ptw.req.fire
  io.cpu.perf.storeBufferEmptyAfterLoad := !(
    (s1_valid && s1_write) ||
    ((s2_valid && s2_write && !s2_waw_hazard) || pstore1_held) ||
    pstore2_valid)
  io.cpu.perf.storeBufferEmptyAfterStore := !(
    (s1_valid && s1_write) ||
    (s2_valid && s2_write && pstore1_rmw) ||
    ((s2_valid && s2_write && !s2_waw_hazard || pstore1_held) && pstore2_valid))
  io.cpu.perf.canAcceptStoreThenLoad := !(
    ((s2_valid && s2_write && pstore1_rmw) && (s1_valid && s1_write && !s1_waw_hazard)) ||
    (pstore2_valid && pstore1_valid_likely && (s1_valid && s1_write)))
  io.cpu.perf.canAcceptStoreThenRMW := io.cpu.perf.canAcceptStoreThenLoad && !pstore2_valid
  io.cpu.perf.canAcceptLoadThenLoad := !((s1_valid && s1_write && needsRead(s1_req)) && ((s2_valid && s2_write && !s2_waw_hazard || pstore1_held) || pstore2_valid))
  io.cpu.perf.blocked := {
    // stop reporting blocked just before unblocking to avoid overly conservative stalling
    val beatsBeforeEnd = outer.crossing match {
      case SynchronousCrossing(_) => 2
      case RationalCrossing(_) => 1 // assumes 1 < ratio <= 2; need more bookkeeping for optimal handling of >2
      case _: AsynchronousCrossing => 1 // likewise
      case _: CreditedCrossing     => 1 // likewise
    }
    val near_end_of_refill = if (cacheBlockBytes / beatBytes <= beatsBeforeEnd) tl_out.d.valid else {
      val refill_count = RegInit(0.U((cacheBlockBytes / beatBytes).log2.W))
      when (tl_out.d.fire && grantIsRefill) { refill_count := refill_count + 1.U }
      refill_count >= (cacheBlockBytes / beatBytes - beatsBeforeEnd).U
    }
    cached_grant_wait && !near_end_of_refill
  }

  // report errors
  val (data_error, data_error_uncorrectable, data_error_addr) =
    if (usingDataScratchpad) (s2_valid_data_error, s2_data_error_uncorrectable, s2_req.addr) else {
      (RegNext(tl_out_c.fire && inWriteback && s2_data_error),
        RegNext(s2_data_error_uncorrectable),
        probe_bits.address) // This is stable for a cycle after tl_out_c.fire, so don't need a register
    }
  {
    val error_addr =
      Mux(metaArb.io.in(1).valid, Cat(s2_first_meta_corrected.tag, metaArb.io.in(1).bits.addr(tagLSB-1, idxLSB)),
          data_error_addr >> idxLSB) << idxLSB
    io.errors.uncorrectable.foreach { u =>
      u.valid := metaArb.io.in(1).valid && s2_meta_error_uncorrectable || data_error && data_error_uncorrectable
      u.bits := error_addr
    }
    io.errors.correctable.foreach { c =>
      c.valid := metaArb.io.in(1).valid || data_error
      c.bits := error_addr
      io.errors.uncorrectable.foreach { u => when (u.valid) { c.valid := false.B } }
    }
    io.errors.bus.valid := tl_out.d.fire && (tl_out.d.bits.denied || tl_out.d.bits.corrupt)
    io.errors.bus.bits := Mux(grantIsCached, s2_req.addr >> idxLSB << idxLSB, 0.U)

    ccoverNotScratchpad(io.errors.bus.valid && grantIsCached, "D_ERROR_CACHED", "D$ D-channel error, cached")
    ccover(io.errors.bus.valid && !grantIsCached, "D_ERROR_UNCACHED", "D$ D-channel error, uncached")
  }

  if (usingDataScratchpad) {
    val data_error_cover = Seq(
      property.CoverBoolean(!data_error, Seq("no_data_error")),
      property.CoverBoolean(data_error && !data_error_uncorrectable, Seq("data_correctable_error")),
      property.CoverBoolean(data_error && data_error_uncorrectable, Seq("data_uncorrectable_error")))
    val request_source = Seq(
      property.CoverBoolean(s2_isManagerPortAccess, Seq("from_TL")),
      property.CoverBoolean(!s2_isManagerPortAccess, Seq("from_CPU")))

    property.cover(new property.CrossProperty(
      Seq(data_error_cover, request_source),
      Seq(),
      "MemorySystem;;Scratchpad Memory Bit Flip Cross Covers"))
  } else {

    val data_error_type = Seq(
      property.CoverBoolean(!s2_valid_data_error, Seq("no_data_error")),
      property.CoverBoolean(s2_valid_data_error && !s2_data_error_uncorrectable, Seq("data_correctable_error")),
      property.CoverBoolean(s2_valid_data_error && s2_data_error_uncorrectable, Seq("data_uncorrectable_error")))
    val data_error_dirty = Seq(
      property.CoverBoolean(!s2_victim_dirty, Seq("data_clean")),
      property.CoverBoolean(s2_victim_dirty, Seq("data_dirty")))
    val request_source = if (supports_flush) {
        Seq(
          property.CoverBoolean(!flushing, Seq("access")),
          property.CoverBoolean(flushing, Seq("during_flush")))
      } else {
        Seq(property.CoverBoolean(true.B, Seq("never_flush")))
      }
    val tag_error_cover = Seq(
      property.CoverBoolean( !s2_meta_error, Seq("no_tag_error")),
      property.CoverBoolean( s2_meta_error && !s2_meta_error_uncorrectable, Seq("tag_correctable_error")),
      property.CoverBoolean( s2_meta_error && s2_meta_error_uncorrectable, Seq("tag_uncorrectable_error")))
    property.cover(new property.CrossProperty(
      Seq(data_error_type, data_error_dirty, request_source, tag_error_cover),
      Seq(),
      "MemorySystem;;Cache Memory Bit Flip Cross Covers"))
  }

  } // leaving gated-clock domain
  val dcacheImpl = withClock (gated_clock) { new DCacheModuleImpl }

  def encodeData(x: UInt, poison: Bool) = x.grouped(eccBits).map(dECC.encode(_, if (dECC.canDetect) poison else false.B)).asUInt
  def dummyEncodeData(x: UInt) = x.grouped(eccBits).map(dECC.swizzle(_)).asUInt
  def decodeData(x: UInt) = x.grouped(dECC.width(eccBits)).map(dECC.decode(_))
  def eccMask(byteMask: UInt) = byteMask.grouped(eccBytes).map(_.orR).asUInt
  def eccByteMask(byteMask: UInt) = FillInterleaved(eccBytes, eccMask(byteMask))

  def likelyNeedsRead(req: HellaCacheReq) = {
    val res = !req.cmd.isOneOf(M_XWR, M_PFW) || req.size < log2Ceil(eccBytes).U
    assert(!needsRead(req) || res)
    res
  }
  def needsRead(req: HellaCacheReq) =
    isRead(req.cmd) ||
    (isWrite(req.cmd) && (req.cmd === M_PWR || req.size < log2Ceil(eccBytes).U))

  def ccover(cond: Bool, label: String, desc: String)(implicit sourceInfo: SourceInfo) =
    property.cover(cond, s"DCACHE_$label", "MemorySystem;;" + desc)
  def ccoverNotScratchpad(cond: Bool, label: String, desc: String)(implicit sourceInfo: SourceInfo) =
    if (!usingDataScratchpad) ccover(cond, label, desc)

  require(!usingVM || tagLSB <= pgIdxBits, s"D$$ set size must not exceed ${1<<(pgIdxBits-10)} KiB; got ${(nSets * cacheBlockBytes)>>10} KiB")
  def tagLSB: Int = untagBits
  def probeIdx(b: TLBundleB): UInt = b.address(idxMSB, idxLSB)
  def addressToProbe(vaddr: UInt, paddr: UInt): TLBundleB = {
    val res = Wire(new TLBundleB(edge.bundle))
    res :#= DontCare
    res.address := paddr
    res.source := (mmioOffset - 1).U
    res
  }
  def acquire(vaddr: UInt, paddr: UInt, param: UInt): TLBundleA = {
    if (!edge.manager.anySupportAcquireB) WireDefault(0.U.asTypeOf(new TLBundleA(edge.bundle)))
    else edge.AcquireBlock(0.U, paddr >> lgCacheBlockBytes << lgCacheBlockBytes, lgCacheBlockBytes.U, param)._2
  }

}
