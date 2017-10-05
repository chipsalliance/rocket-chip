// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket

import Chisel._
import Chisel.ImplicitConversions._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.coreplex.{RationalCrossing, RocketCrossing, RocketTilesKey}
import freechips.rocketchip.diplomacy.{AddressSet, RegionType}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import TLMessages._

class DCacheErrors(implicit p: Parameters) extends L1HellaCacheBundle()(p)
    with CanHaveErrors {
  val correctable = (cacheParams.tagECC.canCorrect || cacheParams.dataECC.canCorrect).option(Valid(UInt(width = paddrBits)))
  val uncorrectable = (cacheParams.tagECC.canDetect || cacheParams.dataECC.canDetect).option(Valid(UInt(width = paddrBits)))
  val bus = Valid(UInt(width = paddrBits))
}

class DCacheDataReq(implicit p: Parameters) extends L1HellaCacheBundle()(p) {
  val eccBytes = cacheParams.dataECCBytes
  val addr = Bits(width = untagBits)
  val write = Bool()
  val wdata = UInt(width = cacheParams.dataECC.width(eccBytes*8) * rowBytes/eccBytes)
  val wordMask = UInt(width = rowBytes / wordBytes)
  val eccMask = UInt(width = wordBytes / eccBytes)
  val way_en = Bits(width = nWays)
}

class DCacheDataArray(implicit p: Parameters) extends L1HellaCacheModule()(p) {
  val io = new Bundle {
    val req = Valid(new DCacheDataReq).flip
    val resp = Vec(nWays, UInt(width = req.bits.wdata.getWidth)).asOutput
  }

  require(rowBytes % wordBytes == 0)
  val eccBits = cacheParams.dataECCBytes * 8
  val encBits = cacheParams.dataECC.width(eccBits)
  val encWordBits = encBits * (wordBits / eccBits)
  val eccMask = if (eccBits == wordBits) Seq(true.B) else io.req.bits.eccMask.toBools
  val wMask = if (nWays == 1) eccMask else (0 until nWays).flatMap(i => eccMask.map(_ && io.req.bits.way_en(i)))
  val wWords = io.req.bits.wdata.grouped(encBits * (wordBits / eccBits))
  val addr = io.req.bits.addr >> rowOffBits
  val data_arrays = Seq.fill(rowBytes / wordBytes) { SeqMem(nSets * refillCycles, Vec(nWays * (wordBits / eccBits), UInt(width = encBits))) }
  val rdata = for ((array, i) <- data_arrays zipWithIndex) yield {
    val valid = io.req.valid && (Bool(data_arrays.size == 1) || io.req.bits.wordMask(i))
    when (valid && io.req.bits.write) {
      val wData = wWords(i).grouped(encBits)
      array.write(addr, Vec((0 until nWays).flatMap(i => wData)), wMask)
    }
    val data = array.read(addr, valid && !io.req.bits.write)
    data.grouped(wordBits / eccBits).map(_.asUInt).toSeq
  }
  (io.resp zip rdata.transpose).foreach { case (resp, data) => resp := data.asUInt }
}

class DCacheMetadataReq(implicit p: Parameters) extends L1HellaCacheBundle()(p) {
  val write = Bool()
  val addr = UInt(width = vaddrBitsExtended)
  val way_en = UInt(width = nWays)
  val data = new L1Metadata
}

class DCache(hartid: Int, val scratch: () => Option[AddressSet] = () => None)(implicit p: Parameters) extends HellaCache(hartid)(p) {
  override lazy val module = new DCacheModule(this) 
}

class DCacheModule(outer: DCache) extends HellaCacheModule(outer) {
  // no tag ECC support
  val tECC = cacheParams.tagECC
  val dECC = cacheParams.dataECC
  val eccBytes = cacheParams.dataECCBytes
  val eccBits = eccBytes * 8
  require(isPow2(eccBytes) && eccBytes <= wordBytes)
  require(eccBytes == 1 || !dECC.isInstanceOf[IdentityCode])
  val usingRMW = eccBytes > 1 || usingAtomics

  // tags
  val replacer = cacheParams.replacement
  val metaArb = Module(new Arbiter(new DCacheMetadataReq, 8))
  val tag_array = SeqMem(nSets, Vec(nWays, UInt(width = tECC.width(metaArb.io.out.bits.data.getWidth))))

  // data
  val data = Module(new DCacheDataArray)
  val dataArb = Module(new Arbiter(new DCacheDataReq, 4))
  data.io.req <> dataArb.io.out
  data.io.req.bits.wdata := encodeData(dataArb.io.out.bits.wdata(rowBits-1, 0))
  dataArb.io.out.ready := true
  metaArb.io.out.ready := true

  val rational = p(RocketCrossing) match {
    case RationalCrossing(_) => true
    case _ => false
  }

  val q_depth = if (rational) (2 min maxUncachedInFlight-1) else 0
  val tl_out_a = Wire(tl_out.a)
  tl_out.a <> (if (q_depth == 0) tl_out_a else Queue(tl_out_a, q_depth, flow = true))
  val (tl_out_c, release_queue_empty) =
    if (cacheParams.acquireBeforeRelease) {
      val q = Module(new Queue(tl_out.c.bits, cacheDataBeats, flow = true))
      tl_out.c <> q.io.deq
      (q.io.enq, q.io.count === 0)
    } else {
      (tl_out.c, true.B)
    }

  val s1_valid = Reg(next=io.cpu.req.fire(), init=Bool(false))
  val s1_probe = Reg(next=tl_out.b.fire(), init=Bool(false))
  val probe_bits = RegEnable(tl_out.b.bits, tl_out.b.fire()) // TODO has data now :(
  val s1_nack = Wire(init=Bool(false))
  val s1_valid_masked = s1_valid && !io.cpu.s1_kill
  val s1_valid_not_nacked = s1_valid && !s1_nack
  val s1_req = Reg(io.cpu.req.bits)
  val s0_clk_en = metaArb.io.out.valid && !metaArb.io.out.bits.write
  when (s0_clk_en) {
    s1_req := io.cpu.req.bits
    s1_req.addr := Cat(metaArb.io.out.bits.addr >> blockOffBits, io.cpu.req.bits.addr(blockOffBits-1,0))
    when (!metaArb.io.in(7).ready) { s1_req.phys := true }
  }
  val s1_read = isRead(s1_req.cmd)
  val s1_write = isWrite(s1_req.cmd)
  val s1_readwrite = s1_read || s1_write
  val s1_sfence = s1_req.cmd === M_SFENCE
  val s1_flush_valid = Reg(Bool())
  val s1_waw_hazard = Wire(Bool())

  val s_ready :: s_voluntary_writeback :: s_probe_rep_dirty :: s_probe_rep_clean :: s_probe_retry :: s_probe_rep_miss :: s_voluntary_write_meta :: s_probe_write_meta :: Nil = Enum(UInt(), 8)
  val cached_grant_wait = Reg(init=Bool(false))
  val release_ack_wait = Reg(init=Bool(false))
  val can_acquire_before_release = !release_ack_wait && release_queue_empty
  val release_state = Reg(init=s_ready)
  val any_pstore_valid = Wire(Bool())
  val inWriteback = release_state.isOneOf(s_voluntary_writeback, s_probe_rep_dirty)
  val releaseWay = Wire(UInt())
  io.cpu.req.ready := (release_state === s_ready) && !cached_grant_wait && !s1_nack

  // I/O MSHRs
  val mmioOffset = if (outer.scratch().isDefined) 0 else 1
  val uncachedInFlight = Seq.fill(maxUncachedInFlight) { RegInit(Bool(false)) }
  val uncachedReqs = Seq.fill(maxUncachedInFlight) { Reg(new HellaCacheReq) }

  // hit initiation path
  val s0_needsRead = needsRead(io.cpu.req.bits)
  val s0_read = isRead(io.cpu.req.bits.cmd)
  dataArb.io.in(3).valid := io.cpu.req.valid && s0_needsRead
  dataArb.io.in(3).bits.write := false
  dataArb.io.in(3).bits.addr := io.cpu.req.bits.addr
  dataArb.io.in(3).bits.wordMask := UIntToOH(io.cpu.req.bits.addr.extract(rowOffBits-1,offsetlsb))
  dataArb.io.in(3).bits.way_en := ~UInt(0, nWays)
  when (!dataArb.io.in(3).ready && s0_read) { io.cpu.req.ready := false }
  val s1_did_read = RegEnable(dataArb.io.in(3).fire(), s0_clk_en)
  metaArb.io.in(7).valid := io.cpu.req.valid
  metaArb.io.in(7).bits.write := false
  metaArb.io.in(7).bits.addr := io.cpu.req.bits.addr
  metaArb.io.in(7).bits.way_en := ~UInt(0, nWays)
  metaArb.io.in(7).bits.data := metaArb.io.in(4).bits.data
  when (!metaArb.io.in(7).ready) { io.cpu.req.ready := false }

  // address translation
  val tlb = Module(new TLB(false, log2Ceil(coreDataBytes), nTLBEntries))
  io.ptw <> tlb.io.ptw
  tlb.io.req.valid := s1_valid && !io.cpu.s1_kill && (s1_readwrite || s1_sfence)
  tlb.io.req.bits.sfence.valid := s1_sfence
  tlb.io.req.bits.sfence.bits.rs1 := s1_req.typ(0)
  tlb.io.req.bits.sfence.bits.rs2 := s1_req.typ(1)
  tlb.io.req.bits.sfence.bits.asid := io.cpu.s1_data.data
  tlb.io.req.bits.sfence.bits.addr := s1_req.addr
  tlb.io.req.bits.passthrough := s1_req.phys
  tlb.io.req.bits.vaddr := s1_req.addr
  tlb.io.req.bits.size := s1_req.typ
  tlb.io.req.bits.cmd := s1_req.cmd
  when (!tlb.io.req.ready && !tlb.io.ptw.resp.valid && !io.cpu.req.bits.phys) { io.cpu.req.ready := false }
  when (s1_valid && s1_readwrite && tlb.io.resp.miss) { s1_nack := true }

  val s1_paddr = tlb.io.resp.paddr
  val s1_victim_way = Wire(init = replacer.way)
  val (s1_hit_way, s1_hit_state, s1_meta, s1_victim_meta) =
    if (usingDataScratchpad) {
      val baseAddr = GetPropertyByHartId(p(RocketTilesKey), _.dcache.flatMap(_.scratch.map(_.U)), io.hartid)
      val inScratchpad = s1_paddr >= baseAddr && s1_paddr < baseAddr + nSets * cacheBlockBytes
      val hitState = Mux(inScratchpad, ClientMetadata.maximum, ClientMetadata.onReset)
      val dummyMeta = L1Metadata(UInt(0), ClientMetadata.onReset)
      (inScratchpad, hitState, Seq(tECC.encode(dummyMeta.asUInt)), dummyMeta)
    } else {
      val metaReq = metaArb.io.out
      val metaIdx = metaReq.bits.addr(idxMSB, idxLSB)
      when (metaReq.valid && metaReq.bits.write) {
        val wdata = tECC.encode(metaReq.bits.data.asUInt)
        val wmask = if (nWays == 1) Seq(true.B) else metaReq.bits.way_en.toBools
        tag_array.write(metaIdx, Vec.fill(nWays)(wdata), wmask)
      }
      val s1_meta = tag_array.read(metaIdx, metaReq.valid && !metaReq.bits.write)
      val s1_meta_uncorrected = s1_meta.map(tECC.decode(_).uncorrected.asTypeOf(new L1Metadata))
      val s1_tag = s1_paddr >> untagBits
      val s1_meta_hit_way = s1_meta_uncorrected.map(r => r.coh.isValid() && r.tag === s1_tag).asUInt
      val s1_meta_hit_state = ClientMetadata.onReset.fromBits(
        s1_meta_uncorrected.map(r => Mux(r.tag === s1_tag, r.coh.asUInt, UInt(0)))
        .reduce (_|_))
      (s1_meta_hit_way, s1_meta_hit_state, s1_meta, s1_meta_uncorrected(s1_victim_way))
    }
  val s1_data_way = Wire(init = Mux(inWriteback, releaseWay, s1_hit_way))
  val s1_all_data_ways = Vec(data.io.resp :+ dummyEncodeData(tl_out.d.bits.data))
  val s1_mask = Mux(s1_req.cmd === M_PWR, io.cpu.s1_data.mask, new StoreGen(s1_req.typ, s1_req.addr, UInt(0), wordBytes).mask)

  val s2_valid_pre_xcpt = Reg(next=s1_valid_masked && !s1_sfence, init=Bool(false))
  val s2_valid = s2_valid_pre_xcpt && !io.cpu.s2_xcpt.asUInt.orR
  val s2_probe = Reg(next=s1_probe, init=Bool(false))
  val releaseInFlight = s1_probe || s2_probe || release_state =/= s_ready
  val s2_valid_masked = s2_valid && Reg(next = !s1_nack)
  val s2_req = Reg(io.cpu.req.bits)
  val s2_req_block_addr = (s2_req.addr >> idxLSB) << idxLSB
  val s2_uncached = Reg(Bool())
  val s2_uncached_resp_addr = Reg(UInt()) // should be DCE'd in synthesis
  when (s1_valid_not_nacked || s1_flush_valid) {
    s2_req := s1_req
    s2_req.addr := s1_paddr
    s2_uncached := !tlb.io.resp.cacheable
  }
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
    val en = s1_valid || inWriteback || tl_out.d.fire()
    if (cacheParams.pipelineWayMux && nWays > 1) {
      val s2_data_way = RegEnable(s1_data_way, en)
      val s2_all_data_ways = (0 to nWays).map(i => RegEnable(s1_all_data_ways(i), en && s1_data_way(i)))
      Mux1H(s2_data_way, s2_all_data_ways)
    } else {
      RegEnable(Mux1H(s1_data_way, s1_all_data_ways), en)
    }
  }
  val s2_probe_way = RegEnable(s1_hit_way, s1_probe)
  val s2_probe_state = RegEnable(s1_hit_state, s1_probe)
  val s2_hit_way = RegEnable(s1_hit_way, s1_valid_not_nacked)
  val s2_hit_state = RegEnable(s1_hit_state, s1_valid_not_nacked)
  val s2_waw_hazard = RegEnable(s1_waw_hazard, s1_valid_not_nacked)
  val s2_store_merge = Wire(Bool())
  val s2_hit_valid = s2_hit_state.isValid()
  val (s2_hit, s2_grow_param, s2_new_hit_state) = s2_hit_state.onAccess(s2_req.cmd)
  val s2_data_decoded = decodeData(s2_data)
  val s2_word_idx = s2_req.addr.extract(log2Up(rowBits/8)-1, log2Up(wordBytes))
  val s2_did_read = RegEnable(s1_did_read, s1_valid_not_nacked)
  val s2_data_error = s2_did_read && (s2_data_decoded.map(_.error).grouped(wordBits/eccBits).map(_.reduce(_||_)).toSeq)(s2_word_idx)
  val s2_data_error_uncorrectable = (s2_data_decoded.map(_.uncorrectable).grouped(wordBits/eccBits).map(_.reduce(_||_)).toSeq)(s2_word_idx)
  val s2_data_corrected = (s2_data_decoded.map(_.corrected): Seq[UInt]).asUInt
  val s2_data_uncorrected = (s2_data_decoded.map(_.uncorrected): Seq[UInt]).asUInt
  val s2_valid_hit_pre_data_ecc = s2_valid_masked && s2_readwrite && !s2_meta_error && s2_hit
  val s2_valid_data_error = s2_valid_hit_pre_data_ecc && s2_data_error && can_acquire_before_release
  val s2_valid_hit = s2_valid_hit_pre_data_ecc && !s2_data_error && (!s2_waw_hazard || s2_store_merge)
  val s2_valid_miss = s2_valid_masked && s2_readwrite && !s2_meta_error && !s2_hit && can_acquire_before_release
  val s2_valid_cached_miss = s2_valid_miss && !s2_uncached && !uncachedInFlight.asUInt.orR
  val s2_victimize = Bool(!usingDataScratchpad) && (s2_valid_cached_miss || s2_valid_data_error || s2_flush_valid)
  val s2_valid_uncached_pending = s2_valid_miss && s2_uncached && !uncachedInFlight.asUInt.andR
  val s2_victim_way = Mux(s2_hit_valid && !s2_flush_valid_pre_tag_ecc, s2_hit_way, UIntToOH(RegEnable(s1_victim_way, s1_valid_not_nacked || s1_flush_valid)))
  val s2_victim_tag = Mux(s2_valid_data_error, s2_req.addr >> untagBits, RegEnable(s1_victim_meta.tag, s1_valid_not_nacked || s1_flush_valid))
  val s2_victim_state = Mux(s2_hit_valid && !s2_flush_valid, s2_hit_state, RegEnable(s1_victim_meta.coh, s1_valid_not_nacked || s1_flush_valid))

  val (s2_prb_ack_data, s2_report_param, probeNewCoh)= s2_probe_state.onProbe(probe_bits.param)
  val (s2_victim_dirty, s2_shrink_param, voluntaryNewCoh) = s2_victim_state.onCacheControl(M_FLUSH)
  val s2_update_meta = s2_hit_state =/= s2_new_hit_state
  io.cpu.s2_nack := s2_valid && !s2_valid_hit && !(s2_valid_uncached_pending && tl_out_a.ready)
  when (io.cpu.s2_nack || (s2_valid_hit && s2_update_meta)) { s1_nack := true }

  // tag updates on ECC errors
  metaArb.io.in(1).valid := s2_meta_error && (s2_valid_masked || s2_flush_valid_pre_tag_ecc || s2_probe)
  metaArb.io.in(1).bits.write := true
  metaArb.io.in(1).bits.way_en := s2_meta_uncorrectable_errors | Mux(s2_meta_error_uncorrectable, 0.U, PriorityEncoderOH(s2_meta_correctable_errors))
  metaArb.io.in(1).bits.addr := Cat(io.cpu.req.bits.addr >> untagBits, Mux(s2_probe, probe_bits.address, s2_req.addr)(idxMSB, 0))
  metaArb.io.in(1).bits.data := PriorityMux(s2_meta_correctable_errors, s2_meta_corrected)
  when (s2_meta_error_uncorrectable) { metaArb.io.in(1).bits.data.coh := ClientMetadata.onReset }

  // tag updates on hit/miss
  metaArb.io.in(2).valid := (s2_valid_hit && s2_update_meta) || (s2_victimize && !s2_victim_dirty)
  metaArb.io.in(2).bits.write := true
  metaArb.io.in(2).bits.way_en := s2_victim_way
  metaArb.io.in(2).bits.addr := Cat(io.cpu.req.bits.addr >> untagBits, s2_req.addr(idxMSB, 0))
  metaArb.io.in(2).bits.data.coh := Mux(s2_valid_hit, s2_new_hit_state, ClientMetadata.onReset)
  metaArb.io.in(2).bits.data.tag := s2_req.addr >> untagBits

  // load reservations and TL error reporting
  val s2_lr = Bool(usingAtomics && !usingDataScratchpad) && s2_req.cmd === M_XLR
  val s2_sc = Bool(usingAtomics && !usingDataScratchpad) && s2_req.cmd === M_XSC
  val lrscCount = Reg(init=UInt(0))
  val tl_error_valid = RegInit(false.B)
  val lrscValid = lrscCount > lrscBackoff
  val lrscAddr = Reg(UInt())
  val lrscAddrMatch = lrscAddr === (s2_req.addr >> blockOffBits)
  val s2_sc_fail = s2_sc && !(lrscValid && lrscAddrMatch)
  val s2_tl_error = tl_error_valid && lrscAddrMatch
  when (s2_valid_hit && s2_lr && !cached_grant_wait || s2_valid_cached_miss) {
    tl_error_valid := false
    lrscCount := Mux(s2_hit, lrscCycles - 1, 0.U)
    lrscAddr := s2_req.addr >> blockOffBits
  }
  when (lrscCount > 0) { lrscCount := lrscCount - 1 }
  when ((s2_valid_masked && lrscCount > 0) || io.cpu.invalidate_lr) { lrscCount := 0 }

  // don't perform data correction if it might clobber a recent store
  val s2_correct = s2_data_error && !any_pstore_valid && !RegNext(any_pstore_valid) && Bool(usingDataScratchpad)
  // pending store buffer
  val s2_valid_correct = s2_valid_hit_pre_data_ecc && s2_correct
  val s2_store_valid = s2_valid_hit && s2_write && !s2_sc_fail
  val pstore1_cmd = RegEnable(s1_req.cmd, s1_valid_not_nacked && s1_write)
  val pstore1_addr = RegEnable(s1_paddr, s1_valid_not_nacked && s1_write)
  val pstore1_data = RegEnable(io.cpu.s1_data.data, s1_valid_not_nacked && s1_write)
  val pstore1_way = RegEnable(s1_hit_way, s1_valid_not_nacked && s1_write)
  val pstore1_mask = RegEnable(s1_mask, s1_valid_not_nacked && s1_write)
  val pstore1_storegen_data = Wire(init = pstore1_data)
  val pstore1_rmw = Bool(usingRMW) && RegEnable(needsRead(s1_req), s1_valid_not_nacked && s1_write)
  val pstore1_valid = Wire(Bool())
  val pstore1_merge = s2_store_valid && s2_store_merge
  val pstore2_valid = Reg(Bool())
  any_pstore_valid := pstore1_valid || pstore2_valid
  val pstore_drain_structural = pstore1_valid && pstore2_valid && ((s1_valid && s1_write) || pstore1_rmw)
  val pstore_drain_opportunistic = !(io.cpu.req.valid && s0_needsRead)
  val pstore_drain_on_miss = releaseInFlight || (s2_valid && !s2_valid_hit && !s2_valid_uncached_pending)
  val pstore_drain = !pstore1_merge &&
    (Bool(usingRMW) && pstore_drain_structural ||
     (((pstore1_valid && !pstore1_rmw) || pstore2_valid) && (pstore_drain_opportunistic || pstore_drain_on_miss)))
  pstore1_valid := {
    val pstore1_held = Reg(Bool())
    assert(!s2_store_valid || !pstore1_held)
    pstore1_held := (s2_store_valid && !s2_store_merge || pstore1_held) && pstore2_valid && !pstore_drain
    s2_store_valid || pstore1_held
  }
  val advance_pstore1 = (pstore1_valid || s2_valid_correct) && (pstore2_valid === pstore_drain)
  pstore2_valid := pstore2_valid && !pstore_drain || advance_pstore1
  val pstore2_addr = RegEnable(Mux(s2_correct, s2_req.addr, pstore1_addr), advance_pstore1)
  val pstore2_way = RegEnable(Mux(s2_correct, s2_hit_way, pstore1_way), advance_pstore1)
  val pstore2_storegen_data = {
    for (i <- 0 until wordBytes)
      yield RegEnable(pstore1_storegen_data(8*(i+1)-1, 8*i), advance_pstore1 || pstore1_merge && pstore1_mask(i))
  }.asUInt
  val pstore2_storegen_mask = {
    val mask = Reg(UInt(width = wordBytes))
    when (advance_pstore1 || pstore1_merge) {
      val mergedMask = pstore1_mask | Mux(pstore1_merge, mask, 0.U)
      mask := ~Mux(s2_correct, 0.U, ~mergedMask)
    }
    mask
  }
  s2_store_merge := {
    // only merge stores to ECC granules that are already stored-to, to avoid
    // WAW hazards
    val wordMatch = (eccMask(pstore2_storegen_mask) | ~eccMask(pstore1_mask)).andR
    val idxMatch = s2_req.addr(untagBits-1, log2Ceil(wordBytes)) === pstore2_addr(untagBits-1, log2Ceil(wordBytes))
    val tagMatch = (s2_hit_way & pstore2_way).orR
    Bool(eccBytes > 1) && pstore2_valid && wordMatch && idxMatch && tagMatch
  }
  dataArb.io.in(0).valid := pstore_drain
  dataArb.io.in(0).bits.write := true
  dataArb.io.in(0).bits.addr := Mux(pstore2_valid, pstore2_addr, pstore1_addr)
  dataArb.io.in(0).bits.way_en := Mux(pstore2_valid, pstore2_way, pstore1_way)
  dataArb.io.in(0).bits.wdata := Fill(rowWords, Mux(pstore2_valid, pstore2_storegen_data, pstore1_data))
  dataArb.io.in(0).bits.wordMask := UIntToOH(Mux(pstore2_valid, pstore2_addr, pstore1_addr).extract(rowOffBits-1,offsetlsb))
  dataArb.io.in(0).bits.eccMask := eccMask(Mux(pstore2_valid, pstore2_storegen_mask, pstore1_mask))

  // store->load RAW hazard detection
  def s1Depends(addr: UInt, mask: UInt) =
    addr(idxMSB, wordOffBits) === s1_req.addr(idxMSB, wordOffBits) &&
    Mux(s1_write, (eccByteMask(mask) & eccByteMask(s1_mask)).orR, (mask & s1_mask).orR)
  val s1_hazard =
    (pstore1_valid && s1Depends(pstore1_addr, pstore1_mask)) ||
     (pstore2_valid && s1Depends(pstore2_addr, pstore2_storegen_mask))
  val s1_raw_hazard = s1_read && s1_hazard
  s1_waw_hazard := Bool(eccBytes > 1) && s1_write && (s1_hazard || needsRead(s1_req) && !s1_did_read)
  when (s1_valid && s1_raw_hazard) { s1_nack := true }

  // Prepare a TileLink request message that initiates a transaction
  val a_source = PriorityEncoder(~uncachedInFlight.asUInt << mmioOffset) // skip the MSHR
  val acquire_address = s2_req_block_addr
  val access_address = s2_req.addr
  val a_size = mtSize(s2_req.typ)
  val a_data = Fill(beatWords, pstore1_data)
  val acquire = if (edge.manager.anySupportAcquireB) {
    edge.AcquireBlock(UInt(0), acquire_address, lgCacheBlockBytes, s2_grow_param)._2 // Cacheability checked by tlb
  } else {
    Wire(new TLBundleA(edge.bundle))
  }
  val get     = edge.Get(a_source, access_address, a_size)._2
  val put     = edge.Put(a_source, access_address, a_size, a_data)._2
  val atomics = if (edge.manager.anySupportLogical) {
    MuxLookup(s2_req.cmd, Wire(new TLBundleA(edge.bundle)), Array(
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
    Wire(new TLBundleA(edge.bundle))
  }

  tl_out_a.valid := (s2_valid_cached_miss && (Bool(cacheParams.acquireBeforeRelease) || !s2_victim_dirty)) || s2_valid_uncached_pending
  tl_out_a.bits := Mux(!s2_uncached, acquire, Mux(!s2_write, get, Mux(!s2_read, put, atomics)))

  // Set pending bits for outstanding TileLink transaction
  val a_sel = UIntToOH(a_source, maxUncachedInFlight+mmioOffset) >> mmioOffset
  when (tl_out_a.fire()) {
    when (s2_uncached) {
      (a_sel.toBools zip (uncachedInFlight zip uncachedReqs)) foreach { case (s, (f, r)) =>
        when (s) {
          f := Bool(true)
          r := s2_req
        }
      }
    }.otherwise {
      cached_grant_wait := true
    }
  }

  // grant
  val (d_first, d_last, d_done, d_address_inc) = edge.addr_inc(tl_out.d)
  val grantIsCached = tl_out.d.bits.opcode.isOneOf(Grant, GrantData)
  val grantIsUncached = tl_out.d.bits.opcode.isOneOf(AccessAck, AccessAckData, HintAck)
  val grantIsUncachedData = tl_out.d.bits.opcode === AccessAckData
  val grantIsVoluntary = tl_out.d.bits.opcode === ReleaseAck // Clears a different pending bit
  val grantIsRefill = tl_out.d.bits.opcode === GrantData     // Writes the data array
  val grantInProgress = Reg(init=Bool(false))
  val blockProbeAfterGrantCount = Reg(init=UInt(0))
  when (blockProbeAfterGrantCount > 0) { blockProbeAfterGrantCount := blockProbeAfterGrantCount - 1 }
  val canAcceptCachedGrant = if (cacheParams.acquireBeforeRelease) !release_state.isOneOf(s_voluntary_writeback, s_voluntary_write_meta) else true.B
  tl_out.d.ready := Mux(grantIsCached, (!d_first || tl_out.e.ready) && canAcceptCachedGrant, true.B)
  when (tl_out.d.fire()) {
    when (grantIsCached) {
      grantInProgress := true
      assert(cached_grant_wait, "A GrantData was unexpected by the dcache.")
      when(d_last) {
        tl_error_valid := tl_out.d.bits.error
        cached_grant_wait := false
        grantInProgress := false
        blockProbeAfterGrantCount := blockProbeAfterGrantCycles - 1
        replacer.miss
      }
    } .elsewhen (grantIsUncached) {
      val d_sel = UIntToOH(tl_out.d.bits.source, maxUncachedInFlight+mmioOffset) >> mmioOffset
      val req = Mux1H(d_sel, uncachedReqs)
      (d_sel.toBools zip uncachedInFlight) foreach { case (s, f) =>
        when (s && d_last) {
          assert(f, "An AccessAck was unexpected by the dcache.") // TODO must handle Ack coming back on same cycle!
          f := false
        }
      }
      when (grantIsUncachedData) {
        s1_data_way := 1.U << nWays
        s2_req.cmd := M_XRD
        s2_req.typ := req.typ
        s2_req.tag := req.tag
        s2_req.addr := Cat(s1_paddr >> beatOffBits /* don't-care */, req.addr(beatOffBits-1, 0))
        s2_uncached_resp_addr := req.addr
      }
    } .elsewhen (grantIsVoluntary) {
      assert(release_ack_wait, "A ReleaseAck was unexpected by the dcache.") // TODO should handle Ack coming back on same cycle!
      release_ack_wait := false
    }
  }

  // Finish TileLink transaction by issuing a GrantAck
  tl_out.e.valid := tl_out.d.valid && d_first && grantIsCached && canAcceptCachedGrant
  tl_out.e.bits := edge.GrantAck(tl_out.d.bits)
  assert(tl_out.e.fire() === (tl_out.d.fire() && d_first && grantIsCached))

  // data refill
  // note this ready-valid signaling ignores E-channel backpressure, which
  // benignly means the data RAM might occasionally be redundantly written
  dataArb.io.in(1).valid := tl_out.d.valid && grantIsRefill && canAcceptCachedGrant
  when (grantIsRefill && !dataArb.io.in(1).ready) {
    tl_out.e.valid := false
    tl_out.d.ready := false
  }
  dataArb.io.in(1).bits.write := true
  dataArb.io.in(1).bits.addr :=  s2_req_block_addr | d_address_inc
  dataArb.io.in(1).bits.way_en := s2_victim_way
  dataArb.io.in(1).bits.wdata := tl_out.d.bits.data
  dataArb.io.in(1).bits.wordMask := ~UInt(0, rowBytes / wordBytes)
  dataArb.io.in(1).bits.eccMask := ~UInt(0, wordBytes / eccBytes)
  // tag updates on refill
  // ignore backpressure from metaArb, which can only be caused by tag ECC
  // errors on hit-under-miss.  failing to write the new tag will leave the
  // line invalid, so we'll simply request the line again later.
  metaArb.io.in(3).valid := grantIsCached && d_done && !tl_out.d.bits.error
  metaArb.io.in(3).bits.write := true
  metaArb.io.in(3).bits.way_en := s2_victim_way
  metaArb.io.in(3).bits.addr := Cat(io.cpu.req.bits.addr >> untagBits, s2_req.addr(idxMSB, 0))
  metaArb.io.in(3).bits.data.coh := s2_hit_state.onGrant(s2_req.cmd, tl_out.d.bits.param)
  metaArb.io.in(3).bits.data.tag := s2_req.addr >> untagBits
  // don't accept uncached grants if there's a structural hazard on s2_data...
  val blockUncachedGrant = Reg(Bool())
  blockUncachedGrant := dataArb.io.out.valid
  when (grantIsUncachedData && (blockUncachedGrant || s1_valid)) {
    tl_out.d.ready := false
    // ...but insert bubble to guarantee grant's eventual forward progress
    when (tl_out.d.valid) {
      io.cpu.req.ready := false
      dataArb.io.in(1).valid := true
      dataArb.io.in(1).bits.write := false
      blockUncachedGrant := !dataArb.io.in(1).ready
    }
  }

  // Handle an incoming TileLink Probe message
  val block_probe = releaseInFlight || grantInProgress || blockProbeAfterGrantCount > 0 || lrscValid || (s2_valid_hit && s2_lr)
  metaArb.io.in(6).valid := tl_out.b.valid && !block_probe
  tl_out.b.ready := metaArb.io.in(6).ready && !block_probe && !s1_valid && (!s2_valid || s2_valid_hit)
  metaArb.io.in(6).bits.write := false
  metaArb.io.in(6).bits.addr := Cat(io.cpu.req.bits.addr >> paddrBits, tl_out.b.bits.address)
  metaArb.io.in(6).bits.way_en := ~UInt(0, nWays)
  metaArb.io.in(6).bits.data := metaArb.io.in(4).bits.data

  // release
  val (c_first, c_last, releaseDone, c_count) = edge.count(tl_out_c)
  val releaseRejected = tl_out_c.valid && !tl_out_c.ready
  val s1_release_data_valid = Reg(next = dataArb.io.in(2).fire())
  val s2_release_data_valid = Reg(next = s1_release_data_valid && !releaseRejected)
  val releaseDataBeat = Cat(UInt(0), c_count) + Mux(releaseRejected, UInt(0), s1_release_data_valid + Cat(UInt(0), s2_release_data_valid))
  val writeback_data_error = s2_data_decoded.map(_.error).reduce(_||_)
  val writeback_data_uncorrectable = s2_data_decoded.map(_.uncorrectable).reduce(_||_)

  val nackResponseMessage = edge.ProbeAck(b = probe_bits, reportPermissions = TLPermissions.NtoN)
  val cleanReleaseMessage = edge.ProbeAck(b = probe_bits, reportPermissions = s2_report_param)
  val dirtyReleaseMessage = edge.ProbeAck(b = probe_bits, reportPermissions = s2_report_param, data = 0.U)

  tl_out_c.valid := s2_release_data_valid
  tl_out_c.bits := nackResponseMessage
  val newCoh = Wire(init = probeNewCoh)
  releaseWay := s2_probe_way

  when (s2_victimize && s2_victim_dirty) {
    assert(!(s2_valid && s2_hit_valid && !s2_data_error))
    release_state := s_voluntary_writeback
    probe_bits.address := Cat(s2_victim_tag, s2_req.addr(idxMSB, idxLSB)) << idxLSB
  }
  when (s2_probe) {
    val probeNack = Wire(init = true.B)
    when (s2_meta_error) {
      release_state := s_probe_retry
    }.elsewhen (s2_prb_ack_data) {
      release_state := s_probe_rep_dirty
    }.elsewhen (s2_probe_state.isValid()) {
      tl_out_c.valid := true
      tl_out_c.bits := cleanReleaseMessage
      release_state := Mux(releaseDone, s_probe_write_meta, s_probe_rep_clean)
    }.otherwise {
      tl_out_c.valid := true
      probeNack := !releaseDone
      release_state := Mux(releaseDone, s_ready, s_probe_rep_miss)
    }
    when (probeNack) { s1_nack := true }
  }
  when (release_state === s_probe_retry) {
    metaArb.io.in(6).valid := true
    metaArb.io.in(6).bits.addr := Cat(io.cpu.req.bits.addr >> paddrBits, probe_bits.address)
    when (metaArb.io.in(6).ready) {
      release_state := s_ready
      s1_probe := true
    }
  }
  when (release_state === s_probe_rep_miss) {
    tl_out_c.valid := true
    when (releaseDone) { release_state := s_ready }
  }
  when (release_state === s_probe_rep_clean) {
    tl_out_c.valid := true
    tl_out_c.bits := cleanReleaseMessage
    when (releaseDone) { release_state := s_probe_write_meta }
  }
  when (release_state === s_probe_rep_dirty) {
    tl_out_c.bits := dirtyReleaseMessage
    when (releaseDone) { release_state := s_probe_write_meta }
  }
  when (release_state.isOneOf(s_voluntary_writeback, s_voluntary_write_meta)) {
    if (edge.manager.anySupportAcquireB)
      tl_out_c.bits := edge.Release(fromSource = 0.U,
                                    toAddress = 0.U,
                                    lgSize = lgCacheBlockBytes,
                                    shrinkPermissions = s2_shrink_param,
                                    data = 0.U)._2
    newCoh := voluntaryNewCoh
    releaseWay := s2_victim_way
    when (releaseDone) { release_state := s_voluntary_write_meta }
    when (tl_out_c.fire() && c_first) { release_ack_wait := true }
  }
  tl_out_c.bits.address := probe_bits.address
  tl_out_c.bits.data := s2_data_corrected
  tl_out_c.bits.error := inWriteback && {
    val accrued = Reg(Bool())
    val next = writeback_data_uncorrectable || (accrued && !c_first)
    when (tl_out_c.fire()) { accrued := next }
    next
  }

  dataArb.io.in(2).valid := inWriteback && releaseDataBeat < refillCycles
  dataArb.io.in(2).bits.write := false
  dataArb.io.in(2).bits.addr := tl_out_c.bits.address | (releaseDataBeat(log2Up(refillCycles)-1,0) << rowOffBits)
  dataArb.io.in(2).bits.wordMask := ~UInt(0, rowBytes / wordBytes)
  dataArb.io.in(2).bits.way_en := ~UInt(0, nWays)

  metaArb.io.in(4).valid := release_state.isOneOf(s_voluntary_write_meta, s_probe_write_meta)
  metaArb.io.in(4).bits.write := true
  metaArb.io.in(4).bits.way_en := releaseWay
  metaArb.io.in(4).bits.addr := Cat(io.cpu.req.bits.addr >> untagBits, tl_out_c.bits.address(idxMSB, 0))
  metaArb.io.in(4).bits.data.coh := newCoh
  metaArb.io.in(4).bits.data.tag := tl_out_c.bits.address >> untagBits
  when (metaArb.io.in(4).fire()) { release_state := s_ready }

  // cached response
  io.cpu.resp.valid := s2_valid_hit
  io.cpu.resp.bits <> s2_req
  io.cpu.resp.bits.has_data := s2_read
  io.cpu.resp.bits.replay := false
  io.cpu.ordered := !(s1_valid || s2_valid || cached_grant_wait || uncachedInFlight.asUInt.orR)

  val s1_xcpt_valid = tlb.io.req.valid && !s1_nack
  val s1_xcpt = tlb.io.resp
  io.cpu.s2_xcpt := Mux(RegNext(s1_xcpt_valid), RegEnable(s1_xcpt, s1_valid_not_nacked), 0.U.asTypeOf(s1_xcpt))
  when (s2_valid_pre_xcpt && s2_tl_error) {
    assert(!s2_valid_hit && !s2_uncached)
    when (s2_write) { io.cpu.s2_xcpt.ae.st := true }
    when (s2_read) { io.cpu.s2_xcpt.ae.ld := true }
  }

  if (usingDataScratchpad) {
    require(!usingVM) // therefore, req.phys means this is a slave-port access
    val s2_isSlavePortAccess = s2_req.phys
    when (s2_isSlavePortAccess) {
      assert(!s2_valid || s2_hit_valid)
      io.cpu.s2_xcpt := 0.U.asTypeOf(io.cpu.s2_xcpt)
    }
    assert(!(s2_valid_masked && s2_req.cmd.isOneOf(M_XLR, M_XSC)))
  }

  // uncached response
  io.cpu.replay_next := tl_out.d.fire() && grantIsUncachedData
  val doUncachedResp = Reg(next = io.cpu.replay_next)
  when (doUncachedResp) {
    assert(!s2_valid_hit)
    io.cpu.resp.valid := true
    io.cpu.resp.bits.replay := true
    io.cpu.resp.bits.addr := s2_uncached_resp_addr
  }

  // load data subword mux/sign extension
  val s2_data_word = ((0 until rowBits by wordBits).map(i => s2_data_uncorrected(wordBits+i-1,i)): Seq[UInt])(s2_word_idx)
  val s2_data_word_corrected = ((0 until rowBits by wordBits).map(i => s2_data_corrected(wordBits+i-1,i)): Seq[UInt])(s2_word_idx)
  val loadgen = new LoadGen(s2_req.typ, mtSigned(s2_req.typ), s2_req.addr, s2_data_word, s2_sc, wordBytes)
  io.cpu.resp.bits.data := loadgen.data | s2_sc_fail
  io.cpu.resp.bits.data_word_bypass := loadgen.wordData
  io.cpu.resp.bits.data_raw := s2_data_word
  io.cpu.resp.bits.store_data := pstore1_data

  // AMOs
  if (usingRMW) {
    val amoalu = Module(new AMOALU(xLen))
    amoalu.io.mask := pstore1_mask
    amoalu.io.cmd := (if (usingAtomics) pstore1_cmd else M_XWR)
    amoalu.io.lhs := s2_data_word
    amoalu.io.rhs := pstore1_data
    pstore1_storegen_data := amoalu.io.out
  } else {
    assert(!(s1_valid_masked && s1_read && s1_write), "unsupported D$ operation")
  }
  when (s2_correct) { pstore1_storegen_data := s2_data_word_corrected }

  // flushes
  val resetting = Reg(init=Bool(true))
  val flushed = Reg(init=Bool(true))
  val flushing = Reg(init=Bool(false))
  val flushCounter = Reg(init=UInt(nSets * (nWays-1), log2Ceil(nSets * nWays)))
  val flushCounterNext = flushCounter +& 1
  val flushDone = (flushCounterNext >> log2Ceil(nSets)) === nWays
  val flushCounterWrap = flushCounterNext(log2Ceil(nSets)-1, 0)
  when (s2_valid_masked && s2_req.cmd === M_FLUSH_ALL) {
    io.cpu.s2_nack := !flushed
    when (!flushed) {
      flushing := !release_ack_wait && !uncachedInFlight.asUInt.orR
    }
  }
  s1_flush_valid := metaArb.io.in(5).fire() && !s1_flush_valid && !s2_flush_valid_pre_tag_ecc && release_state === s_ready && !release_ack_wait
  metaArb.io.in(5).valid := flushing
  metaArb.io.in(5).bits.write := false
  metaArb.io.in(5).bits.addr := Cat(io.cpu.req.bits.addr >> untagBits, flushCounter(idxBits-1, 0) << blockOffBits)
  metaArb.io.in(5).bits.way_en := ~UInt(0, nWays)
  metaArb.io.in(5).bits.data := metaArb.io.in(4).bits.data
  // Only flush D$ on FENCE.I if some cached executable regions are untracked.
  if (!edge.manager.managers.forall(m => !m.supportsAcquireB || !m.executable || m.regionType >= RegionType.TRACKED)) {
    when (tl_out_a.fire() && !s2_uncached) { flushed := false }
    when (flushing) {
      s1_victim_way := flushCounter >> log2Up(nSets)
      when (s2_flush_valid) {
        flushCounter := flushCounterNext
        when (flushDone) {
          flushed := true
          if (!isPow2(nWays)) flushCounter := flushCounterWrap
        }
      }
      when (flushed && release_state === s_ready && !release_ack_wait) {
        flushing := false
      }
    }
  }
  metaArb.io.in(0).valid := resetting
  metaArb.io.in(0).bits.addr := metaArb.io.in(5).bits.addr
  metaArb.io.in(0).bits.write := true
  metaArb.io.in(0).bits.way_en := ~UInt(0, nWays)
  metaArb.io.in(0).bits.data.coh := ClientMetadata.onReset
  metaArb.io.in(0).bits.data.tag := s2_req.addr >> untagBits
  when (resetting) {
    flushCounter := flushCounterNext
    when (flushDone) {
      resetting := false
      if (!isPow2(nWays)) flushCounter := flushCounterWrap
    }
  }

  // performance events
  io.cpu.perf.acquire := edge.done(tl_out_a)
  io.cpu.perf.release := edge.done(tl_out_c)
  io.cpu.perf.tlbMiss := io.ptw.req.fire()

  // report errors
  {
    val (data_error, data_error_uncorrectable, data_error_addr) =
      if (usingDataScratchpad) (s2_valid_data_error, s2_data_error_uncorrectable, s2_req.addr) else {
        (tl_out_c.fire() && inWriteback && writeback_data_error,
         writeback_data_uncorrectable,
         tl_out_c.bits.address)
      }
    val error_addr =
      Mux(metaArb.io.in(1).valid, Cat(metaArb.io.in(1).bits.data.tag, metaArb.io.in(1).bits.addr(untagBits-1, idxLSB)),
          data_error_addr >> idxLSB) << idxLSB
    io.errors.uncorrectable.foreach { u =>
      u.valid := metaArb.io.in(1).valid && s2_meta_error_uncorrectable || data_error && data_error_uncorrectable
      u.bits := error_addr
    }
    io.errors.correctable.foreach { c =>
      c.valid := metaArb.io.in(1).valid || data_error
      c.bits := error_addr
      io.errors.uncorrectable.foreach { u => when (u.valid) { c.valid := false } }
    }
    io.errors.bus.valid := tl_out.d.fire() && tl_out.d.bits.error
    io.errors.bus.bits := Mux(grantIsCached, s2_req.addr >> idxLSB << idxLSB, 0.U)
  }

  def encodeData(x: UInt) = x.grouped(eccBits).map(dECC.encode(_)).asUInt
  def dummyEncodeData(x: UInt) = x.grouped(eccBits).map(dECC.swizzle(_)).asUInt
  def decodeData(x: UInt) = x.grouped(dECC.width(eccBits)).map(dECC.decode(_))
  def eccMask(byteMask: UInt) = byteMask.grouped(eccBytes).map(_.orR).asUInt
  def eccByteMask(byteMask: UInt) = FillInterleaved(eccBytes, eccMask(byteMask))

  def needsRead(req: HellaCacheReq) =
    isRead(req.cmd) ||
    (isWrite(req.cmd) && (req.cmd === M_PWR || mtSize(req.typ) < log2Ceil(eccBytes)))
}
