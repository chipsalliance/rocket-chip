// See LICENSE for license details.

package rocket

import Chisel._
import junctions._
import diplomacy._
import uncore.tilelink._
import uncore.tilelink2._
import uncore.agents._
import uncore.coherence._
import uncore.constants._
import uncore.util._
import util._
import Chisel.ImplicitConversions._
import cde.{Parameters, Field}

class DCacheDataReq(implicit p: Parameters) extends L1HellaCacheBundle()(p) {
  val addr = Bits(width = untagBits)
  val write = Bool()
  val wdata = Bits(width = rowBits)
  val wmask = Bits(width = rowBytes)
  val way_en = Bits(width = nWays)
}

class DCacheDataArray(implicit p: Parameters) extends L1HellaCacheModule()(p) {
  val io = new Bundle {
    val req = Valid(new DCacheDataReq).flip
    val resp = Vec(nWays, Bits(OUTPUT, rowBits))
  }

  val addr = io.req.bits.addr >> rowOffBits
  for (w <- 0 until nWays) {
    val array = SeqMem(nSets*refillCycles, Vec(rowBytes, Bits(width=8)))
    val valid = io.req.valid && (Bool(nWays == 1) || io.req.bits.way_en(w))
    when (valid && io.req.bits.write) {
      val data = Vec.tabulate(rowBytes)(i => io.req.bits.wdata(8*(i+1)-1, 8*i))
      array.write(addr, data, io.req.bits.wmask.toBools)
    }
    io.resp(w) := array.read(addr, valid && !io.req.bits.write).asUInt
  }
}

class DCache(implicit p: Parameters) extends L1HellaCacheModule()(p) {
  val io = new Bundle {
    val cpu = (new HellaCacheIO).flip
    val ptw = new TLBPTWIO()
    val mem = new ClientTileLinkIO
  }

  val fq = Module(new FinishQueue(1))

  require(rowBits == encRowBits) // no ECC
  require(refillCyclesPerBeat == 1)
  require(rowBits >= coreDataBits)

  // tags
  val replacer = p(Replacer)()
  def onReset = L1Metadata(UInt(0), ClientMetadata.onReset)
  val metaReadArb = Module(new Arbiter(new MetaReadReq, 3))
  val metaWriteArb = Module(new Arbiter(new L1MetaWriteReq, 3))

  // data
  val data = Module(new DCacheDataArray)
  val dataArb = Module(new Arbiter(new DCacheDataReq, 4))
  data.io.req <> dataArb.io.out
  dataArb.io.out.ready := true

  val s1_valid = Reg(next=io.cpu.req.fire(), init=Bool(false))
  val s1_probe = Reg(next=io.mem.probe.fire(), init=Bool(false))
  val probe_bits = RegEnable(io.mem.probe.bits, io.mem.probe.fire())
  val s1_nack = Wire(init=Bool(false))
  val s1_valid_masked = s1_valid && !io.cpu.s1_kill && !io.cpu.xcpt.asUInt.orR
  val s1_valid_not_nacked = s1_valid_masked && !s1_nack
  val s1_req = Reg(io.cpu.req.bits)
  when (metaReadArb.io.out.valid) {
    s1_req := io.cpu.req.bits
    s1_req.addr := Cat(io.cpu.req.bits.addr >> untagBits, metaReadArb.io.out.bits.idx, io.cpu.req.bits.addr(blockOffBits-1,0))
  }
  val s1_read = isRead(s1_req.cmd)
  val s1_write = isWrite(s1_req.cmd)
  val s1_readwrite = s1_read || s1_write
  val s1_flush_valid = Reg(Bool())

  val s_ready :: s_voluntary_writeback :: s_probe_rep_dirty :: s_probe_rep_clean :: s_probe_rep_miss :: s_voluntary_write_meta :: s_probe_write_meta :: Nil = Enum(UInt(), 7)
  val cached_grant_wait = Reg(init=Bool(false))
  val release_ack_wait = Reg(init=Bool(false))
  val release_state = Reg(init=s_ready)
  val pstore1_valid = Wire(Bool())
  val pstore2_valid = Reg(Bool())
  val inWriteback = release_state.isOneOf(s_voluntary_writeback, s_probe_rep_dirty)
  val releaseWay = Wire(UInt())
  io.cpu.req.ready := (release_state === s_ready) && !cached_grant_wait && !s1_nack

  // I/O MSHRs
  val maxUncachedInFlight = (1 << io.mem.acquire.bits.client_xact_id.getWidth) - 1
  val uncachedInFlight = Reg(init=Vec.fill(maxUncachedInFlight)(Bool(false)))
  val uncachedReqs = Reg(Vec(maxUncachedInFlight, new HellaCacheReq))

  // hit initiation path
  dataArb.io.in(3).valid := io.cpu.req.valid && isRead(io.cpu.req.bits.cmd)
  dataArb.io.in(3).bits.write := false
  dataArb.io.in(3).bits.addr := io.cpu.req.bits.addr
  dataArb.io.in(3).bits.way_en := ~UInt(0, nWays)
  when (!dataArb.io.in(3).ready && isRead(io.cpu.req.bits.cmd)) { io.cpu.req.ready := false }
  metaReadArb.io.in(2).valid := io.cpu.req.valid
  metaReadArb.io.in(2).bits.idx := io.cpu.req.bits.addr(idxMSB, idxLSB)
  metaReadArb.io.in(2).bits.way_en := ~UInt(0, nWays)
  when (!metaReadArb.io.in(2).ready) { io.cpu.req.ready := false }

  // address translation
  val tlb = Module(new TLB)
  io.ptw <> tlb.io.ptw
  tlb.io.req.valid := s1_valid_masked && s1_readwrite
  tlb.io.req.bits.passthrough := s1_req.phys
  tlb.io.req.bits.vpn := s1_req.addr >> pgIdxBits
  tlb.io.req.bits.instruction := false
  tlb.io.req.bits.store := s1_write
  when (!tlb.io.req.ready && !io.cpu.req.bits.phys) { io.cpu.req.ready := false }
  when (s1_valid && s1_readwrite && tlb.io.resp.miss) { s1_nack := true }

  val s1_paddr = Cat(tlb.io.resp.ppn, s1_req.addr(pgIdxBits-1,0))
  val s1_tag = Mux(s1_probe, probe_bits.addr_block >> idxBits, s1_paddr(paddrBits-1, untagBits))
  val s1_victim_way = Wire(init = replacer.way)
  val (s1_hit_way, s1_hit_state, s1_victim_meta) =
    if (usingDataScratchpad) {
      require(nWays == 1)
      metaWriteArb.io.out.ready := true
      metaReadArb.io.out.ready := !metaWriteArb.io.out.valid
      val inScratchpad = addrMap(s"io:cbus:dmem${tileId}").containsAddress(s1_paddr)
      val hitState = Mux(inScratchpad, ClientMetadata.onReset.onHit(M_XWR), ClientMetadata.onReset)
      (inScratchpad, hitState, L1Metadata(UInt(0), ClientMetadata.onReset))
    } else {
      val meta = Module(new MetadataArray(onReset _))
      meta.io.read <> metaReadArb.io.out
      meta.io.write <> metaWriteArb.io.out
      val s1_meta = meta.io.resp
      val s1_meta_hit_way = s1_meta.map(r => r.coh.isValid() && r.tag === s1_tag).asUInt
      val s1_meta_hit_state = ClientMetadata.onReset.fromBits(
        s1_meta.map(r => Mux(r.tag === s1_tag, r.coh.asUInt, UInt(0)))
        .reduce (_|_))
      (s1_meta_hit_way, s1_meta_hit_state, s1_meta(s1_victim_way))
    }
  val s1_data_way = Mux(inWriteback, releaseWay, s1_hit_way)
  val s1_data = Mux1H(s1_data_way, data.io.resp) // retime into s2 if critical

  val s2_valid = Reg(next=s1_valid_masked, init=Bool(false))
  val s2_probe = Reg(next=s1_probe, init=Bool(false))
  val releaseInFlight = s1_probe || s2_probe || release_state =/= s_ready
  val s2_valid_masked = s2_valid && Reg(next = !s1_nack)
  val s2_req = Reg(io.cpu.req.bits)
  val s2_uncached = Reg(Bool())
  when (s1_valid_not_nacked || s1_flush_valid) {
    s2_req := s1_req
    s2_req.addr := s1_paddr
    s2_uncached := !tlb.io.resp.cacheable || Bool(usingDataScratchpad)
  }
  val s2_read = isRead(s2_req.cmd)
  val s2_write = isWrite(s2_req.cmd)
  val s2_readwrite = s2_read || s2_write
  val s2_flush_valid = RegNext(s1_flush_valid)
  val s2_data = RegEnable(s1_data, s1_valid || inWriteback)
  val s2_probe_way = RegEnable(s1_hit_way, s1_probe)
  val s2_probe_state = RegEnable(s1_hit_state, s1_probe)
  val s2_hit_way = RegEnable(s1_hit_way, s1_valid_not_nacked)
  val s2_hit_state = RegEnable(s1_hit_state, s1_valid_not_nacked)
  val s2_hit = s2_hit_state.isHit(s2_req.cmd)
  val s2_valid_hit = s2_valid_masked && s2_readwrite && s2_hit
  val s2_valid_miss = s2_valid_masked && s2_readwrite && !s2_hit && !(pstore1_valid || pstore2_valid) && !release_ack_wait
  val s2_valid_cached_miss = s2_valid_miss && !s2_uncached
  val s2_victimize = s2_valid_cached_miss || s2_flush_valid
  val s2_valid_uncached = s2_valid_miss && s2_uncached
  val s2_victim_way = Mux(s2_hit_state.isValid() && !s2_flush_valid, s2_hit_way, UIntToOH(RegEnable(s1_victim_way, s1_valid_not_nacked || s1_flush_valid)))
  val s2_victim_tag = RegEnable(s1_victim_meta.tag, s1_valid_not_nacked || s1_flush_valid)
  val s2_victim_state = Mux(s2_hit_state.isValid() && !s2_flush_valid, s2_hit_state, RegEnable(s1_victim_meta.coh, s1_valid_not_nacked || s1_flush_valid))
  val s2_victim_valid = s2_victim_state.isValid()
  val s2_victim_dirty = s2_victim_state.requiresVoluntaryWriteback()
  val s2_new_hit_state = s2_hit_state.onHit(s2_req.cmd)
  val s2_update_meta = s2_hit_state =/= s2_new_hit_state
  io.cpu.s2_nack := s2_valid && !s2_valid_hit && !(s2_valid_uncached && io.mem.acquire.ready && !uncachedInFlight.asUInt.andR)
  when (s2_valid && (!s2_valid_hit || s2_update_meta)) { s1_nack := true }

  // exceptions
  val s1_storegen = new StoreGen(s1_req.typ, s1_req.addr, UInt(0), wordBytes)
  io.cpu.xcpt.ma.ld := s1_read && s1_storegen.misaligned
  io.cpu.xcpt.ma.st := s1_write && s1_storegen.misaligned
  io.cpu.xcpt.pf.ld := s1_read && tlb.io.resp.xcpt_ld
  io.cpu.xcpt.pf.st := s1_write && tlb.io.resp.xcpt_st

  // load reservations
  val s2_lr = Bool(usingAtomics) && s2_req.cmd === M_XLR
  val s2_sc = Bool(usingAtomics) && s2_req.cmd === M_XSC
  val lrscCount = Reg(init=UInt(0))
  val lrscValid = lrscCount > 0
  val lrscAddr = Reg(UInt())
  val s2_sc_fail = s2_sc && !(lrscValid && lrscAddr === (s2_req.addr >> blockOffBits))
  when (s2_valid_hit && s2_lr) {
    lrscCount := lrscCycles - 1
    lrscAddr := s2_req.addr >> blockOffBits
  }
  when (lrscValid) { lrscCount := lrscCount - 1 }
  when ((s2_valid_masked && lrscValid) || io.cpu.invalidate_lr) { lrscCount := 0 }

  // pending store buffer
  val pstore1_cmd = RegEnable(s1_req.cmd, s1_valid_not_nacked && s1_write)
  val pstore1_typ = RegEnable(s1_req.typ, s1_valid_not_nacked && s1_write)
  val pstore1_addr = RegEnable(s1_paddr, s1_valid_not_nacked && s1_write)
  val pstore1_data = RegEnable(io.cpu.s1_data, s1_valid_not_nacked && s1_write)
  val pstore1_way = RegEnable(s1_hit_way, s1_valid_not_nacked && s1_write)
  val pstore1_storegen = new StoreGen(pstore1_typ, pstore1_addr, pstore1_data, wordBytes)
  val pstore1_storegen_data = Wire(init = pstore1_storegen.data)
  val pstore1_amo = Bool(usingAtomics) && isRead(pstore1_cmd)
  val pstore_drain_structural = pstore1_valid && pstore2_valid && ((s1_valid && s1_write) || pstore1_amo)
  val pstore_drain_opportunistic = !(io.cpu.req.valid && isRead(io.cpu.req.bits.cmd))
  val pstore_drain_on_miss = releaseInFlight || io.cpu.s2_nack
  val pstore_drain =
    Bool(usingAtomics) && pstore_drain_structural ||
    (((pstore1_valid && !pstore1_amo) || pstore2_valid) && (pstore_drain_opportunistic || pstore_drain_on_miss))
  pstore1_valid := {
    val s2_store_valid = s2_valid_hit && s2_write && !s2_sc_fail
    val pstore1_held = Reg(Bool())
    assert(!s2_store_valid || !pstore1_held)
    pstore1_held := (s2_store_valid || pstore1_held) && pstore2_valid && !pstore_drain
    s2_store_valid || pstore1_held
  }
  val advance_pstore1 = pstore1_valid && (pstore2_valid === pstore_drain)
  pstore2_valid := pstore2_valid && !pstore_drain || advance_pstore1
  val pstore2_addr = RegEnable(pstore1_addr, advance_pstore1)
  val pstore2_way = RegEnable(pstore1_way, advance_pstore1)
  val pstore2_storegen_data = RegEnable(pstore1_storegen_data, advance_pstore1)
  val pstore2_storegen_mask = RegEnable(pstore1_storegen.mask, advance_pstore1)
  dataArb.io.in(0).valid := pstore_drain
  dataArb.io.in(0).bits.write := true
  dataArb.io.in(0).bits.addr := Mux(pstore2_valid, pstore2_addr, pstore1_addr)
  dataArb.io.in(0).bits.way_en := Mux(pstore2_valid, pstore2_way, pstore1_way)
  dataArb.io.in(0).bits.wdata := Fill(rowWords, Mux(pstore2_valid, pstore2_storegen_data, pstore1_storegen_data))
  val pstore_mask_shift = Mux(pstore2_valid, pstore2_addr, pstore1_addr).extract(rowOffBits-1,offsetlsb) << wordOffBits
  dataArb.io.in(0).bits.wmask := Mux(pstore2_valid, pstore2_storegen_mask, pstore1_storegen.mask) << pstore_mask_shift

  // store->load RAW hazard detection
  val s1_idx = s1_req.addr(idxMSB, wordOffBits)
  val s1_raw_hazard = s1_read &&
    ((pstore1_valid && pstore1_addr(idxMSB, wordOffBits) === s1_idx && (pstore1_storegen.mask & s1_storegen.mask).orR) ||
     (pstore2_valid && pstore2_addr(idxMSB, wordOffBits) === s1_idx && (pstore2_storegen_mask & s1_storegen.mask).orR))
  when (s1_valid && s1_raw_hazard) { s1_nack := true }

  metaWriteArb.io.in(0).valid := (s2_valid_hit && s2_update_meta) || (s2_victimize && !s2_victim_dirty)
  metaWriteArb.io.in(0).bits.way_en := s2_victim_way
  metaWriteArb.io.in(0).bits.idx := s2_req.addr(idxMSB, idxLSB)
  metaWriteArb.io.in(0).bits.data.coh := Mux(s2_valid_hit, s2_new_hit_state, ClientMetadata.onReset)
  metaWriteArb.io.in(0).bits.data.tag := s2_req.addr(paddrBits-1, untagBits)

  // acquire
  val xact_id = PriorityEncoder(~uncachedInFlight.asUInt)
  val cachedGetMessage = s2_hit_state.makeAcquire(
    client_xact_id = UInt(maxUncachedInFlight - 1),
    addr_block = s2_req.addr(paddrBits-1, blockOffBits),
    op_code = s2_req.cmd)
  val uncachedGetMessage = Get(
    client_xact_id = xact_id,
    addr_block = s2_req.addr(paddrBits-1, blockOffBits),
    addr_beat = s2_req.addr(blockOffBits-1, beatOffBits),
    addr_byte = s2_req.addr(beatOffBits-1, 0),
    operand_size = s2_req.typ,
    alloc = Bool(false))
  val uncachedPutOffset = s2_req.addr.extract(beatOffBits-1, wordOffBits)
  val uncachedPutMessage = Put(
    client_xact_id = xact_id,
    addr_block = s2_req.addr(paddrBits-1, blockOffBits),
    addr_beat = s2_req.addr(blockOffBits-1, beatOffBits),
    data = Fill(beatWords, pstore1_storegen.data),
    wmask = Some(pstore1_storegen.mask << (uncachedPutOffset << wordOffBits)),
    alloc = Bool(false))
  val uncachedPutAtomicMessage = PutAtomic(
    client_xact_id = xact_id,
    addr_block = s2_req.addr(paddrBits-1, blockOffBits),
    addr_beat = s2_req.addr(blockOffBits-1, beatOffBits),
    addr_byte = s2_req.addr(beatOffBits-1, 0),
    atomic_opcode = s2_req.cmd,
    operand_size = s2_req.typ,
    data = Fill(beatWords, pstore1_storegen.data))
  io.mem.acquire.valid := ((s2_valid_cached_miss && !s2_victim_dirty) || (s2_valid_uncached && !uncachedInFlight.asUInt.andR)) && fq.io.enq.ready
  io.mem.acquire.bits := cachedGetMessage
  when (s2_uncached) {
    if (!usingDataScratchpad)
      assert(!s2_valid_masked || !s2_hit_state.isValid(), "cache hit on uncached access")
    io.mem.acquire.bits := uncachedGetMessage
    when (s2_write) {
      io.mem.acquire.bits := uncachedPutMessage
      when (pstore1_amo) {
        io.mem.acquire.bits := uncachedPutAtomicMessage
      }
    }
  }
  when (io.mem.acquire.fire()) {
    when (s2_uncached) {
      uncachedInFlight(xact_id) := true
      uncachedReqs(xact_id) := s2_req
    }.otherwise {
      cached_grant_wait := true
    }
  }

  // grant
  val grantIsRefill = io.mem.grant.bits.hasMultibeatData()
  val grantIsVoluntary = io.mem.grant.bits.isVoluntary()
  val grantIsUncached = !grantIsRefill && !grantIsVoluntary
  io.mem.grant.ready := true
  when (io.mem.grant.fire()) {
    when (grantIsRefill) { assert(cached_grant_wait) }
    when (grantIsUncached) {
      assert(uncachedInFlight(io.mem.grant.bits.client_xact_id))
      uncachedInFlight(io.mem.grant.bits.client_xact_id) := false
      s2_data := io.mem.grant.bits.data
      val req = uncachedReqs(io.mem.grant.bits.client_xact_id)
      s2_req.cmd := req.cmd
      s2_req.typ := req.typ
      s2_req.tag := req.tag
      s2_req.addr := Cat(s1_paddr >> wordOffBits /* don't-care */, req.addr(wordOffBits-1, 0))
    }
    when (grantIsVoluntary) {
      assert(release_ack_wait)
      release_ack_wait := false
    }
  }
  val (refillCount, refillDone) = Counter(io.mem.grant.fire() && grantIsRefill, refillCycles)
  when (io.mem.grant.fire() && refillDone) { cached_grant_wait := false }

  // data refill
  val doRefillBeat = grantIsRefill && io.mem.grant.valid
  dataArb.io.in(1).valid := doRefillBeat
  assert(dataArb.io.in(1).ready || !doRefillBeat)
  dataArb.io.in(1).bits.write := true
  dataArb.io.in(1).bits.addr := Cat(s2_req.addr(paddrBits-1, blockOffBits), io.mem.grant.bits.addr_beat) << beatOffBits
  dataArb.io.in(1).bits.way_en := s2_victim_way
  dataArb.io.in(1).bits.wdata := io.mem.grant.bits.data
  dataArb.io.in(1).bits.wmask := ~UInt(0, rowBytes)
  // tag updates on refill
  metaWriteArb.io.in(1).valid := refillDone
  assert(!metaWriteArb.io.in(1).valid || metaWriteArb.io.in(1).ready)
  metaWriteArb.io.in(1).bits.way_en := s2_victim_way
  metaWriteArb.io.in(1).bits.idx := s2_req.addr(idxMSB, idxLSB)
  metaWriteArb.io.in(1).bits.data.coh := s2_hit_state.onGrant(io.mem.grant.bits, s2_req.cmd)
  metaWriteArb.io.in(1).bits.data.tag := s2_req.addr(paddrBits-1, untagBits)
  // don't accept uncached grants if there's a structural hazard on s2_data...
  val blockUncachedGrant = Reg(Bool())
  blockUncachedGrant := dataArb.io.out.valid
  when (grantIsUncached) {
    io.mem.grant.ready := !(blockUncachedGrant || s1_valid)
    // ...but insert bubble to guarantee grant's eventual forward progress
    when (io.mem.grant.valid && !io.mem.grant.ready) {
      io.cpu.req.ready := false
      dataArb.io.in(1).valid := true
      dataArb.io.in(1).bits.write := false
      blockUncachedGrant := !dataArb.io.in(1).ready
    }
  }

  // finish
  fq.io.enq.valid := io.mem.grant.fire() && io.mem.grant.bits.requiresAck() && (!grantIsRefill || refillDone)
  fq.io.enq.bits := io.mem.grant.bits.makeFinish()
  io.mem.finish <> fq.io.deq
  when (fq.io.enq.valid) { assert(fq.io.enq.ready) }
  when (refillDone) { replacer.miss }

  // probe
  val block_probe = releaseInFlight || lrscValid || (s2_valid_hit && s2_lr)
  metaReadArb.io.in(1).valid := io.mem.probe.valid && !block_probe
  io.mem.probe.ready := metaReadArb.io.in(1).ready && !block_probe && !s1_valid && (!s2_valid || s2_valid_hit)
  metaReadArb.io.in(1).bits.idx := io.mem.probe.bits.addr_block
  metaReadArb.io.in(1).bits.way_en := ~UInt(0, nWays)

  // release
  val (writebackCount, writebackDone) = Counter(io.mem.release.fire() && inWriteback, refillCycles)
  val releaseDone = writebackDone || (io.mem.release.fire() && !inWriteback)
  val releaseRejected = io.mem.release.valid && !io.mem.release.ready
  val s1_release_data_valid = Reg(next = dataArb.io.in(2).fire())
  val s2_release_data_valid = Reg(next = s1_release_data_valid && !releaseRejected)
  val releaseDataBeat = Cat(UInt(0), writebackCount) + Mux(releaseRejected, UInt(0), s1_release_data_valid + Cat(UInt(0), s2_release_data_valid))
  io.mem.release.valid := s2_release_data_valid
  io.mem.release.bits := ClientMetadata.onReset.makeRelease(probe_bits)
  val voluntaryReleaseMessage = s2_victim_state.makeVoluntaryWriteback(UInt(maxUncachedInFlight - 1), UInt(0))
  val voluntaryNewCoh = s2_victim_state.onCacheControl(M_FLUSH)
  val probeResponseMessage = s2_probe_state.makeRelease(probe_bits)
  val probeNewCoh = s2_probe_state.onProbe(probe_bits)
  val newCoh = Wire(init = probeNewCoh)
  releaseWay := s2_probe_way
  when (s2_victimize && s2_victim_dirty) {
    assert(!(s2_valid && s2_hit_state.isValid()))
    release_state := s_voluntary_writeback
    probe_bits.addr_block := Cat(s2_victim_tag, s2_req.addr(idxMSB, idxLSB))
  }
  when (s2_probe) {
    when (s2_probe_state.requiresVoluntaryWriteback()) { release_state := s_probe_rep_dirty }
    .elsewhen (s2_probe_state.isValid()) { release_state := s_probe_rep_clean }
    .otherwise {
      io.mem.release.valid := true
      release_state := s_probe_rep_miss
    }
  }
  when (releaseDone) { release_state := s_ready }
  when (release_state.isOneOf(s_probe_rep_miss, s_probe_rep_clean)) {
    io.mem.release.valid := true
  }
  when (release_state.isOneOf(s_probe_rep_clean, s_probe_rep_dirty)) {
    io.mem.release.bits := probeResponseMessage
    when (releaseDone) { release_state := s_probe_write_meta }
  }
  when (release_state.isOneOf(s_voluntary_writeback, s_voluntary_write_meta)) {
    io.mem.release.bits := voluntaryReleaseMessage
    newCoh := voluntaryNewCoh
    releaseWay := s2_victim_way
    when (releaseDone) {
      release_state := s_voluntary_write_meta
      release_ack_wait := true
    }
  }
  when (s2_probe && !io.mem.release.fire()) { s1_nack := true }
  io.mem.release.bits.addr_block := probe_bits.addr_block
  io.mem.release.bits.addr_beat := writebackCount
  io.mem.release.bits.data := s2_data

  dataArb.io.in(2).valid := inWriteback && releaseDataBeat < refillCycles
  dataArb.io.in(2).bits.write := false
  dataArb.io.in(2).bits.addr := Cat(io.mem.release.bits.addr_block, releaseDataBeat(log2Up(refillCycles)-1,0)) << rowOffBits
  dataArb.io.in(2).bits.way_en := ~UInt(0, nWays)

  metaWriteArb.io.in(2).valid := release_state.isOneOf(s_voluntary_write_meta, s_probe_write_meta)
  metaWriteArb.io.in(2).bits.way_en := releaseWay
  metaWriteArb.io.in(2).bits.idx := io.mem.release.bits.full_addr()(idxMSB, idxLSB)
  metaWriteArb.io.in(2).bits.data.coh := newCoh
  metaWriteArb.io.in(2).bits.data.tag := io.mem.release.bits.full_addr()(paddrBits-1, untagBits)
  when (metaWriteArb.io.in(2).fire()) { release_state := s_ready }

  // cached response
  io.cpu.resp.valid := s2_valid_hit
  io.cpu.resp.bits <> s2_req
  io.cpu.resp.bits.has_data := s2_read
  io.cpu.resp.bits.replay := false
  io.cpu.ordered := !(s1_valid || s2_valid || cached_grant_wait || uncachedInFlight.asUInt.orR)

  // uncached response
  io.cpu.replay_next := io.mem.grant.fire() && grantIsUncached
  val doUncachedResp = Reg(next = io.cpu.replay_next)
  when (doUncachedResp) {
    assert(!s2_valid_hit)
    io.cpu.resp.valid := true
    io.cpu.resp.bits.replay := true
  }

  // load data subword mux/sign extension
  val s2_word_idx = s2_req.addr.extract(log2Up(rowBits/8)-1, log2Up(wordBytes))
  val s2_data_word = s2_data >> Cat(s2_word_idx, UInt(0, log2Up(coreDataBits)))
  val loadgen = new LoadGen(s2_req.typ, mtSigned(s2_req.typ), s2_req.addr, s2_data_word, s2_sc, wordBytes)
  io.cpu.resp.bits.data := loadgen.data | s2_sc_fail
  io.cpu.resp.bits.data_word_bypass := loadgen.wordData
  io.cpu.resp.bits.store_data := pstore1_data

  // AMOs
  if (usingAtomics) {
    val amoalu = Module(new AMOALU(xLen))
    amoalu.io.addr := pstore1_addr
    amoalu.io.cmd := pstore1_cmd
    amoalu.io.typ := pstore1_typ
    amoalu.io.lhs := s2_data_word
    amoalu.io.rhs := pstore1_data
    pstore1_storegen_data := amoalu.io.out
  } else {
    assert(!(s1_valid_masked && s1_read && s1_write), "unsupported D$ operation")
  }

  // flushes
  val flushed = Reg(init=Bool(true))
  val flushing = Reg(init=Bool(false))
  val flushCounter = Counter(nSets * nWays)
  when (io.mem.acquire.fire() && !s2_uncached) { flushed := false }
  when (s2_valid_masked && s2_req.cmd === M_FLUSH_ALL) {
    io.cpu.s2_nack := !flushed
    when (!flushed) {
      flushing := !release_ack_wait && !uncachedInFlight.asUInt.orR
    }
  }
  s1_flush_valid := metaReadArb.io.in(0).fire() && !s1_flush_valid && !s2_flush_valid && release_state === s_ready && !release_ack_wait
  metaReadArb.io.in(0).valid := flushing
  metaReadArb.io.in(0).bits.idx := flushCounter.value
  metaReadArb.io.in(0).bits.way_en := ~UInt(0, nWays)
  when (flushing) {
    s1_victim_way := flushCounter.value >> log2Up(nSets)
    when (s2_flush_valid) {
      when (flushCounter.inc()) {
        flushed := true
      }
    }
    when (flushed && release_state === s_ready && !release_ack_wait) {
      flushing := false
    }
  }
}

class ScratchpadSlavePort(implicit p: Parameters) extends LazyModule {
  val beatBytes = p(XLen)/8
  val node = TLManagerNode(TLManagerPortParameters(
    Seq(TLManagerParameters(
      address            = List(AddressSet(0x80000000L, p(DataScratchpadSize))),
      regionType         = RegionType.UNCACHED,
      executable         = true,
      supportsPutPartial = TransferSizes(1, beatBytes),
      supportsPutFull    = TransferSizes(1, beatBytes),
      fifoId             = Some(0))), // requests handled in FIFO order
    beatBytes = beatBytes,
    minLatency = 1))

  lazy val module = new LazyModuleImp(this) with HasCoreParameters {
    val io = new Bundle {
      val tl_in = node.bundleIn
      val dmem = new HellaCacheIO
    }

    val tl_in = io.tl_in(0)
    val edge = node.edgesIn(0)
    val beatBytes = edge.manager.beatBytes

    require(coreDataBits == beatBytes*8)
    require(usingDataScratchpad)

    val s_ready :: s_wait :: s_replay :: s_grant :: Nil = Enum(UInt(), 4)
    val state = Reg(init = s_ready)
    when (io.dmem.resp.valid) { state := s_grant }
    when (tl_in.d.fire()) { state := s_ready }
    when (io.dmem.s2_nack) { state := s_replay }
    when (io.dmem.req.fire()) { state := s_wait }

    val acq = Reg(tl_in.a.bits)
    when (io.dmem.resp.valid) { acq.data := io.dmem.resp.bits.data }
    when (tl_in.a.fire()) { acq := tl_in.a.bits }

    val isWrite = edge.hasData(acq)
    val isRead = !isWrite

    def formCacheReq(acq: TLBundleA) = {
      val req = Wire(new HellaCacheReq)
      // treat all loads as full words, so bytes appear in correct lane
      req.typ := Mux(isRead, log2Ceil(beatBytes), acq.size)
      req.cmd := Mux(isRead, M_XRD, M_XWR)
      req.addr := Mux(isRead, ~(~acq.address | (beatBytes-1)), acq.address)
      req.tag := UInt(0)
      req
    }

    val ready = state === s_ready || tl_in.d.fire()
    io.dmem.req.valid := (tl_in.a.valid && ready) || state === s_replay
    tl_in.a.ready := io.dmem.req.ready && ready
    io.dmem.req.bits := formCacheReq(Mux(state === s_replay, acq, tl_in.a.bits))
    // this blows.  the TL data is already in the correct byte lane, but the D$
    // expects right-justified store data, so that it can steer the bytes.
    io.dmem.s1_data := new LoadGen(acq.size, Bool(false), acq.address(log2Ceil(beatBytes)-1,0), acq.data, Bool(false), beatBytes).data
    io.dmem.s1_kill := false
    io.dmem.invalidate_lr := false

    // place AMO data in correct word lane
    val minAMOBytes = 4
    val grantData = Mux(io.dmem.resp.valid, io.dmem.resp.bits.data, acq.data)
    val alignedGrantData = Mux(acq.size <= log2Ceil(minAMOBytes), Fill(coreDataBytes/minAMOBytes, grantData(8*minAMOBytes-1, 0)), grantData)

    tl_in.d.valid := io.dmem.resp.valid || state === s_grant
    tl_in.d.bits := Mux(isRead,
      edge.AccessAck(acq, UInt(0), alignedGrantData),
      edge.AccessAck(acq, UInt(0)))
  }
}
