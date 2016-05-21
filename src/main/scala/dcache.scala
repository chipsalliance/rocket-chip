// See LICENSE for license details.

package rocket

import Chisel._
import uncore._
import junctions._
import cde.{Parameters, Field}
import Util._

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
    val valid = io.req.valid && io.req.bits.way_en(w)
    when (valid && io.req.bits.write) {
      val data = Vec.tabulate(rowBytes)(i => io.req.bits.wdata(8*(i+1)-1, 8*i))
      array.write(addr, data, io.req.bits.wmask.toBools)
    }
    io.resp(w) := array.read(addr, valid && !io.req.bits.write).toBits
  }
}

class DCache(implicit p: Parameters) extends L1HellaCacheModule()(p) {
  val io = new Bundle {
    val cpu = (new HellaCacheIO).flip
    val ptw = new TLBPTWIO()
    val mem = new ClientTileLinkIO
  }

  val fq = Module(new FinishQueue(1))

  require(nWays == 1) // TODO associativity
  require(rowBits == encRowBits) // no ECC
  require(refillCyclesPerBeat == 1)
  require(rowBits >= coreDataBits)

  // tags
  val replacer = p(Replacer)()
  def onReset = L1Metadata(UInt(0), ClientMetadata.onReset)
  val meta = Module(new MetadataArray(onReset _))
  val metaReadArb = Module(new Arbiter(new MetaReadReq, 2))
  val metaWriteArb = Module(new Arbiter(new L1MetaWriteReq, 3))
  meta.io.read <> metaReadArb.io.out
  meta.io.write <> metaWriteArb.io.out

  // data
  val data = Module(new DCacheDataArray)
  val dataArb = Module(new Arbiter(new DCacheDataReq, 4))
  data.io.req <> dataArb.io.out
  dataArb.io.out.ready := true

  val s1_valid = Reg(next=io.cpu.req.fire(), init=Bool(false))
  val s1_probe = Reg(next=io.mem.probe.fire(), init=Bool(false))
  val probe_bits = RegEnable(io.mem.probe.bits, io.mem.probe.fire())
  val s1_nack = Wire(init=Bool(false))
  val s1_valid_masked = s1_valid && !io.cpu.s1_kill
  val s1_valid_not_nacked = s1_valid_masked && !s1_nack
  val s1_req = RegEnable(io.cpu.req.bits, io.cpu.req.valid)
  val s1_read = isRead(s1_req.cmd)
  val s1_write = isWrite(s1_req.cmd)

  val s_ready :: s_grant_wait :: s_voluntary_writeback :: s_probe_rep_dirty :: s_probe_rep_clean :: s_probe_rep_miss :: s_voluntary_write_meta :: s_probe_write_meta :: Nil = Enum(UInt(), 8)
  val grant_wait = Reg(init=Bool(false))
  val release_state = Reg(init=s_ready)
  val pstore_valid = Reg(init=Bool(false))
  val inWriteback = release_state === s_voluntary_writeback || release_state === s_probe_rep_dirty
  io.cpu.req.ready := (release_state === s_ready) && !grant_wait && !s1_nack

  // hit initiation path
  dataArb.io.in(3).valid := io.cpu.req.valid && isRead(io.cpu.req.bits.cmd)
  dataArb.io.in(3).bits.write := false
  dataArb.io.in(3).bits.addr := io.cpu.req.bits.addr
  dataArb.io.in(3).bits.way_en := ~UInt(0, nWays)
  when (!dataArb.io.in(3).ready && isRead(io.cpu.req.bits.cmd)) { io.cpu.req.ready := false }
  metaReadArb.io.in(1).valid := io.cpu.req.valid
  metaReadArb.io.in(1).bits.idx := io.cpu.req.bits.addr(idxMSB, idxLSB)
  metaReadArb.io.in(1).bits.way_en := ~UInt(0, nWays)
  when (!metaReadArb.io.in(1).ready) { io.cpu.req.ready := false }

  // address translation
  val tlb = Module(new TLB)
  io.ptw <> tlb.io.ptw
  tlb.io.req.valid := s1_valid_masked
  tlb.io.req.bits.passthrough := s1_req.phys
  tlb.io.req.bits.asid := 0
  tlb.io.req.bits.vpn := s1_req.addr >> pgIdxBits
  tlb.io.req.bits.instruction := false
  tlb.io.req.bits.store := s1_write
  when (!tlb.io.req.ready && !io.cpu.req.bits.phys) { io.cpu.req.ready := false }
  when (s1_valid && tlb.io.resp.miss) { s1_nack := true }

  val s1_paddr = Cat(tlb.io.resp.ppn, s1_req.addr(pgIdxBits-1,0))
  val s1_tag = Mux(s1_probe || inWriteback, probe_bits.addr_block >> idxBits, s1_paddr(paddrBits-1, untagBits))
  val s1_hit_way = meta.io.resp.map(r => r.coh.isValid() && r.tag === s1_tag)
  val s1_hit_state = Mux1H(s1_hit_way, meta.io.resp.map(_.coh))
  val s1_data = Mux1H(s1_hit_way, data.io.resp) // retime into s2 if critical

  val s2_valid = Reg(next=s1_valid_masked, init=Bool(false))
  val s2_probe = Reg(next=s1_probe, init=Bool(false))
  val releaseInFlight = s1_probe || s2_probe || release_state =/= s_ready
  val s2_valid_masked = s2_valid && Reg(next = !s1_nack)
  val s2_req = Reg(io.cpu.req.bits)
  when (s1_valid_not_nacked) {
    s2_req := s1_req
    s2_req.addr := s1_paddr
  }
  val s2_data = RegEnable(s1_data, s1_valid || inWriteback)
  val s2_hit_way = RegEnable(Cat(s1_hit_way.reverse), s1_valid_not_nacked || s1_probe)
  val s2_hit_state = RegEnable(s1_hit_state, s1_valid_not_nacked || s1_probe)
  val s2_hit = s2_hit_way.orR && s2_hit_state.isHit(s2_req.cmd)
  val s2_hit_dirty = s2_hit && s2_hit_state.requiresVoluntaryWriteback()
  val s2_valid_hit = s2_valid_masked && s2_hit
  val s2_valid_miss = s2_valid_masked && !s2_hit && !pstore_valid
  val s2_repl = RegEnable(meta.io.resp(replacer.way), s1_valid_not_nacked)
  val s2_repl_dirty = s2_repl.coh.requiresVoluntaryWriteback()
  io.cpu.s2_nack := s2_valid && !s2_valid_hit
  when (io.cpu.s2_nack) { s1_nack := true }

  // exceptions
  val misaligned = new StoreGen(s1_req.typ, s1_req.addr, UInt(0), wordBytes).misaligned
  io.cpu.xcpt.ma.ld := s1_read && misaligned
  io.cpu.xcpt.ma.st := s1_write && misaligned
  io.cpu.xcpt.pf.ld := s1_read && tlb.io.resp.xcpt_ld
  io.cpu.xcpt.pf.st := s1_write && tlb.io.resp.xcpt_st
  assert(!(Reg(next=
    (io.cpu.xcpt.ma.ld || io.cpu.xcpt.ma.st || io.cpu.xcpt.pf.ld || io.cpu.xcpt.pf.st)) &&
    io.cpu.resp.valid),
      "DCache exception occurred - cache response not killed.")

  // committed stores
  val s2_store_valid = s2_valid_hit && isWrite(s2_req.cmd)
  val s2_store_data = RegEnable(io.cpu.s1_data, s1_valid && s1_write)
  val s2_storegen = new StoreGen(s2_req.typ, s2_req.addr, s2_store_data, wordBytes)
  val s2_storegen_data = Wire(init = s2_storegen.data)
  val pstore_drain = s2_store_valid || releaseInFlight || io.cpu.s2_nack || !(io.cpu.req.valid && isRead(io.cpu.req.bits.cmd))
  pstore_valid := s2_store_valid || (pstore_valid && !pstore_drain)
  val pstore_addr = RegEnable(s2_req.addr, s2_store_valid)
  val pstore_way = RegEnable(s2_hit_way, s2_store_valid)
  val pstore_data = RegEnable(s2_storegen_data, s2_store_valid)
  val pstore_mask = RegEnable(s2_storegen.mask, s2_store_valid)
  dataArb.io.in(0).valid := pstore_valid && pstore_drain
  dataArb.io.in(0).bits.write := true
  dataArb.io.in(0).bits.addr := pstore_addr
  dataArb.io.in(0).bits.way_en := pstore_way
  dataArb.io.in(0).bits.wdata := Fill(rowWords, pstore_data)
  dataArb.io.in(0).bits.wmask := pstore_mask << (if (rowOffBits > offsetlsb) (pstore_addr(rowOffBits-1,offsetlsb) << wordOffBits) else UInt(0))

  // store->load RAW hazard detection
  val s1_idx = s1_req.addr(idxMSB, wordOffBits)
  val s1_raw_hazard = s1_read &&
    ((s2_store_valid && s2_req.addr(idxMSB, wordOffBits) === s1_idx) ||
     (pstore_valid && pstore_addr(idxMSB, wordOffBits) === s1_idx))
  when (s1_valid && s1_raw_hazard) { s1_nack := true }

  val s2_new_hit_state = s2_hit_state.onHit(s2_req.cmd)
  metaWriteArb.io.in(0).valid := (s2_valid_hit && s2_hit_state =/= s2_new_hit_state) || (s2_valid_miss && !s2_repl_dirty)
  metaWriteArb.io.in(0).bits.way_en := Mux(s2_hit, s2_hit_way, UIntToOH(replacer.way))
  metaWriteArb.io.in(0).bits.idx := s2_req.addr(idxMSB, idxLSB)
  metaWriteArb.io.in(0).bits.data.coh := Mux(s2_hit, s2_new_hit_state, ClientMetadata.onReset)
  metaWriteArb.io.in(0).bits.data.tag := s2_req.addr(paddrBits-1, untagBits)

  // acquire
  io.mem.acquire.valid := s2_valid_miss && !s2_repl_dirty && fq.io.enq.ready
  io.mem.acquire.bits := s2_hit_state.makeAcquire(addr_block = s2_req.addr(paddrBits-1, blockOffBits), client_xact_id = UInt(0), op_code = s2_req.cmd)
  when (io.mem.acquire.fire()) { grant_wait := true }

  // grant
  val grantIsRefill = io.mem.grant.bits.hasMultibeatData()
  val grantHasData = io.mem.grant.bits.hasData()
  val grantIsUncached = grantHasData && !grantIsRefill
  when (io.mem.grant.valid) {
    assert(grantIsRefill === io.mem.grant.bits.requiresAck(), "")
    assert(!grantIsUncached, "TODO uncached")
  }
  val (refillCount, refillDone) = Counter(io.mem.grant.fire() && grantIsRefill, refillCycles)
  val grantDone = refillDone || grantIsUncached
  when (io.mem.grant.fire() && grantDone) { grant_wait := false }

  // data refill
  dataArb.io.in(1).valid := grantIsRefill && io.mem.grant.valid
  io.mem.grant.ready := true
  assert(dataArb.io.in(1).ready || !dataArb.io.in(1).valid, "")
  dataArb.io.in(1).bits.write := true
  dataArb.io.in(1).bits.addr := Cat(s2_req.addr(paddrBits-1, blockOffBits), io.mem.grant.bits.addr_beat) << beatOffBits
  dataArb.io.in(1).bits.way_en := UIntToOH(replacer.way)
  dataArb.io.in(1).bits.wdata := io.mem.grant.bits.data
  dataArb.io.in(1).bits.wmask := ~UInt(0, rowBytes)
  // tag updates on refill
  metaWriteArb.io.in(1).valid := refillDone
  assert(!metaWriteArb.io.in(1).valid || metaWriteArb.io.in(1).ready, "")
  metaWriteArb.io.in(1).bits.way_en := UIntToOH(replacer.way)
  metaWriteArb.io.in(1).bits.idx := s2_req.addr(idxMSB, idxLSB)
  metaWriteArb.io.in(1).bits.data.coh := s2_hit_state.onGrant(io.mem.grant.bits, s2_req.cmd)
  metaWriteArb.io.in(1).bits.data.tag := s2_req.addr(paddrBits-1, untagBits)

  // probe
  metaReadArb.io.in(0).valid := io.mem.probe.valid
  io.mem.probe.ready := metaReadArb.io.in(0).ready && !releaseInFlight && !s1_valid && (!s2_valid || s2_valid_hit)
  metaReadArb.io.in(0).bits.idx := io.mem.probe.bits.addr_block
  metaReadArb.io.in(0).bits.way_en := ~UInt(0, nWays)

  // finish
  fq.io.enq.valid := refillDone
  fq.io.enq.bits := io.mem.grant.bits.makeFinish()
  io.mem.finish <> fq.io.deq
  when (fq.io.enq.valid) {
    assert(fq.io.enq.ready, "")
    replacer.miss
  }

  // release
  val (writebackCount, writebackDone) = Counter(io.mem.release.fire() && inWriteback, refillCycles)
  val releaseDone = writebackDone || (io.mem.release.fire() && !inWriteback)
  val new_coh = Wire(init = s2_hit_state.onProbe(probe_bits))
  val release_way = Wire(init = s2_hit_way)
  val releaseRejected = io.mem.release.valid && !io.mem.release.ready
  val s1_release_data_valid = Reg(next = dataArb.io.in(2).fire())
  val s2_release_data_valid = Reg(next = s1_release_data_valid && !releaseRejected)
  val releaseDataBeat = Cat(UInt(0), writebackCount) + Mux(releaseRejected, UInt(0), s1_release_data_valid + Cat(UInt(0), s2_release_data_valid))
  io.mem.release.valid := s2_release_data_valid
  io.mem.release.bits := ClientMetadata.onReset.makeRelease(probe_bits)
  when (s2_valid_miss && s2_repl_dirty) {
    release_state := s_voluntary_writeback
    probe_bits.addr_block := Cat(s2_repl.tag, s2_req.addr(idxMSB, idxLSB))
  }
  when (s2_probe) {
    when (s2_hit_dirty) { release_state := s_probe_rep_dirty }
    .elsewhen (s2_hit) { release_state := s_probe_rep_clean }
    .otherwise {
      io.mem.release.valid := true
      release_state := s_probe_rep_miss
    }
  }
  when (releaseDone) { release_state := s_ready }
  when (release_state === s_probe_rep_miss || release_state === s_probe_rep_clean) {
    io.mem.release.valid := true
  }
  when (release_state === s_probe_rep_clean || release_state === s_probe_rep_dirty) {
    io.mem.release.bits := s2_hit_state.makeRelease(probe_bits)
    when (releaseDone) { release_state := s_probe_write_meta }
  }
  when (release_state === s_voluntary_writeback || release_state === s_voluntary_write_meta) {
    io.mem.release.bits := s2_hit_state.makeVoluntaryWriteback(UInt(0), UInt(0))
    new_coh := s2_hit_state.onCacheControl(M_FLUSH)
    release_way := UIntToOH(replacer.way)
    when (releaseDone) { release_state := s_voluntary_write_meta }
  }
  when (s2_probe && !io.mem.release.fire()) { s1_nack := true }
  io.mem.release.bits.addr_block := probe_bits.addr_block
  io.mem.release.bits.addr_beat := writebackCount
  io.mem.release.bits.data := s2_data

  dataArb.io.in(2).valid := inWriteback && releaseDataBeat < refillCycles
  dataArb.io.in(2).bits.write := false
  dataArb.io.in(2).bits.addr := Cat(io.mem.release.bits.addr_block, releaseDataBeat(log2Up(refillCycles)-1,0)) << rowOffBits
  dataArb.io.in(2).bits.way_en := ~UInt(0, nWays)

  metaWriteArb.io.in(2).valid := (release_state === s_voluntary_write_meta || release_state === s_probe_write_meta)
  metaWriteArb.io.in(2).bits.way_en := release_way
  metaWriteArb.io.in(2).bits.idx := io.mem.release.bits.full_addr()(idxMSB, idxLSB)
  metaWriteArb.io.in(2).bits.data.coh := new_coh
  metaWriteArb.io.in(2).bits.data.tag := io.mem.release.bits.full_addr()(paddrBits-1, untagBits)
  when (metaWriteArb.io.in(2).fire()) { release_state := s_ready }

  // response
  io.cpu.replay_next := io.mem.grant.valid && grantIsUncached
  io.cpu.resp.valid := s2_valid_hit || io.cpu.resp.bits.replay
  io.cpu.resp.bits := s2_req // TODO uncached
  io.cpu.resp.bits.has_data := isRead(s2_req.cmd) // TODO uncached
  io.cpu.resp.bits.replay := Reg(next = io.cpu.replay_next)
  io.cpu.ordered := !(s1_valid || s2_valid || grant_wait)

  // load data subword mux/sign extension
  val s2_sc = Bool(false)
  val s2_word_idx = s2_req.addr(log2Up(rowWords*coreDataBytes)-1, log2Up(wordBytes))
  val s2_data_word = s2_data >> Cat(s2_word_idx, UInt(0, log2Up(coreDataBits)))
  val loadgen = new LoadGen(s2_req.typ, s2_req.addr, s2_data_word, s2_sc, wordBytes)
  io.cpu.resp.bits.data := loadgen.data
  io.cpu.resp.bits.data_word_bypass := loadgen.wordData
  io.cpu.resp.bits.store_data := s2_store_data

  // AMOs
  if (usingAtomics) {
    val amoalu = Module(new AMOALU)
    amoalu.io.addr := s2_req.addr
    amoalu.io.cmd := s2_req.cmd
    amoalu.io.typ := s2_req.typ
    amoalu.io.lhs := s2_data_word
    amoalu.io.rhs := s2_store_data
    s2_storegen_data := amoalu.io.out
  }
}
