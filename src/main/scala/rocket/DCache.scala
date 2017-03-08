// See LICENSE.SiFive for license details.

package rocket

import Chisel._
import Chisel.ImplicitConversions._
import config._
import diplomacy._
import uncore.constants._
import uncore.tilelink2._
import uncore.util._
import util._
import TLMessages._

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

class DCache(val scratch: () => Option[AddressSet] = () => None)(implicit p: Parameters) extends HellaCache()(p) {
  override lazy val module = new DCacheModule(this) 
}

class DCacheModule(outer: DCache) extends HellaCacheModule(outer) {
  require(rowBits == encRowBits) // no ECC

  val grantackq = Module(new Queue(tl_out.e.bits,1)) // TODO don't need this in scratchpad mode

  // tags
  val replacer = cacheParams.replacement
  def onReset = L1Metadata(UInt(0), ClientMetadata.onReset)
  val metaReadArb = Module(new Arbiter(new L1MetaReadReq, 3))
  val metaWriteArb = Module(new Arbiter(new L1MetaWriteReq, 3))

  // data
  val data = Module(new DCacheDataArray)
  val dataArb = Module(new Arbiter(new DCacheDataReq, 4))
  data.io.req <> dataArb.io.out
  dataArb.io.out.ready := true

  val s1_valid = Reg(next=io.cpu.req.fire(), init=Bool(false))
  val s1_probe = Reg(next=tl_out.b.fire(), init=Bool(false))
  val probe_bits = RegEnable(tl_out.b.bits, tl_out.b.fire()) // TODO has data now :(
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
  val tlb = Module(new TLB(nTLBEntries))
  io.ptw <> tlb.io.ptw
  tlb.io.req.valid := s1_valid_masked && s1_readwrite
  tlb.io.req.bits.passthrough := s1_req.phys
  tlb.io.req.bits.vpn := s1_req.addr >> pgIdxBits
  tlb.io.req.bits.instruction := false
  tlb.io.req.bits.store := s1_write
  when (!tlb.io.req.ready && !io.cpu.req.bits.phys) { io.cpu.req.ready := false }
  when (s1_valid && s1_readwrite && tlb.io.resp.miss) { s1_nack := true }

  val s1_paddr = Cat(tlb.io.resp.ppn, s1_req.addr(pgIdxBits-1,0))
  val s1_tag = Mux(s1_probe, probe_bits.address, s1_paddr)(paddrBits-1, untagBits)
  val s1_victim_way = Wire(init = replacer.way)
  val (s1_hit_way, s1_hit_state, s1_victim_meta) =
    if (usingDataScratchpad) {
      metaWriteArb.io.out.ready := true
      metaReadArb.io.out.ready := !metaWriteArb.io.out.valid
      val inScratchpad = outer.scratch().map(_.contains(s1_paddr)).getOrElse(Bool(false))
      val hitState = Mux(inScratchpad, ClientMetadata.maximum, ClientMetadata.onReset)
      (inScratchpad, hitState, L1Metadata(UInt(0), ClientMetadata.onReset))
    } else {
      val meta = Module(new L1MetadataArray(onReset _))
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
  val s2_req_block_addr = (s2_req.addr >> idxLSB) << idxLSB
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
  val s2_hit_valid = s2_hit_state.isValid()
  val (s2_hit, s2_grow_param, s2_new_hit_state) = s2_hit_state.onAccess(s2_req.cmd)
  val s2_valid_hit = s2_valid_masked && s2_readwrite && s2_hit
  val s2_valid_miss = s2_valid_masked && s2_readwrite && !s2_hit && !(pstore1_valid || pstore2_valid) && !release_ack_wait
  val s2_valid_cached_miss = s2_valid_miss && !s2_uncached
  val s2_victimize = s2_valid_cached_miss || s2_flush_valid
  val s2_valid_uncached = s2_valid_miss && s2_uncached
  val s2_victim_way = Mux(s2_hit_valid && !s2_flush_valid, s2_hit_way, UIntToOH(RegEnable(s1_victim_way, s1_valid_not_nacked || s1_flush_valid)))
  val s2_victim_tag = RegEnable(s1_victim_meta.tag, s1_valid_not_nacked || s1_flush_valid)
  val s2_victim_state = Mux(s2_hit_valid && !s2_flush_valid, s2_hit_state, RegEnable(s1_victim_meta.coh, s1_valid_not_nacked || s1_flush_valid))
  val s2_victim_valid = s2_victim_state.isValid()
  val (s2_prb_ack_data, s2_report_param, probeNewCoh)= s2_probe_state.onProbe(probe_bits.param)
  val (s2_victim_dirty, s2_shrink_param, voluntaryNewCoh) = s2_victim_state.onCacheControl(M_FLUSH)
  val s2_update_meta = s2_hit_state =/= s2_new_hit_state
  io.cpu.s2_nack := s2_valid && !s2_valid_hit && !(s2_valid_uncached && tl_out.a.ready && !uncachedInFlight.asUInt.andR)
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

  // Prepare a TileLink request message that initiates a transaction
  val a_source = PriorityEncoder(~uncachedInFlight.asUInt)
  val acquire_address = s2_req_block_addr
  val access_address = s2_req.addr
  val a_size = s2_req.typ(MT_SZ-2, 0)
  val a_data = Fill(beatWords, pstore1_storegen.data)
  val acquire = if (edge.manager.anySupportAcquireB) {
    edge.Acquire(a_source, acquire_address, lgCacheBlockBytes, s2_grow_param)._2 // Cacheability checked by tlb
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
    assert (!(tl_out.a.valid && pstore1_amo && s2_write && s2_uncached))
    Wire(new TLBundleA(edge.bundle))
  }

  tl_out.a.valid := grantackq.io.enq.ready && ((s2_valid_cached_miss && !s2_victim_dirty) ||
                                        (s2_valid_uncached && !uncachedInFlight.asUInt.andR))
  tl_out.a.bits := Mux(!s2_uncached, acquire, Mux(!s2_write, get, Mux(!pstore1_amo, put, atomics)))

  // Set pending bits for outstanding TileLink transaction
  when (tl_out.a.fire()) {
    when (s2_uncached) {
      uncachedInFlight(a_source) := true
      uncachedReqs(a_source) := s2_req
    }.otherwise {
      cached_grant_wait := true
    }
  }

  // grant
  val (d_first, d_last, d_done, d_address_inc) = edge.addr_inc(tl_out.d)
  val grantIsCached = tl_out.d.bits.opcode.isOneOf(Grant, GrantData)
  val grantIsUncached = tl_out.d.bits.opcode.isOneOf(AccessAck, AccessAckData, HintAck)
  val grantIsVoluntary = tl_out.d.bits.opcode === ReleaseAck // Clears a different pending bit
  val grantIsRefill = tl_out.d.bits.opcode === GrantData     // Writes the data array
  tl_out.d.ready := true
  when (tl_out.d.fire()) {
    when (grantIsCached) {
      assert(cached_grant_wait, "A GrantData was unexpected by the dcache.")
      when(d_last) { cached_grant_wait := false }
    } .elsewhen (grantIsUncached) {
      val id = tl_out.d.bits.source
      val req = uncachedReqs(id)
      assert(uncachedInFlight(id), "An AccessAck was unexpected by the dcache.") // TODO must handle Ack coming back on same cycle!
      when(d_last) { uncachedInFlight(id) := false }
      s2_data := tl_out.d.bits.data
      s2_req.cmd := req.cmd
      s2_req.typ := req.typ
      s2_req.tag := req.tag
      s2_req.addr := Cat(s1_paddr >> beatOffBits /* don't-care */, req.addr(beatOffBits-1, 0))
    } .elsewhen (grantIsVoluntary) {
      assert(release_ack_wait, "A ReleaseAck was unexpected by the dcache.") // TODO should handle Ack coming back on same cycle!
      release_ack_wait := false
    }
  }

  // data refill
  val doRefillBeat = grantIsRefill && tl_out.d.valid
  dataArb.io.in(1).valid := doRefillBeat
  assert(dataArb.io.in(1).ready || !doRefillBeat)
  dataArb.io.in(1).bits.write := true
  dataArb.io.in(1).bits.addr :=  s2_req_block_addr | d_address_inc
  dataArb.io.in(1).bits.way_en := s2_victim_way
  dataArb.io.in(1).bits.wdata := tl_out.d.bits.data
  dataArb.io.in(1).bits.wmask := ~UInt(0, rowBytes)
  // tag updates on refill
  metaWriteArb.io.in(1).valid := grantIsCached && d_done
  assert(!metaWriteArb.io.in(1).valid || metaWriteArb.io.in(1).ready)
  metaWriteArb.io.in(1).bits.way_en := s2_victim_way
  metaWriteArb.io.in(1).bits.idx := s2_req.addr(idxMSB, idxLSB)
  metaWriteArb.io.in(1).bits.data.coh := s2_hit_state.onGrant(s2_req.cmd, tl_out.d.bits.param)
  metaWriteArb.io.in(1).bits.data.tag := s2_req.addr(paddrBits-1, untagBits)
  // don't accept uncached grants if there's a structural hazard on s2_data...
  val blockUncachedGrant = Reg(Bool())
  blockUncachedGrant := dataArb.io.out.valid
  when (grantIsUncached) {
    tl_out.d.ready := !(blockUncachedGrant || s1_valid)
    // ...but insert bubble to guarantee grant's eventual forward progress
    when (tl_out.d.valid && !tl_out.d.ready) {
      io.cpu.req.ready := false
      dataArb.io.in(1).valid := true
      dataArb.io.in(1).bits.write := false
      blockUncachedGrant := !dataArb.io.in(1).ready
    }
  }

  // Finish TileLink transaction by issuing a GrantAck
  grantackq.io.enq.valid := d_done && edge.hasFollowUp(tl_out.d.bits)
  grantackq.io.enq.bits := edge.GrantAck(tl_out.d.bits)
  tl_out.e <> grantackq.io.deq
  assert(!grantackq.io.enq.valid || grantackq.io.enq.ready, "Too many Grants received by dcache.")
  when (d_done) { replacer.miss }

  // Handle an incoming TileLink Probe message
  val block_probe = releaseInFlight || lrscValid || (s2_valid_hit && s2_lr)
  metaReadArb.io.in(1).valid := tl_out.b.valid && !block_probe
  tl_out.b.ready := metaReadArb.io.in(1).ready && !block_probe && !s1_valid && (!s2_valid || s2_valid_hit)
  metaReadArb.io.in(1).bits.idx := tl_out.b.bits.address(idxMSB, idxLSB)
  metaReadArb.io.in(1).bits.way_en := ~UInt(0, nWays)

  // release
  val (_, c_last, releaseDone, c_count) = edge.count(tl_out.c)
  val releaseRejected = tl_out.c.valid && !tl_out.c.ready
  val s1_release_data_valid = Reg(next = dataArb.io.in(2).fire())
  val s2_release_data_valid = Reg(next = s1_release_data_valid && !releaseRejected)
  val releaseDataBeat = Cat(UInt(0), c_count) + Mux(releaseRejected, UInt(0), s1_release_data_valid + Cat(UInt(0), s2_release_data_valid))

  val nackResponseMessage     = edge.ProbeAck(
                                  b = probe_bits,
                                  reportPermissions = TLPermissions.NtoN)

  val voluntaryReleaseMessage = if (edge.manager.anySupportAcquireB) {
                                edge.Release(
                                  fromSource = UInt(maxUncachedInFlight - 1),
                                  toAddress = probe_bits.address,
                                  lgSize = lgCacheBlockBytes,
                                  shrinkPermissions = s2_shrink_param,
                                  data = s2_data)._2
  } else {
    Wire(new TLBundleC(edge.bundle))
  }

  val probeResponseMessage = Mux(!s2_prb_ack_data,
                                edge.ProbeAck(
                                  b = probe_bits,
                                  reportPermissions = s2_report_param),
                                edge.ProbeAck(
                                  b = probe_bits,
                                  reportPermissions = s2_report_param,
                                  data = s2_data))

  tl_out.c.valid := s2_release_data_valid
  tl_out.c.bits := nackResponseMessage
  val newCoh = Wire(init = probeNewCoh)
  releaseWay := s2_probe_way

  when (s2_victimize && s2_victim_dirty) {
    assert(!(s2_valid && s2_hit_valid))
    release_state := s_voluntary_writeback
    probe_bits.address := Cat(s2_victim_tag, s2_req.addr(idxMSB, idxLSB)) << idxLSB
  }
  when (s2_probe) {
    when (s2_prb_ack_data) { release_state := s_probe_rep_dirty }
    .elsewhen (s2_probe_state.isValid()) { release_state := s_probe_rep_clean }
    .otherwise {
      tl_out.c.valid := true
      release_state := s_probe_rep_miss
    }
  }
  when (releaseDone) { release_state := s_ready }
  when (release_state.isOneOf(s_probe_rep_miss, s_probe_rep_clean)) {
    tl_out.c.valid := true
  }
  when (release_state.isOneOf(s_probe_rep_clean, s_probe_rep_dirty)) {
    tl_out.c.bits := probeResponseMessage
    when (releaseDone) { release_state := s_probe_write_meta }
  }
  when (release_state.isOneOf(s_voluntary_writeback, s_voluntary_write_meta)) {
    tl_out.c.bits := voluntaryReleaseMessage
    newCoh := voluntaryNewCoh
    releaseWay := s2_victim_way
    when (releaseDone) {
      release_state := s_voluntary_write_meta
      release_ack_wait := true
    }
  }
  when (s2_probe && !tl_out.c.fire()) { s1_nack := true }
  tl_out.c.bits.address := probe_bits.address
  tl_out.c.bits.data := s2_data

  dataArb.io.in(2).valid := inWriteback && releaseDataBeat < refillCycles
  dataArb.io.in(2).bits.write := false
  dataArb.io.in(2).bits.addr := tl_out.c.bits.address | (releaseDataBeat(log2Up(refillCycles)-1,0) << rowOffBits)
  dataArb.io.in(2).bits.way_en := ~UInt(0, nWays)

  metaWriteArb.io.in(2).valid := release_state.isOneOf(s_voluntary_write_meta, s_probe_write_meta)
  metaWriteArb.io.in(2).bits.way_en := releaseWay
  metaWriteArb.io.in(2).bits.idx := tl_out.c.bits.address(idxMSB, idxLSB)
  metaWriteArb.io.in(2).bits.data.coh := newCoh
  metaWriteArb.io.in(2).bits.data.tag := tl_out.c.bits.address(paddrBits-1, untagBits)
  when (metaWriteArb.io.in(2).fire()) { release_state := s_ready }

  // cached response
  io.cpu.resp.valid := s2_valid_hit
  io.cpu.resp.bits <> s2_req
  io.cpu.resp.bits.has_data := s2_read
  io.cpu.resp.bits.replay := false
  io.cpu.ordered := !(s1_valid || s2_valid || cached_grant_wait || uncachedInFlight.asUInt.orR)

  // uncached response
  io.cpu.replay_next := tl_out.d.fire() && grantIsUncached
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
  when (tl_out.a.fire() && !s2_uncached) { flushed := false }
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
