// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket

import Chisel._
import Chisel.ImplicitConversions._
import chisel3.withClock
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.experimental.chiselName
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.subsystem.CacheBlockBytes
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property
import freechips.rocketchip.diplomaticobjectmodel.model.OMSRAM
import scala.collection.mutable.ListBuffer

class PTWReq(implicit p: Parameters) extends CoreBundle()(p) {
  val addr = UInt(width = vpnBits)
  val need_gpa = Bool()
  val vstage1 = Bool()
  val stage2 = Bool()
}

class PTWResp(implicit p: Parameters) extends CoreBundle()(p) {
  val ae_ptw = Bool()
  val ae_final = Bool()
  val gf = Bool()
  val hr = Bool()
  val hw = Bool()
  val hx = Bool()
  val pte = new PTE
  val level = UInt(width = log2Ceil(pgLevels))
  val fragmented_superpage = Bool()
  val homogeneous = Bool()
  val gpa = Valid(UInt(vaddrBits.W))
}

class TLBPTWIO(implicit p: Parameters) extends CoreBundle()(p)
    with HasCoreParameters {
  val req = Decoupled(Valid(new PTWReq))
  val resp = Valid(new PTWResp).flip
  val ptbr = new PTBR().asInput
  val hgatp = new PTBR().asInput
  val vsatp = new PTBR().asInput
  val status = new MStatus().asInput
  val hstatus = new HStatus().asInput
  val gstatus = new MStatus().asInput
  val pmp = Vec(nPMPs, new PMP).asInput
  val customCSRs = coreParams.customCSRs.asInput
}

class PTWPerfEvents extends Bundle {
  val l2miss = Bool()
  val l2hit = Bool()
  val pte_miss = Bool()
  val pte_hit = Bool()
}

class DatapathPTWIO(implicit p: Parameters) extends CoreBundle()(p)
    with HasCoreParameters {
  val ptbr = new PTBR().asInput
  val hgatp = new PTBR().asInput
  val vsatp = new PTBR().asInput
  val sfence = Valid(new SFenceReq).flip
  val status = new MStatus().asInput
  val hstatus = new HStatus().asInput
  val gstatus = new MStatus().asInput
  val pmp = Vec(nPMPs, new PMP).asInput
  val perf = new PTWPerfEvents().asOutput
  val customCSRs = coreParams.customCSRs.asInput
  val clock_enabled = Bool(OUTPUT)
}

class PTE(implicit p: Parameters) extends CoreBundle()(p) {
  val ppn = UInt(width = 54)
  val reserved_for_software = Bits(width = 2)
  val d = Bool()
  val a = Bool()
  val g = Bool()
  val u = Bool()
  val x = Bool()
  val w = Bool()
  val r = Bool()
  val v = Bool()

  def table(dummy: Int = 0) = v && !r && !w && !x && !d && !a && !u
  def leaf(dummy: Int = 0) = v && (r || (x && !w)) && a
  def ur(dummy: Int = 0) = sr() && u
  def uw(dummy: Int = 0) = sw() && u
  def ux(dummy: Int = 0) = sx() && u
  def sr(dummy: Int = 0) = leaf() && r
  def sw(dummy: Int = 0) = leaf() && w && d
  def sx(dummy: Int = 0) = leaf() && x
  def isFullPerm(dummy: Int = 0) = uw() && ux()
}

class L2TLBEntry(nSets: Int)(implicit p: Parameters) extends CoreBundle()(p)
    with HasCoreParameters {
  val idxBits = log2Ceil(nSets)
  val tagBits = maxSVAddrBits - pgIdxBits - idxBits + (if (usingHypervisor) 1 else 0)
  val tag = UInt(width = tagBits)
  val ppn = UInt(width = ppnBits)
  val d = Bool()
  val a = Bool()
  val u = Bool()
  val x = Bool()
  val w = Bool()
  val r = Bool()

}

@chiselName
class PTW(n: Int)(implicit edge: TLEdgeOut, p: Parameters) extends CoreModule()(p) {
  val io = new Bundle {
    val requestor = Vec(n, new TLBPTWIO).flip
    val mem = new HellaCacheIO
    val dpath = new DatapathPTWIO
  }

  val omSRAMs = collection.mutable.ListBuffer[OMSRAM]()

  val s_ready :: s_req :: s_wait1 :: s_dummy1 :: s_wait2 :: s_wait3 :: s_dummy2 :: s_fragment_superpage :: Nil = Enum(UInt(), 8)
  val state = Reg(init=s_ready)
  val l2_refill_wire = Wire(Bool())

  val arb = Module(new Arbiter(Valid(new PTWReq), n))
  arb.io.in <> io.requestor.map(_.req)
  arb.io.out.ready := (state === s_ready) && !l2_refill_wire

  val resp_valid = Reg(next = Vec.fill(io.requestor.size)(Bool(false)))

  val clock_en = state =/= s_ready || l2_refill_wire || arb.io.out.valid || io.dpath.sfence.valid || io.dpath.customCSRs.disableDCacheClockGate
  io.dpath.clock_enabled := usingVM && clock_en
  val gated_clock =
    if (!usingVM || !tileParams.dcache.get.clockGate) clock
    else ClockGate(clock, clock_en, "ptw_clock_gate")
  withClock (gated_clock) { // entering gated-clock domain

  val invalidated = Reg(Bool())
  val count = Reg(UInt(width = log2Ceil(pgLevels)))
  val resp_ae_ptw = Reg(Bool())
  val resp_ae_final = Reg(Bool())
  val resp_gf = Reg(Bool())
  val resp_hr = Reg(Bool())
  val resp_hw = Reg(Bool())
  val resp_hx = Reg(Bool())
  val resp_fragmented_superpage = Reg(Bool())

  val r_req = Reg(new PTWReq)
  val r_req_dest = Reg(Bits())
  val r_pte = Reg(new PTE)
  val r_hgatp = Reg(new PTBR)

  val aux_count = Reg(UInt(log2Ceil(pgLevels).W))
  val aux_pte = Reg(new PTE)
  val gpa_pgoff = Reg(UInt(pgIdxBits.W)) // only valid in resp_gf case
  val stage2 = Reg(Bool())
  val stage2_final = Reg(Bool())

  val satp = Mux(arb.io.out.bits.bits.vstage1, io.dpath.vsatp, io.dpath.ptbr)
  val r_hgatp_initial_count = pgLevels - minPgLevels - r_hgatp.additionalPgLevels
  val do_both_stages = r_req.vstage1 && r_req.stage2
  val max_count = count max aux_count
  val vpn = Mux(r_req.vstage1 && stage2, aux_pte.ppn, r_req.addr)

  val mem_resp_valid = RegNext(io.mem.resp.valid)
  val mem_resp_data = RegNext(io.mem.resp.bits.data)
  io.mem.uncached_resp.map { resp =>
    assert(!(resp.valid && io.mem.resp.valid))
    resp.ready := true
    when (resp.valid) {
      mem_resp_valid := true
      mem_resp_data := resp.bits.data
    }
  }

  val (pte, invalid_paddr) = {
    val tmp = new PTE().fromBits(mem_resp_data)
    val res = Wire(init = tmp)
    res.ppn := Mux(do_both_stages && !stage2, tmp.ppn(vpnBits-1, 0), tmp.ppn(ppnBits-1, 0))
    when (tmp.r || tmp.w || tmp.x) {
      // for superpage mappings, make sure PPN LSBs are zero
      for (i <- 0 until pgLevels-1)
        when (count <= i && tmp.ppn((pgLevels-1-i)*pgLevelBits-1, (pgLevels-2-i)*pgLevelBits) =/= 0) { res.v := false }
    }
    (res, Mux(do_both_stages && !stage2, (tmp.ppn >> vpnBits) =/= 0, (tmp.ppn >> ppnBits) =/= 0))
  }
  val traverse = pte.table() && !invalid_paddr && count < pgLevels-1
  val pte_addr = if (!usingVM) 0.U else {
    val vpn_idxs = (0 until pgLevels).map { i =>
      val width = pgLevelBits + (if (i <= pgLevels - minPgLevels) hypervisorExtraAddrBits else 0)
      (vpn >> (pgLevels - i - 1) * pgLevelBits)(width - 1, 0)
    }
    val mask     = Mux(stage2 && count === r_hgatp_initial_count, ((1 << (hypervisorExtraAddrBits + pgLevelBits)) - 1).U, ((1 << pgLevelBits) - 1).U)
    val vpn_idx  = vpn_idxs(count) & mask
    val size = if (usingHypervisor) vaddrBits else paddrBits
    (((r_pte.ppn << pgLevelBits) | vpn_idx) << log2Ceil(xLen / 8))(size - 1, 0)
  }
  val pte_cache_addr = if (!usingHypervisor) pte_addr else {
    val vpn_idxs = (0 until pgLevels-1).map(i => (aux_pte.ppn >> (pgLevels-i-1)*pgLevelBits)(pgLevelBits-1,0))
    val vpn_idx = vpn_idxs(count)
    (Cat(r_pte.ppn, vpn_idx) << log2Ceil(xLen/8))(vaddrBits-1, 0)
  }
  val stage2_pte_cache_addr = if (!usingHypervisor) 0.U else {
    val vpn_idxs = (0 until pgLevels - 1).map(i => (r_req.addr >> (pgLevels - i - 1) * pgLevelBits)(pgLevelBits - 1, 0))
    val vpn_idx  = vpn_idxs(aux_count)
    (Cat(aux_pte.ppn, vpn_idx) << log2Ceil(xLen / 8))(vaddrBits - 1, 0)
  }

  def makeFragmentedSuperpagePPN(ppn: UInt): Seq[UInt] = {
    (pgLevels-1 until 0 by -1).map(i => Cat(ppn >> (pgLevelBits*i), r_req.addr(((pgLevelBits*i) min vpnBits)-1, 0).padTo(pgLevelBits*i)))
  }

  def makePTECache(s2: Boolean): (Bool, UInt) = {
    val plru = new PseudoLRU(coreParams.nPTECacheEntries)
    val valid = RegInit(0.U(coreParams.nPTECacheEntries.W))
    val tags = Reg(Vec(coreParams.nPTECacheEntries, UInt((if (usingHypervisor) 1 + vaddrBits else paddrBits).W)))
    val data = Reg(Vec(coreParams.nPTECacheEntries, UInt((if (usingHypervisor && s2) vpnBits else ppnBits).W)))
    val can_hit =
      if (s2) count === r_hgatp_initial_count && aux_count < pgLevels-1 && r_req.vstage1 && stage2 && !stage2_final
      else count < pgLevels-1 && Mux(r_req.vstage1, stage2, !r_req.stage2)
    val can_refill =
      if (s2) do_both_stages && !stage2 && !stage2_final
      else can_hit
    val tag =
      if (s2) Cat(true.B, stage2_pte_cache_addr)
      else Cat(r_req.vstage1, pte_cache_addr)

    val hits = tags.map(_ === tag).asUInt & valid
    val hit = hits.orR && can_hit
    when (mem_resp_valid && traverse && can_refill && !hits.orR && !invalidated) {
      val r = Mux(valid.andR, plru.way, PriorityEncoder(~valid))
      valid := valid | UIntToOH(r)
      tags(r) := tag
      data(r) := pte.ppn
      plru.access(r)
    }
    when (hit && state === s_req) { plru.access(OHToUInt(hits)) }
    when (io.dpath.sfence.valid && (!io.dpath.sfence.bits.rs1 || usingHypervisor && io.dpath.sfence.bits.hg)) { valid := 0.U }

    val lcount = if (s2) aux_count else count
    for (i <- 0 until pgLevels-1) {
      ccover(hit && state === s_req && lcount === i, s"PTE_CACHE_HIT_L$i", s"PTE cache hit, level $i")
    }

    (hit, Mux1H(hits, data))
  }

  val (pte_cache_hit, pte_cache_data) = makePTECache(false)
  val (stage2_pte_cache_hit, stage2_pte_cache_data) = makePTECache(true)

  val pte_hit = RegNext(false.B)
  io.dpath.perf.pte_miss := false
  io.dpath.perf.pte_hit := pte_hit && (state === s_req) && !io.dpath.perf.l2hit
  assert(!(io.dpath.perf.l2hit && (io.dpath.perf.pte_miss || io.dpath.perf.pte_hit)),
    "PTE Cache Hit/Miss Performance Monitor Events are lower priority than L2TLB Hit event")

  val l2_refill = RegNext(false.B)
  l2_refill_wire := l2_refill
  io.dpath.perf.l2miss := false
  io.dpath.perf.l2hit := false
  val (l2_hit, l2_error, l2_pte, l2_tlb_ram) = if (coreParams.nL2TLBEntries == 0) (false.B, false.B, Wire(new PTE), None) else {
    val code = new ParityCode
    require(isPow2(coreParams.nL2TLBEntries))
    require(isPow2(coreParams.nL2TLBWays))
    require(coreParams.nL2TLBEntries >= coreParams.nL2TLBWays)
    val nL2TLBSets = coreParams.nL2TLBEntries / coreParams.nL2TLBWays
    require(isPow2(nL2TLBSets))
    val idxBits = log2Ceil(nL2TLBSets)

    val l2_plru = new SetAssocLRU(nL2TLBSets, coreParams.nL2TLBWays, "plru")

    val (ram, omSRAM) =  DescribedSRAM(
      name = "l2_tlb_ram",
      desc = "L2 TLB",
      size = nL2TLBSets,
      data = Vec(coreParams.nL2TLBWays, UInt(width = code.width(new L2TLBEntry(nL2TLBSets).getWidth)))
    )

    val g = Reg(Vec(coreParams.nL2TLBWays, UInt(width = nL2TLBSets)))
    val valid = RegInit(Vec(Seq.fill(coreParams.nL2TLBWays)(0.U(nL2TLBSets.W))))
    val (r_tag, r_idx) = Split(Cat(r_req.vstage1, r_req.addr(maxSVAddrBits-pgIdxBits-1, 0)), idxBits)
    val r_valid_vec = valid.map(_(r_idx)).asUInt
    val r_valid_vec_q = Reg(UInt(coreParams.nL2TLBWays.W))
    val r_l2_plru_way = Reg(UInt(log2Ceil(coreParams.nL2TLBWays max 1).W))
    r_valid_vec_q := r_valid_vec
    r_l2_plru_way := (if (coreParams.nL2TLBWays > 1) l2_plru.way(r_idx) else 0.U)
    when (l2_refill && !invalidated) {
      val entry = Wire(new L2TLBEntry(nL2TLBSets))
      entry := r_pte
      entry.tag := r_tag

      val wmask = if (coreParams.nL2TLBWays > 1) Mux(r_valid_vec_q.andR, UIntToOH(r_l2_plru_way, coreParams.nL2TLBWays), PriorityEncoderOH(~r_valid_vec_q)) else 1.U(1.W)
      ram.write(r_idx, Vec(Seq.fill(coreParams.nL2TLBWays)(code.encode(entry.asUInt))), wmask.asBools)

      val mask = UIntToOH(r_idx)
      for (way <- 0 until coreParams.nL2TLBWays) {
        when (wmask(way)) {
          valid(way) := valid(way) | mask
          g(way) := Mux(r_pte.g, g(way) | mask, g(way) & ~mask)
        }
      }
    }

    when (io.dpath.sfence.valid) {
      val hg = usingHypervisor && io.dpath.sfence.bits.hg
      for (way <- 0 until coreParams.nL2TLBWays) {
        valid(way) :=
          Mux(!hg && io.dpath.sfence.bits.rs1, valid(way) & ~UIntToOH(io.dpath.sfence.bits.addr(idxBits+pgIdxBits-1, pgIdxBits)),
          Mux(!hg && io.dpath.sfence.bits.rs2, valid(way) & g(way),
          0.U))
      }
    }

    val s0_valid = !l2_refill && arb.io.out.fire()
    val s0_suitable = arb.io.out.bits.bits.vstage1 === arb.io.out.bits.bits.stage2 && !arb.io.out.bits.bits.need_gpa
    val s1_valid = RegNext(s0_valid && s0_suitable && arb.io.out.bits.valid)
    val s2_valid = RegNext(s1_valid)
    val s1_rdata = ram.read(arb.io.out.bits.bits.addr(idxBits-1, 0), s0_valid)
    val s2_rdata = s1_rdata.map(s1_rdway => code.decode(RegEnable(s1_rdway, s1_valid)))
    val s2_valid_vec = RegEnable(r_valid_vec, s1_valid)
    val s2_g_vec = RegEnable(Vec(g.map(_(r_idx))), s1_valid)
    val s2_error = (0 until coreParams.nL2TLBWays).map(way => s2_valid_vec(way) && s2_rdata(way).error).orR
    when (s2_valid && s2_error) { valid.foreach { _ := 0.U }}

    val s2_entry_vec = s2_rdata.map(_.uncorrected.asTypeOf(new L2TLBEntry(nL2TLBSets)))
    val s2_hit_vec = (0 until coreParams.nL2TLBWays).map(way => s2_valid_vec(way) && (r_tag === s2_entry_vec(way).tag))
    val s2_hit = s2_valid && s2_hit_vec.orR
    io.dpath.perf.l2miss := s2_valid && !(s2_hit_vec.orR)
    io.dpath.perf.l2hit := s2_hit
    when (s2_hit) {
      l2_plru.access(r_idx, OHToUInt(s2_hit_vec))
      assert((PopCount(s2_hit_vec) === 1.U) || s2_error, "L2 TLB multi-hit")
    }

    val s2_pte = Wire(new PTE)
    s2_pte   := Mux1H(s2_hit_vec, s2_entry_vec)
    s2_pte.g := Mux1H(s2_hit_vec, s2_g_vec)
    s2_pte.v := true

    for (way <- 0 until coreParams.nL2TLBWays) {
      ccover(s2_hit && s2_hit_vec(way), s"L2_TLB_HIT_WAY$way", s"L2 TLB hit way$way")
    }

    omSRAMs += omSRAM
    (s2_hit, s2_error, s2_pte, Some(ram))
  }

  // if SFENCE occurs during walk, don't refill PTE cache or L2 TLB until next walk
  invalidated := io.dpath.sfence.valid || (invalidated && state =/= s_ready)

  io.mem.req.valid := state === s_req || state === s_dummy1
  io.mem.req.bits.phys := Bool(true)
  io.mem.req.bits.cmd  := M_XRD
  io.mem.req.bits.size := log2Ceil(xLen/8)
  io.mem.req.bits.signed := false
  io.mem.req.bits.addr := pte_addr
  io.mem.req.bits.idx.foreach(_ := pte_addr)
  io.mem.req.bits.dprv := PRV.S.U   // PTW accesses are S-mode by definition
  io.mem.req.bits.dv := do_both_stages && !stage2
  io.mem.s1_kill := l2_hit || state =/= s_wait1
  io.mem.s2_kill := Bool(false)

  val pageGranularityPMPs = pmpGranularity >= (1 << pgIdxBits)
  require(!usingHypervisor || pageGranularityPMPs, s"hypervisor requires pmpGranularity >= ${1<<pgIdxBits}")

  val pmaPgLevelHomogeneous = (0 until pgLevels) map { i =>
    val pgSize = BigInt(1) << (pgIdxBits + ((pgLevels - 1 - i) * pgLevelBits))
    if (pageGranularityPMPs && i == pgLevels - 1) {
      require(TLBPageLookup.homogeneous(edge.manager.managers, pgSize), s"All memory regions must be $pgSize-byte aligned")
      true.B
    } else {
      TLBPageLookup(edge.manager.managers, xLen, p(CacheBlockBytes), pgSize)(r_pte.ppn << pgIdxBits).homogeneous
    }
  }
  val pmaHomogeneous = pmaPgLevelHomogeneous(count)
  val pmpHomogeneous = new PMPHomogeneityChecker(io.dpath.pmp).apply(r_pte.ppn << pgIdxBits, count)
  val homogeneous = pmaHomogeneous && pmpHomogeneous

  for (i <- 0 until io.requestor.size) {
    io.requestor(i).resp.valid := resp_valid(i)
    io.requestor(i).resp.bits.ae_ptw := resp_ae_ptw
    io.requestor(i).resp.bits.ae_final := resp_ae_final
    io.requestor(i).resp.bits.gf := resp_gf
    io.requestor(i).resp.bits.hr := resp_hr
    io.requestor(i).resp.bits.hw := resp_hw
    io.requestor(i).resp.bits.hx := resp_hx
    io.requestor(i).resp.bits.pte := r_pte
    io.requestor(i).resp.bits.level := max_count
    io.requestor(i).resp.bits.homogeneous := homogeneous || pageGranularityPMPs
    io.requestor(i).resp.bits.fragmented_superpage := resp_fragmented_superpage && pageGranularityPMPs
    io.requestor(i).resp.bits.gpa.valid := r_req.need_gpa
    io.requestor(i).resp.bits.gpa.bits :=
      Cat(Mux(!stage2_final || !r_req.vstage1 || aux_count === (pgLevels - 1), aux_pte.ppn, makeFragmentedSuperpagePPN(aux_pte.ppn)(aux_count)), gpa_pgoff)
    io.requestor(i).ptbr := io.dpath.ptbr
    io.requestor(i).hgatp := io.dpath.hgatp
    io.requestor(i).vsatp := io.dpath.vsatp
    io.requestor(i).customCSRs := io.dpath.customCSRs
    io.requestor(i).status := io.dpath.status
    io.requestor(i).hstatus := io.dpath.hstatus
    io.requestor(i).gstatus := io.dpath.gstatus
    io.requestor(i).pmp := io.dpath.pmp
  }

  // control state machine
  val next_state = Wire(init = state)
  state := OptimizationBarrier(next_state)
  val do_switch = Wire(init = false.B)

  switch (state) {
    is (s_ready) {
      when (arb.io.out.fire()) {
        val satp_initial_count = pgLevels - minPgLevels - satp.additionalPgLevels
        val vsatp_initial_count = pgLevels - minPgLevels - io.dpath.vsatp.additionalPgLevels
        val hgatp_initial_count = pgLevels - minPgLevels - io.dpath.hgatp.additionalPgLevels

        r_req := arb.io.out.bits.bits
        r_req_dest := arb.io.chosen
        next_state := Mux(arb.io.out.bits.valid, s_req, s_ready)
        stage2       := arb.io.out.bits.bits.stage2
        stage2_final := arb.io.out.bits.bits.stage2 && !arb.io.out.bits.bits.vstage1
        count       := Mux(arb.io.out.bits.bits.stage2, hgatp_initial_count, satp_initial_count)
        aux_count   := Mux(arb.io.out.bits.bits.vstage1, vsatp_initial_count, 0.U)
        aux_pte.ppn := Mux(arb.io.out.bits.bits.vstage1, io.dpath.vsatp.ppn, arb.io.out.bits.bits.addr)
        resp_ae_ptw := false
        resp_ae_final := false
        resp_gf := false
        resp_hr := true
        resp_hw := true
        resp_hx := true
        resp_fragmented_superpage := false
        r_hgatp := io.dpath.hgatp

        assert(!arb.io.out.bits.bits.need_gpa || arb.io.out.bits.bits.stage2)
      }
    }
    is (s_req) {
      when(stage2 && count === r_hgatp_initial_count) {
        gpa_pgoff := Mux(aux_count === pgLevels-1, r_req.addr << (xLen/8).log2, stage2_pte_cache_addr)
      }

      when (stage2_pte_cache_hit) {
        aux_count := aux_count + 1
        aux_pte.ppn := stage2_pte_cache_data
        pte_hit := true
      }.elsewhen (pte_cache_hit) {
        count := count + 1
        pte_hit := true
      }.otherwise {
        next_state := Mux(io.mem.req.ready, s_wait1, s_req)
      }
    }
    is (s_wait1) {
      // This Mux is for the l2_error case; the l2_hit && !l2_error case is overriden below
      next_state := Mux(l2_hit, s_req, s_wait2)
    }
    is (s_wait2) {
      next_state := s_wait3
      io.dpath.perf.pte_miss := count < pgLevels-1
      when (io.mem.s2_xcpt.ae.ld) {
        resp_ae_ptw := true
        next_state := s_ready
        resp_valid(r_req_dest) := true
      }
    }
    is (s_fragment_superpage) {
      next_state := s_ready
      resp_valid(r_req_dest) := true
      when (!homogeneous) {
        count := pgLevels-1
        resp_fragmented_superpage := true
      }
      when (do_both_stages) {
        resp_fragmented_superpage := true
      }
    }
  }

  val merged_pte = {
    val superpage_masks = (0 until pgLevels).map(i => ((BigInt(1) << pte.ppn.getWidth) - (BigInt(1) << (pgLevels-1-i)*pgLevelBits)).U)
    val superpage_mask = superpage_masks(Mux(stage2_final, max_count, (pgLevels-1).U))
    val stage1_ppns = (0 until pgLevels-1).map(i => Cat(pte.ppn(pte.ppn.getWidth-1, (pgLevels-i-1)*pgLevelBits), aux_pte.ppn((pgLevels-i-1)*pgLevelBits-1,0))) :+ pte.ppn
    val stage1_ppn = stage1_ppns(count)
    makePTE(stage1_ppn & superpage_mask, aux_pte)
  }

  r_pte := OptimizationBarrier(
    Mux(l2_hit && !l2_error, l2_pte,
    Mux(state === s_req && !stage2_pte_cache_hit && pte_cache_hit, makePTE(pte_cache_data, l2_pte),
    Mux(do_switch, makeHypervisorRootPTE(r_hgatp, pte.ppn, r_pte),
    Mux(mem_resp_valid, Mux(!traverse && (r_req.vstage1 && stage2), merged_pte, pte),
    Mux(state === s_fragment_superpage && !homogeneous, makePTE(makeFragmentedSuperpagePPN(r_pte.ppn)(count), r_pte),
    Mux(arb.io.out.fire(), Mux(arb.io.out.bits.bits.stage2, makeHypervisorRootPTE(io.dpath.hgatp, io.dpath.vsatp.ppn, r_pte), makePTE(satp.ppn, r_pte)),
    r_pte)))))))

  when (l2_hit && !l2_error) {
    assert(state === s_req || state === s_wait1)
    next_state := s_ready
    resp_valid(r_req_dest) := true
    count := pgLevels-1
  }
  when (mem_resp_valid) {
    assert(state === s_wait3)
    next_state := s_req
    when (traverse) {
      when (do_both_stages && !stage2) { do_switch := true }
      count := count + 1
    }.otherwise {
      val gf = stage2 && !stage2_final && !pte.ur()
      val ae = pte.v && invalid_paddr
      val success = pte.v && !ae && !gf

      when (do_both_stages && !stage2_final && success) {
        when (stage2) {
          stage2 := false
          count := aux_count
        }.otherwise {
          stage2_final := true
          do_switch := true
        }
      }.otherwise {
        l2_refill := success && count === pgLevels-1 && !r_req.need_gpa &&
          (!r_req.vstage1 && !r_req.stage2 ||
           do_both_stages && aux_count === pgLevels-1 && pte.isFullPerm())
        count := max_count

        when (pageGranularityPMPs && !(count === pgLevels-1 && (!do_both_stages || aux_count === pgLevels-1))) {
          next_state := s_fragment_superpage
        }.otherwise {
          next_state := s_ready
          resp_valid(r_req_dest) := true
        }

        resp_ae_final := ae
        resp_gf := gf
        resp_hr := !stage2 || !gf && pte.ur()
        resp_hw := !stage2 || !gf && pte.uw()
        resp_hx := !stage2 || !gf && pte.ux()
      }
    }
  }
  when (io.mem.s2_nack) {
    assert(state === s_wait2)
    next_state := s_req
  }

  when (do_switch) {
    aux_count := Mux(traverse, count + 1, count)
    count := r_hgatp_initial_count
    aux_pte := Mux(traverse, pte, {
      val s1_ppns = (0 until pgLevels-1).map(i => Cat(pte.ppn(pte.ppn.getWidth-1, (pgLevels-i-1)*pgLevelBits), r_req.addr(((pgLevels-i-1)*pgLevelBits min vpnBits)-1,0).padTo((pgLevels-i-1)*pgLevelBits))) :+ pte.ppn
      makePTE(s1_ppns(count), pte)
    })
    stage2 := true
  }

  for (i <- 0 until pgLevels) {
    val leaf = mem_resp_valid && !traverse && count === i
    ccover(leaf && pte.v && !invalid_paddr, s"L$i", s"successful page-table access, level $i")
    ccover(leaf && pte.v && invalid_paddr, s"L${i}_BAD_PPN_MSB", s"PPN too large, level $i")
    ccover(leaf && !mem_resp_data(0), s"L${i}_INVALID_PTE", s"page not present, level $i")
    if (i != pgLevels-1)
      ccover(leaf && !pte.v && mem_resp_data(0), s"L${i}_BAD_PPN_LSB", s"PPN LSBs not zero, level $i")
  }
  ccover(mem_resp_valid && count === pgLevels-1 && pte.table(), s"TOO_DEEP", s"page table too deep")
  ccover(io.mem.s2_nack, "NACK", "D$ nacked page-table access")
  ccover(state === s_wait2 && io.mem.s2_xcpt.ae.ld, "AE", "access exception while walking page table")

  } // leaving gated-clock domain

  private def ccover(cond: Bool, label: String, desc: String)(implicit sourceInfo: SourceInfo) =
    if (usingVM) property.cover(cond, s"PTW_$label", "MemorySystem;;" + desc)

  private def makePTE(ppn: UInt, default: PTE) = {
    val pte = Wire(init = default)
    pte.ppn := ppn
    pte
  }

  private def makeHypervisorRootPTE(hgatp: PTBR, vpn: UInt, default: PTE) = {
    val count = pgLevels - minPgLevels - hgatp.additionalPgLevels
    val idxs = (0 to pgLevels-minPgLevels).map(i => (vpn >> (pgLevels-i)*pgLevelBits))
    val lsbs = Wire(t = UInt(maxHypervisorExtraAddrBits.W), init = idxs(count))
    val pte = Wire(init = default)
    pte.ppn := Cat(hgatp.ppn >> maxHypervisorExtraAddrBits, lsbs)
    pte
  }
}

/** Mix-ins for constructing tiles that might have a PTW */
trait CanHavePTW extends HasTileParameters with HasHellaCache { this: BaseTile =>
  val module: CanHavePTWModule
  val utlbOMSRAMs = collection.mutable.ListBuffer[OMSRAM]()
  var nPTWPorts = 1
  nDCachePorts += usingPTW.toInt
}

trait CanHavePTWModule extends HasHellaCacheModule {
  val outer: CanHavePTW
  val ptwPorts = ListBuffer(outer.dcache.module.io.ptw)
  val ptw = Module(new PTW(outer.nPTWPorts)(outer.dcache.node.edges.out(0), outer.p))
  if (outer.usingPTW) {
    dcachePorts += ptw.io.mem
    outer.utlbOMSRAMs ++= ptw.omSRAMs
  }
}
