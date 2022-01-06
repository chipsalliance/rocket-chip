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
}

class PTWResp(implicit p: Parameters) extends CoreBundle()(p) {
  val ae = Bool()
  val pte = new PTE
  val level = UInt(width = log2Ceil(pgLevels))
  val fragmented_superpage = Bool()
  val homogeneous = Bool()
}

class TLBPTWIO(implicit p: Parameters) extends CoreBundle()(p)
    with HasCoreParameters {
  val req = Decoupled(Valid(new PTWReq))
  val resp = Valid(new PTWResp).flip
  val ptbr = new PTBR().asInput
  val status = new MStatus().asInput
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
  val sfence = Valid(new SFenceReq).flip
  val status = new MStatus().asInput
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
}

/**
  * |vpn2  |vpn1  |vpn0    |
  * -> set associated
  * |vpn2  |vpn1  |vpn0|idx|
  *
  * idx decides set.
  *
  * */
class L2TLBEntry(nSets: Int)(implicit p: Parameters) extends CoreBundle()(p)
    with HasCoreParameters {
  val idxBits = log2Ceil(nSets)
  val tagBits = vpnBits - idxBits
  val tag = UInt(width = tagBits)
  val ppn = UInt(width = ppnBits)
  val d = Bool()
  val a = Bool()
  val u = Bool()
  val x = Bool()
  val w = Bool()
  val r = Bool()

}

/**
  * RISC-V PTW implementation with L2 TLB.
  *
  */
@chiselName
class PTW(n: Int)(implicit edge: TLEdgeOut, p: Parameters) extends CoreModule()(p) {
  val io = new Bundle {
    /** requests from different TLBs. */
    val requestor = Vec(n, new TLBPTWIO).flip
    /** PTW to memory interface, it directly connected to L1D$.
      * In single Rocket, when page is manipulated, it must exist in L1D$.
      * So L2TLB won't need additional probe procedure at coherence manager.
      */
    val mem = new HellaCacheIO
    val dpath = new DatapathPTWIO
  }

  val omSRAMs = collection.mutable.ListBuffer[OMSRAM]()

  /**
    * At [[s_req]], access PTE cache & L2TLB at the same time(cycle 1)
    * in cycle 1, `pte_cache_hit` is known,
    *             if not hit, just to [[s_wait1]](this will be blocked by I$ ready.)
    * in cycle 2(if I$ is not blocked), `l2_hit` is known.
    *
    *
    * [[s_ready]]:              000 ready to accept from L1TLBs
    * [[s_req]]:                001 access PTE cache, increase page count.
    *                               except PTE cache hit(pte_cache_hit).
    *                               if missed jump to [[s_wait1]](wait for main memory)
    *                               send request to D$ & L2TLB.
    *
    * [[s_wait1]]:              010 can query L2TLB response: `l2_hit`
    *                               if missed, go to [[s_wait2]]
    *
    * [[s_wait2]]:              100 D$ will return if access fault or not at second cycle.
    *                               if AE, crashed and reset PTW(go to [[s_ready]])
    *                               if no AE, go to [[s_wait3]]
    *
    * [[s_wait3]]:              101 wait for main memory.(can be multiple cycles.)
    * [[s_fragment_superpage]]: 111 process super page.
    *
    * [[s_dummy1]]:             011 make state is s_req -> 0?1
    * [[s_dummy2]]:             110
    */
  val s_ready :: s_req :: s_wait1 :: s_dummy1 :: s_wait2 :: s_wait3 :: s_dummy2 :: s_fragment_superpage :: Nil = Enum(UInt(), 8)
  /** state machine register, define under the non-gated clock domain. */
  val state = Reg(init=s_ready)
  /** is refilling L2TLB, requests from L1TLB is not available. */
  val l2_refill_wire = Wire(Bool())

  /** arbitration from different L1TLBs(ITLB, DTLB, RoCC TLB...). */
  val arb = Module(new Arbiter(Valid(new PTWReq), n))
  arb.io.in <> io.requestor.map(_.req)
  arb.io.out.ready := (state === s_ready) && !l2_refill_wire

  /** Maintain a respond valid register, if true, response to correspond requestor. */
  val resp_valid = Reg(next = Vec.fill(io.requestor.size)(Bool(false)))

  /** clock gate logic.
    * manually clock gate for `state === s_ready`.
    */
  val clock_en = state =/= s_ready || l2_refill_wire || arb.io.out.valid || io.dpath.sfence.valid || io.dpath.customCSRs.disableDCacheClockGate
  io.dpath.clock_enabled := usingVM && clock_en
  val gated_clock =
    if (!usingVM || !tileParams.dcache.get.clockGate) clock
    else ClockGate(clock, clock_en, "ptw_clock_gate")
  withClock (gated_clock) { // entering gated-clock domain

  /** logic to invalidate TLB(handle SFENCE.VMA).
    * don't invalidate during PTW.
    */
  val invalidated = Reg(Bool())
  /** current page level. */
  val count = Reg(UInt(width = log2Up(pgLevels)))
  /** respond access fault. */
  val resp_ae = RegNext(false.B)
  /** not last page level but leaf page. */
  val resp_fragmented_superpage = RegNext(false.B)

  /** latch request from arbiter. */
  val r_req = Reg(new PTWReq)
  /** which L1TLB should respond to. */
  val r_req_dest = Reg(Bits())
  /** PTE to respond. */
  val r_pte = Reg(new PTE)

  /** latch valid from memory(hella cache interface). */
  val mem_resp_valid = RegNext(io.mem.resp.valid)
  /** latch data from memory(hella cache interface). */
  val mem_resp_data = RegNext(io.mem.resp.bits.data)
  // support page from uncached region.
  // add a mux to [[mem_resp_valid]], [[mem_resp_data]]
  io.mem.uncached_resp.map { resp =>
    assert(!(resp.valid && io.mem.resp.valid))
    resp.ready := true
    when (resp.valid) {
      mem_resp_valid := true
      mem_resp_data := resp.bits.data
    }
  }

  /* construct PTE from memory. */
  val (pte, invalid_paddr) = {
    val tmp = new PTE().fromBits(mem_resp_data)
    val res = Wire(init = tmp)
    res.ppn := tmp.ppn(ppnBits-1, 0)
    when (tmp.r || tmp.w || tmp.x) {
      // for superpage mappings, make sure PPN LSBs are zero
      for (i <- 0 until pgLevels-1)
        when (count <= i && tmp.ppn((pgLevels-1-i)*pgLevelBits-1, (pgLevels-2-i)*pgLevelBits) =/= 0) { res.v := false }
    }
    (res, (tmp.ppn >> ppnBits) =/= 0)
  }
  /** is traversing page table(PTE on D$ is non-leaf) */
  val traverse = pte.table() && !invalid_paddr && count < pgLevels-1
  /* get pte address.
   * TODO: why << 3/2
   */
  val pte_addr = if (!usingVM) 0.U else {
    val vpn_idxs = (0 until pgLevels).map(i => (r_req.addr >> (pgLevels-i-1)*pgLevelBits)(pgLevelBits-1,0))
    val vpn_idx = vpn_idxs(count)
    Cat(r_pte.ppn, vpn_idx) << log2Ceil(xLen/8)
  }
  val fragmented_superpage_ppn = {
    val choices = (pgLevels-1 until 0 by -1).map(i => Cat(r_pte.ppn >> (pgLevelBits*i), r_req.addr(((pgLevelBits*i) min vpnBits)-1, 0).padTo(pgLevelBits*i)))
    // dynamic selection but not too big.
    choices(count)
  }

  /* logic to select from arbiter. */
  when (arb.io.out.fire()) {
    r_req := arb.io.out.bits.bits
    r_req_dest := arb.io.chosen
  }

  /** PTE Cache
    *
    * data: PPN
    * tag: paddr(page table address) PTBR at first.
    *
    * replacement policy: full -> plru, not full -> find first empty.
    * write en: D$ respond, and not SFENCE.VMA
    * read en: s_req
    */
  val (pte_cache_hit, pte_cache_data) = {
    val size = 1 << log2Up(pgLevels * 2)
    /** replacement logic is pseudo lru
      * TODO: really good? I doubt random will be enough.
      */
    val plru = new PseudoLRU(size)
    val valid = RegInit(0.U(size.W))
    val tags = Reg(Vec(size, UInt(width = paddrBits)))
    val data = Reg(Vec(size, UInt(width = ppnBits)))

    val hits = tags.map(_ === pte_addr).asUInt & valid
    val hit = hits.orR
    /* cache write logic. */
    when (mem_resp_valid && traverse && !hit && !invalidated) {
      // if full -> plru, not full -> find first empty.
      val r = Mux(valid.andR, plru.way, PriorityEncoder(~valid))
      valid := valid | UIntToOH(r)
      tags(r) := pte_addr
      data(r) := pte.ppn
    }
    // update PLRU
    when (hit && state === s_req) { plru.access(OHToUInt(hits)) }
    // clean cache when SFENCE.VMA
    when (io.dpath.sfence.valid && !io.dpath.sfence.bits.rs1) { valid := 0.U }

    for (i <- 0 until pgLevels-1)
      ccover(hit && state === s_req && count === i, s"PTE_CACHE_HIT_L$i", s"PTE cache hit, level $i")

    (hit && count < pgLevels-1, Mux1H(hits, data))
  }
  /** PTE hit at [[s_req]] state. */
  val pte_hit = RegNext(false.B)
  io.dpath.perf.pte_miss := false
  io.dpath.perf.pte_hit := pte_hit && (state === s_req) && !io.dpath.perf.l2hit
  assert(!(io.dpath.perf.l2hit && (io.dpath.perf.pte_miss || io.dpath.perf.pte_hit)),
    "PTE Cache Hit/Miss Performance Monitor Events are lower priority than L2TLB Hit event")

  /** signal to write L2TLB. */
  val l2_refill = RegNext(false.B)
  l2_refill_wire := l2_refill
  io.dpath.perf.l2miss := false
  io.dpath.perf.l2hit := false

  /** L2TLB Cache
    *
    * Sync Read RAM
    * set associated.
    * ParityCoded
    *
    * size: nL2TLBSets
    * data: L2TLBEntry
    * index: vpn - idxbits
    * replacement policy: full -> plru, not full -> find first empty.
    * write en: l2_refill & !SFENCE.VMA
    * read en: s0_valid(s_req)
    */
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
    /** valid register */
    val valid = RegInit(Vec(Seq.fill(coreParams.nL2TLBWays)(0.U(nL2TLBSets.W))))
    // vaddr tag & vaddr index
    val (r_tag, r_idx) = Split(r_req.addr, idxBits)
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
      // replacement.
      ram.write(r_idx, Vec(Seq.fill(coreParams.nL2TLBWays)(code.encode(entry.asUInt))), wmask.asBools)

      val mask = UIntToOH(r_idx)
      for (way <- 0 until coreParams.nL2TLBWays) {
        when (wmask(way)) {
          valid(way) := valid(way) | mask
          g(way) := Mux(r_pte.g, g(way) | mask, g(way) & ~mask)
        }
      }
    }
    /** based on SFEENCE.VMA clear correspond TLB. */
    when (io.dpath.sfence.valid) {
      for (way <- 0 until coreParams.nL2TLBWays) {
        valid(way) :=
          Mux(io.dpath.sfence.bits.rs1, valid(way) & ~UIntToOH(io.dpath.sfence.bits.addr(idxBits+pgIdxBits-1, pgIdxBits)),
          Mux(io.dpath.sfence.bits.rs2, valid(way) & g(way), 0.U))
      }
    }

    // write to L2TLB will block read.
    val s0_valid = !l2_refill && arb.io.out.fire()
    val s1_valid = RegNext(s0_valid && arb.io.out.bits.valid)
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
  // TODO: resolve state transition when s_req -> s_wait1 -> s_req (PTE cache hit)?
  //       but not so sure here. s2_kill := state =/= s_wait1
  io.mem.s1_kill := l2_hit || state =/= s_wait1
  io.mem.s2_kill := Bool(false)

  val pageGranularityPMPs = pmpGranularity >= (1 << pgIdxBits)
  /** generate PMA homogeneous check for each page level. */
  val pmaPgLevelHomogeneous = (0 until pgLevels) map { i =>
    val pgSize = BigInt(1) << (pgIdxBits + ((pgLevels - 1 - i) * pgLevelBits))
    if (pageGranularityPMPs && i == pgLevels - 1) {
      require(TLBPageLookup.homogeneous(edge.manager.managers, pgSize), s"All memory regions must be $pgSize-byte aligned")
      true.B
    } else {
      TLBPageLookup(edge.manager.managers, xLen, p(CacheBlockBytes), pgSize)(pte_addr).homogeneous
    }
  }
  val pmaHomogeneous = pmaPgLevelHomogeneous(count)
  val pmpHomogeneous = new PMPHomogeneityChecker(io.dpath.pmp).apply(pte_addr >> pgIdxBits << pgIdxBits, count)
  /** if super page didn't cross PMP and PMA, it is homogeneous. */
  val homogeneous = pmaHomogeneous && pmpHomogeneous

  for (i <- 0 until io.requestor.size) {
    io.requestor(i).resp.valid := resp_valid(i)
    io.requestor(i).resp.bits.ae := resp_ae
    io.requestor(i).resp.bits.pte := r_pte
    io.requestor(i).resp.bits.level := count
    io.requestor(i).resp.bits.homogeneous := homogeneous || pageGranularityPMPs
    io.requestor(i).resp.bits.fragmented_superpage := resp_fragmented_superpage && pageGranularityPMPs
    io.requestor(i).ptbr := io.dpath.ptbr
    io.requestor(i).customCSRs := io.dpath.customCSRs
    io.requestor(i).status := io.dpath.status
    io.requestor(i).pmp := io.dpath.pmp
  }

  // control state machine
  val next_state = Wire(init = state)
  state := OptimizationBarrier(next_state)

  switch (state) {
    is (s_ready) {
      // even request is fired, it might be non-valid
      when (arb.io.out.fire()) {
        next_state := Mux(arb.io.out.bits.valid, s_req, s_ready)
      }
      count := pgLevels - minPgLevels - io.dpath.ptbr.additionalPgLevels
    }
    is (s_req) {
      when (pte_cache_hit) {
        // to next page level
        count := count + 1
        pte_hit := true
      }.otherwise {
        // if not hit, and D$ is able to process request, just to [[s_wait1]]
        // notice there will be another transition: next_state -> s_ready if TLB hit.
        next_state := Mux(io.mem.req.ready, s_wait1, s_req)
      }
    }
    is (s_wait1) {
      // This Mux is for the l2_error case; the l2_hit && !l2_error case is overriden below
      next_state := Mux(l2_hit, s_req, s_wait2)
    }
    is (s_wait2) {
      // This is the 2nd cycle to access D$, get access fault(load access fault).
      next_state := s_wait3
      io.dpath.perf.pte_miss := count < pgLevels-1
      when (io.mem.s2_xcpt.ae.ld) {
        resp_ae := true
        next_state := s_ready
        resp_valid(r_req_dest) := true
      }
    }
    is (s_fragment_superpage) {
      next_state := s_ready
      resp_valid(r_req_dest) := true
      resp_ae := false
      when (!homogeneous) {
        count := pgLevels-1
        resp_fragmented_superpage := true
      }
    }
  }

  def makePTE(ppn: UInt, default: PTE) = {
    val pte = Wire(init = default)
    pte.ppn := ppn
    pte
  }
  r_pte := OptimizationBarrier(
    // PTW access forward
    Mux(mem_resp_valid, pte,
    // L2 TLB cache hit
    Mux(l2_hit && !l2_error, l2_pte,
    // superpage
    Mux(state === s_fragment_superpage && !homogeneous, makePTE(fragmented_superpage_ppn, r_pte),
    // small PTE cache hit
    Mux(state === s_req && pte_cache_hit, makePTE(pte_cache_data, l2_pte),
    // query level 1 page table via ptbr
    // for physical design, when start a PTW
    // only need PPN from PTBR, let other PTE entries not being updated to save logic
    Mux(arb.io.out.fire(), makePTE(io.dpath.ptbr.ppn, r_pte),
    r_pte))))))

  when (l2_hit && !l2_error) {
    assert(state === s_req || state === s_wait1)
    next_state := s_ready
    resp_valid(r_req_dest) := true
    resp_ae := false
    count := pgLevels-1
  }

  when (mem_resp_valid) {
    assert(state === s_wait3)
    when (traverse) {
      next_state := s_req
      count := count + 1
    }.otherwise {
      // refill only when leaf PTE and last page level.
      l2_refill := pte.v && !invalid_paddr && count === pgLevels-1
      val ae = pte.v && invalid_paddr
      resp_ae := ae
      // when not highest page level, but this is leaf, process it as fragment_superpage
      when (pageGranularityPMPs && count =/= pgLevels-1 && !ae) {
        next_state := s_fragment_superpage
      }.otherwise {
        next_state := s_ready
        resp_valid(r_req_dest) := true
      }
    }
  }
  // go back to s_req, and retry.
  when (io.mem.s2_nack) {
    assert(state === s_wait2)
    next_state := s_req
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
