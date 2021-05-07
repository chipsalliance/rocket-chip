// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.rocket

import Chisel._
import Chisel.ImplicitConversions._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.subsystem.CacheBlockBytes
import freechips.rocketchip.diplomacy.RegionType
import freechips.rocketchip.tile.{CoreModule, CoreBundle}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property._
import freechips.rocketchip.devices.debug.DebugModuleKey
import chisel3.internal.sourceinfo.SourceInfo

case object PgLevels extends Field[Int](2)
case object ASIdBits extends Field[Int](0)

/**
  * rs1 rs2
  *  0   0 -> flush All
  *  0   1 -> flush by ASID
  *  1   1 -> flush by ADDR
  *  1   0 -> flush by ADDR and ASID
  * If rs1=x0 and rs2=x0, the fence orders all reads and writes made to any level of the page tables, for all address spaces.
  * If rs1=x0 and rs2!=x0, the fence orders all reads and writes made to any level of the page tables, but only for the address space identified by integer register rs2. Accesses to global mappings (see Section 4.3.1) are not ordered.
  * If rs1!=x0 and rs2=x0, the fence orders only reads and writes made to the leaf page table entry corresponding to the virtual address in rs1, for all address spaces.
  * If rs1!=x0 and rs2!=x0, the fence orders only reads and writes made to the leaf page table entry corresponding to the virtual address in rs1, for the address space identified by integer register rs2. Accesses to global mappings are not ordered.
  */
class SFenceReq(implicit p: Parameters) extends CoreBundle()(p) {
  val rs1 = Bool()
  val rs2 = Bool()
  val addr = UInt(width = vaddrBits)
  val asid = UInt(width = asIdBits max 1) // TODO zero-width
}

class TLBReq(lgMaxSize: Int)(implicit p: Parameters) extends CoreBundle()(p) {
  /** request address from CPU.
    * |vaddr                                                 |
    * |                 ppn/vpn                  | pgIndex   |
    * |                                          |           |
    * |           |nSets             |nSector    |           |
    */
  val vaddr = UInt(width = vaddrBitsExtended)
  /** don't lookup TLB, bypass vaddr as paddr. */
  val passthrough = Bool()
  /** @todo seems granularity */
  val size = UInt(width = log2Ceil(lgMaxSize + 1))
  /** memory command. */
  val cmd  = Bits(width = M_SZ)

  override def cloneType = new TLBReq(lgMaxSize).asInstanceOf[this.type]
}

class TLBExceptions extends Bundle {
  val ld = Bool()
  val st = Bool()
  val inst = Bool()
}

class TLBResp(implicit p: Parameters) extends CoreBundle()(p) {
  // lookup responses
  /** @todo */
  val miss = Bool()
  /** physical address. */
  val paddr = UInt(width = paddrBits)
  /** page fault exception. */
  val pf = new TLBExceptions
  /** access exception. */
  val ae = new TLBExceptions
  /** misaligned access exception. */
  val ma = new TLBExceptions
  /** @todo */
  val cacheable = Bool()
  /** @todo */
  val must_alloc = Bool()
  /** @todo */
  val prefetchable = Bool()
}

class TLBEntryData(implicit p: Parameters) extends CoreBundle()(p) {
  val ppn = UInt(width = ppnBits)
  /** pte.u user */
  val u = Bool()
  /** pte.g global */
  val g = Bool()

  /** access exception.
    * D$ -> PTW -> TLB AE
    * Alignment failed.
    */
  val ae = Bool()

  /** supervisor write */
  val sw = Bool()
  /** supervisor execute */
  val sx = Bool()
  /** supervisor read */
  val sr = Bool()
  /** prot_w */
  val pw = Bool()
  /** prot_x */
  val px = Bool()
  /** prot_r */
  val pr = Bool()

  val ppp = Bool() // PutPartial
  val pal = Bool() // AMO logical
  val paa = Bool() // AMO arithmetic
  val eff = Bool() // get/put effects
  val c = Bool()
  val fragmented_superpage = Bool()
}

class TLBEntry(val nSectors: Int, val superpage: Boolean, val superpageOnly: Boolean)(implicit p: Parameters) extends CoreBundle()(p) {
  require(nSectors == 1 || !superpage)
  require(!superpageOnly || superpage)

  val level = UInt(width = log2Ceil(pgLevels))
  val tag = UInt(width = vpnBits)
  val data = Vec(nSectors, UInt(width = new TLBEntryData().getWidth))
  val valid = Vec(nSectors, Bool())
  def entry_data = data.map(_.asTypeOf(new TLBEntryData))

  private def sectorIdx(vpn: UInt) = vpn.extract(nSectors.log2-1, 0)
  def getData(vpn: UInt) = OptimizationBarrier(data(sectorIdx(vpn)).asTypeOf(new TLBEntryData))
  def sectorHit(vpn: UInt) = valid.orR && sectorTagMatch(vpn)
  def sectorTagMatch(vpn: UInt) = ((tag ^ vpn) >> nSectors.log2) === 0
  def hit(vpn: UInt) = {
    if (superpage && usingVM) {
      var tagMatch = valid.head
      for (j <- 0 until pgLevels) {
        val base = vpnBits - (j + 1) * pgLevelBits
        val ignore = level < j || superpageOnly && j == pgLevels - 1
        tagMatch = tagMatch && (ignore || tag(base + pgLevelBits - 1, base) === vpn(base + pgLevelBits - 1, base))
      }
      tagMatch
    } else {
      val idx = sectorIdx(vpn)
      valid(idx) && sectorTagMatch(vpn)
    }
  }
  def ppn(vpn: UInt, data: TLBEntryData) = {
    if (superpage && usingVM) {
      var res = data.ppn >> pgLevelBits*(pgLevels - 1)
      for (j <- 1 until pgLevels) {
        val ignore = level < j || superpageOnly && j == pgLevels - 1
        res = Cat(res, (Mux(ignore, vpn, 0.U) | data.ppn)(vpnBits - j*pgLevelBits - 1, vpnBits - (j + 1)*pgLevelBits))
      }
      res
    } else {
      data.ppn
    }
  }

  def insert(tag: UInt, level: UInt, entry: TLBEntryData): Unit = {
    this.tag := tag
    this.level := level.extract(log2Ceil(pgLevels - superpageOnly.toInt)-1, 0)

    val idx = sectorIdx(tag)
    valid(idx) := true
    data(idx) := entry.asUInt
  }

  def invalidate(): Unit = { valid.foreach(_ := false) }
  def invalidateVPN(vpn: UInt): Unit = {
    if (superpage) {
      when (hit(vpn)) { invalidate() }
    } else {
      when (sectorTagMatch(vpn)) { valid(sectorIdx(vpn)) := false }

      // For fragmented superpage mappings, we assume the worst (largest)
      // case, and zap entries whose most-significant VPNs match
      when (((tag ^ vpn) >> (pgLevelBits * (pgLevels - 1))) === 0) {
        for ((v, e) <- valid zip entry_data)
          when (e.fragmented_superpage) { v := false }
      }
    }
  }
  def invalidateNonGlobal(): Unit = {
    for ((v, e) <- valid zip entry_data)
      when (!e.g) { v := false }
  }
}

case class TLBConfig(
    nSets: Int,
    nWays: Int,
    nSectors: Int = 4,
    nSuperpageEntries: Int = 4)

/** MMU Block which can be used as PMA & TLB
  * @todo PMA -> consume diplomacy parameter generate physical memory address checking logic.
  * Boom use Rocket ITLB, and its own DTLB.
  * Accelerators:
  *   sha3: DTLB
  *   gemmini: DTLB
  *   hwacha: DTLB*2+ITLB
  *
  * @param instruction true for ITLB, false for DTLB
  * @param lgMaxSize @todo seems granularity
  * @param cfg [[TLBConfig]]
  * @param edge collect SoC metadata.
  */
class TLB(instruction: Boolean, lgMaxSize: Int, cfg: TLBConfig)(implicit edge: TLEdgeOut, p: Parameters) extends CoreModule()(p) {
  val io = new Bundle {
    /** request from CPU. */
    val req = Decoupled(new TLBReq(lgMaxSize)).flip
    /** response to CPU. */
    val resp = new TLBResp().asOutput
    /** SFence Input. */
    val sfence = Valid(new SFenceReq).asInput
    /** IO to PTW. */
    val ptw = new TLBPTWIO
    /** suppress a TLB refill, one cycle after a miss. */
    val kill = Bool(INPUT)
  }

  /** @todo WTF pmpGranularity? seems a bug here. */
  val pageGranularityPMPs = pmpGranularity >= (1 << pgIdxBits)
  /** virtual memory. */
  val vpn = io.req.bits.vaddr(vaddrBits-1, pgIdxBits)
  val memIdx = vpn.extract(cfg.nSectors.log2 + cfg.nSets.log2 - 1, cfg.nSectors.log2)

  /** TLB Entry */
  val sectored_entries = Reg(Vec(cfg.nSets, Vec(cfg.nWays / cfg.nSectors, new TLBEntry(cfg.nSectors, false, false))))
  /** Superpage Entry */
  val superpage_entries = Reg(Vec(cfg.nSuperpageEntries, new TLBEntry(1, true, true)))
  /** Special Entry
    * If PMP granularity is less than page size, thus need additional "special" entry manage PMP.
    */
  val special_entry = (!pageGranularityPMPs).option(Reg(new TLBEntry(1, true, false)))

  /** @todo */
  def ordinary_entries = sectored_entries(memIdx) ++ superpage_entries
  /** @todo */
  def all_entries = ordinary_entries ++ special_entry
  /** @todo */
  def all_real_entries = sectored_entries.flatten ++ superpage_entries ++ special_entry

  /** State Machine
    * s_ready: ready to accept request from CPU.
    * s_request: send request to PTW(L2TLB), when L1TLB(this) miss.
    * s_wait: wait for PTW to refill L1TLB.
    * s_wait_invalidate: L1TLB is waiting for respond from PTW, but L1TLB will invalidate respond from PTW.
    *
    */
  val s_ready :: s_request :: s_wait :: s_wait_invalidate :: Nil = Enum(UInt(), 4)
  val state = Reg(init=s_ready)
  val r_refill_tag = Reg(UInt(width = vpnBits))
  val r_superpage_repl_addr = Reg(UInt(log2Ceil(superpage_entries.size).W))
  val r_sectored_repl_addr = Reg(UInt(log2Ceil(sectored_entries(0).size).W))
  val r_sectored_hit_addr = Reg(UInt(log2Ceil(sectored_entries(0).size).W))
  val r_sectored_hit = Reg(Bool())

  /** privilege mode */
  val priv = if (instruction) io.ptw.status.prv else io.ptw.status.dprv
  val priv_s = priv(0)
  // user mode and supervisor mode
  val priv_uses_vm = priv <= PRV.S
  /** Enable Virtual Memory when:
    * 1. statically configured
    * 2. PTBR highest bits enabled
    *    see RV-priv spec 4.1.11 Supervisor Address Translation and Protection (satp) Register
    *    RV64:          RV32:
    *    0xxx           0
    *    1000 -> SV39   1 -> SV32
    *    1001 -> SV48
    *    1010 -> SV57
    *    1011 -> SV64
    * 3. priv mode in U and S.
    * 4. in H & M mode, disable VM.
    * 5. no passthrough(micro-arch defined.)
    */
  val vm_enabled = Bool(usingVM) && io.ptw.ptbr.mode(io.ptw.ptbr.mode.getWidth-1) && priv_uses_vm && !io.req.bits.passthrough

  // share a single physical memory attribute checker (unshare if critical path)
  val refill_ppn = io.ptw.resp.bits.pte.ppn(ppnBits-1, 0)
  val do_refill = Bool(usingVM) && io.ptw.resp.valid
  /** 1? will invalidate refill.
    * refill but invalidated?
    */
  val invalidate_refill = state.isOneOf(s_request /* don't care */, s_wait_invalidate) || io.sfence.valid
  /** todo: special entry is empty -> use vaddr */
  val mpu_ppn = Mux(do_refill, refill_ppn,
                Mux(vm_enabled && special_entry.nonEmpty, special_entry.map(e => e.ppn(vpn, e.getData(vpn))).getOrElse(0.U), io.req.bits.vaddr >> pgIdxBits))
  val mpu_physaddr = Cat(mpu_ppn, io.req.bits.vaddr(pgIdxBits-1, 0))
  /** 3 widths: */
  val mpu_priv = Mux[UInt](Bool(usingVM) && (do_refill || io.req.bits.passthrough /* PTW */), PRV.S, Cat(io.ptw.status.debug, priv))

  // TODO: PMP
  val pmp = Module(new PMPChecker(lgMaxSize))
  pmp.io.addr := mpu_physaddr
  pmp.io.size := io.req.bits.size
  pmp.io.pmp := (io.ptw.pmp: Seq[PMP])
  pmp.io.prv := mpu_priv

  // PMA
  // check exist a slave can consume this address.
  val legal_address = edge.manager.findSafe(mpu_physaddr).reduce(_||_)
  // check utility to help check SoC property.
  def fastCheck(member: TLManagerParameters => Boolean) =
    legal_address && edge.manager.fastProperty(mpu_physaddr, member, (b:Boolean) => Bool(b))
  // todo: using DataScratchpad doesn't support cacheable.
  val cacheable = fastCheck(_.supportsAcquireB) && (instruction || !usingDataScratchpad)
  // todo: I guess: this page can be covered by a TLB entry.
  val homogeneous = TLBPageLookup(edge.manager.managers, xLen, p(CacheBlockBytes), BigInt(1) << pgIdxBits)(mpu_physaddr).homogeneous
  // In M mode, if access DM address(debug module program buffer)
  val deny_access_to_debug = mpu_priv <= PRV.M && p(DebugModuleKey).map(dmp => dmp.address.contains(mpu_physaddr)).getOrElse(false)
  val prot_r = fastCheck(_.supportsGet) && !deny_access_to_debug && pmp.io.r
  val prot_w = fastCheck(_.supportsPutFull) && !deny_access_to_debug && pmp.io.w
  val prot_pp = fastCheck(_.supportsPutPartial)
  val prot_al = fastCheck(_.supportsLogical)
  val prot_aa = fastCheck(_.supportsArithmetic)
  val prot_x = fastCheck(_.executable) && !deny_access_to_debug && pmp.io.x
  val prot_eff = fastCheck(Seq(RegionType.PUT_EFFECTS, RegionType.GET_EFFECTS) contains _.regionType)

  // query sets sectored_entries(memIdx)
  // in each sets, use VPN to query hit or not.
  val sector_hits = sectored_entries(memIdx).map(_.sectorHit(vpn))
  val superpage_hits = superpage_entries.map(_.hit(vpn))
  val hitsVec = all_entries.map(vm_enabled && _.hit(vpn))
  val real_hits = hitsVec.asUInt
  // todo: the MSB seems to have special propose? For
  val hits = Cat(!vm_enabled, real_hits)

  // permission bit arrays
  // Refill
  when (do_refill) {
    val pte = io.ptw.resp.bits.pte
    val newEntry = Wire(new TLBEntryData)
    newEntry.ppn := pte.ppn
    newEntry.c := cacheable
    newEntry.u := pte.u
    newEntry.g := pte.g && pte.v
    newEntry.ae := io.ptw.resp.bits.ae
    newEntry.sr := pte.sr()
    newEntry.sw := pte.sw()
    newEntry.sx := pte.sx()
    newEntry.pr := prot_r
    newEntry.pw := prot_w
    newEntry.px := prot_x
    newEntry.ppp := prot_pp
    newEntry.pal := prot_al
    newEntry.paa := prot_aa
    newEntry.eff := prot_eff
    newEntry.fragmented_superpage := io.ptw.resp.bits.fragmented_superpage

    when (special_entry.nonEmpty && !io.ptw.resp.bits.homogeneous) {
      special_entry.foreach { e =>
        e.insert(r_refill_tag, io.ptw.resp.bits.level, newEntry)
        when (invalidate_refill) { e.invalidate() }
      }
    }.elsewhen (io.ptw.resp.bits.level < pgLevels-1) {
      for ((e, i) <- superpage_entries.zipWithIndex) when (r_superpage_repl_addr === i) {
        e.insert(r_refill_tag, io.ptw.resp.bits.level, newEntry)
        when (invalidate_refill) { e.invalidate() }
      }
    }.otherwise {
      val r_memIdx = r_refill_tag.extract(cfg.nSectors.log2 + cfg.nSets.log2 - 1, cfg.nSectors.log2)
      val waddr = Mux(r_sectored_hit, r_sectored_hit_addr, r_sectored_repl_addr)
      for ((e, i) <- sectored_entries(r_memIdx).zipWithIndex) when (waddr === i) {
        when (!r_sectored_hit) { e.invalidate() }
        e.insert(r_refill_tag, 0.U, newEntry)
        when (invalidate_refill) { e.invalidate() }
      }
    }
  }

  // get all entries.
  val entries = all_entries.map(_.getData(vpn))
  val normal_entries = entries.take(ordinary_entries.size)
  // parallel query PPN from [[all_entries]], if VM not enabled return VPN instead
  val ppn = Mux1H(hitsVec :+ !vm_enabled, (all_entries zip entries).map{ case (entry, data) => entry.ppn(vpn, data) } :+ vpn(ppnBits-1, 0))

  // todo: sepcial_entry
  val nPhysicalEntries = 1 + special_entry.size
  // generally PTW misaligned load exception.
  val ptw_ae_array = Cat(false.B, entries.map(_.ae).asUInt)
  // if in hypervisor/machine mode, cannot read/write user entries.
  // if in superviosr/user mode, "If the SUM bit in the sstatus register is set, supervisor mode software may also access pages with U=1.(from spec)"
  val priv_rw_ok = Mux(!priv_s || io.ptw.status.sum, entries.map(_.u).asUInt, 0.U) | Mux(priv_s, ~entries.map(_.u).asUInt, 0.U)
  // if in hypervisor/machine mode, other than user pages, all pages are executable.
  // if in superviosr/user mode, only user page can execute.
  // TODO: how about G?
  val priv_x_ok = Mux(priv_s, ~entries.map(_.u).asUInt, entries.map(_.u).asUInt)
  // "The vsstatus field MXR, which makes execute-only pages readable, only overrides VS-stage page protection.(from spec)"
  val r_array = Cat(true.B, priv_rw_ok & (entries.map(_.sr).asUInt | Mux(io.ptw.status.mxr, entries.map(_.sx).asUInt, UInt(0))))
  val w_array = Cat(true.B, priv_rw_ok & entries.map(_.sw).asUInt)
  val x_array = Cat(true.B, priv_x_ok & entries.map(_.sx).asUInt)

  // These array is for each TLB entries.
  // user mode can read: PMA OK, TLB OK, AE OK
  val pr_array = Cat(Fill(nPhysicalEntries, prot_r), normal_entries.map(_.pr).asUInt) & ~ptw_ae_array
  // user mode can write: PMA OK, TLB OK, AE OK
  val pw_array = Cat(Fill(nPhysicalEntries, prot_w), normal_entries.map(_.pw).asUInt) & ~ptw_ae_array
  // user mode can write: PMA OK, TLB OK, AE OK
  val px_array = Cat(Fill(nPhysicalEntries, prot_x), normal_entries.map(_.px).asUInt) & ~ptw_ae_array
  // put effect
  val eff_array = Cat(Fill(nPhysicalEntries, prot_eff), normal_entries.map(_.eff).asUInt)
  // cacheable
  val c_array = Cat(Fill(nPhysicalEntries, cacheable), normal_entries.map(_.c).asUInt)
  // put partial
  val ppp_array = Cat(Fill(nPhysicalEntries, prot_pp), normal_entries.map(_.ppp).asUInt)
  // atomic arithmetic
  val paa_array = Cat(Fill(nPhysicalEntries, prot_aa), normal_entries.map(_.paa).asUInt)
  // atomic logic
  val pal_array = Cat(Fill(nPhysicalEntries, prot_al), normal_entries.map(_.pal).asUInt)
  val ppp_array_if_cached = ppp_array | c_array
  val paa_array_if_cached = paa_array | Mux(usingAtomicsInCache, c_array, 0.U)
  val pal_array_if_cached = pal_array | Mux(usingAtomicsInCache, c_array, 0.U)
  val prefetchable_array = Cat((cacheable && homogeneous) << (nPhysicalEntries-1), normal_entries.map(_.c).asUInt)

  val misaligned = (io.req.bits.vaddr & (UIntToOH(io.req.bits.size) - 1)).orR
  val bad_va = if (!usingVM || (minPgLevels == pgLevels && vaddrBits == vaddrBitsExtended)) false.B else vm_enabled && {
    val nPgLevelChoices = pgLevels - minPgLevels + 1
    val minVAddrBits = pgIdxBits + minPgLevels * pgLevelBits
    (for (i <- 0 until nPgLevelChoices) yield {
      val mask = ((BigInt(1) << vaddrBitsExtended) - (BigInt(1) << (minVAddrBits + i * pgLevelBits - 1))).U
      val maskedVAddr = io.req.bits.vaddr & mask
      io.ptw.ptbr.additionalPgLevels === i && !(maskedVAddr === 0 || maskedVAddr === mask)
    }).orR
  }

  val cmd_lrsc = Bool(usingAtomics) && io.req.bits.cmd.isOneOf(M_XLR, M_XSC)
  val cmd_amo_logical = Bool(usingAtomics) && isAMOLogical(io.req.bits.cmd)
  val cmd_amo_arithmetic = Bool(usingAtomics) && isAMOArithmetic(io.req.bits.cmd)
  val cmd_put_partial = io.req.bits.cmd === M_PWR
  val cmd_read = isRead(io.req.bits.cmd)
  val cmd_write = isWrite(io.req.bits.cmd)
  val cmd_write_perms = cmd_write ||
    io.req.bits.cmd.isOneOf(M_FLUSH_ALL, M_WOK) // not a write, but needs write permissions

  val lrscAllowed = Mux(Bool(usingDataScratchpad || usingAtomicsOnlyForIO), 0.U, c_array)
  val ae_array =
    Mux(misaligned, eff_array, 0.U) |
    Mux(cmd_lrsc, ~lrscAllowed, 0.U)

  // access exception needs SoC information from PMA.
  val ae_ld_array = Mux(cmd_read, ae_array | ~pr_array, 0.U)
  val ae_st_array =
    Mux(cmd_write_perms, ae_array | ~pw_array, 0.U) |
    Mux(cmd_put_partial, ~ppp_array_if_cached, 0.U) |
    Mux(cmd_amo_logical, ~pal_array_if_cached, 0.U) |
    Mux(cmd_amo_arithmetic, ~paa_array_if_cached, 0.U)
  val must_alloc_array =
    Mux(cmd_put_partial, ~ppp_array, 0.U) |
    Mux(cmd_amo_logical, ~paa_array, 0.U) |
    Mux(cmd_amo_arithmetic, ~pal_array, 0.U) |
    Mux(cmd_lrsc, ~0.U(pal_array.getWidth.W), 0.U)
  val ma_ld_array = Mux(misaligned && cmd_read, ~eff_array, 0.U)
  val ma_st_array = Mux(misaligned && cmd_write, ~eff_array, 0.U)
  val pf_ld_array = Mux(cmd_read, ~(r_array | ptw_ae_array), 0.U)
  val pf_st_array = Mux(cmd_write_perms, ~(w_array | ptw_ae_array), 0.U)
  val pf_inst_array = ~(x_array | ptw_ae_array)

  val tlb_hit = real_hits.orR
  val tlb_miss = vm_enabled && !bad_va && !tlb_hit

  val sectored_plru = new SetAssocLRU(cfg.nSets, sectored_entries(0).size, "plru")
  val superpage_plru = new PseudoLRU(superpage_entries.size)
  when (io.req.valid && vm_enabled) {
    when (sector_hits.orR) { sectored_plru.access(memIdx, OHToUInt(sector_hits)) }
    when (superpage_hits.orR) { superpage_plru.access(OHToUInt(superpage_hits)) }
  }

  // Superpages create the possibility that two entries in the TLB may match.
  // This corresponds to a software bug, but we can't return complete garbage;
  // we must return either the old translation or the new translation.  This
  // isn't compatible with the Mux1H approach.  So, flush the TLB and report
  // a miss on duplicate entries.
  val multipleHits = PopCountAtLeast(real_hits, 2)

  // only pull up req.ready when this is s_ready state.
  io.req.ready := state === s_ready

  io.resp.pf.ld := (bad_va && cmd_read) || (pf_ld_array & hits).orR
  io.resp.pf.st := (bad_va && cmd_write_perms) || (pf_st_array & hits).orR
  io.resp.pf.inst := bad_va || (pf_inst_array & hits).orR

  io.resp.ae.ld := (ae_ld_array & hits).orR
  io.resp.ae.st := (ae_st_array & hits).orR
  io.resp.ae.inst := (~px_array & hits).orR

  io.resp.ma.ld := (ma_ld_array & hits).orR
  io.resp.ma.st := (ma_st_array & hits).orR
  io.resp.ma.inst := false // this is up to the pipeline to figure out

  io.resp.cacheable := (c_array & hits).orR
  io.resp.must_alloc := (must_alloc_array & hits).orR
  io.resp.prefetchable := (prefetchable_array & hits).orR && edge.manager.managers.forall(m => !m.supportsAcquireB || m.supportsHint)
  io.resp.miss := do_refill || tlb_miss || multipleHits
  io.resp.paddr := Cat(ppn, io.req.bits.vaddr(pgIdxBits-1, 0))

  io.ptw.req.valid := state === s_request
  io.ptw.req.bits.valid := !io.kill
  io.ptw.req.bits.bits.addr := r_refill_tag

  if (usingVM) {
    val sfence = io.sfence.valid
    // this is [[s_ready]]
    // handle miss/hit at the first cycle.
    // if miss, request next PTW(L2TLB).
    when (io.req.fire() && tlb_miss) {
      state := s_request
      r_refill_tag := vpn

      r_superpage_repl_addr := replacementEntry(superpage_entries, superpage_plru.way)
      r_sectored_repl_addr := replacementEntry(sectored_entries(memIdx), sectored_plru.way(memIdx))
      r_sectored_hit_addr := OHToUInt(sector_hits)
      r_sectored_hit := sector_hits.orR
    }
    // Handle SFENCE.VMA when send request to PTW.
    // SFENCE.VMA    io.ptw.req.ready     kill
    //       ?                 ?            1
    //       0                 0            0
    //       0                 1            0 -> s_wait
    //       1                 0            0 -> s_wait_invalidate
    //       1                 0            0 -> s_ready
    when (state === s_request) {
      // SFENCE.VMA will kill TLB entries based on rs1 and rs2. It will take 1 cycle.
      when (sfence) { state := s_ready }
      // here should be io.ptw.req.fire, but assert(io.ptw.req.ready === true.B)
      // fire -> s_wait
      when (io.ptw.req.ready) { state := Mux(sfence, s_wait_invalidate, s_wait) }
      // If CPU kills request(frontend.s2_redirect)
      when (io.kill) { state := s_ready }
    }
    when (state === s_wait && sfence) {
      state := s_wait_invalidate
    }
    // after CPU acquire the responds, go back to s_ready.
    when (io.ptw.resp.valid) {
      state := s_ready
    }

    // SFENCE processing logic.
    when (sfence) {
      assert(!io.sfence.bits.rs1 || (io.sfence.bits.addr >> pgIdxBits) === vpn)
      for (e <- all_real_entries) {
        when (io.sfence.bits.rs1) { e.invalidateVPN(vpn) }
        .elsewhen (io.sfence.bits.rs2) { e.invalidateNonGlobal() }
        .otherwise { e.invalidate() }
      }
    }
    when (multipleHits || reset) {
      all_real_entries.foreach(_.invalidate())
    }

    ccover(io.ptw.req.fire(), "MISS", "TLB miss")
    ccover(io.ptw.req.valid && !io.ptw.req.ready, "PTW_STALL", "TLB miss, but PTW busy")
    ccover(state === s_wait_invalidate, "SFENCE_DURING_REFILL", "flush TLB during TLB refill")
    ccover(sfence && !io.sfence.bits.rs1 && !io.sfence.bits.rs2, "SFENCE_ALL", "flush TLB")
    ccover(sfence && !io.sfence.bits.rs1 && io.sfence.bits.rs2, "SFENCE_ASID", "flush TLB ASID")
    ccover(sfence && io.sfence.bits.rs1 && !io.sfence.bits.rs2, "SFENCE_LINE", "flush TLB line")
    ccover(sfence && io.sfence.bits.rs1 && io.sfence.bits.rs2, "SFENCE_LINE_ASID", "flush TLB line/ASID")
    ccover(multipleHits, "MULTIPLE_HITS", "Two matching translations in TLB")
  }

  def ccover(cond: Bool, label: String, desc: String)(implicit sourceInfo: SourceInfo) =
    cover(cond, s"${if (instruction) "I" else "D"}TLB_$label", "MemorySystem;;" + desc)

  def replacementEntry(set: Seq[TLBEntry], alt: UInt) = {
    val valids = set.map(_.valid.orR).asUInt
    Mux(valids.andR, alt, PriorityEncoder(~valids))
  }
}
