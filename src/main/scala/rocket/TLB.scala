// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.rocket

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.subsystem.CacheBlockBytes
import freechips.rocketchip.diplomacy.RegionType
import freechips.rocketchip.tile.{CoreModule, CoreBundle}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property
import freechips.rocketchip.devices.debug.DebugModuleKey
import chisel3.experimental.SourceInfo

case object PgLevels extends Field[Int](2)
case object ASIdBits extends Field[Int](0)
case object VMIdBits extends Field[Int](0)

/** =SFENCE=
  * rs1 rs2
  * {{{
  *  0   0 -> flush All
  *  0   1 -> flush by ASID
  *  1   1 -> flush by ADDR
  *  1   0 -> flush by ADDR and ASID
  * }}}
  * {{{
  * If rs1=x0 and rs2=x0, the fence orders all reads and writes made to any level of the page tables, for all address spaces.
  * If rs1=x0 and rs2!=x0, the fence orders all reads and writes made to any level of the page tables, but only for the address space identified by integer register rs2. Accesses to global mappings (see Section 4.3.1) are not ordered.
  * If rs1!=x0 and rs2=x0, the fence orders only reads and writes made to the leaf page table entry corresponding to the virtual address in rs1, for all address spaces.
  * If rs1!=x0 and rs2!=x0, the fence orders only reads and writes made to the leaf page table entry corresponding to the virtual address in rs1, for the address space identified by integer register rs2. Accesses to global mappings are not ordered.
  * }}}
  */
class SFenceReq(implicit p: Parameters) extends CoreBundle()(p) {
  val rs1 = Bool()
  val rs2 = Bool()
  val addr = UInt(vaddrBits.W)
  val asid = UInt((asIdBits max 1).W) // TODO zero-width
  val hv = Bool()
  val hg = Bool()
}

class TLBReq(lgMaxSize: Int)(implicit p: Parameters) extends CoreBundle()(p) {
  /** request address from CPU. */
  val vaddr = UInt(vaddrBitsExtended.W)
  /** don't lookup TLB, bypass vaddr as paddr */
  val passthrough = Bool()
  /** granularity */
  val size = UInt(log2Ceil(lgMaxSize + 1).W)
  /** memory command. */
  val cmd  = Bits(M_SZ.W)
  val prv = UInt(PRV.SZ.W)
  /** virtualization mode */
  val v = Bool()

}

class TLBExceptions extends Bundle {
  val ld = Bool()
  val st = Bool()
  val inst = Bool()
}

class TLBResp(implicit p: Parameters) extends CoreBundle()(p) {
  // lookup responses
  val miss = Bool()
  /** physical address */
  val paddr = UInt(paddrBits.W)
  val gpa = UInt(vaddrBitsExtended.W)
  val gpa_is_pte = Bool()
  /** page fault exception */
  val pf = new TLBExceptions
  /** guest page fault exception */
  val gf = new TLBExceptions
  /** access exception */
  val ae = new TLBExceptions
  /** misaligned access exception */
  val ma = new TLBExceptions
  /** if this address is cacheable */
  val cacheable = Bool()
  /** if caches must allocate this address */
  val must_alloc = Bool()
  /** if this address is prefetchable for caches*/
  val prefetchable = Bool()
}

class TLBEntryData(implicit p: Parameters) extends CoreBundle()(p) {
  val ppn = UInt(ppnBits.W)
  /** pte.u user */
  val u = Bool()
  /** pte.g global */
  val g = Bool()
  /** access exception.
    * D$ -> PTW -> TLB AE
    * Alignment failed.
    */
  val ae_ptw = Bool()
  val ae_final = Bool()
  val ae_stage2 = Bool()
  /** page fault */
  val pf = Bool()
  /** guest page fault */
  val gf = Bool()
  /** supervisor write */
  val sw = Bool()
  /** supervisor execute */
  val sx = Bool()
  /** supervisor read */
  val sr = Bool()
  /** hypervisor write */
  val hw = Bool()
  /** hypervisor excute */
  val hx = Bool()
  /** hypervisor read */
  val hr = Bool()
  /** prot_w */
  val pw = Bool()
  /** prot_x */
  val px = Bool()
  /** prot_r */
  val pr = Bool()

  /** PutPartial */
  val ppp = Bool()
  /** AMO logical */
  val pal = Bool()
  /** AMO arithmetic */
  val paa = Bool()
  /** get/put effects */
  val eff = Bool()
  /** cacheable */
  val c = Bool()
  /** fragmented_superpage support */
  val fragmented_superpage = Bool()
}

/** basic cell for TLB data */
class TLBEntry(val nSectors: Int, val superpage: Boolean, val superpageOnly: Boolean)(implicit p: Parameters) extends CoreBundle()(p) {
  require(nSectors == 1 || !superpage)
  require(!superpageOnly || superpage)

  val level = UInt(log2Ceil(pgLevels).W)
  /** use vpn as tag */
  val tag_vpn = UInt(vpnBits.W)
  /** tag in vitualization mode */
  val tag_v = Bool()
  /** entry data */
  val data = Vec(nSectors, UInt(new TLBEntryData().getWidth.W))
  /** valid bit */
  val valid = Vec(nSectors, Bool())
  /** returns all entry data in this entry */
  def entry_data = data.map(_.asTypeOf(new TLBEntryData))
  /** returns the index of sector */
  private def sectorIdx(vpn: UInt) = vpn.extract(nSectors.log2-1, 0)
  /** returns the entry data matched with this vpn*/
  def getData(vpn: UInt) = OptimizationBarrier(data(sectorIdx(vpn)).asTypeOf(new TLBEntryData))
  /** returns whether a sector hits */
  def sectorHit(vpn: UInt, virtual: Bool) = valid.orR && sectorTagMatch(vpn, virtual)
  /** returns whether tag matches vpn */
  def sectorTagMatch(vpn: UInt, virtual: Bool) = (((tag_vpn ^ vpn) >> nSectors.log2) === 0.U) && (tag_v === virtual)
  /** returns hit signal */
  def hit(vpn: UInt, virtual: Bool): Bool = {
    if (superpage && usingVM) {
      var tagMatch = valid.head && (tag_v === virtual)
      for (j <- 0 until pgLevels) {
        val base = (pgLevels - 1 - j) * pgLevelBits
        val n = pgLevelBits + (if (j == 0) hypervisorExtraAddrBits else 0)
        val ignore = level < j.U || (superpageOnly && j == pgLevels - 1).B
        tagMatch = tagMatch && (ignore || (tag_vpn ^ vpn)(base + n - 1, base) === 0.U)
      }
      tagMatch
    } else {
      val idx = sectorIdx(vpn)
      valid(idx) && sectorTagMatch(vpn, virtual)
    }
  }
  /** returns the ppn of the input TLBEntryData */
  def ppn(vpn: UInt, data: TLBEntryData) = {
    val supervisorVPNBits = pgLevels * pgLevelBits
    if (superpage && usingVM) {
      var res = data.ppn >> pgLevelBits*(pgLevels - 1)
      for (j <- 1 until pgLevels) {
        val ignore = level < j.U || (superpageOnly && j == pgLevels - 1).B
        res = Cat(res, (Mux(ignore, vpn, 0.U) | data.ppn)(supervisorVPNBits - j*pgLevelBits - 1, supervisorVPNBits - (j + 1)*pgLevelBits))
      }
      res
    } else {
      data.ppn
    }
  }
  /** does the refill
    *
    * find the target entry with vpn tag
    * and replace the target entry with the input entry data
    */
  def insert(vpn: UInt, virtual: Bool, level: UInt, entry: TLBEntryData): Unit = {
    this.tag_vpn := vpn
    this.tag_v := virtual
    this.level := level.extract(log2Ceil(pgLevels - superpageOnly.toInt)-1, 0)

    val idx = sectorIdx(vpn)
    valid(idx) := true.B
    data(idx) := entry.asUInt
  }

  def invalidate(): Unit = { valid.foreach(_ := false.B) }
  def invalidate(virtual: Bool): Unit = {
    for ((v, e) <- valid zip entry_data)
      when (tag_v === virtual) { v := false.B }
  }
  def invalidateVPN(vpn: UInt, virtual: Bool): Unit = {
    if (superpage) {
      when (hit(vpn, virtual)) { invalidate() }
    } else {
      when (sectorTagMatch(vpn, virtual)) {
        for (((v, e), i) <- (valid zip entry_data).zipWithIndex)
          when (tag_v === virtual && i.U === sectorIdx(vpn)) { v := false.B }
      }
    }
    // For fragmented superpage mappings, we assume the worst (largest)
    // case, and zap entries whose most-significant VPNs match
    when (((tag_vpn ^ vpn) >> (pgLevelBits * (pgLevels - 1))) === 0.U) {
      for ((v, e) <- valid zip entry_data)
        when (tag_v === virtual && e.fragmented_superpage) { v := false.B }
    }
  }
  def invalidateNonGlobal(virtual: Bool): Unit = {
    for ((v, e) <- valid zip entry_data)
      when (tag_v === virtual && !e.g) { v := false.B }
  }
}

/** TLB config
  *
  * @param nSets the number of sets of PTE, follow [[ICacheParams.nSets]]
  * @param nWays the total number of wayss of PTE, follow [[ICacheParams.nWays]]
  * @param nSectors the number of ways in a single PTE TLBEntry
  * @param nSuperpageEntries the number of SuperpageEntries
  */
case class TLBConfig(
    nSets: Int,
    nWays: Int,
    nSectors: Int = 4,
    nSuperpageEntries: Int = 4)

/** =Overview=
  * [[TLB]] is a TLB template which contains PMA logic and PMP checker.
  *
  * TLB caches PTE and accelerates the address translation process.
  * When tlb miss happens, ask PTW(L2TLB) for Page Table Walk.
  * Perform PMP and PMA check during the translation and throw exception if there were any.
  *
  *  ==Cache Structure==
  *  - Sectored Entry (PTE)
  *   - set-associative or direct-mapped
  *    - nsets = [[TLBConfig.nSets]]
  *    - nways = [[TLBConfig.nWays]] / [[TLBConfig.nSectors]]
  *    - PTEEntry( sectors = [[TLBConfig.nSectors]] )
  *   - LRU(if set-associative)
  *
  *  - Superpage Entry(superpage PTE)
  *   - fully associative
  *    - nsets = [[TLBConfig.nSuperpageEntries]]
  *    - PTEEntry(sectors = 1)
  *   - PseudoLRU
  *
  *  - Special Entry(PTE across PMP)
  *   - nsets = 1
  *   - PTEEntry(sectors = 1)
  *
  * ==Address structure==
  * {{{
  * |vaddr                                                 |
  * |ppn/vpn                                   | pgIndex   |
  * |                                          |           |
  * |           |nSets             |nSector    |           |}}}
  *
  * ==State Machine==
  * {{{
  * s_ready: ready to accept request from CPU.
  * s_request: when L1TLB(this) miss, send request to PTW(L2TLB), .
  * s_wait: wait for PTW to refill L1TLB.
  * s_wait_invalidate: L1TLB is waiting for respond from PTW, but L1TLB will invalidate respond from PTW.}}}
  *
  * ==PMP==
  * pmp check
  *  - special_entry: always check
  *  - other entry: check on refill
  *
  * ==Note==
  * PMA consume diplomacy parameter generate physical memory address checking logic
  *
  * Boom use Rocket ITLB, and its own DTLB.
  *
  * Accelerators:{{{
  *   sha3: DTLB
  *   gemmini: DTLB
  *   hwacha: DTLB*2+ITLB}}}
  * @param instruction true for ITLB, false for DTLB
  * @param lgMaxSize @todo seems granularity
  * @param cfg [[TLBConfig]]
  * @param edge collect SoC metadata.
  */
class TLB(instruction: Boolean, lgMaxSize: Int, cfg: TLBConfig)(implicit edge: TLEdgeOut, p: Parameters) extends CoreModule()(p) {
  val io = IO(new Bundle {
    /** request from Core */
    val req = Flipped(Decoupled(new TLBReq(lgMaxSize)))
    /** response to Core */
    val resp = Output(new TLBResp())
    /** SFence Input */
    val sfence = Flipped(Valid(new SFenceReq))
    /** IO to PTW */
    val ptw = new TLBPTWIO
    /** suppress a TLB refill, one cycle after a miss */
    val kill = Input(Bool())
  })
  io.ptw.customCSRs := DontCare

  val pageGranularityPMPs = pmpGranularity >= (1 << pgIdxBits)
  val vpn = io.req.bits.vaddr(vaddrBits-1, pgIdxBits)
  /** index for sectored_Entry */
  val memIdx = vpn.extract(cfg.nSectors.log2 + cfg.nSets.log2 - 1, cfg.nSectors.log2)
  /** TLB Entry */
  val sectored_entries = Reg(Vec(cfg.nSets, Vec(cfg.nWays / cfg.nSectors, new TLBEntry(cfg.nSectors, false, false))))
  /** Superpage Entry */
  val superpage_entries = Reg(Vec(cfg.nSuperpageEntries, new TLBEntry(1, true, true)))
  /** Special Entry
    *
    * If PMP granularity is less than page size, thus need additional "special" entry manage PMP.
    */
  val special_entry = (!pageGranularityPMPs).option(Reg(new TLBEntry(1, true, false)))
  def ordinary_entries = sectored_entries(memIdx) ++ superpage_entries
  def all_entries = ordinary_entries ++ special_entry
  def all_real_entries = sectored_entries.flatten ++ superpage_entries ++ special_entry

  val s_ready :: s_request :: s_wait :: s_wait_invalidate :: Nil = Enum(4)
  val state = RegInit(s_ready)
  // use vpn as refill_tag
  val r_refill_tag = Reg(UInt(vpnBits.W))
  val r_superpage_repl_addr = Reg(UInt(log2Ceil(superpage_entries.size).W))
  val r_sectored_repl_addr = Reg(UInt(log2Ceil(sectored_entries.head.size).W))
  val r_sectored_hit = Reg(Valid(UInt(log2Ceil(sectored_entries.head.size).W)))
  val r_superpage_hit = Reg(Valid(UInt(log2Ceil(superpage_entries.size).W)))
  val r_vstage1_en = Reg(Bool())
  val r_stage2_en = Reg(Bool())
  val r_need_gpa = Reg(Bool())
  val r_gpa_valid = Reg(Bool())
  val r_gpa = Reg(UInt(vaddrBits.W))
  val r_gpa_vpn = Reg(UInt(vpnBits.W))
  val r_gpa_is_pte = Reg(Bool())

  /** privilege mode */
  val priv = io.req.bits.prv
  val priv_v = usingHypervisor.B && io.req.bits.v
  val priv_s = priv(0)
  // user mode and supervisor mode
  val priv_uses_vm = priv <= PRV.S.U
  val satp = Mux(priv_v, io.ptw.vsatp, io.ptw.ptbr)
  val stage1_en = usingVM.B && satp.mode(satp.mode.getWidth-1)
  /** VS-stage translation enable */
  val vstage1_en = usingHypervisor.B && priv_v && io.ptw.vsatp.mode(io.ptw.vsatp.mode.getWidth-1)
  /** G-stage translation enable */
  val stage2_en  = usingHypervisor.B && priv_v && io.ptw.hgatp.mode(io.ptw.hgatp.mode.getWidth-1)
  /** Enable Virtual Memory when:
    *  1. statically configured
    *  1. satp highest bits enabled
    *   i. RV32:
    *     - 0 -> Bare
    *     - 1 -> SV32
    *   i. RV64:
    *     - 0000 -> Bare
    *     - 1000 -> SV39
    *     - 1001 -> SV48
    *     - 1010 -> SV57
    *     - 1011 -> SV64
    *  1. In virtualization mode, vsatp highest bits enabled
    *  1. priv mode in U and S.
    *  1. in H & M mode, disable VM.
    *  1. no passthrough(micro-arch defined.)
    *
    * @see RV-priv spec 4.1.11 Supervisor Address Translation and Protection (satp) Register
    * @see RV-priv spec 8.2.18 Virtual Supervisor Address Translation and Protection Register (vsatp)
    */
  val vm_enabled = (stage1_en || stage2_en) && priv_uses_vm && !io.req.bits.passthrough

  // flush guest entries on vsatp.MODE Bare <-> SvXX transitions
  val v_entries_use_stage1 = RegInit(false.B)
  val vsatp_mode_mismatch  = priv_v && (vstage1_en =/= v_entries_use_stage1) && !io.req.bits.passthrough

  // share a single physical memory attribute checker (unshare if critical path)
  val refill_ppn = io.ptw.resp.bits.pte.ppn(ppnBits-1, 0)
  /** refill signal */
  val do_refill = usingVM.B && io.ptw.resp.valid
  /** sfence invalidate refill */
  val invalidate_refill = state.isOneOf(s_request /* don't care */, s_wait_invalidate) || io.sfence.valid
  // PMP
  val mpu_ppn = Mux(do_refill, refill_ppn,
                Mux(vm_enabled && special_entry.nonEmpty.B, special_entry.map(e => e.ppn(vpn, e.getData(vpn))).getOrElse(0.U), io.req.bits.vaddr >> pgIdxBits))
  val mpu_physaddr = Cat(mpu_ppn, io.req.bits.vaddr(pgIdxBits-1, 0))
  val mpu_priv = Mux[UInt](usingVM.B && (do_refill || io.req.bits.passthrough /* PTW */), PRV.S.U, Cat(io.ptw.status.debug, priv))
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
    legal_address && edge.manager.fastProperty(mpu_physaddr, member, (b:Boolean) => b.B)
  // todo: using DataScratchpad doesn't support cacheable.
  val cacheable = fastCheck(_.supportsAcquireB) && (instruction || !usingDataScratchpad).B
  val homogeneous = TLBPageLookup(edge.manager.managers, xLen, p(CacheBlockBytes), BigInt(1) << pgIdxBits)(mpu_physaddr).homogeneous
  // In M mode, if access DM address(debug module program buffer)
  val deny_access_to_debug = mpu_priv <= PRV.M.U && p(DebugModuleKey).map(dmp => dmp.address.contains(mpu_physaddr)).getOrElse(false.B)
  val prot_r = fastCheck(_.supportsGet) && !deny_access_to_debug && pmp.io.r
  val prot_w = fastCheck(_.supportsPutFull) && !deny_access_to_debug && pmp.io.w
  val prot_pp = fastCheck(_.supportsPutPartial)
  val prot_al = fastCheck(_.supportsLogical)
  val prot_aa = fastCheck(_.supportsArithmetic)
  val prot_x = fastCheck(_.executable) && !deny_access_to_debug && pmp.io.x
  val prot_eff = fastCheck(Seq(RegionType.PUT_EFFECTS, RegionType.GET_EFFECTS) contains _.regionType)

  // hit check
  val sector_hits = sectored_entries(memIdx).map(_.sectorHit(vpn, priv_v))
  val superpage_hits = superpage_entries.map(_.hit(vpn, priv_v))
  val hitsVec = all_entries.map(vm_enabled && _.hit(vpn, priv_v))
  val real_hits = hitsVec.asUInt
  val hits = Cat(!vm_enabled, real_hits)

  // use ptw response to refill
  // permission bit arrays
  when (do_refill) {
    val pte = io.ptw.resp.bits.pte
    val refill_v = r_vstage1_en || r_stage2_en
    val newEntry = Wire(new TLBEntryData)
    newEntry.ppn := pte.ppn
    newEntry.c := cacheable
    newEntry.u := pte.u
    newEntry.g := pte.g && pte.v
    newEntry.ae_ptw := io.ptw.resp.bits.ae_ptw
    newEntry.ae_final := io.ptw.resp.bits.ae_final
    newEntry.ae_stage2 := io.ptw.resp.bits.ae_final && io.ptw.resp.bits.gpa_is_pte && r_stage2_en
    newEntry.pf := io.ptw.resp.bits.pf
    newEntry.gf := io.ptw.resp.bits.gf
    newEntry.hr := io.ptw.resp.bits.hr
    newEntry.hw := io.ptw.resp.bits.hw
    newEntry.hx := io.ptw.resp.bits.hx
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
    // refill special_entry
    when (special_entry.nonEmpty.B && !io.ptw.resp.bits.homogeneous) {
      special_entry.foreach(_.insert(r_refill_tag, refill_v, io.ptw.resp.bits.level, newEntry))
    }.elsewhen (io.ptw.resp.bits.level < (pgLevels-1).U) {
      val waddr = Mux(r_superpage_hit.valid && usingHypervisor.B, r_superpage_hit.bits, r_superpage_repl_addr)
      for ((e, i) <- superpage_entries.zipWithIndex) when (r_superpage_repl_addr === i.U) {
        e.insert(r_refill_tag, refill_v, io.ptw.resp.bits.level, newEntry)
        when (invalidate_refill) { e.invalidate() }
      }
    // refill sectored_hit
    }.otherwise {
      val r_memIdx = r_refill_tag.extract(cfg.nSectors.log2 + cfg.nSets.log2 - 1, cfg.nSectors.log2)
      val waddr = Mux(r_sectored_hit.valid, r_sectored_hit.bits, r_sectored_repl_addr)
      for ((e, i) <- sectored_entries(r_memIdx).zipWithIndex) when (waddr === i.U) {
        when (!r_sectored_hit.valid) { e.invalidate() }
        e.insert(r_refill_tag, refill_v, 0.U, newEntry)
        when (invalidate_refill) { e.invalidate() }
      }
    }

    r_gpa_valid := io.ptw.resp.bits.gpa.valid
    r_gpa := io.ptw.resp.bits.gpa.bits
    r_gpa_is_pte := io.ptw.resp.bits.gpa_is_pte
  }

  // get all entries data.
  val entries = all_entries.map(_.getData(vpn))
  val normal_entries = entries.take(ordinary_entries.size)
  // parallel query PPN from [[all_entries]], if VM not enabled return VPN instead
  val ppn = Mux1H(hitsVec :+ !vm_enabled, (all_entries zip entries).map{ case (entry, data) => entry.ppn(vpn, data) } :+ vpn(ppnBits-1, 0))

  val nPhysicalEntries = 1 + special_entry.size
  // generally PTW misaligned load exception.
  val ptw_ae_array = Cat(false.B, entries.map(_.ae_ptw).asUInt)
  val final_ae_array = Cat(false.B, entries.map(_.ae_final).asUInt)
  val ptw_pf_array = Cat(false.B, entries.map(_.pf).asUInt)
  val ptw_gf_array = Cat(false.B, entries.map(_.gf).asUInt)
  val sum = Mux(priv_v, io.ptw.gstatus.sum, io.ptw.status.sum)
  // if in hypervisor/machine mode, cannot read/write user entries.
  // if in superviosr/user mode, "If the SUM bit in the sstatus register is set, supervisor mode software may also access pages with U=1.(from spec)"
  val priv_rw_ok = Mux(!priv_s || sum, entries.map(_.u).asUInt, 0.U) | Mux(priv_s, ~entries.map(_.u).asUInt, 0.U)
  // if in hypervisor/machine mode, other than user pages, all pages are executable.
  // if in superviosr/user mode, only user page can execute.
  val priv_x_ok = Mux(priv_s, ~entries.map(_.u).asUInt, entries.map(_.u).asUInt)
  val stage1_bypass = Fill(entries.size, usingHypervisor.B) & (Fill(entries.size, !stage1_en) | entries.map(_.ae_stage2).asUInt)
  val mxr = io.ptw.status.mxr | Mux(priv_v, io.ptw.gstatus.mxr, false.B)
  // "The vsstatus field MXR, which makes execute-only pages readable, only overrides VS-stage page protection.(from spec)"
  val r_array = Cat(true.B, (priv_rw_ok & (entries.map(_.sr).asUInt | Mux(mxr, entries.map(_.sx).asUInt, 0.U))) | stage1_bypass)
  val w_array = Cat(true.B, (priv_rw_ok & entries.map(_.sw).asUInt) | stage1_bypass)
  val x_array = Cat(true.B, (priv_x_ok & entries.map(_.sx).asUInt) | stage1_bypass)
  val stage2_bypass = Fill(entries.size, !stage2_en)
  val hr_array = Cat(true.B, entries.map(_.hr).asUInt | Mux(io.ptw.status.mxr, entries.map(_.hx).asUInt, 0.U) | stage2_bypass)
  val hw_array = Cat(true.B, entries.map(_.hw).asUInt | stage2_bypass)
  val hx_array = Cat(true.B, entries.map(_.hx).asUInt | stage2_bypass)
  // These array is for each TLB entries.
  // user mode can read: PMA OK, TLB OK, AE OK
  val pr_array = Cat(Fill(nPhysicalEntries, prot_r), normal_entries.map(_.pr).asUInt) & ~(ptw_ae_array | final_ae_array)
  // user mode can write: PMA OK, TLB OK, AE OK
  val pw_array = Cat(Fill(nPhysicalEntries, prot_w), normal_entries.map(_.pw).asUInt) & ~(ptw_ae_array | final_ae_array)
  // user mode can write: PMA OK, TLB OK, AE OK
  val px_array = Cat(Fill(nPhysicalEntries, prot_x), normal_entries.map(_.px).asUInt) & ~(ptw_ae_array | final_ae_array)
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
  val paa_array_if_cached = paa_array | (if(usingAtomicsInCache) c_array else 0.U)
  val pal_array_if_cached = pal_array | (if(usingAtomicsInCache) c_array else 0.U)
  val prefetchable_array = Cat((cacheable && homogeneous) << (nPhysicalEntries-1), normal_entries.map(_.c).asUInt)

  // vaddr misaligned: vaddr[1:0]=b00
  val misaligned = (io.req.bits.vaddr & (UIntToOH(io.req.bits.size) - 1.U)).orR
  def badVA(guestPA: Boolean): Bool = {
    val additionalPgLevels = (if (guestPA) io.ptw.hgatp else satp).additionalPgLevels
    val extraBits = if (guestPA) hypervisorExtraAddrBits else 0
    val signed = !guestPA
    val nPgLevelChoices = pgLevels - minPgLevels + 1
    val minVAddrBits = pgIdxBits + minPgLevels * pgLevelBits + extraBits
    (for (i <- 0 until nPgLevelChoices) yield {
      val mask = ((BigInt(1) << vaddrBitsExtended) - (BigInt(1) << (minVAddrBits + i * pgLevelBits - signed.toInt))).U
      val maskedVAddr = io.req.bits.vaddr & mask
      additionalPgLevels === i.U && !(maskedVAddr === 0.U || signed.B && maskedVAddr === mask)
    }).orR
  }
  val bad_gpa =
    if (!usingHypervisor) false.B
    else vm_enabled && !stage1_en && badVA(true)
  val bad_va =
    if (!usingVM || (minPgLevels == pgLevels && vaddrBits == vaddrBitsExtended)) false.B
    else vm_enabled && stage1_en && badVA(false)

  val cmd_lrsc = usingAtomics.B && io.req.bits.cmd.isOneOf(M_XLR, M_XSC)
  val cmd_amo_logical = usingAtomics.B && isAMOLogical(io.req.bits.cmd)
  val cmd_amo_arithmetic = usingAtomics.B && isAMOArithmetic(io.req.bits.cmd)
  val cmd_put_partial = io.req.bits.cmd === M_PWR
  val cmd_read = isRead(io.req.bits.cmd)
  val cmd_readx = usingHypervisor.B && io.req.bits.cmd === M_HLVX
  val cmd_write = isWrite(io.req.bits.cmd)
  val cmd_write_perms = cmd_write ||
    io.req.bits.cmd.isOneOf(M_FLUSH_ALL, M_WOK) // not a write, but needs write permissions

  val lrscAllowed = Mux((usingDataScratchpad || usingAtomicsOnlyForIO).B, 0.U, c_array)
  val ae_array =
    Mux(misaligned, eff_array, 0.U) |
    Mux(cmd_lrsc, ~lrscAllowed, 0.U)

  // access exception needs SoC information from PMA
  val ae_ld_array = Mux(cmd_read, ae_array | ~pr_array, 0.U)
  val ae_st_array =
    Mux(cmd_write_perms, ae_array | ~pw_array, 0.U) |
    Mux(cmd_put_partial, ~ppp_array_if_cached, 0.U) |
    Mux(cmd_amo_logical, ~pal_array_if_cached, 0.U) |
    Mux(cmd_amo_arithmetic, ~paa_array_if_cached, 0.U)
  val must_alloc_array =
    Mux(cmd_put_partial, ~ppp_array, 0.U) |
    Mux(cmd_amo_logical, ~pal_array, 0.U) |
    Mux(cmd_amo_arithmetic, ~paa_array, 0.U) |
    Mux(cmd_lrsc, ~0.U(pal_array.getWidth.W), 0.U)
  val pf_ld_array = Mux(cmd_read, ((~Mux(cmd_readx, x_array, r_array) & ~ptw_ae_array) | ptw_pf_array) & ~ptw_gf_array, 0.U)
  val pf_st_array = Mux(cmd_write_perms, ((~w_array & ~ptw_ae_array) | ptw_pf_array) & ~ptw_gf_array, 0.U)
  val pf_inst_array = ((~x_array & ~ptw_ae_array) | ptw_pf_array) & ~ptw_gf_array
  val gf_ld_array = Mux(priv_v && cmd_read, ~Mux(cmd_readx, hx_array, hr_array) & ~ptw_ae_array, 0.U)
  val gf_st_array = Mux(priv_v && cmd_write_perms, ~hw_array & ~ptw_ae_array, 0.U)
  val gf_inst_array = Mux(priv_v, ~hx_array & ~ptw_ae_array, 0.U)

  val gpa_hits = {
    val need_gpa_mask = if (instruction) gf_inst_array else gf_ld_array | gf_st_array
    val hit_mask = Fill(ordinary_entries.size, r_gpa_valid && r_gpa_vpn === vpn) | Fill(all_entries.size, !vstage1_en)
    hit_mask | ~need_gpa_mask(all_entries.size-1, 0)
  }

  val tlb_hit_if_not_gpa_miss = real_hits.orR
  val tlb_hit = (real_hits & gpa_hits).orR
  // leads to s_request
  val tlb_miss = vm_enabled && !vsatp_mode_mismatch && !bad_va && !tlb_hit

  val sectored_plru = new SetAssocLRU(cfg.nSets, sectored_entries.head.size, "plru")
  val superpage_plru = new PseudoLRU(superpage_entries.size)
  when (io.req.valid && vm_enabled) {
    // replace
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
  // page fault
  io.resp.pf.ld := (bad_va && cmd_read) || (pf_ld_array & hits).orR
  io.resp.pf.st := (bad_va && cmd_write_perms) || (pf_st_array & hits).orR
  io.resp.pf.inst := bad_va || (pf_inst_array & hits).orR
  // guest page fault
  io.resp.gf.ld := (bad_gpa && cmd_read) || (gf_ld_array & hits).orR
  io.resp.gf.st := (bad_gpa && cmd_write_perms) || (gf_st_array & hits).orR
  io.resp.gf.inst := bad_gpa || (gf_inst_array & hits).orR
  // access exception
  io.resp.ae.ld := (ae_ld_array & hits).orR
  io.resp.ae.st := (ae_st_array & hits).orR
  io.resp.ae.inst := (~px_array & hits).orR
  // misaligned
  io.resp.ma.ld := misaligned && cmd_read
  io.resp.ma.st := misaligned && cmd_write
  io.resp.ma.inst := false.B // this is up to the pipeline to figure out
  io.resp.cacheable := (c_array & hits).orR
  io.resp.must_alloc := (must_alloc_array & hits).orR
  io.resp.prefetchable := (prefetchable_array & hits).orR && edge.manager.managers.forall(m => !m.supportsAcquireB || m.supportsHint).B
  io.resp.miss := do_refill || vsatp_mode_mismatch || tlb_miss || multipleHits
  io.resp.paddr := Cat(ppn, io.req.bits.vaddr(pgIdxBits-1, 0))
  io.resp.gpa_is_pte := vstage1_en && r_gpa_is_pte
  io.resp.gpa := {
    val page = Mux(!vstage1_en, Cat(bad_gpa, vpn), r_gpa >> pgIdxBits)
    val offset = Mux(io.resp.gpa_is_pte, r_gpa(pgIdxBits-1, 0), io.req.bits.vaddr(pgIdxBits-1, 0))
    Cat(page, offset)
  }

  io.ptw.req.valid := state === s_request
  io.ptw.req.bits.valid := !io.kill
  io.ptw.req.bits.bits.addr := r_refill_tag
  io.ptw.req.bits.bits.vstage1 := r_vstage1_en
  io.ptw.req.bits.bits.stage2 := r_stage2_en
  io.ptw.req.bits.bits.need_gpa := r_need_gpa

  if (usingVM) {
    when(io.ptw.req.fire && io.ptw.req.bits.valid) {
      r_gpa_valid := false.B
      r_gpa_vpn   := r_refill_tag
    }

    val sfence = io.sfence.valid
    // this is [[s_ready]]
    // handle miss/hit at the first cycle.
    // if miss, request PTW(L2TLB).
    when (io.req.fire && tlb_miss) {
      state := s_request
      r_refill_tag := vpn
      r_need_gpa := tlb_hit_if_not_gpa_miss
      r_vstage1_en := vstage1_en
      r_stage2_en := stage2_en
      r_superpage_repl_addr := replacementEntry(superpage_entries, superpage_plru.way)
      r_sectored_repl_addr := replacementEntry(sectored_entries(memIdx), sectored_plru.way(memIdx))
      r_sectored_hit.valid := sector_hits.orR
      r_sectored_hit.bits := OHToUInt(sector_hits)
      r_superpage_hit.valid := superpage_hits.orR
      r_superpage_hit.bits := OHToUInt(superpage_hits)
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
    // sfence in refill will results in invalidate
    when (state === s_wait && sfence) {
      state := s_wait_invalidate
    }
    // after CPU acquire response, go back to s_ready.
    when (io.ptw.resp.valid) {
      state := s_ready
    }

    // SFENCE processing logic.
    when (sfence) {
      assert(!io.sfence.bits.rs1 || (io.sfence.bits.addr >> pgIdxBits) === vpn)
      for (e <- all_real_entries) {
        val hv = usingHypervisor.B && io.sfence.bits.hv
        val hg = usingHypervisor.B && io.sfence.bits.hg
        when (!hg && io.sfence.bits.rs1) { e.invalidateVPN(vpn, hv) }
        .elsewhen (!hg && io.sfence.bits.rs2) { e.invalidateNonGlobal(hv) }
        .otherwise { e.invalidate(hv || hg) }
      }
    }
    when(io.req.fire && vsatp_mode_mismatch) {
      all_real_entries.foreach(_.invalidate(true.B))
      v_entries_use_stage1 := vstage1_en
    }
    when (multipleHits || reset.asBool) {
      all_real_entries.foreach(_.invalidate())
    }

    ccover(io.ptw.req.fire, "MISS", "TLB miss")
    ccover(io.ptw.req.valid && !io.ptw.req.ready, "PTW_STALL", "TLB miss, but PTW busy")
    ccover(state === s_wait_invalidate, "SFENCE_DURING_REFILL", "flush TLB during TLB refill")
    ccover(sfence && !io.sfence.bits.rs1 && !io.sfence.bits.rs2, "SFENCE_ALL", "flush TLB")
    ccover(sfence && !io.sfence.bits.rs1 && io.sfence.bits.rs2, "SFENCE_ASID", "flush TLB ASID")
    ccover(sfence && io.sfence.bits.rs1 && !io.sfence.bits.rs2, "SFENCE_LINE", "flush TLB line")
    ccover(sfence && io.sfence.bits.rs1 && io.sfence.bits.rs2, "SFENCE_LINE_ASID", "flush TLB line/ASID")
    ccover(multipleHits, "MULTIPLE_HITS", "Two matching translations in TLB")
  }

  def ccover(cond: Bool, label: String, desc: String)(implicit sourceInfo: SourceInfo) =
    property.cover(cond, s"${if (instruction) "I" else "D"}TLB_$label", "MemorySystem;;" + desc)
  /** Decides which entry to be replaced
    *
    * If there is a invalid entry, replace it with priorityencoder;
    * if not, replace the alt entry
    *
    * @return mask for TLBEntry replacement
    */
  def replacementEntry(set: Seq[TLBEntry], alt: UInt) = {
    val valids = set.map(_.valid.orR).asUInt
    Mux(valids.andR, alt, PriorityEncoder(~valids))
  }
}
