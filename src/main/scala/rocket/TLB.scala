// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.rocket

import Chisel._
import Chisel.ImplicitConversions._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.subsystem.CacheBlockBytes
import freechips.rocketchip.diplomacy.RegionType
import freechips.rocketchip.tile.{XLen, CoreModule, CoreBundle}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property._
import chisel3.internal.sourceinfo.SourceInfo

case object PgLevels extends Field[Int](2)
case object ASIdBits extends Field[Int](0)

class SFenceReq(implicit p: Parameters) extends CoreBundle()(p) {
  val rs1 = Bool()
  val rs2 = Bool()
  val addr = UInt(width = vaddrBits)
  val asid = UInt(width = asIdBits max 1) // TODO zero-width
}

class TLBReq(lgMaxSize: Int)(implicit p: Parameters) extends CoreBundle()(p) {
  val vaddr = UInt(width = vaddrBitsExtended)
  val passthrough = Bool()
  val size = UInt(width = log2Ceil(lgMaxSize + 1))
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
  val miss = Bool()
  val paddr = UInt(width = paddrBits)
  val pf = new TLBExceptions
  val ae = new TLBExceptions
  val ma = new TLBExceptions
  val cacheable = Bool()
  val prefetchable = Bool()
}

case class TLBConfig(
    nEntries: Int,
    nSectors: Int = 4,
    nSuperpageEntries: Int = 4)

class TLB(instruction: Boolean, lgMaxSize: Int, cfg: TLBConfig)(implicit edge: TLEdgeOut, p: Parameters) extends CoreModule()(p) {
  val io = new Bundle {
    val req = Decoupled(new TLBReq(lgMaxSize)).flip
    val resp = new TLBResp().asOutput
    val sfence = Valid(new SFenceReq).asInput
    val ptw = new TLBPTWIO
    val kill = Bool(INPUT) // suppress a TLB refill, one cycle after a miss
  }

  class EntryData extends Bundle {
    val ppn = UInt(width = ppnBits)
    val u = Bool()
    val g = Bool()
    val ae = Bool()
    val sw = Bool()
    val sx = Bool()
    val sr = Bool()
    val pw = Bool()
    val px = Bool()
    val pr = Bool()
    val pal = Bool() // AMO logical
    val paa = Bool() // AMO arithmetic
    val eff = Bool() // get/put effects
    val c = Bool()
    val fragmented_superpage = Bool()
  }

  class Entry(val nSectors: Int, val superpage: Boolean, val superpageOnly: Boolean) extends Bundle {
    require(nSectors == 1 || !superpage)
    require(!superpageOnly || superpage)

    val level = UInt(width = log2Ceil(pgLevels))
    val tag = UInt(width = vpnBits)
    val data = Vec(nSectors, UInt(width = new EntryData().getWidth))
    val valid = Vec(nSectors, Bool())
    def entry_data = data.map(_.asTypeOf(new EntryData))

    private def sectorIdx(vpn: UInt) = vpn.extract(nSectors.log2-1, 0)
    def getData(vpn: UInt) = data(sectorIdx(vpn)).asTypeOf(new EntryData)
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
    def ppn(vpn: UInt) = {
      val data = getData(vpn)
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

    def insert(tag: UInt, level: UInt, entry: EntryData) {
      this.tag := tag
      this.level := level.extract(log2Ceil(pgLevels - superpageOnly.toInt)-1, 0)

      val idx = sectorIdx(tag)
      valid(idx) := true
      data(idx) := entry.asUInt
    }

    def invalidate() { valid.foreach(_ := false) }
    def invalidateVPN(vpn: UInt) {
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
    def invalidateNonGlobal() {
      for ((v, e) <- valid zip entry_data)
        when (!e.g) { v := false }
    }
  }

  val pageGranularityPMPs = pmpGranularity >= (1 << pgIdxBits)
  val sectored_entries = Reg(Vec(cfg.nEntries / cfg.nSectors, new Entry(cfg.nSectors, false, false)))
  val superpage_entries = Reg(Vec(cfg.nSuperpageEntries, new Entry(1, true, true)))
  val special_entry = (!pageGranularityPMPs).option(Reg(new Entry(1, true, false)))
  def ordinary_entries = sectored_entries ++ superpage_entries
  def all_entries = ordinary_entries ++ special_entry

  val s_ready :: s_request :: s_wait :: s_wait_invalidate :: Nil = Enum(UInt(), 4)
  val state = Reg(init=s_ready)
  val r_refill_tag = Reg(UInt(width = vpnBits))
  val r_superpage_repl_addr = Reg(UInt(log2Ceil(superpage_entries.size).W))
  val r_sectored_repl_addr = Reg(UInt(log2Ceil(sectored_entries.size).W))
  val r_sectored_hit_addr = Reg(UInt(log2Ceil(sectored_entries.size).W))
  val r_sectored_hit = Reg(Bool())

  val priv = if (instruction) io.ptw.status.prv else io.ptw.status.dprv
  val priv_s = priv(0)
  val priv_uses_vm = priv <= PRV.S
  val vm_enabled = Bool(usingVM) && io.ptw.ptbr.mode(io.ptw.ptbr.mode.getWidth-1) && priv_uses_vm && !io.req.bits.passthrough

  // share a single physical memory attribute checker (unshare if critical path)
  val vpn = io.req.bits.vaddr(vaddrBits-1, pgIdxBits)
  val refill_ppn = io.ptw.resp.bits.pte.ppn(ppnBits-1, 0)
  val do_refill = Bool(usingVM) && io.ptw.resp.valid
  val invalidate_refill = state.isOneOf(s_request /* don't care */, s_wait_invalidate)
  val mpu_ppn = Mux(do_refill, refill_ppn,
                Mux(vm_enabled && special_entry.nonEmpty, special_entry.map(_.ppn(vpn)).getOrElse(0.U), io.req.bits.vaddr >> pgIdxBits))
  val mpu_physaddr = Cat(mpu_ppn, io.req.bits.vaddr(pgIdxBits-1, 0))
  val pmp = Module(new PMPChecker(lgMaxSize))
  pmp.io.addr := mpu_physaddr
  pmp.io.size := io.req.bits.size
  pmp.io.pmp := (io.ptw.pmp: Seq[PMP])
  pmp.io.prv := Mux(Bool(usingVM) && (do_refill || io.req.bits.passthrough /* PTW */), PRV.S, priv)
  val legal_address = edge.manager.findSafe(mpu_physaddr).reduce(_||_)
  def fastCheck(member: TLManagerParameters => Boolean) =
    legal_address && edge.manager.fastProperty(mpu_physaddr, member, (b:Boolean) => Bool(b))
  val cacheable = fastCheck(_.supportsAcquireT) && (instruction || !usingDataScratchpad)
  val homogeneous = TLBPageLookup(edge.manager.managers, xLen, p(CacheBlockBytes), BigInt(1) << pgIdxBits)(mpu_physaddr).homogeneous
  val prot_r = fastCheck(_.supportsGet) && pmp.io.r
  val prot_w = fastCheck(_.supportsPutFull) && pmp.io.w
  val prot_al = fastCheck(_.supportsLogical) || (cacheable && usingAtomicsInCache)
  val prot_aa = fastCheck(_.supportsArithmetic) || (cacheable && usingAtomicsInCache)
  val prot_x = fastCheck(_.executable) && pmp.io.x
  val prot_eff = fastCheck(Seq(RegionType.PUT_EFFECTS, RegionType.GET_EFFECTS) contains _.regionType)

  val sector_hits = sectored_entries.map(_.sectorHit(vpn))
  val superpage_hits = superpage_entries.map(_.hit(vpn))
  val hitsVec = all_entries.map(vm_enabled && _.hit(vpn))
  val real_hits = hitsVec.asUInt
  val hits = Cat(!vm_enabled, real_hits)
  val ppn = Mux1H(hitsVec :+ !vm_enabled, all_entries.map(_.ppn(vpn)) :+ vpn(ppnBits-1, 0))

  // permission bit arrays
  when (do_refill && !invalidate_refill) {
    val pte = io.ptw.resp.bits.pte
    val newEntry = Wire(new EntryData)
    newEntry.ppn := pte.ppn
    newEntry.c := cacheable
    newEntry.u := pte.u
    newEntry.g := pte.g
    newEntry.ae := io.ptw.resp.bits.ae
    newEntry.sr := pte.sr()
    newEntry.sw := pte.sw()
    newEntry.sx := pte.sx()
    newEntry.pr := prot_r
    newEntry.pw := prot_w
    newEntry.px := prot_x
    newEntry.pal := prot_al
    newEntry.paa := prot_aa
    newEntry.eff := prot_eff
    newEntry.fragmented_superpage := io.ptw.resp.bits.fragmented_superpage

    when (special_entry.nonEmpty && !io.ptw.resp.bits.homogeneous) {
      special_entry.foreach(_.insert(r_refill_tag, io.ptw.resp.bits.level, newEntry))
    }.elsewhen (io.ptw.resp.bits.level < pgLevels-1) {
      for ((e, i) <- superpage_entries.zipWithIndex) when (r_superpage_repl_addr === i) {
        e.insert(r_refill_tag, io.ptw.resp.bits.level, newEntry)
      }
    }.otherwise {
      val waddr = Mux(r_sectored_hit, r_sectored_hit_addr, r_sectored_repl_addr)
      for ((e, i) <- sectored_entries.zipWithIndex) when (waddr === i) {
        when (!r_sectored_hit) { e.invalidate() }
        e.insert(r_refill_tag, 0.U, newEntry)
      }
    }
  }

  val entries = all_entries.map(_.getData(vpn))
  val normal_entries = ordinary_entries.map(_.getData(vpn))
  val nPhysicalEntries = 1 + special_entry.size
  val ptw_ae_array = Cat(false.B, entries.map(_.ae).asUInt)
  val priv_rw_ok = Mux(!priv_s || io.ptw.status.sum, entries.map(_.u).asUInt, 0.U) | Mux(priv_s, ~entries.map(_.u).asUInt, 0.U)
  val priv_x_ok = Mux(priv_s, ~entries.map(_.u).asUInt, entries.map(_.u).asUInt)
  val r_array = Cat(true.B, priv_rw_ok & (entries.map(_.sr).asUInt | Mux(io.ptw.status.mxr, entries.map(_.sx).asUInt, UInt(0))))
  val w_array = Cat(true.B, priv_rw_ok & entries.map(_.sw).asUInt)
  val x_array = Cat(true.B, priv_x_ok & entries.map(_.sx).asUInt)
  val pr_array = Cat(Fill(nPhysicalEntries, prot_r), normal_entries.map(_.pr).asUInt) & ~ptw_ae_array
  val pw_array = Cat(Fill(nPhysicalEntries, prot_w), normal_entries.map(_.pw).asUInt) & ~ptw_ae_array
  val px_array = Cat(Fill(nPhysicalEntries, prot_x), normal_entries.map(_.px).asUInt) & ~ptw_ae_array
  val paa_array = Cat(Fill(nPhysicalEntries, prot_aa), normal_entries.map(_.paa).asUInt)
  val pal_array = Cat(Fill(nPhysicalEntries, prot_al), normal_entries.map(_.pal).asUInt)
  val eff_array = Cat(Fill(nPhysicalEntries, prot_eff), normal_entries.map(_.eff).asUInt)
  val c_array = Cat(Fill(nPhysicalEntries, cacheable), normal_entries.map(_.c).asUInt)
  val prefetchable_array = Cat((cacheable && homogeneous) << (nPhysicalEntries-1), normal_entries.map(_.c).asUInt)

  val misaligned = (io.req.bits.vaddr & (UIntToOH(io.req.bits.size) - 1)).orR
  val bad_va = vm_enabled &&
    (if (vpnBits == vpnBitsExtended) Bool(false)
     else (io.req.bits.vaddr.asSInt < 0.S) =/= (vpn.asSInt < 0.S))

  val lrscAllowed = Mux(Bool(usingDataScratchpad || usingAtomicsOnlyForIO), 0.U, c_array)
  val ae_array =
    Mux(misaligned, eff_array, 0.U) |
    Mux(Bool(usingAtomics) && io.req.bits.cmd.isOneOf(M_XLR, M_XSC), ~lrscAllowed, 0.U)
  val ae_ld_array = Mux(isRead(io.req.bits.cmd), ae_array | ~pr_array, 0.U)
  val ae_st_array =
    Mux(isWrite(io.req.bits.cmd), ae_array | ~pw_array, 0.U) |
    Mux(Bool(usingAtomics) && isAMOLogical(io.req.bits.cmd), ~pal_array, 0.U) |
    Mux(Bool(usingAtomics) && isAMOArithmetic(io.req.bits.cmd), ~paa_array, 0.U)
  val ma_ld_array = Mux(misaligned && isRead(io.req.bits.cmd), ~eff_array, 0.U)
  val ma_st_array = Mux(misaligned && isWrite(io.req.bits.cmd), ~eff_array, 0.U)
  val pf_ld_array = Mux(isRead(io.req.bits.cmd), ~(r_array | ptw_ae_array), 0.U)
  val pf_st_array = Mux(isWrite(io.req.bits.cmd), ~(w_array | ptw_ae_array), 0.U)
  val pf_inst_array = ~(x_array | ptw_ae_array)

  val tlb_hit = real_hits.orR
  val tlb_miss = vm_enabled && !bad_va && !tlb_hit

  val sectored_plru = new PseudoLRU(sectored_entries.size)
  val superpage_plru = new PseudoLRU(superpage_entries.size)
  when (io.req.valid && vm_enabled) {
    when (sector_hits.orR) { sectored_plru.access(OHToUInt(sector_hits)) }
    when (superpage_hits.orR) { superpage_plru.access(OHToUInt(superpage_hits)) }
  }

  // Superpages create the possibility that two entries in the TLB may match.
  // This corresponds to a software bug, but we can't return complete garbage;
  // we must return either the old translation or the new translation.  This
  // isn't compatible with the Mux1H approach.  So, flush the TLB and report
  // a miss on duplicate entries.
  val multipleHits = PopCountAtLeast(real_hits, 2)

  io.req.ready := state === s_ready
  io.resp.pf.ld := (bad_va && isRead(io.req.bits.cmd)) || (pf_ld_array & hits).orR
  io.resp.pf.st := (bad_va && isWrite(io.req.bits.cmd)) || (pf_st_array & hits).orR
  io.resp.pf.inst := bad_va || (pf_inst_array & hits).orR
  io.resp.ae.ld := (ae_ld_array & hits).orR
  io.resp.ae.st := (ae_st_array & hits).orR
  io.resp.ae.inst := (~px_array & hits).orR
  io.resp.ma.ld := (ma_ld_array & hits).orR
  io.resp.ma.st := (ma_st_array & hits).orR
  io.resp.ma.inst := false // this is up to the pipeline to figure out
  io.resp.cacheable := (c_array & hits).orR
  io.resp.prefetchable := (prefetchable_array & hits).orR && edge.manager.managers.forall(m => !m.supportsAcquireB || m.supportsHint)
  io.resp.miss := do_refill || tlb_miss || multipleHits
  io.resp.paddr := Cat(ppn, io.req.bits.vaddr(pgIdxBits-1, 0))

  io.ptw.req.valid := state === s_request
  io.ptw.req.bits.valid := !io.kill
  io.ptw.req.bits.bits.addr := r_refill_tag

  if (usingVM) {
    val sfence = io.sfence.valid
    when (io.req.fire() && tlb_miss) {
      state := s_request
      r_refill_tag := vpn

      r_superpage_repl_addr := replacementEntry(superpage_entries, superpage_plru.replace)
      r_sectored_repl_addr := replacementEntry(sectored_entries, sectored_plru.replace)
      r_sectored_hit_addr := OHToUInt(sector_hits)
      r_sectored_hit := sector_hits.orR
    }
    when (state === s_request) {
      when (sfence) { state := s_ready }
      when (io.ptw.req.ready) { state := Mux(sfence, s_wait_invalidate, s_wait) }
      when (io.kill) { state := s_ready }
    }
    when (state === s_wait && sfence) {
      state := s_wait_invalidate
    }
    when (io.ptw.resp.valid) {
      state := s_ready
    }

    when (sfence) {
      assert(!io.sfence.bits.rs1 || (io.sfence.bits.addr >> pgIdxBits) === vpn)
      for (e <- all_entries) {
        when (io.sfence.bits.rs1) { e.invalidateVPN(vpn) }
        .elsewhen (io.sfence.bits.rs2) { e.invalidateNonGlobal() }
        .otherwise { e.invalidate() }
      }
    }
    when (multipleHits || reset) {
      all_entries.foreach(_.invalidate())
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

  def replacementEntry(set: Seq[Entry], alt: UInt) = {
    val valids = set.map(_.valid.orR).asUInt
    Mux(valids.andR, alt, PriorityEncoder(~valids))
  }
}
