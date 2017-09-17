// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.rocket

import Chisel._
import Chisel.ImplicitConversions._

import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.coreplex.CacheBlockBytes
import freechips.rocketchip.diplomacy.RegionType
import freechips.rocketchip.tile.{XLen, CoreModule, CoreBundle}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

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
  val instruction = Bool()
  val sfence = Valid(new SFenceReq)
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

class TLB(instruction: Boolean, lgMaxSize: Int, nEntries: Int)(implicit edge: TLEdgeOut, p: Parameters) extends CoreModule()(p) {
  val io = new Bundle {
    val req = Decoupled(new TLBReq(lgMaxSize)).flip
    val resp = new TLBResp().asOutput
    val ptw = new TLBPTWIO
  }

  class Entry extends Bundle {
    val ppn = UInt(width = ppnBits)
    val tag = UInt(width = asIdBits + vpnBits)
    val level = UInt(width = log2Ceil(pgLevels))
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
  }

  val totalEntries = nEntries + 1
  val normalEntries = nEntries
  val specialEntry = nEntries
  val aeEntry = specialEntry - (1 << log2Floor(nEntries))
  val valid = Reg(init = UInt(0, totalEntries))
  val reg_entries = Reg(Vec(totalEntries, UInt(width = new Entry().getWidth)))
  val entries = reg_entries.map(_.asTypeOf(new Entry))

  val s_ready :: s_request :: s_wait :: s_wait_invalidate :: Nil = Enum(UInt(), 4)
  val state = Reg(init=s_ready)
  val r_refill_tag = Reg(UInt(width = asIdBits + vpnBits))
  val r_refill_waddr = Reg(UInt(width = log2Ceil(normalEntries)))
  val r_req = Reg(new TLBReq(lgMaxSize))

  val priv = if (instruction) io.ptw.status.prv else io.ptw.status.dprv
  val priv_s = priv(0)
  val priv_uses_vm = priv <= PRV.S
  val vm_enabled = Bool(usingVM) && io.ptw.ptbr.mode(io.ptw.ptbr.mode.getWidth-1) && priv_uses_vm && !io.req.bits.passthrough

  // share a single physical memory attribute checker (unshare if critical path)
  val (vpn, pgOffset) = Split(io.req.bits.vaddr, pgIdxBits)
  val refill_ppn = io.ptw.resp.bits.pte.ppn(ppnBits-1, 0)
  val do_refill = Bool(usingVM) && io.ptw.resp.valid
  val invalidate_refill = state.isOneOf(s_request /* don't care */, s_wait_invalidate)
  val mpu_ppn = Mux(do_refill, refill_ppn,
                Mux(vm_enabled, entries.last.ppn, vpn))
  val mpu_physaddr = Cat(mpu_ppn, io.req.bits.vaddr(pgIdxBits-1, 0))
  val pmp = Module(new PMPChecker(lgMaxSize))
  pmp.io.addr := mpu_physaddr
  pmp.io.size := io.req.bits.size
  pmp.io.pmp := (io.ptw.pmp: Seq[PMP])
  pmp.io.prv := Mux(Bool(usingVM) && (do_refill || io.req.bits.passthrough /* PTW */), PRV.S, priv)
  val legal_address = edge.manager.findSafe(mpu_physaddr).reduce(_||_)
  def fastCheck(member: TLManagerParameters => Boolean) =
    legal_address && edge.manager.fastProperty(mpu_physaddr, member, (b:Boolean) => Bool(b))
  val cacheable = fastCheck(_.supportsAcquireB) && (instruction || !usingDataScratchpad)
  val homogeneous = TLBPageLookup(edge.manager.managers, xLen, p(CacheBlockBytes), BigInt(1) << pgIdxBits)(mpu_physaddr).homogeneous
  val prot_r = fastCheck(_.supportsGet) && pmp.io.r
  val prot_w = fastCheck(_.supportsPutFull) && pmp.io.w
  val prot_al = fastCheck(_.supportsLogical) || cacheable
  val prot_aa = fastCheck(_.supportsArithmetic) || cacheable
  val prot_x = fastCheck(_.executable) && pmp.io.x
  val prot_eff = fastCheck(Seq(RegionType.PUT_EFFECTS, RegionType.GET_EFFECTS) contains _.regionType)

  val lookup_tag = Cat(io.ptw.ptbr.asid, vpn(vpnBits-1,0))
  val hitsVec = (0 until totalEntries).map { i => vm_enabled && {
    var tagMatch = valid(i)
    for (j <- 0 until pgLevels) {
      val base = vpnBits - (j + 1) * pgLevelBits
      tagMatch = tagMatch && (entries(i).level < j || entries(i).tag(base + pgLevelBits - 1, base) === vpn(base + pgLevelBits - 1, base))
    }
    tagMatch
  }} :+ !vm_enabled
  val hits = hitsVec.asUInt
  val level = Mux1H(hitsVec.init, entries.map(_.level))
  val partialPPN = Mux1H(hitsVec.init, entries.map(_.ppn))
  val ppn = {
    var ppn = Mux(vm_enabled, partialPPN, vpn)(pgLevelBits*pgLevels - 1, pgLevelBits*(pgLevels - 1))
    for (i <- 1 until pgLevels)
      ppn = Cat(ppn, (Mux(level < i, vpn, 0.U) | partialPPN)(vpnBits - i*pgLevelBits - 1, vpnBits - (i + 1)*pgLevelBits))
    ppn
  }

  // permission bit arrays
  when (do_refill && !invalidate_refill) {
    val waddr = Mux(io.ptw.resp.bits.ae, aeEntry.U, Mux(!io.ptw.resp.bits.homogeneous, specialEntry.U, r_refill_waddr))
    val pte = io.ptw.resp.bits.pte
    val newEntry = Wire(new Entry)
    newEntry.ppn := pte.ppn
    newEntry.tag := r_refill_tag
    newEntry.level := io.ptw.resp.bits.level
    newEntry.c := cacheable
    newEntry.u := pte.u
    newEntry.g := pte.g
    newEntry.ae := io.ptw.resp.bits.ae
    newEntry.sr := pte.sr()
    newEntry.sw := pte.sw()
    newEntry.sx := pte.sx()
    newEntry.pr := prot_r && !io.ptw.resp.bits.ae
    newEntry.pw := prot_w && !io.ptw.resp.bits.ae
    newEntry.px := prot_x && !io.ptw.resp.bits.ae
    newEntry.pal := prot_al
    newEntry.paa := prot_aa
    newEntry.eff := prot_eff

    valid := valid | UIntToOH(waddr)
    reg_entries(waddr) := newEntry.asUInt
  }

  val plru = new PseudoLRU(normalEntries)
  val repl_waddr = Mux(!valid(normalEntries-1, 0).andR, PriorityEncoder(~valid(normalEntries-1, 0)), plru.replace)

  val ptw_ae_array = entries(aeEntry).ae << aeEntry
  val priv_rw_ok = Mux(!priv_s || io.ptw.status.sum, entries.map(_.u).asUInt, 0.U) | Mux(priv_s, ~entries.map(_.u).asUInt, 0.U)
  val priv_x_ok = Mux(priv_s, ~entries.map(_.u).asUInt, entries.map(_.u).asUInt)
  val r_array = Cat(true.B, priv_rw_ok & (entries.map(_.sr).asUInt | Mux(io.ptw.status.mxr, entries.map(_.sx).asUInt, UInt(0))))
  val w_array = Cat(true.B, priv_rw_ok & entries.map(_.sw).asUInt)
  val x_array = Cat(true.B, priv_x_ok & entries.map(_.sx).asUInt)
  val pr_array = Cat(Fill(2, prot_r), entries.init.map(_.pr).asUInt)
  val pw_array = Cat(Fill(2, prot_w), entries.init.map(_.pw).asUInt)
  val px_array = Cat(Fill(2, prot_x), entries.init.map(_.px).asUInt)
  val paa_array = Cat(Fill(2, prot_aa), entries.init.map(_.paa).asUInt)
  val pal_array = Cat(Fill(2, prot_al), entries.init.map(_.pal).asUInt)
  val eff_array = Cat(Fill(2, prot_eff), entries.init.map(_.eff).asUInt)
  val c_array = Cat(Fill(2, cacheable), entries.init.map(_.c).asUInt)
  val prefetchable_array = Cat(cacheable && homogeneous, false.B, entries.init.map(_.c).asUInt)

  val misaligned = (io.req.bits.vaddr & (UIntToOH(io.req.bits.size) - 1)).orR
  val bad_va = vm_enabled &&
    (if (vpnBits == vpnBitsExtended) Bool(false)
     else vpn(vpnBits) =/= vpn(vpnBits-1))

  val lrscAllowed = Mux(Bool(usingDataScratchpad), 0.U, c_array)
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

  val tlb_hit = hits(totalEntries-1, 0).orR
  val tlb_miss = vm_enabled && !bad_va && !tlb_hit && !io.req.bits.sfence.valid
  when (io.req.valid && !tlb_miss && !hits(specialEntry)) {
    plru.access(OHToUInt(hits(normalEntries-1, 0)))
  }

  // Superpages create the possibility that two entries in the TLB may match.
  // This corresponds to a software bug, but we can't return complete garbage;
  // we must return either the old translation or the new translation.  This
  // isn't compatible with the Mux1H approach.  So, flush the TLB and report
  // a miss on duplicate entries.
  val multipleHits = PopCountAtLeast(hits(totalEntries-1, 0), 2)

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
  io.resp.paddr := Cat(ppn, pgOffset)

  io.ptw.req.valid := state === s_request
  io.ptw.req.bits <> io.ptw.status
  io.ptw.req.bits.addr := r_refill_tag

  if (usingVM) {
    val sfence = io.req.valid && io.req.bits.sfence.valid
    when (io.req.fire() && tlb_miss) {
      state := s_request
      r_refill_tag := lookup_tag
      r_refill_waddr := repl_waddr
      r_req := io.req.bits
    }
    when (state === s_request) {
      when (sfence) { state := s_ready }
      when (io.ptw.req.ready) { state := Mux(sfence, s_wait_invalidate, s_wait) }
    }
    when (state === s_wait && sfence) {
      state := s_wait_invalidate
    }
    when (io.ptw.resp.valid) {
      state := s_ready
    }

    when (sfence) {
      assert((io.req.bits.sfence.bits.addr >> pgIdxBits) === vpn(vpnBits-1,0))
      valid := Mux(io.req.bits.sfence.bits.rs1, valid & ~hits(totalEntries-1, 0),
               Mux(io.req.bits.sfence.bits.rs2, valid & entries.map(_.g).asUInt, 0))
    }
    when (multipleHits) {
      valid := 0
    }
  }
}
