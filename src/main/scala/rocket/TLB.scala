// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package rocket

import Chisel._
import Chisel.ImplicitConversions._
import config._
import diplomacy._
import coreplex.CacheBlockBytes
import tile.{XLen, CoreModule, CoreBundle}
import uncore.tilelink2._
import util._

case object PAddrBits extends Field[Int]
case object PgLevels extends Field[Int]
case object ASIdBits extends Field[Int]

class SFenceReq(implicit p: Parameters) extends CoreBundle()(p) {
  val rs1 = Bool()
  val rs2 = Bool()
  val asid = UInt(width = asIdBits max 1) // TODO zero-width
}

class TLBReq(implicit p: Parameters) extends CoreBundle()(p) {
  val vaddr = UInt(width = vaddrBitsExtended)
  val passthrough = Bool()
  val instruction = Bool()
  val store = Bool()
  val sfence = Valid(new SFenceReq)
}

class TLBResp(implicit p: Parameters) extends CoreBundle()(p) {
  // lookup responses
  val miss = Bool(OUTPUT)
  val paddr = UInt(OUTPUT, paddrBits)
  val xcpt_ld = Bool(OUTPUT)
  val xcpt_st = Bool(OUTPUT)
  val xcpt_if = Bool(OUTPUT)
  val cacheable = Bool(OUTPUT)
}

class TLB(entries: Int)(implicit edge: TLEdgeOut, p: Parameters) extends CoreModule()(p) {
  val io = new Bundle {
    val req = Decoupled(new TLBReq).flip
    val resp = new TLBResp
    val ptw = new TLBPTWIO
  }
  val totalEntries = entries + 1
  val normalEntries = entries
  val specialEntry = entries
  val valid = Reg(init = UInt(0, totalEntries))
  val ppns = Reg(Vec(totalEntries, UInt(width = ppnBits)))
  val tags = Reg(Vec(totalEntries, UInt(width = asIdBits + vpnBits)))
  val levels = Reg(Vec(totalEntries, UInt(width = log2Ceil(pgLevels))))

  val s_ready :: s_request :: s_wait :: s_wait_invalidate :: Nil = Enum(UInt(), 4)
  val state = Reg(init=s_ready)
  val r_refill_tag = Reg(UInt(width = asIdBits + vpnBits))
  val r_refill_waddr = Reg(UInt(width = log2Ceil(normalEntries)))
  val r_req = Reg(new TLBReq)

  val do_mprv = io.ptw.status.mprv && !io.req.bits.instruction
  val priv = Mux(do_mprv, io.ptw.status.mpp, io.ptw.status.prv)
  val priv_s = priv === PRV.S
  val priv_uses_vm = priv <= PRV.S && !io.ptw.status.debug
  val vm_enabled = Bool(usingVM) && io.ptw.ptbr.mode(io.ptw.ptbr.mode.getWidth-1) && priv_uses_vm && !io.req.bits.passthrough

  // share a single physical memory attribute checker (unshare if critical path)
  val (vpn, pgOffset) = Split(io.req.bits.vaddr, pgIdxBits)
  val refill_ppn = io.ptw.resp.bits.pte.ppn(ppnBits-1, 0)
  val do_refill = Bool(usingVM) && io.ptw.resp.valid
  val invalidate_refill = state.isOneOf(s_request /* don't care */, s_wait_invalidate)
  val mpu_ppn = Mux(do_refill, refill_ppn,
                Mux(vm_enabled, ppns.last, vpn(ppnBits-1, 0)))
  val mpu_physaddr = Cat(mpu_ppn, io.req.bits.vaddr(pgIdxBits-1, 0))
  val legal_address = edge.manager.findSafe(mpu_physaddr).reduce(_||_)
  def fastCheck(member: TLManagerParameters => Boolean) =
    legal_address && Mux1H(edge.manager.findFast(mpu_physaddr), edge.manager.managers.map(m => Bool(member(m))))
  val prot_r = fastCheck(_.supportsGet)
  val prot_w = fastCheck(_.supportsPutFull)
  val prot_x = fastCheck(_.executable)
  val cacheable = fastCheck(_.supportsAcquireB)
  val isSpecial = {
    val homogeneous = Wire(init = false.B)
    for (i <- 0 until pgLevels) {
      when (io.ptw.resp.bits.level === i) { homogeneous := TLBPageLookup(edge.manager.managers, xLen, p(CacheBlockBytes), BigInt(1) << (pgIdxBits + ((pgLevels - 1 - i) * pgLevelBits)))(mpu_physaddr).homogeneous }
    }
    !homogeneous
  }

  val lookup_tag = Cat(io.ptw.ptbr.asid, vpn(vpnBits-1,0))
  val hitsVec = (0 until totalEntries).map { i => vm_enabled && {
    var tagMatch = valid(i)
    for (j <- 0 until pgLevels) {
      val base = vpnBits - (j + 1) * pgLevelBits
      tagMatch = tagMatch && (levels(i) < j || tags(i)(base + pgLevelBits - 1, base) === vpn(base + pgLevelBits - 1, base))
    }
    tagMatch
  }} :+ !vm_enabled
  val hits = hitsVec.asUInt
  val level = Mux1H(hitsVec.init, levels)
  val partialPPN = Mux1H(hitsVec.init, ppns)
  val ppn = {
    var ppn = Mux(vm_enabled, partialPPN, vpn)(pgLevelBits*pgLevels - 1, pgLevelBits*(pgLevels - 1))
    for (i <- 1 until pgLevels)
      ppn = Cat(ppn, (Mux(level < i, vpn, 0.U) | partialPPN)(vpnBits - i*pgLevelBits - 1, vpnBits - (i + 1)*pgLevelBits))
    ppn
  }
  
  // permission bit arrays
  val u_array = Reg(UInt(width = totalEntries)) // user permission
  val sw_array = Reg(UInt(width = totalEntries)) // write permission
  val sx_array = Reg(UInt(width = totalEntries)) // execute permission
  val sr_array = Reg(UInt(width = totalEntries)) // read permission
  val xr_array = Reg(UInt(width = totalEntries)) // read permission to executable page
  val cash_array = Reg(UInt(width = normalEntries)) // cacheable
  when (do_refill && !invalidate_refill) {
    val waddr = Mux(isSpecial, specialEntry.U, r_refill_waddr)
    val pte = io.ptw.resp.bits.pte
    ppns(waddr) := pte.ppn
    tags(waddr) := r_refill_tag
    levels(waddr) := io.ptw.resp.bits.level

    val mask = UIntToOH(waddr)
    valid := valid | mask
    u_array := Mux(pte.u, u_array | mask, u_array & ~mask)
    sw_array := Mux(pte.sw() && (isSpecial || prot_w), sw_array | mask, sw_array & ~mask)
    sx_array := Mux(pte.sx() && (isSpecial || prot_x), sx_array | mask, sx_array & ~mask)
    sr_array := Mux(pte.sr() && (isSpecial || prot_r), sr_array | mask, sr_array & ~mask)
    xr_array := Mux(pte.sx() && (isSpecial || prot_r), xr_array | mask, xr_array & ~mask)
    cash_array := Mux(cacheable, cash_array | mask, cash_array & ~mask)
  }
 
  val plru = new PseudoLRU(normalEntries)
  val repl_waddr = Mux(!valid.andR, PriorityEncoder(~valid), plru.replace)

  val priv_ok = Mux(priv_s, ~Mux(io.ptw.status.pum, u_array, UInt(0)), u_array)
  val w_array = Cat(prot_w, priv_ok & ~(~prot_w << specialEntry) & sw_array)
  val x_array = Cat(prot_x, priv_ok & ~(~prot_x << specialEntry) & sx_array)
  val r_array = Cat(prot_r, priv_ok & ~(~prot_r << specialEntry) & (sr_array | Mux(io.ptw.status.mxr, xr_array, UInt(0))))
  val c_array = Cat(cacheable, cacheable, cash_array)

  val bad_va =
    if (vpnBits == vpnBitsExtended) Bool(false)
    else vpn(vpnBits) =/= vpn(vpnBits-1)
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
  io.resp.xcpt_ld := bad_va || (~r_array & hits).orR
  io.resp.xcpt_st := bad_va || (~w_array & hits).orR
  io.resp.xcpt_if := bad_va || (~x_array & hits).orR
  io.resp.cacheable := (c_array & hits).orR
  io.resp.miss := do_refill || tlb_miss || multipleHits
  io.resp.paddr := Cat(ppn, pgOffset)

  io.ptw.req.valid := state === s_request
  io.ptw.req.bits <> io.ptw.status
  io.ptw.req.bits.addr := r_refill_tag
  io.ptw.req.bits.store := r_req.store
  io.ptw.req.bits.fetch := r_req.instruction

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

    when (sfence && io.req.bits.sfence.bits.rs1) {
      valid := valid & ~hits(totalEntries-1, 0)
    }
    when (sfence && !io.req.bits.sfence.bits.rs1 || multipleHits) {
      valid := 0
    }
  }
}

class DecoupledTLB(entries: Int)(implicit edge: TLEdgeOut, p: Parameters) extends Module {
  val io = new Bundle {
    val req = Decoupled(new TLBReq).flip
    val resp = Decoupled(new TLBResp)
    val ptw = new TLBPTWIO
  }

  val req = Reg(new TLBReq)
  val resp = Reg(new TLBResp)
  val tlb = Module(new TLB(entries))

  val s_idle :: s_tlb_req :: s_tlb_resp :: s_done :: Nil = Enum(Bits(), 4)
  val state = Reg(init = s_idle)

  when (io.req.fire()) {
    req := io.req.bits
    state := s_tlb_req
  }

  when (tlb.io.req.fire()) {
    state := s_tlb_resp
  }

  when (state === s_tlb_resp) {
    when (tlb.io.resp.miss) {
      state := s_tlb_req
    } .otherwise {
      resp := tlb.io.resp
      state := s_done
    }
  }

  when (io.resp.fire()) { state := s_idle }

  io.req.ready := state === s_idle

  tlb.io.req.valid := state === s_tlb_req
  tlb.io.req.bits := req

  io.resp.valid := state === s_done
  io.resp.bits := resp

  io.ptw <> tlb.io.ptw
}
