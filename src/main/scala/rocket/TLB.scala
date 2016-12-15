// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package rocket

import Chisel._
import util._
import Chisel.ImplicitConversions._
import scala.math._
import config._
import diplomacy._
import uncore.util._
import uncore.tilelink2._

case object PgLevels extends Field[Int]
case object ASIdBits extends Field[Int]

trait HasTLBParameters extends HasL1CacheParameters {
  val entries = p(p(CacheName)).nTLBEntries
  val camAddrBits = log2Ceil(entries)
  val camTagBits = asIdBits + vpnBits
}

class TLBReq(implicit p: Parameters) extends CoreBundle()(p) {
  val vpn = UInt(width = vpnBitsExtended)
  val passthrough = Bool()
  val instruction = Bool()
  val store = Bool()
}

class TLBResp(implicit p: Parameters) extends CoreBundle()(p) {
  // lookup responses
  val miss = Bool(OUTPUT)
  val ppn = UInt(OUTPUT, ppnBits)
  val xcpt_ld = Bool(OUTPUT)
  val xcpt_st = Bool(OUTPUT)
  val xcpt_if = Bool(OUTPUT)
  val cacheable = Bool(OUTPUT)
}

class TLB(implicit edge: TLEdgeOut, val p: Parameters) extends Module with HasTLBParameters {
  val io = new Bundle {
    val req = Decoupled(new TLBReq).flip
    val resp = new TLBResp
    val ptw = new TLBPTWIO
  }

  val valid = Reg(init = UInt(0, entries))
  val ppns = Reg(Vec(entries, UInt(width = ppnBits)))
  val tags = Reg(Vec(entries, UInt(width = asIdBits + vpnBits)))

  val s_ready :: s_request :: s_wait :: s_wait_invalidate :: Nil = Enum(UInt(), 4)
  val state = Reg(init=s_ready)
  val r_refill_tag = Reg(UInt(width = asIdBits + vpnBits))
  val r_refill_waddr = Reg(UInt(width = log2Ceil(entries)))
  val r_req = Reg(new TLBReq)

  val do_mprv = io.ptw.status.mprv && !io.req.bits.instruction
  val priv = Mux(do_mprv, io.ptw.status.mpp, io.ptw.status.prv)
  val priv_s = priv === PRV.S
  val priv_uses_vm = priv <= PRV.S && !io.ptw.status.debug

  // share a single physical memory attribute checker (unshare if critical path)
  val passthrough_ppn = io.req.bits.vpn(ppnBits-1, 0)
  val refill_ppn = io.ptw.resp.bits.pte.ppn(ppnBits-1, 0)
  val do_refill = Bool(usingVM) && io.ptw.resp.valid
  val mpu_ppn = Mux(do_refill, refill_ppn, passthrough_ppn)
  val mpu_physaddr = mpu_ppn << pgIdxBits
  val legal_address = edge.manager.findSafe(mpu_physaddr).reduce(_||_)
  def fastCheck(member: TLManagerParameters => Boolean) =
    legal_address && Mux1H(edge.manager.findFast(mpu_physaddr), edge.manager.managers.map(m => Bool(member(m))))
  val prot_r = fastCheck(_.supportsGet)
  val prot_w = fastCheck(_.supportsPutFull)
  val prot_x = fastCheck(_.executable)
  val cacheable = fastCheck(_.supportsAcquire)
  val allSizes = TransferSizes(1, cacheBlockBytes)
  val amoSizes = TransferSizes(1, xLen/8)
  edge.manager.managers.foreach { m =>
    require (m.minAlignment >= 4096, s"MemoryMap region ${m.name} must be page-aligned (is ${m.minAlignment})")
    require (!m.supportsGet     || m.supportsGet    .contains(allSizes), s"MemoryMap region ${m.name} only supports ${m.supportsGet} Get, but must support ${allSizes}")
    require (!m.supportsPutFull || m.supportsPutFull.contains(allSizes), s"MemoryMap region ${m.name} only supports ${m.supportsPutFull} PutFull, but must support ${allSizes}")
    require (!m.supportsAcquire || m.supportsAcquire.contains(allSizes), s"MemoryMap region ${m.name} only supports ${m.supportsAcquire} Acquire, but must support ${allSizes}")
    require (!m.supportsLogical || m.supportsLogical.contains(amoSizes), s"MemoryMap region ${m.name} only supports ${m.supportsLogical} Logical, but must support ${amoSizes}")
    require (!m.supportsArithmetic || m.supportsArithmetic.contains(amoSizes), s"MemoryMap region ${m.name} only supports ${m.supportsArithmetic} Arithmetic, but must support ${amoSizes}")
  }

  val lookup_tag = Cat(io.ptw.ptbr.asid, io.req.bits.vpn(vpnBits-1,0))
  val vm_enabled = Bool(usingVM) && io.ptw.status.vm(3) && priv_uses_vm && !io.req.bits.passthrough
  val hitsVec = (0 until entries).map(i => valid(i) && vm_enabled && tags(i) === lookup_tag) :+ !vm_enabled
  val hits = hitsVec.asUInt
  
  // permission bit arrays
  val pte_array = Reg(new PTE)
  val u_array = Reg(UInt(width = entries)) // user permission
  val sw_array = Reg(UInt(width = entries)) // write permission
  val sx_array = Reg(UInt(width = entries)) // execute permission
  val sr_array = Reg(UInt(width = entries)) // read permission
  val xr_array = Reg(UInt(width = entries)) // read permission to executable page
  val cash_array = Reg(UInt(width = entries)) // cacheable
  val dirty_array = Reg(UInt(width = entries)) // PTE dirty bit
  when (do_refill) {
    val pte = io.ptw.resp.bits.pte
    ppns(r_refill_waddr) := pte.ppn
    tags(r_refill_waddr) := r_refill_tag

    val mask = UIntToOH(r_refill_waddr)
    valid := valid | mask
    u_array := Mux(pte.u, u_array | mask, u_array & ~mask)
    sw_array := Mux(pte.sw() && prot_w, sw_array | mask, sw_array & ~mask)
    sx_array := Mux(pte.sx() && prot_x, sx_array | mask, sx_array & ~mask)
    sr_array := Mux(pte.sr() && prot_r, sr_array | mask, sr_array & ~mask)
    xr_array := Mux(pte.sx() && prot_r, xr_array | mask, xr_array & ~mask)
    cash_array := Mux(cacheable, cash_array | mask, cash_array & ~mask)
    dirty_array := Mux(pte.d, dirty_array | mask, dirty_array & ~mask)
  }
 
  val plru = new PseudoLRU(entries)
  val repl_waddr = Mux(!valid.andR, PriorityEncoder(~valid), plru.replace)

  val priv_ok = Mux(priv_s, ~Mux(io.ptw.status.pum, u_array, UInt(0)), u_array)
  val w_array = Cat(prot_w, priv_ok & sw_array)
  val x_array = Cat(prot_x, priv_ok & sx_array)
  val r_array = Cat(prot_r, priv_ok & (sr_array | Mux(io.ptw.status.mxr, xr_array, UInt(0))))
  val c_array = Cat(cacheable, cash_array)

  val bad_va =
    if (vpnBits == vpnBitsExtended) Bool(false)
    else io.req.bits.vpn(vpnBits) =/= io.req.bits.vpn(vpnBits-1)
  // it's only a store hit if the dirty bit is set
  val tlb_hits = hits(entries-1, 0) & (dirty_array | ~Mux(io.req.bits.store, w_array, UInt(0)))
  val tlb_hit = tlb_hits.orR
  val tlb_miss = vm_enabled && !bad_va && !tlb_hit

  when (io.req.valid && !tlb_miss) {
    plru.access(OHToUInt(hits(entries-1, 0)))
  }

  io.req.ready := state === s_ready
  io.resp.xcpt_ld := bad_va || (~r_array & hits).orR
  io.resp.xcpt_st := bad_va || (~w_array & hits).orR
  io.resp.xcpt_if := bad_va || (~x_array & hits).orR
  io.resp.cacheable := (c_array & hits).orR
  io.resp.miss := do_refill || tlb_miss
  io.resp.ppn := Mux1H(hitsVec, ppns :+ passthrough_ppn)

  io.ptw.req.valid := state === s_request
  io.ptw.req.bits <> io.ptw.status
  io.ptw.req.bits.addr := r_refill_tag
  io.ptw.req.bits.store := r_req.store
  io.ptw.req.bits.fetch := r_req.instruction

  if (usingVM) {
    when (io.req.fire() && tlb_miss) {
      state := s_request
      r_refill_tag := lookup_tag
      r_refill_waddr := repl_waddr
      r_req := io.req.bits
    }
    when (state === s_request) {
      when (io.ptw.invalidate) {
        state := s_ready
      }
      when (io.ptw.req.ready) {
        state := s_wait
        when (io.ptw.invalidate) { state := s_wait_invalidate }
      }
    }
    when (state === s_wait && io.ptw.invalidate) {
      state := s_wait_invalidate
    }
    when (io.ptw.resp.valid) {
      state := s_ready
    }

    when (io.ptw.invalidate) {
      valid := 0
    }
  }
}

class DecoupledTLB(implicit edge: TLEdgeOut, p: Parameters) extends Module {
  val io = new Bundle {
    val req = Decoupled(new TLBReq).flip
    val resp = Decoupled(new TLBResp)
    val ptw = new TLBPTWIO
  }

  val req = Reg(new TLBReq)
  val resp = Reg(new TLBResp)
  val tlb = Module(new TLB)

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
