// See LICENSE for license details.

package rocket

import Chisel._
import Util._
import junctions._
import scala.math._
import cde.{Parameters, Field}
import uncore.agents.PseudoLRU
import uncore.coherence._

case object NTLBEntries extends Field[Int]

trait HasTLBParameters extends HasCoreParameters {
  val entries = p(NTLBEntries)
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
}

class TLB(implicit val p: Parameters) extends Module with HasTLBParameters {
  val io = new Bundle {
    val req = Decoupled(new TLBReq).flip
    val resp = new TLBResp
    val ptw = new TLBPTWIO
  }

  val valid = Reg(init = UInt(0, entries))
  val ppns = Reg(Vec(entries, io.ptw.resp.bits.pte.ppn))
  val tags = Reg(Vec(entries, UInt(width = asIdBits + vpnBits)))

  val s_ready :: s_request :: s_wait :: s_wait_invalidate :: Nil = Enum(UInt(), 4)
  val state = Reg(init=s_ready)
  val r_refill_tag = Reg(UInt(width = asIdBits + vpnBits))
  val r_refill_waddr = Reg(UInt(width = log2Ceil(entries)))
  val r_req = Reg(new TLBReq)
  
  val lookup_tag = Cat(io.ptw.ptbr.asid, io.req.bits.vpn(vpnBits-1,0)).toUInt
  val hitsVec = (0 until entries).map(i => valid(i) && tags(i) === lookup_tag)
  val hits = hitsVec.toBits
  
  // permission bit arrays
  val ur_array = Reg(UInt(width = entries)) // user read permission
  val uw_array = Reg(UInt(width = entries)) // user write permission
  val ux_array = Reg(UInt(width = entries)) // user execute permission
  val sr_array = Reg(UInt(width = entries)) // supervisor read permission
  val sw_array = Reg(UInt(width = entries)) // supervisor write permission
  val sx_array = Reg(UInt(width = entries)) // supervisor execute permission
  val dirty_array = Reg(UInt(width = entries)) // PTE dirty bit
  when (io.ptw.resp.valid) {
    val pte = io.ptw.resp.bits.pte
    ppns(r_refill_waddr) := pte.ppn
    tags(r_refill_waddr) := r_refill_tag

    val mask = UIntToOH(r_refill_waddr)
    valid := valid | mask
    ur_array := Mux(pte.ur(), ur_array | mask, ur_array & ~mask)
    uw_array := Mux(pte.uw(), uw_array | mask, uw_array & ~mask)
    ux_array := Mux(pte.ux(), ux_array | mask, ux_array & ~mask)
    sr_array := Mux(pte.sr(), sr_array | mask, sr_array & ~mask)
    sw_array := Mux(pte.sw(), sw_array | mask, sw_array & ~mask)
    sx_array := Mux(pte.sx(), sx_array | mask, sx_array & ~mask)
    dirty_array := Mux(pte.d, dirty_array | mask, dirty_array & ~mask)
  }
 
  // high if there are any unused (invalid) entries in the TLB
  val plru = new PseudoLRU(entries)
  val repl_waddr = Mux(!valid.andR, PriorityEncoder(~valid), plru.replace)

  val do_mprv = io.ptw.status.mprv && !io.req.bits.instruction
  val priv = Mux(do_mprv, io.ptw.status.mpp, io.ptw.status.prv)
  val priv_s = priv === PRV.S
  val priv_uses_vm = priv <= PRV.S && !io.ptw.status.debug

  val pum_ok = ~Mux(io.ptw.status.pum, ur_array, UInt(0))
  val r_array = Mux(priv_s, sr_array & pum_ok, ur_array)
  val w_array = Mux(priv_s, sw_array & pum_ok, uw_array)
  val x_array = Mux(priv_s, sx_array, ux_array)

  val vm_enabled = Bool(usingVM) && io.ptw.status.vm(3) && priv_uses_vm && !io.req.bits.passthrough
  val bad_va =
    if (vpnBits == vpnBitsExtended) Bool(false)
    else io.req.bits.vpn(vpnBits) =/= io.req.bits.vpn(vpnBits-1)
  // it's only a store hit if the dirty bit is set
  val tag_hits = hits & (dirty_array | ~Mux(io.req.bits.store, w_array, UInt(0)))
  val tag_hit = tag_hits.orR
  val tlb_hit = vm_enabled && tag_hit
  val tlb_miss = vm_enabled && !tag_hit && !bad_va

  when (io.req.valid && tlb_hit) {
    plru.access(OHToUInt(hits))
  }

  val paddr = Cat(io.resp.ppn, UInt(0, pgIdxBits))
  val addr_prot = addrMap.getProt(paddr)

  io.req.ready := state === s_ready
  io.resp.xcpt_ld := bad_va || (!tlb_miss && !addr_prot.r) || (tlb_hit && !(r_array & hits).orR)
  io.resp.xcpt_st := bad_va || (!tlb_miss && !addr_prot.w) || (tlb_hit && !(w_array & hits).orR)
  io.resp.xcpt_if := bad_va || (!tlb_miss && !addr_prot.x) || (tlb_hit && !(x_array & hits).orR)
  io.resp.miss := tlb_miss
  io.resp.ppn := Mux(vm_enabled, Mux1H(hitsVec, ppns), io.req.bits.vpn(ppnBits-1,0))

  io.ptw.req.valid := state === s_request
  io.ptw.req.bits.addr := r_refill_tag
  io.ptw.req.bits.prv := io.ptw.status.prv
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

class DecoupledTLB(implicit p: Parameters) extends Module {
  val io = new Bundle {
    val req = Decoupled(new TLBReq).flip
    val resp = Decoupled(new TLBResp)
    val ptw = new TLBPTWIO
  }

  val reqq = Queue(io.req)
  val tlb = Module(new TLB)

  val resp_helper = DecoupledHelper(
    reqq.valid, tlb.io.req.ready, io.resp.ready)
  val tlb_miss = tlb.io.resp.miss

  tlb.io.req.valid := resp_helper.fire(tlb.io.req.ready)
  tlb.io.req.bits := reqq.bits
  reqq.ready := resp_helper.fire(reqq.valid, !tlb_miss)

  io.resp.valid := resp_helper.fire(io.resp.ready, !tlb_miss)
  io.resp.bits := tlb.io.resp

  io.ptw <> tlb.io.ptw
}
