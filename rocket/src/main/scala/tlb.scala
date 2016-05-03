// See LICENSE for license details.

package rocket

import Chisel._
import Util._
import junctions._
import scala.math._
import cde.{Parameters, Field}
import uncore.PseudoLRU

case object NTLBEntries extends Field[Int]

trait HasTLBParameters extends HasCoreParameters {
  val entries = p(NTLBEntries)
  val camAddrBits = log2Ceil(entries)
  val camTagBits = asIdBits + vpnBits
}

abstract class TLBModule(implicit val p: Parameters) extends Module
  with HasTLBParameters
abstract class TLBBundle(implicit val p: Parameters) extends ParameterizedBundle()(p)
  with HasTLBParameters

class CAMIO(implicit p: Parameters) extends TLBBundle()(p) {
    val clear        = Bool(INPUT)
    val clear_mask   = Bits(INPUT, entries)
    val tag          = Bits(INPUT, camTagBits)
    val hit          = Bool(OUTPUT)
    val hits         = UInt(OUTPUT, entries)
    val valid_bits   = Bits(OUTPUT, entries)
    
    val write        = Bool(INPUT)
    val write_tag    = Bits(INPUT, camTagBits)
    val write_addr    = UInt(INPUT, camAddrBits)
}

class RocketCAM(implicit p: Parameters) extends TLBModule()(p) {
  val io = new CAMIO
  val cam_tags = Mem(entries, Bits(width = camTagBits))

  val vb_array = Reg(init=Bits(0, entries))
  when (io.write) {
    vb_array := vb_array.bitSet(io.write_addr, Bool(true))
    cam_tags(io.write_addr) := io.write_tag
  }
  when (io.clear) {
    vb_array := vb_array & ~io.clear_mask
  }
  
  val hits = (0 until entries).map(i => vb_array(i) && cam_tags(i) === io.tag)
  
  io.valid_bits := vb_array
  io.hits := Vec(hits).toBits
  io.hit := io.hits.orR
}

class TLBReq(implicit p: Parameters) extends CoreBundle()(p) {
  val asid = UInt(width = asIdBits)
  val vpn = UInt(width = vpnBitsExtended)
  val passthrough = Bool()
  val instruction = Bool()
  val store = Bool()
}

class TLBRespNoHitIndex(implicit p: Parameters) extends CoreBundle()(p) {
  // lookup responses
  val miss = Bool(OUTPUT)
  val ppn = UInt(OUTPUT, ppnBits)
  val xcpt_ld = Bool(OUTPUT)
  val xcpt_st = Bool(OUTPUT)
  val xcpt_if = Bool(OUTPUT)
}

class TLBResp(implicit p: Parameters) extends TLBRespNoHitIndex()(p) with HasTLBParameters {
  val hit_idx = UInt(OUTPUT, entries)
}

class TLB(implicit p: Parameters) extends TLBModule()(p) {
  val io = new Bundle {
    val req = Decoupled(new TLBReq).flip
    val resp = new TLBResp
    val ptw = new TLBPTWIO
  }

  val tag_cam = Module(new RocketCAM)
  val tag_ram = Mem(entries, io.ptw.resp.bits.pte.ppn)

  val s_ready :: s_request :: s_wait :: s_wait_invalidate :: Nil = Enum(UInt(), 4)
  val state = Reg(init=s_ready)
  val r_refill_tag = Reg(tag_cam.io.write_tag)
  val r_refill_waddr = Reg(tag_cam.io.write_addr)
  val r_req = Reg(new TLBReq)
  
  val lookup_tag = Cat(io.req.bits.asid, io.req.bits.vpn).toUInt
  tag_cam.io.tag := lookup_tag
  tag_cam.io.write := state === s_wait && io.ptw.resp.valid
  tag_cam.io.write_tag := r_refill_tag
  tag_cam.io.write_addr := r_refill_waddr
  val tag_hit_addr = OHToUInt(tag_cam.io.hits)
  
  // permission bit arrays
  val ur_array = Reg(Vec(entries, Bool())) // user read permission
  val uw_array = Reg(Vec(entries, Bool())) // user write permission
  val ux_array = Reg(Vec(entries, Bool())) // user execute permission
  val sr_array = Reg(Vec(entries, Bool())) // supervisor read permission
  val sw_array = Reg(Vec(entries, Bool())) // supervisor write permission
  val sx_array = Reg(Vec(entries, Bool())) // supervisor execute permission
  val dirty_array = Reg(Vec(entries, Bool())) // PTE dirty bit
  when (io.ptw.resp.valid) {
    val pte = io.ptw.resp.bits.pte
    tag_ram(r_refill_waddr) := pte.ppn
    ur_array(r_refill_waddr) := pte.ur()
    uw_array(r_refill_waddr) := pte.uw()
    ux_array(r_refill_waddr) := pte.ux()
    sr_array(r_refill_waddr) := pte.sr()
    sw_array(r_refill_waddr) := pte.sw()
    sx_array(r_refill_waddr) := pte.sx()
    dirty_array(r_refill_waddr) := pte.d
  }
 
  // high if there are any unused (invalid) entries in the TLB
  val has_invalid_entry = !tag_cam.io.valid_bits.andR
  val invalid_entry = PriorityEncoder(~tag_cam.io.valid_bits)
  val plru = new PseudoLRU(entries)
  val repl_waddr = Mux(has_invalid_entry, invalid_entry, plru.replace)

  val do_mprv = io.ptw.status.mprv && !io.req.bits.instruction
  val priv = Mux(do_mprv, io.ptw.status.mpp, io.ptw.status.prv)
  val priv_s = priv === PRV.S
  val priv_uses_vm = priv <= PRV.S
  val req_xwr = Cat(!r_req.store, r_req.store, !(r_req.instruction || r_req.store))

  val ur_bits = ur_array.toBits
  val pum_ok = ~Mux(io.ptw.status.pum, ur_bits, UInt(0))
  val r_array = Mux(priv_s, sr_array.toBits & pum_ok, ur_bits)
  val w_array = Mux(priv_s, sw_array.toBits & pum_ok, uw_array.toBits)
  val x_array = Mux(priv_s, sx_array.toBits, ux_array.toBits)

  val vm_enabled = Bool(usingVM) && io.ptw.status.vm(3) && priv_uses_vm && !io.req.bits.passthrough
  val bad_va =
    if (vpnBits == vpnBitsExtended) Bool(false)
    else io.req.bits.vpn(vpnBits) =/= io.req.bits.vpn(vpnBits-1)
  // it's only a store hit if the dirty bit is set
  val tag_hits = tag_cam.io.hits & (dirty_array.toBits | ~Mux(io.req.bits.store, w_array, UInt(0)))
  val tag_hit = tag_hits.orR
  val tlb_hit = vm_enabled && tag_hit
  val tlb_miss = vm_enabled && !tag_hit && !bad_va

  when (io.req.valid && tlb_hit) {
    plru.access(OHToUInt(tag_cam.io.hits))
  }

  val paddr = Cat(io.resp.ppn, UInt(0, pgIdxBits))
  val addr_prot = addrMap.getProt(paddr)

  io.req.ready := state === s_ready
  io.resp.xcpt_ld := bad_va || (!tlb_miss && !addr_prot.r) || (tlb_hit && !(r_array & tag_cam.io.hits).orR)
  io.resp.xcpt_st := bad_va || (!tlb_miss && !addr_prot.w) || (tlb_hit && !(w_array & tag_cam.io.hits).orR)
  io.resp.xcpt_if := bad_va || (!tlb_miss && !addr_prot.x) || (tlb_hit && !(x_array & tag_cam.io.hits).orR)
  io.resp.miss := tlb_miss
  io.resp.ppn := Mux(vm_enabled, Mux1H(tag_cam.io.hits, tag_ram), io.req.bits.vpn(ppnBits-1,0))
  io.resp.hit_idx := tag_cam.io.hits

  // clear entries on a TLB flush.
  // TODO: selective flushing.  careful with superpage mappings (flush it all)
  tag_cam.io.clear := io.ptw.invalidate
  tag_cam.io.clear_mask := ~UInt(0, entries)
  
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
