// See LICENSE for license details.

package rocket

import Chisel._
import Util._
import junctions._
import scala.math._

case object NTLBEntries extends Field[Int]

abstract trait TLBParameters extends CoreParameters {
  val entries = params(NTLBEntries)
  val camAddrBits = ceil(log(entries)/log(2)).toInt
  val camTagBits = asIdBits + vpnBits
}

abstract class TLBBundle extends Bundle with TLBParameters
abstract class TLBModule extends Module with TLBParameters

class CAMIO extends TLBBundle {
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

class RocketCAM extends TLBModule {
  val io = new CAMIO
  val cam_tags = Mem(Bits(width = camTagBits), entries)

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

class PseudoLRU(n: Int)
{
  val state = Reg(Bits(width = n))
  def access(way: UInt) = {
    var next_state = state
    var idx = UInt(1,1)
    for (i <- log2Up(n)-1 to 0 by -1) {
      val bit = way(i)
      val mask = (UInt(1,n) << idx)(n-1,0)
      next_state = next_state & ~mask | Mux(bit, UInt(0), mask)
      //next_state.bitSet(idx, !bit)
      idx = Cat(idx, bit)
    }
    state := next_state
  }
  def replace = {
    var idx = UInt(1,1)
    for (i <- 0 until log2Up(n))
      idx = Cat(idx, state(idx))
    idx(log2Up(n)-1,0)
  }
}

class TLBReq extends CoreBundle {
  val asid = UInt(width = asIdBits)
  val vpn = UInt(width = vpnBits+1)
  val passthrough = Bool()
  val instruction = Bool()
  val store = Bool()
}

class TLBRespNoHitIndex extends CoreBundle {
  // lookup responses
  val miss = Bool(OUTPUT)
  val ppn = UInt(OUTPUT, ppnBits)
  val xcpt_ld = Bool(OUTPUT)
  val xcpt_st = Bool(OUTPUT)
  val xcpt_if = Bool(OUTPUT)
}

class TLBResp extends TLBRespNoHitIndex with TLBParameters {
  val hit_idx = UInt(OUTPUT, entries)
}

class TLB extends TLBModule {
  val io = new Bundle {
    val req = Decoupled(new TLBReq).flip
    val resp = new TLBResp
    val ptw = new TLBPTWIO
  }

  val s_ready :: s_request :: s_wait :: s_wait_invalidate :: Nil = Enum(UInt(), 4)
  val state = Reg(init=s_ready)
  val r_refill_tag = Reg(UInt())
  val r_refill_waddr = Reg(UInt())
  val r_req = Reg(new TLBReq)

  val tag_cam = Module(new RocketCAM)
  val tag_ram = Mem(io.ptw.resp.bits.pte.ppn, entries)
  
  val lookup_tag = Cat(io.req.bits.asid, io.req.bits.vpn).toUInt
  tag_cam.io.tag := lookup_tag
  tag_cam.io.write := state === s_wait && io.ptw.resp.valid
  tag_cam.io.write_tag := r_refill_tag
  tag_cam.io.write_addr := r_refill_waddr
  val tag_hit_addr = OHToUInt(tag_cam.io.hits)
  
  // permission bit arrays
  val valid_array = Reg(Vec(Bool(), entries)) // PTE is valid (not equivalent to CAM tag valid bit!)
  val ur_array = Reg(Vec(Bool(), entries)) // user read permission
  val uw_array = Reg(Vec(Bool(), entries)) // user write permission
  val ux_array = Reg(Vec(Bool(), entries)) // user execute permission
  val sr_array = Reg(Vec(Bool(), entries)) // supervisor read permission
  val sw_array = Reg(Vec(Bool(), entries)) // supervisor write permission
  val sx_array = Reg(Vec(Bool(), entries)) // supervisor execute permission
  val dirty_array = Reg(Vec(Bool(), entries)) // PTE dirty bit
  when (io.ptw.resp.valid) {
    val pte = io.ptw.resp.bits.pte
    tag_ram(r_refill_waddr) := pte.ppn
    valid_array(r_refill_waddr) := !io.ptw.resp.bits.error
    ur_array(r_refill_waddr) := pte.ur() && !io.ptw.resp.bits.error
    uw_array(r_refill_waddr) := pte.uw() && !io.ptw.resp.bits.error
    ux_array(r_refill_waddr) := pte.ux() && !io.ptw.resp.bits.error
    sr_array(r_refill_waddr) := pte.sr() && !io.ptw.resp.bits.error
    sw_array(r_refill_waddr) := pte.sw() && !io.ptw.resp.bits.error
    sx_array(r_refill_waddr) := pte.sx() && !io.ptw.resp.bits.error
    dirty_array(r_refill_waddr) := pte.d
  }
 
  // high if there are any unused (invalid) entries in the TLB
  val has_invalid_entry = !tag_cam.io.valid_bits.andR
  val invalid_entry = PriorityEncoder(~tag_cam.io.valid_bits)
  val plru = new PseudoLRU(entries)
  val repl_waddr = Mux(has_invalid_entry, invalid_entry, plru.replace)
 
  val priv = Mux(io.ptw.status.mprv && !io.req.bits.instruction, io.ptw.status.prv1, io.ptw.status.prv)
  val priv_s = priv === PRV_S
  val priv_uses_vm = priv <= PRV_S
  val req_xwr = Cat(!r_req.store, r_req.store, !(r_req.instruction || r_req.store))

  val r_array = Mux(priv_s, sr_array.toBits, ur_array.toBits)
  val w_array = Mux(priv_s, sw_array.toBits, uw_array.toBits)
  val x_array = Mux(priv_s, sx_array.toBits, ux_array.toBits)

  val vm_enabled = io.ptw.status.vm(3) && priv_uses_vm
  val bad_va = io.req.bits.vpn(vpnBits) != io.req.bits.vpn(vpnBits-1)
  // it's only a store hit if the dirty bit is set
  val tag_hits = tag_cam.io.hits & (dirty_array.toBits | ~(io.req.bits.store.toSInt & w_array))
  val tag_hit = tag_hits.orR
  val tlb_hit = vm_enabled && tag_hit
  val tlb_miss = vm_enabled && !tag_hit && !bad_va
  
  when (io.req.valid && tlb_hit) {
    plru.access(OHToUInt(tag_cam.io.hits))
  }

  io.req.ready := state === s_ready
  io.resp.xcpt_ld := bad_va || tlb_hit && !(r_array & tag_cam.io.hits).orR
  io.resp.xcpt_st := bad_va || tlb_hit && !(w_array & tag_cam.io.hits).orR
  io.resp.xcpt_if := bad_va || tlb_hit && !(x_array & tag_cam.io.hits).orR
  io.resp.miss := tlb_miss
  io.resp.ppn := Mux(vm_enabled && !io.req.bits.passthrough, Mux1H(tag_cam.io.hits, tag_ram), io.req.bits.vpn(params(PPNBits)-1,0))
  io.resp.hit_idx := tag_cam.io.hits

  // clear invalid entries on access, or all entries on a TLB flush
  tag_cam.io.clear := io.ptw.invalidate || io.req.fire()
  tag_cam.io.clear_mask := ~valid_array.toBits | (tag_cam.io.hits & ~tag_hits)
  when (io.ptw.invalidate) { tag_cam.io.clear_mask := SInt(-1) }
  
  io.ptw.req.valid := state === s_request
  io.ptw.req.bits.addr := r_refill_tag
  io.ptw.req.bits.prv := io.ptw.status.prv
  io.ptw.req.bits.store := r_req.store
  io.ptw.req.bits.fetch := r_req.instruction

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
