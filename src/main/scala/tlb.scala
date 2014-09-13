// See LICENSE for license details.

package rocket

import Chisel._
import uncore._
import scala.math._

class CAMIO(entries: Int, addr_bits: Int, tag_bits: Int) extends Bundle {
    val clear        = Bool(INPUT)
    val clear_hit    = Bool(INPUT)
    val tag          = Bits(INPUT, tag_bits)
    val hit          = Bool(OUTPUT)
    val hits         = UInt(OUTPUT, entries)
    val valid_bits   = Bits(OUTPUT, entries)
    
    val write        = Bool(INPUT)
    val write_tag    = Bits(INPUT, tag_bits)
    val write_addr    = UInt(INPUT, addr_bits)
}

class RocketCAM(entries: Int, tag_bits: Int) extends Module {
  val addr_bits = ceil(log(entries)/log(2)).toInt
  val io = new CAMIO(entries, addr_bits, tag_bits)
  val cam_tags = Mem(Bits(width = tag_bits), entries)

  val vb_array = Reg(init=Bits(0, entries))
  when (io.write) {
    vb_array := vb_array.bitSet(io.write_addr, Bool(true))
    cam_tags(io.write_addr) := io.write_tag
  }
  when (io.clear) {
    vb_array := Bits(0, entries)
  }
  .elsewhen (io.clear_hit) {
    vb_array := vb_array & ~io.hits
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

class TLBReq extends Bundle
{
  val asid = UInt(width = params(ASIdBits))
  val vpn = UInt(width = params(VPNBits)+1)
  val passthrough = Bool()
  val instruction = Bool()
}

class TLBResp(entries: Int) extends Bundle
{
  // lookup responses
  val miss = Bool(OUTPUT)
  val hit_idx = UInt(OUTPUT, entries)
  val ppn = UInt(OUTPUT, params(PPNBits))
  val xcpt_ld = Bool(OUTPUT)
  val xcpt_st = Bool(OUTPUT)
  val xcpt_if = Bool(OUTPUT)
}

class TLB(entries: Int) extends Module
{
  val io = new Bundle {
    val req = Decoupled(new TLBReq).flip
    val resp = new TLBResp(entries)
    val ptw = new TLBPTWIO
  }

  val s_ready :: s_request :: s_wait :: s_wait_invalidate :: Nil = Enum(UInt(), 4)
  val state = Reg(init=s_ready)
  val r_refill_tag = Reg(UInt())
  val r_refill_waddr = Reg(UInt())

  val tag_cam = Module(new RocketCAM(entries, params(ASIdBits)+params(VPNBits)))
  val tag_ram = Mem(io.ptw.resp.bits.ppn.clone, entries)
  
  val lookup_tag = Cat(io.req.bits.asid, io.req.bits.vpn).toUInt
  tag_cam.io.clear := io.ptw.invalidate
  tag_cam.io.clear_hit := io.req.fire() && Mux(io.req.bits.instruction, io.resp.xcpt_if, io.resp.xcpt_ld && io.resp.xcpt_st)
  tag_cam.io.tag := lookup_tag
  tag_cam.io.write := state === s_wait && io.ptw.resp.valid
  tag_cam.io.write_tag := r_refill_tag
  tag_cam.io.write_addr := r_refill_waddr
  val tag_hit = tag_cam.io.hit
  val tag_hit_addr = OHToUInt(tag_cam.io.hits)
  
  // permission bit arrays
  val ur_array = Reg(Bits()) // user read permission
  val uw_array = Reg(Bits()) // user write permission
  val ux_array = Reg(Bits()) // user execute permission
  val sr_array = Reg(Bits()) // supervisor read permission
  val sw_array = Reg(Bits()) // supervisor write permission
  val sx_array = Reg(Bits()) // supervisor execute permission
  when (io.ptw.resp.valid) {
    tag_ram(r_refill_waddr) := io.ptw.resp.bits.ppn
    val perm = (!io.ptw.resp.bits.error).toSInt & io.ptw.resp.bits.perm
    ur_array := ur_array.bitSet(r_refill_waddr, perm(0))
    uw_array := uw_array.bitSet(r_refill_waddr, perm(1))
    ux_array := ux_array.bitSet(r_refill_waddr, perm(2))
    sr_array := sr_array.bitSet(r_refill_waddr, perm(3))
    sw_array := sw_array.bitSet(r_refill_waddr, perm(4))
    sx_array := sx_array.bitSet(r_refill_waddr, perm(5))
  }
 
  // high if there are any unused (invalid) entries in the TLB
  val has_invalid_entry = !tag_cam.io.valid_bits.andR
  val invalid_entry = PriorityEncoder(~tag_cam.io.valid_bits)
  val plru = new PseudoLRU(entries)
  val repl_waddr = Mux(has_invalid_entry, invalid_entry, plru.replace)
  
  val bad_va = io.req.bits.vpn(params(VPNBits)) != io.req.bits.vpn(params(VPNBits)-1)
  val tlb_hit  = io.ptw.status.vm && tag_hit
  val tlb_miss = io.ptw.status.vm && !tag_hit && !bad_va
  
  when (io.req.valid && tlb_hit) {
    plru.access(OHToUInt(tag_cam.io.hits))
  }

  io.req.ready := state === s_ready
  io.resp.xcpt_ld := bad_va || tlb_hit && !Mux(io.ptw.status.s, (sr_array & tag_cam.io.hits).orR, (ur_array & tag_cam.io.hits).orR)
  io.resp.xcpt_st := bad_va || tlb_hit && !Mux(io.ptw.status.s, (sw_array & tag_cam.io.hits).orR, (uw_array & tag_cam.io.hits).orR)
  io.resp.xcpt_if := bad_va || tlb_hit && !Mux(io.ptw.status.s, (sx_array & tag_cam.io.hits).orR, (ux_array & tag_cam.io.hits).orR)
  io.resp.miss := tlb_miss
  io.resp.ppn := Mux(io.ptw.status.vm && !io.req.bits.passthrough, Mux1H(tag_cam.io.hits, tag_ram), io.req.bits.vpn(params(PPNBits)-1,0))
  io.resp.hit_idx := tag_cam.io.hits
  
  io.ptw.req.valid := state === s_request
  io.ptw.req.bits := r_refill_tag

  when (io.req.fire() && tlb_miss) {
    state := s_request
    r_refill_tag := lookup_tag
    r_refill_waddr := repl_waddr
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
