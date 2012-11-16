package rocket

import Chisel._;
import Node._;
import Constants._;
import scala.math._;
import hwacha._

class ioCAM(entries: Int, addr_bits: Int, tag_bits: Int) extends Bundle {
    val clear        = Bool(INPUT);
    val clear_hit    = Bool(INPUT)
    val tag          = Bits(INPUT, tag_bits);
    val hit          = Bool(OUTPUT);
    val hits         = UFix(OUTPUT, entries);
    val valid_bits   = Bits(OUTPUT, entries);
    
    val write        = Bool(INPUT);
    val write_tag    = Bits(INPUT, tag_bits);
    val write_addr    = UFix(INPUT, addr_bits);
}

class rocketCAM(entries: Int, tag_bits: Int) extends Component {
  val addr_bits = ceil(log(entries)/log(2)).toInt;
  val io = new ioCAM(entries, addr_bits, tag_bits);
  val cam_tags = Vec(entries) { Reg() { Bits(width = tag_bits) } }

  val vb_array = Reg(resetVal = Bits(0, entries));
  when (io.write) {
    vb_array := vb_array.bitSet(io.write_addr, Bool(true));
    cam_tags(io.write_addr) := io.write_tag
  }
  when (io.clear) {
    vb_array := Bits(0, entries);
  }
  .elsewhen (io.clear_hit) {
    vb_array := vb_array & ~io.hits
  }
  
  val hits = (0 until entries).map(i => vb_array(i) && cam_tags(i) === io.tag)
  
  io.valid_bits := vb_array;
  io.hits := Vec(hits){Bool()}.toBits.toUFix
  io.hit := io.hits.orR
}

class PseudoLRU(n: Int)
{
  val state = Reg() { Bits(width = n) }
  def access(way: UFix) = {
    var next_state = state
    var idx = UFix(1,1)
    for (i <- log2Up(n)-1 to 0 by -1) {
      val bit = way(i)
      val mask = (UFix(1,n) << idx)(n-1,0)
      next_state = next_state & ~mask | Mux(bit, UFix(0), mask)
      //next_state.bitSet(idx, !bit)
      idx = Cat(idx, bit)
    }
    state := next_state
  }
  def replace = {
    var idx = UFix(1,1)
    for (i <- 0 until log2Up(n))
      idx = Cat(idx, state(idx))
    idx(log2Up(n)-1,0)
  }
}

class TLBReq extends Bundle
{
  val asid = UFix(width = ASID_BITS)
  val vpn = UFix(width = VPN_BITS+1)
  val passthrough = Bool()
  val instruction = Bool()
}

class TLBResp(entries: Int) extends Bundle
{
  // lookup responses
  val miss = Bool(OUTPUT)
  val hit_idx = UFix(OUTPUT, entries)
  val ppn = UFix(OUTPUT, PPN_BITS)
  val xcpt_ld = Bool(OUTPUT)
  val xcpt_st = Bool(OUTPUT)
  val xcpt_if = Bool(OUTPUT)

  override def clone = new TLBResp(entries).asInstanceOf[this.type]
}

class TLB(entries: Int) extends Component
{
  val io = new Bundle {
    val req = new FIFOIO()(new TLBReq).flip
    val resp = new TLBResp(entries)
    val ptw = new IOTLBPTW
  }

  val s_ready :: s_request :: s_wait :: s_wait_invalidate :: Nil = Enum(4) { UFix() }
  val state = Reg(resetVal = s_ready)
  val r_refill_tag = Reg() { UFix() }
  val r_refill_waddr = Reg() { UFix() }

  val tag_cam = new rocketCAM(entries, ASID_BITS+VPN_BITS);
  val tag_ram = Vec(entries) { Reg() { io.ptw.resp.bits.ppn.clone } }
  
  val lookup_tag = Cat(io.req.bits.asid, io.req.bits.vpn).toUFix
  tag_cam.io.clear := io.ptw.invalidate
  tag_cam.io.clear_hit := io.req.fire() && Mux(io.req.bits.instruction, io.resp.xcpt_if, io.resp.xcpt_ld && io.resp.xcpt_st)
  tag_cam.io.tag := lookup_tag
  tag_cam.io.write := state === s_wait && io.ptw.resp.valid
  tag_cam.io.write_tag := r_refill_tag
  tag_cam.io.write_addr := r_refill_waddr
  val tag_hit = tag_cam.io.hit
  val tag_hit_addr = OHToUFix(tag_cam.io.hits)
  
  // permission bit arrays
  val ur_array = Reg{Bits()} // user read permission
  val uw_array = Reg{Bits()} // user write permission
  val ux_array = Reg{Bits()} // user execute permission
  val sr_array = Reg{Bits()} // supervisor read permission
  val sw_array = Reg{Bits()} // supervisor write permission
  val sx_array = Reg{Bits()} // supervisor execute permission
  when (io.ptw.resp.valid) {
    tag_ram(r_refill_waddr) := io.ptw.resp.bits.ppn
    val perm = (!io.ptw.resp.bits.error).toFix & io.ptw.resp.bits.perm(5,0)
    ur_array := ur_array.bitSet(r_refill_waddr, perm(2))
    uw_array := uw_array.bitSet(r_refill_waddr, perm(1))
    ux_array := ux_array.bitSet(r_refill_waddr, perm(0))
    sr_array := sr_array.bitSet(r_refill_waddr, perm(5))
    sw_array := sw_array.bitSet(r_refill_waddr, perm(4))
    sx_array := sx_array.bitSet(r_refill_waddr, perm(3))
  }
 
  // high if there are any unused (invalid) entries in the TLB
  val has_invalid_entry = !tag_cam.io.valid_bits.andR
  val invalid_entry = PriorityEncoder(~tag_cam.io.valid_bits)
  val plru = new PseudoLRU(entries)
  val repl_waddr = Mux(has_invalid_entry, invalid_entry, plru.replace)
  
  val status_s  = io.ptw.status(SR_S)  // user/supervisor mode
  val status_vm = io.ptw.status(SR_VM) // virtual memory enable
  val bad_va = io.req.bits.vpn(VPN_BITS) != io.req.bits.vpn(VPN_BITS-1)
  val tlb_hit  = status_vm && tag_hit
  val tlb_miss = status_vm && !tag_hit && !bad_va
  
  when (io.req.valid && tlb_hit) {
    plru.access(OHToUFix(tag_cam.io.hits))
  }

  io.req.ready := state === s_ready
  io.resp.xcpt_ld := bad_va || tlb_hit && !Mux(status_s, (sr_array & tag_cam.io.hits).orR, (ur_array & tag_cam.io.hits).orR)
  io.resp.xcpt_st := bad_va || tlb_hit && !Mux(status_s, (sw_array & tag_cam.io.hits).orR, (uw_array & tag_cam.io.hits).orR)
  io.resp.xcpt_if := bad_va || tlb_hit && !Mux(status_s, (sx_array & tag_cam.io.hits).orR, (ux_array & tag_cam.io.hits).orR)
  io.resp.miss := tlb_miss
  io.resp.ppn := Mux(status_vm && !io.req.bits.passthrough, Mux1H(tag_cam.io.hits, tag_ram), io.req.bits.vpn(PPN_BITS-1,0))
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
