package rocket

import Chisel._;
import Node._;
import Constants._;
import scala.math._;
import hwacha._

// ioDTLB_CPU also located in hwacha/src/vuVXU-Interface.scala
// should keep them in sync

class ioDTLB_CPU_req_bundle extends Bundle
{
  // lookup requests
  val kill  = Bool()
  val cmd  = Bits(width=4) // load/store/amo
  val asid = Bits(width=ASID_BITS)
  val vpn  = Bits(width=VPN_BITS+1)
}
class ioDTLB_CPU_req extends hwacha.ioDecoupled()( { new ioDTLB_CPU_req_bundle() } )

class ioDTLB_CPU_resp extends Bundle
{
  // lookup responses
  val miss = Bool(OUTPUT)
  val ppn = Bits(PPN_BITS, OUTPUT)
  val xcpt_ld = Bool(OUTPUT)
  val xcpt_st = Bool(OUTPUT)
  val xcpt_pf = Bool(OUTPUT)
}

class ioDTLB extends Bundle
{
  // status bits (from PCR), to check current permission and whether VM is enabled
  val status = Bits(32, INPUT)
  // invalidate all TLB entries
  val invalidate = Bool(INPUT)
  val cpu_req = new ioDTLB_CPU_req().flip
  val cpu_resp = new ioDTLB_CPU_resp()
  val ptw = new ioTLB_PTW()
}

class rocketDTLB(entries: Int) extends Component
{
  val io = new ioDTLB();

  val addr_bits = ceil(log10(entries)/log10(2)).toInt;  

  val s_ready :: s_request :: s_wait :: Nil = Enum(3) { UFix() };
  val state = Reg(resetVal = s_ready);
  
  val r_cpu_req_val     = Reg(resetVal = Bool(false));
  val r_cpu_req_vpn     = Reg() { Bits() }
  val r_cpu_req_cmd     = Reg() { Bits() }
  val r_cpu_req_asid    = Reg() { Bits() }
  val r_refill_tag      = Reg() { Bits() }
  val r_refill_waddr    = Reg() { UFix() }
  
  when (io.cpu_req.valid && io.cpu_req.ready) {
    r_cpu_req_vpn   := io.cpu_req.bits.vpn;
    r_cpu_req_cmd   := io.cpu_req.bits.cmd;
    r_cpu_req_asid  := io.cpu_req.bits.asid;
    r_cpu_req_val   := Bool(true);
  }
  .otherwise {
    r_cpu_req_val   := Bool(false);
  }
  
  val req_load  = (r_cpu_req_cmd === M_XRD);
  val req_store = (r_cpu_req_cmd === M_XWR);
  val req_amo   = r_cpu_req_cmd(3).toBool;
  val req_pf    = (r_cpu_req_cmd === M_PFR) || (r_cpu_req_cmd === M_PFW)
  
  val bad_va = r_cpu_req_vpn(VPN_BITS) != r_cpu_req_vpn(VPN_BITS-1);

  val tag_cam = new rocketCAM(entries, ASID_BITS+VPN_BITS);
  val tag_ram = Mem(entries) { io.ptw.resp_ppn.clone }
  when (io.ptw.resp_val) { tag_ram(r_refill_waddr) := io.ptw.resp_ppn }
  
  val lookup_tag = Cat(r_cpu_req_asid, r_cpu_req_vpn);
  tag_cam.io.clear      := io.invalidate;
  tag_cam.io.clear_hit  := io.cpu_resp.xcpt_ld || io.cpu_resp.xcpt_st || io.cpu_resp.xcpt_pf
  tag_cam.io.tag        := lookup_tag;
  tag_cam.io.write      := io.ptw.resp_val || io.ptw.resp_err;
  tag_cam.io.write_tag  := r_refill_tag;
  tag_cam.io.write_addr := r_refill_waddr;
  val tag_hit            = tag_cam.io.hit || bad_va;
  val tag_hit_addr       = tag_cam.io.hit_addr;
  
  // extract fields from status register
  val status_s  = io.status(SR_S).toBool; // user/supervisor mode
  val status_u  = !status_s;
  val status_vm = io.status(SR_VM).toBool // virtual memory enable
  
  // extract fields from PT permission bits
  val ptw_perm_ur = io.ptw.resp_perm(2);
  val ptw_perm_uw = io.ptw.resp_perm(1);
  val ptw_perm_sr = io.ptw.resp_perm(5);
  val ptw_perm_sw = io.ptw.resp_perm(4);
  
  // permission bit arrays
  val ur_array = Reg(resetVal = Bits(0, entries)); // user read permission
  val uw_array = Reg(resetVal = Bits(0, entries)); // user write permission
  val sr_array = Reg(resetVal = Bits(0, entries)); // supervisor read permission
  val sw_array = Reg(resetVal = Bits(0, entries)); // supervisor write permission
  when (io.ptw.resp_val) {
    ur_array := ur_array.bitSet(r_refill_waddr, ptw_perm_ur);
    uw_array := uw_array.bitSet(r_refill_waddr, ptw_perm_uw);
    sr_array := sr_array.bitSet(r_refill_waddr, ptw_perm_sr);
    sw_array := sw_array.bitSet(r_refill_waddr, ptw_perm_sw);
  }
 
  // when the page table lookup reports an error, set all permission
  // bits to 0 so the next access will cause an exception
  when (io.ptw.resp_err) {
    ur_array := ur_array.bitSet(r_refill_waddr, Bool(false));
    uw_array := uw_array.bitSet(r_refill_waddr, Bool(false));
    sr_array := sr_array.bitSet(r_refill_waddr, Bool(false));
    sw_array := sw_array.bitSet(r_refill_waddr, Bool(false));
  }
 
  // high if there are any unused (invalid) entries in the TLB
  val has_invalid_entry = !tag_cam.io.valid_bits.andR
  val invalid_entry = PriorityEncoder(~tag_cam.io.valid_bits)
  val plru = new PseudoLRU(entries)
  val repl_waddr = Mux(has_invalid_entry, invalid_entry, plru.replace).toUFix;
  
  val lookup = (state === s_ready) && r_cpu_req_val && !io.cpu_req.bits.kill && (req_load || req_store || req_amo || req_pf);
  val lookup_hit  = lookup && tag_hit;
  val lookup_miss = lookup && !tag_hit;
  val tlb_hit  = status_vm && lookup_hit;
  val tlb_miss = status_vm && lookup_miss;
  
  // currently replace TLB entries in LIFO order
  // TODO: implement LRU replacement policy
  when (tlb_miss) {
    r_refill_tag := lookup_tag;
    r_refill_waddr := repl_waddr;
  }
  when (tlb_hit) {
    plru.access(tag_hit_addr)
  }

  val load_fault_common = tlb_hit &&
    ((status_s && !sr_array(tag_hit_addr)) ||
     (status_u && !ur_array(tag_hit_addr)) ||
     bad_va)
  val store_fault_common = tlb_hit &&
    ((status_s && !sw_array(tag_hit_addr)) ||
     (status_u && !uw_array(tag_hit_addr)) ||
     bad_va)

  io.cpu_resp.xcpt_ld := load_fault_common && (req_load || req_amo)
  io.cpu_resp.xcpt_st := store_fault_common && (req_store || req_amo)
  io.cpu_resp.xcpt_pf := load_fault_common && req_pf

  io.cpu_req.ready   := (state === s_ready) && !tlb_miss;
  io.cpu_resp.miss := tlb_miss;
  io.cpu_resp.ppn  :=
    Mux(status_vm, tag_ram(tag_hit_addr), r_cpu_req_vpn(PPN_BITS-1,0));
  
  io.ptw.req_val := (state === s_request);
  io.ptw.req_vpn := r_refill_tag(VPN_BITS-1,0);
  
  // control state machine
  switch (state) {
    is (s_ready) {
      when (tlb_miss) {
        state := s_request;
      }
    }
    is (s_request) {
      when (io.ptw.req_rdy) {
        state := s_wait;
      }
    }
    is (s_wait) {
      when (io.ptw.resp_val || io.ptw.resp_err) {
        state := s_ready;
      }
    }
  }  
}
