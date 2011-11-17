package Top
{

import Chisel._;
import Node._;
import Constants._;
import scala.math._;

// interface between DTLB and pipeline
class ioDTLB_CPU(view: List[String] = null) extends Bundle(view)
{
  // status bits (from PCR), to check current permission and whether VM is enabled
  val status = Bits(17, 'input);
  // invalidate all TLB entries
  val invalidate = Bool('input);
  // lookup requests
  val req_val  = Bool('input);
  val req_cmd  = Bits(4, 'input); // load/store/amo
  val req_rdy  = Bool('output);
  val req_asid = Bits(ASID_BITS, 'input);
  val req_vpn  = UFix(VPN_BITS, 'input);
  // lookup responses
  val resp_busy = Bool('output);
  val resp_miss = Bool('output);
//   val resp_val = Bool('output);
  val resp_ppn = UFix(PPN_BITS, 'output);
  val xcpt_ld = Bool('output);
  val xcpt_st = Bool('output);
}

class ioDTLB extends Bundle
{
  val cpu = new ioDTLB_CPU();
  val ptw = new ioTLB_PTW();
}

class rocketDTLB(entries: Int) extends Component
{
  val io = new ioDTLB();

  val addr_bits = ceil(log10(entries)/log10(2)).toInt;  

  val s_ready :: s_request :: s_wait :: Nil = Enum(3) { UFix() };
  val state = Reg(resetVal = s_ready);
  
  val r_cpu_req_vpn     = Reg(resetVal = Bits(0, VPN_BITS));
  val r_cpu_req_val     = Reg(resetVal = Bool(false));
  val r_cpu_req_cmd     = Reg(resetVal = Bits(0,4));
  val r_cpu_req_asid    = Reg(resetVal = Bits(0,ASID_BITS));
  val r_refill_tag      = Reg(resetVal = Bits(0,ASID_BITS+VPN_BITS));
  val r_refill_waddr    = Reg(resetVal = UFix(0,addr_bits));
  val repl_count        = Reg(resetVal = UFix(0,addr_bits));
  
  when (io.cpu.req_val && io.cpu.req_rdy) { 
    r_cpu_req_vpn   <== io.cpu.req_vpn;
    r_cpu_req_cmd   <== io.cpu.req_cmd;
    r_cpu_req_asid  <== io.cpu.req_asid;
  }
  
  when (io.cpu.req_rdy) {
    r_cpu_req_val <== io.cpu.req_val; 
  }
  
  val req_load  = (r_cpu_req_cmd === M_XRD);
  val req_store = (r_cpu_req_cmd === M_XWR);
  val req_flush = (r_cpu_req_cmd === M_FLA);
  val req_amo   = io.cpu.req_cmd(3).toBool;
  
  val lookup_tag = Cat(r_cpu_req_asid, r_cpu_req_vpn);

  val tag_cam = new rocketCAM(entries, addr_bits, ASID_BITS+VPN_BITS);  
  val tag_ram = Mem(entries, io.ptw.resp_val, r_refill_waddr.toUFix, io.ptw.resp_ppn);
  
  tag_cam.io.clear      := io.cpu.invalidate;
  tag_cam.io.tag        := lookup_tag;
  tag_cam.io.write      := io.ptw.resp_val || io.ptw.resp_err;
  tag_cam.io.write_tag  := r_refill_tag;
  tag_cam.io.write_addr := r_refill_waddr;
  val tag_hit            = tag_cam.io.hit;
  val tag_hit_addr       = tag_cam.io.hit_addr;
  
  // extract fields from status register
  val status_s  = io.cpu.status(SR_S).toBool; // user/supervisor mode
  val status_u  = !status_s;
  val status_vm = io.cpu.status(SR_VM).toBool // virtual memory enable
  
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
    ur_array <== ur_array.bitSet(r_refill_waddr, ptw_perm_ur);
    uw_array <== uw_array.bitSet(r_refill_waddr, ptw_perm_uw);
    sr_array <== sr_array.bitSet(r_refill_waddr, ptw_perm_sr);
    sw_array <== sw_array.bitSet(r_refill_waddr, ptw_perm_sw);
  }
 
  // when the page table lookup reports an error, set all permission
  // bits to 0 so the next access will cause an exception
  when (io.ptw.resp_err) {
    ur_array <== ur_array.bitSet(r_refill_waddr, Bool(false));
    uw_array <== uw_array.bitSet(r_refill_waddr, Bool(false));
    sr_array <== sr_array.bitSet(r_refill_waddr, Bool(false));
    sw_array <== sw_array.bitSet(r_refill_waddr, Bool(false));
  }
 
  // high if there are any unused (invalid) entries in the TLB
  val invalid_entry = (tag_cam.io.valid_bits != ~Bits(0,entries));
  val ie_enc = new priorityEncoder(entries);
  ie_enc.io.in := ~tag_cam.io.valid_bits.toUFix;
  val ie_addr = ie_enc.io.out;
  
  val repl_waddr = Mux(invalid_entry, ie_addr, repl_count).toUFix;
  
  val lookup = (state === s_ready) && r_cpu_req_val && !req_flush;
  val lookup_hit  = lookup && tag_hit;
  val lookup_miss = lookup && !tag_hit;
  val tlb_hit  = status_vm && lookup_hit;
  val tlb_miss = status_vm && lookup_miss;
  
  // currently replace TLB entries in LIFO order
  // TODO: implement LRU replacement policy
  when (tlb_miss) {
    r_refill_tag <== lookup_tag;
    r_refill_waddr <== repl_waddr;
    when (!invalid_entry) {
      repl_count <== repl_count + UFix(1);
    }
  }

  // exception check
  val outofrange = !tlb_miss && (io.cpu.resp_ppn > UFix(MEMSIZE_PAGES, PPN_BITS));

   val access_fault_ld =
    tlb_hit && (req_load || req_amo) &&
    ((status_s && !sr_array(tag_hit_addr).toBool) ||
     (status_u && !ur_array(tag_hit_addr).toBool));

  io.cpu.xcpt_ld := access_fault_ld;
//     (lookup && (req_load || req_amo) && outofrange) || access_fault_ld;

  val access_fault_st =
    tlb_hit && (req_store || req_amo) &&
    ((status_s && !sw_array(tag_hit_addr).toBool) ||
     (status_u && !uw_array(tag_hit_addr).toBool));

  io.cpu.xcpt_st := access_fault_st;
//     (lookup && (req_store || req_amo) && outofrange) || access_fault_st;

  io.cpu.req_rdy   := Mux(status_vm, (state === s_ready) && !tlb_miss, Bool(true));
  io.cpu.resp_busy := tlb_miss || (state != s_ready);
  io.cpu.resp_miss := tlb_miss;
  io.cpu.resp_ppn  := 
    Mux(status_vm, Mux(req_flush, Bits(0,PPN_BITS), tag_ram(tag_hit_addr)), 
      r_cpu_req_vpn(PPN_BITS-1,0)).toUFix;
  
  io.ptw.req_val := (state === s_request);
  io.ptw.req_vpn := r_refill_tag(VPN_BITS-1,0);
  
  // control state machine
  switch (state) {
    is (s_ready) {
      when (tlb_miss) {
        state <== s_request;
      }
    }
    is (s_request) {
      when (io.ptw.req_rdy) {
        state <== s_wait;
      }
    }
    is (s_wait) {
      when (io.ptw.resp_val || io.ptw.resp_err) {
        state <== s_ready;
      }
    }
  }  
}
}