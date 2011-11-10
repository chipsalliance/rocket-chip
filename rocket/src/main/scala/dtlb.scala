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
  val req_addr  = UFix(VADDR_BITS, 'input);
  // lookup responses
  val resp_miss = Bool('output);
  val resp_val = Bool('output);
  val resp_addr = UFix(PADDR_BITS, 'output);
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
  val addr_bits = ceil(log10(entries)/log10(2)).toInt;  
  val io = new ioDTLB();

  val s_ready :: s_request :: s_wait :: Nil = Enum(3) { UFix() };
  val state = Reg(resetVal = s_ready);

  val tag_cam = new rocketCAM(entries, addr_bits, ASID_BITS+VPN_BITS);
  
  val req_vpn = io.cpu.req_addr(VADDR_BITS-1,PGIDX_BITS);
  val req_idx = io.cpu.req_addr(PGIDX_BITS-1,0);
  val req_load  = io.cpu.req_val && (io.cpu.req_cmd === M_XRD);
  val req_store = io.cpu.req_val && (io.cpu.req_cmd === M_XWR);
//   val req_amo   = io.cpu.req_cmd(3).toBool;
  
  val lookup_tag = Cat(io.cpu.req_asid, req_vpn);
  val r_refill_tag   = Reg(resetVal = Bits(0, ASID_BITS+VPN_BITS));
  val r_refill_waddr = Reg(resetVal = UFix(0, addr_bits));
  val repl_count = Reg(resetVal = UFix(0, addr_bits));
  
  val tag_ram = Mem(entries, io.ptw.resp_val, r_refill_waddr.toUFix, io.ptw.resp_ppn);
  
  tag_cam.io.clear      := io.cpu.invalidate;
  tag_cam.io.tag        := lookup_tag;
  tag_cam.io.write      := io.ptw.resp_val;
  tag_cam.io.write_tag  := r_refill_tag;
  tag_cam.io.write_addr := r_refill_waddr;
  val tag_hit_addr = tag_cam.io.hit_addr;
  
  // extract fields from status register
  val status_mode = io.cpu.status(6).toBool; // user/supervisor mode
  val status_vm   = io.cpu.status(16).toBool // virtual memory enable
  
  // extract fields from PT permission bits
  val ptw_perm_ur = io.ptw.resp_perm(1);
  val ptw_perm_uw = io.ptw.resp_perm(2);
  val ptw_perm_sr = io.ptw.resp_perm(4);
  val ptw_perm_sw = io.ptw.resp_perm(5);
  
  // permission bit arrays
  val ur_array = Reg(resetVal = Bits(0, entries)); // user execute permission
  val uw_array = Reg(resetVal = Bits(0, entries)); // user execute permission
  val sr_array = Reg(resetVal = Bits(0, entries)); // supervisor execute permission
  val sw_array = Reg(resetVal = Bits(0, entries)); // supervisor execute permission
  when (io.ptw.resp_val) {
    ur_array <== ur_array.bitSet(r_refill_waddr, ptw_perm_ur);
    uw_array <== ur_array.bitSet(r_refill_waddr, ptw_perm_uw);
    sr_array <== sr_array.bitSet(r_refill_waddr, ptw_perm_sr);
    sw_array <== sw_array.bitSet(r_refill_waddr, ptw_perm_sw);
  }
 
  // when the page table lookup reports an error, set all permission
  // bits to 0 so the next access will cause an exception
  when (io.ptw.resp_err) {
    ur_array <== ur_array.bitSet(r_refill_waddr, Bool(false));
    uw_array <== ur_array.bitSet(r_refill_waddr, Bool(false));
    sr_array <== sr_array.bitSet(r_refill_waddr, Bool(false));
    sw_array <== sw_array.bitSet(r_refill_waddr, Bool(false));
  }
 
  // high if there are any unused (invalid) entries in the TLB
  val invalid_entry = (tag_cam.io.valid_bits != ~Bits(0,entries));
  val ie_enc = new priorityEncoder(entries);
  ie_enc.io.in := ~tag_cam.io.valid_bits.toUFix;
  val ie_addr = ie_enc.io.out;
  
  val repl_waddr = Mux(invalid_entry, ie_addr, repl_count).toUFix;
  
  val tag_hit = io.cpu.req_val && tag_cam.io.hit;
  val lookup_miss = (state === s_ready) && status_vm && !tag_hit;

  when (lookup_miss) {
    r_refill_tag <== lookup_tag;
    r_refill_waddr <== repl_waddr;
    when (!invalid_entry) {
      repl_count <== repl_count + UFix(1);
    }
  }

  io.cpu.xcpt_ld :=
    status_vm && tag_hit && req_load &&
    !((status_mode && sw_array(tag_hit_addr).toBool) ||
     (!status_mode && uw_array(tag_hit_addr).toBool));

  io.cpu.xcpt_st :=
    status_vm && tag_hit && req_store &&
    !((status_mode && sr_array(tag_hit_addr).toBool) ||
     (!status_mode && ur_array(tag_hit_addr).toBool));

  io.cpu.req_rdy   := (state === s_ready);
  io.cpu.resp_miss := lookup_miss;
  io.cpu.resp_val  := Mux(status_vm, tag_hit, io.cpu.req_val);
  io.cpu.resp_addr  := 
    Mux(status_vm, Cat(tag_ram(tag_hit_addr), req_idx),
      io.cpu.req_addr(PADDR_BITS-1,0)).toUFix;
  
  io.ptw.req_val := (state === s_request);
  io.ptw.req_vpn := r_refill_tag(VPN_BITS-1,0);
  
  // control state machine
  switch (state) {
    is (s_ready) {
      when (status_vm && io.cpu.req_val && !tag_hit) {
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