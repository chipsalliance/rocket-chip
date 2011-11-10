package Top
{

import Chisel._;
import Node._;
import Constants._;
import scala.math._;

class ioCAM(entries: Int, addr_bits: Int, tag_bits: Int) extends Bundle {
    val clear        = Bool('input);
    val tag          = Bits(tag_bits, 'input);
    val hit          = Bool('output);
    val hit_addr     = UFix(addr_bits, 'output);
    val valid_bits   = Bits(entries, 'output);
    
    val write        = Bool('input);
    val write_tag    = Bits(tag_bits, 'input);
    val write_addr    = UFix(addr_bits, 'input);
}

class rocketCAM(entries: Int, addr_bits: Int, tag_bits: Int) extends Component {
  val io = new ioCAM(entries, addr_bits, tag_bits);
  val cam_tags = Mem(entries, io.write, io.write_addr, io.write_tag);

  val l_hit      = Wire() { Bool() };
  val l_hit_addr = Wire() { UFix() };
    
  val vb_array = Reg(resetVal = Bits(0, entries));
  when (io.clear) {
    vb_array <== Bits(0, entries);
  }
  when (io.write) {
    vb_array <== vb_array.bitSet(io.write_addr, Bool(true));
  }
    
  for (i <- 0 to entries-1) {
      when (vb_array(UFix(i)).toBool && (cam_tags(UFix(i)) === io.tag)) {
          l_hit      <== Bool(true);
          l_hit_addr <== UFix(i,addr_bits);
      }
  }
  
  l_hit <== Bool(false);
  l_hit_addr <== UFix(0, addr_bits);
  
  io.valid_bits := vb_array;
  io.hit := l_hit;
  io.hit_addr := l_hit_addr;
}

// interface between TLB and PTW
class ioTLB_PTW extends Bundle
{
  // requests
  val req_val = Bool('output);
  val req_rdy = Bool('input);
  val req_vpn = Bits(VPN_BITS, 'output);
  // responses
  val resp_val = Bool('input);
  val resp_err = Bool('input);
  val resp_ppn = Bits(PPN_BITS, 'input);
  val resp_perm = Bits(PERM_BITS, 'input);
}

// interface between ITLB and fetch stage of pipeline
class ioITLB_CPU(view: List[String] = null) extends Bundle(view)
{
  // status bits (from PCR), to check current permission and whether VM is enabled
  val status = Bits(17, 'input);
  // invalidate all TLB entries
  val invalidate = Bool('input);
  // lookup requests
  val req_val  = Bool('input);
  val req_rdy  = Bool('output);
  val req_asid = Bits(ASID_BITS, 'input);
  val req_addr  = UFix(VADDR_BITS, 'input);
  // lookup responses
  val resp_val = Bool('output);
  val resp_addr = UFix(PADDR_BITS, 'output);
  val exception = Bool('output);
}

class ioITLB extends Bundle
{
  val cpu = new ioITLB_CPU();
  val ptw = new ioTLB_PTW();
}

class rocketITLB(entries: Int) extends Component
{
  val addr_bits = ceil(log10(entries)/log10(2)).toInt;  
  val io = new ioITLB();

  val s_ready :: s_request :: s_wait :: Nil = Enum(3) { UFix() };
  val state = Reg(resetVal = s_ready);

  val tag_cam = new rocketCAM(entries, addr_bits, ASID_BITS+VPN_BITS);
  
  val req_vpn = io.cpu.req_addr(VADDR_BITS-1,PGIDX_BITS);
  val req_idx = io.cpu.req_addr(PGIDX_BITS-1,0);
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
  val ptw_perm_ux = io.ptw.resp_perm(0);
  val ptw_perm_sx = io.ptw.resp_perm(3);
  
  // permission bit arrays
  val ux_array = Reg(resetVal = Bits(0, entries)); // user execute permission
  val sx_array = Reg(resetVal = Bits(0, entries)); // supervisor execute permission
  when (io.ptw.resp_val) {
    ux_array <== ux_array.bitSet(r_refill_waddr, ptw_perm_ux);
    sx_array <== sx_array.bitSet(r_refill_waddr, ptw_perm_sx);
  }
 
  // when the page table lookup reports an error, set both execute permission
  // bits to 0 so the next access will cause an exceptions
  when (io.ptw.resp_err) {
    ux_array <== ux_array.bitSet(r_refill_waddr, Bool(false));
    sx_array <== sx_array.bitSet(r_refill_waddr, Bool(false));
  }
 
  // high if there are any unused entries in the ITLB
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

  val itlb_exception = 
    tag_hit && 
    !((status_mode && sx_array(tag_hit_addr).toBool) ||
     (!status_mode && ux_array(tag_hit_addr).toBool));
  
  io.cpu.req_rdy   := (state === s_ready);
  io.cpu.resp_val  := Mux(status_vm, tag_hit, io.cpu.req_val);
  io.cpu.resp_addr  := 
    Mux(status_vm, Cat(tag_ram(tag_hit_addr), req_idx),
      io.cpu.req_addr(PADDR_BITS-1,0)).toUFix;
  io.cpu.exception := status_vm && itlb_exception;
  
  io.ptw.req_val := (state === s_request);
  io.ptw.req_vpn := r_refill_tag(VPN_BITS-1,0);
  
  // control state machine
  switch (state) {
    is (s_ready) {
      when (status_vm && !tag_hit) {
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