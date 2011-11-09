package Top
{

import Chisel._;
import Node._;
import Constants._;
import scala.math._;

class ioCAM(addr_bits: Int, tag_bits: Int) extends Bundle {
    val tag          = Bits(tag_bits, 'input);
    val hit          = Bool('output);
    val hit_addr     = UFix(addr_bits, 'output);
    
    val write        = Bool('input);
    val write_tag    = Bits(tag_bits, 'input);
    val write_addr    = UFix(addr_bits, 'input);
}

class rocketCAM(entries: Int, addr_bits: Int, tag_bits: Int) extends Component {
    val io = new ioCAM(addr_bits, tag_bits);
    val cam_tags = Mem(entries, io.write, io.write_addr, io.write_tag);

    val l_hit      = Wire() { Bool() };
    val l_hit_addr = Wire() { UFix() };
    
    for (i <- 0 to entries-1) {
        when (cam_tags(UFix(i)) === io.tag) {
            l_hit      <== Bool(true);
            l_hit_addr <== UFix(i,addr_bits);
        }
    }
    
    l_hit <== Bool(false);
    l_hit_addr <== UFix(0, addr_bits);
    
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
class ioITLB_CPU extends Bundle
{
  // status bits (from PCR), to check current permission and whether VM is enabled
  val status = Bits(17, 'input);
  // invalidate all TLB entries
  val invalidate = Bool('input);
  // lookup requests
  val req_val  = Bool('input);
  val req_rdy  = Bool('output);
  val req_asid = Bits(ASID_BITS, 'input);
  val req_vpn  = Bits(VPN_BITS, 'input);
  // lookup responses
  val resp_val = Bool('output);
  val resp_ppn = Bits(PPN_BITS, 'output);
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
  
  val lookup_tag = Cat(io.cpu.req_asid, io.cpu.req_vpn);
  val r_refill_tag   = Reg(resetVal = Bits(0, ASID_BITS+VPN_BITS));
  val r_refill_waddr = Reg(resetVal = UFix(0, addr_bits));
  val repl_count = Reg(resetVal = UFix(0, addr_bits));
  
  val tag_ram = Mem(entries, io.ptw.resp_val, r_refill_waddr.toUFix, io.ptw.resp_ppn);
  
  tag_cam.io.tag        := lookup_tag;
  tag_cam.io.write      := io.ptw.resp_val;
  tag_cam.io.write_tag  := r_refill_tag;
  tag_cam.io.write_addr := r_refill_waddr;
  val tag_hit_addr = tag_cam.io.hit_addr;
  
  // extract fields from status register
  val status_mode = io.cpu.status(6).toBool;
  val status_vm   = io.cpu.status(16).toBool
  
  // extract fields from PT permission bits
  val ptw_perm_ux = io.ptw.resp_perm(4);
  val ptw_perm_sx = io.ptw.resp_perm(7);
  
  // valid bit array
  val vb_array = Reg(resetVal = Bits(0, entries));
  when (io.cpu.invalidate) {
    vb_array <== Bits(0, entries);
  }
  when (io.ptw.resp_val) {
    vb_array <== vb_array.bitSet(r_refill_waddr, Bool(true));
  }
  
  // permission bit arrays
  val ux_array = Reg(resetVal = Bits(0, entries)); // user execute permission
  val sx_array = Reg(resetVal = Bits(0, entries)); // supervisor execute permission
  when (io.ptw.resp_val) {
    ux_array <== ux_array.bitSet(r_refill_waddr, ptw_perm_ux);
    sx_array <== ux_array.bitSet(r_refill_waddr, ptw_perm_sx);
  }
 
  // high if there are any unused (invalid) entries in the ITLB
//   val invalid_entry = orR(~vb_array);
  val invalid_entry = ~vb_array.toUFix.orR();
  val ie_enc = new priorityEncoder(entries);
  ie_enc.io.in := vb_array.toUFix;
  val ie_addr = ie_enc.io.out;
  
  val repl_waddr = Mux(invalid_entry, ie_addr, repl_count).toUFix;
  
  val tag_hit = tag_cam.io.hit && vb_array(tag_hit_addr).toBool;
  val lookup_miss = (state === s_ready) && status_vm && io.cpu.req_val && !tag_hit;

  when (lookup_miss) {
    r_refill_tag <== lookup_tag;
    r_refill_waddr <== repl_waddr;
    when (!invalid_entry) {
      repl_count <== repl_count + UFix(1);
    }
  }

  val itlb_exception = 
    tag_hit && 
    ((status_mode && !sx_array(tag_hit_addr).toBool) ||
    (!status_mode && !ux_array(tag_hit_addr).toBool));
  
  io.cpu.req_rdy   := (state === s_ready);
  io.cpu.resp_val  := Mux(status_vm, tag_hit, io.cpu.req_val);
  io.cpu.resp_ppn  := Mux(status_vm, io.cpu.req_vpn(PPN_BITS-1, 0), tag_ram(tag_hit_addr));
  io.cpu.exception := itlb_exception;
  
  io.ptw.req_val := (state === s_request);
  io.ptw.req_vpn := r_refill_tag;
  
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
      when (io.ptw.resp_val) {
        state <== s_ready;
      }
    }
  }  
}
}