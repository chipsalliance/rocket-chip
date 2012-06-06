package rocket

import Chisel._;
import Node._;
import Constants._;
import scala.math._;

class ioCAM(entries: Int, addr_bits: Int, tag_bits: Int) extends Bundle {
    val clear        = Bool(INPUT);
    val clear_hit    = Bool(INPUT)
    val tag          = Bits(tag_bits, INPUT);
    val hit          = Bool(OUTPUT);
    val hit_addr     = UFix(addr_bits, OUTPUT);
    val valid_bits   = Bits(entries, OUTPUT);
    
    val write        = Bool(INPUT);
    val write_tag    = Bits(tag_bits, INPUT);
    val write_addr    = UFix(addr_bits, INPUT);
}

class rocketCAM(entries: Int, tag_bits: Int) extends Component {
  val addr_bits = ceil(log(entries)/log(2)).toInt;
  val io = new ioCAM(entries, addr_bits, tag_bits);
  val cam_tags = Vec(entries) { Reg() { Bits(width = tag_bits) } }
  val mux = (new Mux1H(entries)) { Bits(width = addr_bits) }

  val vb_array = Reg(resetVal = Bits(0, entries));
  when (io.write) {
    vb_array := vb_array.bitSet(io.write_addr, Bool(true));
    cam_tags(io.write_addr) := io.write_tag
  }
  when (io.clear) {
    vb_array := Bits(0, entries);
  }
  .elsewhen (io.clear_hit) {
    vb_array := vb_array & ~mux.io.sel.toBits
  }
  
  var l_hit = Bool(false)
  for (i <- 0 to entries-1) {
    val my_hit = vb_array(UFix(i)).toBool && (cam_tags(i) === io.tag)
    l_hit = l_hit || my_hit
    mux.io.in(i) := Bits(i)
    mux.io.sel(i) := my_hit
  }
  
  io.valid_bits := vb_array;
  io.hit := l_hit;
  io.hit_addr := mux.io.out.toUFix;
}

class PseudoLRU(n: Int)
{
  val state = Reg() { Bits(width = n) }
  def access(way: UFix) = {
    var next_state = state
    var idx = UFix(1,1)
    for (i <- log2up(n)-1 to 0 by -1) {
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
    for (i <- 0 until log2up(n))
      idx = Cat(idx, state(idx))
    idx(log2up(n)-1,0)
  }
}

// interface between TLB and PTW
class ioTLB_PTW extends Bundle
{
  // requests
  val req_val = Bool(OUTPUT);
  val req_rdy = Bool(INPUT);
  val req_vpn = Bits(VPN_BITS, OUTPUT);
  // responses
  val resp_val = Bool(INPUT);
  val resp_err = Bool(INPUT);
  val resp_ppn = Bits(PPN_BITS, INPUT);
  val resp_perm = Bits(PERM_BITS, INPUT);
}

// interface between ITLB and fetch stage of pipeline
class ioITLB_CPU(view: List[String] = null) extends Bundle(view)
{
  // status bits (from PCR), to check current permission and whether VM is enabled
  val status = Bits(32, INPUT);
  // invalidate all TLB entries
  val invalidate = Bool(INPUT);
  // lookup requests
  val req_val  = Bool(INPUT);
  val req_rdy  = Bool(OUTPUT);
  val req_asid = Bits(ASID_BITS, INPUT);
  val req_vpn  = UFix(VPN_BITS+1, INPUT);
  // lookup responses
  val resp_miss = Bool(OUTPUT);
//   val resp_val = Bool(OUTPUT);
  val resp_ppn = UFix(PPN_BITS, OUTPUT);
  val exception = Bool(OUTPUT);
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
  
  val r_cpu_req_val     = Reg(resetVal = Bool(false));
  val r_cpu_req_vpn     = Reg() { Bits() };
  val r_cpu_req_asid    = Reg() { Bits() };
  val r_refill_tag   = Reg() { Bits() };
  val r_refill_waddr = Reg() { UFix() };
  
  when (io.cpu.req_val && io.cpu.req_rdy) { 
    r_cpu_req_vpn   := io.cpu.req_vpn;
    r_cpu_req_asid  := io.cpu.req_asid;
    r_cpu_req_val   := Bool(true);
  }
  .otherwise {
    r_cpu_req_val   := Bool(false);
  }

  val bad_va = r_cpu_req_vpn(VPN_BITS) != r_cpu_req_vpn(VPN_BITS-1);
  
  val tag_cam = new rocketCAM(entries, ASID_BITS+VPN_BITS);
  val tag_ram = Mem(entries) { io.ptw.resp_ppn.clone }
  when (io.ptw.resp_val) { tag_ram(r_refill_waddr) := io.ptw.resp_ppn }
  
  val lookup_tag = Cat(r_cpu_req_asid, r_cpu_req_vpn);
  tag_cam.io.clear      := io.cpu.invalidate;
  tag_cam.io.clear_hit  := io.cpu.exception
  tag_cam.io.tag        := lookup_tag;
  tag_cam.io.write      := io.ptw.resp_val || io.ptw.resp_err;
  tag_cam.io.write_tag  := r_refill_tag;
  tag_cam.io.write_addr := r_refill_waddr;
  val tag_hit            = tag_cam.io.hit || bad_va;
  val tag_hit_addr       = tag_cam.io.hit_addr;
  
  // extract fields from status register
  val status_s  = io.cpu.status(SR_S).toBool; // user/supervisor mode
  val status_u  = !status_s;
  val status_vm = io.cpu.status(SR_VM).toBool // virtual memory enable
  
  // extract fields from PT permission bits
  val ptw_perm_ux = io.ptw.resp_perm(0);
  val ptw_perm_sx = io.ptw.resp_perm(3);
  
  // permission bit arrays
  val ux_array = Reg(resetVal = Bits(0, entries)); // user execute permission
  val sx_array = Reg(resetVal = Bits(0, entries)); // supervisor execute permission
  when (io.ptw.resp_val) {
    ux_array := ux_array.bitSet(r_refill_waddr, ptw_perm_ux);
    sx_array := sx_array.bitSet(r_refill_waddr, ptw_perm_sx);
  }
 
  // when the page table lookup reports an error, set both execute permission
  // bits to 0 so the next access will cause an exceptions
  when (io.ptw.resp_err) {
    ux_array := ux_array.bitSet(r_refill_waddr, Bool(false));
    sx_array := sx_array.bitSet(r_refill_waddr, Bool(false));
  }
 
  // high if there are any unused entries in the ITLB
  val has_invalid_entry = !tag_cam.io.valid_bits.andR
  val invalid_entry = PriorityEncoder(~tag_cam.io.valid_bits)
  val plru = new PseudoLRU(entries)
  val repl_waddr = Mux(has_invalid_entry, invalid_entry, plru.replace).toUFix;
  
  val lookup = (state === s_ready) && r_cpu_req_val;
  val lookup_hit  = lookup && tag_hit;
  val lookup_miss = lookup && !tag_hit;
  val tlb_hit  = status_vm && lookup_hit;
  val tlb_miss = status_vm && lookup_miss;

  when (tlb_miss) {
    r_refill_tag := lookup_tag;
    r_refill_waddr := repl_waddr;
  }
  when (tlb_hit) {
    plru.access(tag_hit_addr)
  }

  val access_fault = 
    tlb_hit &&
    ((status_s && !sx_array(tag_hit_addr).toBool) ||
     (status_u && !ux_array(tag_hit_addr).toBool) ||
     bad_va);

  io.cpu.exception := access_fault;
  io.cpu.req_rdy   := Mux(status_vm, (state === s_ready) && (!r_cpu_req_val || tag_hit), Bool(true));
  io.cpu.resp_miss := tlb_miss || (state != s_ready);
  io.cpu.resp_ppn := Mux(status_vm, tag_ram(tag_hit_addr), r_cpu_req_vpn(PPN_BITS-1,0)).toUFix;
  
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
