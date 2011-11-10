package Top {

import Chisel._;
import Node._;
import Constants._;
import scala.math._;

class ioDmemArbiter extends Bundle
{
  val ptw = new ioDmem(List("req_val", "req_rdy", "req_cmd", "req_type", "req_addr", "resp_data", "resp_val"));
  val cpu = new ioDmem();
  val mem = new ioDmem().flip();
}

class rocketDmemArbiter extends Component
{
  val io = new ioDmemArbiter();
  
  io.mem.req_val  := io.ptw.req_val || io.cpu.req_val;
  io.mem.req_cmd  := Mux(io.ptw.req_val, io.ptw.req_cmd,  io.cpu.req_cmd);
  io.mem.req_type := Mux(io.ptw.req_val, io.ptw.req_type, io.cpu.req_type);
  io.mem.req_addr := Mux(io.ptw.req_val, io.ptw.req_addr, io.cpu.req_addr);
  io.mem.req_data := io.cpu.req_data;
  io.mem.req_tag  := Mux(io.ptw.req_val, Bits(0,5), io.cpu.req_tag);
  
  io.ptw.req_rdy   := io.mem.req_rdy;
  io.cpu.req_rdy   := io.mem.req_rdy && !io.ptw.req_val;  
  io.cpu.resp_miss := io.mem.resp_miss; // FIXME

  io.cpu.resp_val  := io.mem.resp_val && !io.mem.resp_tag(12).toBool;
  io.ptw.resp_val  := io.mem.resp_val &&  io.mem.resp_tag(12).toBool; 

  io.ptw.resp_data := io.mem.resp_data;
  io.cpu.resp_data := io.mem.resp_data;
  io.cpu.resp_tag  := io.mem.resp_tag;
  
}

class ioPTW extends Bundle
{
  val itlb = new ioTLB_PTW().flip();
//   val dtlb = new ioTLB_PTW.flip();
  val dmem = new ioDmem(List("req_val", "req_rdy", "req_cmd", "req_type", "req_addr", "resp_data", "resp_val")).flip();
  val ptbr = UFix(PADDR_BITS, 'input);
}

class rocketPTW extends Component
{
  val io = new ioPTW();
  
  val s_ready :: s_l1_req :: s_l1_wait :: s_l1_fake :: s_l2_req :: s_l2_wait :: s_l2_fake:: s_l3_req :: s_l3_wait :: s_done :: s_error :: Nil = Enum(11) { UFix() };
  val state = Reg(resetVal = s_ready);
  
  val r_req_vpn = Reg(resetVal = Bits(0,VPN_BITS));
  
  val req_addr = Reg(resetVal = UFix(0,PPN_BITS+PGIDX_BITS));
  val r_resp_ppn = Reg(resetVal = Bits(0,PPN_BITS));
  val r_resp_perm = Reg(resetVal = Bits(0,PERM_BITS));
  
  val vpn_idx = Mux(state === s_l2_wait, r_req_vpn(9,0), r_req_vpn(19,10)); 
  
  when ((state === s_ready) && io.itlb.req_val) {
    r_req_vpn  <== io.itlb.req_vpn;
    req_addr <== Cat(io.ptbr(PADDR_BITS-1,PGIDX_BITS), io.itlb.req_vpn(VPN_BITS-1,VPN_BITS-10)).toUFix;
  }
  when (io.dmem.resp_val) {
    req_addr <== Cat(io.dmem.resp_data(PADDR_BITS-1, PGIDX_BITS), vpn_idx).toUFix;
    r_resp_perm <== io.dmem.resp_data(9,4);
    r_resp_ppn  <== io.dmem.resp_data(PADDR_BITS-1, PGIDX_BITS);
  }
  
  io.dmem.req_val :=
    (state === s_l1_req) ||
    (state === s_l2_req) ||
    (state === s_l3_req);
    
  io.dmem.req_cmd  := M_PRD;
  io.dmem.req_type := MT_D;
  io.dmem.req_addr := req_addr;
  
  io.itlb.req_rdy   := (state === s_ready);
  io.itlb.resp_val  := (state === s_done) || (state === s_l1_fake) || (state === s_l2_fake);
  io.itlb.resp_err  := (state === s_error);
  io.itlb.resp_perm := r_resp_perm;
  io.itlb.resp_ppn  :=
    Mux(state === s_l1_fake, Cat(r_resp_ppn(PPN_BITS-1, PPN_BITS-7),  r_req_vpn(VPN_BITS-11, 0)),
    Mux(state === s_l2_fake, Cat(r_resp_ppn(PPN_BITS-1, PPN_BITS-17), r_req_vpn(VPN_BITS-21, 0)),
      r_resp_ppn));

  val resp_ptd = (io.dmem.resp_data(1,0) === Bits(1,2));
  val resp_pte = (io.dmem.resp_data(1,0) === Bits(2,2));
  
  // control state machine
  switch (state) {
    is (s_ready) {
      when (io.itlb.req_val) {
        state <== s_l1_req;
      }
    }
    // level 1
    is (s_l1_req) {
      when (io.dmem.req_rdy) {
        state <== s_l1_wait;
      }
    }
    is (s_l1_wait) {
      when (io.dmem.resp_val) {
        when (resp_ptd) { // page table descriptor
          state <== s_l2_req;
        }
        when (resp_pte) { // page table entry
          state <== s_l1_fake;
        }
        otherwise {
          state <== s_error;
        }
      }
    }
    is (s_l1_fake) {
      state <== s_ready;
    }
    // level 2
    is (s_l2_req) {
      when (io.dmem.req_rdy) {
        state <== s_l2_wait;
      }
    }
    is (s_l2_wait) {
      when (io.dmem.resp_val) {
        when (resp_ptd) { // page table descriptor
          state <== s_l3_req;
        }
        when (resp_pte) { // page table entry
          state <== s_l2_fake;
        }
        otherwise {
          state <== s_error;
        }
      }
    }
    is (s_l2_fake) {
      state <== s_ready;
    }
    // level 3
    is (s_l3_req) {
      when (io.dmem.req_rdy) {
        state <== s_l3_wait;
      }
    }
    is (s_l3_wait) {
      when (io.dmem.resp_val) {
        when (resp_pte) { // page table entry
          state <== s_done;
        }
        otherwise {
          state <== s_error;
        }
      }
    }  
    is (s_done) {
      state <== s_ready;
    }
    is (s_error) {
      state <== s_ready;
    }
  }
}

}