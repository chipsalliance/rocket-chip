package rocket

import Chisel._;
import Node._;
import Constants._;
import scala.math._;

class ioDmemArbiter(n: Int) extends Bundle
{
  val dmem = new ioDmem().flip
  val requestor = Vec(n) { new ioDmem() }
}

class rocketDmemArbiter(n: Int) extends Component
{
  val io = new ioDmemArbiter(n)
  require(DCACHE_TAG_BITS >= log2up(n) + CPU_TAG_BITS)

  var req_val = Bool(false)
  var req_rdy = io.dmem.req_rdy
  for (i <- 0 until n)
  {
    io.requestor(i).req_rdy := req_rdy
    req_val = req_val || io.requestor(i).req_val
    req_rdy = req_rdy && !io.requestor(i).req_val
  }

  var req_cmd = io.requestor(n-1).req_cmd
  var req_type = io.requestor(n-1).req_type
  var req_idx = io.requestor(n-1).req_idx
  var req_ppn = io.requestor(n-1).req_ppn
  var req_data = io.requestor(n-1).req_data
  var req_tag = io.requestor(n-1).req_tag
  var req_kill = io.requestor(n-1).req_kill
  for (i <- n-1 to 0 by -1)
  {
    req_cmd = Mux(io.requestor(i).req_val, io.requestor(i).req_cmd, req_cmd)
    req_type = Mux(io.requestor(i).req_val, io.requestor(i).req_type, req_type)
    req_idx = Mux(io.requestor(i).req_val, io.requestor(i).req_idx, req_idx)
    req_ppn = Mux(Reg(io.requestor(i).req_val), io.requestor(i).req_ppn, req_ppn)
    req_data = Mux(Reg(io.requestor(i).req_val), io.requestor(i).req_data, req_data)
    req_tag = Mux(io.requestor(i).req_val, Cat(io.requestor(i).req_tag, UFix(i, log2up(n))), req_tag)
    req_kill = Mux(Reg(io.requestor(i).req_val), io.requestor(i).req_kill, req_kill)
  }

  io.dmem.req_val := req_val
  io.dmem.req_cmd := req_cmd
  io.dmem.req_type := req_type
  io.dmem.req_idx := req_idx
  io.dmem.req_ppn := req_ppn
  io.dmem.req_data := req_data
  io.dmem.req_tag := req_tag
  io.dmem.req_kill := req_kill

  for (i <- 0 until n)
  {
    val tag_hit = io.dmem.resp_tag(log2up(n)-1,0) === UFix(i)
    io.requestor(i).xcpt_ma_ld := io.dmem.xcpt_ma_ld && Reg(io.requestor(i).req_val)
    io.requestor(i).xcpt_ma_st := io.dmem.xcpt_ma_st && Reg(io.requestor(i).req_val)
    io.requestor(i).resp_nack := io.dmem.resp_nack && Reg(io.requestor(i).req_val)
    io.requestor(i).resp_miss := io.dmem.resp_miss && tag_hit
    io.requestor(i).resp_val := io.dmem.resp_val && tag_hit
    io.requestor(i).resp_replay := io.dmem.resp_replay && tag_hit
    io.requestor(i).resp_data := io.dmem.resp_data
    io.requestor(i).resp_data_subword := io.dmem.resp_data_subword
    io.requestor(i).resp_type := io.dmem.resp_type
    io.requestor(i).resp_tag := io.dmem.resp_tag >> UFix(log2up(n))
  }
}

class ioPTW extends Bundle
{
  val itlb = new ioTLB_PTW().flip
  val dtlb = new ioTLB_PTW().flip
  val vitlb = new ioTLB_PTW().flip
  val dmem = new ioDmem().flip
  val ptbr = UFix(PADDR_BITS, INPUT);
}

class rocketPTW extends Component
{
  val io = new ioPTW();
  
  val s_ready :: s_l1_req :: s_l1_wait :: s_l1_fake :: s_l2_req :: s_l2_wait :: s_l2_fake:: s_l3_req :: s_l3_wait :: s_done :: s_error :: Nil = Enum(11) { UFix() };
  val state = Reg(resetVal = s_ready);
  
  val r_req_vpn = Reg() { Bits() }
  val r_req_dest = Reg() { Bits() }
  
  val req_addr = Reg() { UFix() };
  val r_resp_ppn = Reg() { Bits() };
  val r_resp_perm = Reg() { Bits() };
  
  val vpn_idx = Mux(state === s_l2_wait, r_req_vpn(9,0), r_req_vpn(19,10)); 
  val req_val = io.itlb.req_val || io.dtlb.req_val || io.vitlb.req_val
  
  // give ITLB requests priority over DTLB requests
  val req_itlb_val = io.itlb.req_val;
  val req_dtlb_val = io.dtlb.req_val && !io.itlb.req_val;
  val req_vitlb_val = io.vitlb.req_val && !io.itlb.req_val && !io.dtlb.req_val
  
  when ((state === s_ready) && req_itlb_val) {
    r_req_vpn  := io.itlb.req_vpn;
    r_req_dest := Bits(0)
    req_addr := Cat(io.ptbr(PADDR_BITS-1,PGIDX_BITS), io.itlb.req_vpn(VPN_BITS-1,VPN_BITS-10), Bits(0,3)).toUFix;
  }

  when ((state === s_ready) && req_dtlb_val) {
    r_req_vpn  := io.dtlb.req_vpn;
    r_req_dest := Bits(1)
    req_addr := Cat(io.ptbr(PADDR_BITS-1,PGIDX_BITS), io.dtlb.req_vpn(VPN_BITS-1,VPN_BITS-10), Bits(0,3)).toUFix;
  }
  
  when ((state === s_ready) && req_vitlb_val) {
    r_req_vpn  := io.vitlb.req_vpn;
    r_req_dest := Bits(2)
    req_addr := Cat(io.ptbr(PADDR_BITS-1,PGIDX_BITS), io.vitlb.req_vpn(VPN_BITS-1,VPN_BITS-10), Bits(0,3)).toUFix;
  }

  val dmem_resp_val = Reg(io.dmem.resp_val, resetVal = Bool(false))
  when (dmem_resp_val) {
    req_addr := Cat(io.dmem.resp_data_subword(PADDR_BITS-1, PGIDX_BITS), vpn_idx, Bits(0,3)).toUFix;
    r_resp_perm := io.dmem.resp_data_subword(9,4);
    r_resp_ppn  := io.dmem.resp_data_subword(PADDR_BITS-1, PGIDX_BITS);
  }
  
  io.dmem.req_val :=
    (state === s_l1_req) ||
    (state === s_l2_req) ||
    (state === s_l3_req);
    
  io.dmem.req_cmd  := M_XRD;
  io.dmem.req_type := MT_D;
  io.dmem.req_idx := req_addr(PGIDX_BITS-1,0);
  io.dmem.req_ppn := Reg(req_addr(PADDR_BITS-1,PGIDX_BITS))
  io.dmem.req_kill := Bool(false)
  
  val resp_val = (state === s_done) || (state === s_l1_fake) || (state === s_l2_fake);
  val resp_err = (state === s_error);
  
  val resp_ptd = (io.dmem.resp_data_subword(1,0) === Bits(1,2));
  val resp_pte = (io.dmem.resp_data_subword(1,0) === Bits(2,2));
  
  io.itlb.req_rdy   := (state === s_ready)
  io.dtlb.req_rdy   := (state === s_ready) && !io.itlb.req_val
  io.vitlb.req_rdy  := (state === s_ready) && !io.itlb.req_val && !io.dtlb.req_val
  io.itlb.resp_val  := r_req_dest === Bits(0) && resp_val
  io.dtlb.resp_val  := r_req_dest === Bits(1) && resp_val
  io.vitlb.resp_val := r_req_dest === Bits(2) && resp_val
  io.itlb.resp_err  := r_req_dest === Bits(0) && resp_err
  io.dtlb.resp_err  := r_req_dest === Bits(1) && resp_err
  io.vitlb.resp_err := r_req_dest === Bits(2) && resp_err
  io.itlb.resp_perm := r_resp_perm
  io.dtlb.resp_perm := r_resp_perm
  io.vitlb.resp_perm:= r_resp_perm
  
  val resp_ppn =
    Mux(state === s_l1_fake, Cat(r_resp_ppn(PPN_BITS-1, PPN_BITS-7),  r_req_vpn(VPN_BITS-11, 0)),
    Mux(state === s_l2_fake, Cat(r_resp_ppn(PPN_BITS-1, PPN_BITS-17), r_req_vpn(VPN_BITS-21, 0)),
      r_resp_ppn));
      
  io.itlb.resp_ppn  := resp_ppn;
  io.dtlb.resp_ppn  := resp_ppn;
  io.vitlb.resp_ppn := resp_ppn;

  // control state machine
  switch (state) {
    is (s_ready) {
      when (req_val) {
        state := s_l1_req;
      }
    }
    // level 1
    is (s_l1_req) {
      when (io.dmem.req_rdy) {
        state := s_l1_wait;
      }
    }
    is (s_l1_wait) {
      when (io.dmem.resp_nack) {
        state := s_l1_req
      }
      when (dmem_resp_val) {
        when (resp_ptd) { // page table descriptor
          state := s_l2_req;
        }
        .elsewhen (resp_pte) { // page table entry
          state := s_l1_fake;
        }
        .otherwise {
          state := s_error;
        }
      }
    }
    is (s_l1_fake) {
      state := s_ready;
    }
    // level 2
    is (s_l2_req) {
      when (io.dmem.req_rdy) {
        state := s_l2_wait;
      }
    }
    is (s_l2_wait) {
      when (io.dmem.resp_nack) {
        state := s_l2_req
      }
      when (dmem_resp_val) {
        when (resp_ptd) { // page table descriptor
          state := s_l3_req;
        }
        .elsewhen (resp_pte) { // page table entry
          state := s_l2_fake;
        }
        .otherwise {
          state := s_error;
        }
      }
    }
    is (s_l2_fake) {
      state := s_ready;
    }
    // level 3
    is (s_l3_req) {
      when (io.dmem.req_rdy) {
        state := s_l3_wait;
      }
    }
    is (s_l3_wait) {
      when (io.dmem.resp_nack) {
        state := s_l3_req
      }
      when (dmem_resp_val) {
        when (resp_pte) { // page table entry
          state := s_done;
        }
        .otherwise {
          state := s_error;
        }
      }
    }  
    is (s_done) {
      state := s_ready;
    }
    is (s_error) {
      state := s_ready;
    }
  }
}
