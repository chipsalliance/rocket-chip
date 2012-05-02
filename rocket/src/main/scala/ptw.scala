package rocket

import Chisel._;
import Node._;
import Constants._;
import scala.math._;

class ioHellaCacheArbiter(n: Int) extends Bundle
{
  val requestor = Vec(n) { new ioHellaCache() }.flip
  val mem = new ioHellaCache
}

class rocketHellaCacheArbiter(n: Int) extends Component
{
  val io = new ioHellaCacheArbiter(n)
  require(DCACHE_TAG_BITS >= log2up(n) + CPU_TAG_BITS)

  var req_val = Bool(false)
  var req_rdy = io.mem.req.ready
  for (i <- 0 until n)
  {
    io.requestor(i).req.ready := req_rdy
    req_val = req_val || io.requestor(i).req.valid
    req_rdy = req_rdy && !io.requestor(i).req.valid
  }

  var req_cmd  = io.requestor(n-1).req.bits.cmd
  var req_type = io.requestor(n-1).req.bits.typ
  var req_idx  = io.requestor(n-1).req.bits.idx
  var req_ppn  = io.requestor(n-1).req.bits.ppn
  var req_data = io.requestor(n-1).req.bits.data
  var req_kill = io.requestor(n-1).req.bits.kill
  var req_tag  = io.requestor(n-1).req.bits.tag
  for (i <- n-1 to 0 by -1)
  {
    val r = io.requestor(i).req
    req_cmd  = Mux(r.valid, r.bits.cmd, req_cmd)
    req_type = Mux(r.valid, r.bits.typ, req_type)
    req_idx  = Mux(r.valid, r.bits.idx, req_idx)
    req_ppn  = Mux(Reg(r.valid), r.bits.ppn, req_ppn)
    req_data = Mux(Reg(r.valid), r.bits.data, req_data)
    req_kill = Mux(Reg(r.valid), r.bits.kill, req_kill)
    req_tag  = Mux(r.valid, Cat(r.bits.tag, UFix(i, log2up(n))), req_tag)
  }

  io.mem.req.valid     := req_val
  io.mem.req.bits.cmd  := req_cmd
  io.mem.req.bits.typ  := req_type
  io.mem.req.bits.idx  := req_idx
  io.mem.req.bits.ppn  := req_ppn
  io.mem.req.bits.data := req_data
  io.mem.req.bits.kill := req_kill
  io.mem.req.bits.tag  := req_tag

  for (i <- 0 until n)
  {
    val r = io.requestor(i).resp
    val x = io.requestor(i).xcpt
    val tag_hit = io.mem.resp.bits.tag(log2up(n)-1,0) === UFix(i)
    x.ma.ld := io.mem.xcpt.ma.ld && Reg(io.requestor(i).req.valid)
    x.ma.st := io.mem.xcpt.ma.st && Reg(io.requestor(i).req.valid)
    r.valid             := io.mem.resp.valid && tag_hit
    r.bits.miss         := io.mem.resp.bits.miss && tag_hit
    r.bits.nack         := io.mem.resp.bits.nack && Reg(io.requestor(i).req.valid)
    r.bits.replay       := io.mem.resp.bits.replay && tag_hit
    r.bits.data         := io.mem.resp.bits.data
    r.bits.data_subword := io.mem.resp.bits.data_subword
    r.bits.typ          := io.mem.resp.bits.typ
    r.bits.tag          := io.mem.resp.bits.tag >> UFix(log2up(n))
  }
}

class ioPTW extends Bundle
{
  val itlb  = (new ioTLB_PTW).flip
  val dtlb  = (new ioTLB_PTW).flip
  val vitlb = (new ioTLB_PTW).flip
  val mem   = new ioHellaCache
  val ptbr  = UFix(PADDR_BITS, INPUT)
}

class rocketPTW extends Component
{
  val io = new ioPTW
  
  val levels = 3
  val bitsPerLevel = VPN_BITS/levels
  require(VPN_BITS == levels * bitsPerLevel)

  val count = Reg() { UFix(width = log2up(levels)) }
  val s_ready :: s_req :: s_wait :: s_done :: s_error :: Nil = Enum(5) { UFix() };
  val state = Reg(resetVal = s_ready);
  
  val r_req_vpn = Reg() { Bits() }
  val r_req_dest = Reg() { Bits() }
  
  val req_addr = Reg() { Bits() }
  val r_resp_ppn = Reg() { Bits() };
  val r_resp_perm = Reg() { Bits() };
  
  val vpn_idxs = (1 until levels).map(i => r_req_vpn((levels-i)*bitsPerLevel-1, (levels-i-1)*bitsPerLevel))
  val vpn_idx = (2 until levels).foldRight(vpn_idxs(0))((i,j) => Mux(count === UFix(i-1), vpn_idxs(i-1), j))
  val req_val = io.itlb.req_val || io.dtlb.req_val || io.vitlb.req_val
  
  // give ITLB requests priority over DTLB requests
  val req_itlb_val = io.itlb.req_val;
  val req_dtlb_val = io.dtlb.req_val && !io.itlb.req_val;
  val req_vitlb_val = io.vitlb.req_val && !io.itlb.req_val && !io.dtlb.req_val
  
  when ((state === s_ready) && req_itlb_val) {
    r_req_vpn  := io.itlb.req_vpn;
    r_req_dest := Bits(0)
    req_addr := Cat(io.ptbr(PADDR_BITS-1,PGIDX_BITS), io.itlb.req_vpn(VPN_BITS-1,VPN_BITS-bitsPerLevel), Bits(0,3))
  }

  when ((state === s_ready) && req_dtlb_val) {
    r_req_vpn  := io.dtlb.req_vpn;
    r_req_dest := Bits(1)
    req_addr := Cat(io.ptbr(PADDR_BITS-1,PGIDX_BITS), io.dtlb.req_vpn(VPN_BITS-1,VPN_BITS-bitsPerLevel), Bits(0,3))
  }
  
  when ((state === s_ready) && req_vitlb_val) {
    r_req_vpn  := io.vitlb.req_vpn;
    r_req_dest := Bits(2)
    req_addr := Cat(io.ptbr(PADDR_BITS-1,PGIDX_BITS), io.vitlb.req_vpn(VPN_BITS-1,VPN_BITS-bitsPerLevel), Bits(0,3))
  }

  val dmem_resp_val = Reg(io.mem.resp.valid, resetVal = Bool(false))
  when (dmem_resp_val) {
    req_addr := Cat(io.mem.resp.bits.data_subword(PADDR_BITS-1, PGIDX_BITS), vpn_idx, Bits(0,3))
    r_resp_perm := io.mem.resp.bits.data_subword(9,4);
    r_resp_ppn  := io.mem.resp.bits.data_subword(PADDR_BITS-1, PGIDX_BITS);
  }
  
  io.mem.req.valid     := state === s_req
  io.mem.req.bits.cmd  := M_XRD
  io.mem.req.bits.typ  := MT_D
  io.mem.req.bits.idx  := req_addr(PGIDX_BITS-1,0)
  io.mem.req.bits.ppn  := Reg(req_addr(PADDR_BITS-1,PGIDX_BITS))
  io.mem.req.bits.kill := Bool(false)
  
  val resp_val = state === s_done
  val resp_err = state === s_error
  
  val resp_ptd = io.mem.resp.bits.data_subword(1,0) === Bits(1)
  val resp_pte = io.mem.resp.bits.data_subword(1,0) === Bits(2)
  
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
 
  val resp_ppns = (0 until levels-1).map(i => Cat(r_resp_ppn(PPN_BITS-1, VPN_BITS-bitsPerLevel*(i+1)), r_req_vpn(VPN_BITS-1-bitsPerLevel*(i+1), 0)))
  val resp_ppn = (0 until levels-1).foldRight(r_resp_ppn)((i,j) => Mux(count === UFix(i), resp_ppns(i), j))
      
  io.itlb.resp_ppn  := resp_ppn;
  io.dtlb.resp_ppn  := resp_ppn;
  io.vitlb.resp_ppn := resp_ppn;

  // control state machine
  switch (state) {
    is (s_ready) {
      when (req_val) {
        state := s_req;
      }
      count := UFix(0)
    }
    is (s_req) {
      when (io.mem.req.ready) {
        state := s_wait;
      }
    }
    is (s_wait) {
      when (io.mem.resp.bits.nack) {
        state := s_req
      }
      when (dmem_resp_val) {
        when (resp_pte) { // page table entry
          state := s_done
        }
        .otherwise {
          count := count + UFix(1)
          when (resp_ptd && count < UFix(levels-1)) {
            state := s_req
          }
          .otherwise {
            state := s_error
          }
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
