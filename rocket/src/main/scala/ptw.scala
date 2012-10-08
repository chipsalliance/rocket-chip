package rocket

import Chisel._
import Node._
import Constants._
import scala.math._

class ioPTW(n: Int) extends Bundle
{
  val requestor = Vec(n) { new ioTLB_PTW }.flip
  val mem   = new ioHellaCache
  val ptbr  = UFix(INPUT, PADDR_BITS)
}

class rocketPTW(n: Int) extends Component
{
  val io = new ioPTW(n)
  
  val levels = 3
  val bitsPerLevel = VPN_BITS/levels
  require(VPN_BITS == levels * bitsPerLevel)

  val count = Reg() { UFix(width = log2Up(levels)) }
  val s_ready :: s_req :: s_wait :: s_done :: s_error :: Nil = Enum(5) { UFix() };
  val state = Reg(resetVal = s_ready);
  
  val r_req_vpn = Reg() { Bits() }
  val r_req_dest = Reg() { Bits() }
  
  val req_addr = Reg() { Bits() }
  val r_resp_ppn = Reg() { Bits() };
  val r_resp_perm = Reg() { Bits() };
  
  val vpn_idxs = (1 until levels).map(i => r_req_vpn((levels-i)*bitsPerLevel-1, (levels-i-1)*bitsPerLevel))
  val vpn_idx = (2 until levels).foldRight(vpn_idxs(0))((i,j) => Mux(count === UFix(i-1), vpn_idxs(i-1), j))
 
  val req_rdy = state === s_ready
  var req_val = Bool(false)
  for (r <- io.requestor) {
    r.req_rdy := req_rdy && !req_val
    req_val = req_val || r.req_val
  }
  val req_dest = PriorityEncoder(io.requestor.map(_.req_val))
  val req_vpn = io.requestor.slice(0, n-1).foldRight(io.requestor(n-1).req_vpn)((r, v) => Mux(r.req_val, r.req_vpn, v))

  when (state === s_ready && req_val) {
    r_req_vpn := req_vpn
    r_req_dest := req_dest
    req_addr := Cat(io.ptbr(PADDR_BITS-1,PGIDX_BITS), req_vpn(VPN_BITS-1,VPN_BITS-bitsPerLevel), Bits(0,3))
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
 
  val resp_ppns = (0 until levels-1).map(i => Cat(r_resp_ppn(PPN_BITS-1, VPN_BITS-bitsPerLevel*(i+1)), r_req_vpn(VPN_BITS-1-bitsPerLevel*(i+1), 0)))
  val resp_ppn = (0 until levels-1).foldRight(r_resp_ppn)((i,j) => Mux(count === UFix(i), resp_ppns(i), j))

  for (i <- 0 until io.requestor.size) {
    val me = r_req_dest === UFix(i)
    io.requestor(i).resp_val := resp_val && me
    io.requestor(i).resp_err := resp_err && me
    io.requestor(i).resp_perm := r_resp_perm
    io.requestor(i).resp_ppn := resp_ppn
  }

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
