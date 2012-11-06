package rocket

import Chisel._
import Node._
import Constants._
import scala.math._

class IOTLBPTW extends Bundle {
  val req = new FIFOIO()(UFix(width = VPN_BITS))
  val resp = new PipeIO()(new Bundle {
    val error = Bool()
    val ppn = UFix(width = PPN_BITS)
    val perm = Bits(width = PERM_BITS)
  }).flip

  val status = Bits(INPUT, width = 32)
  val invalidate = Bool(INPUT)
}

class IODatapathPTW extends Bundle {
  val ptbr = UFix(INPUT, PADDR_BITS)
  val invalidate = Bool(INPUT)
  val status = Bits(INPUT, 32)
}

class PTW(n: Int)(implicit conf: RocketConfiguration) extends Component
{
  val io = new Bundle {
    val requestor = Vec(n) { new IOTLBPTW }.flip
    val mem = new ioHellaCache()(conf.dcache)
    val dpath = new IODatapathPTW
  }
  
  val levels = 3
  val bitsPerLevel = VPN_BITS/levels
  require(VPN_BITS == levels * bitsPerLevel)

  val count = Reg() { UFix(width = log2Up(levels)) }
  val s_ready :: s_req :: s_wait :: s_done :: s_error :: Nil = Enum(5) { UFix() };
  val state = Reg(resetVal = s_ready);
  
  val r_req_vpn = Reg() { Bits() }
  val r_req_dest = Reg() { Bits() }
  
  val req_addr = Reg() { UFix() }
  val r_resp_ppn = Reg() { Bits() };
  val r_resp_perm = Reg() { Bits() };
  
  val vpn_idxs = (1 until levels).map(i => r_req_vpn((levels-i)*bitsPerLevel-1, (levels-i-1)*bitsPerLevel))
  val vpn_idx = (2 until levels).foldRight(vpn_idxs(0))((i,j) => Mux(count === UFix(i-1), vpn_idxs(i-1), j))

  val arb = new RRArbiter(n)(UFix(width = VPN_BITS))
  arb.io.in <> io.requestor.map(_.req)
  arb.io.out.ready := state === s_ready

  when (arb.io.out.fire()) {
    r_req_vpn := arb.io.out.bits
    r_req_dest := arb.io.chosen
    req_addr := Cat(io.dpath.ptbr(PADDR_BITS-1,PGIDX_BITS), arb.io.out.bits(VPN_BITS-1,VPN_BITS-bitsPerLevel), UFix(0,3))
  }

  val dmem_resp_val = Reg(io.mem.resp.valid, resetVal = Bool(false))
  when (dmem_resp_val) {
    req_addr := Cat(io.mem.resp.bits.data_subword(PADDR_BITS-1, PGIDX_BITS), vpn_idx, UFix(0,3)).toUFix
    r_resp_perm := io.mem.resp.bits.data_subword(9,4);
    r_resp_ppn  := io.mem.resp.bits.data_subword(PADDR_BITS-1, PGIDX_BITS);
  }
  
  io.mem.req.valid     := state === s_req
  io.mem.req.bits.phys := Bool(true)
  io.mem.req.bits.cmd  := M_XRD
  io.mem.req.bits.typ  := MT_D
  io.mem.req.bits.addr := req_addr
  io.mem.req.bits.kill := Bool(false)
  
  val resp_val = state === s_done || state === s_error
  val resp_err = state === s_error || state === s_wait
  
  val resp_ptd = io.mem.resp.bits.data_subword(1,0) === Bits(1)
  val resp_pte = io.mem.resp.bits.data_subword(1,0) === Bits(2)
 
  val resp_ppns = (0 until levels-1).map(i => Cat(r_resp_ppn(PPN_BITS-1, VPN_BITS-bitsPerLevel*(i+1)), r_req_vpn(VPN_BITS-1-bitsPerLevel*(i+1), 0)))
  val resp_ppn = (0 until levels-1).foldRight(r_resp_ppn)((i,j) => Mux(count === UFix(i), resp_ppns(i), j))

  for (i <- 0 until io.requestor.size) {
    val me = r_req_dest === UFix(i)
    io.requestor(i).resp.valid := resp_val && me
    io.requestor(i).resp.bits.error := resp_err
    io.requestor(i).resp.bits.perm := r_resp_perm
    io.requestor(i).resp.bits.ppn := resp_ppn.toUFix
    io.requestor(i).invalidate := io.dpath.invalidate
    io.requestor(i).status := io.dpath.status
  }

  // control state machine
  switch (state) {
    is (s_ready) {
      when (arb.io.out.valid) {
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
