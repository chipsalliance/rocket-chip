package rocket

import Chisel._
import Node._
import Constants._
import Util._

class TLBPTWIO extends Bundle {
  val req = new FIFOIO()(UFix(width = VPN_BITS))
  val resp = new PipeIO()(new Bundle {
    val error = Bool()
    val ppn = UFix(width = PPN_BITS)
    val perm = Bits(width = PERM_BITS)
  }).flip

  val status = new Status().asInput
  val invalidate = Bool(INPUT)
  val eret = Bool(INPUT)
}

class DatapathPTWIO extends Bundle {
  val ptbr = UFix(INPUT, PADDR_BITS)
  val invalidate = Bool(INPUT)
  val eret = Bool(INPUT)
  val status = new Status().asInput
}

class PTW(n: Int)(implicit conf: RocketConfiguration) extends Component
{
  val io = new Bundle {
    val requestor = Vec(n) { new TLBPTWIO }.flip
    val mem = new HellaCacheIO()(conf.dcache)
    val dpath = new DatapathPTWIO
  }
  
  val levels = 3
  val bitsPerLevel = VPN_BITS/levels
  require(VPN_BITS == levels * bitsPerLevel)

  val s_ready :: s_req :: s_wait :: s_done :: s_error :: Nil = Enum(5) { UFix() };
  val state = Reg(resetVal = s_ready)
  val count = Reg{UFix(width = log2Up(levels))}

  val r_req_vpn = Reg{Bits()}
  val r_req_dest = Reg{Bits()}
  val r_pte = Reg{Bits()}
  
  val vpn_idx = AVec((0 until levels).map(i => (r_req_vpn >> (levels-i-1)*bitsPerLevel)(bitsPerLevel-1,0)))(count)

  val arb = new RRArbiter(n)(UFix(width = VPN_BITS))
  arb.io.in <> io.requestor.map(_.req)
  arb.io.out.ready := state === s_ready

  when (arb.io.out.fire()) {
    r_req_vpn := arb.io.out.bits
    r_req_dest := arb.io.chosen
    r_pte := Cat(io.dpath.ptbr(PADDR_BITS-1,PGIDX_BITS), io.mem.resp.bits.data(PGIDX_BITS-1,0))
  }

  when (io.mem.resp.valid) {
    r_pte := io.mem.resp.bits.data
  }
  
  io.mem.req.valid     := state === s_req
  io.mem.req.bits.phys := Bool(true)
  io.mem.req.bits.cmd  := M_XRD
  io.mem.req.bits.typ  := MT_D
  io.mem.req.bits.addr := Cat(r_pte(PADDR_BITS-1,PGIDX_BITS), vpn_idx).toUFix << log2Up(conf.xprlen/8)
  io.mem.req.bits.kill := Bool(false)
  
  val resp_val = state === s_done || state === s_error
  val resp_err = state === s_error || state === s_wait
  
  val resp_ptd = io.mem.resp.bits.data(1,0) === Bits(1)
  val resp_pte = io.mem.resp.bits.data(1,0) === Bits(2)

  val r_resp_ppn = io.mem.req.bits.addr >> PGIDX_BITS
  val resp_ppn = AVec((0 until levels-1).map(i => Cat(r_resp_ppn >> bitsPerLevel*(levels-i-1), r_req_vpn(bitsPerLevel*(levels-i-1)-1,0))) :+ r_resp_ppn)(count)

  for (i <- 0 until io.requestor.size) {
    val me = r_req_dest === UFix(i)
    io.requestor(i).resp.valid := resp_val && me
    io.requestor(i).resp.bits.error := resp_err
    io.requestor(i).resp.bits.perm := r_pte(9,4)
    io.requestor(i).resp.bits.ppn := resp_ppn.toUFix
    io.requestor(i).invalidate := io.dpath.invalidate
    io.requestor(i).eret := io.dpath.eret
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
      when (io.mem.resp.valid) {
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
