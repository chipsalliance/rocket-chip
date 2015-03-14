// See LICENSE for license details.

package rocket

import Chisel._
import uncore._
import Util._

class PTWReq extends CoreBundle {
  val addr = UInt(width = vpnBits)
  val perm = Bits(width = permBits)
}

class PTWResp extends CoreBundle {
  val error = Bool()
  val ppn = UInt(width = ppnBits)
  val perm = Bits(width = permBits)
  val dirty = Bool()
}

class TLBPTWIO extends CoreBundle {
  val req = Decoupled(new PTWReq)
  val resp = Valid(new PTWResp).flip
  val status = new MStatus().asInput
  val invalidate = Bool(INPUT)
}

class DatapathPTWIO extends CoreBundle {
  val ptbr = UInt(INPUT, paddrBits)
  val invalidate = Bool(INPUT)
  val status = new MStatus().asInput
}

class PTW(n: Int) extends CoreModule
{
  val io = new Bundle {
    val requestor = Vec.fill(n){new TLBPTWIO}.flip
    val mem = new HellaCacheIO
    val dpath = new DatapathPTWIO
  }
  
  val levels = 3
  val bitsPerLevel = vpnBits/levels
  require(vpnBits == levels * bitsPerLevel)

  val s_ready :: s_req :: s_wait :: s_set_dirty :: s_wait_dirty :: s_done :: s_error :: Nil = Enum(UInt(), 7)
  val state = Reg(init=s_ready)
  val count = Reg(UInt(width = log2Up(levels)))

  val r_req = Reg(new PTWReq)
  val r_req_dest = Reg(Bits())
  val r_pte = Reg(Bits())
  
  val vpn_idx = Vec((0 until levels).map(i => (r_req.addr >> (levels-i-1)*bitsPerLevel)(bitsPerLevel-1,0)))(count)

  val arb = Module(new RRArbiter(new PTWReq, n))
  arb.io.in <> io.requestor.map(_.req)
  arb.io.out.ready := state === s_ready

  val pte = io.mem.resp.bits.data
  when (arb.io.out.fire()) {
    r_req := arb.io.out.bits
    r_req_dest := arb.io.chosen
    r_pte := Cat(io.dpath.ptbr(paddrBits-1,pgIdxBits), pte(pgIdxBits-1,0))
  }

  val perm_ok = (pte(8,3) & r_req.perm).orR
  val is_store = r_req.perm(1) || r_req.perm(4)
  val set_dirty_bit = perm_ok && !pte(1) && (!pte(9) || (is_store && !pte(10)))
  when (io.mem.resp.valid && state === s_wait && !set_dirty_bit) {
    r_pte := pte
  }
  
  io.mem.req.valid     := state === s_req || state === s_set_dirty
  io.mem.req.bits.phys := Bool(true)
  io.mem.req.bits.cmd  := Mux(state === s_set_dirty, M_XA_OR, M_XRD)
  io.mem.req.bits.typ  := MT_D
  io.mem.req.bits.addr := Cat(r_pte(paddrBits-1,pgIdxBits), vpn_idx).toUInt << log2Up(xLen/8)
  io.mem.req.bits.kill := Bool(false)
  io.mem.req.bits.data := UInt(1<<9) | Mux(is_store, UInt(1<<10), UInt(0))
  
  val resp_err = state === s_error
  val resp_val = state === s_done || resp_err

  val r_resp_ppn = io.mem.req.bits.addr >> UInt(pgIdxBits)
  val resp_ppn = Vec((0 until levels-1).map(i => Cat(r_resp_ppn >> bitsPerLevel*(levels-i-1), r_req.addr(bitsPerLevel*(levels-i-1)-1,0))) :+ r_resp_ppn)(count)

  for (i <- 0 until io.requestor.size) {
    val me = r_req_dest === UInt(i)
    io.requestor(i).resp.valid := resp_val && me
    io.requestor(i).resp.bits.error := resp_err
    io.requestor(i).resp.bits.perm := r_pte(8,3)
    io.requestor(i).resp.bits.dirty := r_pte(10)
    io.requestor(i).resp.bits.ppn := resp_ppn.toUInt
    io.requestor(i).invalidate := io.dpath.invalidate
    io.requestor(i).status := io.dpath.status
  }

  // control state machine
  switch (state) {
    is (s_ready) {
      when (arb.io.out.valid) {
        state := s_req
      }
      count := UInt(0)
    }
    is (s_req) {
      when (io.mem.req.ready) {
        state := s_wait
      }
    }
    is (s_wait) {
      when (io.mem.resp.bits.nack) {
        state := s_req
      }
      when (io.mem.resp.valid) {
        state := s_error
        when (pte(0)) {
          when (set_dirty_bit) {
            state := s_set_dirty
          }.elsewhen (!pte(1)) {
            state := s_done
          }.elsewhen (count < levels-1) {
            state := s_req
            count := count + 1
          }
        }
      }
    }
    is (s_set_dirty) {
      when (io.mem.req.ready) {
        state := s_wait_dirty
      }
    }
    is (s_wait_dirty) {
      when (io.mem.resp.bits.nack) {
        state := s_set_dirty
      }
      when (io.mem.resp.valid) {
        state := s_req
      }
    }
    is (s_done) {
      state := s_ready
    }
    is (s_error) {
      state := s_ready
    }
  }
}
