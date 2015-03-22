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
  val pte = new PTE
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

class PTE extends CoreBundle {
  val ppn = Bits(width = ppnBits)
  val reserved = Bits(width = 2)
  val d = Bool()
  val r = Bool()
  val perm = Bits(width = 6)
  val g = Bool()
  val t = Bool()
  val v = Bool()
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
  val r_pte = Reg(new PTE)
  
  val vpn_idx = Vec((0 until levels).map(i => (r_req.addr >> (levels-i-1)*bitsPerLevel)(bitsPerLevel-1,0)))(count)

  val arb = Module(new RRArbiter(new PTWReq, n))
  arb.io.in <> io.requestor.map(_.req)
  arb.io.out.ready := state === s_ready

  val pte = new PTE().fromBits(io.mem.resp.bits.data)
  val pte_addr = Cat(r_pte.ppn, vpn_idx).toUInt << log2Up(xLen/8)

  when (arb.io.out.fire()) {
    r_req := arb.io.out.bits
    r_req_dest := arb.io.chosen
    r_pte.ppn := io.dpath.ptbr(paddrBits-1,pgIdxBits)
  }

  val (pte_cache_hit, pte_cache_data) = {
    val size = log2Up(levels * 2)
    val plru = new PseudoLRU(size)
    val valid = Reg(init = Bits(0, size))
    val tags = Mem(UInt(width = paddrBits), size)
    val data = Mem(UInt(width = paddrBits - pgIdxBits), size)

    val hits = Vec(tags.map(_ === pte_addr)).toBits & valid
    val hit = hits.orR
    when (io.mem.resp.valid && io.mem.resp.bits.data(1,0).andR && !hit) {
      val r = Mux(valid.andR, plru.replace, PriorityEncoder(~valid))
      valid(r) := true
      tags(r) := pte_addr
      data(r) := io.mem.resp.bits.data(paddrBits-1,pgIdxBits)
    }
    when (hit && state === s_req) { plru.access(OHToUInt(hits)) }
    when (io.dpath.invalidate) { valid := 0 }

    (hit, Mux1H(hits, data))
  }

  val perm_ok = (pte.perm & r_req.perm).orR
  val is_store = r_req.perm(1) || r_req.perm(4)
  val set_dirty_bit = perm_ok && !pte.t && (!pte.r || (is_store && !pte.d))
  when (io.mem.resp.valid && state === s_wait && !set_dirty_bit) {
    r_pte := pte
  }
  
  io.mem.req.valid     := state === s_req || state === s_set_dirty
  io.mem.req.bits.phys := Bool(true)
  io.mem.req.bits.cmd  := Mux(state === s_set_dirty, M_XA_OR, M_XRD)
  io.mem.req.bits.typ  := MT_D
  io.mem.req.bits.addr := pte_addr
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
    io.requestor(i).resp.bits.pte := r_pte
    io.requestor(i).resp.bits.pte.ppn := resp_ppn
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
      when (pte_cache_hit && count < levels-1) {
        io.mem.req.valid := false
        state := s_req
        count := count + 1
        r_pte.ppn := pte_cache_data
      }.elsewhen (io.mem.req.ready) {
        state := s_wait
      }
    }
    is (s_wait) {
      when (io.mem.resp.bits.nack) {
        state := s_req
      }
      when (io.mem.resp.valid) {
        state := s_error
        when (pte.v) {
          when (set_dirty_bit) {
            state := s_set_dirty
          }.elsewhen (!pte.t) {
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
