// See LICENSE for license details.

package rocket

import Chisel._
import uncore.agents._
import uncore.constants._
import Util._
import cde.{Parameters, Field}

class PTWReq(implicit p: Parameters) extends CoreBundle()(p) {
  val prv = Bits(width = 2)
  val pum = Bool()
  val mxr = Bool()
  val addr = UInt(width = vpnBits)
  val store = Bool()
  val fetch = Bool()
}

class PTWResp(implicit p: Parameters) extends CoreBundle()(p) {
  val pte = new PTE
}

class TLBPTWIO(implicit p: Parameters) extends CoreBundle()(p) {
  val req = Decoupled(new PTWReq)
  val resp = Valid(new PTWResp).flip
  val ptbr = new PTBR().asInput
  val invalidate = Bool(INPUT)
  val status = new MStatus().asInput
}

class DatapathPTWIO(implicit p: Parameters) extends CoreBundle()(p) {
  val ptbr = new PTBR().asInput
  val invalidate = Bool(INPUT)
  val status = new MStatus().asInput
}

class PTE(implicit p: Parameters) extends CoreBundle()(p) {
  val reserved_for_hardware = Bits(width = 16)
  val ppn = UInt(width = 38)
  val reserved_for_software = Bits(width = 2)
  val d = Bool()
  val a = Bool()
  val g = Bool()
  val u = Bool()
  val x = Bool()
  val w = Bool()
  val r = Bool()
  val v = Bool()

  def table(dummy: Int = 0) = v && !r && !w && !x
  def leaf(dummy: Int = 0) = v && (r || (x && !w))
  def ur(dummy: Int = 0) = sr() && u
  def uw(dummy: Int = 0) = sw() && u
  def ux(dummy: Int = 0) = sx() && u
  def sr(dummy: Int = 0) = leaf() && r
  def sw(dummy: Int = 0) = leaf() && w
  def sx(dummy: Int = 0) = leaf() && x

  def access_ok(req: PTWReq) = {
    val perm_ok = Mux(req.fetch, x, Mux(req.store, w, r || (x && req.mxr)))
    val priv_ok = Mux(u, !req.pum, req.prv(0))
    leaf() && priv_ok && perm_ok
  }
}

class PTW(n: Int)(implicit p: Parameters) extends CoreModule()(p) {
  val io = new Bundle {
    val requestor = Vec(n, new TLBPTWIO).flip
    val mem = new HellaCacheIO
    val dpath = new DatapathPTWIO
  }

  require(usingAtomics, "PTW requires atomic memory operations")
  
  val s_ready :: s_req :: s_wait :: s_set_dirty :: s_wait_dirty :: s_done :: Nil = Enum(UInt(), 6)
  val state = Reg(init=s_ready)
  val count = Reg(UInt(width = log2Up(pgLevels)))

  val r_req = Reg(new PTWReq)
  val r_req_dest = Reg(Bits())
  val r_pte = Reg(new PTE)
  
  val vpn_idxs = (0 until pgLevels).map(i => (r_req.addr >> (pgLevels-i-1)*pgLevelBits)(pgLevelBits-1,0))
  val vpn_idx = vpn_idxs(count)

  val arb = Module(new RRArbiter(new PTWReq, n))
  arb.io.in <> io.requestor.map(_.req)
  arb.io.out.ready := state === s_ready

  val pte = new PTE().fromBits(io.mem.resp.bits.data)
  val pte_addr = Cat(r_pte.ppn, vpn_idx).toUInt << log2Up(xLen/8)

  when (arb.io.out.fire()) {
    r_req := arb.io.out.bits
    r_req_dest := arb.io.chosen
    r_pte.ppn := io.dpath.ptbr.ppn
  }

  val (pte_cache_hit, pte_cache_data) = {
    val size = 1 << log2Up(pgLevels * 2)
    val plru = new PseudoLRU(size)
    val valid = Reg(init = UInt(0, size))
    val tags = Reg(Vec(size, UInt(width = paddrBits)))
    val data = Reg(Vec(size, UInt(width = ppnBits)))

    val hits = tags.map(_ === pte_addr).toBits & valid
    val hit = hits.orR
    when (io.mem.resp.valid && pte.table() && !hit) {
      val r = Mux(valid.andR, plru.replace, PriorityEncoder(~valid))
      valid := valid | UIntToOH(r)
      tags(r) := pte_addr
      data(r) := pte.ppn
    }
    when (hit && state === s_req) { plru.access(OHToUInt(hits)) }
    when (io.dpath.invalidate) { valid := 0 }

    (hit, Mux1H(hits, data))
  }

  val set_dirty_bit = pte.access_ok(r_req) && (!pte.a || (r_req.store && !pte.d))
  when (io.mem.resp.valid && state === s_wait && !set_dirty_bit) {
    r_pte := pte
  }

  val pte_wdata = Wire(init=new PTE().fromBits(0))
  pte_wdata.a := true
  pte_wdata.d := r_req.store
  
  io.mem.req.valid     := state === s_req || state === s_set_dirty
  io.mem.req.bits.phys := Bool(true)
  io.mem.req.bits.cmd  := Mux(state === s_set_dirty, M_XA_OR, M_XRD)
  io.mem.req.bits.typ  := MT_D
  io.mem.req.bits.addr := pte_addr
  io.mem.s1_data := pte_wdata.toBits
  io.mem.s1_kill := Bool(false)
  io.mem.invalidate_lr := Bool(false)
  
  val r_resp_ppn = io.mem.req.bits.addr >> pgIdxBits
  val resp_ppns = (0 until pgLevels-1).map(i => Cat(r_resp_ppn >> pgLevelBits*(pgLevels-i-1), r_req.addr(pgLevelBits*(pgLevels-i-1)-1,0))) :+ r_resp_ppn
  val resp_ppn = resp_ppns(count)
  val resp_val = state === s_done

  for (i <- 0 until io.requestor.size) {
    io.requestor(i).resp.valid := resp_val && (r_req_dest === i)
    io.requestor(i).resp.bits.pte := r_pte
    io.requestor(i).resp.bits.pte.ppn := resp_ppn
    io.requestor(i).ptbr := io.dpath.ptbr
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
      when (pte_cache_hit && count < pgLevels-1) {
        io.mem.req.valid := false
        state := s_req
        count := count + 1
        r_pte.ppn := pte_cache_data
      }.elsewhen (io.mem.req.ready) {
        state := s_wait
      }
    }
    is (s_wait) {
      when (io.mem.s2_nack) {
        state := s_req
      }
      when (io.mem.resp.valid) {
        state := s_done
        when (set_dirty_bit) {
          state := s_set_dirty
        }
        when (pte.table() && count < pgLevels-1) {
          state := s_req
          count := count + 1
        }
      }
    }
    is (s_set_dirty) {
      when (io.mem.req.ready) {
        state := s_wait_dirty
      }
    }
    is (s_wait_dirty) {
      when (io.mem.s2_nack) {
        state := s_set_dirty
      }
      when (io.mem.resp.valid) {
        state := s_req
      }
    }
    is (s_done) {
      state := s_ready
    }
  }
}
