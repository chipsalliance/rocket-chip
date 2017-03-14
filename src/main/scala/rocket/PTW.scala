// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package rocket

import Chisel._
import Chisel.ImplicitConversions._
import config._
import tile._
import uncore.constants._
import util._

import scala.collection.mutable.ListBuffer

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
  val level = UInt(width = log2Ceil(pgLevels))
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
  val ppn = UInt(width = 54)
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
  def leaf(dummy: Int = 0) = v && (r || (x && !w)) && a
  def ur(dummy: Int = 0) = sr() && u
  def uw(dummy: Int = 0) = sw() && u
  def ux(dummy: Int = 0) = sx() && u
  def sr(dummy: Int = 0) = leaf() && r
  def sw(dummy: Int = 0) = leaf() && w && d
  def sx(dummy: Int = 0) = leaf() && x
}

class PTW(n: Int)(implicit p: Parameters) extends CoreModule()(p) {
  val io = new Bundle {
    val requestor = Vec(n, new TLBPTWIO).flip
    val mem = new HellaCacheIO
    val dpath = new DatapathPTWIO
  }

  require(usingAtomics, "PTW requires atomic memory operations")

  val s_ready :: s_req :: s_wait1 :: s_wait2 :: Nil = Enum(UInt(), 4)
  val state = Reg(init=s_ready)
  val count = Reg(UInt(width = log2Up(pgLevels)))
  val s1_kill = Reg(next = Bool(false))
  val resp_valid = Reg(next = Bool(false))

  val r_req = Reg(new PTWReq)
  val r_req_dest = Reg(Bits())
  val r_pte = Reg(new PTE)
  
  val vpn_idxs = (0 until pgLevels).map(i => (r_req.addr >> (pgLevels-i-1)*pgLevelBits)(pgLevelBits-1,0))
  val vpn_idx = vpn_idxs(count)

  val arb = Module(new RRArbiter(new PTWReq, n))
  arb.io.in <> io.requestor.map(_.req)
  arb.io.out.ready := state === s_ready

  val pte = {
    val tmp = new PTE().fromBits(io.mem.resp.bits.data)
    val res = Wire(init = new PTE().fromBits(io.mem.resp.bits.data))
    res.ppn := tmp.ppn(ppnBits-1, 0)
    when ((tmp.ppn >> ppnBits) =/= 0) { res.v := false }
    res
  }
  val pte_addr = Cat(r_pte.ppn, vpn_idx) << log2Ceil(xLen/8)

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

    val hits = tags.map(_ === pte_addr).asUInt & valid
    val hit = hits.orR
    when (io.mem.resp.valid && pte.table() && !hit) {
      val r = Mux(valid.andR, plru.replace, PriorityEncoder(~valid))
      valid := valid | UIntToOH(r)
      tags(r) := pte_addr
      data(r) := pte.ppn
    }
    when (hit && state === s_req) { plru.access(OHToUInt(hits)) }
    when (io.dpath.invalidate) { valid := 0 }

    (hit && count < pgLevels-1, Mux1H(hits, data))
  }
  
  io.mem.req.valid     := state === s_req
  io.mem.req.bits.phys := Bool(true)
  io.mem.req.bits.cmd  := M_XRD
  io.mem.req.bits.typ  := log2Ceil(xLen/8)
  io.mem.req.bits.addr := pte_addr
  io.mem.s1_kill := s1_kill
  io.mem.invalidate_lr := Bool(false)
  
  for (i <- 0 until io.requestor.size) {
    io.requestor(i).resp.valid := resp_valid && (r_req_dest === i)
    io.requestor(i).resp.bits.pte := r_pte
    io.requestor(i).resp.bits.level := count
    io.requestor(i).resp.bits.pte.ppn := pte_addr >> pgIdxBits
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
      when (pte_cache_hit) {
        s1_kill := true
        state := s_req
        count := count + 1
        r_pte.ppn := pte_cache_data
      }.elsewhen (io.mem.req.ready) {
        state := s_wait1
      }
    }
    is (s_wait1) {
      state := s_wait2
      when (io.mem.xcpt.pf.ld) {
        r_pte.v := false
        state := s_ready
        resp_valid := true
      }
    }
    is (s_wait2) {
      when (io.mem.s2_nack) {
        state := s_req
      }
      when (io.mem.resp.valid) {
        r_pte := pte
        when (pte.table() && count < pgLevels-1) {
          state := s_req
          count := count + 1
        }.otherwise {
          state := s_ready
          resp_valid := true
        }
      }
    }
  }
}

/** Mix-ins for constructing tiles that might have a PTW */
trait CanHavePTW extends HasHellaCache {
  implicit val p: Parameters
  val module: CanHavePTWModule
  var nPTWPorts = 1
  nDCachePorts += usingPTW.toInt
}

trait CanHavePTWModule extends HasHellaCacheModule {
  val outer: CanHavePTW
  val ptwPorts = ListBuffer(outer.dcache.module.io.ptw)
  val ptwOpt = if (outer.usingPTW) { Some(Module(new PTW(outer.nPTWPorts)(outer.p))) } else None
  ptwOpt foreach { ptw => dcachePorts += ptw.io.mem }
}
