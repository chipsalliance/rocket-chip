// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket

import Chisel._
import Chisel.ImplicitConversions._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.coreplex.CacheBlockBytes
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import scala.collection.mutable.ListBuffer

class PTWReq(implicit p: Parameters) extends CoreBundle()(p) {
  val addr = UInt(width = vpnBits)
}

class PTWResp(implicit p: Parameters) extends CoreBundle()(p) {
  val ae = Bool()
  val pte = new PTE
  val level = UInt(width = log2Ceil(pgLevels))
  val homogeneous = Bool()
}

class TLBPTWIO(implicit p: Parameters) extends CoreBundle()(p)
    with HasCoreParameters {
  val req = Decoupled(new PTWReq)
  val resp = Valid(new PTWResp).flip
  val ptbr = new PTBR().asInput
  val status = new MStatus().asInput
  val pmp = Vec(nPMPs, new PMP).asInput
}

class PTWPerfEvents extends Bundle {
  val l2miss = Bool()
}

class DatapathPTWIO(implicit p: Parameters) extends CoreBundle()(p)
    with HasCoreParameters {
  val ptbr = new PTBR().asInput
  val sfence = Valid(new SFenceReq).flip
  val status = new MStatus().asInput
  val pmp = Vec(nPMPs, new PMP).asInput
  val perf = new PTWPerfEvents().asOutput
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

class PTW(n: Int)(implicit edge: TLEdgeOut, p: Parameters) extends CoreModule()(p) {
  val io = new Bundle {
    val requestor = Vec(n, new TLBPTWIO).flip
    val mem = new HellaCacheIO
    val dpath = new DatapathPTWIO
  }

  val s_ready :: s_req :: s_wait1 :: s_wait2 :: Nil = Enum(UInt(), 4)
  val state = Reg(init=s_ready)
  val count = Reg(UInt(width = log2Up(pgLevels)))
  val s1_kill = Reg(next = Bool(false))
  val resp_valid = Reg(next = Vec.fill(io.requestor.size)(Bool(false)))
  val resp_ae = Reg(Bool())

  val r_req = Reg(new PTWReq)
  val r_req_dest = Reg(Bits())
  val r_pte = Reg(new PTE)
  
  val vpn_idxs = (0 until pgLevels).map(i => (r_req.addr >> (pgLevels-i-1)*pgLevelBits)(pgLevelBits-1,0))
  val vpn_idx = vpn_idxs(count)

  val arb = Module(new RRArbiter(new PTWReq, n))
  arb.io.in <> io.requestor.map(_.req)
  arb.io.out.ready := state === s_ready

  val (pte, invalid_paddr) = {
    val tmp = new PTE().fromBits(io.mem.resp.bits.data)
    val res = Wire(init = new PTE().fromBits(io.mem.resp.bits.data))
    res.ppn := tmp.ppn(ppnBits-1, 0)
    when (tmp.r || tmp.w || tmp.x) {
      // for superpage mappings, make sure PPN LSBs are zero
      for (i <- 0 until pgLevels-1)
        when (count <= i && tmp.ppn((pgLevels-1-i)*pgLevelBits-1, (pgLevels-2-i)*pgLevelBits) =/= 0) { res.v := false }
    }
    (res, (tmp.ppn >> ppnBits) =/= 0)
  }
  val traverse = pte.table() && !invalid_paddr && count < pgLevels-1
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
    when (io.mem.resp.valid && traverse && !hit) {
      val r = Mux(valid.andR, plru.replace, PriorityEncoder(~valid))
      valid := valid | UIntToOH(r)
      tags(r) := pte_addr
      data(r) := pte.ppn
    }
    when (hit && state === s_req) { plru.access(OHToUInt(hits)) }
    when (io.dpath.sfence.valid && !io.dpath.sfence.bits.rs1) { valid := 0 }

    (hit && count < pgLevels-1, Mux1H(hits, data))
  }

  val l2_refill = RegNext(false.B)
  io.dpath.perf.l2miss := false
  val (l2_hit, l2_valid, l2_pte, l2_tlb_ram) = if (coreParams.nL2TLBEntries == 0) (false.B, false.B, Wire(new PTE), None) else {
    val code = new ParityCode
    require(isPow2(coreParams.nL2TLBEntries))
    val idxBits = log2Ceil(coreParams.nL2TLBEntries)
    val tagBits = vpnBits - idxBits

    class Entry extends Bundle {
      val tag = UInt(width = tagBits)
      val ppn = UInt(width = ppnBits)
      val d = Bool()
      val a = Bool()
      val u = Bool()
      val x = Bool()
      val w = Bool()
      val r = Bool()

      override def cloneType = new Entry().asInstanceOf[this.type]
    }

    val ram = SeqMem(coreParams.nL2TLBEntries, UInt(width = code.width(new Entry().getWidth)))
    val g = Reg(UInt(width = coreParams.nL2TLBEntries))
    val valid = RegInit(UInt(0, coreParams.nL2TLBEntries))
    val (r_tag, r_idx) = Split(r_req.addr, idxBits)
    when (l2_refill) {
      val entry = Wire(new Entry)
      entry := r_pte
      entry.tag := r_tag
      ram.write(r_idx, code.encode(entry.asUInt))

      val mask = UIntToOH(r_idx)
      valid := valid | mask
      g := Mux(r_pte.g, g | mask, g & ~mask)
    }
    when (io.dpath.sfence.valid) {
      valid :=
        Mux(io.dpath.sfence.bits.rs1, valid & ~UIntToOH(io.dpath.sfence.bits.addr(idxBits+pgIdxBits-1, pgIdxBits)),
        Mux(io.dpath.sfence.bits.rs2, valid & g, 0.U))
    }

    val s0_valid = !l2_refill && arb.io.out.fire()
    val s1_valid = RegNext(s0_valid)
    val s2_valid = RegNext(s1_valid)
    val s1_rdata = ram.read(arb.io.out.bits.addr(idxBits-1, 0), s0_valid)
    val s2_rdata = code.decode(RegEnable(s1_rdata, s1_valid))
    val s2_valid_bit = RegEnable(valid(r_idx), s1_valid)
    val s2_g = RegEnable(g(r_idx), s1_valid)
    when (s2_valid && s2_valid_bit && s2_rdata.error) { valid := 0.U }

    val s2_entry = s2_rdata.uncorrected.asTypeOf(new Entry)
    val s2_hit = s2_valid && s2_valid_bit && !s2_rdata.error && r_tag === s2_entry.tag
    io.dpath.perf.l2miss := s2_valid && !(s2_valid_bit && r_tag === s2_entry.tag)
    val s2_pte = Wire(new PTE)
    s2_pte := s2_entry
    s2_pte.g := s2_g
    s2_pte.v := true

    (s2_hit, s2_valid && s2_valid_bit, s2_pte, Some(ram))
  }
  
  io.mem.req.valid := state === s_req && !l2_valid
  io.mem.req.bits.phys := Bool(true)
  io.mem.req.bits.cmd  := M_XRD
  io.mem.req.bits.typ  := log2Ceil(xLen/8)
  io.mem.req.bits.addr := pte_addr
  io.mem.s1_kill := s1_kill || l2_hit
  io.mem.invalidate_lr := Bool(false)
  
  val pmaPgLevelHomogeneous = (0 until pgLevels) map { i =>
    TLBPageLookup(edge.manager.managers, xLen, p(CacheBlockBytes), BigInt(1) << (pgIdxBits + ((pgLevels - 1 - i) * pgLevelBits)))(pte_addr >> pgIdxBits << pgIdxBits).homogeneous
  }
  val pmaHomogeneous = pmaPgLevelHomogeneous(count)
  val pmpHomogeneous = new PMPHomogeneityChecker(io.dpath.pmp).apply(pte_addr >> pgIdxBits << pgIdxBits, count)

  for (i <- 0 until io.requestor.size) {
    io.requestor(i).resp.valid := resp_valid(i)
    io.requestor(i).resp.bits.ae := resp_ae
    io.requestor(i).resp.bits.pte := r_pte
    io.requestor(i).resp.bits.level := count
    io.requestor(i).resp.bits.pte.ppn := pte_addr >> pgIdxBits
    io.requestor(i).resp.bits.homogeneous := pmpHomogeneous && pmaHomogeneous
    io.requestor(i).ptbr := io.dpath.ptbr
    io.requestor(i).status := io.dpath.status
    io.requestor(i).pmp := io.dpath.pmp
  }

  // control state machine
  switch (state) {
    is (s_ready) {
      when (arb.io.out.fire()) {
        state := s_req
      }
      count := UInt(0)
    }
    is (s_req) {
      when (pte_cache_hit) {
        s1_kill := true
        count := count + 1
        r_pte.ppn := pte_cache_data
      }.elsewhen (io.mem.req.fire()) {
        state := s_wait1
      }
    }
    is (s_wait1) {
      state := s_wait2
    }
    is (s_wait2) {
      when (io.mem.s2_nack) {
        state := s_req
      }
      when (io.mem.resp.valid) {
        r_pte := pte
        when (traverse) {
          state := s_req
          count := count + 1
        }.otherwise {
          l2_refill := pte.v && !invalid_paddr && count === pgLevels-1
          resp_ae := pte.v && invalid_paddr
          state := s_ready
          resp_valid(r_req_dest) := true
        }
      }
      when (io.mem.s2_xcpt.ae.ld) {
        resp_ae := true
        state := s_ready
        resp_valid(r_req_dest) := true
      }
    }
  }
  when (l2_hit) {
    state := s_ready
    resp_valid(r_req_dest) := true
    resp_ae := false
    r_pte := l2_pte
    count := pgLevels-1
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
  val ptw = Module(new PTW(outer.nPTWPorts)(outer.dcache.node.edges.out(0), outer.p))
  if (outer.usingPTW)
    dcachePorts += ptw.io.mem
}
