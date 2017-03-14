// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package rocket

import Chisel._
import Chisel.ImplicitConversions._
import config._
import coreplex._
import diplomacy._
import uncore.tilelink2._
import tile._
import util._

class FrontendReq(implicit p: Parameters) extends CoreBundle()(p) {
  val pc = UInt(width = vaddrBitsExtended)
  val speculative = Bool()
}

class FrontendResp(implicit p: Parameters) extends CoreBundle()(p) {
  val btb = Valid(new BTBResp)
  val pc = UInt(width = vaddrBitsExtended)  // ID stage PC
  val data = UInt(width = fetchWidth * coreInstBits)
  val mask = Bits(width = fetchWidth)
  val xcpt_if = Bool()
  val replay = Bool()
}

class FrontendIO(implicit p: Parameters) extends CoreBundle()(p) {
  val req = Valid(new FrontendReq)
  val resp = Decoupled(new FrontendResp).flip
  val btb_update = Valid(new BTBUpdate)
  val bht_update = Valid(new BHTUpdate)
  val ras_update = Valid(new RASUpdate)
  val flush_icache = Bool(OUTPUT)
  val flush_tlb = Bool(OUTPUT)
  val npc = UInt(INPUT, width = vaddrBitsExtended)

  // performance events
  val acquire = Bool(INPUT)
}

class Frontend(implicit p: Parameters) extends LazyModule {
  lazy val module = new FrontendModule(this)
  val icache = LazyModule(new ICache(latency = 2))
  val node = TLOutputNode()

  node := icache.node
}

class FrontendBundle(outer: Frontend) extends CoreBundle()(outer.p) {
  val cpu = new FrontendIO().flip
  val ptw = new TLBPTWIO()
  val mem = outer.node.bundleOut
  val resetVector = UInt(INPUT, vaddrBitsExtended)
}

class FrontendModule(outer: Frontend) extends LazyModuleImp(outer)
    with HasCoreParameters
    with HasL1ICacheParameters {
  val io = new FrontendBundle(outer)
  implicit val edge = outer.node.edgesOut(0)
  val icache = outer.icache.module

  val tlb = Module(new TLB(nTLBEntries))

  val s1_pc_ = Reg(UInt(width=vaddrBitsExtended))
  val s1_pc = ~(~s1_pc_ | (coreInstBytes-1)) // discard PC LSBS (this propagates down the pipeline)
  val s1_speculative = Reg(Bool())
  val s1_same_block = Reg(Bool())
  val s2_valid = Reg(init=Bool(true))
  val s2_pc = Reg(init=io.resetVector)
  val s2_btb_resp_valid = Reg(init=Bool(false))
  val s2_btb_resp_bits = Reg(new BTBResp)
  val s2_xcpt_if = Reg(init=Bool(false))
  val s2_speculative = Reg(init=Bool(false))
  val s2_cacheable = Reg(init=Bool(false))

  val ntpc = ~(~s1_pc | (coreInstBytes*fetchWidth-1)) + UInt(coreInstBytes*fetchWidth)
  val ntpc_same_block = (ntpc & rowBytes) === (s1_pc & rowBytes)
  val predicted_npc = Wire(init = ntpc)
  val predicted_taken = Wire(init = Bool(false))
  val icmiss = s2_valid && !icache.io.resp.valid
  val npc = Mux(icmiss, s2_pc, predicted_npc)
  val s0_same_block = !predicted_taken && !icmiss && !io.cpu.req.valid && ntpc_same_block

  val stall = io.cpu.resp.valid && !io.cpu.resp.ready
  when (!stall) {
    s1_same_block := s0_same_block && !tlb.io.resp.miss
    s1_pc_ := io.cpu.npc
    // consider RVC fetches across blocks to be non-speculative if the first
    // part was non-speculative
    val s0_speculative =
      if (usingCompressed) s1_speculative || s2_valid && !s2_speculative || predicted_taken
      else Bool(true)
    s1_speculative := Mux(icmiss, s2_speculative, s0_speculative)
    s2_valid := !icmiss
    when (!icmiss) {
      s2_pc := s1_pc
      s2_speculative := s1_speculative
      s2_cacheable := tlb.io.resp.cacheable
      s2_xcpt_if := tlb.io.resp.xcpt_if && !tlb.io.resp.miss
    }
  }
  when (io.cpu.req.valid) {
    s1_same_block := Bool(false)
    s1_pc_ := io.cpu.npc
    s1_speculative := io.cpu.req.bits.speculative
    s2_valid := Bool(false)
  }

  if (usingBTB) {
    val btb = Module(new BTB)
    btb.io.req.valid := false
    btb.io.req.bits.addr := s1_pc_
    btb.io.btb_update := io.cpu.btb_update
    btb.io.bht_update := io.cpu.bht_update
    btb.io.ras_update := io.cpu.ras_update
    when (!stall && !icmiss) {
      btb.io.req.valid := true
      s2_btb_resp_valid := btb.io.resp.valid
      s2_btb_resp_bits := btb.io.resp.bits
    }
    when (btb.io.resp.valid && btb.io.resp.bits.taken) {
      predicted_npc := btb.io.resp.bits.target.sextTo(vaddrBitsExtended)
      predicted_taken := Bool(true)
    }
  }

  io.ptw <> tlb.io.ptw
  tlb.io.req.valid := !stall && !icmiss
  tlb.io.req.bits.vaddr := s1_pc
  tlb.io.req.bits.passthrough := Bool(false)
  tlb.io.req.bits.instruction := Bool(true)
  tlb.io.req.bits.store := Bool(false)

  icache.io.req.valid := !stall && !s0_same_block
  icache.io.req.bits.addr := io.cpu.npc
  icache.io.invalidate := io.cpu.flush_icache
  icache.io.s1_paddr := tlb.io.resp.paddr
  icache.io.s1_kill := io.cpu.req.valid || tlb.io.resp.miss || tlb.io.resp.xcpt_if || icmiss || io.cpu.flush_tlb
  icache.io.s2_kill := s2_speculative && !s2_cacheable
  icache.io.resp.ready := !stall && !s1_same_block

  io.cpu.resp.valid := s2_valid && (icache.io.resp.valid || icache.io.s2_kill || s2_xcpt_if)
  io.cpu.resp.bits.pc := s2_pc
  io.cpu.npc := Mux(io.cpu.req.valid, io.cpu.req.bits.pc, npc)

  require(fetchWidth * coreInstBytes <= rowBytes && isPow2(fetchWidth))
  io.cpu.resp.bits.data := icache.io.resp.bits.datablock >> (s2_pc.extract(log2Ceil(rowBytes)-1,log2Ceil(fetchWidth*coreInstBytes)) << log2Ceil(fetchWidth*coreInstBits))
  io.cpu.resp.bits.mask := UInt((1 << fetchWidth)-1) << s2_pc.extract(log2Ceil(fetchWidth)+log2Ceil(coreInstBytes)-1, log2Ceil(coreInstBytes))
  io.cpu.resp.bits.xcpt_if := s2_xcpt_if
  io.cpu.resp.bits.replay := icache.io.s2_kill && !icache.io.resp.valid && !s2_xcpt_if
  io.cpu.resp.bits.btb.valid := s2_btb_resp_valid
  io.cpu.resp.bits.btb.bits := s2_btb_resp_bits

  // performance events
  io.cpu.acquire := edge.done(icache.io.mem(0).a)
}

/** Mix-ins for constructing tiles that have an ICache-based pipeline frontend */
trait HasICacheFrontend extends CanHavePTW with HasTileLinkMasterPort {
  val module: HasICacheFrontendModule
  val frontend = LazyModule(new Frontend)
  masterNode := frontend.node
  nPTWPorts += 1
}

trait HasICacheFrontendBundle extends HasTileLinkMasterPortBundle {
  val outer: HasICacheFrontend
}

trait HasICacheFrontendModule extends CanHavePTWModule with HasTileLinkMasterPortModule {
  val outer: HasICacheFrontend
  ptwPorts += outer.frontend.module.io.ptw
}
