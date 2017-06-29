// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package rocket

import Chisel._
import Chisel.ImplicitConversions._
import chisel3.core.withReset
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

class FrontendExceptions extends Bundle {
  val pf = new Bundle {
    val inst = Bool()
  }
  val ae = new Bundle {
    val inst = Bool()
  }
}

class FrontendResp(implicit p: Parameters) extends CoreBundle()(p) {
  val btb = Valid(new BTBResp)
  val pc = UInt(width = vaddrBitsExtended)  // ID stage PC
  val data = UInt(width = fetchWidth * coreInstBits)
  val mask = Bits(width = fetchWidth)
  val xcpt = new FrontendExceptions
  val replay = Bool()
}

class FrontendPerfEvents extends Bundle {
  val acquire = Bool()
  val tlbMiss = Bool()
}

class FrontendIO(implicit p: Parameters) extends CoreBundle()(p) {
  val req = Valid(new FrontendReq)
  val sfence = Valid(new SFenceReq)
  val resp = Decoupled(new FrontendResp).flip
  val btb_update = Valid(new BTBUpdate)
  val bht_update = Valid(new BHTUpdate)
  val ras_update = Valid(new RASUpdate)
  val flush_icache = Bool(OUTPUT)
  val npc = UInt(INPUT, width = vaddrBitsExtended)
  val perf = new FrontendPerfEvents().asInput
}

class Frontend(val icacheParams: ICacheParams, hartid: Int, owner: => Option[Device] = None)(implicit p: Parameters) extends LazyModule {
  lazy val module = new FrontendModule(this)
  val icache = LazyModule(new ICache(icacheParams, hartid, owner))
  val masterNode = TLOutputNode()
  val slaveNode = TLInputNode()

  masterNode := icache.masterNode
  // Avoid breaking tile dedup due to address constants in the monitor
  icache.slaveNode.map { _ connectButDontMonitor slaveNode }
}

class FrontendBundle(outer: Frontend) extends CoreBundle()(outer.p) {
  val cpu = new FrontendIO().flip
  val ptw = new TLBPTWIO()
  val tl_out = outer.masterNode.bundleOut
  val tl_in = outer.slaveNode.bundleIn
  val resetVector = UInt(INPUT, vaddrBitsExtended)
  val hartid = UInt(INPUT, hartIdLen)
}

class FrontendModule(outer: Frontend) extends LazyModuleImp(outer)
    with HasCoreParameters
    with HasL1ICacheParameters {
  val io = new FrontendBundle(outer)
  implicit val edge = outer.masterNode.edgesOut.head
  val icache = outer.icache.module
  require(fetchWidth*coreInstBytes == outer.icacheParams.fetchBytes)

  val tlb = Module(new TLB(log2Ceil(coreInstBytes*fetchWidth), nTLBEntries))
  val fq = withReset(reset || io.cpu.req.valid) { Module(new ShiftQueue(new FrontendResp, 4, flow = true)) }

  val s0_valid = io.cpu.req.valid || !fq.io.mask(fq.io.mask.getWidth-2)
  val s1_pc = Reg(UInt(width=vaddrBitsExtended))
  val s1_speculative = Reg(Bool())
  val s2_valid = RegInit(false.B)
  val s2_pc = Reg(init=io.resetVector)
  val s2_btb_resp_valid = Reg(init=Bool(false))
  val s2_btb_resp_bits = Reg(new BTBResp)
  val s2_tlb_resp = Reg(tlb.io.resp)
  val s2_xcpt = !s2_tlb_resp.miss && fq.io.enq.bits.xcpt.asUInt.orR
  val s2_speculative = Reg(init=Bool(false))

  val fetchBytes = coreInstBytes * fetchWidth
  val s1_base_pc = ~(~s1_pc | (fetchBytes - 1))
  val ntpc = s1_base_pc + fetchBytes.U
  val predicted_npc = Wire(init = ntpc)
  val predicted_taken = Wire(init = Bool(false))

  val s2_replay = Wire(Bool())
  s2_replay := (s2_valid && !fq.io.enq.fire()) || RegNext(s2_replay && !s0_valid, true.B)
  val npc = Mux(s2_replay, s2_pc, predicted_npc)

  s1_pc := io.cpu.npc
  // consider RVC fetches across blocks to be non-speculative if the first
  // part was non-speculative
  val s0_speculative =
    if (usingCompressed) s1_speculative || s2_valid && !s2_speculative || predicted_taken
    else Bool(true)
  s1_speculative := Mux(io.cpu.req.valid, io.cpu.req.bits.speculative, Mux(s2_replay, s2_speculative, s0_speculative))

  s2_valid := false
  when (!s2_replay && !io.cpu.req.valid) {
    s2_valid := true
    s2_pc := s1_pc
    s2_speculative := s1_speculative
    s2_tlb_resp := tlb.io.resp
  }

  if (usingBTB) {
    val btb = Module(new BTB)
    btb.io.req.valid := false
    btb.io.req.bits.addr := s1_pc
    btb.io.btb_update := io.cpu.btb_update
    btb.io.bht_update := io.cpu.bht_update
    btb.io.ras_update := io.cpu.ras_update
    when (!s2_replay) {
      btb.io.req.valid := true
      s2_btb_resp_valid := btb.io.resp.valid
      s2_btb_resp_bits := btb.io.resp.bits
    }
    when (btb.io.resp.valid && btb.io.resp.bits.taken) {
      predicted_npc := btb.io.resp.bits.target.sextTo(vaddrBitsExtended)
      predicted_taken := Bool(true)
    }

    // push RAS speculatively
    btb.io.ras_update.valid := btb.io.req.valid && btb.io.resp.valid && btb.io.resp.bits.cfiType.isOneOf(CFIType.call, CFIType.ret)
    val returnAddrLSBs = btb.io.resp.bits.bridx +& 1
    btb.io.ras_update.bits.returnAddr :=
      Mux(returnAddrLSBs(log2Ceil(fetchWidth)), ntpc, s1_base_pc | ((returnAddrLSBs << log2Ceil(coreInstBytes)) & (fetchBytes - 1)))
    btb.io.ras_update.bits.cfiType := btb.io.resp.bits.cfiType
    btb.io.ras_update.bits.prediction.valid := true
  }

  io.ptw <> tlb.io.ptw
  tlb.io.req.valid := !s2_replay
  tlb.io.req.bits.vaddr := s1_pc
  tlb.io.req.bits.passthrough := Bool(false)
  tlb.io.req.bits.instruction := Bool(true)
  tlb.io.req.bits.sfence := io.cpu.sfence
  tlb.io.req.bits.size := log2Ceil(coreInstBytes*fetchWidth)

  icache.io.hartid := io.hartid
  icache.io.req.valid := s0_valid
  icache.io.req.bits.addr := io.cpu.npc
  icache.io.invalidate := io.cpu.flush_icache
  icache.io.s1_paddr := tlb.io.resp.paddr
  icache.io.s2_vaddr := s2_pc
  icache.io.s1_kill := io.cpu.req.valid || tlb.io.resp.miss || s2_replay
  icache.io.s2_kill := s2_valid && (s2_speculative && !s2_tlb_resp.cacheable || s2_xcpt)

  fq.io.enq.valid := s2_valid && (icache.io.resp.valid || icache.io.s2_kill)
  fq.io.enq.bits.pc := s2_pc
  io.cpu.npc := ~(~Mux(io.cpu.req.valid, io.cpu.req.bits.pc, npc) | (coreInstBytes-1)) // discard LSB(s)

  fq.io.enq.bits.data := icache.io.resp.bits
  fq.io.enq.bits.mask := UInt((1 << fetchWidth)-1) << s2_pc.extract(log2Ceil(fetchWidth)+log2Ceil(coreInstBytes)-1, log2Ceil(coreInstBytes))
  fq.io.enq.bits.xcpt := s2_tlb_resp
  fq.io.enq.bits.replay := icache.io.s2_kill && !icache.io.resp.valid && !s2_xcpt
  fq.io.enq.bits.btb.valid := s2_btb_resp_valid
  fq.io.enq.bits.btb.bits := s2_btb_resp_bits

  io.cpu.resp <> fq.io.deq

  // performance events
  io.cpu.perf.acquire := edge.done(icache.io.tl_out(0).a)
  io.cpu.perf.tlbMiss := io.ptw.req.fire()
}

/** Mix-ins for constructing tiles that have an ICache-based pipeline frontend */
trait HasICacheFrontend extends CanHavePTW with HasTileLinkMasterPort {
  val module: HasICacheFrontendModule
  def itimOwner : Option[Device] = None
  val frontend = LazyModule(new Frontend(tileParams.icache.get, hartid: Int, itimOwner))
  val hartid: Int
  tileBus.node := frontend.masterNode
  nPTWPorts += 1
}

trait HasICacheFrontendBundle extends HasTileLinkMasterPortBundle {
  val outer: HasICacheFrontend
}

trait HasICacheFrontendModule extends CanHavePTWModule with HasTileLinkMasterPortModule {
  val outer: HasICacheFrontend
  ptwPorts += outer.frontend.module.io.ptw
}
