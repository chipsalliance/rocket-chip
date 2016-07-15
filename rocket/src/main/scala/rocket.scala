// See LICENSE for license details.

package rocket

import Chisel._
import junctions._
import uncore.devices._
import uncore.agents.CacheName
import uncore.constants._
import Util._
import cde.{Parameters, Field}

case object UseFPU extends Field[Boolean]
case object FDivSqrt extends Field[Boolean]
case object XLen extends Field[Int]
case object FetchWidth extends Field[Int]
case object RetireWidth extends Field[Int]
case object UseVM extends Field[Boolean]
case object UseUser extends Field[Boolean]
case object UseDebug extends Field[Boolean]
case object UseAtomics extends Field[Boolean]
case object UsePerfCounters extends Field[Boolean]
case object FastLoadWord extends Field[Boolean]
case object FastLoadByte extends Field[Boolean]
case object MulUnroll extends Field[Int]
case object DivEarlyOut extends Field[Boolean]
case object CoreInstBits extends Field[Int]
case object CoreDataBits extends Field[Int]
case object CoreDCacheReqTagBits extends Field[Int]
case object NCustomMRWCSRs extends Field[Int]
case object MtvecWritable extends Field[Boolean]
case object MtvecInit extends Field[BigInt]
case object ResetVector extends Field[BigInt]
case object NBreakpoints extends Field[Int]

trait HasCoreParameters extends HasAddrMapParameters {
  implicit val p: Parameters
  val xLen = p(XLen)

  val usingVM = p(UseVM)
  val usingUser = p(UseUser)
  val usingDebug = p(UseDebug)
  val usingFPU = p(UseFPU)
  val usingAtomics = p(UseAtomics)
  val usingFDivSqrt = p(FDivSqrt)
  val usingRoCC = !p(BuildRoCC).isEmpty
  val mulUnroll = p(MulUnroll)
  val divEarlyOut = p(DivEarlyOut)
  val fastLoadWord = p(FastLoadWord)
  val fastLoadByte = p(FastLoadByte)

  val retireWidth = p(RetireWidth)
  val fetchWidth = p(FetchWidth)
  val coreInstBits = p(CoreInstBits)
  val coreInstBytes = coreInstBits/8
  val coreDataBits = xLen
  val coreDataBytes = coreDataBits/8
  val coreDCacheReqTagBits = 7 + (2 + (if(!usingRoCC) 0 else 1))
  val vpnBitsExtended = vpnBits + (vaddrBits < xLen).toInt
  val vaddrBitsExtended = vpnBitsExtended + pgIdxBits
  val coreMaxAddrBits = paddrBits max vaddrBitsExtended
  val nCustomMrwCsrs = p(NCustomMRWCSRs)
  val roccCsrs = if (p(BuildRoCC).isEmpty) Nil
    else p(BuildRoCC).flatMap(_.csrs)
  val nRoccCsrs = p(RoccNCSRs)
  val nCores = p(NTiles)

  // Print out log of committed instructions and their writeback values.
  // Requires post-processing due to out-of-order writebacks.
  val enableCommitLog = false
  val usingPerfCounters = p(UsePerfCounters)

  val maxPAddrBits = xLen match {
    case 32 => 34
    case 64 => 50
  }

  require(paddrBits < maxPAddrBits)
  require(!fastLoadByte || fastLoadWord)
}

abstract class CoreModule(implicit val p: Parameters) extends Module
  with HasCoreParameters
abstract class CoreBundle(implicit val p: Parameters) extends ParameterizedBundle()(p)
  with HasCoreParameters

class RegFile(n: Int, w: Int, zero: Boolean = false) {
  private val rf = Mem(n, UInt(width = w))
  private def access(addr: UInt) = rf(~addr(log2Up(n)-1,0))
  private val reads = collection.mutable.ArrayBuffer[(UInt,UInt)]()
  private var canRead = true
  def read(addr: UInt) = {
    require(canRead)
    reads += addr -> Wire(UInt())
    reads.last._2 := Mux(Bool(zero) && addr === UInt(0), UInt(0), access(addr))
    reads.last._2
  }
  def write(addr: UInt, data: UInt) = {
    canRead = false
    when (addr =/= UInt(0)) {
      access(addr) := data
      for ((raddr, rdata) <- reads)
        when (addr === raddr) { rdata := data }
    }
  }
}

object ImmGen {
  def apply(sel: UInt, inst: UInt) = {
    val sign = Mux(sel === IMM_Z, SInt(0), inst(31).toSInt)
    val b30_20 = Mux(sel === IMM_U, inst(30,20).toSInt, sign)
    val b19_12 = Mux(sel =/= IMM_U && sel =/= IMM_UJ, sign, inst(19,12).toSInt)
    val b11 = Mux(sel === IMM_U || sel === IMM_Z, SInt(0),
              Mux(sel === IMM_UJ, inst(20).toSInt,
              Mux(sel === IMM_SB, inst(7).toSInt, sign)))
    val b10_5 = Mux(sel === IMM_U || sel === IMM_Z, Bits(0), inst(30,25))
    val b4_1 = Mux(sel === IMM_U, Bits(0),
               Mux(sel === IMM_S || sel === IMM_SB, inst(11,8),
               Mux(sel === IMM_Z, inst(19,16), inst(24,21))))
    val b0 = Mux(sel === IMM_S, inst(7),
             Mux(sel === IMM_I, inst(20),
             Mux(sel === IMM_Z, inst(15), Bits(0))))

    Cat(sign, b30_20, b19_12, b11, b10_5, b4_1, b0).toSInt
  }
}

class Rocket(implicit p: Parameters) extends CoreModule()(p) {
  val io = new Bundle {
    val prci = new PRCITileIO().flip
    val imem  = new FrontendIO()(p.alterPartial({case CacheName => "L1I" }))
    val dmem = new HellaCacheIO()(p.alterPartial({ case CacheName => "L1D" }))
    val ptw = new DatapathPTWIO().flip
    val fpu = new FPUIO().flip
    val rocc = new RoCCInterface().flip
  }

  val decode_table = {
    (if (true) new MDecode +: (if (xLen > 32) Seq(new M64Decode) else Nil) else Nil) ++:
    (if (usingAtomics) new ADecode +: (if (xLen > 32) Seq(new A64Decode) else Nil) else Nil) ++:
    (if (usingFPU) new FDecode +: (if (xLen > 32) Seq(new F64Decode) else Nil) else Nil) ++:
    (if (usingFPU && usingFDivSqrt) Some(new FDivSqrtDecode) else None) ++:
    (if (usingRoCC) Some(new RoCCDecode) else None) ++:
    (if (xLen > 32) Some(new I64Decode) else None) ++:
    (if (usingVM) Some(new SDecode) else None) ++:
    (if (usingDebug) Some(new DebugDecode) else None) ++:
    Seq(new IDecode)
  } flatMap(_.table)

  val ex_ctrl = Reg(new IntCtrlSigs)
  val mem_ctrl = Reg(new IntCtrlSigs)
  val wb_ctrl = Reg(new IntCtrlSigs)

  val ex_reg_xcpt_interrupt  = Reg(Bool())
  val ex_reg_valid           = Reg(Bool())
  val ex_reg_btb_hit         = Reg(Bool())
  val ex_reg_btb_resp        = Reg(io.imem.btb_resp.bits)
  val ex_reg_xcpt            = Reg(Bool())
  val ex_reg_flush_pipe      = Reg(Bool())
  val ex_reg_load_use        = Reg(Bool())
  val ex_reg_cause           = Reg(UInt())
  val ex_reg_replay = Reg(Bool())
  val ex_reg_pc = Reg(UInt())
  val ex_reg_inst = Reg(Bits())

  val mem_reg_xcpt_interrupt  = Reg(Bool())
  val mem_reg_valid           = Reg(Bool())
  val mem_reg_btb_hit         = Reg(Bool())
  val mem_reg_btb_resp        = Reg(io.imem.btb_resp.bits)
  val mem_reg_xcpt            = Reg(Bool())
  val mem_reg_replay          = Reg(Bool())
  val mem_reg_flush_pipe      = Reg(Bool())
  val mem_reg_cause           = Reg(UInt())
  val mem_reg_slow_bypass     = Reg(Bool())
  val mem_reg_load            = Reg(Bool())
  val mem_reg_store           = Reg(Bool())
  val mem_reg_pc = Reg(UInt())
  val mem_reg_inst = Reg(Bits())
  val mem_reg_wdata = Reg(Bits())
  val mem_reg_rs2 = Reg(Bits())
  val take_pc_mem = Wire(Bool())

  val wb_reg_valid           = Reg(Bool())
  val wb_reg_xcpt            = Reg(Bool())
  val wb_reg_mem_xcpt        = Reg(Bool())
  val wb_reg_replay          = Reg(Bool())
  val wb_reg_cause           = Reg(UInt())
  val wb_reg_pc = Reg(UInt())
  val wb_reg_inst = Reg(Bits())
  val wb_reg_wdata = Reg(Bits())
  val wb_reg_rs2 = Reg(Bits())
  val take_pc_wb = Wire(Bool())

  val take_pc_mem_wb = take_pc_wb || take_pc_mem
  val take_pc = take_pc_mem_wb

  // decode stage
  val id_pc = io.imem.resp.bits.pc
  val id_inst = io.imem.resp.bits.data(0).toBits; require(fetchWidth == 1)
  val id_ctrl = Wire(new IntCtrlSigs()).decode(id_inst, decode_table)
  val id_raddr3 = id_inst(31,27)
  val id_raddr2 = id_inst(24,20)
  val id_raddr1 = id_inst(19,15)
  val id_waddr  = id_inst(11,7)
  val id_load_use = Wire(Bool())
  val id_reg_fence = Reg(init=Bool(false))
  val id_ren = IndexedSeq(id_ctrl.rxs1, id_ctrl.rxs2)
  val id_raddr = IndexedSeq(id_raddr1, id_raddr2)
  val rf = new RegFile(31, xLen)
  val id_rs = id_raddr.map(rf.read _)
  val ctrl_killd = Wire(Bool())

  val csr = Module(new CSRFile)
  val id_csr_en = id_ctrl.csr =/= CSR.N
  val id_system_insn = id_ctrl.csr === CSR.I
  val id_csr_ren = (id_ctrl.csr === CSR.S || id_ctrl.csr === CSR.C) && id_raddr1 === UInt(0)
  val id_csr = Mux(id_csr_ren, CSR.R, id_ctrl.csr)
  val id_csr_addr = id_inst(31,20)
  // this is overly conservative
  val safe_csrs = CSRs.sscratch :: CSRs.sepc :: CSRs.mscratch :: CSRs.mepc :: CSRs.mcause :: CSRs.mbadaddr :: Nil
  val legal_csrs = collection.mutable.LinkedHashSet(CSRs.all:_*)
  val id_csr_flush = id_system_insn || (id_csr_en && !id_csr_ren && !DecodeLogic(id_csr_addr, safe_csrs.map(UInt(_)), (legal_csrs -- safe_csrs).toList.map(UInt(_))))

  val id_illegal_insn = !id_ctrl.legal ||
    id_ctrl.fp && !csr.io.status.fs.orR ||
    id_ctrl.rocc && !csr.io.status.xs.orR
  // stall decode for fences (now, for AMO.aq; later, for AMO.rl and FENCE)
  val id_amo_aq = id_inst(26)
  val id_amo_rl = id_inst(25)
  val id_fence_next = id_ctrl.fence || id_ctrl.amo && id_amo_rl
  val id_mem_busy = !io.dmem.ordered || io.dmem.req.valid
  val id_rocc_busy = Bool(usingRoCC) &&
    (io.rocc.busy || ex_reg_valid && ex_ctrl.rocc ||
     mem_reg_valid && mem_ctrl.rocc || wb_reg_valid && wb_ctrl.rocc)
  id_reg_fence := id_fence_next || id_reg_fence && id_mem_busy
  val id_do_fence = id_rocc_busy && id_ctrl.fence ||
    id_mem_busy && (id_ctrl.amo && id_amo_aq || id_ctrl.fence_i || id_reg_fence && (id_ctrl.mem || id_ctrl.rocc) || id_csr_en)

  val bpu = Module(new BreakpointUnit)
  bpu.io.status := csr.io.status
  bpu.io.bp := csr.io.bp
  bpu.io.pc := id_pc
  bpu.io.ea := mem_reg_wdata

  val (id_xcpt, id_cause) = checkExceptions(List(
    (csr.io.interrupt,          csr.io.interrupt_cause),
    (bpu.io.xcpt_if,            UInt(Causes.breakpoint)),
    (io.imem.resp.bits.xcpt_if, UInt(Causes.fault_fetch)),
    (id_illegal_insn,           UInt(Causes.illegal_instruction))))

  val dcache_bypass_data =
    if (fastLoadByte) io.dmem.resp.bits.data
    else if (fastLoadWord) io.dmem.resp.bits.data_word_bypass
    else wb_reg_wdata

  // detect bypass opportunities
  val ex_waddr = ex_reg_inst(11,7)
  val mem_waddr = mem_reg_inst(11,7)
  val wb_waddr = wb_reg_inst(11,7)
  val bypass_sources = IndexedSeq(
    (Bool(true), UInt(0), UInt(0)), // treat reading x0 as a bypass
    (ex_reg_valid && ex_ctrl.wxd, ex_waddr, mem_reg_wdata),
    (mem_reg_valid && mem_ctrl.wxd && !mem_ctrl.mem, mem_waddr, wb_reg_wdata),
    (mem_reg_valid && mem_ctrl.wxd, mem_waddr, dcache_bypass_data))
  val id_bypass_src = id_raddr.map(raddr => bypass_sources.map(s => s._1 && s._2 === raddr))

  // execute stage
  val bypass_mux = Vec(bypass_sources.map(_._3))
  val ex_reg_rs_bypass = Reg(Vec(id_raddr.size, Bool()))
  val ex_reg_rs_lsb = Reg(Vec(id_raddr.size, UInt()))
  val ex_reg_rs_msb = Reg(Vec(id_raddr.size, UInt()))
  val ex_rs = for (i <- 0 until id_raddr.size)
    yield Mux(ex_reg_rs_bypass(i), bypass_mux(ex_reg_rs_lsb(i)), Cat(ex_reg_rs_msb(i), ex_reg_rs_lsb(i)))
  val ex_imm = ImmGen(ex_ctrl.sel_imm, ex_reg_inst)
  val ex_op1 = MuxLookup(ex_ctrl.sel_alu1, SInt(0), Seq(
    A1_RS1 -> ex_rs(0).toSInt,
    A1_PC -> ex_reg_pc.toSInt))
  val ex_op2 = MuxLookup(ex_ctrl.sel_alu2, SInt(0), Seq(
    A2_RS2 -> ex_rs(1).toSInt,
    A2_IMM -> ex_imm,
    A2_FOUR -> SInt(4)))

  val alu = Module(new ALU)
  alu.io.dw := ex_ctrl.alu_dw
  alu.io.fn := ex_ctrl.alu_fn
  alu.io.in2 := ex_op2.toUInt
  alu.io.in1 := ex_op1.toUInt
  
  // multiplier and divider
  val div = Module(new MulDiv(width = xLen,
                              unroll = mulUnroll,
                              earlyOut = divEarlyOut))

  div.io.req.valid := ex_reg_valid && ex_ctrl.div
  div.io.req.bits.dw := ex_ctrl.alu_dw
  div.io.req.bits.fn := ex_ctrl.alu_fn
  div.io.req.bits.in1 := ex_rs(0)
  div.io.req.bits.in2 := ex_rs(1)
  div.io.req.bits.tag := ex_waddr

  ex_reg_valid := !ctrl_killd
  ex_reg_replay := !take_pc && io.imem.resp.valid && io.imem.resp.bits.replay
  ex_reg_xcpt := !ctrl_killd && id_xcpt
  ex_reg_xcpt_interrupt := !take_pc && io.imem.resp.valid && csr.io.interrupt
  when (id_xcpt) { ex_reg_cause := id_cause }

  when (!ctrl_killd) {
    ex_ctrl := id_ctrl
    ex_ctrl.csr := id_csr
    ex_reg_btb_hit := io.imem.btb_resp.valid
    when (io.imem.btb_resp.valid) { ex_reg_btb_resp := io.imem.btb_resp.bits }
    ex_reg_flush_pipe := id_ctrl.fence_i || id_csr_flush || csr.io.singleStep
    ex_reg_load_use := id_load_use

    when (id_ctrl.jalr && csr.io.status.debug) {
      ex_reg_flush_pipe := true
      ex_ctrl.fence_i := true
    }

    for (i <- 0 until id_raddr.size) {
      val do_bypass = id_bypass_src(i).reduce(_||_)
      val bypass_src = PriorityEncoder(id_bypass_src(i))
      ex_reg_rs_bypass(i) := do_bypass
      ex_reg_rs_lsb(i) := bypass_src
      when (id_ren(i) && !do_bypass) {
        ex_reg_rs_lsb(i) := id_rs(i)(bypass_src.getWidth-1,0)
        ex_reg_rs_msb(i) := id_rs(i) >> bypass_src.getWidth
      }
    }
  }
  when (!ctrl_killd || csr.io.interrupt || io.imem.resp.bits.replay) {
    ex_reg_inst := id_inst
    ex_reg_pc := id_pc
  }

  // replay inst in ex stage?
  val ex_pc_valid = ex_reg_valid || ex_reg_replay || ex_reg_xcpt_interrupt
  val wb_dcache_miss = wb_ctrl.mem && !io.dmem.resp.valid
  val replay_ex_structural = ex_ctrl.mem && !io.dmem.req.ready ||
                             ex_ctrl.div && !div.io.req.ready
  val replay_ex_load_use = wb_dcache_miss && ex_reg_load_use
  val replay_ex = ex_reg_replay || (ex_reg_valid && (replay_ex_structural || replay_ex_load_use))
  val ctrl_killx = take_pc_mem_wb || replay_ex || !ex_reg_valid
  // detect 2-cycle load-use delay for LB/LH/SC
  val ex_slow_bypass = ex_ctrl.mem_cmd === M_XSC || Vec(MT_B, MT_BU, MT_H, MT_HU).contains(ex_ctrl.mem_type)

  val (ex_xcpt, ex_cause) = checkExceptions(List(
    (ex_reg_xcpt_interrupt || ex_reg_xcpt, ex_reg_cause),
    (ex_ctrl.fp && io.fpu.illegal_rm,      UInt(Causes.illegal_instruction))))

  // memory stage
  val mem_br_taken = mem_reg_wdata(0)
  val mem_br_target = mem_reg_pc.toSInt +
    Mux(mem_ctrl.branch && mem_br_taken, ImmGen(IMM_SB, mem_reg_inst),
    Mux(mem_ctrl.jal, ImmGen(IMM_UJ, mem_reg_inst), SInt(4)))
  val mem_int_wdata = Mux(mem_ctrl.jalr, mem_br_target, mem_reg_wdata.toSInt).toUInt
  val mem_npc = (Mux(mem_ctrl.jalr, encodeVirtualAddress(mem_reg_wdata, mem_reg_wdata).toSInt, mem_br_target) & SInt(-2)).toUInt
  val mem_wrong_npc = Mux(ex_pc_valid, mem_npc =/= ex_reg_pc, Mux(io.imem.resp.valid, mem_npc =/= id_pc, Bool(true)))
  val mem_npc_misaligned = mem_npc(1)
  val mem_cfi = mem_ctrl.branch || mem_ctrl.jalr || mem_ctrl.jal
  val mem_cfi_taken = (mem_ctrl.branch && mem_br_taken) || mem_ctrl.jalr || mem_ctrl.jal
  val mem_misprediction =
    if (p(BtbKey).nEntries == 0) mem_cfi_taken
    else mem_wrong_npc
  val want_take_pc_mem = mem_reg_valid && (mem_misprediction || mem_reg_flush_pipe)
  take_pc_mem := want_take_pc_mem && !mem_npc_misaligned

  mem_reg_valid := !ctrl_killx
  mem_reg_replay := !take_pc_mem_wb && replay_ex
  mem_reg_xcpt := !ctrl_killx && ex_xcpt
  mem_reg_xcpt_interrupt := !take_pc_mem_wb && ex_reg_xcpt_interrupt
  when (ex_xcpt) { mem_reg_cause := ex_cause }

  when (ex_pc_valid) {
    mem_ctrl := ex_ctrl
    mem_reg_load := ex_ctrl.mem && isRead(ex_ctrl.mem_cmd)
    mem_reg_store := ex_ctrl.mem && isWrite(ex_ctrl.mem_cmd)
    mem_reg_btb_hit := ex_reg_btb_hit
    when (ex_reg_btb_hit) { mem_reg_btb_resp := ex_reg_btb_resp }
    mem_reg_flush_pipe := ex_reg_flush_pipe
    mem_reg_slow_bypass := ex_slow_bypass

    mem_reg_inst := ex_reg_inst
    mem_reg_pc := ex_reg_pc
    mem_reg_wdata := alu.io.out
    when (ex_ctrl.rxs2 && (ex_ctrl.mem || ex_ctrl.rocc)) {
      mem_reg_rs2 := ex_rs(1)
    }
  }

  val (mem_new_xcpt, mem_new_cause) = checkExceptions(List(
    (mem_reg_load && bpu.io.xcpt_ld,         UInt(Causes.breakpoint)),
    (mem_reg_store && bpu.io.xcpt_st,        UInt(Causes.breakpoint)),
    (want_take_pc_mem && mem_npc_misaligned, UInt(Causes.misaligned_fetch)),
    (mem_ctrl.mem && io.dmem.xcpt.ma.st,     UInt(Causes.misaligned_store)),
    (mem_ctrl.mem && io.dmem.xcpt.ma.ld,     UInt(Causes.misaligned_load)),
    (mem_ctrl.mem && io.dmem.xcpt.pf.st,     UInt(Causes.fault_store)),
    (mem_ctrl.mem && io.dmem.xcpt.pf.ld,     UInt(Causes.fault_load))))

  val (mem_xcpt, mem_cause) = checkExceptions(List(
    (mem_reg_xcpt_interrupt || mem_reg_xcpt, mem_reg_cause),
    (mem_reg_valid && mem_new_xcpt,          mem_new_cause)))

  val dcache_kill_mem = mem_reg_valid && mem_ctrl.wxd && io.dmem.replay_next // structural hazard on writeback port
  val fpu_kill_mem = mem_reg_valid && mem_ctrl.fp && io.fpu.nack_mem
  val replay_mem  = dcache_kill_mem || mem_reg_replay || fpu_kill_mem
  val killm_common = dcache_kill_mem || take_pc_wb || mem_reg_xcpt || !mem_reg_valid
  div.io.kill := killm_common && Reg(next = div.io.req.fire())
  val ctrl_killm = killm_common || mem_xcpt || fpu_kill_mem

  // writeback stage
  wb_reg_valid := !ctrl_killm
  wb_reg_replay := replay_mem && !take_pc_wb
  wb_reg_xcpt := mem_xcpt && !take_pc_wb
  wb_reg_mem_xcpt := mem_reg_valid && mem_new_xcpt && !(mem_reg_xcpt_interrupt || mem_reg_xcpt)
  when (mem_xcpt) { wb_reg_cause := mem_cause }
  when (mem_reg_valid || mem_reg_replay || mem_reg_xcpt_interrupt) {
    wb_ctrl := mem_ctrl
    wb_reg_wdata := Mux(mem_ctrl.fp && mem_ctrl.wxd, io.fpu.toint_data, mem_int_wdata)
    when (mem_ctrl.rocc) {
      wb_reg_rs2 := mem_reg_rs2
    }
    wb_reg_inst := mem_reg_inst
    wb_reg_pc := mem_reg_pc
  }

  val wb_set_sboard = wb_ctrl.div || wb_dcache_miss || wb_ctrl.rocc
  val replay_wb_common = io.dmem.s2_nack || wb_reg_replay
  val replay_wb_rocc = wb_reg_valid && wb_ctrl.rocc && !io.rocc.cmd.ready
  val replay_wb = replay_wb_common || replay_wb_rocc
  val wb_xcpt = wb_reg_xcpt || csr.io.csr_xcpt
  take_pc_wb := replay_wb || wb_xcpt || csr.io.eret

  // writeback arbitration
  val dmem_resp_xpu = !io.dmem.resp.bits.tag(0).toBool
  val dmem_resp_fpu =  io.dmem.resp.bits.tag(0).toBool
  val dmem_resp_waddr = io.dmem.resp.bits.tag >> 1
  val dmem_resp_valid = io.dmem.resp.valid && io.dmem.resp.bits.has_data
  val dmem_resp_replay = dmem_resp_valid && io.dmem.resp.bits.replay

  div.io.resp.ready := !(wb_reg_valid && wb_ctrl.wxd)
  val ll_wdata = Wire(init = div.io.resp.bits.data)
  val ll_waddr = Wire(init = div.io.resp.bits.tag)
  val ll_wen = Wire(init = div.io.resp.fire())
  if (usingRoCC) {
    io.rocc.resp.ready := !(wb_reg_valid && wb_ctrl.wxd)
    when (io.rocc.resp.fire()) {
      div.io.resp.ready := Bool(false)
      ll_wdata := io.rocc.resp.bits.data
      ll_waddr := io.rocc.resp.bits.rd
      ll_wen := Bool(true)
    }
  }
  when (dmem_resp_replay && dmem_resp_xpu) {
    div.io.resp.ready := Bool(false)
    if (usingRoCC)
      io.rocc.resp.ready := Bool(false)
    ll_waddr := dmem_resp_waddr
    ll_wen := Bool(true)
  }

  val wb_valid = wb_reg_valid && !replay_wb && !wb_xcpt
  val wb_wen = wb_valid && wb_ctrl.wxd
  val rf_wen = wb_wen || ll_wen 
  val rf_waddr = Mux(ll_wen, ll_waddr, wb_waddr)
  val rf_wdata = Mux(dmem_resp_valid && dmem_resp_xpu, io.dmem.resp.bits.data,
                 Mux(ll_wen, ll_wdata,
                 Mux(wb_ctrl.csr =/= CSR.N, csr.io.rw.rdata,
                 wb_reg_wdata)))
  when (rf_wen) { rf.write(rf_waddr, rf_wdata) }

  // hook up control/status regfile
  csr.io.exception := wb_reg_xcpt
  csr.io.cause := wb_reg_cause
  csr.io.retire := wb_valid
  csr.io.prci <> io.prci
  io.fpu.fcsr_rm := csr.io.fcsr_rm
  csr.io.fcsr_flags := io.fpu.fcsr_flags
  io.rocc.csr <> csr.io.rocc.csr
  csr.io.rocc.interrupt <> io.rocc.interrupt
  csr.io.pc := wb_reg_pc
  csr.io.badaddr := Mux(wb_reg_mem_xcpt, encodeVirtualAddress(wb_reg_wdata, wb_reg_wdata), wb_reg_pc)
  io.ptw.ptbr := csr.io.ptbr
  io.ptw.invalidate := csr.io.fatc
  io.ptw.status := csr.io.status
  csr.io.rw.addr := wb_reg_inst(31,20)
  csr.io.rw.cmd := Mux(wb_reg_valid, wb_ctrl.csr, CSR.N)
  csr.io.rw.wdata := wb_reg_wdata

  val hazard_targets = Seq((id_ctrl.rxs1 && id_raddr1 =/= UInt(0), id_raddr1),
                           (id_ctrl.rxs2 && id_raddr2 =/= UInt(0), id_raddr2),
                           (id_ctrl.wxd  && id_waddr  =/= UInt(0), id_waddr))
  val fp_hazard_targets = Seq((io.fpu.dec.ren1, id_raddr1),
                              (io.fpu.dec.ren2, id_raddr2),
                              (io.fpu.dec.ren3, id_raddr3),
                              (io.fpu.dec.wen, id_waddr))

  val sboard = new Scoreboard(32)
  sboard.clear(ll_wen, ll_waddr)
  val id_sboard_hazard = checkHazards(hazard_targets, sboard.read _)
  sboard.set(wb_set_sboard && wb_wen, wb_waddr)

  // stall for RAW/WAW hazards on CSRs, loads, AMOs, and mul/div in execute stage.
  val ex_cannot_bypass = ex_ctrl.csr =/= CSR.N || ex_ctrl.jalr || ex_ctrl.mem || ex_ctrl.div || ex_ctrl.fp || ex_ctrl.rocc
  val data_hazard_ex = ex_ctrl.wxd && checkHazards(hazard_targets, _ === ex_waddr)
  val fp_data_hazard_ex = ex_ctrl.wfd && checkHazards(fp_hazard_targets, _ === ex_waddr)
  val id_ex_hazard = ex_reg_valid && (data_hazard_ex && ex_cannot_bypass || fp_data_hazard_ex)

  // stall for RAW/WAW hazards on CSRs, LB/LH, and mul/div in memory stage.
  val mem_mem_cmd_bh =
    if (fastLoadWord) Bool(!fastLoadByte) && mem_reg_slow_bypass
    else Bool(true)
  val mem_cannot_bypass = mem_ctrl.csr =/= CSR.N || mem_ctrl.mem && mem_mem_cmd_bh || mem_ctrl.div || mem_ctrl.fp || mem_ctrl.rocc
  val data_hazard_mem = mem_ctrl.wxd && checkHazards(hazard_targets, _ === mem_waddr)
  val fp_data_hazard_mem = mem_ctrl.wfd && checkHazards(fp_hazard_targets, _ === mem_waddr)
  val id_mem_hazard = mem_reg_valid && (data_hazard_mem && mem_cannot_bypass || fp_data_hazard_mem)
  id_load_use := mem_reg_valid && data_hazard_mem && mem_ctrl.mem

  // stall for RAW/WAW hazards on load/AMO misses and mul/div in writeback.
  val data_hazard_wb = wb_ctrl.wxd && checkHazards(hazard_targets, _ === wb_waddr)
  val fp_data_hazard_wb = wb_ctrl.wfd && checkHazards(fp_hazard_targets, _ === wb_waddr)
  val id_wb_hazard = wb_reg_valid && (data_hazard_wb && wb_set_sboard || fp_data_hazard_wb)

  val id_stall_fpu = if (usingFPU) {
    val fp_sboard = new Scoreboard(32)
    fp_sboard.set((wb_dcache_miss && wb_ctrl.wfd || io.fpu.sboard_set) && wb_valid, wb_waddr)
    fp_sboard.clear(dmem_resp_replay && dmem_resp_fpu, dmem_resp_waddr)
    fp_sboard.clear(io.fpu.sboard_clr, io.fpu.sboard_clra)

    id_csr_en && !io.fpu.fcsr_rdy || checkHazards(fp_hazard_targets, fp_sboard.read _)
  } else Bool(false)

  val dcache_blocked = Reg(Bool())
  dcache_blocked := !io.dmem.req.ready && (io.dmem.req.valid || dcache_blocked)
  val rocc_blocked = Reg(Bool())
  rocc_blocked := !wb_reg_xcpt && !io.rocc.cmd.ready && (io.rocc.cmd.valid || rocc_blocked)

  val ctrl_stalld =
    id_ex_hazard || id_mem_hazard || id_wb_hazard || id_sboard_hazard ||
    id_ctrl.fp && id_stall_fpu ||
    id_ctrl.mem && dcache_blocked || // reduce activity during D$ misses
    id_ctrl.rocc && rocc_blocked || // reduce activity while RoCC is busy
    id_do_fence ||
    csr.io.csr_stall
  ctrl_killd := !io.imem.resp.valid || io.imem.resp.bits.replay || take_pc || ctrl_stalld || csr.io.interrupt

  io.imem.req.valid := take_pc
  io.imem.req.bits.speculative := !take_pc_wb
  io.imem.req.bits.pc :=
    Mux(wb_xcpt || csr.io.eret, csr.io.evec,     // exception or [m|s]ret
    Mux(replay_wb,              wb_reg_pc,       // replay
                                mem_npc)).toUInt // mispredicted branch
  io.imem.flush_icache := wb_reg_valid && wb_ctrl.fence_i && !io.dmem.s2_nack
  io.imem.flush_tlb := csr.io.fatc
  io.imem.resp.ready := !ctrl_stalld || csr.io.interrupt || take_pc_mem

  io.imem.btb_update.valid := mem_reg_valid && !mem_npc_misaligned && mem_wrong_npc && mem_cfi_taken && !take_pc_wb
  io.imem.btb_update.bits.isJump := mem_ctrl.jal || mem_ctrl.jalr
  io.imem.btb_update.bits.isReturn := mem_ctrl.jalr && mem_reg_inst(19,15) === BitPat("b00??1")
  io.imem.btb_update.bits.pc := mem_reg_pc
  io.imem.btb_update.bits.target := io.imem.req.bits.pc
  io.imem.btb_update.bits.br_pc := mem_reg_pc
  io.imem.btb_update.bits.prediction.valid := mem_reg_btb_hit
  io.imem.btb_update.bits.prediction.bits := mem_reg_btb_resp

  io.imem.bht_update.valid := mem_reg_valid && mem_ctrl.branch && !take_pc_wb
  io.imem.bht_update.bits.pc := mem_reg_pc
  io.imem.bht_update.bits.taken := mem_br_taken
  io.imem.bht_update.bits.mispredict := mem_wrong_npc
  io.imem.bht_update.bits.prediction := io.imem.btb_update.bits.prediction

  io.imem.ras_update.valid := mem_reg_valid && io.imem.btb_update.bits.isJump && !mem_npc_misaligned && !take_pc_wb
  io.imem.ras_update.bits.returnAddr := mem_int_wdata
  io.imem.ras_update.bits.isCall := mem_ctrl.wxd && mem_waddr(0)
  io.imem.ras_update.bits.isReturn := io.imem.btb_update.bits.isReturn
  io.imem.ras_update.bits.prediction := io.imem.btb_update.bits.prediction

  io.fpu.valid := !ctrl_killd && id_ctrl.fp
  io.fpu.killx := ctrl_killx
  io.fpu.killm := killm_common
  io.fpu.inst := id_inst
  io.fpu.fromint_data := ex_rs(0)
  io.fpu.dmem_resp_val := dmem_resp_valid && dmem_resp_fpu
  io.fpu.dmem_resp_data := io.dmem.resp.bits.data_word_bypass
  io.fpu.dmem_resp_type := io.dmem.resp.bits.typ
  io.fpu.dmem_resp_tag := dmem_resp_waddr

  io.dmem.req.valid     := ex_reg_valid && ex_ctrl.mem
  val ex_dcache_tag = Cat(ex_waddr, ex_ctrl.fp)
  require(coreDCacheReqTagBits >= ex_dcache_tag.getWidth)
  io.dmem.req.bits.tag  := ex_dcache_tag
  io.dmem.req.bits.cmd  := ex_ctrl.mem_cmd
  io.dmem.req.bits.typ  := ex_ctrl.mem_type
  io.dmem.req.bits.phys := Bool(false)
  io.dmem.req.bits.addr := encodeVirtualAddress(ex_rs(0), alu.io.adder_out)
  io.dmem.s1_kill := killm_common || mem_xcpt
  io.dmem.s1_data := Mux(mem_ctrl.fp, io.fpu.store_data, mem_reg_rs2)
  io.dmem.invalidate_lr := wb_xcpt

  io.rocc.cmd.valid := wb_reg_valid && wb_ctrl.rocc && !replay_wb_common
  io.rocc.exception := wb_xcpt && csr.io.status.xs.orR
  io.rocc.status := csr.io.status
  io.rocc.cmd.bits.inst := new RoCCInstruction().fromBits(wb_reg_inst)
  io.rocc.cmd.bits.rs1 := wb_reg_wdata
  io.rocc.cmd.bits.rs2 := wb_reg_rs2

  if (enableCommitLog) {
    val pc = Wire(SInt(width=xLen))
    pc := wb_reg_pc
    val inst = wb_reg_inst
    val rd = RegNext(RegNext(RegNext(id_waddr)))
    val wfd = wb_ctrl.wfd
    val wxd = wb_ctrl.wxd
    val has_data = wb_wen && !wb_set_sboard
    val priv = csr.io.status.prv

    when (wb_valid) {
      when (wfd) {
        printf ("%d 0x%x (0x%x) f%d p%d 0xXXXXXXXXXXXXXXXX\n", priv, pc, inst, rd, rd+UInt(32))
      }
      .elsewhen (wxd && rd =/= UInt(0) && has_data) {
        printf ("%d 0x%x (0x%x) x%d 0x%x\n", priv, pc, inst, rd, rf_wdata)
      }
      .elsewhen (wxd && rd =/= UInt(0) && !has_data) {
        printf ("%d 0x%x (0x%x) x%d p%d 0xXXXXXXXXXXXXXXXX\n", priv, pc, inst, rd, rd)
      }
      .otherwise {
        printf ("%d 0x%x (0x%x)\n", priv, pc, inst)
      }
    }

    when (ll_wen && rf_waddr =/= UInt(0)) {
      printf ("x%d p%d 0x%x\n", rf_waddr, rf_waddr, rf_wdata)
    }
  }
  else {
    printf("C%d: %d [%d] pc=[%x] W[r%d=%x][%d] R[r%d=%x] R[r%d=%x] inst=[%x] DASM(%x)\n",
         io.prci.id, csr.io.time(31,0), wb_valid, wb_reg_pc,
         Mux(rf_wen, rf_waddr, UInt(0)), rf_wdata, rf_wen,
         wb_reg_inst(19,15), Reg(next=Reg(next=ex_rs(0))),
         wb_reg_inst(24,20), Reg(next=Reg(next=ex_rs(1))),
         wb_reg_inst, wb_reg_inst)
  }

  def checkExceptions(x: Seq[(Bool, UInt)]) =
    (x.map(_._1).reduce(_||_), PriorityMux(x))

  def checkHazards(targets: Seq[(Bool, UInt)], cond: UInt => Bool) =
    targets.map(h => h._1 && cond(h._2)).reduce(_||_)

  def encodeVirtualAddress(a0: UInt, ea: UInt) = if (vaddrBitsExtended == vaddrBits) ea else {
    // efficient means to compress 64-bit VA into vaddrBits+1 bits
    // (VA is bad if VA(vaddrBits) != VA(vaddrBits-1))
    val a = a0 >> vaddrBits-1
    val e = ea(vaddrBits,vaddrBits-1).toSInt
    val msb =
      Mux(a === UInt(0) || a === UInt(1), e =/= SInt(0),
      Mux(a.toSInt === SInt(-1) || a.toSInt === SInt(-2), e === SInt(-1), e(0)))
    Cat(msb, ea(vaddrBits-1,0))
  }

  class Scoreboard(n: Int)
  {
    def set(en: Bool, addr: UInt): Unit = update(en, _next | mask(en, addr))
    def clear(en: Bool, addr: UInt): Unit = update(en, _next & ~mask(en, addr))
    def read(addr: UInt): Bool = r(addr)
    def readBypassed(addr: UInt): Bool = _next(addr)

    private val r = Reg(init=Bits(0, n))
    private var _next = r
    private var ens = Bool(false)
    private def mask(en: Bool, addr: UInt) = Mux(en, UInt(1) << addr, UInt(0))
    private def update(en: Bool, update: UInt) = {
      _next = update
      ens = ens || en
      when (ens) { r := _next }
    }
  }
}
