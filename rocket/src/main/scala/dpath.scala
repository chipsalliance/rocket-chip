// See LICENSE for license details.

package rocket

import Chisel._
import Instructions._
import Util._
import uncore._

class Datapath extends Module
{
  val io = new Bundle {
    val host  = new HTIFIO
    val ctrl  = new CtrlDpathIO().flip
    val dmem = new HellaCacheIO
    val ptw = new DatapathPTWIO().flip
    val imem  = new CPUFrontendIO
    val fpu = new DpathFPUIO
    val rocc = new RoCCInterface().flip
  }

  // execute definitions
  val ex_reg_pc = Reg(UInt())
  val ex_reg_inst = Reg(Bits())
  val ex_reg_ctrl_fn_dw = Reg(UInt())
  val ex_reg_ctrl_fn_alu = Reg(UInt())
  val ex_reg_sel_alu2 = Reg(UInt())
  val ex_reg_sel_alu1 = Reg(UInt())
  val ex_reg_sel_imm = Reg(UInt())
  val ex_reg_kill = Reg(Bool())
  val ex_reg_rs_bypass = Vec.fill(2)(Reg(Bool()))
  val ex_reg_rs_lsb = Vec.fill(2)(Reg(Bits()))
  val ex_reg_rs_msb = Vec.fill(2)(Reg(Bits()))

  // memory definitions
  val mem_reg_pc = Reg(UInt())
  val mem_reg_inst = Reg(Bits())
  val mem_reg_wdata = Reg(Bits())
  val mem_reg_kill = Reg(Bool())
  val mem_reg_rs2 = Reg(Bits())
  
  // writeback definitions
  val wb_reg_pc = Reg(UInt())
  val wb_reg_inst = Reg(Bits())
  val wb_reg_wdata = Reg(Bits())
  val wb_wdata = Bits()
  val wb_reg_rs2 = Reg(Bits())

  // instruction decode stage
  val id_inst = io.imem.resp.bits.data
  val id_pc = io.imem.resp.bits.pc
 
  class RegFile {
    private val rf = Mem(UInt(width = 64), 31)
    private val reads = collection.mutable.ArrayBuffer[(UInt,UInt)]()
    private var canRead = true
    def read(addr: UInt) = {
      require(canRead)
      reads += addr -> UInt()
      reads.last._2 := rf(~addr)
      reads.last._2
    }
    def write(addr: UInt, data: UInt) = {
      canRead = false
      when (addr != UInt(0)) {
        rf(~addr) := data
        for ((raddr, rdata) <- reads)
          when (addr === raddr) { rdata := data }
      }
    }
  }
  val rf = new RegFile

  // RF read ports + bypass from WB stage
  val id_raddr = Vec(id_inst(19,15), id_inst(24,20))
  val id_rs = id_raddr.map(rf.read _)

  // immediate generation
  def imm(sel: Bits, inst: Bits) = {
    val sign = inst(31).toSInt
    val b30_20 = Mux(sel === IMM_U, inst(30,20).toSInt, sign)
    val b19_12 = Mux(sel != IMM_U && sel != IMM_UJ, sign, inst(19,12).toSInt)
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

  io.ctrl.inst := id_inst
  io.fpu.inst := id_inst

  // execute stage
  ex_reg_kill := io.ctrl.killd
  when (!io.ctrl.killd) {
    ex_reg_pc := id_pc
    ex_reg_inst := id_inst
    ex_reg_ctrl_fn_dw := io.ctrl.fn_dw.toUInt
    ex_reg_ctrl_fn_alu := io.ctrl.fn_alu
    ex_reg_sel_alu2 := io.ctrl.sel_alu2
    ex_reg_sel_alu1 := io.ctrl.sel_alu1
    ex_reg_sel_imm := io.ctrl.sel_imm
    ex_reg_rs_bypass := io.ctrl.bypass
    for (i <- 0 until id_rs.size) {
      when (io.ctrl.ren(i)) {
        ex_reg_rs_lsb(i) := id_rs(i)(SZ_BYP-1,0)
        when (!io.ctrl.bypass(i)) {
          ex_reg_rs_msb(i) := id_rs(i) >> SZ_BYP
        }
      }
      when (io.ctrl.bypass(i)) { ex_reg_rs_lsb(i) := io.ctrl.bypass_src(i) }
    }
  }

  val bypass = Vec.fill(NBYP)(Bits())
  bypass(BYP_0) := Bits(0)
  bypass(BYP_EX) := mem_reg_wdata
  bypass(BYP_MEM) := wb_reg_wdata
  bypass(BYP_DC) := (if(params(FastLoadByte)) io.dmem.resp.bits.data_subword
                     else if(params(FastLoadWord)) io.dmem.resp.bits.data
                     else wb_reg_wdata)

  val ex_rs = for (i <- 0 until id_rs.size)
    yield Mux(ex_reg_rs_bypass(i), bypass(ex_reg_rs_lsb(i)), Cat(ex_reg_rs_msb(i), ex_reg_rs_lsb(i)))
  val ex_imm = imm(ex_reg_sel_imm, ex_reg_inst)
  val ex_op1 = MuxLookup(ex_reg_sel_alu1, SInt(0), Seq(
    A1_RS1 -> ex_rs(0).toSInt,
    A1_PC -> ex_reg_pc.toSInt))
  val ex_op2 = MuxLookup(ex_reg_sel_alu2, SInt(0), Seq(
    A2_RS2 -> ex_rs(1).toSInt,
    A2_IMM -> ex_imm,
    A2_FOUR -> SInt(4)))

  val alu = Module(new ALU)
  alu.io.dw := ex_reg_ctrl_fn_dw
  alu.io.fn := ex_reg_ctrl_fn_alu
  alu.io.in2 := ex_op2.toUInt
  alu.io.in1 := ex_op1
  
  // multiplier and divider
  val div = Module(new MulDiv(mulUnroll = if(params(FastMulDiv)) 8 else 1,
                       earlyOut = params(FastMulDiv)))
  div.io.req.valid := io.ctrl.div_mul_val
  div.io.req.bits.dw := ex_reg_ctrl_fn_dw
  div.io.req.bits.fn := ex_reg_ctrl_fn_alu
  div.io.req.bits.in1 := ex_rs(0)
  div.io.req.bits.in2 := ex_rs(1)
  div.io.req.bits.tag := io.ctrl.ex_waddr
  div.io.kill := io.ctrl.div_mul_kill
  io.ctrl.div_mul_rdy := div.io.req.ready
  
  io.fpu.fromint_data := ex_rs(0)

  def vaSign(a0: UInt, ea: Bits) = {
    // efficient means to compress 64-bit VA into params(VAddrBits)+1 bits
    // (VA is bad if VA(params(VAddrBits)) != VA(params(VAddrBits)-1))
    val a = a0 >> params(VAddrBits)-1
    val e = ea(params(VAddrBits),params(VAddrBits)-1)
    Mux(a === UInt(0) || a === UInt(1), e != UInt(0),
    Mux(a === SInt(-1) || a === SInt(-2), e === SInt(-1),
    e(0)))
  }

  // D$ request interface (registered inside D$ module)
  // other signals (req_val, req_rdy) connect to control module  
  io.dmem.req.bits.addr := Cat(vaSign(ex_rs(0), alu.io.adder_out), alu.io.adder_out(params(VAddrBits)-1,0)).toUInt
  io.dmem.req.bits.tag := Cat(io.ctrl.ex_waddr, io.ctrl.ex_fp_val)
  require(io.dmem.req.bits.tag.getWidth >= 6)
  require(params(CoreDCacheReqTagBits) >= 6)

  // processor control regfile read
  val pcr = Module(new CSRFile)
  pcr.io.host <> io.host
  pcr.io <> io.ctrl
  pcr.io <> io.fpu
  pcr.io.rocc <> io.rocc
  pcr.io.pc := wb_reg_pc
  io.ctrl.csr_replay := pcr.io.replay
  pcr.io.uarch_counters.foreach(_ := Bool(false))

  io.ptw.ptbr := pcr.io.ptbr
  io.ptw.invalidate := pcr.io.fatc
  io.ptw.sret := io.ctrl.sret
  io.ptw.status := pcr.io.status

  // memory stage
  mem_reg_kill := ex_reg_kill
  when (!ex_reg_kill) {
    mem_reg_pc := ex_reg_pc
    mem_reg_inst := ex_reg_inst
    mem_reg_wdata := alu.io.out
  }
  when (io.ctrl.ex_rs2_val) {
    mem_reg_rs2 := ex_rs(1)
  }

  io.dmem.req.bits.data := Mux(io.ctrl.mem_fp_val, io.fpu.store_data, mem_reg_rs2)

  // writeback arbitration
  val dmem_resp_xpu = !io.dmem.resp.bits.tag(0).toBool
  val dmem_resp_fpu =  io.dmem.resp.bits.tag(0).toBool
  val dmem_resp_waddr = io.dmem.resp.bits.tag.toUInt >> UInt(1)
  val dmem_resp_valid = io.dmem.resp.valid && io.dmem.resp.bits.has_data
  val dmem_resp_replay = io.dmem.resp.bits.replay && io.dmem.resp.bits.has_data

  val ll_wdata = Bits()
  div.io.resp.ready := io.ctrl.ll_ready
  ll_wdata := div.io.resp.bits.data
  io.ctrl.ll_waddr := div.io.resp.bits.tag
  io.ctrl.ll_wen := div.io.resp.fire()
  if (!params(BuildRoCC).isEmpty) {
    io.rocc.resp.ready := io.ctrl.ll_ready
    when (io.rocc.resp.fire()) {
      div.io.resp.ready := Bool(false)
      ll_wdata := io.rocc.resp.bits.data
      io.ctrl.ll_waddr := io.rocc.resp.bits.rd
      io.ctrl.ll_wen := Bool(true)
    }
  }
  when (dmem_resp_replay && dmem_resp_xpu) {
    div.io.resp.ready := Bool(false)
    if (!params(BuildRoCC).isEmpty)
      io.rocc.resp.ready := Bool(false)
    io.ctrl.ll_waddr := dmem_resp_waddr
    io.ctrl.ll_wen := Bool(true)
  }

  io.fpu.dmem_resp_val := dmem_resp_valid && dmem_resp_fpu
  io.fpu.dmem_resp_data := io.dmem.resp.bits.data
  io.fpu.dmem_resp_type := io.dmem.resp.bits.typ
  io.fpu.dmem_resp_tag := dmem_resp_waddr

  io.ctrl.mem_br_taken := mem_reg_wdata(0)
  val mem_br_target = mem_reg_pc +
    Mux(io.ctrl.mem_branch && io.ctrl.mem_br_taken, imm(IMM_SB, mem_reg_inst),
    Mux(!io.ctrl.mem_jalr && !io.ctrl.mem_branch, imm(IMM_UJ, mem_reg_inst), SInt(4)))
  val mem_npc = Mux(io.ctrl.mem_jalr, Cat(vaSign(mem_reg_wdata, mem_reg_wdata), mem_reg_wdata(params(VAddrBits)-1,0)), mem_br_target)
  io.ctrl.mem_misprediction := mem_npc != ex_reg_pc || !io.ctrl.ex_valid
  io.ctrl.mem_rs1_ra := mem_reg_inst(19,15) === 1
  val mem_int_wdata = Mux(io.ctrl.mem_jalr, mem_br_target, mem_reg_wdata)

  // writeback stage
  when (!mem_reg_kill) {
    wb_reg_pc := mem_reg_pc
    wb_reg_inst := mem_reg_inst
    wb_reg_wdata := Mux(io.ctrl.mem_fp_val && io.ctrl.mem_wen, io.fpu.toint_data, mem_int_wdata)
  }
  when (io.ctrl.mem_rocc_val) {
    wb_reg_rs2 := mem_reg_rs2
  }
  wb_wdata := Mux(dmem_resp_valid && dmem_resp_xpu, io.dmem.resp.bits.data_subword,
              Mux(io.ctrl.ll_wen, ll_wdata,
              Mux(io.ctrl.csr != CSR.N, pcr.io.rw.rdata,
              wb_reg_wdata)))

  val wb_wen = io.ctrl.ll_wen || io.ctrl.wb_wen
  val wb_waddr = Mux(io.ctrl.ll_wen, io.ctrl.ll_waddr, io.ctrl.wb_waddr)
  when (wb_wen) { rf.write(wb_waddr, wb_wdata) }

  // scoreboard clear (for div/mul and D$ load miss writebacks)
  io.ctrl.fp_sboard_clr  := dmem_resp_replay && dmem_resp_fpu
  io.ctrl.fp_sboard_clra := dmem_resp_waddr

  // processor control regfile write
  pcr.io.rw.addr := wb_reg_inst(31,20)
  pcr.io.rw.cmd  := io.ctrl.csr
  pcr.io.rw.wdata := Mux(io.ctrl.csr === CSR.S, pcr.io.rw.rdata | wb_reg_wdata,
                     Mux(io.ctrl.csr === CSR.C, pcr.io.rw.rdata & ~wb_reg_wdata,
                     wb_reg_wdata))

  io.rocc.cmd.bits.inst := new RoCCInstruction().fromBits(wb_reg_inst)
  io.rocc.cmd.bits.rs1 := wb_reg_wdata
  io.rocc.cmd.bits.rs2 := wb_reg_rs2

  // hook up I$
  io.imem.req.bits.pc :=
    Mux(io.ctrl.sel_pc === PC_MEM, mem_npc,
    Mux(io.ctrl.sel_pc === PC_PCR, pcr.io.evec,
        wb_reg_pc)).toUInt // PC_WB
  io.imem.btb_update.bits.pc := mem_reg_pc
  io.imem.btb_update.bits.target := io.imem.req.bits.pc
  io.imem.btb_update.bits.returnAddr := mem_int_wdata
  
  // for hazard/bypass opportunity detection
  io.ctrl.ex_waddr := ex_reg_inst(11,7)
  io.ctrl.mem_waddr := mem_reg_inst(11,7)
  io.ctrl.wb_waddr := wb_reg_inst(11,7)

  printf("C%d: %d [%d] pc=[%x] W[r%d=%x][%d] R[r%d=%x] R[r%d=%x] inst=[%x] DASM(%x)\n",
         io.host.id, pcr.io.time(32,0), io.ctrl.retire, wb_reg_pc,
         Mux(wb_wen, wb_waddr, UInt(0)), wb_wdata, wb_wen,
         wb_reg_inst(19,15), Reg(next=Reg(next=ex_rs(0))),
         wb_reg_inst(24,20), Reg(next=Reg(next=ex_rs(1))),
         wb_reg_inst, wb_reg_inst)
}
