package rocket

import Chisel._
import Instructions._
import Util._
import uncore.constants.AddressConstants._

class Datapath(implicit conf: RocketConfiguration) extends Module
{
  val io = new Bundle {
    val host  = new HTIFIO(conf.tl.ln.nClients)
    val ctrl  = (new CtrlDpathIO).flip
    val dmem = new HellaCacheIO()(conf.dcache)
    val ptw = (new DatapathPTWIO).flip
    val imem  = new CPUFrontendIO()(conf.icache)
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
  val ex_reg_ctrl_sel_wb = Reg(UInt())
  val ex_reg_kill = Reg(Bool())
  val ex_reg_rs1_bypass = Reg(Bool())
  val ex_reg_rs1_lsb = Reg(Bits())
  val ex_reg_rs1_msb = Reg(Bits())
  val ex_reg_rs2_bypass = Reg(Bool())
  val ex_reg_rs2_lsb = Reg(Bits())
  val ex_reg_rs2_msb = Reg(Bits())

  // memory definitions
  val mem_reg_pc = Reg(UInt())
  val mem_reg_inst = Reg(Bits())
  val mem_reg_wdata = Reg(Bits())
  val mem_reg_kill = Reg(Bool())
  val mem_reg_rs2 = Reg(Bits())
  
  // writeback definitions
  val wb_reg_pc = Reg(UInt())
  val wb_reg_inst = Reg(Bits())
  val wb_reg_waddr = Reg(UInt())
  val wb_reg_wdata = Reg(Bits())
  val wb_reg_ll_wb = Reg(init=Bool(false))
  val wb_wdata = Bits()
  val wb_reg_rs2 = Reg(Bits())
  val wb_wen = io.ctrl.wb_wen && io.ctrl.wb_valid || wb_reg_ll_wb

  // instruction decode stage
  val id_inst = io.imem.resp.bits.data
  val id_pc = io.imem.resp.bits.pc
  
  val regfile_ = Mem(Bits(width = 64), 31)
  def readRF(a: UInt) = regfile_(~a)
  def writeRF(a: UInt, d: Bits) = regfile_(~a) := d

  val id_raddr1 = id_inst(19,15).toUInt;
  val id_raddr2 = id_inst(24,20).toUInt;

  // bypass muxes
  val id_rs1_zero = id_raddr1 === UInt(0)
  val id_rs1_ex_bypass = io.ctrl.ex_wen && id_raddr1 === io.ctrl.ex_waddr
  val id_rs1_mem_bypass = io.ctrl.mem_wen && id_raddr1 === io.ctrl.mem_waddr
  val id_rs1_bypass = id_rs1_zero || id_rs1_ex_bypass || id_rs1_mem_bypass || io.ctrl.mem_ll_bypass_rs1
  val id_rs1_bypass_src = Mux(id_rs1_zero, UInt(0), Mux(id_rs1_ex_bypass, UInt(1), Mux(io.ctrl.mem_load, UInt(3), UInt(2))))
  val id_rs1 = Mux(wb_wen && id_raddr1 === wb_reg_waddr, wb_wdata, readRF(id_raddr1))

  val id_rs2_zero = id_raddr2 === UInt(0)
  val id_rs2_ex_bypass = io.ctrl.ex_wen && id_raddr2 === io.ctrl.ex_waddr
  val id_rs2_mem_bypass = io.ctrl.mem_wen && id_raddr2 === io.ctrl.mem_waddr 
  val id_rs2_bypass = id_rs2_zero || id_rs2_ex_bypass || id_rs2_mem_bypass || io.ctrl.mem_ll_bypass_rs2
  val id_rs2_bypass_src = Mux(id_rs2_zero, UInt(0), Mux(id_rs2_ex_bypass, UInt(1), Mux(io.ctrl.mem_load, UInt(3), UInt(2))))
  val id_rs2 = Mux(wb_wen && id_raddr2 === wb_reg_waddr, wb_wdata, readRF(id_raddr2))

  // immediate generation
  def imm(sel: Bits, inst: Bits) = {
    val sign = inst(31).toSInt
    val b30_20 = Mux(sel === IMM_U, inst(30,20).toSInt, sign)
    val b19_12 = Mux(sel != IMM_U && sel != IMM_UJ, sign, inst(19,12).toSInt)
    val b11 = Mux(sel === IMM_U, SInt(0),
              Mux(sel === IMM_UJ, inst(20).toSInt,
              Mux(sel === IMM_SB, inst(7).toSInt, sign)))
    val b10_5 = Mux(sel === IMM_U, Bits(0), inst(30,25))
    val b4_1 = Mux(sel === IMM_U, Bits(0),
               Mux(sel === IMM_S || sel === IMM_SB, inst(11,8), inst(24,21)))
    val b0 = Mux(sel === IMM_S, inst(7), Mux(sel === IMM_I, inst(20), Bits(0)))
    
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
    ex_reg_ctrl_sel_wb := io.ctrl.sel_wb
    ex_reg_rs1_bypass := id_rs1_bypass && io.ctrl.ren1
    ex_reg_rs2_bypass := id_rs2_bypass && io.ctrl.ren2
    when (io.ctrl.ren1) {
      ex_reg_rs1_lsb := id_rs1_bypass_src
      when (!id_rs1_bypass) {
        ex_reg_rs1_lsb := id_rs1(id_rs1_bypass_src.getWidth-1,0)
        ex_reg_rs1_msb := id_rs1(63,id_rs1_bypass_src.getWidth)
      }
    }
    when (io.ctrl.ren2) {
      ex_reg_rs2_lsb := id_rs2_bypass_src
      when (!id_rs2_bypass) {
        ex_reg_rs2_lsb := id_rs2(id_rs2_bypass_src.getWidth-1,0)
        ex_reg_rs2_msb := id_rs2(63,id_rs2_bypass_src.getWidth)
      }
    }
  }

  val ex_raddr1 = ex_reg_inst(19,15)
  val ex_raddr2 = ex_reg_inst(24,20)

  val dmem_resp_data = if (conf.fastLoadByte) io.dmem.resp.bits.data_subword else io.dmem.resp.bits.data
  val ex_rs1 =
    Mux(ex_reg_rs1_bypass && ex_reg_rs1_lsb === UInt(3) && Bool(conf.fastLoadWord), dmem_resp_data,
    Mux(ex_reg_rs1_bypass && ex_reg_rs1_lsb === UInt(2), wb_reg_wdata,
    Mux(ex_reg_rs1_bypass && ex_reg_rs1_lsb === UInt(1), mem_reg_wdata,
    Mux(ex_reg_rs1_bypass && ex_reg_rs1_lsb === UInt(0), Bits(0),
    Mux(AVec(A1_ZERO, A1_PCHI) contains ex_reg_sel_alu1, Bits(0),
    Cat(ex_reg_rs1_msb, ex_reg_rs1_lsb))))))
  val ex_rs2 =
    Mux(ex_reg_rs2_bypass && ex_reg_rs2_lsb === UInt(3) && Bool(conf.fastLoadWord), dmem_resp_data,
    Mux(ex_reg_rs2_bypass && ex_reg_rs2_lsb === UInt(2), wb_reg_wdata,
    Mux(ex_reg_rs2_bypass && ex_reg_rs2_lsb === UInt(1), mem_reg_wdata,
    Mux(ex_reg_rs2_bypass && ex_reg_rs2_lsb === UInt(0), Bits(0),
    Cat(ex_reg_rs2_msb, ex_reg_rs2_lsb)))))

  val ex_imm = imm(ex_reg_sel_imm, ex_reg_inst)
  val ex_op1_hi = Mux(AVec(A1_PC, A1_PCHI) contains ex_reg_sel_alu1, ex_reg_pc >> 12, ex_rs1 >> 12).toSInt
  val ex_op1_lo = Mux(ex_reg_sel_alu1 === A1_PC, ex_reg_pc(11,0), ex_rs1(11,0)).toSInt
  val ex_op1 = Cat(ex_op1_hi, ex_op1_lo)
  val ex_op2 = Mux(ex_reg_sel_alu2 === A2_RS2,  ex_rs2.toSInt,
               Mux(ex_reg_sel_alu2 === A2_IMM,  ex_imm,
               Mux(ex_reg_sel_alu2 === A2_ZERO, SInt(0),
                                                SInt(4))))

  val alu = Module(new ALU)
  alu.io.dw := ex_reg_ctrl_fn_dw;
  alu.io.fn := ex_reg_ctrl_fn_alu;
  alu.io.in2 := ex_op2.toUInt
  alu.io.in1 := ex_op1.toUInt
  
  // multiplier and divider
  val div = Module(new MulDiv(mulUnroll = if (conf.fastMulDiv) 8 else 1,
                       earlyOut = conf.fastMulDiv))
  div.io.req.valid := io.ctrl.div_mul_val
  div.io.req.bits.dw := ex_reg_ctrl_fn_dw
  div.io.req.bits.fn := ex_reg_ctrl_fn_alu
  div.io.req.bits.in1 := ex_rs1
  div.io.req.bits.in2 := ex_rs2
  div.io.req.bits.tag := io.ctrl.ex_waddr
  div.io.kill := io.ctrl.div_mul_kill
  div.io.resp.ready := !io.ctrl.mem_wen
  io.ctrl.div_mul_rdy := div.io.req.ready
  
  io.fpu.fromint_data := ex_rs1
  io.ctrl.ex_waddr := ex_reg_inst(11,7)

  def vaSign(a0: UInt, ea: Bits) = {
    // efficient means to compress 64-bit VA into VADDR_BITS+1 bits
    // (VA is bad if VA(VADDR_BITS) != VA(VADDR_BITS-1))
    val a = a0 >> VADDR_BITS-1
    val e = ea(VADDR_BITS,VADDR_BITS-1)
    Mux(a === UInt(0) || a === UInt(1), e != UInt(0),
    Mux(a === SInt(-1) || a === SInt(-2), e === SInt(-1),
    e(0)))
  }
  val ex_br_base = Mux(io.ctrl.ex_jalr, ex_rs1, ex_reg_pc)
  val ex_br_offset = Mux(io.ctrl.ex_predicted_taken && !io.ctrl.ex_jalr, SInt(4), ex_imm)
  val ex_br64 = ex_br_base + ex_br_offset
  val ex_br_msb = Mux(io.ctrl.ex_jalr, vaSign(ex_rs1, ex_br64), vaSign(ex_reg_pc, ex_br64))
  val ex_br_addr = Cat(ex_br_msb, ex_br64(VADDR_BITS-1,0))

  // D$ request interface (registered inside D$ module)
  // other signals (req_val, req_rdy) connect to control module  
  io.dmem.req.bits.addr := Cat(vaSign(ex_rs1, alu.io.adder_out), alu.io.adder_out(VADDR_BITS-1,0)).toUInt
  io.dmem.req.bits.data := Mux(io.ctrl.mem_fp_val, io.fpu.store_data, mem_reg_rs2)
  io.dmem.req.bits.tag := Cat(io.ctrl.ex_waddr, io.ctrl.ex_fp_val)
  require(io.dmem.req.bits.tag.getWidth >= 6)

	// processor control regfile read
  val pcr = Module(new PCR)
  pcr.io.host <> io.host
  pcr.io <> io.ctrl
  pcr.io.pc := wb_reg_pc
  io.ctrl.pcr_replay := pcr.io.replay

  io.ptw.ptbr := pcr.io.ptbr
  io.ptw.invalidate := pcr.io.fatc
  io.ptw.eret := io.ctrl.eret
  io.ptw.status := pcr.io.status
  
	// branch resolution logic
  io.ctrl.jalr_eq := ex_rs1 === id_pc.toSInt && ex_reg_inst(31,20) === UInt(0)
  io.ctrl.ex_br_taken :=
    Mux(io.ctrl.ex_br_type === BR_EQ,  ex_rs1 === ex_rs2,
    Mux(io.ctrl.ex_br_type === BR_NE,  ex_rs1 !=  ex_rs2,
    Mux(io.ctrl.ex_br_type === BR_LT,  ex_rs1.toSInt < ex_rs2.toSInt,
    Mux(io.ctrl.ex_br_type === BR_GE,  ex_rs1.toSInt >= ex_rs2.toSInt,
    Mux(io.ctrl.ex_br_type === BR_LTU, ex_rs1 < ex_rs2,
    Mux(io.ctrl.ex_br_type === BR_GEU, ex_rs1 >= ex_rs2,
        io.ctrl.ex_br_type === BR_J))))))

  val tsc_reg = WideCounter(64)
  val irt_reg = WideCounter(64, io.ctrl.wb_valid)
  
	// writeback select mux
  val ex_wdata =
    Mux(ex_reg_ctrl_sel_wb === WB_TSC, tsc_reg.value,
    Mux(ex_reg_ctrl_sel_wb === WB_IRT, irt_reg.value,
        alu.io.out)).toBits // WB_ALU

  // memory stage
  mem_reg_kill := ex_reg_kill
  when (!ex_reg_kill) {
    mem_reg_pc := ex_reg_pc
    mem_reg_inst := ex_reg_inst
    mem_reg_wdata := ex_wdata
    when (io.ctrl.ex_rs2_val) {
      mem_reg_rs2 := ex_rs2
    }
  }
  
  // for load/use hazard detection (load byte/halfword)
  io.ctrl.mem_waddr := mem_reg_inst(11,7)

  // writeback arbitration
  val dmem_resp_xpu = !io.dmem.resp.bits.tag(0).toBool
  val dmem_resp_fpu =  io.dmem.resp.bits.tag(0).toBool
  val dmem_resp_waddr = io.dmem.resp.bits.tag.toUInt >> UInt(1)
  val dmem_resp_valid = io.dmem.resp.valid && io.dmem.resp.bits.has_data
  val dmem_resp_replay = io.dmem.resp.bits.replay && io.dmem.resp.bits.has_data

  val mem_ll_wdata = Bits()
  mem_ll_wdata := div.io.resp.bits.data
  io.ctrl.mem_ll_waddr := div.io.resp.bits.tag
  io.ctrl.mem_ll_wb := div.io.resp.valid && !io.ctrl.mem_wen
  if (!conf.rocc.isEmpty) {
    io.rocc.resp.ready := !io.ctrl.mem_wen && !io.ctrl.mem_rocc_val
    when (io.rocc.resp.fire()) {
      div.io.resp.ready := Bool(false)
      mem_ll_wdata := io.rocc.resp.bits.data
      io.ctrl.mem_ll_waddr := io.rocc.resp.bits.rd
      io.ctrl.mem_ll_wb := Bool(true)
    }
  }
  when (dmem_resp_replay && dmem_resp_xpu) {
    div.io.resp.ready := Bool(false)
    if (!conf.rocc.isEmpty)
      io.rocc.resp.ready := Bool(false)
    mem_ll_wdata := io.dmem.resp.bits.data_subword
    io.ctrl.mem_ll_waddr := dmem_resp_waddr
    io.ctrl.mem_ll_wb := Bool(true)
  }
  when (io.ctrl.mem_ll_waddr === UInt(0)) { io.ctrl.mem_ll_wb := Bool(false) }

  io.fpu.dmem_resp_val := dmem_resp_valid && dmem_resp_fpu
  io.fpu.dmem_resp_data := io.dmem.resp.bits.data
  io.fpu.dmem_resp_type := io.dmem.resp.bits.typ
  io.fpu.dmem_resp_tag := dmem_resp_waddr

  // writeback stage
  when (!mem_reg_kill) {
    wb_reg_pc := mem_reg_pc
    wb_reg_waddr := io.ctrl.mem_waddr
    wb_reg_inst := mem_reg_inst
    wb_reg_wdata := Mux(io.ctrl.mem_fp_val && io.ctrl.mem_wen, io.fpu.toint_data, mem_reg_wdata)
  }
  when (io.ctrl.mem_rocc_val) {
    wb_reg_rs2 := mem_reg_rs2
  }
  wb_reg_ll_wb := io.ctrl.mem_ll_wb
  when (io.ctrl.mem_ll_wb) {
    wb_reg_waddr := io.ctrl.mem_ll_waddr
    wb_reg_wdata := mem_ll_wdata
  }
  wb_wdata := Mux(io.ctrl.wb_load, io.dmem.resp.bits.data_subword,
              Mux(io.ctrl.pcr != PCR.N, pcr.io.rw.rdata,
              wb_reg_wdata))

  when (wb_wen) { writeRF(wb_reg_waddr, wb_wdata) }
  io.ctrl.wb_waddr := wb_reg_waddr

  // scoreboard clear (for div/mul and D$ load miss writebacks)
  io.ctrl.fp_sboard_clr  := dmem_resp_replay && dmem_resp_fpu
  io.ctrl.fp_sboard_clra := dmem_resp_waddr

	// processor control regfile write
  pcr.io.rw.addr := wb_reg_inst(19,15).toUInt
  pcr.io.rw.cmd  := io.ctrl.pcr
  pcr.io.rw.wdata := wb_reg_wdata

  io.rocc.cmd.bits.inst := new RoCCInstruction().fromBits(wb_reg_inst)
  io.rocc.cmd.bits.rs1 := wb_reg_wdata
  io.rocc.cmd.bits.rs2 := wb_reg_rs2

  // hook up I$
  io.imem.req.bits.currentpc := ex_reg_pc
  io.imem.req.bits.pc :=
    Mux(io.ctrl.sel_pc === PC_EX,  ex_br_addr,
    Mux(io.ctrl.sel_pc === PC_PCR, pcr.io.evec,
        wb_reg_pc)).toUInt // PC_WB

  printf("C: %d [%d] pc=[%x] W[r%d=%x] R[r%d=%x] R[r%d=%x] inst=[%x] DASM(%x)\n",
         tsc_reg(32,0), io.ctrl.wb_valid, wb_reg_pc,
         Mux(wb_wen, wb_reg_waddr, UInt(0)), wb_wdata,
         wb_reg_inst(19,15), Reg(next=Reg(next=ex_rs1)),
         wb_reg_inst(24,20), Reg(next=Reg(next=ex_rs2)),
         wb_reg_inst, wb_reg_inst)
}
