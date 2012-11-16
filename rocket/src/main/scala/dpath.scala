package rocket

import Chisel._
import Node._
import Constants._
import Instructions._
import Util._
import hwacha._

class Datapath(implicit conf: RocketConfiguration) extends Component
{
  val io = new Bundle {
    val host  = new ioHTIF(conf.ntiles)
    val ctrl  = new ioCtrlDpath().flip
    val dmem = new ioHellaCache()(conf.dcache)
    val ptw = new IODatapathPTW().flip
    val imem  = new IOCPUFrontend()(conf.icache)
    val fpu = new ioDpathFPU();
    val vec_ctrl = new ioCtrlDpathVec().flip
    val vec_iface = new ioDpathVecInterface()
    val vec_imul_req = new io_imul_req
    val vec_imul_resp = Bits(INPUT, hwacha.Constants.SZ_XLEN)
  }

  // execute definitions
  val ex_reg_pc             = Reg() { UFix() };
  val ex_reg_inst           = Reg() { Bits() };
  val ex_reg_raddr1         = Reg() { UFix() };
  val ex_reg_raddr2         = Reg() { UFix() };
  val ex_reg_op2            = Reg() { Bits() };
  val ex_reg_rs2            = Reg() { Bits() };
  val ex_reg_rs1            = Reg() { Bits() };
  val ex_reg_waddr          = Reg() { UFix() };
  val ex_reg_ctrl_fn_dw     = Reg() { UFix() };
  val ex_reg_ctrl_fn_alu    = Reg() { UFix() };
  val ex_reg_ctrl_sel_wb    = Reg() { UFix() };
  val ex_wdata		    = Bits()
  val ex_reg_kill = Reg() { Bool() }

  // memory definitions
  val mem_reg_pc             = Reg() { UFix() };
  val mem_reg_inst           = Reg() { Bits() };
  val mem_reg_rs2            = Reg() { Bits() };
  val mem_reg_waddr          = Reg() { UFix() };
  val mem_reg_wdata          = Reg() { Bits() };
  val mem_reg_raddr1         = Reg() { UFix() };
  val mem_reg_raddr2         = Reg() { UFix() };
  val mem_reg_kill = Reg() { Bool() }
  
  // writeback definitions
  val wb_reg_pc             = Reg() { UFix() };
  val wb_reg_inst           = Reg() { Bits() };
  val wb_reg_rs2            = Reg() { Bits() };
  val wb_reg_waddr          = Reg() { UFix() }
  val wb_reg_wdata          = Reg() { Bits() }
  val wb_reg_vec_waddr      = Reg() { UFix() }
  val wb_reg_vec_wdata      = Reg() { Bits() }
  val wb_reg_raddr1         = Reg() { UFix() };
  val wb_reg_raddr2         = Reg() { UFix() };
  val wb_reg_ll_wb          = Reg(resetVal = Bool(false));
  val wb_wdata              = Bits(); 	

  // instruction decode stage
  val id_inst = io.imem.resp.bits.data
  val id_pc = io.imem.resp.bits.pc
  debug(id_inst)
  debug(id_pc)
  
  val regfile_ = Mem(31){Bits(width = 64)}
  def readRF(a: UFix) = regfile_(~a)
  def writeRF(a: UFix, d: Bits) = regfile_(~a) := d

  val id_raddr1 = id_inst(26,22).toUFix;
  val id_raddr2 = id_inst(21,17).toUFix;

  // bypass muxes
  val id_rs1_dmem_bypass = id_raddr1 != UFix(0) &&
    Mux(io.ctrl.ex_wen && id_raddr1 === ex_reg_waddr, Bool(false),
    Mux(io.ctrl.mem_wen && id_raddr1 === mem_reg_waddr, io.ctrl.mem_load,
        Bool(false)))
  val id_rs1 =
    Mux(id_raddr1 === UFix(0), UFix(0),
    Mux(io.ctrl.ex_wen && id_raddr1 === ex_reg_waddr,  ex_wdata,
    Mux(io.ctrl.mem_wen && id_raddr1 === mem_reg_waddr, mem_reg_wdata,
    Mux((io.ctrl.wb_wen || wb_reg_ll_wb) && id_raddr1 === wb_reg_waddr, wb_wdata,
        readRF(id_raddr1)))))

  val id_rs2_dmem_bypass = id_raddr2 != UFix(0) &&
    Mux(io.ctrl.ex_wen && id_raddr2 === ex_reg_waddr, Bool(false),
    Mux(io.ctrl.mem_wen && id_raddr2 === mem_reg_waddr, io.ctrl.mem_load,
        Bool(false)))
  val id_rs2 = Mux(id_raddr2 === UFix(0), UFix(0),
    Mux(io.ctrl.ex_wen && id_raddr2 === ex_reg_waddr,  ex_wdata,
    Mux(io.ctrl.mem_wen && id_raddr2 === mem_reg_waddr, mem_reg_wdata,
    Mux((io.ctrl.wb_wen || wb_reg_ll_wb) && id_raddr2 === wb_reg_waddr, wb_wdata,
        readRF(id_raddr2)))))

  // immediate generation
  val id_imm_bj = io.ctrl.sel_alu2 === A2_BTYPE || io.ctrl.sel_alu2 === A2_JTYPE
  val id_imm_l = io.ctrl.sel_alu2 === A2_LTYPE
  val id_imm_zero = io.ctrl.sel_alu2 === A2_ZERO || io.ctrl.sel_alu2 === A2_RTYPE
  val id_imm_ibz = io.ctrl.sel_alu2 === A2_ITYPE || io.ctrl.sel_alu2 === A2_BTYPE || id_imm_zero
  val id_imm_sign = Mux(id_imm_bj, id_inst(31),
                    Mux(id_imm_l, id_inst(26),
                    Mux(id_imm_zero, Bits(0,1),
                        id_inst(21)))) // IMM_ITYPE
  val id_imm_small = Mux(id_imm_zero, Bits(0,12),
                         Cat(Mux(id_imm_bj, id_inst(31,27), id_inst(21,17)), id_inst(16,10)))
  val id_imm = Cat(Fill(32, id_imm_sign),
                   Mux(id_imm_l, Cat(id_inst(26,7), Bits(0,12)),
                   Mux(id_imm_ibz, Cat(Fill(20, id_imm_sign), id_imm_small),
                       Cat(Fill(7, id_imm_sign), id_inst(31,7))))) // A2_JTYPE

  val id_op2_dmem_bypass = id_rs2_dmem_bypass && io.ctrl.sel_alu2 === A2_RTYPE
  val id_op2 = Mux(io.ctrl.sel_alu2 === A2_RTYPE, id_rs2, id_imm)

  io.ctrl.inst := id_inst
  io.fpu.inst := id_inst

  // execute stage
  ex_reg_kill := io.ctrl.killd
  when (!io.ctrl.killd) {
    ex_reg_pc := id_pc
    ex_reg_inst := id_inst
    ex_reg_raddr1 := id_raddr1
    ex_reg_raddr2 := id_raddr2
    ex_reg_op2 := id_op2
    ex_reg_waddr := Mux(io.ctrl.sel_wa === WA_RD, id_inst(31,27).toUFix, RA)
    ex_reg_ctrl_fn_dw := io.ctrl.fn_dw.toUFix
    ex_reg_ctrl_fn_alu := io.ctrl.fn_alu
    ex_reg_ctrl_sel_wb := io.ctrl.sel_wb
    when (io.ctrl.ren1) { ex_reg_rs1 := id_rs1 }
    when (io.ctrl.ren2) { ex_reg_rs2 := id_rs2 }
  }

  val dmem_resp_data = if (conf.fastLoadByte) io.dmem.resp.bits.data_subword else io.dmem.resp.bits.data
  val ex_rs1 = Mux(Reg(id_rs1_dmem_bypass), dmem_resp_data, ex_reg_rs1)
  val ex_rs2 = Mux(Reg(id_rs2_dmem_bypass), dmem_resp_data, ex_reg_rs2)
  val ex_op2 = Mux(Reg(id_op2_dmem_bypass), dmem_resp_data, ex_reg_op2)

  val alu = new ALU
  alu.io.dw := ex_reg_ctrl_fn_dw;
  alu.io.fn := ex_reg_ctrl_fn_alu;
  alu.io.in2 := ex_op2.toUFix
  alu.io.in1 := ex_rs1.toUFix
  
  // divider
  val div = new rocketDivider(earlyOut = true)
  div.io.req.valid := io.ctrl.div_val
  div.io.req.bits.fn := Cat(ex_reg_ctrl_fn_dw, io.ctrl.div_fn)
  div.io.req.bits.in0 := ex_rs1
  div.io.req.bits.in1 := ex_op2
  div.io.req_tag := ex_reg_waddr
  div.io.req_kill := io.ctrl.div_kill
  div.io.resp_rdy := Bool(true)
  io.ctrl.div_rdy := div.io.req.ready
  io.ctrl.div_result_val := div.io.resp_val
  
  // multiplier
  var mul_io = new rocketMultiplier(unroll = 4, earlyOut = true).io
  if (HAVE_VEC)
  {
    val vu_mul = new rocketVUMultiplier(nwbq = 1)
    vu_mul.io.vu.req <> io.vec_imul_req
    vu_mul.io.vu.resp <> io.vec_imul_resp
    mul_io = vu_mul.io.cpu
  }
  mul_io.req.valid := io.ctrl.mul_val
  mul_io.req.bits.fn := Cat(ex_reg_ctrl_fn_dw, io.ctrl.mul_fn)
  mul_io.req.bits.in0 := ex_rs1
  mul_io.req.bits.in1 := ex_op2
  mul_io.req_tag := ex_reg_waddr
  mul_io.req_kill := io.ctrl.mul_kill
  mul_io.resp_rdy := Bool(true)
  io.ctrl.mul_rdy := mul_io.req.ready
  io.ctrl.mul_result_val := mul_io.resp_val
  
  io.fpu.fromint_data := ex_rs1
  io.ctrl.ex_waddr := ex_reg_waddr

  def vaSign(a0: Bits, ea: Bits) = {
    // efficient means to compress 64-bit VA into VADDR_BITS+1 bits
    // (VA is bad if VA(VADDR_BITS) != VA(VADDR_BITS-1))
    val a = a0 >> VADDR_BITS-1
    val e = ea(VADDR_BITS,VADDR_BITS-1)
    Mux(a === UFix(0) || a === UFix(1), e != UFix(0),
    Mux(a === Fix(-1) || a === Fix(-2), e === Fix(-1),
    Bool(false)))
  }
  val ex_effective_address = Cat(vaSign(ex_rs1, alu.io.adder_out), alu.io.adder_out(VADDR_BITS-1,0)).toUFix

  // D$ request interface (registered inside D$ module)
  // other signals (req_val, req_rdy) connect to control module  
  io.dmem.req.bits.addr := ex_effective_address
  io.dmem.req.bits.data := Mux(io.ctrl.mem_fp_val, io.fpu.store_data, mem_reg_rs2)
  io.dmem.req.bits.tag := Cat(ex_reg_waddr, io.ctrl.ex_fp_val)
  require(io.dmem.req.bits.tag.getWidth >= 6)

	// processor control regfile read
  val pcr = new rocketDpathPCR()
  pcr.io.r.en   := io.ctrl.pcr != PCR_N
  pcr.io.r.addr := wb_reg_raddr1

  pcr.io.host <> io.host
  pcr.io <> io.ctrl
  pcr.io.pc := wb_reg_pc
  io.ctrl.pcr_replay := pcr.io.replay

  io.ptw.ptbr := pcr.io.ptbr
  io.ptw.invalidate := pcr.io.ptbr_wen
  io.ptw.status := pcr.io.status
  
	// branch resolution logic
  io.ctrl.jalr_eq := ex_reg_rs1 === id_pc.toFix && ex_reg_op2(id_imm_small.getWidth-1,0) === UFix(0)
  io.ctrl.ex_br_taken :=
    Mux(io.ctrl.ex_br_type === BR_EQ,  ex_rs1 === ex_rs2,
    Mux(io.ctrl.ex_br_type === BR_NE,  ex_rs1 !=  ex_rs2,
    Mux(io.ctrl.ex_br_type === BR_LT,  ex_rs1.toFix < ex_rs2.toFix,
    Mux(io.ctrl.ex_br_type === BR_GE,  ex_rs1.toFix >= ex_rs2.toFix,
    Mux(io.ctrl.ex_br_type === BR_LTU, ex_rs1 < ex_rs2,
    Mux(io.ctrl.ex_br_type === BR_GEU, ex_rs1 >= ex_rs2,
        io.ctrl.ex_br_type === BR_J))))))

  val ex_pc_plus4 = ex_reg_pc + UFix(4)
  val ex_branch_target = ex_reg_pc + Cat(ex_reg_op2(VADDR_BITS-1,0), Bits(0,1)).toUFix

  // time stamp counter
  val tsc_reg = Reg(resetVal = UFix(0,64));
  tsc_reg := tsc_reg + UFix(1);
  // instructions retired counter
  val irt_reg = Reg(resetVal = UFix(0,64));
  when (io.ctrl.wb_valid) { irt_reg := irt_reg + UFix(1); }
  
	// writeback select mux
  ex_wdata :=
    Mux(ex_reg_ctrl_sel_wb === WB_PC,  ex_pc_plus4.toFix,
    Mux(ex_reg_ctrl_sel_wb === WB_TSC, tsc_reg,
    Mux(ex_reg_ctrl_sel_wb === WB_IRT, irt_reg,
        alu.io.out))).toBits // WB_ALU

  // memory stage
  mem_reg_kill := ex_reg_kill
  when (!ex_reg_kill) {
    mem_reg_pc := ex_reg_pc
    mem_reg_inst := ex_reg_inst
    mem_reg_rs2 := StoreGen(io.ctrl.ex_mem_type, Bits(0), ex_rs2).data
    mem_reg_waddr := ex_reg_waddr
    mem_reg_wdata := ex_wdata
    mem_reg_raddr1 := ex_reg_raddr1
    mem_reg_raddr2 := ex_reg_raddr2
  }
  
  // for load/use hazard detection (load byte/halfword)
  io.ctrl.mem_waddr := mem_reg_waddr;

  // writeback arbitration
  val dmem_resp_xpu = !io.dmem.resp.bits.tag(0).toBool
  val dmem_resp_fpu =  io.dmem.resp.bits.tag(0).toBool
  val dmem_resp_waddr = io.dmem.resp.bits.tag.toUFix >> UFix(1)
  val dmem_resp_replay = io.dmem.resp.bits.replay && dmem_resp_xpu

  val mem_ll_wdata = Bits()
  mem_ll_wdata := mul_io.resp_bits
  io.ctrl.mem_ll_waddr := mul_io.resp_tag
  io.ctrl.mem_ll_wb := mul_io.resp_val
  when (div.io.resp_val) {
    mul_io.resp_rdy := Bool(false)
    mem_ll_wdata := div.io.resp_bits
    io.ctrl.mem_ll_waddr := div.io.resp_tag
    io.ctrl.mem_ll_wb := Bool(true)
  }
  when (dmem_resp_replay) {
    mul_io.resp_rdy := Bool(false)
    div.io.resp_rdy := Bool(false)
    mem_ll_wdata := io.dmem.resp.bits.data_subword
    io.ctrl.mem_ll_waddr := dmem_resp_waddr
    io.ctrl.mem_ll_wb := Bool(true)
  }

  io.fpu.dmem_resp_val := io.dmem.resp.valid && dmem_resp_fpu
  io.fpu.dmem_resp_data := io.dmem.resp.bits.data
  io.fpu.dmem_resp_type := io.dmem.resp.bits.typ
  io.fpu.dmem_resp_tag := dmem_resp_waddr

  // writeback stage
  when (!mem_reg_kill) {
    wb_reg_pc := mem_reg_pc
    wb_reg_inst := mem_reg_inst
    wb_reg_rs2 := mem_reg_rs2
    wb_reg_vec_waddr := mem_reg_waddr
    wb_reg_vec_wdata := mem_reg_wdata
    wb_reg_raddr1 := mem_reg_raddr1
    wb_reg_raddr2 := mem_reg_raddr2
    wb_reg_waddr := mem_reg_waddr
    wb_reg_wdata := Mux(io.ctrl.mem_fp_val && io.ctrl.mem_wen, io.fpu.toint_data, mem_reg_wdata)
  }
  wb_reg_ll_wb := io.ctrl.mem_ll_wb
  when (io.ctrl.mem_ll_wb) {
    wb_reg_waddr := io.ctrl.mem_ll_waddr
    wb_reg_wdata := mem_ll_wdata
  }

  if (HAVE_VEC)
  {
    // vector datapath
    val vec = new rocketDpathVec()

    vec.io.ctrl <> io.vec_ctrl
    io.vec_iface <> vec.io.iface 

    vec.io.valid := io.ctrl.wb_valid && pcr.io.status(SR_EV)
    vec.io.inst := wb_reg_inst
    vec.io.waddr := wb_reg_vec_waddr
    vec.io.raddr1 := wb_reg_raddr1
    vec.io.vecbank := pcr.io.vecbank
    vec.io.vecbankcnt := pcr.io.vecbankcnt
    vec.io.wdata := wb_reg_vec_wdata
    vec.io.rs2 := wb_reg_rs2

    pcr.io.vec_irq_aux := vec.io.irq_aux
    pcr.io.vec_appvl := vec.io.appvl
    pcr.io.vec_nxregs := vec.io.nxregs
    pcr.io.vec_nfregs := vec.io.nfregs

    wb_wdata :=
      Mux(vec.io.wen, Cat(Bits(0,52), vec.io.appvl),
      Mux(io.ctrl.wb_load, io.dmem.resp.bits.data_subword,
          wb_reg_wdata))
  }
  else
  {
    pcr.io.vec_irq_aux := UFix(0)
    pcr.io.vec_appvl := UFix(0)
    pcr.io.vec_nxregs := UFix(0)
    pcr.io.vec_nfregs := UFix(0)

    wb_wdata :=
      Mux(io.ctrl.wb_load, io.dmem.resp.bits.data_subword,
          wb_reg_wdata)
  }

  val rf_wen = io.ctrl.wb_wen || wb_reg_ll_wb
  val rf_waddr = wb_reg_waddr
  val rf_wdata = Mux(io.ctrl.wb_wen && io.ctrl.pcr != PCR_N, pcr.io.r.data, wb_wdata)
  List(rf_wen, rf_waddr, rf_wdata).map(debug _)
  when (rf_wen) { writeRF(rf_waddr, rf_wdata) }

  io.ctrl.wb_waddr := wb_reg_waddr

  // scoreboard clear (for div/mul and D$ load miss writebacks)
  io.ctrl.fp_sboard_clr  := io.dmem.resp.bits.replay && dmem_resp_fpu
  io.ctrl.fp_sboard_clra := dmem_resp_waddr

	// processor control regfile write
  pcr.io.w.addr := wb_reg_raddr1
  pcr.io.w.en   := io.ctrl.pcr === PCR_T || io.ctrl.pcr === PCR_S || io.ctrl.pcr === PCR_C
  pcr.io.w.data := Mux(io.ctrl.pcr === PCR_S, pcr.io.r.data | wb_reg_wdata,
                   Mux(io.ctrl.pcr === PCR_C, pcr.io.r.data & ~wb_reg_wdata,
                   wb_reg_wdata))

  // hook up I$
  io.imem.req.bits.currentpc := ex_reg_pc
  io.imem.req.bits.pc :=
    Mux(io.ctrl.sel_pc === PC_EX4, ex_pc_plus4,
    Mux(io.ctrl.sel_pc === PC_EX,  Mux(io.ctrl.ex_jalr, ex_effective_address, ex_branch_target),
    Mux(io.ctrl.sel_pc === PC_PCR, Cat(pcr.io.evec(VADDR_BITS-1), pcr.io.evec).toUFix,
        wb_reg_pc))) // PC_WB
}
