package rocket

import Chisel._
import Instructions._
import Util._
import hwacha._
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
    val vec_ctrl = (new CtrlDpathVecIO).flip
    val vec_iface = new DpathVecInterfaceIO
  }

  // execute definitions
  val ex_reg_pc = Reg(UInt())
  val ex_reg_inst = Reg(Bits())
  val ex_reg_waddr = Reg(UInt())
  val ex_reg_ctrl_fn_dw = Reg(UInt())
  val ex_reg_ctrl_fn_alu = Reg(UInt())
  val ex_reg_sel_alu2 = Reg(UInt())
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
  val mem_reg_waddr = Reg(UInt())
  val mem_reg_wdata = Reg(Bits())
  val mem_reg_kill = Reg(Bool())
  val mem_reg_store_data = Reg(Bits())
  val mem_reg_rs1 = Reg(Bits())
  val mem_reg_rs2 = Reg(Bits())
  
  // writeback definitions
  val wb_reg_pc = Reg(UInt())
  val wb_reg_inst = Reg(Bits())
  val wb_reg_waddr = Reg(UInt())
  val wb_reg_wdata = Reg(Bits())
  val wb_reg_ll_wb = Reg(init=Bool(false))
  val wb_wdata = Bits()
  val wb_reg_store_data = Reg(Bits())
  val wb_reg_rs1 = Reg(Bits())
  val wb_reg_rs2 = Reg(Bits())
  val wb_wen = io.ctrl.wb_wen && io.ctrl.wb_valid || wb_reg_ll_wb

  // instruction decode stage
  val id_inst = io.imem.resp.bits.data
  val id_pc = io.imem.resp.bits.pc
  
  val regfile_ = Mem(Bits(width = 64), 31)
  def readRF(a: UInt) = regfile_(~a)
  def writeRF(a: UInt, d: Bits) = regfile_(~a) := d

  val id_raddr1 = id_inst(26,22).toUInt;
  val id_raddr2 = id_inst(21,17).toUInt;

  // bypass muxes
  val id_rs1_zero = id_raddr1 === UInt(0)
  val id_rs1_ex_bypass = io.ctrl.ex_wen && id_raddr1 === ex_reg_waddr
  val id_rs1_mem_bypass = io.ctrl.mem_wen && id_raddr1 === mem_reg_waddr
  val id_rs1_bypass = id_rs1_zero || id_rs1_ex_bypass || id_rs1_mem_bypass || io.ctrl.mem_ll_bypass_rs1
  val id_rs1_bypass_src = Mux(id_rs1_zero, UInt(0), Mux(id_rs1_ex_bypass, UInt(1), Mux(io.ctrl.mem_load, UInt(3), UInt(2))))
  val id_rs1 = Mux(wb_wen && id_raddr1 === wb_reg_waddr, wb_wdata, readRF(id_raddr1))

  val id_rs2_zero = id_raddr2 === UInt(0)
  val id_rs2_ex_bypass = io.ctrl.ex_wen && id_raddr2 === ex_reg_waddr
  val id_rs2_mem_bypass = io.ctrl.mem_wen && id_raddr2 === mem_reg_waddr 
  val id_rs2_bypass = id_rs2_zero || id_rs2_ex_bypass || id_rs2_mem_bypass || io.ctrl.mem_ll_bypass_rs2
  val id_rs2_bypass_src = Mux(id_rs2_zero, UInt(0), Mux(id_rs2_ex_bypass, UInt(1), Mux(io.ctrl.mem_load, UInt(3), UInt(2))))
  val id_rs2 = Mux(wb_wen && id_raddr2 === wb_reg_waddr, wb_wdata, readRF(id_raddr2))

  // immediate generation
  def imm(sel: Bits, inst: Bits) = {
    val lsbs = Mux(sel === A2_LTYPE || sel === A2_ZERO, Bits(0),
               Mux(sel === A2_BTYPE, Cat(inst(31,27), inst(16,10)),
               Mux(sel === A2_JTYPE, inst(18,7),
               inst(21,10))))
    val msbs = Mux(sel === A2_ZERO,  SInt(0),
               Mux(sel === A2_LTYPE, inst(26,7).toSInt,
               Mux(sel === A2_JTYPE, inst(31,19).toSInt,
               Mux(sel === A2_ITYPE, inst(21), inst(31)).toSInt)))
    Cat(msbs, lsbs).toSInt
  }

  io.ctrl.inst := id_inst
  io.fpu.inst := id_inst

  // execute stage
  ex_reg_kill := io.ctrl.killd
  when (!io.ctrl.killd) {
    ex_reg_pc := id_pc
    ex_reg_inst := id_inst
    ex_reg_waddr := Mux(io.ctrl.sel_wa === WA_RD, id_inst(31,27).toUInt, RA)
    ex_reg_ctrl_fn_dw := io.ctrl.fn_dw.toUInt
    ex_reg_ctrl_fn_alu := io.ctrl.fn_alu
    ex_reg_sel_alu2 := io.ctrl.sel_alu2
    ex_reg_ctrl_sel_wb := io.ctrl.sel_wb
    when (io.ctrl.ren1) {
      ex_reg_rs1_bypass := id_rs1_bypass
      ex_reg_rs1_lsb := id_rs1_bypass_src
      when (!id_rs1_bypass) {
        ex_reg_rs1_lsb := id_rs1(id_rs1_bypass_src.getWidth-1,0)
        ex_reg_rs1_msb := id_rs1(63,id_rs1_bypass_src.getWidth)
      }
    }
    when (io.ctrl.ren2) {
      ex_reg_rs2_bypass := id_rs2_bypass
      ex_reg_rs2_lsb := id_rs2_bypass_src
      when (!id_rs2_bypass) {
        ex_reg_rs2_lsb := id_rs2(id_rs2_bypass_src.getWidth-1,0)
        ex_reg_rs2_msb := id_rs2(63,id_rs2_bypass_src.getWidth)
      }
    }
  }

  val ex_raddr1 = ex_reg_inst(26,22)
  val ex_raddr2 = ex_reg_inst(21,17)

  val dmem_resp_data = if (conf.fastLoadByte) io.dmem.resp.bits.data_subword else io.dmem.resp.bits.data
  val ex_rs1 =
    Mux(ex_reg_rs1_bypass && ex_reg_rs1_lsb === UInt(3) && Bool(conf.fastLoadWord), dmem_resp_data,
    Mux(ex_reg_rs1_bypass && ex_reg_rs1_lsb === UInt(2), wb_reg_wdata,
    Mux(ex_reg_rs1_bypass && ex_reg_rs1_lsb === UInt(1), mem_reg_wdata,
    Mux(ex_reg_rs1_bypass && ex_reg_rs1_lsb === UInt(0), Bits(0),
    Cat(ex_reg_rs1_msb, ex_reg_rs1_lsb)))))
  val ex_rs2 =
    Mux(ex_reg_rs2_bypass && ex_reg_rs2_lsb === UInt(3) && Bool(conf.fastLoadWord), dmem_resp_data,
    Mux(ex_reg_rs2_bypass && ex_reg_rs2_lsb === UInt(2), wb_reg_wdata,
    Mux(ex_reg_rs2_bypass && ex_reg_rs2_lsb === UInt(1), mem_reg_wdata,
    Mux(ex_reg_rs2_bypass && ex_reg_rs2_lsb === UInt(0), Bits(0),
    Cat(ex_reg_rs2_msb, ex_reg_rs2_lsb)))))
  val ex_imm = imm(ex_reg_sel_alu2, ex_reg_inst)
  val ex_op2 = Mux(ex_reg_sel_alu2 != A2_RTYPE, ex_imm, ex_rs2)

  val alu = Module(new ALU)
  alu.io.dw := ex_reg_ctrl_fn_dw;
  alu.io.fn := ex_reg_ctrl_fn_alu;
  alu.io.in2 := ex_op2.toUInt
  alu.io.in1 := ex_rs1.toUInt
  
  // multiplier and divider
  val div = Module(new MulDiv(mulUnroll = if (conf.fastMulDiv) 8 else 1,
                       earlyOut = conf.fastMulDiv))
  div.io.req.valid := io.ctrl.div_mul_val
  div.io.req.bits.dw := ex_reg_ctrl_fn_dw
  div.io.req.bits.fn := ex_reg_ctrl_fn_alu
  div.io.req.bits.in1 := ex_rs1
  div.io.req.bits.in2 := ex_rs2
  div.io.req.bits.tag := ex_reg_waddr
  div.io.kill := io.ctrl.div_mul_kill
  div.io.resp.ready := !io.ctrl.mem_wen
  io.ctrl.div_mul_rdy := div.io.req.ready
  
  io.fpu.fromint_data := ex_rs1
  io.ctrl.ex_waddr := ex_reg_waddr

  def vaSign(a0: UInt, ea: Bits) = {
    // efficient means to compress 64-bit VA into VADDR_BITS+1 bits
    // (VA is bad if VA(VADDR_BITS) != VA(VADDR_BITS-1))
    val a = a0 >> VADDR_BITS-1
    val e = ea(VADDR_BITS,VADDR_BITS-1)
    Mux(a === UInt(0) || a === UInt(1), e != UInt(0),
    Mux(a === SInt(-1) || a === SInt(-2), e === SInt(-1),
    e(0)))
  }
  val ex_effective_address = Cat(vaSign(ex_rs1, alu.io.adder_out), alu.io.adder_out(VADDR_BITS-1,0)).toUInt

  // D$ request interface (registered inside D$ module)
  // other signals (req_val, req_rdy) connect to control module  
  io.dmem.req.bits.addr := ex_effective_address
  io.dmem.req.bits.data := Mux(io.ctrl.mem_fp_val, io.fpu.store_data, mem_reg_store_data)
  io.dmem.req.bits.tag := Cat(ex_reg_waddr, io.ctrl.ex_fp_val)
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
  io.ctrl.jalr_eq := ex_rs1 === id_pc.toSInt && ex_reg_inst(21,10) === UInt(0)
  io.ctrl.ex_br_taken :=
    Mux(io.ctrl.ex_br_type === BR_EQ,  ex_rs1 === ex_rs2,
    Mux(io.ctrl.ex_br_type === BR_NE,  ex_rs1 !=  ex_rs2,
    Mux(io.ctrl.ex_br_type === BR_LT,  ex_rs1.toSInt < ex_rs2.toSInt,
    Mux(io.ctrl.ex_br_type === BR_GE,  ex_rs1.toSInt >= ex_rs2.toSInt,
    Mux(io.ctrl.ex_br_type === BR_LTU, ex_rs1 < ex_rs2,
    Mux(io.ctrl.ex_br_type === BR_GEU, ex_rs1 >= ex_rs2,
        io.ctrl.ex_br_type === BR_J))))))

  val ex_pc_plus4 = ex_reg_pc.toSInt + Mux(ex_reg_sel_alu2 === A2_LTYPE, ex_reg_inst(26,7).toSInt << 12, SInt(4))
  val ex_branch_target = ex_reg_pc.toSInt + (ex_imm << 1)
  val ex_jalr_target = (ex_effective_address >> 1 << 1).toSInt

  val tsc_reg = WideCounter(64)
  val irt_reg = WideCounter(64, io.ctrl.wb_valid)
  
	// writeback select mux
  val ex_wdata =
    Mux(ex_reg_ctrl_sel_wb === WB_PC,  ex_pc_plus4,
    Mux(ex_reg_ctrl_sel_wb === WB_TSC, tsc_reg.value,
    Mux(ex_reg_ctrl_sel_wb === WB_IRT, irt_reg.value,
        alu.io.out))).toBits // WB_ALU

  // memory stage
  mem_reg_kill := ex_reg_kill
  when (!ex_reg_kill) {
    mem_reg_pc := ex_reg_pc
    mem_reg_inst := ex_reg_inst
    mem_reg_waddr := ex_reg_waddr
    mem_reg_wdata := ex_wdata
    mem_reg_rs1 := ex_rs1
    mem_reg_rs2 := ex_rs2
    when (io.ctrl.ex_rs2_val) {
      mem_reg_store_data := StoreGen(io.ctrl.ex_mem_type, Bits(0), ex_rs2).data
    }
  }
  
  // for load/use hazard detection (load byte/halfword)
  io.ctrl.mem_waddr := mem_reg_waddr;

  // writeback arbitration
  val dmem_resp_xpu = !io.dmem.resp.bits.tag(0).toBool
  val dmem_resp_fpu =  io.dmem.resp.bits.tag(0).toBool
  val dmem_resp_waddr = io.dmem.resp.bits.tag.toUInt >> UInt(1)
  val dmem_resp_replay = io.dmem.resp.bits.replay && dmem_resp_xpu

  val mem_ll_wdata = Bits()
  mem_ll_wdata := div.io.resp.bits.data
  io.ctrl.mem_ll_waddr := div.io.resp.bits.tag
  io.ctrl.mem_ll_wb := div.io.resp.valid && !io.ctrl.mem_wen
  when (dmem_resp_replay) {
    div.io.resp.ready := Bool(false)
    mem_ll_wdata := io.dmem.resp.bits.data_subword
    io.ctrl.mem_ll_waddr := dmem_resp_waddr
    io.ctrl.mem_ll_wb := Bool(true)
  }
  when (io.ctrl.mem_ll_waddr === UInt(0)) { io.ctrl.mem_ll_wb := Bool(false) }

  io.fpu.dmem_resp_val := io.dmem.resp.valid && dmem_resp_fpu
  io.fpu.dmem_resp_data := io.dmem.resp.bits.data
  io.fpu.dmem_resp_type := io.dmem.resp.bits.typ
  io.fpu.dmem_resp_tag := dmem_resp_waddr

  // writeback stage
  when (!mem_reg_kill) {
    wb_reg_pc := mem_reg_pc
    wb_reg_inst := mem_reg_inst
    wb_reg_waddr := mem_reg_waddr
    wb_reg_wdata := Mux(io.ctrl.mem_fp_val && io.ctrl.mem_wen, io.fpu.toint_data, mem_reg_wdata)
    wb_reg_rs1 := mem_reg_rs1
    wb_reg_rs2 := mem_reg_rs2
    when (io.ctrl.mem_rs2_val) {
      wb_reg_store_data := mem_reg_store_data
    }
  }
  wb_reg_ll_wb := io.ctrl.mem_ll_wb
  when (io.ctrl.mem_ll_wb) {
    wb_reg_waddr := io.ctrl.mem_ll_waddr
    wb_reg_wdata := mem_ll_wdata
  }
  wb_wdata := Mux(io.ctrl.wb_load, io.dmem.resp.bits.data_subword,
              Mux(io.ctrl.pcr != PCR.N, pcr.io.rw.rdata,
              wb_reg_wdata))

  if (conf.vec)
  {
    // vector datapath
    val vec = Module(new rocketDpathVec)

    vec.io.ctrl <> io.vec_ctrl
    io.vec_iface <> vec.io.iface 

    vec.io.valid := io.ctrl.wb_valid && pcr.io.status.ev
    vec.io.inst := wb_reg_inst
    vec.io.vecbank := pcr.io.vecbank
    vec.io.vecbankcnt := pcr.io.vecbankcnt
    vec.io.wdata := wb_reg_wdata
    vec.io.rs2 := wb_reg_store_data

    pcr.io.vec_irq_aux := vec.io.irq_aux
    pcr.io.vec_appvl := vec.io.appvl
    pcr.io.vec_nxregs := vec.io.nxregs
    pcr.io.vec_nfregs := vec.io.nfregs

    when (vec.io.wen) { wb_wdata := vec.io.appvl }
  }

  when (wb_wen) { writeRF(wb_reg_waddr, wb_wdata) }
  io.ctrl.wb_waddr := wb_reg_waddr

  // scoreboard clear (for div/mul and D$ load miss writebacks)
  io.ctrl.fp_sboard_clr  := io.dmem.resp.bits.replay && dmem_resp_fpu
  io.ctrl.fp_sboard_clra := dmem_resp_waddr

	// processor control regfile write
  pcr.io.rw.addr := wb_reg_inst(26,22).toUInt
  pcr.io.rw.cmd  := io.ctrl.pcr
  pcr.io.rw.wdata := wb_reg_wdata

  // hook up I$
  io.imem.req.bits.currentpc := ex_reg_pc
  io.imem.req.bits.pc :=
    Mux(io.ctrl.sel_pc === PC_EX4, ex_pc_plus4,
    Mux(io.ctrl.sel_pc === PC_EX,  Mux(io.ctrl.ex_jalr, ex_jalr_target, ex_branch_target),
    Mux(io.ctrl.sel_pc === PC_PCR, pcr.io.evec,
        wb_reg_pc))).toUInt // PC_WB

  printf("C: %d [%d] pc=[%x] W[r%d=%x] R[r%d=%x] R[r%d=%x] inst=[%x] %s\n",
         tsc_reg(32,0), io.ctrl.wb_valid, wb_reg_pc,
         Mux(wb_wen, wb_reg_waddr, UInt(0)), wb_wdata,
         wb_reg_inst(26,22), wb_reg_rs1,
         wb_reg_inst(21,17), wb_reg_rs2,
         wb_reg_inst, Disassemble(wb_reg_inst))
}
