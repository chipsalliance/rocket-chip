package rocket

import Chisel._
import Instructions._
import Util._
import uncore.HTIFIO
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

  val ex_raddr1 = ex_reg_inst(19,15)
  val ex_raddr2 = ex_reg_inst(24,20)

  val bypass = Vec.fill(NBYP)(Bits())
  bypass(BYP_0) := Bits(0)
  bypass(BYP_EX) := mem_reg_wdata
  bypass(BYP_MEM) := wb_reg_wdata
  bypass(BYP_DC) := (if (conf.fastLoadByte) io.dmem.resp.bits.data_subword
                     else if (conf.fastLoadWord) io.dmem.resp.bits.data
                     else wb_reg_wdata)

  val ex_rs = for (i <- 0 until id_rs.size)
    yield Mux(ex_reg_rs_bypass(i), bypass(ex_reg_rs_lsb(i)), Cat(ex_reg_rs_msb(i), ex_reg_rs_lsb(i)))
  val ex_imm = imm(ex_reg_sel_imm, ex_reg_inst)
  val ex_op1_hi = MuxLookup(ex_reg_sel_alu1, ex_reg_pc.toSInt >> 12, Seq(
    A1_RS1 -> (ex_rs(0).toSInt >> 12),
    A1_ZERO -> SInt(0)))
  val ex_op1_lo = MuxLookup(ex_reg_sel_alu1, UInt(0), Seq(
    A1_RS1 -> ex_rs(0)(11,0),
    A1_PC -> ex_reg_pc(11,0)))
  val ex_op1 = Cat(ex_op1_hi, ex_op1_lo)
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
  val div = Module(new MulDiv(mulUnroll = if (conf.fastMulDiv) 8 else 1,
                       earlyOut = conf.fastMulDiv))
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
    // efficient means to compress 64-bit VA into VADDR_BITS+1 bits
    // (VA is bad if VA(VADDR_BITS) != VA(VADDR_BITS-1))
    val a = a0 >> VADDR_BITS-1
    val e = ea(VADDR_BITS,VADDR_BITS-1)
    Mux(a === UInt(0) || a === UInt(1), e != UInt(0),
    Mux(a === SInt(-1) || a === SInt(-2), e === SInt(-1),
    e(0)))
  }
  val ex_br_base = Mux(io.ctrl.ex_jalr, ex_rs(0), ex_reg_pc)
  val ex_br_offset = Mux(io.ctrl.ex_predicted_taken, SInt(4), ex_imm(20,0).toSInt)
  val ex_br64 = ex_br_base + ex_br_offset
  val ex_br_msb = Mux(io.ctrl.ex_jalr, vaSign(ex_rs(0), ex_br64), vaSign(ex_reg_pc, ex_br64))
  val ex_br_addr = Cat(ex_br_msb, ex_br64(VADDR_BITS-1,0))

  // D$ request interface (registered inside D$ module)
  // other signals (req_val, req_rdy) connect to control module  
  io.dmem.req.bits.addr := Cat(vaSign(ex_rs(0), alu.io.adder_out), alu.io.adder_out(VADDR_BITS-1,0)).toUInt
  io.dmem.req.bits.tag := Cat(io.ctrl.ex_waddr, io.ctrl.ex_fp_val)
  require(io.dmem.req.bits.tag.getWidth >= 6)

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
  
  // branch resolution logic
  io.ctrl.jalr_eq := ex_rs(0) === id_pc.toSInt && ex_reg_inst(31,20) === UInt(0)
  io.ctrl.ex_br_taken :=
    Mux(io.ctrl.ex_br_type === BR_EQ,  ex_rs(0) === ex_rs(1),
    Mux(io.ctrl.ex_br_type === BR_NE,  ex_rs(0) !=  ex_rs(1),
    Mux(io.ctrl.ex_br_type === BR_LT,  ex_rs(0).toSInt < ex_rs(1).toSInt,
    Mux(io.ctrl.ex_br_type === BR_GE,  ex_rs(0).toSInt >= ex_rs(1).toSInt,
    Mux(io.ctrl.ex_br_type === BR_LTU, ex_rs(0) < ex_rs(1),
    Mux(io.ctrl.ex_br_type === BR_GEU, ex_rs(0) >= ex_rs(1),
        io.ctrl.ex_br_type === BR_J))))))

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
  if (!conf.rocc.isEmpty) {
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
    if (!conf.rocc.isEmpty)
      io.rocc.resp.ready := Bool(false)
    io.ctrl.ll_waddr := dmem_resp_waddr
    io.ctrl.ll_wen := Bool(true)
  }

  io.fpu.dmem_resp_val := dmem_resp_valid && dmem_resp_fpu
  io.fpu.dmem_resp_data := io.dmem.resp.bits.data
  io.fpu.dmem_resp_type := io.dmem.resp.bits.typ
  io.fpu.dmem_resp_tag := dmem_resp_waddr

  // writeback stage
  when (!mem_reg_kill) {
    wb_reg_pc := mem_reg_pc
    wb_reg_inst := mem_reg_inst
    wb_reg_wdata := Mux(io.ctrl.mem_fp_val && io.ctrl.mem_wen, io.fpu.toint_data, mem_reg_wdata)
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
  io.imem.req.bits.currentpc := ex_reg_pc
  io.imem.req.bits.pc :=
    Mux(io.ctrl.sel_pc === PC_EX,  ex_br_addr,
    Mux(io.ctrl.sel_pc === PC_PCR, pcr.io.evec,
        wb_reg_pc)).toUInt // PC_WB
  
  // for hazard/bypass opportunity detection
  io.ctrl.ex_waddr := ex_reg_inst(11,7)
  io.ctrl.mem_waddr := mem_reg_inst(11,7)
  io.ctrl.wb_waddr := wb_reg_inst(11,7)

  printf("C: %d [%d] pc=[%x] W[r%d=%x] R[r%d=%x] R[r%d=%x] inst=[%x] DASM(%x)\n",
         pcr.io.time(32,0), io.ctrl.retire, wb_reg_pc,
         Mux(wb_wen, wb_waddr, UInt(0)), wb_wdata,
         wb_reg_inst(19,15), Reg(next=Reg(next=ex_rs(0))),
         wb_reg_inst(24,20), Reg(next=Reg(next=ex_rs(1))),
         wb_reg_inst, wb_reg_inst)
}
