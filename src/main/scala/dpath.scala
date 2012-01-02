package Top {

import Chisel._
import Node._;
import Constants._
import Instructions._

class ioDpathDmem extends Bundle()
{
  val req_addr  = UFix(VADDR_BITS, 'output);
  val req_tag   = UFix(CPU_TAG_BITS, 'output);
  val req_data  = Bits(64, 'output);
  val resp_val  = Bool('input);
  val resp_replay = Bool('input);
  val resp_tag  = Bits(CPU_TAG_BITS, 'input);
  val resp_data = Bits(64, 'input);
  val resp_data_subword = Bits(64, 'input);
}

class ioDpathImem extends Bundle()
{
  val req_addr  = UFix(VADDR_BITS, 'output);
  val resp_data = Bits(32, 'input);
}

class ioDpathAll extends Bundle()
{
  val host  = new ioHost();
  val ctrl  = new ioCtrlDpath().flip();
  val console = new ioConsole(List("valid","bits"));
  val debug = new ioDebug();
  val dmem  = new ioDpathDmem();
  val imem  = new ioDpathImem();
  val ptbr_wen = Bool('output);
  val ptbr = UFix(PADDR_BITS, 'output);
}

class rocketDpath extends Component
{
  val io  = new ioDpathAll();
  
  val btb = new rocketDpathBTB(8); // # of entries in BTB
  
  val if_btb_target = btb.io.target;

  val pcr = new rocketDpathPCR(); 
  val ex_pcr = pcr.io.r.data;

  val alu = new rocketDpathALU(); 
  val ex_alu_out = alu.io.out; 
  val ex_alu_adder_out = alu.io.adder_out; 
  val ex_jr_target = ex_alu_adder_out(VADDR_BITS-1,0);

  val div = new rocketDivider(64);
  val div_result = div.io.div_result_bits;
  val div_result_tag = div.io.div_result_tag;
  val div_result_val = div.io.div_result_val;
  
  val mul = new rocketMultiplier();
  val mul_result = mul.io.result; 
  val mul_result_tag = mul.io.result_tag; 
  val mul_result_val = mul.io.result_val;
  
  val rfile = new rocketDpathRegfile();

  // instruction fetch definitions
  val if_reg_pc         = Reg(resetVal = UFix(START_ADDR,VADDR_BITS));

  // instruction decode definitions
  val id_reg_valid     = Reg(resetVal = Bool(false));
  val id_reg_inst      = Reg(resetVal = NOP);
  val id_reg_pc        = Reg() { UFix() };
  val id_reg_pc_plus4  = Reg() { UFix() };

  // execute definitions
  val ex_reg_valid          = Reg(resetVal = Bool(false));
  val ex_reg_pc             = Reg() { UFix() };
  val ex_reg_pc_plus4       = Reg() { UFix() };
  val ex_reg_inst           = Reg() { Bits() };
  val ex_reg_raddr2         = Reg() { UFix() };
  val ex_reg_raddr1         = Reg() { UFix() };
  val ex_reg_rs2            = Reg() { Bits() };
  val ex_reg_rs1            = Reg() { Bits() };
  val ex_reg_waddr          = Reg() { UFix() };
  val ex_reg_ctrl_sel_alu2  = Reg() { UFix() };
  val ex_reg_ctrl_sel_alu1  = Reg() { UFix() };
  val ex_reg_ctrl_eret      = Reg(resetVal = Bool(false));
  val ex_reg_ctrl_fn_dw     = Reg() { UFix() };
  val ex_reg_ctrl_fn_alu    = Reg() { UFix() };
  val ex_reg_ctrl_ll_wb     = Reg(resetVal = Bool(false));
  val ex_reg_ctrl_mul_val   = Reg(resetVal = Bool(false));
  val ex_reg_ctrl_mul_fn    = Reg() { UFix() };
  val ex_reg_ctrl_div_val   = Reg(resetVal = Bool(false));
  val ex_reg_ctrl_div_fn    = Reg() { UFix() };
  val ex_reg_ctrl_sel_wb    = Reg() { UFix() };
  val ex_reg_ctrl_wen       = Reg(resetVal = Bool(false));
  val ex_reg_ctrl_ren_pcr   = Reg(resetVal = Bool(false));
  val ex_reg_ctrl_wen_pcr   = Reg(resetVal = Bool(false));
 	val ex_wdata						  = Wire() { Bits() }; 	

  // memory definitions
  val mem_reg_valid          = Reg(resetVal = Bool(false));
  val mem_reg_pc             = Reg() { UFix() };
  val mem_reg_waddr          = Reg() { UFix() };
  val mem_reg_wdata          = Reg() { Bits() };
  val mem_reg_raddr2         = Reg() { UFix() };
  val mem_reg_ctrl_ll_wb     = Reg(resetVal = Bool(false));
  val mem_reg_ctrl_wen       = Reg(resetVal = Bool(false));
  val mem_reg_ctrl_wen_pcr   = Reg(resetVal = Bool(false));
  
  // writeback definitions
  val wb_reg_waddr          = Reg() { UFix() };
  val wb_reg_wdata          = Reg() { Bits() };
  val wb_reg_ctrl_ll_wb     = Reg(resetVal = Bool(false));
  val wb_reg_ctrl_wen       = Reg(resetVal = Bool(false));

  val r_dmem_resp_val       = Reg(resetVal = Bool(false));
  val r_dmem_resp_waddr     = Reg() { UFix() };
  
  // instruction fetch stage
  val if_pc_plus4 = if_reg_pc + UFix(4);

  val ex_sign_extend = 
    Cat(Fill(52, ex_reg_inst(21)), ex_reg_inst(21,10));
  val ex_sign_extend_split = 
    Cat(Fill(52, ex_reg_inst(31)), ex_reg_inst(31,27), ex_reg_inst(16,10));

  val branch_adder_rhs =
    Mux(io.ctrl.ex_jmp, Cat(Fill(VADDR_BITS-26, ex_reg_inst(31)), ex_reg_inst(31,7), UFix(0,1)),
      Cat(ex_sign_extend_split(VADDR_BITS-2,0), UFix(0, 1)));

  val ex_branch_target = ex_reg_pc + branch_adder_rhs.toUFix;

  btb.io.correct_target := Mux(io.ctrl.ex_jr, ex_jr_target, ex_branch_target);

  val if_next_pc =
    Mux(io.ctrl.sel_pc === PC_BTB,  if_btb_target,
    Mux(io.ctrl.sel_pc === PC_EX4,  ex_reg_pc_plus4,
    Mux(io.ctrl.sel_pc === PC_BR,   ex_branch_target,
    Mux(io.ctrl.sel_pc === PC_JR,   ex_jr_target.toUFix,
    Mux(io.ctrl.sel_pc === PC_PCR,  mem_reg_wdata(VADDR_BITS-1,0), // only used for ERET
    Mux(io.ctrl.sel_pc === PC_EVEC, pcr.io.evec,
    Mux(io.ctrl.sel_pc === PC_MEM,  mem_reg_pc,
        if_pc_plus4))))))); // PC_4
        
  when (!io.ctrl.stallf && io.host.start) {
    if_reg_pc <== if_next_pc.toUFix;
  }
  
  // FIXME: make sure PCs are properly sign extended
  io.ctrl.xcpt_ma_inst := if_next_pc(1,0) != Bits(0,2)

  io.imem.req_addr :=
    Mux(io.ctrl.stallf, if_reg_pc,
        if_next_pc.toUFix);

  btb.io.current_pc4    := if_pc_plus4;
  btb.io.hit            ^^ io.ctrl.btb_hit;
  btb.io.wen            ^^ io.ctrl.wen_btb;
  btb.io.correct_pc4    := ex_reg_pc_plus4;
  io.ctrl.btb_match     := id_reg_pc === btb.io.correct_target;

  // instruction decode stage
  when (!io.ctrl.stalld) {
    id_reg_pc <== if_reg_pc;
    id_reg_pc_plus4 <== if_pc_plus4; 
    when(io.ctrl.killf) {
      id_reg_inst  <== NOP;
      id_reg_valid <== Bool(false);
    }
    otherwise {
      id_reg_inst  <== io.imem.resp_data;
      id_reg_valid <== Bool(true);
    }
  }

  val id_raddr1 = id_reg_inst(26,22).toUFix;
  val id_raddr2 = id_reg_inst(21,17).toUFix;

	// regfile read
  rfile.io.r0.en   ^^ io.ctrl.ren2;
  rfile.io.r0.addr := id_raddr2;
  val id_rdata2 = rfile.io.r0.data;

  rfile.io.r1.en   ^^ io.ctrl.ren1;
  rfile.io.r1.addr := id_raddr1;
  val id_rdata1 = rfile.io.r1.data;

  // destination register selection
  val id_waddr =
    Mux(io.ctrl.div_wb, div_result_tag,
    Mux(io.ctrl.mul_wb, mul_result_tag,
    Mux(io.ctrl.sel_wa === WA_RD, id_reg_inst(31,27).toUFix,
    Mux(io.ctrl.sel_wa === WA_RA, RA,
        UFix(0, 5)))));

  // bypass muxes
  val rs1_mem_lu_bypass = id_raddr1 != UFix(0, 5) && io.ctrl.mem_load && id_raddr1 === mem_reg_waddr;
  val id_rs1 =
  	Mux(io.ctrl.div_wb, div_result,
  	Mux(io.ctrl.mul_wb, mul_result,
    Mux(id_raddr1 != UFix(0, 5) && ex_reg_ctrl_wen  && id_raddr1 === ex_reg_waddr,  ex_wdata,
    Mux(id_raddr1 != UFix(0, 5) && mem_reg_ctrl_wen && id_raddr1 === mem_reg_waddr, mem_reg_wdata,
    Mux(rs1_mem_lu_bypass, io.dmem.resp_data,
    Mux(id_raddr1 != UFix(0, 5) && r_dmem_resp_val  && id_raddr1 === r_dmem_resp_waddr, io.dmem.resp_data_subword,
    Mux(id_raddr1 != UFix(0, 5) && wb_reg_ctrl_wen  && id_raddr1 === wb_reg_waddr,  wb_reg_wdata,
        id_rdata1)))))));

  val rs2_mem_lu_bypass = id_raddr2 != UFix(0, 5) && io.ctrl.mem_load && id_raddr2 === mem_reg_waddr;
  val id_rs2 =
    Mux(id_raddr2 != UFix(0, 5) && ex_reg_ctrl_wen  && id_raddr2 === ex_reg_waddr,  ex_wdata,
    Mux(id_raddr2 != UFix(0, 5) && mem_reg_ctrl_wen && id_raddr2 === mem_reg_waddr, mem_reg_wdata,
    Mux(rs2_mem_lu_bypass, io.dmem.resp_data,
    Mux(id_raddr2 != UFix(0, 5) && r_dmem_resp_val  && id_raddr2 === r_dmem_resp_waddr, io.dmem.resp_data_subword,
    Mux(id_raddr2 != UFix(0, 5) && wb_reg_ctrl_wen  && id_raddr2 === wb_reg_waddr,  wb_reg_wdata,
        id_rdata2)))));

  io.ctrl.mem_lu_bypass := rs1_mem_lu_bypass || rs2_mem_lu_bypass;
  io.ctrl.inst := id_reg_inst;

  // execute stage
  ex_reg_pc             <== id_reg_pc;
  ex_reg_pc_plus4       <== id_reg_pc_plus4;
  ex_reg_inst           <== id_reg_inst;
  ex_reg_raddr2         <== id_raddr2;
  ex_reg_raddr1         <== id_raddr1;
  ex_reg_rs2            <== id_rs2;
  ex_reg_rs1            <== id_rs1;
  ex_reg_waddr          <== id_waddr;
  ex_reg_ctrl_sel_alu2  <== io.ctrl.sel_alu2;
  ex_reg_ctrl_sel_alu1  <== io.ctrl.sel_alu1.toUFix;
  ex_reg_ctrl_fn_dw     <== io.ctrl.fn_dw.toUFix;
  ex_reg_ctrl_fn_alu    <== io.ctrl.fn_alu;
  ex_reg_ctrl_mul_fn    <== io.ctrl.mul_fn;
  ex_reg_ctrl_div_fn    <== io.ctrl.div_fn;
  ex_reg_ctrl_ll_wb     <== io.ctrl.div_wb | io.ctrl.mul_wb; // TODO: verify
  ex_reg_ctrl_sel_wb    <== io.ctrl.sel_wb;
  ex_reg_ctrl_ren_pcr   <== io.ctrl.ren_pcr;

  when(io.ctrl.killd) {
    ex_reg_valid          <== Bool(false);
    ex_reg_ctrl_div_val 	<== Bool(false);
    ex_reg_ctrl_mul_val   <== Bool(false);
    ex_reg_ctrl_wen     	<== Bool(false);
    ex_reg_ctrl_wen_pcr 	<== Bool(false);
    ex_reg_ctrl_eret			<== Bool(false);
  } 
  otherwise {
    ex_reg_valid          <== id_reg_valid;
    ex_reg_ctrl_div_val 	<== io.ctrl.div_val;
  	ex_reg_ctrl_mul_val   <== io.ctrl.mul_val;
    ex_reg_ctrl_wen     	<== io.ctrl.wen;
    ex_reg_ctrl_wen_pcr		<== io.ctrl.wen_pcr;
    ex_reg_ctrl_eret			<== io.ctrl.id_eret;
  }

  val ex_alu_in2 =
    Mux(ex_reg_ctrl_sel_alu2 === A2_SEXT,  ex_sign_extend,
    Mux(ex_reg_ctrl_sel_alu2 === A2_SPLIT, ex_sign_extend_split,
    Mux(ex_reg_ctrl_sel_alu2 === A2_RS2,   ex_reg_rs2,
        UFix(0, 64)))); // A2_0

  val ex_alu_in1 =
    Mux(ex_reg_ctrl_sel_alu1 === A1_RS1, ex_reg_rs1,
        Cat(Fill(32, ex_reg_inst(26)),ex_reg_inst(26,7),UFix(0, 12))); // A1_LUI

  val ex_alu_shamt =
    Cat(ex_alu_in2(5) & ex_reg_ctrl_fn_dw === DW_64, ex_alu_in2(4,0)).toUFix;

  alu.io.dw    := ex_reg_ctrl_fn_dw;
  alu.io.fn    := ex_reg_ctrl_fn_alu;
  alu.io.shamt := ex_alu_shamt.toUFix;
  alu.io.in2   := ex_alu_in2.toUFix;
  alu.io.in1   := ex_alu_in1.toUFix;
  
  // divider
  div.io.dw        := ex_reg_ctrl_fn_dw;
  div.io.div_fn    := ex_reg_ctrl_div_fn;
  div.io.div_val   := ex_reg_ctrl_div_val && !io.ctrl.killx;
  div.io.div_waddr := ex_reg_waddr;
  div.io.dpath_rs1 := ex_reg_rs1;
  div.io.dpath_rs2 := ex_reg_rs2;
  div.io.div_result_rdy := io.ctrl.div_wb;
  
  io.ctrl.div_rdy 				:= div.io.div_rdy;
  io.ctrl.div_result_val := div.io.div_result_val;
  
  // multiplier
  mul.io.mul_val := ex_reg_ctrl_mul_val && !io.ctrl.killx;
  mul.io.dw      := ex_reg_ctrl_fn_dw;
  mul.io.mul_fn	 := ex_reg_ctrl_mul_fn;
  mul.io.mul_tag := ex_reg_waddr;
  mul.io.in0		 := ex_reg_rs1;
  mul.io.in1		 := ex_reg_rs2;
 
  io.ctrl.mul_rdy        := mul.io.mul_rdy
  io.ctrl.mul_result_val := mul.io.result_val;
  mul.io.result_rdy      := io.ctrl.mul_wb
  
  io.ctrl.ex_waddr := ex_reg_waddr; // for load/use hazard detection & bypass control

  // D$ request interface (registered inside D$ module)
  // other signals (req_val, req_rdy) connect to control module  
  io.dmem.req_addr  := ex_alu_adder_out(VADDR_BITS-1,0);
  io.dmem.req_data  := ex_reg_rs2;
  io.dmem.req_tag   := ex_reg_waddr;

	// processor control regfile read
  pcr.io.r.en   := ex_reg_ctrl_ren_pcr | ex_reg_ctrl_eret;
  pcr.io.r.addr := 
  	Mux(ex_reg_ctrl_eret, PCR_EPC, 
  		ex_reg_raddr2);

  pcr.io.host.from_wen ^^ io.host.from_wen;
  pcr.io.host.from     ^^ io.host.from;
  pcr.io.host.to       ^^ io.host.to;

  io.ctrl.irq_timer    := pcr.io.irq_timer;
  io.ctrl.irq_ipi      := pcr.io.irq_ipi;  
  io.ctrl.status       := pcr.io.status;
  io.ptbr              := pcr.io.ptbr;
  io.ptbr_wen          := pcr.io.ptbr_wen;
 	io.debug.error_mode  := pcr.io.debug.error_mode;
  
	// branch resolution logic
  io.ctrl.br_eq   := (ex_reg_rs1 === ex_reg_rs2);
  io.ctrl.br_ltu  := (ex_reg_rs1.toUFix < ex_reg_rs2.toUFix);
  io.ctrl.br_lt :=
    (~(ex_reg_rs1(63) ^ ex_reg_rs2(63)) & io.ctrl.br_ltu |
    ex_reg_rs1(63) & ~ex_reg_rs2(63)).toBool;

  // time stamp counter
  val tsc_reg = Reg(resetVal = UFix(0,64));
  tsc_reg <== tsc_reg + UFix(1);
  // instructions retired counter
  val irt_reg = Reg(resetVal = UFix(0,64));
  when (mem_reg_valid) { irt_reg <== irt_reg + UFix(1); }
  
	// writeback select mux
  ex_wdata :=
    Mux(ex_reg_ctrl_ll_wb || ex_reg_ctrl_wen_pcr, ex_reg_rs1,
    Mux(ex_reg_ctrl_sel_wb === WB_PC,  Cat(Fill(64-VADDR_BITS, ex_reg_pc_plus4(VADDR_BITS-1)), ex_reg_pc_plus4),
    Mux(ex_reg_ctrl_sel_wb === WB_PCR, ex_pcr,
    Mux(ex_reg_ctrl_sel_wb === WB_TSC, tsc_reg,
    Mux(ex_reg_ctrl_sel_wb === WB_IRT, irt_reg,
        ex_alu_out))))).toBits; // WB_ALU
        
  // memory stage
  mem_reg_pc                <== ex_reg_pc;
  mem_reg_waddr             <== ex_reg_waddr;
  mem_reg_wdata             <== ex_wdata;
  mem_reg_ctrl_ll_wb        <== ex_reg_ctrl_ll_wb;
  mem_reg_raddr2            <== ex_reg_raddr2;
  
  when (io.ctrl.killx) {
    mem_reg_valid          <== Bool(false);
    mem_reg_ctrl_wen     	 <== Bool(false);
    mem_reg_ctrl_wen_pcr 	 <== Bool(false);
  }
  otherwise {
    mem_reg_valid          <== ex_reg_valid;
    mem_reg_ctrl_wen     	 <== ex_reg_ctrl_wen;
    mem_reg_ctrl_wen_pcr 	 <== ex_reg_ctrl_wen_pcr;
  }
  
  // for load/use hazard detection (load byte/halfword)
  io.ctrl.mem_waddr := mem_reg_waddr;
  io.ctrl.mem_valid := mem_reg_valid;

  // 32/64 bit load handling (moved to earlier in file)
      
  // writeback stage
  r_dmem_resp_val     <== io.dmem.resp_val;
  r_dmem_resp_waddr   <== io.dmem.resp_tag.toUFix

  wb_reg_waddr          <== mem_reg_waddr;
  wb_reg_wdata          <== mem_reg_wdata;
  wb_reg_ctrl_ll_wb     <== mem_reg_ctrl_ll_wb;

  when (io.ctrl.killm) {
    wb_reg_ctrl_wen       <== Bool(false);
  }
  otherwise {
    wb_reg_ctrl_wen     	<== mem_reg_ctrl_wen;
  }

  // crossbar/sign extension for 8/16 bit loads (moved to earlier in file)

	// regfile write
  rfile.io.w0.addr := wb_reg_waddr;
  rfile.io.w0.en   := wb_reg_ctrl_wen | wb_reg_ctrl_ll_wb;
  rfile.io.w0.data := wb_reg_wdata; 
  
  rfile.io.w1.addr := r_dmem_resp_waddr;
  rfile.io.w1.en   := r_dmem_resp_val;
  rfile.io.w1.data := io.dmem.resp_data_subword;
  
  io.ctrl.wb_waddr := wb_reg_waddr;
  
  // scoreboard clear (for div/mul and D$ load miss writebacks)
  io.ctrl.sboard_clr0   := wb_reg_ctrl_ll_wb;
  io.ctrl.sboard_clr0a  := wb_reg_waddr;
  io.ctrl.sboard_clr1   := r_dmem_resp_val;
  io.ctrl.sboard_clr1a  := r_dmem_resp_waddr;  

	// processor control regfile write
  pcr.io.w.addr := mem_reg_raddr2;
  pcr.io.w.en   := mem_reg_ctrl_wen_pcr && !io.ctrl.killm;
  pcr.io.w.data := mem_reg_wdata;

  pcr.io.di           := io.ctrl.irq_disable;
  pcr.io.ei           := io.ctrl.irq_enable;
  pcr.io.eret      	  := io.ctrl.mem_eret;
  pcr.io.exception 	  := io.ctrl.exception;
  pcr.io.cause 			  := io.ctrl.cause;
  pcr.io.pc					  := mem_reg_pc;
  pcr.io.badvaddr_wen := io.ctrl.badvaddr_wen;
  io.console.bits := pcr.io.console_data;
  io.console.valid := pcr.io.console_val;
}

}
