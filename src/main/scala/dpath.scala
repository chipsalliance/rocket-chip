package Top {

import Chisel._
import Node._;
import Constants._
import Instructions._

class ioDpath extends Bundle()
{
  val btb_hit = Bool('output);
  val inst    = Bits(32, 'output);
  val rs2     = Bits(64, 'output);
  val rs1     = Bits(64, 'output);
  val br_eq   = Bool('output);
  val br_lt   = Bool('output);
  val br_ltu  = Bool('output);
  val div_result_val = Bool('output);
  val div_rdy = Bool('output);
  val mul_result_val = Bool('output);
  val wen     = Bool('output);
  val waddr   = UFix(5, 'output);
  val alu_out = UFix(64, 'output);
  val exception = Bool('output);
  val status  = Bits(8, 'output);
}

class ioDpathImem extends Bundle()
{
  val req_addr  = UFix(32, 'output);
  val resp_data = Bits(32, 'input);
}

class ioDpathWB extends Bundle()
{
  val waddr = UFix(5, 'input);
  val wen   = Bool('input);
  val wdata = Bits(64, 'input);
}

class ioDpathAll extends Bundle()
{
  val dpath = new ioDpath();
  val host  = new ioHost();
  val ctrl  = new ioCtrl().flip();
  val debug = new ioDebug();
  val wb    = new ioDpathWB();
  val imem  = new ioDpathImem();
}

class rocketDpath extends Component
{
  val io  = new ioDpathAll();
  
  val btb = new rocketDpathBTB(); 
  val if_btb_target = btb.io.target;

  val pcr = new rocketDpathPCR(); 
  val ex_pcr = pcr.io.r.data;

  val alu = new rocketDpathALU(); 
  val ex_alu_out = alu.io.out; 
  val ex_jr_target = ex_alu_out(31,0);

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
  val if_reg_pc     = Reg(width = 32, resetVal = UFix(0, 32));

  // instruction decode definitions
  val id_reg_pc        = Reg(){UFix(width = 32)};
  val id_reg_pc_plus4  = Reg(){UFix(width = 32)};
  val id_reg_inst      = Reg(width = 32, resetVal = NOP);

  // execute definitions
  val ex_reg_pc             = Reg(width = 32, resetVal = UFix(0, 32));
  val ex_reg_pc_plus4       = Reg(width = 32, resetVal = UFix(0, 32));
  val ex_reg_inst           = Reg(width = 32, resetVal = Bits(0, 32));
  val ex_reg_raddr2         = Reg(width = 5, resetVal = UFix(0, 5));
  val ex_reg_raddr1         = Reg(width = 5, resetVal = UFix(0, 5));
  val ex_reg_rs2            = Reg(width = 64, resetVal = Bits(0, 64));
  val ex_reg_rs1            = Reg(width = 64, resetVal = Bits(0, 64));
  val ex_reg_waddr          = Reg(width = 5, resetVal = UFix(0, 5));
  val ex_reg_ctrl_sel_alu2  = Reg(width = 2, resetVal = A2_X);
  val ex_reg_ctrl_sel_alu1  = Reg(width = 1, resetVal = A1_X);
  val ex_reg_ctrl_fn_dw     = Reg(width = 1, resetVal = DW_X);
  val ex_reg_ctrl_fn_alu    = Reg(width = 4, resetVal = FN_X);
  val ex_reg_ctrl_ll_wb     = Reg(width = 1, resetVal = Bool(false));
  val ex_reg_ctrl_mul_val   = Reg(width = 1, resetVal = Bool(false));
  val ex_reg_ctrl_mul_fn    = Reg(width = 3, resetVal = MUL_X);
  val ex_reg_ctrl_div_val   = Reg(width = 1, resetVal = Bool(false));
  val ex_reg_ctrl_div_fn    = Reg(width = 4, resetVal = DIV_X);
  val ex_reg_ctrl_sel_wb    = Reg(width = 3, resetVal = WB_X);
  val ex_reg_ctrl_wen       = Reg(width = 1, resetVal = Bool(false));
  val ex_reg_ctrl_ren_pcr   = Reg(width = 1, resetVal = Bool(false));
  val ex_reg_ctrl_wen_pcr   = Reg(width = 1, resetVal = Bool(false));
  val ex_reg_ctrl_eret      = Reg(width = 1, resetVal = Bool(false));
  val ex_reg_ctrl_exception = Reg(width = 1, resetVal = Bool(false));
  val ex_reg_ctrl_cause     = Reg(width = 5, resetVal = UFix(0,5));
	val ex_wdata						  = Wire() { Bits() };
	
  // instruction fetch stage
  val if_pc_plus4 = if_reg_pc + UFix(4, 32);

  val ex_sign_extend = 
    Cat(Fill(52, ex_reg_inst(21)), ex_reg_inst(21,10));
  val ex_sign_extend_split = 
    Cat(Fill(52, ex_reg_inst(31)), ex_reg_inst(31,27), ex_reg_inst(16,10));

  val branch_adder_rhs =
    Mux(io.ctrl.sel_pc === PC_BR, Cat(ex_sign_extend_split(30,0), UFix(0, 1)),
        Cat(Fill(6, ex_reg_inst(31)), ex_reg_inst(31,7),          UFix(0, 1)));

  val ex_branch_target = ex_reg_pc + branch_adder_rhs.toUFix;

  btb.io.correct_target := ex_branch_target;

  val if_next_pc =
    Mux(io.ctrl.sel_pc === PC_4,   if_pc_plus4,
    Mux(io.ctrl.sel_pc === PC_BTB, if_btb_target,
    Mux(io.ctrl.sel_pc === PC_EX4, ex_reg_pc_plus4,
    Mux(io.ctrl.sel_pc === PC_BR,  ex_branch_target,
    Mux(io.ctrl.sel_pc === PC_J,   ex_branch_target,
    Mux(io.ctrl.sel_pc === PC_JR,  ex_jr_target.toUFix,
    Mux(io.ctrl.sel_pc === PC_PCR, ex_pcr(31,0).toUFix,
        UFix(0, 32))))))));

  when (!io.host.start){
    if_reg_pc <== UFix(0, 32); //32'hFFFF_FFFC;
  }  
  when (!io.ctrl.stallf) {
    if_reg_pc <== if_next_pc;
  }

  io.imem.req_addr :=
    Mux(io.ctrl.stallf, if_reg_pc,
        if_next_pc);

  btb.io.current_pc4    := if_pc_plus4;
  btb.io.hit            ^^ io.dpath.btb_hit;
  btb.io.wen            ^^ io.ctrl.wen_btb;
  btb.io.correct_pc4    := ex_reg_pc_plus4;

  // instruction decode stage
  when (!io.ctrl.stalld) {
    id_reg_pc <== if_reg_pc;
    id_reg_pc_plus4 <== if_pc_plus4; 
    when(io.ctrl.killf) {
      id_reg_inst <== NOP;
    }
    otherwise {
      id_reg_inst <== io.imem.resp_data;
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

  val id_waddr =
    Mux(io.ctrl.div_wb, div_result_tag,
    Mux(io.ctrl.mul_wb, mul_result_tag,
    Mux(io.ctrl.sel_wa === WA_RD, id_reg_inst(31,27).toUFix,
    Mux(io.ctrl.sel_wa === WA_RA, RA,
        UFix(0, 5)))));

  val id_rs1 =
  	Mux(io.ctrl.div_wb, div_result,
  	Mux(io.ctrl.mul_wb, mul_result,
    Mux(id_raddr1 != UFix(0, 5) && ex_reg_ctrl_wen && id_raddr1 === ex_reg_waddr, ex_wdata,
        id_rdata1)));

  val id_rs2 =
    Mux(id_raddr2 != UFix(0, 5) && ex_reg_ctrl_wen && id_raddr2 === ex_reg_waddr, ex_wdata,
        id_rdata2);
        
	val id_exception = io.ctrl.xcpt_illegal || io.ctrl.xcpt_privileged || io.ctrl.xcpt_fpu || io.ctrl.xcpt_syscall;
	val id_cause = 
		Mux(io.ctrl.xcpt_illegal, UFix(2,5),
		Mux(io.ctrl.xcpt_privileged, UFix(3,5),
		Mux(io.ctrl.xcpt_fpu, UFix(4,5),
		Mux(io.ctrl.xcpt_syscall, UFix(6,5), 
			UFix(0,5)))));

  io.dpath.inst := id_reg_inst;
  io.dpath.rs1  := id_rs1;
  io.dpath.rs2  := id_rs2;

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
  ex_reg_ctrl_cause     <== id_cause;

  when(io.ctrl.killd) {
    ex_reg_ctrl_div_val 	<== Bool(false);
    ex_reg_ctrl_mul_val   <== Bool(false);
    ex_reg_ctrl_wen     	<== Bool(false);
    ex_reg_ctrl_wen_pcr 	<== Bool(false);
    ex_reg_ctrl_eret			<== Bool(false);
  	ex_reg_ctrl_exception <== Bool(false);
  } 
  otherwise {
    ex_reg_ctrl_div_val 	<== io.ctrl.div_val;
  	ex_reg_ctrl_mul_val   <== io.ctrl.mul_val;
    ex_reg_ctrl_wen     	<== io.ctrl.wen;
    ex_reg_ctrl_wen_pcr		<== io.ctrl.wen_pcr;
    ex_reg_ctrl_eret			<== io.ctrl.eret;
    ex_reg_ctrl_exception <== id_exception;
  }

  val ex_alu_in2 =
    Mux(ex_reg_ctrl_sel_alu2 === A2_0,     UFix(0, 64),
    Mux(ex_reg_ctrl_sel_alu2 === A2_SEXT,  ex_sign_extend,
    Mux(ex_reg_ctrl_sel_alu2 === A2_SPLIT, ex_sign_extend_split,
    Mux(ex_reg_ctrl_sel_alu2 === A2_RS2,   ex_reg_rs2,
        UFix(0, 64)))));

  val ex_alu_in1 =
    Mux(ex_reg_ctrl_sel_alu1 === A1_RS1, ex_reg_rs1,
    Mux(ex_reg_ctrl_sel_alu1 === A1_LUI, Cat(Fill(32, ex_reg_inst(26)),ex_reg_inst(26,7),UFix(0, 12)),
        UFix(0, 64)));

  val ex_alu_shamt =
    Cat(ex_alu_in2(5) & ex_reg_ctrl_fn_dw === DW_64, ex_alu_in2(4,0)).toUFix;

  alu.io.dw    := ex_reg_ctrl_fn_dw;
  alu.io.fn    := ex_reg_ctrl_fn_alu;
  alu.io.shamt := ex_alu_shamt.toUFix;
  alu.io.in2   := ex_alu_in2.toUFix;
  alu.io.in1   := ex_alu_in1.toUFix;
  
  // divider
  div.io.div_fn    := ex_reg_ctrl_div_fn;
  div.io.div_val   := ex_reg_ctrl_div_val;
  div.io.div_waddr := ex_reg_waddr;
  div.io.dpath_rs1 := ex_reg_rs1;
  div.io.dpath_rs2 := ex_reg_rs2;
  div.io.div_result_rdy := io.ctrl.div_wb;
  
  io.dpath.div_rdy 				:= div.io.div_rdy;
  io.dpath.div_result_val := div.io.div_result_val;
  
  // multiplier
  mul.io.mul_val := ex_reg_ctrl_mul_val;
  mul.io.mul_fn	 := ex_reg_ctrl_mul_fn;
  mul.io.mul_tag := ex_reg_waddr;
  mul.io.in0		 := ex_reg_rs1;
  mul.io.in1		 := ex_reg_rs2;
  
  io.dpath.mul_result_val := mul.io.result_val;

	// processor control register i/o
  pcr.io.host.from_wen ^^ io.host.from_wen;
  pcr.io.host.from     ^^ io.host.from;
  pcr.io.host.to       ^^ io.host.to;

  pcr.io.r.en   := ex_reg_ctrl_ren_pcr | ex_reg_ctrl_exception | ex_reg_ctrl_eret;
  pcr.io.r.addr := 
  	Mux(ex_reg_ctrl_exception, PCR_EVEC, 
  	Mux(ex_reg_ctrl_eret, PCR_EPC, 
  		ex_reg_raddr2));
  
  pcr.io.w.addr := ex_reg_raddr2;
  pcr.io.w.en   := ex_reg_ctrl_wen_pcr;
  pcr.io.w.data := ex_reg_rs1;
  
  pcr.io.eret      	:= ex_reg_ctrl_eret;
  pcr.io.exception 	:= ex_reg_ctrl_exception;
  pcr.io.cause 			:= ex_reg_ctrl_cause;
  pcr.io.pc					:= ex_reg_pc;
  
  io.dpath.status   := pcr.io.status;
//  io.debug 					^^ pcr.io.debug;
  
 	io.debug.error_mode  := pcr.io.debug.error_mode;
 	io.debug.log_control := pcr.io.debug.log_control;

	// branch resolution logic
  io.dpath.br_eq   := (ex_reg_rs1 === ex_reg_rs2);
  io.dpath.br_ltu  := (ex_reg_rs1.toUFix < ex_reg_rs2.toUFix);
  io.dpath.br_lt :=
    (~(ex_reg_rs1(63) ^ ex_reg_rs2(63)) & io.dpath.br_ltu |
    ex_reg_rs1(63) & ~ex_reg_rs2(63)).toBool;

  io.dpath.alu_out := ex_alu_out;

	// writeback select mux
  ex_wdata :=
    Mux(ex_reg_ctrl_ll_wb, ex_reg_rs1,
    Mux(ex_reg_ctrl_sel_wb === WB_PC,  ex_reg_pc_plus4,
    Mux(ex_reg_ctrl_sel_wb === WB_ALU, ex_alu_out,
    Mux(ex_reg_ctrl_sel_wb === WB_PCR, ex_pcr,
        Bits(0, 64))))).toBits;
        
	// regfile write
  rfile.io.w0.addr := ex_reg_waddr;
  rfile.io.w0.en   := ex_reg_ctrl_wen | ex_reg_ctrl_ll_wb;
  rfile.io.w0.data := ex_wdata; 
  
  rfile.io.w1.addr ^^ io.wb.waddr;
  rfile.io.w1.en   ^^ io.wb.wen;
  rfile.io.w1.data ^^ io.wb.wdata;
  
  // clear scoreboard for "long latency" writebacks
  io.dpath.wen   := ex_reg_ctrl_ll_wb;
  io.dpath.waddr := ex_reg_waddr;
  
  // exception signal to control (for NPC select)
  io.dpath.exception := ex_reg_ctrl_exception;

}

}
