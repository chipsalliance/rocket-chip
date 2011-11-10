package Top {

import Chisel._
import Node._;

import Constants._
import Instructions._

class ioCtrlDpath extends Bundle()
{
  // outputs to datapath
  val sel_pc   = UFix(4, 'output);
  val wen_btb  = Bool('output);
  val stallf   = Bool('output);
  val stalld   = Bool('output);
  val killf    = Bool('output);
  val killd    = Bool('output);
  val killx    = Bool('output);
  val killm    = Bool('output);
  val ren2     = Bool('output);
  val ren1     = Bool('output);
  val sel_alu2 = UFix(2, 'output);
  val sel_alu1 = Bool('output);
  val fn_dw    = Bool('output);
  val fn_alu   = UFix(4, 'output);
  val mul_val  = Bool('output);
  val mul_fn   = UFix(3, 'output);
  val mul_wb   = Bool('output);
  val div_val  = Bool('output);
  val div_fn   = UFix(4, 'output);
  val div_wb   = Bool('output);
  val sel_wa   = Bool('output);
  val sel_wb   = UFix(3, 'output);
  val ren_pcr  = Bool('output);
  val wen_pcr  = Bool('output);
  val eret     = Bool('output);
  val mem_load = Bool('output);
  val wen      = Bool('output);
  // exception handling
  val exception = Bool('output);
  val cause    = UFix(5,'output);
  val badvaddr_wen = Bool('output); // high for any access fault
  val badvaddr_sel = Bool('output); // select between instruction PC or load/store addr
  // inputs from datapath
  val btb_hit = Bool('input);
  val inst    = Bits(32, 'input);
  val br_eq   = Bool('input);
  val br_lt   = Bool('input);
  val br_ltu  = Bool('input);
  val div_rdy = Bool('input);
  val div_result_val = Bool('input);
  val mul_result_val = Bool('input);
  val ex_waddr = UFix(5,'input);  // write addr from execute stage
  val mem_waddr = UFix(5,'input); // write addr from memory stage
  val wb_waddr = UFix(5,'input);  // write addr from writeback stage
  val status  = Bits(17, 'input);
  val sboard_clr0  = Bool('input);
  val sboard_clr0a = UFix(5, 'input);
  val sboard_clr1  = Bool('input);
  val sboard_clr1a = UFix(5, 'input);
}

class ioCtrlAll extends Bundle()
{
  val dpath   = new ioCtrlDpath();
  val console = new ioConsole(List("rdy", "valid"));
  val imem    = new ioImem(List("req_val", "req_rdy", "resp_val")).flip();
  val dmem    = new ioDmem(List("req_val", "req_rdy", "req_cmd", "req_type", "resp_miss")).flip();
  val host    = new ioHost(List("start"));
  val dtlb_miss = Bool('input);
  val xcpt_dtlb_ld = Bool('input);
  val xcpt_dtlb_st = Bool('input);
  val xcpt_itlb = Bool('input);
}

class rocketCtrl extends Component
{
  val io = new ioCtrlAll();
  
  val xpr64 = Y;
  val cs =   
  ListLookup(io.dpath.inst,
     List(            N,     BR_N,  REN_N,REN_N,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,N,N,N,N),
     Array(
      BNE->      List(Y,     BR_NE, REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,N,N,N,N),
      ADDI->     List(Y,     BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,N,N,N,N),
      BEQ->      List(Y,     BR_EQ, REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,N,N,N,N),
      BLT->      List(Y,     BR_LT, REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,N,N,N,N),
      BLTU->     List(Y,     BR_LTU,REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,N,N,N,N),
      BGE->      List(Y,     BR_GE, REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,N,N,N,N),
      BGEU->     List(Y,     BR_GEU,REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,N,N,N,N),

      J->        List(Y,     BR_J,  REN_N,REN_N,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,N,N,N,N),
      JAL->      List(Y,     BR_J,  REN_N,REN_N,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RA,WB_PC, REN_N,WEN_N,N,N,N,N),
      JALR_C->   List(Y,     BR_JR, REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_PC, REN_N,WEN_N,N,N,N,N),
      JALR_J->   List(Y,     BR_JR, REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_PC, REN_N,WEN_N,N,N,N,N),
      JALR_R->   List(Y,     BR_JR, REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_PC, REN_N,WEN_N,N,N,N,N),
      RDNPC->    List(Y,     BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_PC, REN_N,WEN_N,N,N,N,N),

      LB->       List(Y,     BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_ADD, M_Y,M_XRD,    MT_B, N,MUL_X,     N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,N,N,N,N),
      LH->       List(Y,     BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_ADD, M_Y,M_XRD,    MT_H, N,MUL_X,     N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,N,N,N,N),
      LW->       List(Y,     BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_ADD, M_Y,M_XRD,    MT_W, N,MUL_X,     N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,N,N,N,N),
      LD->       List(xpr64, BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_ADD, M_Y,M_XRD,    MT_D, N,MUL_X,     N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,N,N,N,N),
      LBU->      List(Y,     BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_ADD, M_Y,M_XRD,    MT_BU,N,MUL_X,     N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,N,N,N,N),
      LHU->      List(Y,     BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_ADD, M_Y,M_XRD,    MT_HU,N,MUL_X,     N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,N,N,N,N),
      LWU->      List(xpr64, BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_ADD, M_Y,M_XRD,    MT_WU,N,MUL_X,     N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,N,N,N,N),
      SB->       List(Y,     BR_N,  REN_Y,REN_Y,A2_SPLIT,A1_RS1,DW_XPR,FN_ADD, M_Y,M_XWR,    MT_B, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,N,N,N,N),
      SH->       List(Y,     BR_N,  REN_Y,REN_Y,A2_SPLIT,A1_RS1,DW_XPR,FN_ADD, M_Y,M_XWR,    MT_H, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,N,N,N,N),
      SW->       List(Y,     BR_N,  REN_Y,REN_Y,A2_SPLIT,A1_RS1,DW_XPR,FN_ADD, M_Y,M_XWR,    MT_W, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,N,N,N,N),
      SD->       List(xpr64, BR_N,  REN_Y,REN_Y,A2_SPLIT,A1_RS1,DW_XPR,FN_ADD, M_Y,M_XWR,    MT_D, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,N,N,N,N),

      LUI->      List(Y,     BR_N,  REN_N,REN_Y,A2_0,    A1_LUI,DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,N,N,N,N),
      SLTI ->    List(Y,     BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_SLT, M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,N,N,N,N),
      SLTIU->    List(Y,     BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_SLTU,M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,N,N,N,N),
      ANDI->     List(Y,     BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_AND, M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,N,N,N,N),
      ORI->      List(Y,     BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_OR,  M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,N,N,N,N),
      XORI->     List(Y,     BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_XOR, M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,N,N,N,N),
      SLLI->     List(Y,     BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_SL,  M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,N,N,N,N),
      SRLI->     List(Y_SH,  BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_SR,  M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,N,N,N,N),
      SRAI->     List(Y_SH,  BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_SRA, M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,N,N,N,N),
      ADD->      List(Y,     BR_N,  REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,N,N,N,N),
      SUB->      List(Y,     BR_N,  REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_XPR,FN_SUB, M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,N,N,N,N),
      SLT->      List(Y,     BR_N,  REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_XPR,FN_SLT, M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,N,N,N,N),
      SLTU->     List(Y,     BR_N,  REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_XPR,FN_SLTU,M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,N,N,N,N),
      riscvAND-> List(Y,     BR_N,  REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_XPR,FN_AND, M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,N,N,N,N),
      riscvOR->  List(Y,     BR_N,  REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_XPR,FN_OR,  M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,N,N,N,N),
      riscvXOR-> List(Y,     BR_N,  REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_XPR,FN_XOR, M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,N,N,N,N),
      SLL->      List(Y,     BR_N,  REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_XPR,FN_SL,  M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,N,N,N,N),
      SRL->      List(Y,     BR_N,  REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_XPR,FN_SR,  M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,N,N,N,N),
      SRA->      List(Y,     BR_N,  REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_XPR,FN_SRA, M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,N,N,N,N),

      ADDIW->    List(xpr64, BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_32,FN_ADD,  M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,N,N,N,N),   
      SLLIW->    List(xpr64, BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_32,FN_SL,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,N,N,N,N),
      SRLIW->    List(xpr64, BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_32,FN_SR,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,N,N,N,N),
      SRAIW->    List(xpr64, BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_32,FN_SRA,  M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,N,N,N,N),
      ADDW->     List(xpr64, BR_N,  REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_32,FN_ADD,  M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,N,N,N,N),
      SUBW->     List(xpr64, BR_N,  REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_32,FN_SUB,  M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,N,N,N,N),
      SLLW->     List(xpr64, BR_N,  REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_32,FN_SL,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,N,N,N,N),
      SRLW->     List(xpr64, BR_N,  REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_32,FN_SR,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,N,N,N,N),
      SRAW->     List(xpr64, BR_N,  REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_32,FN_SRA,  M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,N,N,N,N),

      MUL->      List(Y,     BR_N,  REN_Y,REN_Y,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, Y,MUL_64,    N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,N,N,N,N),
      MULH->     List(Y,     BR_N,  REN_Y,REN_Y,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, Y,MUL_64H,   N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,N,N,N,N),
      MULHU->    List(Y,     BR_N,  REN_Y,REN_Y,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, Y,MUL_64HU,  N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,N,N,N,N),
      MULHSU->   List(Y,     BR_N,  REN_Y,REN_Y,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, Y,MUL_64HSU, N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,N,N,N,N),
      MULW->     List(xpr64, BR_N,  REN_Y,REN_Y,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, Y,MUL_32,    N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,N,N,N,N),

      DIV->      List(Y,     BR_N,  REN_Y,REN_Y,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     Y,DIV_64D,  WEN_N,WA_RD,WB_X,  REN_N,WEN_N,N,N,N,N),
      DIVU->     List(Y,     BR_N,  REN_Y,REN_Y,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     Y,DIV_64DU, WEN_N,WA_RD,WB_X,  REN_N,WEN_N,N,N,N,N),
      REM->      List(Y,     BR_N,  REN_Y,REN_Y,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     Y,DIV_64R,  WEN_N,WA_RD,WB_X,  REN_N,WEN_N,N,N,N,N),
      REMU->     List(Y,     BR_N,  REN_Y,REN_Y,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     Y,DIV_64RU, WEN_N,WA_RD,WB_X,  REN_N,WEN_N,N,N,N,N),
      DIVW->     List(xpr64, BR_N,  REN_Y,REN_Y,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     Y,DIV_32D,  WEN_N,WA_RD,WB_X,  REN_N,WEN_N,N,N,N,N),
      DIVUW->    List(xpr64, BR_N,  REN_Y,REN_Y,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     Y,DIV_32DU, WEN_N,WA_RD,WB_X,  REN_N,WEN_N,N,N,N,N),
      REMW->     List(xpr64, BR_N,  REN_Y,REN_Y,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     Y,DIV_32R,  WEN_N,WA_RD,WB_X,  REN_N,WEN_N,N,N,N,N),
      REMUW->    List(xpr64, BR_N,  REN_Y,REN_Y,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     Y,DIV_32RU, WEN_N,WA_RD,WB_X,  REN_N,WEN_N,N,N,N,N),

      SYSCALL->  List(Y,     BR_N,  REN_N,REN_N,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,N,N,Y,N),
      EI->       List(Y,     BR_N,  REN_N,REN_N,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,N,N,N,Y),
      DI->       List(Y,     BR_N,  REN_N,REN_N,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,N,N,N,Y),
      ERET->     List(Y,     BR_N,  REN_N,REN_N,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,N,Y,N,Y),
      FENCE->    List(Y,     BR_N,  REN_N,REN_N,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,Y,N,N,N),
      CFLUSH->   List(Y,     BR_N,  REN_Y,REN_N,A2_X,    A1_X,  DW_X,  FN_X,   M_Y,M_FLA,    MT_X, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,N,N,N,Y),
      MFPCR->    List(Y,     BR_N,  REN_N,REN_N,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_PCR,REN_Y,WEN_N,N,N,N,Y),
      MTPCR->    List(Y,     BR_N,  REN_N,REN_Y,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_Y,N,N,N,Y)
      
      // Instructions that have not yet been implemented
/*  
      // floating point
      FLW->      List(FPU_Y, BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_ADD, M_Y,M_FRD,    MT_WU,N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,N,N,N,N),
      FLD->      List(FPU_Y, BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_ADD, M_Y,M_FRD,    MT_D, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,N,N,N,N),
      FSW->      List(FPU_Y, BR_N,  REN_Y,REN_Y,A2_SPLIT,A1_RS1,DW_XPR,FN_ADD, M_Y,M_FWR,    MT_W, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,N,N,N,N),
      FSD->      List(FPU_Y, BR_N,  REN_Y,REN_Y,A2_SPLIT,A1_RS1,DW_XPR,FN_ADD, M_Y,M_FWR,    MT_D, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,N,N,N,N),
      
      // atomic memory operations
      AMOADD_W-> List(Y,     BR_N,  REN_Y,REN_Y,A2_0,    A1_RS1,DW_XPR,FN_ADD, M_Y,M_XA_ADD, MT_W, N,MUL_X,     N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,N,N,N,N),
      AMOSWAP_W->List(Y,     BR_N,  REN_Y,REN_Y,A2_0,    A1_RS1,DW_XPR,FN_ADD, M_Y,M_XA_SWAP,MT_W, N,MUL_X,     N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,N,N,N,N),
      AMOAND_W-> List(Y,     BR_N,  REN_Y,REN_Y,A2_0,    A1_RS1,DW_XPR,FN_ADD, M_Y,M_XA_AND, MT_W, N,MUL_X,     N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,N,N,N,N),
      AMOOR_W->  List(Y,     BR_N,  REN_Y,REN_Y,A2_0,    A1_RS1,DW_XPR,FN_ADD, M_Y,M_XA_OR,  MT_W, N,MUL_X,     N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,N,N,N,N),
      AMOMIN_W-> List(Y,     BR_N,  REN_Y,REN_Y,A2_0,    A1_RS1,DW_XPR,FN_ADD, M_Y,M_XA_MIN, MT_W, N,MUL_X,     N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,N,N,N,N),
      AMOMAX_W-> List(Y,     BR_N,  REN_Y,REN_Y,A2_0,    A1_RS1,DW_XPR,FN_ADD, M_Y,M_XA_MAX, MT_W, N,MUL_X,     N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,N,N,N,N),
      AMOMINU_W->List(Y,     BR_N,  REN_Y,REN_Y,A2_0,    A1_RS1,DW_XPR,FN_ADD, M_Y,M_XA_MINU,MT_W, N,MUL_X,     N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,N,N,N,N),
      AMOMAXU_W->List(Y,     BR_N,  REN_Y,REN_Y,A2_0,    A1_RS1,DW_XPR,FN_ADD, M_Y,M_XA_MAXU,MT_W, N,MUL_X,     N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,N,N,N,N),
      AMOADD_D-> List(xpr64, BR_N,  REN_Y,REN_Y,A2_0,    A1_RS1,DW_XPR,FN_ADD, M_Y,M_XA_ADD, MT_D, N,MUL_X,     N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,N,N,N,N),
      AMOSWAP_D->List(xpr64, BR_N,  REN_Y,REN_Y,A2_0,    A1_RS1,DW_XPR,FN_ADD, M_Y,M_XA_SWAP,MT_D, N,MUL_X,     N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,N,N,N,N),
      AMOAND_D-> List(xpr64, BR_N,  REN_Y,REN_Y,A2_0,    A1_RS1,DW_XPR,FN_ADD, M_Y,M_XA_AND, MT_D, N,MUL_X,     N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,N,N,N,N),
      AMOOR_D->  List(xpr64, BR_N,  REN_Y,REN_Y,A2_0,    A1_RS1,DW_XPR,FN_ADD, M_Y,M_XA_OR,  MT_D, N,MUL_X,     N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,N,N,N,N),
      AMOMIN_D-> List(xpr64, BR_N,  REN_Y,REN_Y,A2_0,    A1_RS1,DW_XPR,FN_ADD, M_Y,M_XA_MIN, MT_D, N,MUL_X,     N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,N,N,N,N),
      AMOMAX_D-> List(xpr64, BR_N,  REN_Y,REN_Y,A2_0,    A1_RS1,DW_XPR,FN_ADD, M_Y,M_XA_MAX, MT_D, N,MUL_X,     N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,N,N,N,N),
      AMOMINU_D->List(xpr64, BR_N,  REN_Y,REN_Y,A2_0,    A1_RS1,DW_XPR,FN_ADD, M_Y,M_XA_MINU,MT_D, N,MUL_X,     N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,N,N,N,N),
      AMOMAXU_D->List(xpr64, BR_N,  REN_Y,REN_Y,A2_0,    A1_RS1,DW_XPR,FN_ADD, M_Y,M_XA_MAXU,MT_D, N,MUL_X,     N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,N,N,N,N),
*/
     ));

  val id_int_val :: id_br_type :: id_renx2 :: id_renx1 :: id_sel_alu2 :: id_sel_alu1 :: id_fn_dw :: id_fn_alu :: csremainder = cs; 
  val id_mem_val :: id_mem_cmd :: id_mem_type :: id_mul_val :: id_mul_fn :: id_div_val :: id_div_fn :: id_wen :: id_sel_wa :: id_sel_wb :: id_ren_pcr :: id_wen_pcr :: id_sync :: id_eret :: id_syscall :: id_privileged :: Nil = csremainder;

  val id_raddr2 = io.dpath.inst(21,17);
  val id_raddr1 = io.dpath.inst(26,22);
  val id_waddr  = io.dpath.inst(31,27);

  val id_ren2 = id_renx2;
  val id_ren1 = id_renx1;

  val id_console_out_val  = id_wen_pcr & (id_raddr2 === PCR_CONSOLE);
  val console_out_fire    = id_console_out_val & ~io.dpath.killd;
  io.console.valid        := console_out_fire.toBool;

  val wb_reg_div_mul_val = Reg(){Bool()};
  val dcache_miss =   Reg(io.dmem.resp_miss);

  val sboard = new rocketCtrlSboard(); 
  sboard.io.raddra  := id_raddr2.toUFix;
  sboard.io.raddrb  := id_raddr1.toUFix;
  sboard.io.raddrc  := id_waddr.toUFix;

  // scoreboard set (for D$ misses, div, mul)
  sboard.io.set     := wb_reg_div_mul_val | dcache_miss;
  sboard.io.seta    := io.dpath.wb_waddr;

  sboard.io.clr0    := io.dpath.sboard_clr0;
  sboard.io.clr0a   := io.dpath.sboard_clr0a;
  sboard.io.clr1    := io.dpath.sboard_clr1;
  sboard.io.clr1a   := io.dpath.sboard_clr1a;

  val id_stall_raddr2 = sboard.io.stalla;
  val id_stall_raddr1 = sboard.io.stallb;
  val id_stall_waddr  = sboard.io.stallc;
  val id_stall_ra     = sboard.io.stallra;

  val id_reg_btb_hit     = Reg(resetVal = Bool(false));
  val id_reg_xcpt_itlb   = Reg(resetVal = Bool(false));
  
  val ex_reg_br_type     = Reg(){UFix(width = 4)};
  val ex_reg_btb_hit     = Reg(){Bool()};
  val ex_reg_div_mul_val = Reg(){Bool()};
  val ex_reg_mem_val     = Reg(){Bool()};
  val ex_reg_mem_cmd     = Reg(){UFix(width = 4)};
  val ex_reg_mem_type    = Reg(){UFix(width = 3)};
  val ex_reg_eret        = Reg(resetVal = Bool(false));
  val ex_reg_privileged  = Reg(resetVal = Bool(false));
  
  val ex_reg_xcpt_itlb       = Reg(resetVal = Bool(false));
  val ex_reg_xcpt_illegal    = Reg(resetVal = Bool(false));
  val ex_reg_xcpt_privileged = Reg(resetVal = Bool(false));
//   val ex_reg_xcpt_fpu        = Reg(resetVal = Bool(false));
  val ex_reg_xcpt_syscall    = Reg(resetVal = Bool(false));
  
  val mem_reg_xcpt_itlb       = Reg(resetVal = Bool(false));
  val mem_reg_xcpt_illegal    = Reg(resetVal = Bool(false));
  val mem_reg_xcpt_privileged = Reg(resetVal = Bool(false));
//   val mem_reg_xcpt_fpu        = Reg(resetVal = Bool(false));
  val mem_reg_xcpt_fpu        = Bool(false); // FIXME: trap on unimplemented FPU instructions
  val mem_reg_xcpt_syscall    = Reg(resetVal = Bool(false));

  when (!io.dpath.stalld) {
    when (io.dpath.killf) {
      id_reg_xcpt_itlb <== Bool(false);
      id_reg_btb_hit <== Bool(false);
    } 
    otherwise{
      id_reg_xcpt_itlb <== io.xcpt_itlb;
      id_reg_btb_hit <== io.dpath.btb_hit;
    }
  }
  
  when (reset.toBool || io.dpath.killd) {
    ex_reg_br_type     <== BR_N;
    ex_reg_btb_hit     <== Bool(false);
    ex_reg_div_mul_val <== Bool(false);
    ex_reg_mem_val     <== Bool(false);
    ex_reg_mem_cmd     <== UFix(0, 4);
    ex_reg_mem_type    <== UFix(0, 3);
    ex_reg_eret        <== Bool(false);
    ex_reg_privileged  <== Bool(false);
    
    ex_reg_xcpt_itlb        <== Bool(false);
    ex_reg_xcpt_illegal     <== Bool(false);
    ex_reg_xcpt_privileged  <== Bool(false);
//     ex_reg_xcpt_fpu         <== Bool(false);
    ex_reg_xcpt_syscall     <== Bool(false);
  } 
  otherwise {
    ex_reg_br_type     <== id_br_type;
    ex_reg_btb_hit     <== id_reg_btb_hit;
    ex_reg_div_mul_val <== id_div_val.toBool || id_mul_val.toBool;
    ex_reg_mem_val     <== id_mem_val.toBool;
    ex_reg_mem_cmd     <== id_mem_cmd;
    ex_reg_mem_type    <== id_mem_type;
    ex_reg_eret        <== id_eret.toBool;
    ex_reg_privileged  <== id_privileged.toBool;
    
    ex_reg_xcpt_itlb        <== id_reg_xcpt_itlb;
    ex_reg_xcpt_illegal     <== ~id_int_val.toBool;
    ex_reg_xcpt_privileged  <== (id_privileged & ~io.dpath.status(5)).toBool;
//     ex_reg_xcpt_fpu         <== Bool(false);
    ex_reg_xcpt_syscall     <== id_syscall.toBool;
  }

  val beq  =  io.dpath.br_eq;
  val bne  = ~io.dpath.br_eq;
  val blt  =  io.dpath.br_lt;
  val bltu =  io.dpath.br_ltu;
  val bge  = ~io.dpath.br_lt;
  val bgeu = ~io.dpath.br_ltu;

  val br_taken =
    (ex_reg_br_type === BR_EQ) & beq |
    (ex_reg_br_type === BR_NE) & bne |
    (ex_reg_br_type === BR_LT) & blt |
    (ex_reg_br_type === BR_LTU) & bltu |
    (ex_reg_br_type === BR_GE) & bge |
    (ex_reg_br_type === BR_GEU) & bgeu;

  val jr_taken = (ex_reg_br_type === BR_JR);
  val j_taken  = (ex_reg_br_type === BR_J);

  io.imem.req_val  := io.host.start; // FIXME
//  io.imem.req_val := Bool(true);

  io.dmem.req_val     := ex_reg_mem_val  && ~io.dpath.killx;
  io.dmem.req_cmd     := ex_reg_mem_cmd;
  io.dmem.req_type    := ex_reg_mem_type;
  
  val mem_reg_div_mul_val = Reg(){Bool()};
  val mem_reg_eret        = Reg(){Bool()};
  val mem_reg_mem_val     = Reg(){Bool()};
  val mem_reg_mem_cmd     = Reg(){UFix(width = 4)};
  val mem_reg_mem_type    = Reg(){UFix(width = 3)};
  val mem_reg_privileged  = Reg(){Bool()};

  when (reset.toBool || io.dpath.killx) {
    mem_reg_div_mul_val <== Bool(false);
    mem_reg_eret        <== Bool(false);
    mem_reg_mem_val     <== Bool(false);
    mem_reg_mem_cmd     <== UFix(0, 4);
    mem_reg_mem_type    <== UFix(0, 3);
    mem_reg_privileged  <== Bool(false);
    
    mem_reg_xcpt_itlb        <== Bool(false);
    mem_reg_xcpt_illegal     <== Bool(false);
    mem_reg_xcpt_privileged  <== Bool(false);
//     mem_reg_xcpt_fpu         <== Bool(false);
    mem_reg_xcpt_syscall     <== Bool(false);
  }
  otherwise {
    mem_reg_div_mul_val <== ex_reg_div_mul_val;
    mem_reg_eret        <== ex_reg_eret;
    mem_reg_mem_val     <== ex_reg_mem_val;
    mem_reg_mem_cmd     <== ex_reg_mem_cmd;
    mem_reg_mem_type    <== ex_reg_mem_type;
    mem_reg_privileged  <== ex_reg_privileged;
    
    mem_reg_xcpt_itlb        <== ex_reg_xcpt_itlb;
    mem_reg_xcpt_illegal     <== mem_reg_xcpt_illegal;
    mem_reg_xcpt_privileged  <== ex_reg_xcpt_privileged;
//     mem_reg_xcpt_fpu         <== Bool(false);
    mem_reg_xcpt_syscall     <== ex_reg_xcpt_syscall;
  }
    
  when (reset.toBool || io.dpath.killm) {
    wb_reg_div_mul_val <== Bool(false);
  }
  otherwise {
    wb_reg_div_mul_val <== mem_reg_div_mul_val;
  }

  // exception handling
	val mem_exception = 
	  io.xcpt_dtlb_ld ||
	  io.xcpt_dtlb_st ||
	  mem_reg_xcpt_illegal || 
	  mem_reg_xcpt_privileged || 
	  mem_reg_xcpt_fpu || 
	  mem_reg_xcpt_syscall || 
	  mem_reg_xcpt_itlb;
	
	val mem_cause = 
	  // instruction address misaligned
	  Mux(mem_reg_xcpt_itlb,        UFix(1,5), // instruction access fault
		Mux(mem_reg_xcpt_illegal,     UFix(2,5), // illegal instruction
		Mux(mem_reg_xcpt_privileged,  UFix(3,5), // privileged instruction
		Mux(mem_reg_xcpt_fpu,         UFix(4,5), // FPU disabled
		// interrupt
		Mux(mem_reg_xcpt_syscall,     UFix(6,5), // system call
		// breakpoint
		// misaligned load
		// misaligned store
		Mux(io.xcpt_dtlb_ld,          UFix(8,5), // load fault
		Mux(io.xcpt_dtlb_st,          UFix(9,5), // store fault
			UFix(0,5))))))));
			
	// write cause to PCR on an exception
	io.dpath.exception := mem_exception;
	io.dpath.cause     := mem_cause;
	io.dpath.badvaddr_wen := io.xcpt_dtlb_ld || io.xcpt_dtlb_st || mem_reg_xcpt_itlb;
	io.dpath.badvaddr_sel := mem_reg_xcpt_itlb;
	
  // replay execute stage PC when the D$ is blocked, when the D$ misses, and for privileged instructions
  val replay_ex = (ex_reg_mem_val && !io.dmem.req_rdy) || io.dmem.resp_miss || mem_reg_privileged;
  
  // replay mem stage PC on a DTLB miss
  val replay_mem = io.dtlb_miss;
  val kill_ex    = replay_ex || replay_mem;
  val kill_mem   = mem_exception || io.dtlb_miss;

  io.dpath.sel_pc :=
    Mux(mem_exception,                PC_EVEC, // exception
    Mux(replay_mem,                   PC_MEM,  // dtlb miss
    Mux(mem_reg_eret,                 PC_PCR,  // eret instruction
    Mux(replay_ex,                    PC_EX,   // D$ blocked, D$ miss, privileged inst
    Mux(!ex_reg_btb_hit && br_taken,  PC_BR,   // mispredicted taken branch
    Mux(ex_reg_btb_hit && !br_taken,  PC_EX4,  // mispredicted not taken branch
    Mux(jr_taken,                     PC_JR,   // jump register
    Mux(j_taken,                      PC_J,    // jump
    Mux(io.dpath.btb_hit,             PC_BTB,  // predicted PC from BTB
        PC_4))))))))); // PC+4

  io.dpath.wen_btb := ~ex_reg_btb_hit & br_taken & ~kill_ex & ~kill_mem;

  val take_pc =
    ~ex_reg_btb_hit & br_taken |
    ex_reg_btb_hit & ~br_taken |
    jr_taken |
    j_taken |
    mem_exception |
    mem_reg_eret |
    replay_ex |
    replay_mem;

  io.dpath.stallf :=
    ~take_pc &
    (
      ~io.imem.req_rdy |
      ~io.imem.resp_val |
      io.dpath.stalld
    );

  // check for loads in execute and mem stages to detect load/use hazards
  val ex_mem_cmd_load = ex_reg_mem_val && (ex_reg_mem_cmd  === M_XRD);
  
  val lu_stall_ex = 
    ex_mem_cmd_load &&
    ((id_ren1.toBool && (id_raddr1 === io.dpath.ex_waddr)) ||
     (id_ren2.toBool && (id_raddr2 === io.dpath.ex_waddr)));
    
  val mem_mem_cmd_load_bh = 
    mem_reg_mem_val &&
    (mem_reg_mem_cmd === M_XRD)   &&
    ((mem_reg_mem_type === MT_B)  ||
     (mem_reg_mem_type === MT_BU) ||
     (mem_reg_mem_type === MT_H)  || 
     (mem_reg_mem_type === MT_HU));
     
  val lu_stall_mem = 
    mem_mem_cmd_load_bh &&
    ((id_ren1.toBool && (id_raddr1 === io.dpath.mem_waddr)) ||
     (id_ren2.toBool && (id_raddr2 === io.dpath.mem_waddr)));

  val lu_stall = lu_stall_ex || lu_stall_mem;
  
  // check for divide and multiply instructions in ex,mem,wb stages
  val dm_stall_ex = 
    ex_reg_div_mul_val &&
    ((id_ren1.toBool && (id_raddr1 === io.dpath.ex_waddr)) ||
     (id_ren2.toBool && (id_raddr2 === io.dpath.ex_waddr)));

  val dm_stall_mem = 
    mem_reg_div_mul_val &&
    ((id_ren1.toBool && (id_raddr1 === io.dpath.mem_waddr)) ||
     (id_ren2.toBool && (id_raddr2 === io.dpath.mem_waddr)));
     
  val dm_stall_wb = 
    wb_reg_div_mul_val &&
    ((id_ren1.toBool && (id_raddr1 === io.dpath.wb_waddr)) ||
     (id_ren2.toBool && (id_raddr2 === io.dpath.wb_waddr)));
     
  val dm_stall = dm_stall_ex || dm_stall_mem || dm_stall_wb;

  val ctrl_stalld =
    ~take_pc &
    (
      dm_stall |
      lu_stall | 
      id_ren2 &  id_stall_raddr2 |
      id_ren1 &  id_stall_raddr1 |
      (id_sel_wa === WA_RD) & id_stall_waddr |
      (id_sel_wa === WA_RA) & id_stall_ra |
      id_mem_val & ~io.dmem.req_rdy |
      id_sync & ~io.dmem.req_rdy |
      id_console_out_val & ~io.console.rdy |
      id_div_val & ~io.dpath.div_rdy |
      io.dpath.div_result_val |
      io.dpath.mul_result_val
    );
    
  val ctrl_killd = take_pc | ctrl_stalld;
      
  // for divider, multiplier writeback
  val mul_wb = io.dpath.mul_result_val;
  val div_wb = io.dpath.div_result_val & !mul_wb;

  io.dpath.stalld   := ctrl_stalld.toBool;

  io.dpath.killf    := take_pc | ~io.imem.resp_val;
  io.dpath.killd    := ctrl_killd.toBool;
  io.dpath.killx    := kill_ex.toBool || kill_mem.toBool;
  io.dpath.killm    := kill_mem.toBool;

  io.dpath.mem_load := mem_reg_mem_val && (mem_reg_mem_cmd === M_XRD);
  io.dpath.ren2     := id_ren2.toBool;
  io.dpath.ren1     := id_ren1.toBool;
  io.dpath.sel_alu2 := id_sel_alu2;
  io.dpath.sel_alu1 := id_sel_alu1.toBool;
  io.dpath.fn_dw    := id_fn_dw.toBool;
  io.dpath.fn_alu   := id_fn_alu;
  io.dpath.div_fn   := id_div_fn;
  io.dpath.div_val  := id_div_val.toBool;
  io.dpath.div_wb   := div_wb;
  io.dpath.mul_fn   := id_mul_fn;
  io.dpath.mul_val  := id_mul_val.toBool;
  io.dpath.mul_wb   := mul_wb;
  io.dpath.wen      := id_wen.toBool;
  io.dpath.sel_wa   := id_sel_wa.toBool;
  io.dpath.sel_wb   := id_sel_wb;
  io.dpath.ren_pcr  := id_ren_pcr.toBool;
  io.dpath.wen_pcr  := id_wen_pcr.toBool;
  io.dpath.eret     := id_eret.toBool;
}

}
