package Top {

import Chisel._
import Node._;

import Constants._
import Instructions._

class ioCtrl extends Bundle()
{
  val sel_pc   = UFix(3, 'output);
  val wen_btb  = Bool('output);
  val stallf   = Bool('output);
  val stalld   = Bool('output);
  val killf    = Bool('output);
  val killd    = Bool('output);
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
  val wen      = Bool('output);
  val sel_wa   = Bool('output);
  val sel_wb   = UFix(3, 'output);
  val ren_pcr  = Bool('output);
  val wen_pcr  = Bool('output);
  val xcpt_illegal = Bool('output);
  val xcpt_privileged = Bool('output);
  val xcpt_fpu = Bool('output);
  val xcpt_syscall = Bool('output);
  val eret    = Bool('output);
}

class ioCtrlDpath extends Bundle()
{
  val btb_hit = Bool('input);
  val inst    = UFix(32, 'input);
  val br_eq   = Bool('input);
  val br_lt   = Bool('input);
  val br_ltu  = Bool('input);
  val div_rdy = Bool('input);
  val div_result_val = Bool('input);
  val mul_result_val = Bool('input);
  val wen     = Bool('input);
  val waddr   = UFix(5, 'input);
  val exception = Bool('input);
  val status  = Bits(8, 'input);
}

class ioCtrlMem extends Bundle()
{
  val mrq_val  = Bool('output);
  val mrq_cmd  = UFix(4, 'output);
  val mrq_type = UFix(3, 'output);
  val mrq_deq  = Bool('input);
  val xsdq_rdy = Bool('input);
  val xsdq_val = Bool('output);
  val dc_busy  = Bool('input);
}

class ioCtrlImem extends Bundle()
{
  val req_val  = Bool('output);
  val req_rdy  = Bool('input);
  val resp_val = Bool('input);
}

class ioCtrlWB extends Bundle()
{
  val waddr     = UFix(5, 'input);
  val wen       = Bool('input);
}

class ioCtrlAll extends Bundle()
{
  val ctrl    = new ioCtrl();
  val console = new ioConsole(List("rdy", "valid"));
  val dpath   = new ioCtrlDpath();
  val imem    = new ioCtrlImem();
  val mem     = new ioCtrlMem();
  val wb      = new ioCtrlWB();
  val host    = new ioHost(List("start"));
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
      
      // miscellaneous
      RDNPC->    List(Y,     BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_PC, REN_N,WEN_N,N,N,N,N),
*/
     ));

  val id_int_val :: id_br_type :: id_renx2 :: id_renx1 :: id_sel_alu2 :: id_sel_alu1 :: id_fn_dw :: id_fn_alu :: csremainder = cs; 
  val id_mem_val :: id_mem_cmd :: id_mem_type :: id_mul_val :: id_mul_fn :: id_div_val :: id_div_fn :: id_wen :: id_sel_wa :: id_sel_wb :: id_ren_pcr :: id_wen_pcr :: id_sync :: id_eret :: id_syscall :: id_privileged :: Nil = csremainder;

  val id_raddr2 = io.dpath.inst(21,17);
  val id_raddr1 = io.dpath.inst(26,22);
  val id_waddr  = io.dpath.inst(31,27);

  val id_ren2 = id_renx2;
  val id_ren1 = id_renx1;

  val id_console_out_val = id_wen_pcr & (id_raddr2 === PCR_CONSOLE);

  val id_mem_val_masked = id_mem_val;

  val mem_xload_val   = id_mem_val_masked & (id_mem_cmd === M_XRD);
  val mem_xstore_val  = id_mem_val_masked & (id_mem_cmd === M_XWR);

  val mem_fire        = id_mem_val_masked & ~io.ctrl.killd;
  val mem_xload_fire  = mem_xload_val & ~io.ctrl.killd;
  val mem_xstore_fire = mem_xstore_val & ~io.ctrl.killd;

  val console_out_fire = id_console_out_val & ~io.ctrl.killd;

  val div_fire = id_div_val & ~io.ctrl.killd;
  val mul_fire = id_mul_val & ~io.ctrl.killd;

  val sboard_wen = mem_xload_fire | div_fire | mul_fire;
  val sboard_waddr = id_waddr;
  
  val sboard = new rocketCtrlSboard(); 
  sboard.io.raddra  := id_raddr2;
  sboard.io.raddrb  := id_raddr1;
  sboard.io.raddrc  := id_waddr;
  sboard.io.set   := sboard_wen.toBool;
  sboard.io.seta  := sboard_waddr;
  sboard.io.clr0  := io.wb.wen.toBool;
  sboard.io.clr0a ^^ io.wb.waddr;
  sboard.io.clr1  := io.dpath.wen.toBool;
  sboard.io.clr1a := io.dpath.waddr;

  val id_stall_raddr2 = sboard.io.stalla;
  val id_stall_raddr1 = sboard.io.stallb;
  val id_stall_waddr  = sboard.io.stallc;
  val id_stall_ra     = sboard.io.stallra;

  val mrq = new rocketCtrlCnt(3, 4);   
  mrq.io.enq   := mem_fire.toBool;
  mrq.io.deq   ^^ io.mem.mrq_deq;
  val id_empty_mrq = mrq.io.empty;
  val id_full_mrq  = mrq.io.full; 

  val id_reg_btb_hit    = Reg(width = 1, resetVal = Bool(false));
  val ex_reg_br_type    = Reg(){UFix(width = 4)};
  val ex_reg_btb_hit    = Reg(){Bool()};
  val ex_reg_mem_val    = Reg(){Bool()};
  val ex_reg_mem_cmd    = Reg(){UFix(width = 4)};
  val ex_reg_mem_type   = Reg(){UFix(width = 3)};
  val ex_reg_eret       = Reg(resetVal = Bool(false));
  val ex_reg_privileged = Reg(resetVal = Bool(false));

  when (!io.ctrl.stalld) {
    when (io.ctrl.killf) {
      id_reg_btb_hit <== Bool(false);
    } 
    otherwise{
      id_reg_btb_hit <== io.dpath.btb_hit;
    }
  }
  when (reset.toBool || io.ctrl.killd) {
    ex_reg_br_type    <== BR_N;
    ex_reg_btb_hit    <== Bool(false);
    ex_reg_mem_val    <== Bool(false);
    ex_reg_mem_cmd    <== UFix(0, 4);
    ex_reg_mem_type   <== UFix(0, 3);
    ex_reg_eret       <== Bool(false);
    ex_reg_privileged <== Bool(false);
  } 
  otherwise {
    ex_reg_br_type    <== id_br_type;
    ex_reg_btb_hit    <== id_reg_btb_hit;
    ex_reg_mem_val    <== id_mem_val_masked.toBool;
    ex_reg_mem_cmd    <== id_mem_cmd;
    ex_reg_mem_type   <== id_mem_type;
    ex_reg_eret       <== id_eret.toBool;
    ex_reg_privileged <== id_privileged.toBool;
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

  io.imem.req_val  := io.host.start;
//  io.imem.req_val := Bool(true);

  io.mem.mrq_val   := ex_reg_mem_val;
  io.mem.mrq_cmd   := ex_reg_mem_cmd;
  io.mem.mrq_type  := ex_reg_mem_type;
  io.mem.xsdq_val  := mem_xstore_fire.toBool;
  io.console.valid := console_out_fire.toBool;

  io.ctrl.sel_pc :=
    Mux(io.dpath.exception || ex_reg_eret, PC_PCR,
    Mux(!ex_reg_btb_hit && br_taken, PC_BR,
    Mux(ex_reg_btb_hit && !br_taken || ex_reg_privileged, PC_EX4,
    Mux(jr_taken,                    PC_JR,
    Mux(j_taken,                     PC_J,
    Mux(io.dpath.btb_hit,            PC_BTB,
        PC_4))))));

  io.ctrl.wen_btb := ~ex_reg_btb_hit & br_taken;

  val take_pc =
    ~ex_reg_btb_hit & br_taken |
    ex_reg_btb_hit & ~br_taken |
    jr_taken |
    j_taken |
    io.dpath.exception |
    ex_reg_privileged |
    ex_reg_eret; 

  io.ctrl.stallf :=
    ~take_pc &
    (
      ~io.imem.req_rdy |
      ~io.imem.resp_val |
      io.ctrl.stalld
    );

  val ctrl_stalld_wo_fpu_rdy =
    ~take_pc &
    (
      id_ren2 & id_stall_raddr2 |
      id_ren1 & id_stall_raddr1 |
      (id_sel_wa === WA_RD) & id_stall_waddr |
      (id_sel_wa === WA_RA) & id_stall_ra |
      id_mem_val_masked & id_full_mrq |
      id_sync & (~id_empty_mrq | io.mem.dc_busy) |
      mem_xstore_val & ~io.mem.xsdq_rdy |
      id_console_out_val & ~io.console.rdy |
      id_div_val & ~io.dpath.div_rdy |
      io.dpath.div_result_val |
      io.dpath.mul_result_val
    );
    
  // for divider, multiplier writeback
  val mul_wb = io.dpath.mul_result_val;
  val div_wb = io.dpath.div_result_val & !mul_wb;

  io.ctrl.stalld := ctrl_stalld_wo_fpu_rdy.toBool;

  io.ctrl.killf := take_pc | ~io.imem.resp_val;
  val ctrl_killd_wo_fpu_rdy = take_pc | ctrl_stalld_wo_fpu_rdy;
  io.ctrl.killd    := ctrl_killd_wo_fpu_rdy.toBool;

  io.ctrl.ren2     := id_ren2.toBool;
  io.ctrl.ren1     := id_ren1.toBool;
  io.ctrl.sel_alu2 := id_sel_alu2;
  io.ctrl.sel_alu1 := id_sel_alu1.toBool;
  io.ctrl.fn_dw    := id_fn_dw.toBool;
  io.ctrl.fn_alu   := id_fn_alu;
  io.ctrl.div_fn   := id_div_fn;
  io.ctrl.div_val  := id_div_val.toBool;
  io.ctrl.div_wb   := div_wb;
  io.ctrl.mul_fn   := id_mul_fn;
  io.ctrl.mul_val  := id_mul_val.toBool;
  io.ctrl.mul_wb   := mul_wb;
  io.ctrl.wen      := id_wen.toBool;
  io.ctrl.sel_wa   := id_sel_wa.toBool;
  io.ctrl.sel_wb   := id_sel_wb;
  io.ctrl.ren_pcr  := id_ren_pcr.toBool;
  io.ctrl.wen_pcr  := id_wen_pcr.toBool;
  io.ctrl.eret     := id_eret.toBool;
  
  io.ctrl.xcpt_illegal    := ~id_int_val.toBool;
  io.ctrl.xcpt_privileged := (id_privileged & ~io.dpath.status(5)).toBool;
  io.ctrl.xcpt_fpu        := Bool(false); 
  io.ctrl.xcpt_syscall    := id_syscall.toBool;
}

}
