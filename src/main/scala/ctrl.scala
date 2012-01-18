package Top {

import Chisel._
import Node._;

import Constants._
import Instructions._

class ioCtrlDpath extends Bundle()
{
  // outputs to datapath
  val sel_pc   = UFix(4, OUTPUT);
  val wen_btb  = Bool(OUTPUT);
  val clr_btb  = Bool(OUTPUT);
  val stallf   = Bool(OUTPUT);
  val stalld   = Bool(OUTPUT);
  val killf    = Bool(OUTPUT);
  val killd    = Bool(OUTPUT);
  val killx    = Bool(OUTPUT);
  val killm    = Bool(OUTPUT);
  val ren2     = Bool(OUTPUT);
  val ren1     = Bool(OUTPUT);
  val sel_alu2 = UFix(2, OUTPUT);
  val sel_alu1 = Bool(OUTPUT);
  val fn_dw    = Bool(OUTPUT);
  val fn_alu   = UFix(4, OUTPUT);
  val mul_val  = Bool(OUTPUT);
  val mul_fn   = UFix(2, OUTPUT);
  val mul_wb   = Bool(OUTPUT);
  val div_val  = Bool(OUTPUT);
  val div_fn   = UFix(2, OUTPUT);
  val div_wb   = Bool(OUTPUT);
  val sel_wa   = Bool(OUTPUT);
  val sel_wb   = UFix(3, OUTPUT);
  val ren_pcr  = Bool(OUTPUT);
  val wen_pcr  = Bool(OUTPUT);
  val id_eret  = Bool(OUTPUT);
  val wb_eret  = Bool(OUTPUT);
  val mem_load = Bool(OUTPUT);
  val wen      = Bool(OUTPUT);
  // instruction in execute is an unconditional jump
  val ex_jmp   = Bool(OUTPUT); 
  val ex_jr    = Bool(OUTPUT);
  // enable/disable interrupts
  val irq_enable   = Bool(OUTPUT);
  val irq_disable   = Bool(OUTPUT);
  // exception handling
  val exception = Bool(OUTPUT);
  val cause    = UFix(5,OUTPUT);
  val badvaddr_wen = Bool(OUTPUT); // high for a load/store access fault
  // inputs from datapath
  val xcpt_ma_inst = Bool(INPUT);  // high on a misaligned/illegal virtual PC
  val btb_hit = Bool(INPUT);
  val btb_match = Bool(INPUT);
  val inst    = Bits(32, INPUT);
  val br_eq   = Bool(INPUT);
  val br_lt   = Bool(INPUT);
  val br_ltu  = Bool(INPUT);
  val div_rdy = Bool(INPUT);
  val div_result_val = Bool(INPUT);
  val mul_rdy = Bool(INPUT);
  val mul_result_val = Bool(INPUT);
  val mem_lu_bypass = Bool(INPUT);
  val ex_waddr = UFix(5,INPUT);  // write addr from execute stage
  val mem_waddr = UFix(5,INPUT); // write addr from memory stage
  val wb_waddr = UFix(5,INPUT);  // write addr from writeback stage
  val status  = Bits(17, INPUT);
  val sboard_clr  = Bool(INPUT);
  val sboard_clra = UFix(5, INPUT);
  val mem_valid = Bool(INPUT); // high if there's a valid (not flushed) instruction in mem stage
  val irq_timer = Bool(INPUT);
  val irq_ipi   = Bool(INPUT);
}

class ioCtrlAll extends Bundle()
{
  val dpath   = new ioCtrlDpath();
  val console = new ioConsole(List("rdy"));
  val imem    = new ioImem(List("req_val", "resp_val")).flip();
  val dmem    = new ioDmem(List("req_val", "req_kill", "req_rdy", "req_cmd", "req_type", "resp_miss", "resp_replay", "resp_nack")).flip();
  val dtlb_val = Bool(OUTPUT);
  val dtlb_kill = Bool(OUTPUT);
  val dtlb_rdy = Bool(INPUT);
  val dtlb_miss = Bool(INPUT);
  val flush_inst = Bool(OUTPUT);
  val xcpt_dtlb_ld = Bool(INPUT);
  val xcpt_dtlb_st = Bool(INPUT);
  val xcpt_itlb = Bool(INPUT);
  val xcpt_ma_ld = Bool(INPUT);
  val xcpt_ma_st = Bool(INPUT);
}

class rocketCtrl extends Component
{
  val io = new ioCtrlAll();
    
//   val fp =
//   ListLookup(io.dpath.inst,
//     List(Bool(false)),
//      Array(
//        FMOVZ -> List(Bool(true)),
//        FMOVN -> List(Bool(true)),
//        FADD_S -> List(Bool(true)),
//        FSUB_S -> List(Bool(true)),
//        FMUL_S -> List(Bool(true)),
//        FDIV_S -> List(Bool(true)),
//        FSQRT_S -> List(Bool(true)),
//        FSGNJ_S -> List(Bool(true)),
//        FSGNJN_S -> List(Bool(true)),
//        FSGNJX_S -> List(Bool(true)),
//        FADD_D -> List(Bool(true)),
//        FSUB_D -> List(Bool(true)),
//        FMUL_D -> List(Bool(true)),
//        FDIV_D -> List(Bool(true)),
//        FSQRT_D -> List(Bool(true)),
//        FSGNJ_D -> List(Bool(true)),
//        FSGNJN_D -> List(Bool(true)),
//        FSGNJX_D -> List(Bool(true)),
//        FCVT_L_S -> List(Bool(true)),
//        FCVT_LU_S -> List(Bool(true)),
//        FCVT_W_S -> List(Bool(true)),
//        FCVT_WU_S -> List(Bool(true)),
//        FCVT_L_D -> List(Bool(true)),
//        FCVT_LU_D -> List(Bool(true)),
//        FCVT_W_D -> List(Bool(true)),
//        FCVT_WU_D -> List(Bool(true)),
//        FCVT_S_L -> List(Bool(true)),
//        FCVT_S_LU -> List(Bool(true)),
//        FCVT_S_W -> List(Bool(true)),
//        FCVT_S_WU -> List(Bool(true)),
//        FCVT_D_L -> List(Bool(true)),
//        FCVT_D_LU -> List(Bool(true)),
//        FCVT_D_W -> List(Bool(true)),
//        FCVT_D_WU -> List(Bool(true)),
//        FCVT_S_D -> List(Bool(true)),
//        FCVT_D_S -> List(Bool(true)),
//        FEQ_S -> List(Bool(true)),
//        FLT_S -> List(Bool(true)),
//        FLE_S -> List(Bool(true)),
//        FEQ_D -> List(Bool(true)),
//        FLT_D -> List(Bool(true)),
//        FLE_D -> List(Bool(true)),
//        FMIN_S -> List(Bool(true)),
//        FMAX_S -> List(Bool(true)),
//        FMIN_D -> List(Bool(true)),
//        FMAX_D -> List(Bool(true)),
//        MFTX_S -> List(Bool(true)),
//        MFTX_D -> List(Bool(true)),
//        MFFSR -> List(Bool(true)),
//        MXTF_S -> List(Bool(true)),
//        MXTF_D -> List(Bool(true)),
//        MTFSR -> List(Bool(true)),
//        FLW -> List(Bool(true)),
//        FLD -> List(Bool(true)),
//        FSW -> List(Bool(true)),
//        FSD -> List(Bool(true)),
//        FMADD_S -> List(Bool(true)),
//        FMSUB_S -> List(Bool(true)),
//        FNMSUB_S -> List(Bool(true)),
//        FNMADD_S -> List(Bool(true)),
//        FMADD_D -> List(Bool(true)),
//        FMSUB_D -> List(Bool(true)),
//        FNMSUB_D -> List(Bool(true)),
//        FNMADD_D -> List(Bool(true))
//      ));
//   val id_fp_val :: Nil = fp;
  
  val xpr64 = Y;
  val cs =   
  ListLookup(io.dpath.inst,
     List(            N,     BR_N,  REN_N,REN_N,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
     Array(
      BNE->      List(Y,     BR_NE, REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      ADDI->     List(Y,     BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      BEQ->      List(Y,     BR_EQ, REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      BLT->      List(Y,     BR_LT, REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      BLTU->     List(Y,     BR_LTU,REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      BGE->      List(Y,     BR_GE, REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      BGEU->     List(Y,     BR_GEU,REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),

      J->        List(Y,     BR_J,  REN_N,REN_N,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      JAL->      List(Y,     BR_J,  REN_N,REN_N,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RA,WB_PC, REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      JALR_C->   List(Y,     BR_JR, REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_PC, REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      JALR_J->   List(Y,     BR_JR, REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_PC, REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      JALR_R->   List(Y,     BR_JR, REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_PC, REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      RDNPC->    List(Y,     BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_PC, REN_N,WEN_N,I_X ,SYNC_N,N,N,N),

      LB->       List(Y,     BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_ADD, M_Y,M_XRD,    MT_B, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      LH->       List(Y,     BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_ADD, M_Y,M_XRD,    MT_H, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      LW->       List(Y,     BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_ADD, M_Y,M_XRD,    MT_W, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      LD->       List(xpr64, BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_ADD, M_Y,M_XRD,    MT_D, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      LBU->      List(Y,     BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_ADD, M_Y,M_XRD,    MT_BU,N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      LHU->      List(Y,     BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_ADD, M_Y,M_XRD,    MT_HU,N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      LWU->      List(xpr64, BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_ADD, M_Y,M_XRD,    MT_WU,N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      SB->       List(Y,     BR_N,  REN_Y,REN_Y,A2_SPLIT,A1_RS1,DW_XPR,FN_ADD, M_Y,M_XWR,    MT_B, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      SH->       List(Y,     BR_N,  REN_Y,REN_Y,A2_SPLIT,A1_RS1,DW_XPR,FN_ADD, M_Y,M_XWR,    MT_H, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      SW->       List(Y,     BR_N,  REN_Y,REN_Y,A2_SPLIT,A1_RS1,DW_XPR,FN_ADD, M_Y,M_XWR,    MT_W, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      SD->       List(xpr64, BR_N,  REN_Y,REN_Y,A2_SPLIT,A1_RS1,DW_XPR,FN_ADD, M_Y,M_XWR,    MT_D, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),

      AMOADD_W-> List(Y,     BR_N,  REN_Y,REN_Y,A2_0,    A1_RS1,DW_XPR,FN_ADD, M_Y,M_XA_ADD, MT_W, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      AMOSWAP_W->List(Y,     BR_N,  REN_Y,REN_Y,A2_0,    A1_RS1,DW_XPR,FN_ADD, M_Y,M_XA_SWAP,MT_W, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      AMOAND_W-> List(Y,     BR_N,  REN_Y,REN_Y,A2_0,    A1_RS1,DW_XPR,FN_ADD, M_Y,M_XA_AND, MT_W, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      AMOOR_W->  List(Y,     BR_N,  REN_Y,REN_Y,A2_0,    A1_RS1,DW_XPR,FN_ADD, M_Y,M_XA_OR,  MT_W, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      AMOADD_D-> List(xpr64, BR_N,  REN_Y,REN_Y,A2_0,    A1_RS1,DW_XPR,FN_ADD, M_Y,M_XA_ADD, MT_D, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      AMOSWAP_D->List(xpr64, BR_N,  REN_Y,REN_Y,A2_0,    A1_RS1,DW_XPR,FN_ADD, M_Y,M_XA_SWAP,MT_D, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      AMOAND_D-> List(xpr64, BR_N,  REN_Y,REN_Y,A2_0,    A1_RS1,DW_XPR,FN_ADD, M_Y,M_XA_AND, MT_D, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      AMOOR_D->  List(xpr64, BR_N,  REN_Y,REN_Y,A2_0,    A1_RS1,DW_XPR,FN_ADD, M_Y,M_XA_OR,  MT_D, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),

      LUI->      List(Y,     BR_N,  REN_N,REN_Y,A2_0,    A1_LUI,DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      SLTI ->    List(Y,     BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_SLT, M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      SLTIU->    List(Y,     BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_SLTU,M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      ANDI->     List(Y,     BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_AND, M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      ORI->      List(Y,     BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_OR,  M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      XORI->     List(Y,     BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_XOR, M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      SLLI->     List(Y,     BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_SL,  M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      SRLI->     List(Y_SH,  BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_SR,  M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      SRAI->     List(Y_SH,  BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_SRA, M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      ADD->      List(Y,     BR_N,  REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      SUB->      List(Y,     BR_N,  REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_XPR,FN_SUB, M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      SLT->      List(Y,     BR_N,  REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_XPR,FN_SLT, M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      SLTU->     List(Y,     BR_N,  REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_XPR,FN_SLTU,M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      riscvAND-> List(Y,     BR_N,  REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_XPR,FN_AND, M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      riscvOR->  List(Y,     BR_N,  REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_XPR,FN_OR,  M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      riscvXOR-> List(Y,     BR_N,  REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_XPR,FN_XOR, M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      SLL->      List(Y,     BR_N,  REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_XPR,FN_SL,  M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      SRL->      List(Y,     BR_N,  REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_XPR,FN_SR,  M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      SRA->      List(Y,     BR_N,  REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_XPR,FN_SRA, M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),

      ADDIW->    List(xpr64, BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_32,FN_ADD,  M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),   
      SLLIW->    List(xpr64, BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_32,FN_SL,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      SRLIW->    List(xpr64, BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_32,FN_SR,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      SRAIW->    List(xpr64, BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_32,FN_SRA,  M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      ADDW->     List(xpr64, BR_N,  REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_32,FN_ADD,  M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      SUBW->     List(xpr64, BR_N,  REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_32,FN_SUB,  M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      SLLW->     List(xpr64, BR_N,  REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_32,FN_SL,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      SRLW->     List(xpr64, BR_N,  REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_32,FN_SR,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      SRAW->     List(xpr64, BR_N,  REN_Y,REN_Y,A2_RS2,  A1_RS1,DW_32,FN_SRA,  M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),

      MUL->      List(Y,     BR_N,  REN_Y,REN_Y,A2_X,    A1_X,  DW_XPR,FN_X,   M_N,M_X,      MT_X, Y,MUL_LO,    N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      MULH->     List(Y,     BR_N,  REN_Y,REN_Y,A2_X,    A1_X,  DW_XPR,FN_X,   M_N,M_X,      MT_X, Y,MUL_HS,    N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      MULHU->    List(Y,     BR_N,  REN_Y,REN_Y,A2_X,    A1_X,  DW_XPR,FN_X,   M_N,M_X,      MT_X, Y,MUL_HU,    N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      MULHSU->   List(Y,     BR_N,  REN_Y,REN_Y,A2_X,    A1_X,  DW_XPR,FN_X,   M_N,M_X,      MT_X, Y,MUL_HSU,   N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      MULW->     List(xpr64, BR_N,  REN_Y,REN_Y,A2_X,    A1_X,  DW_32, FN_X,   M_N,M_X,      MT_X, Y,MUL_LO,    N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),

      DIV->      List(Y,     BR_N,  REN_Y,REN_Y,A2_X,    A1_X,  DW_XPR,FN_X,   M_N,M_X,      MT_X, N,MUL_X,     Y,DIV_D,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      DIVU->     List(Y,     BR_N,  REN_Y,REN_Y,A2_X,    A1_X,  DW_XPR,FN_X,   M_N,M_X,      MT_X, N,MUL_X,     Y,DIV_DU,   WEN_N,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      REM->      List(Y,     BR_N,  REN_Y,REN_Y,A2_X,    A1_X,  DW_XPR,FN_X,   M_N,M_X,      MT_X, N,MUL_X,     Y,DIV_R,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      REMU->     List(Y,     BR_N,  REN_Y,REN_Y,A2_X,    A1_X,  DW_XPR,FN_X,   M_N,M_X,      MT_X, N,MUL_X,     Y,DIV_RU,   WEN_N,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      DIVW->     List(xpr64, BR_N,  REN_Y,REN_Y,A2_X,    A1_X,  DW_32, FN_X,   M_N,M_X,      MT_X, N,MUL_X,     Y,DIV_D,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      DIVUW->    List(xpr64, BR_N,  REN_Y,REN_Y,A2_X,    A1_X,  DW_32, FN_X,   M_N,M_X,      MT_X, N,MUL_X,     Y,DIV_DU,   WEN_N,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      REMW->     List(xpr64, BR_N,  REN_Y,REN_Y,A2_X,    A1_X,  DW_32, FN_X,   M_N,M_X,      MT_X, N,MUL_X,     Y,DIV_R,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      REMUW->    List(xpr64, BR_N,  REN_Y,REN_Y,A2_X,    A1_X,  DW_32, FN_X,   M_N,M_X,      MT_X, N,MUL_X,     Y,DIV_RU,   WEN_N,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),

      SYSCALL->  List(Y,     BR_N,  REN_N,REN_N,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,Y,N),
      EI->       List(Y,     BR_N,  REN_N,REN_N,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_EI,SYNC_N,N,N,Y),
      DI->       List(Y,     BR_N,  REN_N,REN_N,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_DI,SYNC_N,N,N,Y),
      ERET->     List(Y,     BR_N,  REN_N,REN_N,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_PCR,REN_N,WEN_N,I_X ,SYNC_N,Y,N,Y),
      FENCE->    List(Y,     BR_N,  REN_N,REN_N,A2_X,    A1_X,  DW_X,  FN_X,   M_Y,M_FENCE,  MT_X, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_D,N,N,N),
      FENCE_I->  List(Y,     BR_N,  REN_N,REN_N,A2_X,    A1_X,  DW_X,  FN_X,   M_Y,M_FLA,    MT_X, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_I,N,N,N),
      CFLUSH->   List(Y,     BR_N,  REN_Y,REN_N,A2_X,    A1_X,  DW_X,  FN_X,   M_Y,M_FLA,    MT_X, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,Y),
      MFPCR->    List(Y,     BR_N,  REN_N,REN_N,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_PCR,REN_Y,WEN_N,I_X ,SYNC_N,N,N,Y),
      MTPCR->    List(Y,     BR_N,  REN_N,REN_Y,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_Y,I_X ,SYNC_N,N,N,Y),
      RDTIME->   List(Y,     BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_TSC,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      RDCYCLE->  List(Y,     BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_TSC,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      RDINSTRET->List(Y,     BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_Y,WA_RD,WB_IRT,REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      
      // Instructions that have not yet been implemented
      // Faking these for now so akaros will boot    
      MFFSR->    List(Y,     BR_N,  REN_N,REN_N,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      MTFSR->    List(Y,     BR_N,  REN_N,REN_N,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      FLW->      List(Y,     BR_N,  REN_N,REN_N,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      FLD->      List(Y,     BR_N,  REN_N,REN_N,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      FSW->      List(Y,     BR_N,  REN_N,REN_N,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      FSD->      List(Y,     BR_N,  REN_N,REN_N,A2_X,    A1_X,  DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N)
/*  
      // floating point
      FLW->      List(FPU_Y, BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_ADD, M_Y,M_FRD,    MT_WU,N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      FLD->      List(FPU_Y, BR_N,  REN_N,REN_Y,A2_SEXT, A1_RS1,DW_XPR,FN_ADD, M_Y,M_FRD,    MT_D, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      FSW->      List(FPU_Y, BR_N,  REN_Y,REN_Y,A2_SPLIT,A1_RS1,DW_XPR,FN_ADD, M_Y,M_FWR,    MT_W, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      FSD->      List(FPU_Y, BR_N,  REN_Y,REN_Y,A2_SPLIT,A1_RS1,DW_XPR,FN_ADD, M_Y,M_FWR,    MT_D, N,MUL_X,     N,DIV_X,    WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      
      // atomic memory operations
      AMOMIN_W-> List(Y,     BR_N,  REN_Y,REN_Y,A2_0,    A1_RS1,DW_XPR,FN_ADD, M_Y,M_XA_MIN, MT_W, N,MUL_X,     N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      AMOMAX_W-> List(Y,     BR_N,  REN_Y,REN_Y,A2_0,    A1_RS1,DW_XPR,FN_ADD, M_Y,M_XA_MAX, MT_W, N,MUL_X,     N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      AMOMINU_W->List(Y,     BR_N,  REN_Y,REN_Y,A2_0,    A1_RS1,DW_XPR,FN_ADD, M_Y,M_XA_MINU,MT_W, N,MUL_X,     N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      AMOMAXU_W->List(Y,     BR_N,  REN_Y,REN_Y,A2_0,    A1_RS1,DW_XPR,FN_ADD, M_Y,M_XA_MAXU,MT_W, N,MUL_X,     N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      AMOMIN_D-> List(xpr64, BR_N,  REN_Y,REN_Y,A2_0,    A1_RS1,DW_XPR,FN_ADD, M_Y,M_XA_MIN, MT_D, N,MUL_X,     N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      AMOMAX_D-> List(xpr64, BR_N,  REN_Y,REN_Y,A2_0,    A1_RS1,DW_XPR,FN_ADD, M_Y,M_XA_MAX, MT_D, N,MUL_X,     N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      AMOMINU_D->List(xpr64, BR_N,  REN_Y,REN_Y,A2_0,    A1_RS1,DW_XPR,FN_ADD, M_Y,M_XA_MINU,MT_D, N,MUL_X,     N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
      AMOMAXU_D->List(xpr64, BR_N,  REN_Y,REN_Y,A2_0,    A1_RS1,DW_XPR,FN_ADD, M_Y,M_XA_MAXU,MT_D, N,MUL_X,     N,DIV_X,    WEN_N,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N),
*/
     ));

  val if_reg_xcpt_ma_inst = Reg(io.dpath.xcpt_ma_inst, resetVal = Bool(false));

  val id_int_val :: id_br_type :: id_renx2 :: id_renx1 :: id_sel_alu2 :: id_sel_alu1 :: id_fn_dw :: id_fn_alu :: csremainder = cs; 
  val id_mem_val :: id_mem_cmd :: id_mem_type :: id_mul_val :: id_mul_fn :: id_div_val :: id_div_fn :: id_wen :: id_sel_wa :: id_sel_wb :: id_ren_pcr :: id_wen_pcr :: id_irq :: id_sync :: id_eret :: id_syscall :: id_privileged :: Nil = csremainder;

  val id_raddr2 = io.dpath.inst(21,17);
  val id_raddr1 = io.dpath.inst(26,22);
  val id_waddr  = Mux(id_sel_wa === WA_RA, RA, io.dpath.inst(31,27));

  val id_console_out_val  = id_wen_pcr.toBool && (id_raddr2 === PCR_CONSOLE);

  val wb_reg_div_mul_val = Reg(resetVal = Bool(false))
  val dcache_miss =   Reg(io.dmem.resp_miss, resetVal = Bool(false));

  val sboard = new rocketCtrlSboard(); 
  sboard.io.raddra  := id_raddr2.toUFix;
  sboard.io.raddrb  := id_raddr1.toUFix;
  sboard.io.raddrc  := id_waddr.toUFix;

  // scoreboard set (for D$ misses, div, mul)
  sboard.io.set     := wb_reg_div_mul_val | dcache_miss;
  sboard.io.seta    := io.dpath.wb_waddr;

  sboard.io.clr    := io.dpath.sboard_clr;
  sboard.io.clra   := io.dpath.sboard_clra;

  val id_stall_raddr2 = sboard.io.stalla;
  val id_stall_raddr1 = sboard.io.stallb;
  val id_stall_waddr  = sboard.io.stallc;

  val id_reg_btb_hit      = Reg(resetVal = Bool(false));
  val id_reg_xcpt_itlb    = Reg(resetVal = Bool(false));
  val id_reg_xcpt_ma_inst = Reg(resetVal = Bool(false));
  val id_reg_icmiss       = Reg(resetVal = Bool(false));
  val id_reg_replay       = Reg(resetVal = Bool(false));
  
  val ex_reg_br_type     = Reg(){UFix(width = 4)};
  val ex_reg_btb_hit     = Reg(){Bool()};
  val ex_reg_div_mul_val = Reg(){Bool()};
  val ex_reg_mem_val     = Reg(){Bool()};
  val ex_reg_mem_cmd     = Reg(){UFix(width = 4)};
  val ex_reg_mem_type    = Reg(){UFix(width = 3)};
  val ex_reg_eret        = Reg(resetVal = Bool(false));
  val ex_reg_privileged  = Reg(resetVal = Bool(false));
  val ex_reg_inst_di          = Reg(resetVal = Bool(false));
  val ex_reg_inst_ei          = Reg(resetVal = Bool(false));
  val ex_reg_flush_inst  = Reg(resetVal = Bool(false));
  val ex_reg_xcpt_ma_inst    = Reg(resetVal = Bool(false));
  val ex_reg_xcpt_itlb       = Reg(resetVal = Bool(false));
  val ex_reg_xcpt_illegal    = Reg(resetVal = Bool(false));
  val ex_reg_xcpt_privileged = Reg(resetVal = Bool(false));
  val ex_reg_xcpt_fpu        = Reg(resetVal = Bool(false));
  val ex_reg_xcpt_syscall    = Reg(resetVal = Bool(false));
  val ex_reg_replay          = Reg(resetVal = Bool(false));

  val mem_reg_inst_di         = Reg(resetVal = Bool(false));
  val mem_reg_inst_ei         = Reg(resetVal = Bool(false));
  val mem_reg_flush_inst      = Reg(resetVal = Bool(false));
  val mem_reg_xcpt_ma_inst    = Reg(resetVal = Bool(false));
  val mem_reg_xcpt_itlb       = Reg(resetVal = Bool(false));
  val mem_reg_xcpt_illegal    = Reg(resetVal = Bool(false));
  val mem_reg_xcpt_privileged = Reg(resetVal = Bool(false));
  val mem_reg_xcpt_fpu        = Reg(resetVal = Bool(false));
  val mem_reg_xcpt_syscall    = Reg(resetVal = Bool(false));
  val mem_reg_replay          = Reg(resetVal = Bool(false));
  val mem_reg_kill            = Reg(resetVal = Bool(false));

  val wb_reg_inst_di         = Reg(resetVal = Bool(false));
  val wb_reg_inst_ei         = Reg(resetVal = Bool(false));
  val wb_reg_eret            = Reg(resetVal = Bool(false));
  val wb_reg_exception       = Reg(resetVal = Bool(false));
  val wb_reg_badvaddr_wen    = Reg(resetVal = Bool(false));
  val wb_reg_replay          = Reg(resetVal = Bool(false));
  val wb_reg_cause           = Reg(){UFix()};

  val take_pc = Wire() { Bool() };

  when (!io.dpath.stalld) {
    when (io.dpath.killf) {
      id_reg_btb_hit <== Bool(false);
      id_reg_xcpt_ma_inst <== Bool(false);   
      id_reg_xcpt_itlb <== Bool(false);
    } 
    otherwise{
      id_reg_btb_hit <== io.dpath.btb_hit;
      id_reg_xcpt_ma_inst <== if_reg_xcpt_ma_inst;
      id_reg_xcpt_itlb <== io.xcpt_itlb;
    }
    id_reg_icmiss <== !io.imem.resp_val;
    id_reg_replay <== !take_pc && !io.imem.resp_val;
  }
  
  // executing ERET when traps are enabled causes an illegal instruction exception (as per ISA sim)
//   val illegal_inst = !(id_int_val.toBool || id_fp_val.toBool) || (id_eret.toBool && io.dpath.status(SR_ET).toBool);
  val illegal_inst = !id_int_val.toBool || (id_eret.toBool && io.dpath.status(SR_ET).toBool);
  
  when (reset.toBool || io.dpath.killd) {
    ex_reg_br_type     <== BR_N;
    ex_reg_btb_hit     <== Bool(false);
    ex_reg_div_mul_val <== Bool(false);
    ex_reg_mem_val     <== Bool(false);
    ex_reg_eret        <== Bool(false);
    ex_reg_privileged  <== Bool(false);
    ex_reg_inst_di     <== Bool(false);
    ex_reg_inst_ei     <== Bool(false);
    ex_reg_flush_inst  <== Bool(false);  
    ex_reg_xcpt_ma_inst     <== Bool(false);
    ex_reg_xcpt_itlb        <== Bool(false);
    ex_reg_xcpt_illegal     <== Bool(false);
    ex_reg_xcpt_privileged  <== Bool(false);
    ex_reg_xcpt_fpu         <== Bool(false);
    ex_reg_xcpt_syscall     <== Bool(false);
    ex_reg_replay           <== Bool(false);
  } 
  otherwise {
    ex_reg_br_type     <== id_br_type;
    ex_reg_btb_hit     <== id_reg_btb_hit;
    ex_reg_div_mul_val <== id_div_val.toBool || id_mul_val.toBool;
    ex_reg_mem_val     <== id_mem_val.toBool;
    ex_reg_eret        <== id_eret.toBool;
    ex_reg_privileged  <== id_privileged.toBool;
    ex_reg_inst_di     <== (id_irq === I_DI);
    ex_reg_inst_ei     <== (id_irq === I_EI);
    ex_reg_flush_inst  <== (id_sync === SYNC_I);
    ex_reg_xcpt_ma_inst     <== id_reg_xcpt_ma_inst;
    ex_reg_xcpt_itlb        <== id_reg_xcpt_itlb;
    ex_reg_xcpt_illegal     <== illegal_inst;
    ex_reg_xcpt_privileged  <== (id_privileged & ~io.dpath.status(SR_S)).toBool;
//     ex_reg_xcpt_fpu         <== id_fp_val.toBool;
    ex_reg_xcpt_fpu         <== Bool(false);
    ex_reg_xcpt_syscall     <== id_syscall.toBool;
    ex_reg_replay           <== id_reg_replay;
  }
  ex_reg_mem_cmd     <== id_mem_cmd;
  ex_reg_mem_type    <== id_mem_type;


  val jr_taken = (ex_reg_br_type === BR_JR);
  val j_taken  = (ex_reg_br_type === BR_J);
  io.dpath.ex_jmp := j_taken;
  io.dpath.ex_jr  := jr_taken;

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
    (ex_reg_br_type === BR_GEU) & bgeu |
    j_taken; // treat J/JAL like a taken branch
  
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
    mem_reg_privileged  <== Bool(false);
    mem_reg_inst_di     <== Bool(false);
    mem_reg_inst_ei     <== Bool(false);
    mem_reg_flush_inst  <== Bool(false);
    mem_reg_xcpt_ma_inst     <== Bool(false);
    mem_reg_xcpt_itlb        <== Bool(false);
    mem_reg_xcpt_illegal     <== Bool(false);
    mem_reg_xcpt_privileged  <== Bool(false);
    mem_reg_xcpt_fpu         <== Bool(false);
    mem_reg_xcpt_syscall     <== Bool(false);
  }
  otherwise {
    mem_reg_div_mul_val <== ex_reg_div_mul_val;
    mem_reg_eret        <== ex_reg_eret;
    mem_reg_mem_val     <== ex_reg_mem_val;
    mem_reg_privileged  <== ex_reg_privileged;
    mem_reg_inst_di     <== ex_reg_inst_di;
    mem_reg_inst_ei     <== ex_reg_inst_ei;
    mem_reg_flush_inst  <== ex_reg_flush_inst;
    mem_reg_xcpt_ma_inst     <== ex_reg_xcpt_ma_inst;
    mem_reg_xcpt_itlb        <== ex_reg_xcpt_itlb;
    mem_reg_xcpt_illegal     <== ex_reg_xcpt_illegal;
    mem_reg_xcpt_privileged  <== ex_reg_xcpt_privileged;
    mem_reg_xcpt_fpu         <== ex_reg_xcpt_fpu;
    mem_reg_xcpt_syscall     <== ex_reg_xcpt_syscall;
  }
  mem_reg_mem_cmd     <== ex_reg_mem_cmd;
  mem_reg_mem_type    <== ex_reg_mem_type;

  when (io.dpath.killm) {
    wb_reg_eret        <== Bool(false);
    wb_reg_inst_di     <== Bool(false);
    wb_reg_inst_ei     <== Bool(false);
    wb_reg_div_mul_val <== Bool(false);
  }
  otherwise {
    wb_reg_eret        <== mem_reg_eret;
    wb_reg_inst_di     <== mem_reg_inst_di;
    wb_reg_inst_ei     <== mem_reg_inst_ei;
    wb_reg_div_mul_val <== mem_reg_div_mul_val;
  }

  // exception handling
  // FIXME: verify PC in MEM stage points to valid, restartable instruction
  val p_irq_timer = (io.dpath.status(15).toBool && io.dpath.irq_timer);
  val p_irq_ipi   = (io.dpath.status(13).toBool && io.dpath.irq_ipi);
  val interrupt = 
    io.dpath.status(SR_ET).toBool && io.dpath.mem_valid && 
    ((io.dpath.status(15).toBool && io.dpath.irq_timer) ||
     (io.dpath.status(13).toBool && io.dpath.irq_ipi));
     
  val interrupt_cause = 
    Mux(p_irq_ipi, UFix(21,5),
    Mux(p_irq_timer, UFix(23,5),
      UFix(0,5)));

  val mem_xcpt_ma_ld = io.xcpt_ma_ld && !mem_reg_kill
  val mem_xcpt_ma_st = io.xcpt_ma_st && !mem_reg_kill
  val mem_xcpt_dtlb_ld = io.xcpt_dtlb_ld && !mem_reg_kill
  val mem_xcpt_dtlb_st = io.xcpt_dtlb_st && !mem_reg_kill
  
	val mem_exception = 
	  interrupt ||
	  mem_xcpt_ma_ld ||
	  mem_xcpt_ma_st ||
	  mem_xcpt_dtlb_ld ||
	  mem_xcpt_dtlb_st ||
	  mem_reg_xcpt_illegal || 
	  mem_reg_xcpt_privileged || 
	  mem_reg_xcpt_fpu || 
	  mem_reg_xcpt_syscall || 
	  mem_reg_xcpt_itlb ||
	  mem_reg_xcpt_ma_inst;
	
	val mem_cause = 
		Mux(interrupt,                interrupt_cause, // asynchronous interrupt
	  Mux(mem_reg_xcpt_itlb,        UFix(1,5), // instruction access fault
		Mux(mem_reg_xcpt_illegal,     UFix(2,5), // illegal instruction
		Mux(mem_reg_xcpt_privileged,  UFix(3,5), // privileged instruction
		Mux(mem_reg_xcpt_fpu,         UFix(4,5), // FPU disabled
		Mux(mem_reg_xcpt_syscall,     UFix(6,5), // system call
		// breakpoint
		Mux(mem_xcpt_ma_ld,           UFix(8,5), // misaligned load
		Mux(mem_xcpt_ma_st,           UFix(9,5), // misaligned store
		Mux(mem_xcpt_dtlb_ld,         UFix(10,5), // load fault
		Mux(mem_xcpt_dtlb_st,         UFix(11,5), // store fault
			UFix(0,5)))))))))));  // instruction address misaligned

	// write cause to PCR on an exception
	io.dpath.exception    := wb_reg_exception;
	io.dpath.cause        := wb_reg_cause;
	io.dpath.badvaddr_wen := wb_reg_badvaddr_wen;

  // control transfer from ex/mem
  val ex_btb_match = ex_reg_btb_hit && io.dpath.btb_match
  val br_jr_taken = br_taken || jr_taken
  val take_pc_ex = !ex_btb_match && br_jr_taken || ex_reg_btb_hit && !br_jr_taken
  val take_pc_wb = wb_reg_replay || wb_reg_exception || wb_reg_eret;
  take_pc <== take_pc_ex || take_pc_wb;

  // replay mem stage PC on a DTLB miss
  val replay_mem  = io.dtlb_miss || io.dmem.resp_nack || mem_reg_replay;
  val kill_mem    = io.dtlb_miss || io.dmem.resp_nack || take_pc_wb || mem_exception || mem_reg_kill;
  val kill_dcache = io.dtlb_miss ||                      take_pc_wb || mem_exception || mem_reg_kill;
	
  // replay execute stage PC when the D$ is blocked, when the D$ misses, 
  // for privileged instructions, and for fence.i instructions
  val replay_ex    = dcache_miss && Reg(io.dpath.mem_lu_bypass) || mem_reg_privileged || mem_reg_flush_inst || 
                     ex_reg_replay || ex_reg_mem_val && !(io.dmem.req_rdy && io.dtlb_rdy)
  val kill_ex      = take_pc_wb || replay_ex

  mem_reg_replay <== replay_ex && !take_pc_wb;
  mem_reg_kill <== kill_ex;

  wb_reg_replay       <== replay_mem && !take_pc_wb;
  wb_reg_exception    <== mem_exception && !take_pc_wb;
  wb_reg_badvaddr_wen <== (mem_xcpt_dtlb_ld || mem_xcpt_dtlb_st) && !take_pc_wb;
  wb_reg_cause        <== mem_cause;

  io.dpath.sel_pc :=
    Mux(wb_reg_exception,               PC_EVEC, // exception
    Mux(wb_reg_replay,                  PC_WB,   // replay
    Mux(wb_reg_eret,                    PC_PCR,  // eret instruction
    Mux(ex_reg_btb_hit && !br_jr_taken, PC_EX4,  // mispredicted not taken branch
    Mux(!ex_btb_match && br_taken,      PC_BR,   // mispredicted taken branch
    Mux(!ex_btb_match && jr_taken,      PC_JR,   // mispredicted jump register
    Mux(io.dpath.btb_hit,               PC_BTB,  // predicted PC from BTB
        PC_4))))))); // PC+4

  io.dpath.wen_btb := !ex_btb_match && br_jr_taken;
  io.dpath.clr_btb := ex_reg_btb_hit && !br_jr_taken || id_reg_icmiss;
  
  io.imem.req_val  := take_pc_wb || !mem_reg_replay && !ex_reg_replay && (take_pc_ex || !id_reg_icmiss)

  // stall for RAW/WAW hazards on loads, AMOs, and mul/div in execute stage.
  val ex_mem_cmd_load = 
    ex_reg_mem_val && ((ex_reg_mem_cmd === M_XRD) || ex_reg_mem_cmd(3).toBool);
  val data_hazard_ex =
    (ex_mem_cmd_load || ex_reg_div_mul_val) &&
    ((id_renx1.toBool && (id_raddr1 === io.dpath.ex_waddr)) ||
     (id_renx2.toBool && (id_raddr2 === io.dpath.ex_waddr)) ||
     (id_wen.toBool   && (id_waddr  === io.dpath.ex_waddr)));
    
  // stall for RAW/WAW hazards on LB/LH and mul/div in memory stage.
  // stall for WAW-but-not-RAW hazards on LW/LD/AMO.
  val mem_mem_cmd_load =
    mem_reg_mem_val && ((mem_reg_mem_cmd === M_XRD) || mem_reg_mem_cmd(3).toBool);
  val mem_mem_cmd_load_bh = 
    mem_mem_cmd_load &&
    ((mem_reg_mem_type === MT_B)  ||
     (mem_reg_mem_type === MT_BU) ||
     (mem_reg_mem_type === MT_H)  || 
     (mem_reg_mem_type === MT_HU));
  val raw_hazard_mem =
    (id_renx1.toBool && (id_raddr1 === io.dpath.mem_waddr)) ||
    (id_renx2.toBool && (id_raddr2 === io.dpath.mem_waddr));
  val waw_hazard_mem =
    (id_wen.toBool   && (id_waddr  === io.dpath.mem_waddr));
  val data_hazard_mem =
    (mem_mem_cmd_load_bh || mem_reg_div_mul_val) && (raw_hazard_mem || waw_hazard_mem) ||
    mem_mem_cmd_load && (!raw_hazard_mem && waw_hazard_mem)

  // stall for RAW/WAW hazards on load/AMO misses and mul/div in writeback.
  val data_hazard_wb =
    (dcache_miss || wb_reg_div_mul_val) &&
    ((id_renx1.toBool && (id_raddr1 === io.dpath.wb_waddr)) ||
     (id_renx2.toBool && (id_raddr2 === io.dpath.wb_waddr)) ||
     (id_wen.toBool   && (id_waddr  === io.dpath.wb_waddr)));

  val data_hazard = data_hazard_ex || data_hazard_mem || data_hazard_wb;
      
  // for divider, multiplier, load miss writeback
  val mem_wb = Reg(io.dmem.resp_replay, resetVal = Bool(false)) // delayed for subword extension
  val mul_wb = io.dpath.mul_result_val && !io.dmem.resp_replay;
  val div_wb = io.dpath.div_result_val && !io.dpath.mul_result_val && !io.dmem.resp_replay;

  val ctrl_stalld =
    !take_pc &&
    (
      data_hazard ||
      id_renx2.toBool && id_stall_raddr2 ||
      id_renx1.toBool && id_stall_raddr1 ||
      id_wen.toBool   && id_stall_waddr  ||
      id_mem_val.toBool && !(io.dmem.req_rdy && io.dtlb_rdy) ||
      ((id_sync === SYNC_D) || (id_sync === SYNC_I)) && !io.dmem.req_rdy ||
      id_console_out_val && !io.console.rdy ||
      id_div_val.toBool && !io.dpath.div_rdy ||
      id_mul_val.toBool && !io.dpath.mul_rdy ||
      io.dpath.div_result_val ||
      io.dpath.mul_result_val ||
      mem_wb
    );
  val ctrl_stallf = ctrl_stalld;
    
  val ctrl_killd = take_pc || ctrl_stalld;
  val ctrl_killf = take_pc || !io.imem.resp_val;
  
  io.flush_inst     := mem_reg_flush_inst;


  io.dpath.stallf   := ctrl_stallf;
  io.dpath.stalld   := ctrl_stalld;
  io.dpath.killf    := ctrl_killf;
  io.dpath.killd    := ctrl_killd;
  io.dpath.killx    := kill_ex;
  io.dpath.killm    := kill_mem;

  io.dpath.mem_load := mem_reg_mem_val && ((mem_reg_mem_cmd === M_XRD) || mem_reg_mem_cmd(3).toBool);
  io.dpath.ren2     := id_renx2.toBool;
  io.dpath.ren1     := id_renx1.toBool;
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
  io.dpath.id_eret  := id_eret.toBool;
  io.dpath.wb_eret  := wb_reg_eret;  
  io.dpath.irq_disable := wb_reg_inst_di;
  io.dpath.irq_enable  := wb_reg_inst_ei;

  io.dtlb_val         := ex_reg_mem_val;
  io.dtlb_kill        := mem_reg_kill;
  io.dmem.req_val     := ex_reg_mem_val;
  io.dmem.req_kill    := kill_dcache;
  io.dmem.req_cmd     := ex_reg_mem_cmd;
  io.dmem.req_type    := ex_reg_mem_type;
}

}
