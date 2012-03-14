package rocket

import Chisel._
import Node._;

import Constants._
import Instructions._
import hwacha._

class ioCtrlDpath extends Bundle()
{
  // outputs to datapath
  val sel_pc   = UFix(3, OUTPUT);
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
  val sel_alu2 = UFix(3, OUTPUT);
  val fn_dw    = Bool(OUTPUT);
  val fn_alu   = UFix(4, OUTPUT);
  val mul_val  = Bool(OUTPUT);
  val mul_fn   = UFix(2, OUTPUT);
  val div_val  = Bool(OUTPUT);
  val div_fn   = UFix(2, OUTPUT);
  val sel_wa   = Bool(OUTPUT);
  val sel_wb   = UFix(3, OUTPUT);
  val ren_pcr  = Bool(OUTPUT);
  val wen_pcr  = Bool(OUTPUT);
  val id_eret  = Bool(OUTPUT);
  val wb_eret  = Bool(OUTPUT);
  val mem_load = Bool(OUTPUT);
  val ex_fp_val= Bool(OUTPUT);
  val mem_fp_val= Bool(OUTPUT);
  val ex_wen   = Bool(OUTPUT);
  val mem_wen  = Bool(OUTPUT);
  val wb_wen   = Bool(OUTPUT);
  val wb_valid = Bool(OUTPUT)
  val flush_inst = Bool(OUTPUT);
  val ex_mem_type = UFix(3,OUTPUT)
  // enable/disable interrupts
  val irq_enable   = Bool(OUTPUT);
  val irq_disable   = Bool(OUTPUT);
  // exception handling
  val exception = Bool(OUTPUT);
  val cause    = UFix(5,OUTPUT);
  val badvaddr_wen = Bool(OUTPUT); // high for a load/store access fault
  val vec_irq_aux_wen = Bool(OUTPUT)
  // inputs from datapath
  val xcpt_ma_inst = Bool(INPUT);  // high on a misaligned/illegal virtual PC
  val btb_hit = Bool(INPUT);
  val inst    = Bits(32, INPUT);
  val br_eq   = Bool(INPUT);
  val br_lt   = Bool(INPUT);
  val br_ltu  = Bool(INPUT);
  val div_rdy = Bool(INPUT);
  val div_result_val = Bool(INPUT);
  val mul_rdy = Bool(INPUT);
  val mul_result_val = Bool(INPUT);
  val mem_wb = Bool(INPUT);
  val ex_waddr = UFix(5,INPUT);  // write addr from execute stage
  val mem_waddr = UFix(5,INPUT); // write addr from memory stage
  val wb_waddr = UFix(5,INPUT);  // write addr from writeback stage
  val status  = Bits(17, INPUT);
  val sboard_clr  = Bool(INPUT);
  val sboard_clra = UFix(5, INPUT);
  val fp_sboard_clr  = Bool(INPUT);
  val fp_sboard_clra = UFix(5, INPUT);
  val irq_timer = Bool(INPUT);
  val irq_ipi   = Bool(INPUT);
}

class ioCtrlAll extends Bundle()
{
  val dpath   = new ioCtrlDpath();
  val imem    = new ioImem(List("req_val", "resp_val")).flip
  val dmem    = new ioDmem(List("req_val", "req_kill", "req_rdy", "req_cmd", "req_type", "resp_miss", "resp_nack", "xcpt_ma_ld", "xcpt_ma_st")).flip
  val dtlb_val = Bool(OUTPUT);
  val dtlb_kill = Bool(OUTPUT);
  val dtlb_rdy = Bool(INPUT);
  val dtlb_miss = Bool(INPUT);
  val xcpt_dtlb_ld = Bool(INPUT);
  val xcpt_dtlb_st = Bool(INPUT);
  val xcpt_itlb = Bool(INPUT);
  val fpu = new ioCtrlFPU();
  val vec_dpath = new ioCtrlDpathVec()
  val vec_iface = new ioCtrlVecInterface()
}

object rocketCtrlDecode
{
  val xpr64 = Y;

  val decode_default =
                //                                                                                                                                        vfence_cv
                //                                                                                                                                        | eret
                //                                                                                                                                        | | syscall
                //         vec_val                                      mem_val             mul_val   div_val                    renpcr                   | | | privileged
                //   val   | brtype renx2 renx1 s_alu2   dw     alu     |   mem_cmd mem_type| mul_fn  | div_fn wen   s_wa  s_wb   |    wenpcr irq  sync   | | | | replay_next
                //   |     | |      |     |     |        |      |       |   |         |     | |       | |      |     |     |      |     |     |    |      | | | | |
                List(N,    N,BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N)

  val xdecode = Array(
                //                                                                                                                                        vfence_cv
                //                                                                                                                                        | eret
                //                                                                                                                                        | | syscall
                //         vec_val                                      mem_val             mul_val   div_val                    renpcr                   | | | privileged
                //   val   | brtype renx2 renx1 s_alu2   dw     alu     |   mem_cmd mem_type| mul_fn  | div_fn wen   s_wa  s_wb   |    wenpcr irq  sync   | | | | replay_next
                //   |     | |      |     |     |        |      |       |   |         |     | |       | |      |     |     |      |     |     |    |      | | | | |
    BNE->       List(Y,    N,BR_NE, REN_Y,REN_Y,A2_BTYPE,DW_X,  FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    BEQ->       List(Y,    N,BR_EQ, REN_Y,REN_Y,A2_BTYPE,DW_X,  FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    BLT->       List(Y,    N,BR_LT, REN_Y,REN_Y,A2_BTYPE,DW_X,  FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    BLTU->      List(Y,    N,BR_LTU,REN_Y,REN_Y,A2_BTYPE,DW_X,  FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    BGE->       List(Y,    N,BR_GE, REN_Y,REN_Y,A2_BTYPE,DW_X,  FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    BGEU->      List(Y,    N,BR_GEU,REN_Y,REN_Y,A2_BTYPE,DW_X,  FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),

    J->         List(Y,    N,BR_J,  REN_N,REN_N,A2_JTYPE,DW_X,  FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    JAL->       List(Y,    N,BR_J,  REN_N,REN_N,A2_JTYPE,DW_X,  FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RA,WB_PC, REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    JALR_C->    List(Y,    N,BR_JR, REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_PC, REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    JALR_J->    List(Y,    N,BR_JR, REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_PC, REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    JALR_R->    List(Y,    N,BR_JR, REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_PC, REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    RDNPC->     List(Y,    N,BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_PC, REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),

    LB->        List(Y,    N,BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_ADD, M_Y,M_XRD,    MT_B, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    LH->        List(Y,    N,BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_ADD, M_Y,M_XRD,    MT_H, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    LW->        List(Y,    N,BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_ADD, M_Y,M_XRD,    MT_W, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    LD->        List(xpr64,N,BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_ADD, M_Y,M_XRD,    MT_D, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    LBU->       List(Y,    N,BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_ADD, M_Y,M_XRD,    MT_BU,N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    LHU->       List(Y,    N,BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_ADD, M_Y,M_XRD,    MT_HU,N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    LWU->       List(xpr64,N,BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_ADD, M_Y,M_XRD,    MT_WU,N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    SB->        List(Y,    N,BR_N,  REN_Y,REN_Y,A2_BTYPE,DW_XPR,FN_ADD, M_Y,M_XWR,    MT_B, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    SH->        List(Y,    N,BR_N,  REN_Y,REN_Y,A2_BTYPE,DW_XPR,FN_ADD, M_Y,M_XWR,    MT_H, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    SW->        List(Y,    N,BR_N,  REN_Y,REN_Y,A2_BTYPE,DW_XPR,FN_ADD, M_Y,M_XWR,    MT_W, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    SD->        List(xpr64,N,BR_N,  REN_Y,REN_Y,A2_BTYPE,DW_XPR,FN_ADD, M_Y,M_XWR,    MT_D, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),

    AMOADD_W->  List(Y,    N,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_Y,M_XA_ADD, MT_W, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    AMOSWAP_W-> List(Y,    N,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_Y,M_XA_SWAP,MT_W, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    AMOAND_W->  List(Y,    N,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_Y,M_XA_AND, MT_W, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    AMOOR_W->   List(Y,    N,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_Y,M_XA_OR,  MT_W, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    AMOMIN_W->  List(Y,    N,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_Y,M_XA_MIN, MT_W, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    AMOMINU_W-> List(Y,    N,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_Y,M_XA_MINU,MT_W, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    AMOMAX_W->  List(Y,    N,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_Y,M_XA_MAX, MT_W, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    AMOMAXU_W-> List(Y,    N,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_Y,M_XA_MAXU,MT_W, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    AMOADD_D->  List(xpr64,N,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_Y,M_XA_ADD, MT_D, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    AMOSWAP_D-> List(xpr64,N,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_Y,M_XA_SWAP,MT_D, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    AMOAND_D->  List(xpr64,N,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_Y,M_XA_AND, MT_D, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    AMOOR_D->   List(xpr64,N,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_Y,M_XA_OR,  MT_D, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    AMOMIN_D->  List(xpr64,N,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_Y,M_XA_MIN, MT_D, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    AMOMINU_D-> List(xpr64,N,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_Y,M_XA_MINU,MT_D, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    AMOMAX_D->  List(xpr64,N,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_Y,M_XA_MAX, MT_D, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    AMOMAXU_D-> List(xpr64,N,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_Y,M_XA_MAXU,MT_D, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),

    LUI->       List(Y,    N,BR_N,  REN_N,REN_N,A2_LTYPE,DW_XPR,FN_OP2, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    ADDI->      List(Y,    N,BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    SLTI ->     List(Y,    N,BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_SLT, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    SLTIU->     List(Y,    N,BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_SLTU,M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    ANDI->      List(Y,    N,BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_AND, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    ORI->       List(Y,    N,BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_OR,  M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    XORI->      List(Y,    N,BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_XOR, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    SLLI->      List(Y,    N,BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_SL,  M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    SRLI->      List(Y,    N,BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_SR,  M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    SRAI->      List(Y,    N,BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_SRA, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    ADD->       List(Y,    N,BR_N,  REN_Y,REN_Y,A2_RTYPE,DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    SUB->       List(Y,    N,BR_N,  REN_Y,REN_Y,A2_RTYPE,DW_XPR,FN_SUB, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    SLT->       List(Y,    N,BR_N,  REN_Y,REN_Y,A2_RTYPE,DW_XPR,FN_SLT, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    SLTU->      List(Y,    N,BR_N,  REN_Y,REN_Y,A2_RTYPE,DW_XPR,FN_SLTU,M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    riscvAND->  List(Y,    N,BR_N,  REN_Y,REN_Y,A2_RTYPE,DW_XPR,FN_AND, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    riscvOR->   List(Y,    N,BR_N,  REN_Y,REN_Y,A2_RTYPE,DW_XPR,FN_OR,  M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    riscvXOR->  List(Y,    N,BR_N,  REN_Y,REN_Y,A2_RTYPE,DW_XPR,FN_XOR, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    SLL->       List(Y,    N,BR_N,  REN_Y,REN_Y,A2_RTYPE,DW_XPR,FN_SL,  M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    SRL->       List(Y,    N,BR_N,  REN_Y,REN_Y,A2_RTYPE,DW_XPR,FN_SR,  M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    SRA->       List(Y,    N,BR_N,  REN_Y,REN_Y,A2_RTYPE,DW_XPR,FN_SRA, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),

    ADDIW->     List(xpr64,N,BR_N,  REN_N,REN_Y,A2_ITYPE,DW_32,FN_ADD,  M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),   
    SLLIW->     List(xpr64,N,BR_N,  REN_N,REN_Y,A2_ITYPE,DW_32,FN_SL,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    SRLIW->     List(xpr64,N,BR_N,  REN_N,REN_Y,A2_ITYPE,DW_32,FN_SR,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    SRAIW->     List(xpr64,N,BR_N,  REN_N,REN_Y,A2_ITYPE,DW_32,FN_SRA,  M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    ADDW->      List(xpr64,N,BR_N,  REN_Y,REN_Y,A2_RTYPE,DW_32,FN_ADD,  M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    SUBW->      List(xpr64,N,BR_N,  REN_Y,REN_Y,A2_RTYPE,DW_32,FN_SUB,  M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    SLLW->      List(xpr64,N,BR_N,  REN_Y,REN_Y,A2_RTYPE,DW_32,FN_SL,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    SRLW->      List(xpr64,N,BR_N,  REN_Y,REN_Y,A2_RTYPE,DW_32,FN_SR,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    SRAW->      List(xpr64,N,BR_N,  REN_Y,REN_Y,A2_RTYPE,DW_32,FN_SRA,  M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),

    MUL->       List(Y,    N,BR_N,  REN_Y,REN_Y,A2_X,    DW_XPR,FN_X,   M_N,M_X,      MT_X, Y,MUL_LO, N,DIV_X, WEN_Y,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    MULH->      List(Y,    N,BR_N,  REN_Y,REN_Y,A2_X,    DW_XPR,FN_X,   M_N,M_X,      MT_X, Y,MUL_H,  N,DIV_X, WEN_Y,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    MULHU->     List(Y,    N,BR_N,  REN_Y,REN_Y,A2_X,    DW_XPR,FN_X,   M_N,M_X,      MT_X, Y,MUL_HU, N,DIV_X, WEN_Y,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    MULHSU->    List(Y,    N,BR_N,  REN_Y,REN_Y,A2_X,    DW_XPR,FN_X,   M_N,M_X,      MT_X, Y,MUL_HSU,N,DIV_X, WEN_Y,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    MULW->      List(xpr64,N,BR_N,  REN_Y,REN_Y,A2_X,    DW_32, FN_X,   M_N,M_X,      MT_X, Y,MUL_LO, N,DIV_X, WEN_Y,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),

    DIV->       List(Y,    N,BR_N,  REN_Y,REN_Y,A2_X,    DW_XPR,FN_X,   M_N,M_X,      MT_X, N,MUL_X,  Y,DIV_D, WEN_Y,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    DIVU->      List(Y,    N,BR_N,  REN_Y,REN_Y,A2_X,    DW_XPR,FN_X,   M_N,M_X,      MT_X, N,MUL_X,  Y,DIV_DU,WEN_Y,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    REM->       List(Y,    N,BR_N,  REN_Y,REN_Y,A2_X,    DW_XPR,FN_X,   M_N,M_X,      MT_X, N,MUL_X,  Y,DIV_R, WEN_Y,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    REMU->      List(Y,    N,BR_N,  REN_Y,REN_Y,A2_X,    DW_XPR,FN_X,   M_N,M_X,      MT_X, N,MUL_X,  Y,DIV_RU,WEN_Y,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    DIVW->      List(xpr64,N,BR_N,  REN_Y,REN_Y,A2_X,    DW_32, FN_X,   M_N,M_X,      MT_X, N,MUL_X,  Y,DIV_D, WEN_Y,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    DIVUW->     List(xpr64,N,BR_N,  REN_Y,REN_Y,A2_X,    DW_32, FN_X,   M_N,M_X,      MT_X, N,MUL_X,  Y,DIV_DU,WEN_Y,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    REMW->      List(xpr64,N,BR_N,  REN_Y,REN_Y,A2_X,    DW_32, FN_X,   M_N,M_X,      MT_X, N,MUL_X,  Y,DIV_R, WEN_Y,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    REMUW->     List(xpr64,N,BR_N,  REN_Y,REN_Y,A2_X,    DW_32, FN_X,   M_N,M_X,      MT_X, N,MUL_X,  Y,DIV_RU,WEN_Y,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),

    SYSCALL->   List(Y,    N,BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,Y,N,N),
    EI->        List(Y,    N,BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_EI,SYNC_N,N,N,N,Y,Y),
    DI->        List(Y,    N,BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_DI,SYNC_N,N,N,N,Y,Y),
    ERET->      List(Y,    N,BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_PCR,REN_N,WEN_N,I_X ,SYNC_N,N,Y,N,Y,N),
    FENCE->     List(Y,    N,BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_Y,M_FENCE,  MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_D,N,N,N,N,N),
    FENCE_I->   List(Y,    N,BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_Y,M_FLA,    MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_I,N,N,N,N,Y),
    CFLUSH->    List(Y,    N,BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_Y,M_FLA,    MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,Y,Y),
    MFPCR->     List(Y,    N,BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_PCR,REN_Y,WEN_N,I_X ,SYNC_N,N,N,N,Y,N),
    MTPCR->     List(Y,    N,BR_N,  REN_N,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_ALU,REN_N,WEN_Y,I_X ,SYNC_N,N,N,N,Y,Y),
    RDTIME->    List(Y,    N,BR_N,  REN_N,REN_N,A2_X,    DW_XPR,FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_TSC,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    RDCYCLE->   List(Y,    N,BR_N,  REN_N,REN_N,A2_X,    DW_XPR,FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_TSC,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    RDINSTRET-> List(Y,    N,BR_N,  REN_N,REN_N,A2_X,    DW_XPR,FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_IRT,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N))
 
  val fdecode = Array(
                //                                                                                                                                        vfence_cv
                //                                                                                                                                        | eret
                //                                                                                                                                        | | syscall
                //         vec_val                                      mem_val             mul_val   div_val                    renpcr                   | | | privileged
                //   val   | brtype renx2 renx1 s_alu2   dw     alu     |   mem_cmd mem_type| mul_fn  | div_fn wen   s_wa  s_wb   |    wenpcr irq  sync   | | | | replay_next
                //   |     | |      |     |     |        |      |       |   |         |     | |       | |      |     |     |      |     |     |    |      | | | | |
    MFTX_S->    List(FPU_Y,N,BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    MFTX_D->    List(FPU_Y,N,BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    FCVT_W_S->  List(FPU_Y,N,BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    FCVT_W_D->  List(FPU_Y,N,BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    FCVT_WU_S-> List(FPU_Y,N,BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    FCVT_WU_D-> List(FPU_Y,N,BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    FCVT_L_S->  List(FPU_Y,N,BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    FCVT_L_D->  List(FPU_Y,N,BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    FCVT_LU_S-> List(FPU_Y,N,BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    FCVT_LU_D-> List(FPU_Y,N,BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    FEQ_S->     List(FPU_Y,N,BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    FEQ_D->     List(FPU_Y,N,BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    FLT_S->     List(FPU_Y,N,BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    FLT_D->     List(FPU_Y,N,BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    FLE_S->     List(FPU_Y,N,BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    FLE_D->     List(FPU_Y,N,BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    MXTF_S->    List(FPU_Y,N,BR_N,  REN_N,REN_Y,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    MXTF_D->    List(FPU_Y,N,BR_N,  REN_N,REN_Y,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    FCVT_S_W->  List(FPU_Y,N,BR_N,  REN_N,REN_Y,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    FCVT_D_W->  List(FPU_Y,N,BR_N,  REN_N,REN_Y,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    FCVT_S_WU-> List(FPU_Y,N,BR_N,  REN_N,REN_Y,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    FCVT_D_WU-> List(FPU_Y,N,BR_N,  REN_N,REN_Y,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    FCVT_S_L->  List(FPU_Y,N,BR_N,  REN_N,REN_Y,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    FCVT_D_L->  List(FPU_Y,N,BR_N,  REN_N,REN_Y,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    FCVT_S_LU-> List(FPU_Y,N,BR_N,  REN_N,REN_Y,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    FCVT_D_LU-> List(FPU_Y,N,BR_N,  REN_N,REN_Y,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    MFFSR->     List(FPU_Y,N,BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    MTFSR->     List(FPU_Y,N,BR_N,  REN_N,REN_Y,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    FLW->       List(FPU_Y,N,BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_ADD, M_Y,M_XRD,    MT_W, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    FLD->       List(FPU_Y,N,BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_ADD, M_Y,M_XRD,    MT_D, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    FSW->       List(FPU_Y,N,BR_N,  REN_N,REN_Y,A2_BTYPE,DW_XPR,FN_ADD, M_Y,M_XWR,    MT_W, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N),
    FSD->       List(FPU_Y,N,BR_N,  REN_N,REN_Y,A2_BTYPE,DW_XPR,FN_ADD, M_Y,M_XWR,    MT_D, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N,N))

  val vdecode = Array(
                //                                                                                                                                        vfence_cv
                //                                                                                                                                        | eret
                //                                                                                                                                        | | syscall
                //         vec_val                                      mem_val             mul_val   div_val                    renpcr                   | | | privileged
                //   val   | brtype renx2 renx1 s_alu2   dw     alu     |   mem_cmd mem_type| mul_fn  | div_fn wen   s_wa  s_wb   |    wenpcr irq  sync   | | | | replay_next
                //   |     | |      |     |     |        |      |       |   |         |     | |       | |      |     |     |      |     |     |    |      | | | | |
    VVCFGIVL->  List(VEC_Y,Y,BR_N,  REN_N,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,Y),
    VVCFG->     List(VEC_Y,Y,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,Y),
    VSETVL->    List(VEC_Y,Y,BR_N,  REN_N,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,Y),
    VF->        List(VEC_Y,Y,BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,N),
    VMVV->      List(VEC_Y,Y,BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_X,  REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,N),
    VMSV->      List(VEC_Y,Y,BR_N,  REN_N,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,N),
    VFMVV->     List(VEC_Y,Y,BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_X,  REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,N),
    FENCE_L_V-> List(VEC_Y,Y,BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,N),
    FENCE_G_V-> List(VEC_Y,Y,BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,N),
    FENCE_L_CV->List(VEC_Y,Y,BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X, SYNC_N,Y,N,N,N,N),
    FENCE_G_CV->List(VEC_Y,Y,BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X, SYNC_N,Y,N,N,N,N),
    VLD->       List(VEC_Y,Y,BR_N,  REN_N,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,N),
    VLW->       List(VEC_Y,Y,BR_N,  REN_N,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,N),
    VLWU->      List(VEC_Y,Y,BR_N,  REN_N,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,N),
    VLH->       List(VEC_Y,Y,BR_N,  REN_N,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,N),
    VLHU->      List(VEC_Y,Y,BR_N,  REN_N,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,N),
    VLB->       List(VEC_Y,Y,BR_N,  REN_N,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,N),
    VLBU->      List(VEC_Y,Y,BR_N,  REN_N,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,N),
    VSD->       List(VEC_Y,Y,BR_N,  REN_N,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,N),
    VSW->       List(VEC_Y,Y,BR_N,  REN_N,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,N),
    VSH->       List(VEC_Y,Y,BR_N,  REN_N,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,N),
    VSB->       List(VEC_Y,Y,BR_N,  REN_N,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,N),
    VFLD->      List(VEC_Y,Y,BR_N,  REN_N,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,N),
    VFLW->      List(VEC_Y,Y,BR_N,  REN_N,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,N),
    VFSD->      List(VEC_Y,Y,BR_N,  REN_N,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,N),
    VFSW->      List(VEC_Y,Y,BR_N,  REN_N,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,N),
    VLSTD->     List(VEC_Y,Y,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_D, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,N),
    VLSTW->     List(VEC_Y,Y,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_D, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,N),
    VLSTWU->    List(VEC_Y,Y,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_D, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,N),
    VLSTH->     List(VEC_Y,Y,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_D, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,N),
    VLSTHU->    List(VEC_Y,Y,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_D, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,N),
    VLSTB->     List(VEC_Y,Y,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_D, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,N),
    VLSTBU->    List(VEC_Y,Y,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_D, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,N),
    VSSTD->     List(VEC_Y,Y,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_D, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,N),
    VSSTW->     List(VEC_Y,Y,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_D, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,N),
    VSSTH->     List(VEC_Y,Y,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_D, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,N),
    VSSTB->     List(VEC_Y,Y,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_D, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,N),
    VFLSTD->    List(VEC_Y,Y,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_D, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,N),
    VFLSTW->    List(VEC_Y,Y,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_D, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,N),
    VFSSTD->    List(VEC_Y,Y,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_D, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,N),
    VFSSTW->    List(VEC_Y,Y,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_D, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,N,N),

    VENQCMD->   List(VEC_Y,Y,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,Y,N),
    VENQIMM1->  List(VEC_Y,Y,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,Y,N),
    VENQIMM2->  List(VEC_Y,Y,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,Y,N),
    VENQCNT->   List(VEC_Y,Y,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,Y,N),
    VXCPTEVAC-> List(VEC_Y,Y,BR_N,  REN_N,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_RD,WB_ALU,REN_N,WEN_N,I_X, SYNC_N,N,N,N,Y,N),
    VXCPTKILL-> List(VEC_Y,Y,BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X, SYNC_N,N,N,N,Y,N),
    VXCPTWAIT-> List(VEC_Y,Y,BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X, SYNC_N,N,N,N,Y,Y),
    VXCPTHOLD-> List(VEC_Y,Y,BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X, SYNC_N,N,N,N,Y,N))
}

class rocketCtrl extends Component
{
  val io = new ioCtrlAll();

  var decode_table = rocketCtrlDecode.xdecode
  if (HAVE_FPU) decode_table ++= rocketCtrlDecode.fdecode
  if (HAVE_VEC) decode_table ++= rocketCtrlDecode.vdecode

  val cs = ListLookup(io.dpath.inst, rocketCtrlDecode.decode_default, decode_table)

  val id_int_val :: id_vec_val :: id_br_type :: id_renx2 :: id_renx1 :: id_sel_alu2 :: id_fn_dw :: id_fn_alu :: cs0 = cs 
  val id_mem_val :: id_mem_cmd :: id_mem_type :: id_mul_val :: id_mul_fn :: id_div_val :: id_div_fn :: id_wen :: id_sel_wa :: id_sel_wb :: cs1 = cs0
  val id_ren_pcr :: id_wen_pcr :: id_irq :: id_sync :: id_vfence_cv :: id_eret :: id_syscall :: id_privileged :: id_replay_next :: Nil = cs1

  val if_reg_xcpt_ma_inst = Reg(io.dpath.xcpt_ma_inst, resetVal = Bool(false));

  val id_raddr3 = io.dpath.inst(16,12);
  val id_raddr2 = io.dpath.inst(21,17);
  val id_raddr1 = io.dpath.inst(26,22);
  val id_waddr  = Mux(id_sel_wa === WA_RA, RA, io.dpath.inst(31,27));

  val wb_reg_div_mul_val = Reg(resetVal = Bool(false))
  val wb_reg_dcache_miss = Reg(io.dmem.resp_miss, resetVal = Bool(false));

  val id_reg_valid        = Reg(resetVal = Bool(false));
  val id_reg_btb_hit      = Reg(resetVal = Bool(false));
  val id_reg_xcpt_itlb    = Reg(resetVal = Bool(false));
  val id_reg_xcpt_ma_inst = Reg(resetVal = Bool(false));
  val id_reg_icmiss       = Reg(resetVal = Bool(false));
  val id_reg_replay       = Reg(resetVal = Bool(false));
  val id_load_use         = Wire(){Bool()};
  
  val ex_reg_br_type     = Reg(){UFix(width = 4)};
  val ex_reg_btb_hit     = Reg(){Bool()};
  val ex_reg_div_val     = Reg(){Bool()};
  val ex_reg_mul_val     = Reg(){Bool()};
  val ex_reg_mem_val     = Reg(){Bool()};
  val ex_reg_mem_cmd     = Reg(){UFix(width = 4)};
  val ex_reg_mem_type    = Reg(){UFix(width = 3)};
  val ex_reg_valid       = Reg(resetVal = Bool(false));
  val ex_reg_wen_pcr     = Reg(resetVal = Bool(false));
  val ex_reg_wen         = Reg(resetVal = Bool(false));
  val ex_reg_fp_wen      = Reg(resetVal = Bool(false));
  val ex_reg_eret        = Reg(resetVal = Bool(false));
  val ex_reg_inst_di          = Reg(resetVal = Bool(false));
  val ex_reg_inst_ei          = Reg(resetVal = Bool(false));
  val ex_reg_flush_inst  = Reg(resetVal = Bool(false));
  val ex_reg_xcpt_ma_inst    = Reg(resetVal = Bool(false));
  val ex_reg_xcpt_itlb       = Reg(resetVal = Bool(false));
  val ex_reg_xcpt_illegal    = Reg(resetVal = Bool(false));
  val ex_reg_xcpt_privileged = Reg(resetVal = Bool(false));
  val ex_reg_xcpt_syscall    = Reg(resetVal = Bool(false));
  val ex_reg_fp_val          = Reg(resetVal = Bool(false));
  val ex_reg_fp_sboard_set   = Reg(resetVal = Bool(false));
  val ex_reg_vec_val         = Reg(resetVal = Bool(false));
  val ex_reg_replay          = Reg(resetVal = Bool(false));
  val ex_reg_load_use        = Reg(resetVal = Bool(false));

  val mem_reg_valid           = Reg(resetVal = Bool(false));
  val mem_reg_wen_pcr         = Reg(resetVal = Bool(false));
  val mem_reg_wen             = Reg(resetVal = Bool(false));
  val mem_reg_fp_wen          = Reg(resetVal = Bool(false));
  val mem_reg_inst_di         = Reg(resetVal = Bool(false));
  val mem_reg_inst_ei         = Reg(resetVal = Bool(false));
  val mem_reg_flush_inst      = Reg(resetVal = Bool(false));
  val mem_reg_xcpt_ma_inst    = Reg(resetVal = Bool(false));
  val mem_reg_xcpt_itlb       = Reg(resetVal = Bool(false));
  val mem_reg_xcpt_illegal    = Reg(resetVal = Bool(false));
  val mem_reg_xcpt_privileged = Reg(resetVal = Bool(false));
  val mem_reg_xcpt_fpu        = Reg(resetVal = Bool(false));
  val mem_reg_xcpt_vec        = Reg(resetVal = Bool(false));
  val mem_reg_xcpt_syscall    = Reg(resetVal = Bool(false));
  val mem_reg_fp_val          = Reg(resetVal = Bool(false));
  val mem_reg_replay          = Reg(resetVal = Bool(false));
  val mem_reg_kill            = Reg(resetVal = Bool(false));
  val mem_reg_fp_sboard_set   = Reg(resetVal = Bool(false));

  val wb_reg_valid           = Reg(resetVal = Bool(false));
  val wb_reg_wen_pcr         = Reg(resetVal = Bool(false));
  val wb_reg_wen             = Reg(resetVal = Bool(false));
  val wb_reg_fp_wen          = Reg(resetVal = Bool(false));
  val wb_reg_inst_di         = Reg(resetVal = Bool(false));
  val wb_reg_inst_ei         = Reg(resetVal = Bool(false));
  val wb_reg_flush_inst      = Reg(resetVal = Bool(false));
  val wb_reg_eret            = Reg(resetVal = Bool(false));
  val wb_reg_exception       = Reg(resetVal = Bool(false));
  val wb_reg_replay          = Reg(resetVal = Bool(false));
  val wb_reg_cause           = Reg(){UFix()};
  val wb_reg_fp_val          = Reg(resetVal = Bool(false));
  val wb_reg_fp_sboard_set   = Reg(resetVal = Bool(false));

  val take_pc = Wire() { Bool() };

  when (!io.dpath.stalld) {
    when (io.dpath.killf) {
      id_reg_valid := Bool(false)
      id_reg_btb_hit := Bool(false);
      id_reg_xcpt_ma_inst := Bool(false);   
      id_reg_xcpt_itlb := Bool(false);
      id_reg_replay := !take_pc; // replay on I$ miss
    } 
    .otherwise{
      id_reg_valid := Bool(true)
      id_reg_btb_hit := io.dpath.btb_hit;
      id_reg_xcpt_ma_inst := if_reg_xcpt_ma_inst;
      id_reg_xcpt_itlb := io.xcpt_itlb;
      id_reg_replay := id_replay_next
    }
    id_reg_icmiss := !io.imem.resp_val;
  }
  
  // executing ERET when traps are enabled causes an illegal instruction exception (as per ISA sim)
   val illegal_inst =
    !(id_int_val.toBool || io.fpu.dec.valid || id_vec_val.toBool) ||
    (id_eret.toBool && io.dpath.status(SR_ET).toBool);
  
  when (reset.toBool || io.dpath.killd) {
    ex_reg_br_type     := BR_N;
    ex_reg_btb_hit     := Bool(false);
    ex_reg_div_val := Bool(false);
    ex_reg_mul_val := Bool(false);
    ex_reg_mem_val     := Bool(false);
    ex_reg_valid       := Bool(false);
    ex_reg_wen_pcr     := Bool(false)
    ex_reg_wen         := Bool(false);
    ex_reg_fp_wen      := Bool(false);
    ex_reg_eret        := Bool(false);
    ex_reg_inst_di     := Bool(false);
    ex_reg_inst_ei     := Bool(false);
    ex_reg_flush_inst  := Bool(false);  
    ex_reg_xcpt_ma_inst     := Bool(false);
    ex_reg_xcpt_itlb        := Bool(false);
    ex_reg_xcpt_illegal     := Bool(false);
    ex_reg_xcpt_privileged  := Bool(false);
    ex_reg_xcpt_syscall     := Bool(false);
    ex_reg_fp_val           := Bool(false);
    ex_reg_fp_sboard_set    := Bool(false);
    ex_reg_vec_val          := Bool(false);
    ex_reg_replay           := Bool(false);
    ex_reg_load_use         := Bool(false);
  } 
  .otherwise {
    ex_reg_br_type     := id_br_type;
    ex_reg_btb_hit     := id_reg_btb_hit;
    ex_reg_div_val     := id_div_val.toBool && id_waddr != UFix(0);
    ex_reg_mul_val     := id_mul_val.toBool && id_waddr != UFix(0);
    ex_reg_mem_val     := id_mem_val.toBool;
    ex_reg_valid       := id_reg_valid
    ex_reg_wen_pcr     := id_wen_pcr
    ex_reg_wen         := id_wen.toBool && id_waddr != UFix(0);
    ex_reg_fp_wen      := io.fpu.dec.wen;
    ex_reg_eret        := id_eret.toBool;
    ex_reg_inst_di     := (id_irq === I_DI);
    ex_reg_inst_ei     := (id_irq === I_EI);
    ex_reg_flush_inst  := (id_sync === SYNC_I);
    ex_reg_xcpt_ma_inst     := id_reg_xcpt_ma_inst;
    ex_reg_xcpt_itlb        := id_reg_xcpt_itlb;
    ex_reg_xcpt_illegal     := illegal_inst;
    ex_reg_xcpt_privileged  := (id_privileged & ~io.dpath.status(SR_S)).toBool;
    ex_reg_xcpt_syscall     := id_syscall.toBool;
    ex_reg_fp_val           := io.fpu.dec.valid
    ex_reg_fp_sboard_set    := io.fpu.dec.sboard
    ex_reg_vec_val          := id_vec_val.toBool
    ex_reg_replay           := id_reg_replay
    ex_reg_load_use         := id_load_use;
  }
  ex_reg_mem_cmd := id_mem_cmd
  ex_reg_mem_type := id_mem_type.toUFix

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
    (ex_reg_br_type === BR_J); // treat J/JAL like taken branches
  val jr_taken = ex_reg_br_type === BR_JR
  
  val mem_reg_div_mul_val = Reg(){Bool()};
  val mem_reg_eret        = Reg(){Bool()};
  val mem_reg_mem_val     = Reg(){Bool()};
  val mem_reg_mem_cmd     = Reg(){UFix(width = 4)};
  val mem_reg_mem_type    = Reg(){UFix(width = 3)};

  when (reset.toBool || io.dpath.killx) {
    mem_reg_valid       := Bool(false);
    mem_reg_wen_pcr     := Bool(false)
    mem_reg_div_mul_val := Bool(false);
    mem_reg_wen         := Bool(false);
    mem_reg_fp_wen      := Bool(false);
    mem_reg_eret        := Bool(false);
    mem_reg_mem_val     := Bool(false);
    mem_reg_inst_di     := Bool(false);
    mem_reg_inst_ei     := Bool(false);
    mem_reg_flush_inst  := Bool(false);
    mem_reg_xcpt_ma_inst     := Bool(false);
    mem_reg_xcpt_itlb        := Bool(false);
    mem_reg_xcpt_illegal     := Bool(false);
    mem_reg_xcpt_privileged  := Bool(false);
    mem_reg_xcpt_fpu         := Bool(false);
    mem_reg_xcpt_vec         := Bool(false);
    mem_reg_xcpt_syscall     := Bool(false);
    mem_reg_fp_val           := Bool(false);
    mem_reg_fp_sboard_set    := Bool(false)
  }
  .otherwise {
    mem_reg_valid       := ex_reg_valid
    mem_reg_wen_pcr     := ex_reg_wen_pcr
    mem_reg_div_mul_val := ex_reg_div_val || ex_reg_mul_val;
    mem_reg_wen         := ex_reg_wen;
    mem_reg_fp_wen      := ex_reg_fp_wen;
    mem_reg_eret        := ex_reg_eret;
    mem_reg_mem_val     := ex_reg_mem_val;
    mem_reg_inst_di     := ex_reg_inst_di;
    mem_reg_inst_ei     := ex_reg_inst_ei;
    mem_reg_flush_inst  := ex_reg_flush_inst;
    mem_reg_xcpt_ma_inst     := ex_reg_xcpt_ma_inst;
    mem_reg_xcpt_itlb        := ex_reg_xcpt_itlb;
    mem_reg_xcpt_illegal     := ex_reg_xcpt_illegal || ex_reg_fp_val && io.fpu.illegal_rm;
    mem_reg_xcpt_privileged  := ex_reg_xcpt_privileged;
    mem_reg_xcpt_fpu         := ex_reg_fp_val && !io.dpath.status(SR_EF).toBool;
    mem_reg_xcpt_vec         := ex_reg_vec_val && !io.dpath.status(SR_EV).toBool;
    mem_reg_xcpt_syscall     := ex_reg_xcpt_syscall;
    mem_reg_fp_val           := ex_reg_fp_val
    mem_reg_fp_sboard_set    := ex_reg_fp_sboard_set
  }
  mem_reg_mem_cmd     := ex_reg_mem_cmd;
  mem_reg_mem_type    := ex_reg_mem_type;

  when (io.dpath.killm) {
    wb_reg_valid       := Bool(false)
    wb_reg_wen_pcr     := Bool(false)
    wb_reg_wen         := Bool(false);
    wb_reg_fp_wen      := Bool(false);
    wb_reg_eret        := Bool(false);
    wb_reg_inst_di     := Bool(false);
    wb_reg_inst_ei     := Bool(false);
    wb_reg_flush_inst  := Bool(false);
    wb_reg_div_mul_val := Bool(false);
    wb_reg_fp_val      := Bool(false)
    wb_reg_fp_sboard_set := Bool(false)
  }
  .otherwise {
    wb_reg_valid       := mem_reg_valid
    wb_reg_wen_pcr     := mem_reg_wen_pcr
    wb_reg_wen         := mem_reg_wen;
    wb_reg_fp_wen      := mem_reg_fp_wen;
    wb_reg_eret        := mem_reg_eret;
    wb_reg_inst_di     := mem_reg_inst_di;
    wb_reg_inst_ei     := mem_reg_inst_ei;
    wb_reg_flush_inst  := mem_reg_flush_inst;
    wb_reg_div_mul_val := mem_reg_div_mul_val;
    wb_reg_fp_val      := mem_reg_fp_val
    wb_reg_fp_sboard_set := mem_reg_fp_sboard_set
  }

  val sboard = new rocketCtrlSboard(32, 3, 2);
  sboard.io.r(0).addr := id_raddr2.toUFix;
  sboard.io.r(1).addr := id_raddr1.toUFix;
  sboard.io.r(2).addr := id_waddr.toUFix;

  // scoreboard set (for D$ misses, div, mul)
  sboard.io.w(0).en := wb_reg_div_mul_val || wb_reg_dcache_miss && wb_reg_wen
  sboard.io.w(0).data := Bool(true)
  sboard.io.w(0).addr := io.dpath.wb_waddr

  sboard.io.w(1).en := io.dpath.sboard_clr
  sboard.io.w(1).data := Bool(false)
  sboard.io.w(1).addr := io.dpath.sboard_clra

  val id_stall_raddr2 = id_renx2.toBool && sboard.io.r(0).data
  val id_stall_raddr1 = id_renx1.toBool && sboard.io.r(1).data
  val id_stall_waddr  = id_wen.toBool && sboard.io.r(2).data

  var id_stall_fpu = Bool(false)
  if (HAVE_FPU) {
    val fp_sboard = new rocketCtrlSboard(32, 4, 3);
    fp_sboard.io.r(0).addr := id_raddr1.toUFix
    fp_sboard.io.r(1).addr := id_raddr2.toUFix
    fp_sboard.io.r(2).addr := id_raddr3.toUFix
    fp_sboard.io.r(3).addr := id_waddr.toUFix

    fp_sboard.io.w(0).en := wb_reg_dcache_miss && wb_reg_fp_wen || wb_reg_fp_sboard_set
    fp_sboard.io.w(0).data := Bool(true)
    fp_sboard.io.w(0).addr := io.dpath.wb_waddr

    fp_sboard.io.w(1).en := io.dpath.fp_sboard_clr
    fp_sboard.io.w(1).data := Bool(false)
    fp_sboard.io.w(1).addr := io.dpath.fp_sboard_clra

    fp_sboard.io.w(2).en := io.fpu.sboard_clr
    fp_sboard.io.w(2).data := Bool(false)
    fp_sboard.io.w(2).addr := io.fpu.sboard_clra

    id_stall_fpu = io.fpu.dec.ren1 && fp_sboard.io.r(0).data ||
                   io.fpu.dec.ren2 && fp_sboard.io.r(1).data ||
                   io.fpu.dec.ren3 && fp_sboard.io.r(2).data ||
                   io.fpu.dec.wen  && fp_sboard.io.r(3).data
  } 

  var vec_replay = Bool(false)
  var vec_stalld = Bool(false)
  var vec_irq = Bool(false)
  var vec_irq_cause = UFix(0,5)
  if (HAVE_VEC)
  {
    // vector control
    val vec = new rocketCtrlVec()

    io.vec_dpath <> vec.io.dpath
    io.vec_iface <> vec.io.iface

    vec.io.s := io.dpath.status(SR_S)
    vec.io.sr_ev := io.dpath.status(SR_EV)
    vec.io.exception := wb_reg_exception
    vec.io.eret := wb_reg_eret

    vec_replay = vec.io.replay
    vec_stalld = vec.io.stalld // || id_vfence_cv && !vec.io.vfence_ready
    vec_irq = vec.io.irq
    vec_irq_cause = vec.io.irq_cause
  }

  // exception handling
  // FIXME: verify PC in MEM stage points to valid, restartable instruction
  val p_irq_timer = (io.dpath.status(15).toBool && io.dpath.irq_timer);
  val p_irq_ipi   = (io.dpath.status(13).toBool && io.dpath.irq_ipi);
  val p_irq_vec   = (io.dpath.status(8) && vec_irq)
  val interrupt = 
    io.dpath.status(SR_ET).toBool && mem_reg_valid && 
    ((io.dpath.status(15).toBool && io.dpath.irq_timer) ||
     (io.dpath.status(13).toBool && io.dpath.irq_ipi) ||
     p_irq_vec);
     
  val interrupt_cause = 
    Mux(p_irq_ipi, UFix(21,5),
    Mux(p_irq_timer, UFix(23,5),
    Mux(p_irq_vec, vec_irq_cause,
        UFix(0,5))))

  val mem_xcpt_ma_ld = io.dmem.xcpt_ma_ld && !mem_reg_kill
  val mem_xcpt_ma_st = io.dmem.xcpt_ma_st && !mem_reg_kill
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
	  mem_reg_xcpt_vec || 
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
		Mux(mem_reg_xcpt_vec,         UFix(12,5), // vector disabled
			UFix(0,5))))))))))));  // instruction address misaligned

  // control transfer from ex/mem
  val take_pc_ex = ex_reg_btb_hit != br_taken || jr_taken
  val take_pc_wb = wb_reg_replay || vec_replay || wb_reg_exception || wb_reg_eret
  take_pc := take_pc_ex || take_pc_wb;

  // replay mem stage PC on a DTLB miss or a long-latency writeback
  val mem_ll_wb = io.dpath.mem_wb || io.dpath.mul_result_val || io.dpath.div_result_val
  val dmem_kill_mem = mem_reg_valid && (io.dtlb_miss || io.dmem.resp_nack)
  val replay_mem  = dmem_kill_mem || mem_reg_wen && mem_ll_wb || mem_reg_replay
  val kill_mem    = dmem_kill_mem || mem_reg_wen && mem_ll_wb || take_pc_wb || mem_exception || mem_reg_kill
  val kill_dcache = io.dtlb_miss  || mem_reg_wen && mem_ll_wb || take_pc_wb || mem_exception || mem_reg_kill
	
  // replay execute stage PC when the D$ is blocked, when the D$ misses, 
  // for privileged instructions, and for fence.i instructions
  val replay_ex    = wb_reg_dcache_miss && ex_reg_load_use || mem_reg_flush_inst || 
                     ex_reg_replay || ex_reg_mem_val && !(io.dmem.req_rdy && io.dtlb_rdy) ||
                     ex_reg_div_val && !io.dpath.div_rdy ||
                     ex_reg_mul_val && !io.dpath.mul_rdy ||
                     ex_reg_fp_val && io.fpu.nack
  val kill_ex      = take_pc_wb || replay_ex

  mem_reg_replay := replay_ex && !take_pc_wb;
  mem_reg_kill := kill_ex;

  wb_reg_replay       := replay_mem && !take_pc_wb
  wb_reg_exception    := mem_exception && !take_pc_wb;
  wb_reg_cause        := mem_cause;

  val replay_wb = wb_reg_replay || vec_replay

  val wb_badvaddr_wen = wb_reg_exception && ((wb_reg_cause === UFix(10)) || (wb_reg_cause === UFix(11)))

	// write cause to PCR on an exception
	io.dpath.exception    := wb_reg_exception;
	io.dpath.cause        := wb_reg_cause;
	io.dpath.badvaddr_wen := wb_badvaddr_wen;
  io.dpath.vec_irq_aux_wen := wb_reg_exception && wb_reg_cause >= UFix(24)

  io.dpath.sel_pc :=
    Mux(wb_reg_exception,               PC_EVEC, // exception
    Mux(replay_wb,                      PC_WB,   // replay
    Mux(wb_reg_eret,                    PC_PCR,  // eret instruction
    Mux(ex_reg_btb_hit && !br_taken,    PC_EX4,  // mispredicted not taken branch
    Mux(!ex_reg_btb_hit && br_taken,    PC_BR,   // mispredicted taken branch
    Mux(jr_taken,                       PC_JR,   // taken JALR
    Mux(io.dpath.btb_hit,               PC_BTB,  // predicted PC from BTB
        PC_4))))))); // PC+4

  io.dpath.wen_btb := !ex_reg_btb_hit && br_taken
  io.dpath.clr_btb := ex_reg_btb_hit && !br_taken || id_reg_icmiss;
  
  io.imem.req_val  := !reset.toBool && (take_pc_wb || !mem_reg_replay && !ex_reg_replay && (take_pc_ex || !id_reg_replay))

  // stall for RAW/WAW hazards on loads, AMOs, and mul/div in execute stage.
  val data_hazard_ex = ex_reg_wen &&
    (id_renx1.toBool && id_raddr1 === io.dpath.ex_waddr ||
     id_renx2.toBool && id_raddr2 === io.dpath.ex_waddr ||
     id_wen.toBool   && id_waddr  === io.dpath.ex_waddr)
  val fp_data_hazard_ex = ex_reg_fp_wen &&
    (io.fpu.dec.ren1 && id_raddr1 === io.dpath.ex_waddr ||
     io.fpu.dec.ren2 && id_raddr2 === io.dpath.ex_waddr ||
     io.fpu.dec.ren3 && id_raddr3 === io.dpath.ex_waddr ||
     io.fpu.dec.wen  && id_waddr  === io.dpath.ex_waddr)
  val id_ex_hazard = data_hazard_ex && (ex_reg_mem_val || ex_reg_div_val || ex_reg_mul_val || ex_reg_fp_val) ||
                     fp_data_hazard_ex && (ex_reg_mem_val || ex_reg_fp_val)
    
  // stall for RAW/WAW hazards on LB/LH and mul/div in memory stage.
  val mem_mem_cmd_bh = 
    (mem_reg_mem_type === MT_B)  || (mem_reg_mem_type === MT_BU) ||
    (mem_reg_mem_type === MT_H)  || (mem_reg_mem_type === MT_HU)
  val data_hazard_mem = mem_reg_wen &&
    (id_renx1.toBool && id_raddr1 === io.dpath.mem_waddr ||
     id_renx2.toBool && id_raddr2 === io.dpath.mem_waddr ||
     id_wen.toBool   && id_waddr  === io.dpath.mem_waddr)
  val fp_data_hazard_mem = mem_reg_fp_wen &&
    (io.fpu.dec.ren1 && id_raddr1 === io.dpath.mem_waddr ||
     io.fpu.dec.ren2 && id_raddr2 === io.dpath.mem_waddr ||
     io.fpu.dec.ren3 && id_raddr3 === io.dpath.mem_waddr ||
     io.fpu.dec.wen  && id_waddr  === io.dpath.mem_waddr)
  val id_mem_hazard = data_hazard_mem && (mem_reg_mem_val && mem_mem_cmd_bh || mem_reg_div_mul_val || mem_reg_fp_val) ||
                      fp_data_hazard_mem && mem_reg_fp_val
  id_load_use := mem_reg_mem_val && (data_hazard_mem || fp_data_hazard_mem)

  // stall for RAW/WAW hazards on load/AMO misses and mul/div in writeback.
  val data_hazard_wb = wb_reg_wen &&
    (id_renx1.toBool && id_raddr1 === io.dpath.wb_waddr ||
     id_renx2.toBool && id_raddr2 === io.dpath.wb_waddr ||
     id_wen.toBool   && id_waddr  === io.dpath.wb_waddr)
  val fp_data_hazard_wb = wb_reg_fp_wen &&
    (io.fpu.dec.ren1 && id_raddr1 === io.dpath.wb_waddr ||
     io.fpu.dec.ren2 && id_raddr2 === io.dpath.wb_waddr ||
     io.fpu.dec.ren3 && id_raddr3 === io.dpath.wb_waddr ||
     io.fpu.dec.wen  && id_waddr  === io.dpath.wb_waddr)
  val id_wb_hazard = data_hazard_wb && (wb_reg_dcache_miss || wb_reg_div_mul_val) ||
                     fp_data_hazard_wb && (wb_reg_dcache_miss || wb_reg_fp_val)

  val ctrl_stalld =
    !take_pc &&
    (
      id_ex_hazard || id_mem_hazard || id_wb_hazard ||
      id_stall_raddr1 || id_stall_raddr2 || id_stall_waddr ||
      id_stall_fpu ||
      id_mem_val.toBool && !(io.dmem.req_rdy && io.dtlb_rdy) ||
      id_vec_val.toBool && !(io.vec_iface.vcmdq_ready && io.vec_iface.vximm1q_ready && io.vec_iface.vximm2q_ready) || // being conservative
      ((id_sync === SYNC_D) || (id_sync === SYNC_I)) && !io.dmem.req_rdy ||
      vec_stalld
    );
  val ctrl_stallf = ctrl_stalld;
    
  val ctrl_killd = take_pc || ctrl_stalld;
  val ctrl_killf = take_pc || !io.imem.resp_val;
  
  io.dpath.flush_inst := wb_reg_flush_inst;
  io.dpath.stallf   := ctrl_stallf;
  io.dpath.stalld   := ctrl_stalld;
  io.dpath.killf    := ctrl_killf;
  io.dpath.killd    := ctrl_killd;
  io.dpath.killx    := kill_ex;
  io.dpath.killm    := kill_mem;

  io.dpath.mem_load := mem_reg_mem_val && mem_reg_wen
  io.dpath.ren2     := id_renx2.toBool;
  io.dpath.ren1     := id_renx1.toBool;
  io.dpath.sel_alu2 := id_sel_alu2
  io.dpath.fn_dw    := id_fn_dw.toBool;
  io.dpath.fn_alu   := id_fn_alu;
  io.dpath.div_fn   := id_div_fn;
  io.dpath.div_val  := id_div_val.toBool && id_waddr != UFix(0);
  io.dpath.mul_fn   := id_mul_fn;
  io.dpath.mul_val  := id_mul_val.toBool && id_waddr != UFix(0);
  io.dpath.ex_fp_val:= ex_reg_fp_val;
  io.dpath.mem_fp_val:= mem_reg_fp_val;
  io.dpath.ex_wen   := ex_reg_wen;
  io.dpath.mem_wen  := mem_reg_wen;
  io.dpath.wb_wen   := wb_reg_wen;
  io.dpath.wb_valid := wb_reg_valid;
  io.dpath.sel_wa   := id_sel_wa.toBool;
  io.dpath.sel_wb   := id_sel_wb;
  io.dpath.ren_pcr  := id_ren_pcr.toBool;
  io.dpath.wen_pcr  := wb_reg_wen_pcr
  io.dpath.id_eret  := id_eret.toBool;
  io.dpath.wb_eret  := wb_reg_eret;  
  io.dpath.irq_disable := wb_reg_inst_di;
  io.dpath.irq_enable  := wb_reg_inst_ei;
  io.dpath.ex_mem_type := ex_reg_mem_type

  io.fpu.valid := !io.dpath.killd && io.fpu.dec.valid
  io.fpu.killx := kill_ex
  io.fpu.killm := kill_mem

  io.dtlb_val         := ex_reg_mem_val
  io.dtlb_kill        := mem_reg_kill;
  io.dmem.req_val     := ex_reg_mem_val
  io.dmem.req_kill    := kill_dcache;
  io.dmem.req_cmd     := ex_reg_mem_cmd;
  io.dmem.req_type    := ex_reg_mem_type;
}
