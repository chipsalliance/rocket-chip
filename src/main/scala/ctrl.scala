package Top {

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
  val ex_wen   = Bool(OUTPUT);
  val mem_wen  = Bool(OUTPUT);
  val wb_wen   = Bool(OUTPUT);
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
  val mem_wb = Bool(INPUT);
  val ex_waddr = UFix(5,INPUT);  // write addr from execute stage
  val mem_waddr = UFix(5,INPUT); // write addr from memory stage
  val wb_waddr = UFix(5,INPUT);  // write addr from writeback stage
  val status  = Bits(17, INPUT);
  val sboard_clr  = Bool(INPUT);
  val sboard_clra = UFix(5, INPUT);
  val fp_sboard_clr  = Bool(INPUT);
  val fp_sboard_clra = UFix(5, INPUT);
  val mem_valid = Bool(INPUT); // high if there's a valid (not flushed) instruction in mem stage
  val irq_timer = Bool(INPUT);
  val irq_ipi   = Bool(INPUT);
}

class ioCtrlAll extends Bundle()
{
  val dpath   = new ioCtrlDpath();
  val console = new ioConsole(List("rdy"));
  val imem    = new ioImem(List("req_val", "resp_val")).flip();
  val dmem    = new ioDmem(List("req_val", "req_kill", "req_rdy", "req_cmd", "req_type", "resp_miss", "resp_nack")).flip();
  val vcmdq   = new io_vec_cmdq(List("ready", "valid"))
  val vximm1q = new io_vec_ximm1q(List("ready", "valid"))
  val vximm2q = new io_vec_ximm2q(List("ready", "valid"))
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

  val fpdec = new rocketFPUDecoder
  fpdec.io.inst := io.dpath.inst
  
  val xpr64 = Y;
  val cs =   
  ListLookup(io.dpath.inst,
               //                                                                                                                                      eret
               //                                                                                                                                      | syscall
               //                                                    mem_val             mul_val   div_val                    renpcr                   | | privileged
               //   val   brtype renx2 renx1 s_alu2   dw     alu     |   mem_cmd mem_type| mul_fn  | div_fn wen   s_wa  s_wb   |    wenpcr irq  sync   | | | replay_next
               //   |     |      |     |     |        |      |       |   |         |     | |       | |      |     |     |      |     |     |    |      | | | |
               List(N,    BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),Array(
    BNE->      List(Y,    BR_NE, REN_Y,REN_Y,A2_BTYPE,DW_X,  FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    BEQ->      List(Y,    BR_EQ, REN_Y,REN_Y,A2_BTYPE,DW_X,  FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    BLT->      List(Y,    BR_LT, REN_Y,REN_Y,A2_BTYPE,DW_X,  FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    BLTU->     List(Y,    BR_LTU,REN_Y,REN_Y,A2_BTYPE,DW_X,  FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    BGE->      List(Y,    BR_GE, REN_Y,REN_Y,A2_BTYPE,DW_X,  FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    BGEU->     List(Y,    BR_GEU,REN_Y,REN_Y,A2_BTYPE,DW_X,  FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),

    J->        List(Y,    BR_J,  REN_N,REN_N,A2_JTYPE,DW_X,  FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    JAL->      List(Y,    BR_J,  REN_N,REN_N,A2_JTYPE,DW_X,  FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RA,WB_PC, REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    JALR_C->   List(Y,    BR_JR, REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_PC, REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    JALR_J->   List(Y,    BR_JR, REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_PC, REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    JALR_R->   List(Y,    BR_JR, REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_PC, REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    RDNPC->    List(Y,    BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_PC, REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),

    LB->       List(Y,    BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_ADD, M_Y,M_XRD,    MT_B, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    LH->       List(Y,    BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_ADD, M_Y,M_XRD,    MT_H, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    LW->       List(Y,    BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_ADD, M_Y,M_XRD,    MT_W, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    LD->       List(xpr64,BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_ADD, M_Y,M_XRD,    MT_D, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    LBU->      List(Y,    BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_ADD, M_Y,M_XRD,    MT_BU,N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    LHU->      List(Y,    BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_ADD, M_Y,M_XRD,    MT_HU,N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    LWU->      List(xpr64,BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_ADD, M_Y,M_XRD,    MT_WU,N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    SB->       List(Y,    BR_N,  REN_Y,REN_Y,A2_BTYPE,DW_XPR,FN_ADD, M_Y,M_XWR,    MT_B, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    SH->       List(Y,    BR_N,  REN_Y,REN_Y,A2_BTYPE,DW_XPR,FN_ADD, M_Y,M_XWR,    MT_H, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    SW->       List(Y,    BR_N,  REN_Y,REN_Y,A2_BTYPE,DW_XPR,FN_ADD, M_Y,M_XWR,    MT_W, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    SD->       List(xpr64,BR_N,  REN_Y,REN_Y,A2_BTYPE,DW_XPR,FN_ADD, M_Y,M_XWR,    MT_D, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),

    AMOADD_W-> List(Y,    BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_Y,M_XA_ADD, MT_W, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    AMOSWAP_W->List(Y,    BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_Y,M_XA_SWAP,MT_W, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    AMOAND_W-> List(Y,    BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_Y,M_XA_AND, MT_W, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    AMOOR_W->  List(Y,    BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_Y,M_XA_OR,  MT_W, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    AMOMIN_W-> List(Y,    BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_Y,M_XA_MIN, MT_W, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    AMOMINU_W->List(Y,    BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_Y,M_XA_MINU,MT_W, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    AMOMAX_W-> List(Y,    BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_Y,M_XA_MAX, MT_W, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    AMOMAXU_W->List(Y,    BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_Y,M_XA_MAXU,MT_W, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    AMOADD_D-> List(xpr64,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_Y,M_XA_ADD, MT_D, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    AMOSWAP_D->List(xpr64,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_Y,M_XA_SWAP,MT_D, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    AMOAND_D-> List(xpr64,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_Y,M_XA_AND, MT_D, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    AMOOR_D->  List(xpr64,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_Y,M_XA_OR,  MT_D, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    AMOMIN_D-> List(xpr64,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_Y,M_XA_MIN, MT_D, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    AMOMINU_D->List(xpr64,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_Y,M_XA_MINU,MT_D, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    AMOMAX_D-> List(xpr64,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_Y,M_XA_MAX, MT_D, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    AMOMAXU_D->List(xpr64,BR_N,  REN_Y,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_Y,M_XA_MAXU,MT_D, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),

    LUI->      List(Y,    BR_N,  REN_N,REN_N,A2_LTYPE,DW_XPR,FN_OP2, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    ADDI->     List(Y,    BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    SLTI ->    List(Y,    BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_SLT, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    SLTIU->    List(Y,    BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_SLTU,M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    ANDI->     List(Y,    BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_AND, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    ORI->      List(Y,    BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_OR,  M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    XORI->     List(Y,    BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_XOR, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    SLLI->     List(Y,    BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_SL,  M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    SRLI->     List(Y_SH, BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_SR,  M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    SRAI->     List(Y_SH, BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_SRA, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    ADD->      List(Y,    BR_N,  REN_Y,REN_Y,A2_RTYPE,DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    SUB->      List(Y,    BR_N,  REN_Y,REN_Y,A2_RTYPE,DW_XPR,FN_SUB, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    SLT->      List(Y,    BR_N,  REN_Y,REN_Y,A2_RTYPE,DW_XPR,FN_SLT, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    SLTU->     List(Y,    BR_N,  REN_Y,REN_Y,A2_RTYPE,DW_XPR,FN_SLTU,M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    riscvAND-> List(Y,    BR_N,  REN_Y,REN_Y,A2_RTYPE,DW_XPR,FN_AND, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    riscvOR->  List(Y,    BR_N,  REN_Y,REN_Y,A2_RTYPE,DW_XPR,FN_OR,  M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    riscvXOR-> List(Y,    BR_N,  REN_Y,REN_Y,A2_RTYPE,DW_XPR,FN_XOR, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    SLL->      List(Y,    BR_N,  REN_Y,REN_Y,A2_RTYPE,DW_XPR,FN_SL,  M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    SRL->      List(Y,    BR_N,  REN_Y,REN_Y,A2_RTYPE,DW_XPR,FN_SR,  M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    SRA->      List(Y,    BR_N,  REN_Y,REN_Y,A2_RTYPE,DW_XPR,FN_SRA, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),

    ADDIW->    List(xpr64,BR_N,  REN_N,REN_Y,A2_ITYPE,DW_32,FN_ADD,  M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),   
    SLLIW->    List(xpr64,BR_N,  REN_N,REN_Y,A2_ITYPE,DW_32,FN_SL,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    SRLIW->    List(xpr64,BR_N,  REN_N,REN_Y,A2_ITYPE,DW_32,FN_SR,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    SRAIW->    List(xpr64,BR_N,  REN_N,REN_Y,A2_ITYPE,DW_32,FN_SRA,  M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    ADDW->     List(xpr64,BR_N,  REN_Y,REN_Y,A2_RTYPE,DW_32,FN_ADD,  M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    SUBW->     List(xpr64,BR_N,  REN_Y,REN_Y,A2_RTYPE,DW_32,FN_SUB,  M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    SLLW->     List(xpr64,BR_N,  REN_Y,REN_Y,A2_RTYPE,DW_32,FN_SL,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    SRLW->     List(xpr64,BR_N,  REN_Y,REN_Y,A2_RTYPE,DW_32,FN_SR,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    SRAW->     List(xpr64,BR_N,  REN_Y,REN_Y,A2_RTYPE,DW_32,FN_SRA,  M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),

    MUL->      List(Y,    BR_N,  REN_Y,REN_Y,A2_X,    DW_XPR,FN_X,   M_N,M_X,      MT_X, Y,MUL_LO, N,DIV_X, WEN_Y,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    MULH->     List(Y,    BR_N,  REN_Y,REN_Y,A2_X,    DW_XPR,FN_X,   M_N,M_X,      MT_X, Y,MUL_HS, N,DIV_X, WEN_Y,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    MULHU->    List(Y,    BR_N,  REN_Y,REN_Y,A2_X,    DW_XPR,FN_X,   M_N,M_X,      MT_X, Y,MUL_HU, N,DIV_X, WEN_Y,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    MULHSU->   List(Y,    BR_N,  REN_Y,REN_Y,A2_X,    DW_XPR,FN_X,   M_N,M_X,      MT_X, Y,MUL_HSU,N,DIV_X, WEN_Y,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    MULW->     List(xpr64,BR_N,  REN_Y,REN_Y,A2_X,    DW_32, FN_X,   M_N,M_X,      MT_X, Y,MUL_LO, N,DIV_X, WEN_Y,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),

    DIV->      List(Y,    BR_N,  REN_Y,REN_Y,A2_X,    DW_XPR,FN_X,   M_N,M_X,      MT_X, N,MUL_X,  Y,DIV_D, WEN_Y,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    DIVU->     List(Y,    BR_N,  REN_Y,REN_Y,A2_X,    DW_XPR,FN_X,   M_N,M_X,      MT_X, N,MUL_X,  Y,DIV_DU,WEN_Y,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    REM->      List(Y,    BR_N,  REN_Y,REN_Y,A2_X,    DW_XPR,FN_X,   M_N,M_X,      MT_X, N,MUL_X,  Y,DIV_R, WEN_Y,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    REMU->     List(Y,    BR_N,  REN_Y,REN_Y,A2_X,    DW_XPR,FN_X,   M_N,M_X,      MT_X, N,MUL_X,  Y,DIV_RU,WEN_Y,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    DIVW->     List(xpr64,BR_N,  REN_Y,REN_Y,A2_X,    DW_32, FN_X,   M_N,M_X,      MT_X, N,MUL_X,  Y,DIV_D, WEN_Y,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    DIVUW->    List(xpr64,BR_N,  REN_Y,REN_Y,A2_X,    DW_32, FN_X,   M_N,M_X,      MT_X, N,MUL_X,  Y,DIV_DU,WEN_Y,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    REMW->     List(xpr64,BR_N,  REN_Y,REN_Y,A2_X,    DW_32, FN_X,   M_N,M_X,      MT_X, N,MUL_X,  Y,DIV_R, WEN_Y,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    REMUW->    List(xpr64,BR_N,  REN_Y,REN_Y,A2_X,    DW_32, FN_X,   M_N,M_X,      MT_X, N,MUL_X,  Y,DIV_RU,WEN_Y,WA_RD,WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),

    SYSCALL->  List(Y,    BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,Y,N,N),
    EI->       List(Y,    BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_EI,SYNC_N,N,N,Y,Y),
    DI->       List(Y,    BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_DI,SYNC_N,N,N,Y,Y),
    ERET->     List(Y,    BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_PCR,REN_N,WEN_N,I_X ,SYNC_N,Y,N,Y,N),
    FENCE->    List(Y,    BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_Y,M_FENCE,  MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_D,N,N,N,N),
    FENCE_I->  List(Y,    BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_Y,M_FLA,    MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_I,N,N,N,N),
    CFLUSH->   List(Y,    BR_N,  REN_Y,REN_N,A2_X,    DW_X,  FN_X,   M_Y,M_FLA,    MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,Y,Y),
    MFPCR->    List(Y,    BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_PCR,REN_Y,WEN_N,I_X ,SYNC_N,N,N,Y,N),
    MTPCR->    List(Y,    BR_N,  REN_N,REN_Y,A2_ZERO, DW_XPR,FN_ADD, M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_ALU,REN_N,WEN_Y,I_X ,SYNC_N,N,N,Y,Y),
    RDTIME->   List(Y,    BR_N,  REN_N,REN_N,A2_X,    DW_XPR,FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_TSC,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    RDCYCLE->  List(Y,    BR_N,  REN_N,REN_N,A2_X,    DW_XPR,FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_TSC,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    RDINSTRET->List(Y,    BR_N,  REN_N,REN_N,A2_X,    DW_XPR,FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_Y,WA_RD,WB_IRT,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    
    // Instructions that have not yet been implemented
    // Faking these for now so akaros will boot    
    //MFFSR->    List(Y,    BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    //MTFSR->    List(Y,    BR_N,  REN_N,REN_N,A2_X,    DW_X,  FN_X,   M_N,M_X,      MT_X, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_X,  REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    FLW->      List(Y,    BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_ADD, M_Y,M_XRD,    MT_W, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    FLD->      List(Y,    BR_N,  REN_N,REN_Y,A2_ITYPE,DW_XPR,FN_ADD, M_Y,M_XRD,    MT_D, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    FSW->      List(Y,    BR_N,  REN_N,REN_Y,A2_BTYPE,DW_XPR,FN_ADD, M_Y,M_XWR,    MT_W, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N),
    FSD->      List(Y,    BR_N,  REN_N,REN_Y,A2_BTYPE,DW_XPR,FN_ADD, M_Y,M_XWR,    MT_D, N,MUL_X,  N,DIV_X, WEN_N,WA_X, WB_ALU,REN_N,WEN_N,I_X ,SYNC_N,N,N,N,N)
  ))

  val id_int_val :: id_br_type :: id_renx2 :: id_renx1 :: id_sel_alu2 :: id_fn_dw :: id_fn_alu :: cs0 = cs 
  val id_mem_val :: id_mem_cmd :: id_mem_type :: id_mul_val :: id_mul_fn :: id_div_val :: id_div_fn :: id_wen :: id_sel_wa :: id_sel_wb :: cs1 = cs0
  val id_ren_pcr :: id_wen_pcr :: id_irq :: id_sync :: id_eret :: id_syscall :: id_privileged :: id_replay_next :: Nil = cs1

  val veccs =
  ListLookup(io.dpath.inst,
                //                                           appvlmask
                //                                           | vcmdq
                //                                           | | vximm1q
                //                                           | | | vximm2q
                // val ren2  ren1  vcmd    vimm      fn      | | | | vackq
                //   | |     |     |       |         |       | | | | |
                List(N,REN_N,REN_N,VCMD_X, VIMM_X,   VEC_X  ,N,N,N,N,N),Array(
    VVCFGIVL->  List(Y,REN_N,REN_Y,VCMD_I, VIMM_VLEN,VEC_CFG,N,Y,Y,N,N),
    VSETVL->    List(Y,REN_N,REN_Y,VCMD_I, VIMM_VLEN,VEC_VL ,N,Y,Y,N,N),
    VF->        List(Y,REN_Y,REN_Y,VCMD_I, VIMM_ALU, VEC_X  ,Y,Y,Y,N,N),
    VMVV->      List(Y,REN_N,REN_N,VCMD_TX,VIMM_X,   VEC_X  ,Y,Y,N,N,N),
    VMSV->      List(Y,REN_N,REN_Y,VCMD_TX,VIMM_RS1, VEC_X  ,Y,Y,Y,N,N),
    VFMVV->     List(Y,REN_N,REN_N,VCMD_TF,VIMM_X,   VEC_X  ,Y,Y,N,N,N),
    FENCE_L_V-> List(Y,REN_N,REN_N,VCMD_F, VIMM_X,   VEC_X  ,N,Y,N,N,N),
    FENCE_G_V-> List(Y,REN_N,REN_N,VCMD_F, VIMM_X,   VEC_X  ,N,Y,N,N,N),
    FENCE_L_CV->List(Y,REN_N,REN_N,VCMD_F, VIMM_X,   VEC_X  ,N,Y,N,N,Y),
    FENCE_G_CV->List(Y,REN_N,REN_N,VCMD_F, VIMM_X,   VEC_X  ,N,Y,N,N,Y),
    VLD->       List(Y,REN_N,REN_Y,VCMD_MX,VIMM_RS1, VEC_X  ,Y,Y,Y,N,N),
    VLW->       List(Y,REN_N,REN_Y,VCMD_MX,VIMM_RS1, VEC_X  ,Y,Y,Y,N,N),
    VLWU->      List(Y,REN_N,REN_Y,VCMD_MX,VIMM_RS1, VEC_X  ,Y,Y,Y,N,N),
    VLH->       List(Y,REN_N,REN_Y,VCMD_MX,VIMM_RS1, VEC_X  ,Y,Y,Y,N,N),
    VLHU->      List(Y,REN_N,REN_Y,VCMD_MX,VIMM_RS1, VEC_X  ,Y,Y,Y,N,N),
    VLB->       List(Y,REN_N,REN_Y,VCMD_MX,VIMM_RS1, VEC_X  ,Y,Y,Y,N,N),
    VLBU->      List(Y,REN_N,REN_Y,VCMD_MX,VIMM_RS1, VEC_X  ,Y,Y,Y,N,N),
    VSD->       List(Y,REN_N,REN_Y,VCMD_MX,VIMM_RS1, VEC_X  ,Y,Y,Y,N,N),
    VSW->       List(Y,REN_N,REN_Y,VCMD_MX,VIMM_RS1, VEC_X  ,Y,Y,Y,N,N),
    VSH->       List(Y,REN_N,REN_Y,VCMD_MX,VIMM_RS1, VEC_X  ,Y,Y,Y,N,N),
    VSB->       List(Y,REN_N,REN_Y,VCMD_MX,VIMM_RS1, VEC_X  ,Y,Y,Y,N,N),
    VFLD->      List(Y,REN_N,REN_Y,VCMD_MF,VIMM_RS1, VEC_X  ,Y,Y,Y,N,N),
    VFLW->      List(Y,REN_N,REN_Y,VCMD_MF,VIMM_RS1, VEC_X  ,Y,Y,Y,N,N),
    VFSD->      List(Y,REN_N,REN_Y,VCMD_MF,VIMM_RS1, VEC_X  ,Y,Y,Y,N,N),
    VFSW->      List(Y,REN_N,REN_Y,VCMD_MF,VIMM_RS1, VEC_X  ,Y,Y,Y,N,N),
    VLSTD->     List(Y,REN_Y,REN_Y,VCMD_MX,VIMM_RS1, VEC_X  ,Y,Y,Y,Y,N),
    VLSTW->     List(Y,REN_Y,REN_Y,VCMD_MX,VIMM_RS1, VEC_X  ,Y,Y,Y,Y,N),
    VLSTWU->    List(Y,REN_Y,REN_Y,VCMD_MX,VIMM_RS1, VEC_X  ,Y,Y,Y,Y,N),
    VLSTH->     List(Y,REN_Y,REN_Y,VCMD_MX,VIMM_RS1, VEC_X  ,Y,Y,Y,Y,N),
    VLSTHU->    List(Y,REN_Y,REN_Y,VCMD_MX,VIMM_RS1, VEC_X  ,Y,Y,Y,Y,N),
    VLSTB->     List(Y,REN_Y,REN_Y,VCMD_MX,VIMM_RS1, VEC_X  ,Y,Y,Y,Y,N),
    VLSTBU->    List(Y,REN_Y,REN_Y,VCMD_MX,VIMM_RS1, VEC_X  ,Y,Y,Y,Y,N),
    VSSTD->     List(Y,REN_Y,REN_Y,VCMD_MX,VIMM_RS1, VEC_X  ,Y,Y,Y,Y,N),
    VSSTW->     List(Y,REN_Y,REN_Y,VCMD_MX,VIMM_RS1, VEC_X  ,Y,Y,Y,Y,N),
    VSSTH->     List(Y,REN_Y,REN_Y,VCMD_MX,VIMM_RS1, VEC_X  ,Y,Y,Y,Y,N),
    VSSTB->     List(Y,REN_Y,REN_Y,VCMD_MX,VIMM_RS1, VEC_X  ,Y,Y,Y,Y,N),
    VFLSTD->    List(Y,REN_Y,REN_Y,VCMD_MF,VIMM_RS1, VEC_X  ,Y,Y,Y,Y,N),
    VFLSTW->    List(Y,REN_Y,REN_Y,VCMD_MF,VIMM_RS1, VEC_X  ,Y,Y,Y,Y,N),
    VFSSTD->    List(Y,REN_Y,REN_Y,VCMD_MF,VIMM_RS1, VEC_X  ,Y,Y,Y,Y,N),
    VFSSTW->    List(Y,REN_Y,REN_Y,VCMD_MF,VIMM_RS1, VEC_X  ,Y,Y,Y,Y,N)
  ))

  val id_vec_val :: id_renv2 :: id_renv1 :: id_sel_vcmd :: id_sel_vimm :: id_fn_vec :: id_vec_appvlmask :: veccs0 = veccs
  val id_vec_cmdq_val :: id_vec_ximm1q_val :: id_vec_ximm2q_val :: id_vec_ackq_wait :: Nil = veccs0

  val if_reg_xcpt_ma_inst = Reg(io.dpath.xcpt_ma_inst, resetVal = Bool(false));

  val id_raddr3 = io.dpath.inst(16,12);
  val id_raddr2 = io.dpath.inst(21,17);
  val id_raddr1 = io.dpath.inst(26,22);
  val id_waddr  = Mux(id_sel_wa === WA_RA, RA, io.dpath.inst(31,27));

  val id_console_out_val  = id_wen_pcr.toBool && (id_raddr2 === PCR_CONSOLE);

  val wb_reg_div_mul_val = Reg(resetVal = Bool(false))
  val wb_reg_dcache_miss = Reg(io.dmem.resp_miss, resetVal = Bool(false));

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
  val ex_reg_wen         = Reg(resetVal = Bool(false));
  val ex_reg_fp_wen      = Reg(resetVal = Bool(false));
  val ex_reg_eret        = Reg(resetVal = Bool(false));
  val ex_reg_replay_next = Reg(resetVal = Bool(false));
  val ex_reg_inst_di          = Reg(resetVal = Bool(false));
  val ex_reg_inst_ei          = Reg(resetVal = Bool(false));
  val ex_reg_flush_inst  = Reg(resetVal = Bool(false));
  val ex_reg_xcpt_ma_inst    = Reg(resetVal = Bool(false));
  val ex_reg_xcpt_itlb       = Reg(resetVal = Bool(false));
  val ex_reg_xcpt_illegal    = Reg(resetVal = Bool(false));
  val ex_reg_xcpt_privileged = Reg(resetVal = Bool(false));
  val ex_reg_xcpt_syscall    = Reg(resetVal = Bool(false));
  val ex_reg_fp_val          = Reg(resetVal = Bool(false));
  val ex_reg_replay          = Reg(resetVal = Bool(false));
  val ex_reg_load_use       = Reg(resetVal = Bool(false));

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
  val mem_reg_xcpt_syscall    = Reg(resetVal = Bool(false));
  val mem_reg_replay          = Reg(resetVal = Bool(false));
  val mem_reg_kill            = Reg(resetVal = Bool(false));

  val wb_reg_wen             = Reg(resetVal = Bool(false));
  val wb_reg_fp_wen          = Reg(resetVal = Bool(false));
  val wb_reg_inst_di         = Reg(resetVal = Bool(false));
  val wb_reg_inst_ei         = Reg(resetVal = Bool(false));
  val wb_reg_flush_inst      = Reg(resetVal = Bool(false));
  val wb_reg_eret            = Reg(resetVal = Bool(false));
  val wb_reg_exception       = Reg(resetVal = Bool(false));
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
   val illegal_inst = !(id_int_val.toBool || fpdec.io.valid) || (id_eret.toBool && io.dpath.status(SR_ET).toBool);
  
  when (reset.toBool || io.dpath.killd) {
    ex_reg_br_type     <== BR_N;
    ex_reg_btb_hit     <== Bool(false);
    ex_reg_div_val <== Bool(false);
    ex_reg_mul_val <== Bool(false);
    ex_reg_mem_val     <== Bool(false);
    ex_reg_wen         <== Bool(false);
    ex_reg_fp_wen      <== Bool(false);
    ex_reg_eret        <== Bool(false);
    ex_reg_replay_next <== Bool(false);
    ex_reg_inst_di     <== Bool(false);
    ex_reg_inst_ei     <== Bool(false);
    ex_reg_flush_inst  <== Bool(false);  
    ex_reg_xcpt_ma_inst     <== Bool(false);
    ex_reg_xcpt_itlb        <== Bool(false);
    ex_reg_xcpt_illegal     <== Bool(false);
    ex_reg_xcpt_privileged  <== Bool(false);
    ex_reg_xcpt_syscall     <== Bool(false);
    ex_reg_fp_val           <== Bool(false);
    ex_reg_replay           <== Bool(false);
    ex_reg_load_use         <== Bool(false);
  } 
  otherwise {
    ex_reg_br_type     <== id_br_type;
    ex_reg_btb_hit     <== id_reg_btb_hit;
    ex_reg_div_val     <== id_div_val.toBool && id_waddr != UFix(0);
    ex_reg_mul_val     <== id_mul_val.toBool && id_waddr != UFix(0);
    ex_reg_mem_val     <== id_mem_val.toBool;
    ex_reg_wen         <== id_wen.toBool && id_waddr != UFix(0);
    ex_reg_fp_wen      <== fpdec.io.wen;
    ex_reg_eret        <== id_eret.toBool;
    ex_reg_replay_next <== id_replay_next.toBool;
    ex_reg_inst_di     <== (id_irq === I_DI);
    ex_reg_inst_ei     <== (id_irq === I_EI);
    ex_reg_flush_inst  <== (id_sync === SYNC_I);
    ex_reg_xcpt_ma_inst     <== id_reg_xcpt_ma_inst;
    ex_reg_xcpt_itlb        <== id_reg_xcpt_itlb;
    ex_reg_xcpt_illegal     <== illegal_inst;
    ex_reg_xcpt_privileged  <== (id_privileged & ~io.dpath.status(SR_S)).toBool;
    ex_reg_xcpt_syscall     <== id_syscall.toBool;
    ex_reg_fp_val           <== fpdec.io.valid;
    ex_reg_replay           <== id_reg_replay || ex_reg_replay_next;
    ex_reg_load_use         <== id_load_use;
  }
  ex_reg_mem_cmd     <== id_mem_cmd;
  ex_reg_mem_type    <== id_mem_type;

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
    (ex_reg_br_type === BR_J) |
    (ex_reg_br_type === BR_JR); // treat J/JAL/JALR like a taken branch
  
  val mem_reg_div_mul_val = Reg(){Bool()};
  val mem_reg_eret        = Reg(){Bool()};
  val mem_reg_mem_val     = Reg(){Bool()};
  val mem_reg_mem_cmd     = Reg(){UFix(width = 4)};
  val mem_reg_mem_type    = Reg(){UFix(width = 3)};

  when (reset.toBool || io.dpath.killx) {
    mem_reg_div_mul_val <== Bool(false);
    mem_reg_wen         <== Bool(false);
    mem_reg_fp_wen      <== Bool(false);
    mem_reg_eret        <== Bool(false);
    mem_reg_mem_val     <== Bool(false);
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
    mem_reg_div_mul_val <== ex_reg_div_val || ex_reg_mul_val;
    mem_reg_wen         <== ex_reg_wen;
    mem_reg_fp_wen      <== ex_reg_fp_wen;
    mem_reg_eret        <== ex_reg_eret;
    mem_reg_mem_val     <== ex_reg_mem_val;
    mem_reg_inst_di     <== ex_reg_inst_di;
    mem_reg_inst_ei     <== ex_reg_inst_ei;
    mem_reg_flush_inst  <== ex_reg_flush_inst;
    mem_reg_xcpt_ma_inst     <== ex_reg_xcpt_ma_inst;
    mem_reg_xcpt_itlb        <== ex_reg_xcpt_itlb;
    mem_reg_xcpt_illegal     <== ex_reg_xcpt_illegal;
    mem_reg_xcpt_privileged  <== ex_reg_xcpt_privileged;
    mem_reg_xcpt_fpu         <== ex_reg_fp_val && !io.dpath.status(SR_EF).toBool;
    mem_reg_xcpt_syscall     <== ex_reg_xcpt_syscall;
  }
  mem_reg_mem_cmd     <== ex_reg_mem_cmd;
  mem_reg_mem_type    <== ex_reg_mem_type;

  when (io.dpath.killm) {
    wb_reg_wen         <== Bool(false);
    wb_reg_fp_wen      <== Bool(false);
    wb_reg_eret        <== Bool(false);
    wb_reg_inst_di     <== Bool(false);
    wb_reg_inst_ei     <== Bool(false);
    wb_reg_flush_inst  <== Bool(false);
    wb_reg_div_mul_val <== Bool(false);
  }
  otherwise {
    wb_reg_wen         <== mem_reg_wen;
    wb_reg_fp_wen      <== mem_reg_fp_wen;
    wb_reg_eret        <== mem_reg_eret;
    wb_reg_inst_di     <== mem_reg_inst_di;
    wb_reg_inst_ei     <== mem_reg_inst_ei;
    wb_reg_flush_inst  <== mem_reg_flush_inst;
    wb_reg_div_mul_val <== mem_reg_div_mul_val;
  }

  val sboard = new rocketCtrlSboard(); 
  sboard.io.raddra  := id_raddr2.toUFix;
  sboard.io.raddrb  := id_raddr1.toUFix;
  sboard.io.raddrc  := id_waddr.toUFix;

  // scoreboard set (for D$ misses, div, mul)
  sboard.io.set     := wb_reg_div_mul_val || wb_reg_dcache_miss && wb_reg_wen;
  sboard.io.seta    := io.dpath.wb_waddr;

  sboard.io.clr    := io.dpath.sboard_clr;
  sboard.io.clra   := io.dpath.sboard_clra;

  val id_stall_raddr2 = id_renx2.toBool && sboard.io.stalla;
  val id_stall_raddr1 = id_renx1.toBool && sboard.io.stallb;
  val id_stall_waddr  = id_wen.toBool && sboard.io.stallc;

  var id_stall_fpu = Bool(false)
  if (HAVE_FPU) {
    val fp_sboard = new rocketCtrlSboard(); 
    fp_sboard.io.raddra  := id_raddr1.toUFix;
    fp_sboard.io.raddrb  := id_raddr2.toUFix;
    fp_sboard.io.raddrc  := id_raddr3.toUFix;
    fp_sboard.io.raddrd  := id_waddr.toUFix;

    fp_sboard.io.set     := wb_reg_dcache_miss && wb_reg_fp_wen;
    fp_sboard.io.seta    := io.dpath.wb_waddr;

    fp_sboard.io.clr    := io.dpath.fp_sboard_clr;
    fp_sboard.io.clra   := io.dpath.fp_sboard_clra;

    id_stall_fpu = fpdec.io.ren1 && fp_sboard.io.stalla ||
                   fpdec.io.ren2 && fp_sboard.io.stallb ||
                   fpdec.io.ren3 && fp_sboard.io.stallc ||
                   fpdec.io.wen  && fp_sboard.io.stalld
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

  // control transfer from ex/mem
  val ex_btb_match = ex_reg_btb_hit && io.dpath.btb_match
  val take_pc_ex = !ex_btb_match && br_taken || ex_reg_btb_hit && !br_taken
  val take_pc_wb = wb_reg_replay || wb_reg_exception || wb_reg_eret;
  take_pc <== take_pc_ex || take_pc_wb;

  // replay mem stage PC on a DTLB miss or a long-latency writeback
  val mem_ll_wb = io.dpath.mem_wb || io.dpath.mul_result_val || io.dpath.div_result_val
  val replay_mem  = io.dtlb_miss || mem_reg_wen && mem_ll_wb || io.dmem.resp_nack || mem_reg_replay
  val kill_mem    = io.dtlb_miss || mem_reg_wen && mem_ll_wb || io.dmem.resp_nack || take_pc_wb || mem_exception || mem_reg_kill
  val kill_dcache = io.dtlb_miss || mem_reg_wen && mem_ll_wb ||                      take_pc_wb || mem_exception || mem_reg_kill
	
  // replay execute stage PC when the D$ is blocked, when the D$ misses, 
  // for privileged instructions, and for fence.i instructions
  val replay_ex    = wb_reg_dcache_miss && ex_reg_load_use || mem_reg_flush_inst || 
                     ex_reg_replay || ex_reg_mem_val && !(io.dmem.req_rdy && io.dtlb_rdy) ||
                     ex_reg_div_val && !io.dpath.div_rdy ||
                     ex_reg_mul_val && !io.dpath.mul_rdy
  val kill_ex      = take_pc_wb || replay_ex

  mem_reg_replay <== replay_ex && !take_pc_wb;
  mem_reg_kill <== kill_ex;

  wb_reg_replay       <== replay_mem && !take_pc_wb;
  wb_reg_exception    <== mem_exception && !take_pc_wb;
  wb_reg_cause        <== mem_cause;

  val wb_badvaddr_wen = wb_reg_exception && ((wb_reg_cause === UFix(10)) || (wb_reg_cause === UFix(11)))

	// write cause to PCR on an exception
	io.dpath.exception    := wb_reg_exception;
	io.dpath.cause        := wb_reg_cause;
	io.dpath.badvaddr_wen := wb_badvaddr_wen;

  io.dpath.sel_pc :=
    Mux(wb_reg_exception,               PC_EVEC, // exception
    Mux(wb_reg_replay,                  PC_WB,   // replay
    Mux(wb_reg_eret,                    PC_PCR,  // eret instruction
    Mux(ex_reg_btb_hit && !br_taken,    PC_EX4,  // mispredicted not taken branch
    Mux(!ex_btb_match && br_taken,      PC_BR,   // mispredicted taken branch
    Mux(io.dpath.btb_hit,               PC_BTB,  // predicted PC from BTB
        PC_4)))))); // PC+4

  io.dpath.wen_btb := !ex_btb_match && br_taken;
  io.dpath.clr_btb := ex_reg_btb_hit && !br_taken || id_reg_icmiss;
  
  io.imem.req_val  := take_pc_wb || !mem_reg_replay && !ex_reg_replay && (take_pc_ex || !id_reg_replay)

  // stall for RAW/WAW hazards on loads, AMOs, and mul/div in execute stage.
  val data_hazard_ex = ex_reg_wen &&
    (id_renx1.toBool && id_raddr1 === io.dpath.ex_waddr ||
     id_renx2.toBool && id_raddr2 === io.dpath.ex_waddr ||
     id_wen.toBool   && id_waddr  === io.dpath.ex_waddr)
  val fp_data_hazard_ex = ex_reg_fp_wen &&
    (fpdec.io.ren1 && id_raddr1 === io.dpath.ex_waddr ||
     fpdec.io.ren2 && id_raddr2 === io.dpath.ex_waddr ||
     fpdec.io.ren3 && id_raddr3 === io.dpath.ex_waddr ||
     fpdec.io.wen  && id_waddr  === io.dpath.ex_waddr)
  val id_ex_hazard = data_hazard_ex && (ex_reg_mem_val || ex_reg_div_val || ex_reg_mul_val) ||
                     fp_data_hazard_ex && ex_reg_mem_val
    
  // stall for RAW/WAW hazards on LB/LH and mul/div in memory stage.
  val mem_mem_cmd_bh = 
    (mem_reg_mem_type === MT_B)  || (mem_reg_mem_type === MT_BU) ||
    (mem_reg_mem_type === MT_H)  || (mem_reg_mem_type === MT_HU)
  val data_hazard_mem = mem_reg_wen &&
    (id_renx1.toBool && id_raddr1 === io.dpath.mem_waddr ||
     id_renx2.toBool && id_raddr2 === io.dpath.mem_waddr ||
     id_wen.toBool   && id_waddr  === io.dpath.mem_waddr)
  val fp_data_hazard_mem = mem_reg_fp_wen &&
    (fpdec.io.ren1 && id_raddr1 === io.dpath.mem_waddr ||
     fpdec.io.ren2 && id_raddr2 === io.dpath.mem_waddr ||
     fpdec.io.ren3 && id_raddr3 === io.dpath.mem_waddr ||
     fpdec.io.wen  && id_waddr  === io.dpath.mem_waddr)
  val id_mem_hazard = data_hazard_mem && (mem_reg_mem_val && mem_mem_cmd_bh || mem_reg_div_mul_val)
  id_load_use := mem_reg_mem_val && (data_hazard_mem || fp_data_hazard_mem)

  // stall for RAW/WAW hazards on load/AMO misses and mul/div in writeback.
  val data_hazard_wb = wb_reg_wen &&
    (id_renx1.toBool && id_raddr1 === io.dpath.wb_waddr ||
     id_renx2.toBool && id_raddr2 === io.dpath.wb_waddr ||
     id_wen.toBool   && id_waddr  === io.dpath.wb_waddr)
  val fp_data_hazard_wb = wb_reg_fp_wen &&
    (fpdec.io.ren1 && id_raddr1 === io.dpath.wb_waddr ||
     fpdec.io.ren2 && id_raddr2 === io.dpath.wb_waddr ||
     fpdec.io.ren3 && id_raddr3 === io.dpath.wb_waddr ||
     fpdec.io.wen  && id_waddr  === io.dpath.wb_waddr)
  val id_wb_hazard = data_hazard_wb && (wb_reg_dcache_miss || wb_reg_div_mul_val) ||
                     fp_data_hazard_wb && wb_reg_dcache_miss

  val ctrl_stalld =
    !take_pc &&
    (
      id_ex_hazard || id_mem_hazard || id_wb_hazard ||
      id_stall_raddr1 || id_stall_raddr2 || id_stall_waddr ||
      id_stall_fpu ||
      id_mem_val.toBool && !(io.dmem.req_rdy && io.dtlb_rdy) ||
      ((id_sync === SYNC_D) || (id_sync === SYNC_I)) && !io.dmem.req_rdy ||
      id_console_out_val && !io.console.rdy
    );
  val ctrl_stallf = ctrl_stalld;
    
  val ctrl_killd = take_pc || ctrl_stalld;
  val ctrl_killf = take_pc || !io.imem.resp_val;
  
  io.flush_inst     := wb_reg_flush_inst;


  io.dpath.stallf   := ctrl_stallf;
  io.dpath.stalld   := ctrl_stalld;
  io.dpath.killf    := ctrl_killf;
  io.dpath.killd    := ctrl_killd;
  io.dpath.killx    := kill_ex;
  io.dpath.killm    := kill_mem;

  io.dpath.mem_load := mem_reg_mem_val && mem_reg_wen
  io.dpath.ren2     := id_renx2.toBool;
  io.dpath.ren1     := id_renx1.toBool;
  io.dpath.sel_alu2 := id_sel_alu2;
  io.dpath.fn_dw    := id_fn_dw.toBool;
  io.dpath.fn_alu   := id_fn_alu;
  io.dpath.div_fn   := id_div_fn;
  io.dpath.div_val  := id_div_val.toBool;
  io.dpath.mul_fn   := id_mul_fn;
  io.dpath.mul_val  := id_mul_val.toBool;
  io.dpath.ex_fp_val:= ex_reg_fp_val;
  io.dpath.ex_wen   := ex_reg_wen;
  io.dpath.mem_wen  := mem_reg_wen;
  io.dpath.wb_wen   := wb_reg_wen;
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
