package rocket

import Chisel._
import Node._
import Constants._
import Instructions._
import hwacha._
import ALU._

class ioCtrlDpath extends Bundle()
{
  // outputs to datapath
  val sel_pc   = UFix(OUTPUT, 3);
  val stalld   = Bool(OUTPUT);
  val killd    = Bool(OUTPUT);
  val killx    = Bool(OUTPUT);
  val killm    = Bool(OUTPUT);
  val ren2     = Bool(OUTPUT);
  val ren1     = Bool(OUTPUT);
  val sel_alu2 = UFix(OUTPUT, 3);
  val fn_dw    = Bool(OUTPUT);
  val fn_alu   = UFix(OUTPUT, 4);
  val mul_val  = Bool(OUTPUT);
  val mul_fn   = UFix(OUTPUT, 2);
  val mul_kill = Bool(OUTPUT)
  val div_val  = Bool(OUTPUT);
  val div_fn   = UFix(OUTPUT, 2);
  val div_kill = Bool(OUTPUT)
  val sel_wa   = Bool(OUTPUT);
  val sel_wb   = UFix(OUTPUT, 3);
  val pcr      = UFix(OUTPUT, 3)
  val id_eret  = Bool(OUTPUT);
  val wb_eret  = Bool(OUTPUT);
  val mem_load = Bool(OUTPUT);
  val ex_fp_val= Bool(OUTPUT);
  val mem_fp_val= Bool(OUTPUT);
  val ex_wen   = Bool(OUTPUT);
  val ex_jalr  = Bool(OUTPUT)
  val mem_wen  = Bool(OUTPUT);
  val wb_wen   = Bool(OUTPUT);
  val wb_valid = Bool(OUTPUT)
  val flush_inst = Bool(OUTPUT);
  val ex_mem_type = UFix(OUTPUT, 3)
  // exception handling
  val exception = Bool(OUTPUT);
  val cause    = UFix(OUTPUT, 6);
  val badvaddr_wen = Bool(OUTPUT); // high for a load/store access fault
  val vec_irq_aux_wen = Bool(OUTPUT)
  // inputs from datapath
  val inst    = Bits(INPUT, 32);
  val jalr_eq = Bool(INPUT)
  val br_eq   = Bool(INPUT);
  val br_lt   = Bool(INPUT);
  val br_ltu  = Bool(INPUT);
  val div_rdy = Bool(INPUT);
  val div_result_val = Bool(INPUT);
  val mul_rdy = Bool(INPUT);
  val mul_result_val = Bool(INPUT);
  val mem_wb = Bool(INPUT);
  val ex_waddr = UFix(INPUT, 5);  // write addr from execute stage
  val mem_waddr = UFix(INPUT, 5); // write addr from memory stage
  val wb_waddr = UFix(INPUT, 5);  // write addr from writeback stage
  val status  = Bits(INPUT, 32);
  val sboard_clr  = Bool(INPUT);
  val sboard_clra = UFix(INPUT, 5);
  val fp_sboard_clr  = Bool(INPUT);
  val fp_sboard_clra = UFix(INPUT, 5);
  val fp_sboard_wb_waddr = UFix(INPUT, 5);
  val irq_timer = Bool(INPUT);
  val irq_ipi   = Bool(INPUT);
  val pcr_replay = Bool(INPUT)
}

class ioCtrlAll extends Bundle()
{
  val dpath   = new ioCtrlDpath();
  val imem = new IOCPUFrontend
  val dmem = new ioHellaCache
  val dtlb_val = Bool(OUTPUT);
  val dtlb_kill = Bool(OUTPUT);
  val dtlb_rdy = Bool(INPUT);
  val dtlb_miss = Bool(INPUT);
  val xcpt_dtlb_ld = Bool(INPUT);
  val xcpt_dtlb_st = Bool(INPUT);
  val fpu = new ioCtrlFPU();
  val vec_dpath = new ioCtrlDpathVec()
  val vec_iface = new ioCtrlVecInterface()
}

abstract trait rocketCtrlDecodeConstants
{
  val xpr64 = Y;

  val decode_default =
                //                    jalr                                                                                    eret
                //         fp_val     | renx2                                                   div_val                       | syscall
                //         | vec_val  | | renx1                     mem_val           mul_val   | wen            pcr          | | privileged
                //   val   | | brtype | | | s_alu2   dw     alu     | mem_cmd mem_type| mul_fn  | | s_wa  s_wb   |     sync   | | | replay_next
                //   |     | | |      | | | |        |      |       | |         |     | |       | | |     |      |     |      | | | |
                List(N,    X,X,BR_X,  X,X,X,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, X,MUL_X,  X,X,WA_X, WB_X,  PCR_X,SYNC_X,X,X,X,X)
                                        
  val table: Array[(Bits, List[Bits])]
}

object rocketCtrlXDecode extends rocketCtrlDecodeConstants
{
  val table = Array(
                //                    jalr                                                                                    eret
                //         fp_val     | renx2                                                   div_val                       | syscall
                //         | vec_val  | | renx1                     mem_val           mul_val   | wen            pcr          | | privileged
                //   val   | | brtype | | | s_alu2   dw     alu     | mem_cmd mem_type| mul_fn  | | s_wa  s_wb   |     sync   | | | replay_next
                //   |     | | |      | | | |        |      |       | |         |     | |       | | |     |      |     |      | | | |
    BNE->       List(Y,    N,N,BR_NE, N,Y,Y,A2_BTYPE,DW_X,  FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,N,WA_X, WB_X,  PCR_N,SYNC_N,N,N,N,N),
    BEQ->       List(Y,    N,N,BR_EQ, N,Y,Y,A2_BTYPE,DW_X,  FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,N,WA_X, WB_X,  PCR_N,SYNC_N,N,N,N,N),
    BLT->       List(Y,    N,N,BR_LT, N,Y,Y,A2_BTYPE,DW_X,  FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,N,WA_X, WB_X,  PCR_N,SYNC_N,N,N,N,N),
    BLTU->      List(Y,    N,N,BR_LTU,N,Y,Y,A2_BTYPE,DW_X,  FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,N,WA_X, WB_X,  PCR_N,SYNC_N,N,N,N,N),
    BGE->       List(Y,    N,N,BR_GE, N,Y,Y,A2_BTYPE,DW_X,  FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,N,WA_X, WB_X,  PCR_N,SYNC_N,N,N,N,N),
    BGEU->      List(Y,    N,N,BR_GEU,N,Y,Y,A2_BTYPE,DW_X,  FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,N,WA_X, WB_X,  PCR_N,SYNC_N,N,N,N,N),
                                        
    J->         List(Y,    N,N,BR_J,  N,N,N,A2_JTYPE,DW_X,  FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,N,WA_X, WB_X,  PCR_N,SYNC_N,N,N,N,N),
    JAL->       List(Y,    N,N,BR_J,  N,N,N,A2_JTYPE,DW_X,  FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RA,WB_PC, PCR_N,SYNC_N,N,N,N,N),
    JALR_C->    List(Y,    N,N,BR_N,  Y,N,Y,A2_ITYPE,DW_XPR,FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_PC, PCR_N,SYNC_N,N,N,N,N),
    JALR_J->    List(Y,    N,N,BR_N,  Y,N,Y,A2_ITYPE,DW_XPR,FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_PC, PCR_N,SYNC_N,N,N,N,N),
    JALR_R->    List(Y,    N,N,BR_N,  Y,N,Y,A2_ITYPE,DW_XPR,FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_PC, PCR_N,SYNC_N,N,N,N,N),
    RDNPC->     List(Y,    N,N,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_PC, PCR_N,SYNC_N,N,N,N,N),
                                        
    LB->        List(Y,    N,N,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_ADD, Y,M_XRD,    MT_B, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    LH->        List(Y,    N,N,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_ADD, Y,M_XRD,    MT_H, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    LW->        List(Y,    N,N,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_ADD, Y,M_XRD,    MT_W, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    LD->        List(xpr64,N,N,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_ADD, Y,M_XRD,    MT_D, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    LBU->       List(Y,    N,N,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_ADD, Y,M_XRD,    MT_BU,N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    LHU->       List(Y,    N,N,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_ADD, Y,M_XRD,    MT_HU,N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    LWU->       List(xpr64,N,N,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_ADD, Y,M_XRD,    MT_WU,N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    SB->        List(Y,    N,N,BR_N,  N,Y,Y,A2_BTYPE,DW_XPR,FN_ADD, Y,M_XWR,    MT_B, N,MUL_X,  N,N,WA_X, WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    SH->        List(Y,    N,N,BR_N,  N,Y,Y,A2_BTYPE,DW_XPR,FN_ADD, Y,M_XWR,    MT_H, N,MUL_X,  N,N,WA_X, WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    SW->        List(Y,    N,N,BR_N,  N,Y,Y,A2_BTYPE,DW_XPR,FN_ADD, Y,M_XWR,    MT_W, N,MUL_X,  N,N,WA_X, WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    SD->        List(xpr64,N,N,BR_N,  N,Y,Y,A2_BTYPE,DW_XPR,FN_ADD, Y,M_XWR,    MT_D, N,MUL_X,  N,N,WA_X, WB_ALU,PCR_N,SYNC_N,N,N,N,N),
                                        
    AMOADD_W->  List(Y,    N,N,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, Y,M_XA_ADD, MT_W, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    AMOSWAP_W-> List(Y,    N,N,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, Y,M_XA_SWAP,MT_W, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    AMOAND_W->  List(Y,    N,N,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, Y,M_XA_AND, MT_W, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    AMOOR_W->   List(Y,    N,N,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, Y,M_XA_OR,  MT_W, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    AMOMIN_W->  List(Y,    N,N,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, Y,M_XA_MIN, MT_W, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    AMOMINU_W-> List(Y,    N,N,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, Y,M_XA_MINU,MT_W, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    AMOMAX_W->  List(Y,    N,N,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, Y,M_XA_MAX, MT_W, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    AMOMAXU_W-> List(Y,    N,N,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, Y,M_XA_MAXU,MT_W, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    AMOADD_D->  List(xpr64,N,N,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, Y,M_XA_ADD, MT_D, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    AMOSWAP_D-> List(xpr64,N,N,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, Y,M_XA_SWAP,MT_D, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    AMOAND_D->  List(xpr64,N,N,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, Y,M_XA_AND, MT_D, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    AMOOR_D->   List(xpr64,N,N,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, Y,M_XA_OR,  MT_D, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    AMOMIN_D->  List(xpr64,N,N,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, Y,M_XA_MIN, MT_D, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    AMOMINU_D-> List(xpr64,N,N,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, Y,M_XA_MINU,MT_D, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    AMOMAX_D->  List(xpr64,N,N,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, Y,M_XA_MAX, MT_D, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    AMOMAXU_D-> List(xpr64,N,N,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, Y,M_XA_MAXU,MT_D, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
                                        
    LUI->       List(Y,    N,N,BR_N,  N,N,N,A2_LTYPE,DW_XPR,FN_OP2, N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    ADDI->      List(Y,    N,N,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    SLTI ->     List(Y,    N,N,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_SLT, N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    SLTIU->     List(Y,    N,N,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_SLTU,N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    ANDI->      List(Y,    N,N,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_AND, N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    ORI->       List(Y,    N,N,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_OR,  N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    XORI->      List(Y,    N,N,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_XOR, N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    SLLI->      List(Y,    N,N,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_SL,  N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    SRLI->      List(Y,    N,N,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_SR,  N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    SRAI->      List(Y,    N,N,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_SRA, N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    ADD->       List(Y,    N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_XPR,FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    SUB->       List(Y,    N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_XPR,FN_SUB, N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    SLT->       List(Y,    N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_XPR,FN_SLT, N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    SLTU->      List(Y,    N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_XPR,FN_SLTU,N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    riscvAND->  List(Y,    N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_XPR,FN_AND, N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    riscvOR->   List(Y,    N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_XPR,FN_OR,  N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    riscvXOR->  List(Y,    N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_XPR,FN_XOR, N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    SLL->       List(Y,    N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_XPR,FN_SL,  N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    SRL->       List(Y,    N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_XPR,FN_SR,  N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    SRA->       List(Y,    N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_XPR,FN_SRA, N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
                                        
    ADDIW->     List(xpr64,N,N,BR_N,  N,N,Y,A2_ITYPE,DW_32,FN_ADD,  N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),   
    SLLIW->     List(xpr64,N,N,BR_N,  N,N,Y,A2_ITYPE,DW_32,FN_SL,   N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    SRLIW->     List(xpr64,N,N,BR_N,  N,N,Y,A2_ITYPE,DW_32,FN_SR,   N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    SRAIW->     List(xpr64,N,N,BR_N,  N,N,Y,A2_ITYPE,DW_32,FN_SRA,  N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    ADDW->      List(xpr64,N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_32,FN_ADD,  N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    SUBW->      List(xpr64,N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_32,FN_SUB,  N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    SLLW->      List(xpr64,N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_32,FN_SL,   N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    SRLW->      List(xpr64,N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_32,FN_SR,   N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    SRAW->      List(xpr64,N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_32,FN_SRA,  N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
                                        
    MUL->       List(Y,    N,N,BR_N,  N,Y,Y,A2_X,    DW_XPR,FN_X,   N,M_X,      MT_X, Y,MUL_LO, N,Y,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    MULH->      List(Y,    N,N,BR_N,  N,Y,Y,A2_X,    DW_XPR,FN_X,   N,M_X,      MT_X, Y,MUL_H,  N,Y,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    MULHU->     List(Y,    N,N,BR_N,  N,Y,Y,A2_X,    DW_XPR,FN_X,   N,M_X,      MT_X, Y,MUL_HU, N,Y,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    MULHSU->    List(Y,    N,N,BR_N,  N,Y,Y,A2_X,    DW_XPR,FN_X,   N,M_X,      MT_X, Y,MUL_HSU,N,Y,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    MULW->      List(xpr64,N,N,BR_N,  N,Y,Y,A2_X,    DW_32, FN_X,   N,M_X,      MT_X, Y,MUL_LO, N,Y,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
                                        
    DIV->       List(Y,    N,N,BR_N,  N,Y,Y,A2_X,    DW_XPR,FN_X,   N,M_X,      MT_X, N,DIV_D,  Y,Y,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    DIVU->      List(Y,    N,N,BR_N,  N,Y,Y,A2_X,    DW_XPR,FN_X,   N,M_X,      MT_X, N,DIV_DU, Y,Y,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    REM->       List(Y,    N,N,BR_N,  N,Y,Y,A2_X,    DW_XPR,FN_X,   N,M_X,      MT_X, N,DIV_R,  Y,Y,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    REMU->      List(Y,    N,N,BR_N,  N,Y,Y,A2_X,    DW_XPR,FN_X,   N,M_X,      MT_X, N,DIV_RU, Y,Y,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    DIVW->      List(xpr64,N,N,BR_N,  N,Y,Y,A2_X,    DW_32, FN_X,   N,M_X,      MT_X, N,DIV_D,  Y,Y,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    DIVUW->     List(xpr64,N,N,BR_N,  N,Y,Y,A2_X,    DW_32, FN_X,   N,M_X,      MT_X, N,DIV_DU, Y,Y,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    REMW->      List(xpr64,N,N,BR_N,  N,Y,Y,A2_X,    DW_32, FN_X,   N,M_X,      MT_X, N,DIV_R,  Y,Y,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    REMUW->     List(xpr64,N,N,BR_N,  N,Y,Y,A2_X,    DW_32, FN_X,   N,M_X,      MT_X, N,DIV_RU, Y,Y,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
                                        
    SYSCALL->   List(Y,    N,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_X, WB_X,  PCR_N,SYNC_N,N,Y,N,N),
    SETPCR->    List(Y,    N,N,BR_N,  N,N,N,A2_ITYPE,DW_XPR,FN_OP2, N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_S,SYNC_N,N,N,Y,Y),
    CLEARPCR->  List(Y,    N,N,BR_N,  N,N,N,A2_ITYPE,DW_XPR,FN_OP2, N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_C,SYNC_N,N,N,Y,Y),
    ERET->      List(Y,    N,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_X, WB_X,  PCR_N,SYNC_N,Y,N,Y,N),
    FENCE->     List(Y,    N,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   Y,M_FENCE,  MT_X, N,MUL_X,  N,N,WA_X, WB_X,  PCR_N,SYNC_D,N,N,N,N),
    FENCE_I->   List(Y,    N,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   Y,M_FLA,    MT_X, N,MUL_X,  N,N,WA_X, WB_X,  PCR_N,SYNC_I,N,N,N,Y),
    CFLUSH->    List(Y,    N,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   Y,M_FLA,    MT_X, N,MUL_X,  N,N,WA_X, WB_X,  PCR_N,SYNC_N,N,N,Y,Y),
    MFPCR->     List(Y,    N,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_X,  PCR_F,SYNC_N,N,N,Y,Y),
    MTPCR->     List(Y,    N,N,BR_N,  N,Y,N,A2_RTYPE,DW_XPR,FN_OP2, N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_T,SYNC_N,N,N,Y,Y),
    RDTIME->    List(Y,    N,N,BR_N,  N,N,N,A2_X,    DW_XPR,FN_X,   N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_TSC,PCR_N,SYNC_N,N,N,N,N),
    RDCYCLE->   List(Y,    N,N,BR_N,  N,N,N,A2_X,    DW_XPR,FN_X,   N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_TSC,PCR_N,SYNC_N,N,N,N,N),
    RDINSTRET-> List(Y,    N,N,BR_N,  N,N,N,A2_X,    DW_XPR,FN_X,   N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_IRT,PCR_N,SYNC_N,N,N,N,N))
}

object rocketCtrlFDecode extends rocketCtrlDecodeConstants
{
  val table = Array(
                //                    jalr                                                                                    eret
                //         fp_val     | renx2                                                   div_val                       | syscall
                //         | vec_val  | | renx1                     mem_val           mul_val   | wen            pcr          | | privileged
                //   val   | | brtype | | | s_alu2   dw     alu     | mem_cmd mem_type| mul_fn  | | s_wa  s_wb   |     sync   | | | replay_next
                //   |     | | |      | | | |        |      |       | |         |     | |       | | |     |      |     |      | | | |
    FCVT_S_D->  List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FCVT_D_S->  List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FSGNJ_S->   List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FSGNJ_D->   List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FSGNJX_S->  List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FSGNJX_D->  List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FSGNJN_S->  List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FSGNJN_D->  List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FMIN_S->    List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FMIN_D->    List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FMAX_S->    List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FMAX_D->    List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FADD_S->    List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FADD_D->    List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FSUB_S->    List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FSUB_D->    List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FMUL_S->    List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FMUL_D->    List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FMADD_S->   List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FMADD_D->   List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FMSUB_S->   List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FMSUB_D->   List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FNMADD_S->  List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FNMADD_D->  List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FNMSUB_S->  List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FNMSUB_D->  List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    MFTX_S->    List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    MFTX_D->    List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FCVT_W_S->  List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FCVT_W_D->  List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FCVT_WU_S-> List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FCVT_WU_D-> List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FCVT_L_S->  List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FCVT_L_D->  List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FCVT_LU_S-> List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FCVT_LU_D-> List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FEQ_S->     List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FEQ_D->     List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FLT_S->     List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FLT_D->     List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FLE_S->     List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FLE_D->     List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    MXTF_S->    List(FPU_Y,Y,N,BR_N,  N,N,Y,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    MXTF_D->    List(FPU_Y,Y,N,BR_N,  N,N,Y,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FCVT_S_W->  List(FPU_Y,Y,N,BR_N,  N,N,Y,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FCVT_D_W->  List(FPU_Y,Y,N,BR_N,  N,N,Y,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FCVT_S_WU-> List(FPU_Y,Y,N,BR_N,  N,N,Y,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FCVT_D_WU-> List(FPU_Y,Y,N,BR_N,  N,N,Y,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FCVT_S_L->  List(FPU_Y,Y,N,BR_N,  N,N,Y,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FCVT_D_L->  List(FPU_Y,Y,N,BR_N,  N,N,Y,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FCVT_S_LU-> List(FPU_Y,Y,N,BR_N,  N,N,Y,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FCVT_D_LU-> List(FPU_Y,Y,N,BR_N,  N,N,Y,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    MFFSR->     List(FPU_Y,Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    MTFSR->     List(FPU_Y,Y,N,BR_N,  N,N,Y,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FLW->       List(FPU_Y,Y,N,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_ADD, Y,M_XRD,    MT_W, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    FLD->       List(FPU_Y,Y,N,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_ADD, Y,M_XRD,    MT_D, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    FSW->       List(FPU_Y,Y,N,BR_N,  N,N,Y,A2_BTYPE,DW_XPR,FN_ADD, Y,M_XWR,    MT_W, N,MUL_X,  N,N,WA_X, WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    FSD->       List(FPU_Y,Y,N,BR_N,  N,N,Y,A2_BTYPE,DW_XPR,FN_ADD, Y,M_XWR,    MT_D, N,MUL_X,  N,N,WA_X, WB_ALU,PCR_N,SYNC_N,N,N,N,N))
}

object rocketCtrlVDecode extends rocketCtrlDecodeConstants
{
  val table = Array(
                //                    jalr                                                                                    eret
                //         fp_val     | renx2                                                   div_val                       | syscall
                //         | vec_val  | | renx1                     mem_val           mul_val   | wen            pcr          | | privileged
                //   val   | | brtype | | | s_alu2   dw     alu     | mem_cmd mem_type| mul_fn  | | s_wa  s_wb   |     sync   | | | replay_next
                //   |     | | |      | | | |        |      |       | |         |     | |       | | |     |      |     |      | | | |
    VVCFGIVL->  List(VEC_Y,N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,Y),
    VVCFG->     List(VEC_Y,N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,Y),
    VSETVL->    List(VEC_Y,N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,Y,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,Y),
    VF->        List(VEC_Y,N,Y,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,N,WA_X, WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    VMVV->      List(VEC_Y,N,Y,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    VMSV->      List(VEC_Y,N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    VFMVV->     List(VEC_Y,N,Y,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FENCE_V_L-> List(VEC_Y,N,Y,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_X, WB_X,  PCR_N,SYNC_N,N,N,N,N),
    FENCE_V_G-> List(VEC_Y,N,Y,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_X, WB_X,  PCR_N,SYNC_D,N,N,N,N),
    VLD->       List(VEC_Y,N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    VLW->       List(VEC_Y,N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    VLWU->      List(VEC_Y,N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    VLH->       List(VEC_Y,N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    VLHU->      List(VEC_Y,N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    VLB->       List(VEC_Y,N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    VLBU->      List(VEC_Y,N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    VSD->       List(VEC_Y,N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    VSW->       List(VEC_Y,N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    VSH->       List(VEC_Y,N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    VSB->       List(VEC_Y,N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    VFLD->      List(VEC_Y,N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    VFLW->      List(VEC_Y,N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    VFSD->      List(VEC_Y,N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    VFSW->      List(VEC_Y,N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    VLSTD->     List(VEC_Y,N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_D, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    VLSTW->     List(VEC_Y,N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_D, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    VLSTWU->    List(VEC_Y,N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_D, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    VLSTH->     List(VEC_Y,N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_D, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    VLSTHU->    List(VEC_Y,N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_D, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    VLSTB->     List(VEC_Y,N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_D, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    VLSTBU->    List(VEC_Y,N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_D, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    VSSTD->     List(VEC_Y,N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_D, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    VSSTW->     List(VEC_Y,N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_D, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    VSSTH->     List(VEC_Y,N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_D, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    VSSTB->     List(VEC_Y,N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_D, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    VFLSTD->    List(VEC_Y,N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_D, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    VFLSTW->    List(VEC_Y,N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_D, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    VFSSTD->    List(VEC_Y,N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_D, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
    VFSSTW->    List(VEC_Y,N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_D, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,N,N),
                                        
    VENQCMD->   List(VEC_Y,N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,Y,N),
    VENQIMM1->  List(VEC_Y,N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,Y,N),
    VENQIMM2->  List(VEC_Y,N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,Y,N),
    VENQCNT->   List(VEC_Y,N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,Y,N),
    VXCPTEVAC-> List(VEC_Y,N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,MUL_X,  N,N,WA_RD,WB_ALU,PCR_N,SYNC_N,N,N,Y,N),
    VXCPTKILL-> List(VEC_Y,N,Y,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_X, WB_X,  PCR_N,SYNC_N,N,N,Y,N),
    VXCPTHOLD-> List(VEC_Y,N,Y,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,MUL_X,  N,N,WA_X, WB_X,  PCR_N,SYNC_N,N,N,Y,N))
}

class rocketCtrl extends Component
{
  val io = new ioCtrlAll();

  var decode_table = rocketCtrlXDecode.table
  if (HAVE_FPU) decode_table ++= rocketCtrlFDecode.table
  if (HAVE_VEC) decode_table ++= rocketCtrlVDecode.table

  val cs = DecodeLogic(io.dpath.inst, rocketCtrlXDecode.decode_default, decode_table)

  val id_int_val :: id_fp_val :: id_vec_val :: id_br_type :: id_jalr :: id_renx2 :: id_renx1 :: id_sel_alu2 :: id_fn_dw :: id_fn_alu :: cs0 = cs 
  val id_mem_val :: id_mem_cmd :: id_mem_type :: id_mul_val :: id_mul_fn :: id_div_val :: id_wen :: id_sel_wa :: id_sel_wb :: cs1 = cs0
  val id_pcr :: id_sync :: id_eret :: id_syscall :: id_privileged :: id_replay_next :: Nil = cs1

  val id_raddr3 = io.dpath.inst(16,12);
  val id_raddr2 = io.dpath.inst(21,17);
  val id_raddr1 = io.dpath.inst(26,22);
  val id_waddr  = Mux(id_sel_wa === WA_RA, RA, io.dpath.inst(31,27));
  val id_load_use = Bool();
  
  val ex_reg_br_type     = Reg(){Bits()}
  val ex_reg_jalr        = Reg(){Bool()}
  val ex_reg_btb_hit     = Reg(){Bool()};
  val ex_reg_div_val     = Reg(){Bool()};
  val ex_reg_mul_val     = Reg(){Bool()};
  val ex_reg_mul_fn      = Reg(){UFix()};
  val ex_reg_mem_val     = Reg(){Bool()};
  val ex_reg_mem_cmd     = Reg(){Bits()};
  val ex_reg_mem_type    = Reg(){UFix(width = 3)};
  val ex_reg_valid       = Reg(resetVal = Bool(false));
  val ex_reg_pcr         = Reg(resetVal = PCR_N);
  val ex_reg_wen         = Reg(resetVal = Bool(false));
  val ex_reg_fp_wen      = Reg(resetVal = Bool(false));
  val ex_reg_eret        = Reg(resetVal = Bool(false));
  val ex_reg_flush_inst  = Reg(resetVal = Bool(false));
  val ex_reg_xcpt_interrupt  = Reg(resetVal = Bool(false));
  val ex_reg_cause           = Reg(){UFix()}
  val ex_reg_xcpt_ma_inst    = Reg(resetVal = Bool(false));
  val ex_reg_xcpt_itlb       = Reg(resetVal = Bool(false));
  val ex_reg_xcpt_illegal    = Reg(resetVal = Bool(false));
  val ex_reg_xcpt_privileged = Reg(resetVal = Bool(false));
  val ex_reg_xcpt_syscall    = Reg(resetVal = Bool(false));
  val ex_reg_fp_val          = Reg(resetVal = Bool(false));
  val ex_reg_fp_sboard_set   = Reg(resetVal = Bool(false));
  val ex_reg_vec_val         = Reg(resetVal = Bool(false));
  val ex_reg_replay_next     = Reg(resetVal = Bool(false));
  val ex_reg_load_use        = Reg(resetVal = Bool(false));

  val mem_reg_valid           = Reg(resetVal = Bool(false));
  val mem_reg_pcr             = Reg(resetVal = PCR_N);
  val mem_reg_wen             = Reg(resetVal = Bool(false));
  val mem_reg_fp_wen          = Reg(resetVal = Bool(false));
  val mem_reg_flush_inst      = Reg(resetVal = Bool(false));
  val mem_reg_xcpt_interrupt  = Reg(resetVal = Bool(false));
  val mem_reg_cause           = Reg(){UFix()}
  val mem_reg_xcpt_ma_inst    = Reg(resetVal = Bool(false));
  val mem_reg_xcpt_itlb       = Reg(resetVal = Bool(false));
  val mem_reg_xcpt_illegal    = Reg(resetVal = Bool(false));
  val mem_reg_xcpt_privileged = Reg(resetVal = Bool(false));
  val mem_reg_xcpt_fpu        = Reg(resetVal = Bool(false));
  val mem_reg_xcpt_vec        = Reg(resetVal = Bool(false));
  val mem_reg_xcpt_syscall    = Reg(resetVal = Bool(false));
  val mem_reg_fp_val          = Reg(resetVal = Bool(false));
  val mem_reg_replay          = Reg(resetVal = Bool(false));
  val mem_reg_replay_next     = Reg(resetVal = Bool(false));
  val mem_reg_kill            = Reg(resetVal = Bool(false));
  val mem_reg_fp_sboard_set   = Reg(resetVal = Bool(false));

  val wb_reg_valid           = Reg(resetVal = Bool(false));
  val wb_reg_pcr             = Reg(resetVal = PCR_N);
  val wb_reg_wen             = Reg(resetVal = Bool(false));
  val wb_reg_fp_wen          = Reg(resetVal = Bool(false));
  val wb_reg_flush_inst      = Reg(resetVal = Bool(false));
  val wb_reg_eret            = Reg(resetVal = Bool(false));
  val wb_reg_exception       = Reg(resetVal = Bool(false));
  val wb_reg_replay          = Reg(resetVal = Bool(false));
  val wb_reg_replay_next     = Reg(resetVal = Bool(false));
  val wb_reg_cause           = Reg(){UFix()};
  val wb_reg_fp_val          = Reg(resetVal = Bool(false));
  val wb_reg_fp_sboard_set   = Reg(resetVal = Bool(false));
  val wb_reg_dcache_miss = Reg(io.dmem.resp.bits.miss || io.dmem.resp.bits.nack, resetVal = Bool(false));
  val wb_reg_div_mul_val = Reg(resetVal = Bool(false))

  val take_pc = Bool()
  val take_pc_wb = Bool()
  val ctrl_killm = Bool()

  var vec_replay = Bool(false)
  var vec_stalld = Bool(false)
  var vec_irq = Bool(false)
  var vec_irq_cause = UFix(CAUSE_INTERRUPT+IRQ_IPI) // don't care
  if (HAVE_VEC)
  {
    // vector control
    val vec = new rocketCtrlVec()

    io.vec_dpath <> vec.io.dpath
    io.vec_iface <> vec.io.iface

    vec.io.valid := wb_reg_valid
    vec.io.s := io.dpath.status(SR_S)
    vec.io.sr_ev := io.dpath.status(SR_EV)
    vec.io.exception := wb_reg_exception
    vec.io.eret := wb_reg_eret

    val vec_dec = new rocketCtrlVecDecoder()
    vec_dec.io.inst := io.dpath.inst

    val s = io.dpath.status(SR_S)
    val mask_cmdq_ready = !vec_dec.io.sigs.enq_cmdq || s && io.vec_iface.vcmdq_ready || !s && io.vec_iface.vcmdq_user_ready
    val mask_ximm1q_ready = !vec_dec.io.sigs.enq_ximm1q || s && io.vec_iface.vximm1q_ready || !s && io.vec_iface.vximm1q_user_ready
    val mask_ximm2q_ready = !vec_dec.io.sigs.enq_ximm2q || s && io.vec_iface.vximm2q_ready || !s && io.vec_iface.vximm2q_user_ready
    val mask_cntq_ready = !vec_dec.io.sigs.enq_cntq || io.vec_iface.vcntq_ready
    val mask_pfcmdq_ready = !vec_dec.io.sigs.enq_pfcmdq || io.vec_iface.vpfcmdq_ready
    val mask_pfximm1q_ready = !vec_dec.io.sigs.enq_pfximm1q || io.vec_iface.vpfximm1q_ready
    val mask_pfximm2q_ready = !vec_dec.io.sigs.enq_pfximm2q || io.vec_iface.vpfximm2q_ready
    val mask_pfcntq_ready = !vec_dec.io.sigs.enq_pfcntq || io.vec_iface.vpfcntq_ready

    vec_stalld =
      id_vec_val && (
        !mask_cmdq_ready || !mask_ximm1q_ready || !mask_ximm2q_ready || !mask_cntq_ready ||
        !mask_pfcmdq_ready || !mask_pfximm1q_ready || !mask_pfximm2q_ready || !mask_pfcntq_ready ||
        vec_dec.io.sigs.vfence && !vec.io.vfence_ready)

    vec_replay = vec.io.replay
    vec_irq = vec.io.irq
    vec_irq_cause = vec.io.irq_cause
  }
  
  // executing ERET when traps are enabled causes an illegal instruction exception
  val illegal_inst = !id_int_val.toBool || (id_eret.toBool && io.dpath.status(SR_ET))

  val p_irq_timer = (io.dpath.status(SR_IM+IRQ_TIMER).toBool && io.dpath.irq_timer);
  val p_irq_ipi   = (io.dpath.status(SR_IM+IRQ_IPI).toBool && io.dpath.irq_ipi);
  val id_interrupt =
    io.dpath.status(SR_ET).toBool &&
    ((io.dpath.status(SR_IM+IRQ_TIMER).toBool && io.dpath.irq_timer) ||
     (io.dpath.status(SR_IM+IRQ_IPI).toBool && io.dpath.irq_ipi) ||
     vec_irq);
  val id_cause =
    Mux(p_irq_ipi, UFix(CAUSE_INTERRUPT+IRQ_IPI,6),
    Mux(p_irq_timer, UFix(CAUSE_INTERRUPT+IRQ_TIMER,6),
    vec_irq_cause))

  when (reset.toBool || io.dpath.killd) {
    ex_reg_br_type     := BR_N;
    ex_reg_jalr        := Bool(false)
    ex_reg_btb_hit     := Bool(false);
    ex_reg_div_val := Bool(false);
    ex_reg_mul_val := Bool(false);
    ex_reg_mem_val     := Bool(false);
    ex_reg_valid       := Bool(false);
    ex_reg_pcr         := PCR_N
    ex_reg_wen         := Bool(false);
    ex_reg_fp_wen      := Bool(false);
    ex_reg_eret        := Bool(false);
    ex_reg_flush_inst  := Bool(false);  
    ex_reg_xcpt_ma_inst     := Bool(false);
    ex_reg_xcpt_itlb        := Bool(false);
    ex_reg_xcpt_illegal     := Bool(false);
    ex_reg_xcpt_privileged  := Bool(false);
    ex_reg_xcpt_syscall     := Bool(false);
    ex_reg_fp_val           := Bool(false);
    ex_reg_fp_sboard_set    := Bool(false);
    ex_reg_vec_val          := Bool(false);
    ex_reg_replay_next      := Bool(false);
    ex_reg_load_use         := Bool(false);
  } 
  .otherwise {
    ex_reg_br_type     := id_br_type;
    ex_reg_jalr        := id_jalr
    ex_reg_btb_hit     := io.imem.resp.bits.taken
    ex_reg_div_val     := id_div_val.toBool && id_waddr != UFix(0);
    ex_reg_mul_val     := id_mul_val.toBool && id_waddr != UFix(0);
    ex_reg_mul_fn      := id_mul_fn.toUFix
    ex_reg_mem_val     := id_mem_val.toBool;
    ex_reg_valid       := Bool(true)
    ex_reg_pcr         := id_pcr
    ex_reg_wen         := id_wen.toBool && id_waddr != UFix(0);
    ex_reg_fp_wen      := id_fp_val && io.fpu.dec.wen
    ex_reg_eret        := id_eret.toBool;
    ex_reg_flush_inst  := (id_sync === SYNC_I);
    ex_reg_xcpt_ma_inst     := io.imem.resp.bits.xcpt_ma
    ex_reg_xcpt_itlb        := io.imem.resp.bits.xcpt_if
    ex_reg_xcpt_illegal     := illegal_inst;
    ex_reg_xcpt_privileged  := (id_privileged & ~io.dpath.status(SR_S)).toBool;
    ex_reg_xcpt_syscall     := id_syscall.toBool;
    ex_reg_fp_val           := id_fp_val
    ex_reg_fp_sboard_set    := io.fpu.dec.sboard
    ex_reg_vec_val          := id_vec_val.toBool
    ex_reg_replay_next      := id_replay_next
    ex_reg_load_use         := id_load_use;
  }
  ex_reg_xcpt_interrupt := !take_pc && id_interrupt
  ex_reg_mem_cmd := id_mem_cmd
  ex_reg_mem_type := id_mem_type.toUFix
  ex_reg_cause := id_cause

  val br_taken =
    Mux(ex_reg_br_type === BR_EQ,   io.dpath.br_eq,
    Mux(ex_reg_br_type === BR_NE,  ~io.dpath.br_eq,
    Mux(ex_reg_br_type === BR_LT,   io.dpath.br_lt,
    Mux(ex_reg_br_type === BR_GE,  ~io.dpath.br_lt,
    Mux(ex_reg_br_type === BR_LTU,  io.dpath.br_ltu,
    Mux(ex_reg_br_type === BR_GEU, ~io.dpath.br_ltu,
        ex_reg_br_type === BR_J))))))
  
  val mem_reg_div_val     = Reg(resetVal = Bool(false))
  val mem_reg_mul_val     = Reg(resetVal = Bool(false))
  val mem_reg_eret        = Reg(){Bool()};
  val mem_reg_mem_val     = Reg(){Bool()};
  val mem_reg_mem_cmd     = Reg(){Bits()}
  val mem_reg_mem_type    = Reg(){Bits()}

  when (reset.toBool || io.dpath.killx) {
    mem_reg_valid       := Bool(false);
    mem_reg_pcr         := PCR_N
    mem_reg_wen         := Bool(false);
    mem_reg_fp_wen      := Bool(false);
    mem_reg_eret        := Bool(false);
    mem_reg_mem_val     := Bool(false);
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
    mem_reg_replay_next      := Bool(false)
  }
  .otherwise {
    mem_reg_valid       := ex_reg_valid
    mem_reg_pcr         := ex_reg_pcr
    mem_reg_wen         := ex_reg_wen;
    mem_reg_fp_wen      := ex_reg_fp_wen;
    mem_reg_eret        := ex_reg_eret;
    mem_reg_mem_val     := ex_reg_mem_val;
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
    mem_reg_replay_next      := ex_reg_replay_next
  }
  mem_reg_div_val     := ex_reg_div_val && io.dpath.div_rdy
  mem_reg_mul_val     := ex_reg_mul_val && io.dpath.mul_rdy
  mem_reg_mem_cmd     := ex_reg_mem_cmd;
  mem_reg_mem_type    := ex_reg_mem_type;
  mem_reg_xcpt_interrupt := ex_reg_xcpt_interrupt && !take_pc_wb
  mem_reg_cause := ex_reg_cause

  when (ctrl_killm) {
    wb_reg_valid       := Bool(false)
    wb_reg_pcr         := PCR_N
    wb_reg_wen         := Bool(false);
    wb_reg_fp_wen      := Bool(false);
    wb_reg_eret        := Bool(false);
    wb_reg_flush_inst  := Bool(false);
    wb_reg_div_mul_val := Bool(false);
    wb_reg_fp_val      := Bool(false)
    wb_reg_fp_sboard_set := Bool(false)
    wb_reg_replay_next := Bool(false)
  }
  .otherwise {
    wb_reg_valid       := mem_reg_valid
    wb_reg_pcr         := mem_reg_pcr
    wb_reg_wen         := mem_reg_wen;
    wb_reg_fp_wen      := mem_reg_fp_wen;
    wb_reg_eret        := mem_reg_eret && !mem_reg_replay
    wb_reg_flush_inst  := mem_reg_flush_inst;
    wb_reg_div_mul_val := mem_reg_div_val || mem_reg_mul_val
    wb_reg_fp_val      := mem_reg_fp_val
    wb_reg_fp_sboard_set := mem_reg_fp_sboard_set
    wb_reg_replay_next := mem_reg_replay_next
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
    fp_sboard.io.w(0).addr := io.dpath.fp_sboard_wb_waddr

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

  // exception handling
  val mem_xcpt_ma_ld = io.dmem.xcpt.ma.ld && !mem_reg_kill
  val mem_xcpt_ma_st = io.dmem.xcpt.ma.st && !mem_reg_kill
  val mem_xcpt_dtlb_ld = io.xcpt_dtlb_ld && !mem_reg_kill
  val mem_xcpt_dtlb_st = io.xcpt_dtlb_st && !mem_reg_kill
  
  val mem_exception =
    mem_reg_xcpt_interrupt ||
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
    Mux(mem_reg_xcpt_interrupt,   mem_reg_cause, // asynchronous interrupt
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
  val take_pc_ex = !Mux(ex_reg_jalr, ex_reg_btb_hit && io.dpath.jalr_eq, ex_reg_btb_hit === br_taken)
  take_pc_wb := wb_reg_replay || vec_replay || wb_reg_exception || wb_reg_eret
  take_pc := take_pc_ex || take_pc_wb;

  // replay mem stage PC on a DTLB miss or a long-latency writeback
  val mem_ll_wb = io.dpath.mem_wb || io.dpath.mul_result_val || io.dpath.div_result_val
  val dmem_kill_mem = mem_reg_valid && (io.dtlb_miss || io.dmem.resp.bits.nack)
  val fpu_kill_mem = mem_reg_fp_val && io.fpu.nack_mem
  val replay_mem  = dmem_kill_mem || mem_reg_wen && mem_ll_wb || mem_reg_replay || fpu_kill_mem
  val killm_common = mem_reg_wen && mem_ll_wb || take_pc_wb || mem_exception || mem_reg_kill
  ctrl_killm := killm_common || dmem_kill_mem || fpu_kill_mem

  // replay execute stage PC when the D$ is blocked, when the D$ misses, 
  // for privileged instructions, and for fence.i instructions
  val replay_ex    = wb_reg_dcache_miss && ex_reg_load_use || mem_reg_flush_inst || 
                     ex_reg_mem_val && !(io.dmem.req.ready && io.dtlb_rdy) ||
                     ex_reg_div_val && !io.dpath.div_rdy ||
                     ex_reg_mul_val && !io.dpath.mul_rdy ||
                     mem_reg_replay_next
  val kill_ex      = take_pc_wb || replay_ex

  mem_reg_replay := replay_ex && !take_pc_wb;
  mem_reg_kill := kill_ex;

  wb_reg_replay       := replay_mem && !take_pc_wb
  wb_reg_exception    := mem_exception && !take_pc_wb && !wb_reg_replay_next
  wb_reg_cause        := mem_cause;

  val replay_wb = wb_reg_replay || vec_replay || io.dpath.pcr_replay

  val wb_badvaddr_wen = wb_reg_exception && ((wb_reg_cause === UFix(10)) || (wb_reg_cause === UFix(11)))

	// write cause to PCR on an exception
	io.dpath.exception    := wb_reg_exception;
	io.dpath.cause        := wb_reg_cause;
	io.dpath.badvaddr_wen := wb_badvaddr_wen;
  io.dpath.vec_irq_aux_wen := wb_reg_exception && wb_reg_cause >= UFix(24) && wb_reg_cause < UFix(32)

  io.dpath.sel_pc :=
    Mux(wb_reg_exception, PC_PCR, // exception
    Mux(wb_reg_eret,      PC_PCR, // eret instruction
    Mux(replay_wb,        PC_WB,  // replay
    Mux(ex_reg_jalr,      PC_EX,  // JALR
    Mux(!ex_reg_btb_hit,  PC_EX,  // mispredicted taken branch
        PC_EX4)))))               // mispredicted not taken branch

  io.imem.req.bits.mispredict := !take_pc_wb && take_pc_ex
  io.imem.req.bits.taken := !ex_reg_btb_hit || ex_reg_jalr
  io.imem.req.valid  := take_pc

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
  val id_mem_hazard = data_hazard_mem && (mem_reg_mem_val && mem_mem_cmd_bh || mem_reg_div_val || mem_reg_mul_val || mem_reg_fp_val) ||
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

  val killd_common = take_pc || id_interrupt || ex_reg_replay_next
  val ctrl_killd = killd_common || !io.imem.resp.valid

  val ctrl_stalld =
    id_ex_hazard || id_mem_hazard || id_wb_hazard ||
    id_stall_raddr1 || id_stall_raddr2 || id_stall_waddr ||
    id_fp_val && id_stall_fpu ||
    id_mem_val && !(io.dmem.req.ready && io.dtlb_rdy) ||
    (id_sync === SYNC_D || id_sync === SYNC_I) && !io.dmem.req.ready ||
    vec_stalld
  
  io.dpath.flush_inst := wb_reg_flush_inst;
  io.dpath.stalld   := !ctrl_killd && ctrl_stalld;
  io.dpath.killd    := ctrl_killd || ctrl_stalld
  io.dpath.killx    := kill_ex;
  io.dpath.killm    := killm_common
  io.imem.resp.ready := killd_common || !ctrl_stalld
  io.imem.req.bits.invalidate := wb_reg_flush_inst

  io.dpath.mem_load := mem_reg_mem_val && mem_reg_wen
  io.dpath.ren2     := id_renx2.toBool;
  io.dpath.ren1     := id_renx1.toBool;
  io.dpath.sel_alu2 := id_sel_alu2.toUFix
  io.dpath.fn_dw    := id_fn_dw.toBool;
  io.dpath.fn_alu   := id_fn_alu.toUFix
  io.dpath.div_fn   := ex_reg_mul_fn
  io.dpath.div_val  := ex_reg_div_val
  io.dpath.div_kill := mem_reg_div_val && killm_common
  io.dpath.mul_fn   := ex_reg_mul_fn
  io.dpath.mul_val  := ex_reg_mul_val
  io.dpath.mul_kill := mem_reg_mul_val && killm_common
  io.dpath.ex_fp_val:= ex_reg_fp_val;
  io.dpath.mem_fp_val:= mem_reg_fp_val;
  io.dpath.ex_jalr  := ex_reg_jalr
  io.dpath.ex_wen   := ex_reg_wen;
  io.dpath.mem_wen  := mem_reg_wen;
  io.dpath.wb_wen   := wb_reg_wen;
  io.dpath.wb_valid := wb_reg_valid && !vec_replay
  io.dpath.sel_wa   := id_sel_wa.toBool;
  io.dpath.sel_wb   := id_sel_wb.toUFix
  io.dpath.pcr      := wb_reg_pcr.toUFix
  io.dpath.id_eret  := id_eret.toBool;
  io.dpath.wb_eret  := wb_reg_eret;  
  io.dpath.ex_mem_type := ex_reg_mem_type

  io.fpu.valid := !io.dpath.killd && id_fp_val
  io.fpu.killx := kill_ex
  io.fpu.killm := killm_common

  io.dtlb_val           := ex_reg_mem_val
  io.dtlb_kill          := mem_reg_kill
  io.dmem.req.valid     := ex_reg_mem_val
  io.dmem.req.bits.kill := killm_common || io.dtlb_miss
  io.dmem.req.bits.cmd  := ex_reg_mem_cmd
  io.dmem.req.bits.typ  := ex_reg_mem_type
}
