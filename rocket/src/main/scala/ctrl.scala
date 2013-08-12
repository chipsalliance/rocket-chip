package rocket

import Chisel._
import Instructions._
import hwacha._
import uncore.constants.MemoryOpConstants._
import ALU._
import Util._

class CtrlDpathIO extends Bundle()
{
  // outputs to datapath
  val sel_pc   = UInt(OUTPUT, 3);
  val killd    = Bool(OUTPUT);
  val ren2     = Bool(OUTPUT);
  val ren1     = Bool(OUTPUT);
  val sel_alu2 = UInt(OUTPUT, 3);
  val fn_dw    = Bool(OUTPUT);
  val fn_alu   = UInt(OUTPUT, SZ_ALU_FN);
  val div_mul_val = Bool(OUTPUT)
  val div_mul_kill = Bool(OUTPUT)
  val div_val  = Bool(OUTPUT);
  val div_kill = Bool(OUTPUT)
  val sel_wa   = Bool(OUTPUT);
  val sel_wb   = UInt(OUTPUT, 3);
  val pcr      = UInt(OUTPUT, 3)
  val eret  = Bool(OUTPUT);
  val mem_load = Bool(OUTPUT);
  val wb_load = Bool(OUTPUT)
  val ex_fp_val= Bool(OUTPUT);
  val mem_fp_val= Bool(OUTPUT);
  val ex_wen   = Bool(OUTPUT);
  val ex_jalr  = Bool(OUTPUT)
  val mem_wen  = Bool(OUTPUT);
  val wb_wen   = Bool(OUTPUT);
  val wb_valid = Bool(OUTPUT)
  val ex_mem_type = Bits(OUTPUT, 3)
  val ex_rs2_val = Bool(OUTPUT)
  val mem_rs2_val = Bool(OUTPUT)
  // exception handling
  val exception = Bool(OUTPUT);
  val cause    = UInt(OUTPUT, 6);
  val badvaddr_wen = Bool(OUTPUT); // high for a load/store access fault
  val vec_irq_aux_wen = Bool(OUTPUT)
  // inputs from datapath
  val inst    = Bits(INPUT, 32);
  val jalr_eq = Bool(INPUT)
  val ex_br_type = Bits(OUTPUT, SZ_BR)
  val ex_br_taken = Bool(INPUT)
  val div_mul_rdy = Bool(INPUT)
  val mem_ll_wb = Bool(INPUT)
  val mem_ll_waddr = UInt(INPUT, 5)
  val ex_waddr = UInt(INPUT, 5);  // write addr from execute stage
  val mem_waddr = UInt(INPUT, 5); // write addr from memory stage
  val wb_waddr = UInt(INPUT, 5);  // write addr from writeback stage
  val status = new Status().asInput
  val fp_sboard_clr  = Bool(INPUT);
  val fp_sboard_clra = UInt(INPUT, 5);
  val pcr_replay = Bool(INPUT)
}

abstract trait DecodeConstants
{
  val xpr64 = Y;

  val decode_default =
                //                                                                                             fence.i
                //                    jalr                                            mul_val                  | eret
                //         fp_val     | renx2                                         | div_val                | | syscall
                //         | vec_val  | | renx1                     mem_val           | | wen            pcr   | | | privileged
                //   val   | | brtype | | | s_alu2   dw     alu     | mem_cmd mem_type| | | s_wa  s_wb   |     | | | | replay_next
                //   |     | | |      | | | |        |      |       | |         |     | | | |     |      |     | | | | |
                List(N,    X,X,BR_X,  X,X,X,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, X,X,X,WA_X, WB_X,  PCR.X,N,X,X,X,X)
                                        
  val table: Array[(UInt, List[UInt])]
}

object XDecode extends DecodeConstants
{
  val table = Array(
                //                                                                                               fence.i
                //                    jalr                                              mul_val                  | eret
                //         fp_val     | renx2                                           | div_val                | | syscall
                //         | vec_val  | | renx1                       mem_val           | | wen            pcr   | | | privileged
                //   val   | | brtype | | | s_alu2   dw     alu       | mem_cmd mem_type| | | s_wa  s_wb   |     | | | | replay_next
                //   |     | | |      | | | |        |      |         | |         |     | | | |     |      |     | | | | |
    BNE->       List(Y,    N,N,BR_NE, N,Y,Y,A2_BTYPE,DW_X,  FN_ADD,   N,M_X,      MT_X, N,N,N,WA_X, WB_X,  PCR.N,N,N,N,N,N),
    BEQ->       List(Y,    N,N,BR_EQ, N,Y,Y,A2_BTYPE,DW_X,  FN_ADD,   N,M_X,      MT_X, N,N,N,WA_X, WB_X,  PCR.N,N,N,N,N,N),
    BLT->       List(Y,    N,N,BR_LT, N,Y,Y,A2_BTYPE,DW_X,  FN_ADD,   N,M_X,      MT_X, N,N,N,WA_X, WB_X,  PCR.N,N,N,N,N,N),
    BLTU->      List(Y,    N,N,BR_LTU,N,Y,Y,A2_BTYPE,DW_X,  FN_ADD,   N,M_X,      MT_X, N,N,N,WA_X, WB_X,  PCR.N,N,N,N,N,N),
    BGE->       List(Y,    N,N,BR_GE, N,Y,Y,A2_BTYPE,DW_X,  FN_ADD,   N,M_X,      MT_X, N,N,N,WA_X, WB_X,  PCR.N,N,N,N,N,N),
    BGEU->      List(Y,    N,N,BR_GEU,N,Y,Y,A2_BTYPE,DW_X,  FN_ADD,   N,M_X,      MT_X, N,N,N,WA_X, WB_X,  PCR.N,N,N,N,N,N),
                                        
    J->         List(Y,    N,N,BR_J,  N,N,N,A2_JTYPE,DW_X,  FN_ADD,   N,M_X,      MT_X, N,N,N,WA_X, WB_X,  PCR.N,N,N,N,N,N),
    JAL->       List(Y,    N,N,BR_J,  N,N,N,A2_JTYPE,DW_X,  FN_ADD,   N,M_X,      MT_X, N,N,Y,WA_RA,WB_PC, PCR.N,N,N,N,N,N),
    JALR_C->    List(Y,    N,N,BR_N,  Y,N,Y,A2_ITYPE,DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,Y,WA_RD,WB_PC, PCR.N,N,N,N,N,N),
    JALR_J->    List(Y,    N,N,BR_N,  Y,N,Y,A2_ITYPE,DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,Y,WA_RD,WB_PC, PCR.N,N,N,N,N,N),
    JALR_R->    List(Y,    N,N,BR_N,  Y,N,Y,A2_ITYPE,DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,Y,WA_RD,WB_PC, PCR.N,N,N,N,N,N),
    AUIPC->     List(Y,    N,N,BR_N,  N,N,N,A2_LTYPE,DW_XPR,FN_OP2,   N,M_X,      MT_X, N,N,Y,WA_RD,WB_PC, PCR.N,N,N,N,N,N),
                                        
    LB->        List(Y,    N,N,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_ADD,   Y,M_XRD,    MT_B, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    LH->        List(Y,    N,N,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_ADD,   Y,M_XRD,    MT_H, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    LW->        List(Y,    N,N,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_ADD,   Y,M_XRD,    MT_W, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    LD->        List(xpr64,N,N,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_ADD,   Y,M_XRD,    MT_D, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    LBU->       List(Y,    N,N,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_ADD,   Y,M_XRD,    MT_BU,N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    LHU->       List(Y,    N,N,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_ADD,   Y,M_XRD,    MT_HU,N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    LWU->       List(xpr64,N,N,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_ADD,   Y,M_XRD,    MT_WU,N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    SB->        List(Y,    N,N,BR_N,  N,Y,Y,A2_BTYPE,DW_XPR,FN_ADD,   Y,M_XWR,    MT_B, N,N,N,WA_X, WB_ALU,PCR.N,N,N,N,N,N),
    SH->        List(Y,    N,N,BR_N,  N,Y,Y,A2_BTYPE,DW_XPR,FN_ADD,   Y,M_XWR,    MT_H, N,N,N,WA_X, WB_ALU,PCR.N,N,N,N,N,N),
    SW->        List(Y,    N,N,BR_N,  N,Y,Y,A2_BTYPE,DW_XPR,FN_ADD,   Y,M_XWR,    MT_W, N,N,N,WA_X, WB_ALU,PCR.N,N,N,N,N,N),
    SD->        List(xpr64,N,N,BR_N,  N,Y,Y,A2_BTYPE,DW_XPR,FN_ADD,   Y,M_XWR,    MT_D, N,N,N,WA_X, WB_ALU,PCR.N,N,N,N,N,N),
                                        
    AMOADD_W->  List(Y,    N,N,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD,   Y,M_XA_ADD, MT_W, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    AMOSWAP_W-> List(Y,    N,N,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD,   Y,M_XA_SWAP,MT_W, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    AMOAND_W->  List(Y,    N,N,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD,   Y,M_XA_AND, MT_W, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    AMOOR_W->   List(Y,    N,N,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD,   Y,M_XA_OR,  MT_W, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    AMOMIN_W->  List(Y,    N,N,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD,   Y,M_XA_MIN, MT_W, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    AMOMINU_W-> List(Y,    N,N,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD,   Y,M_XA_MINU,MT_W, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    AMOMAX_W->  List(Y,    N,N,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD,   Y,M_XA_MAX, MT_W, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    AMOMAXU_W-> List(Y,    N,N,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD,   Y,M_XA_MAXU,MT_W, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    AMOADD_D->  List(xpr64,N,N,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD,   Y,M_XA_ADD, MT_D, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    AMOSWAP_D-> List(xpr64,N,N,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD,   Y,M_XA_SWAP,MT_D, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    AMOAND_D->  List(xpr64,N,N,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD,   Y,M_XA_AND, MT_D, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    AMOOR_D->   List(xpr64,N,N,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD,   Y,M_XA_OR,  MT_D, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    AMOMIN_D->  List(xpr64,N,N,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD,   Y,M_XA_MIN, MT_D, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    AMOMINU_D-> List(xpr64,N,N,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD,   Y,M_XA_MINU,MT_D, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    AMOMAX_D->  List(xpr64,N,N,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD,   Y,M_XA_MAX, MT_D, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    AMOMAXU_D-> List(xpr64,N,N,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD,   Y,M_XA_MAXU,MT_D, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),

    LR_W->      List(Y,    N,N,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD,   Y,M_XLR,    MT_W, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    LR_D->      List(xpr64,N,N,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD,   Y,M_XLR,    MT_D, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    SC_W->      List(Y,    N,N,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD,   Y,M_XSC,    MT_W, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    SC_D->      List(xpr64,N,N,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD,   Y,M_XSC,    MT_D, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
                                        
    LUI->       List(Y,    N,N,BR_N,  N,N,N,A2_LTYPE,DW_XPR,FN_OP2,   N,M_X,      MT_X, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    ADDI->      List(Y,    N,N,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    SLTI ->     List(Y,    N,N,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_SLT,   N,M_X,      MT_X, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    SLTIU->     List(Y,    N,N,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_SLTU,  N,M_X,      MT_X, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    ANDI->      List(Y,    N,N,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_AND,   N,M_X,      MT_X, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    ORI->       List(Y,    N,N,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_OR,    N,M_X,      MT_X, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    XORI->      List(Y,    N,N,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_XOR,   N,M_X,      MT_X, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    SLLI->      List(Y,    N,N,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_SL,    N,M_X,      MT_X, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    SRLI->      List(Y,    N,N,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_SR,    N,M_X,      MT_X, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    SRAI->      List(Y,    N,N,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_SRA,   N,M_X,      MT_X, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    ADD->       List(Y,    N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    SUB->       List(Y,    N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_XPR,FN_SUB,   N,M_X,      MT_X, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    SLT->       List(Y,    N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_XPR,FN_SLT,   N,M_X,      MT_X, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    SLTU->      List(Y,    N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_XPR,FN_SLTU,  N,M_X,      MT_X, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    riscvAND->  List(Y,    N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_XPR,FN_AND,   N,M_X,      MT_X, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    riscvOR->   List(Y,    N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_XPR,FN_OR,    N,M_X,      MT_X, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    riscvXOR->  List(Y,    N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_XPR,FN_XOR,   N,M_X,      MT_X, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    SLL->       List(Y,    N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_XPR,FN_SL,    N,M_X,      MT_X, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    SRL->       List(Y,    N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_XPR,FN_SR,    N,M_X,      MT_X, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    SRA->       List(Y,    N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_XPR,FN_SRA,   N,M_X,      MT_X, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
                                        
    ADDIW->     List(xpr64,N,N,BR_N,  N,N,Y,A2_ITYPE,DW_32,FN_ADD,    N,M_X,      MT_X, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),   
    SLLIW->     List(xpr64,N,N,BR_N,  N,N,Y,A2_ITYPE,DW_32,FN_SL,     N,M_X,      MT_X, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    SRLIW->     List(xpr64,N,N,BR_N,  N,N,Y,A2_ITYPE,DW_32,FN_SR,     N,M_X,      MT_X, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    SRAIW->     List(xpr64,N,N,BR_N,  N,N,Y,A2_ITYPE,DW_32,FN_SRA,    N,M_X,      MT_X, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    ADDW->      List(xpr64,N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_32,FN_ADD,    N,M_X,      MT_X, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    SUBW->      List(xpr64,N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_32,FN_SUB,    N,M_X,      MT_X, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    SLLW->      List(xpr64,N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_32,FN_SL,     N,M_X,      MT_X, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    SRLW->      List(xpr64,N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_32,FN_SR,     N,M_X,      MT_X, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    SRAW->      List(xpr64,N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_32,FN_SRA,    N,M_X,      MT_X, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
                                        
    MUL->       List(Y,    N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_XPR,FN_MUL,   N,M_X,      MT_X, Y,N,Y,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    MULH->      List(Y,    N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_XPR,FN_MULH,  N,M_X,      MT_X, Y,N,Y,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    MULHU->     List(Y,    N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_XPR,FN_MULHU, N,M_X,      MT_X, Y,N,Y,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    MULHSU->    List(Y,    N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_XPR,FN_MULHSU,N,M_X,      MT_X, Y,N,Y,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    MULW->      List(xpr64,N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_32, FN_MUL,   N,M_X,      MT_X, Y,N,Y,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
                                        
    DIV->       List(Y,    N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_XPR,FN_DIV,   N,M_X,      MT_X, N,Y,Y,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    DIVU->      List(Y,    N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_XPR,FN_DIVU,  N,M_X,      MT_X, N,Y,Y,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    REM->       List(Y,    N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_XPR,FN_REM,   N,M_X,      MT_X, N,Y,Y,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    REMU->      List(Y,    N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_XPR,FN_REMU,  N,M_X,      MT_X, N,Y,Y,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    DIVW->      List(xpr64,N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_32, FN_DIV,   N,M_X,      MT_X, N,Y,Y,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    DIVUW->     List(xpr64,N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_32, FN_DIVU,  N,M_X,      MT_X, N,Y,Y,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    REMW->      List(xpr64,N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_32, FN_REM,   N,M_X,      MT_X, N,Y,Y,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    REMUW->     List(xpr64,N,N,BR_N,  N,Y,Y,A2_RTYPE,DW_32, FN_REMU,  N,M_X,      MT_X, N,Y,Y,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
                                        
    SYSCALL->   List(Y,    N,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_X, WB_X,  PCR.N,N,N,Y,N,N),
    SETPCR->    List(Y,    N,N,BR_N,  N,N,N,A2_ITYPE,DW_XPR,FN_OP2,   N,M_X,      MT_X, N,N,Y,WA_RD,WB_ALU,PCR.S,N,N,N,Y,N),
    CLEARPCR->  List(Y,    N,N,BR_N,  N,N,N,A2_ITYPE,DW_XPR,FN_OP2,   N,M_X,      MT_X, N,N,Y,WA_RD,WB_ALU,PCR.C,N,N,N,Y,N),
    ERET->      List(Y,    N,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_X, WB_X,  PCR.N,N,Y,N,Y,N),
    FENCE->     List(Y,    N,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     Y,M_FENCE,  MT_X, N,N,N,WA_X, WB_X,  PCR.N,N,N,N,N,N),
    FENCE_I->   List(Y,    N,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     Y,M_FENCE,  MT_X, N,N,N,WA_X, WB_X,  PCR.N,Y,N,N,N,Y),
    MFPCR->     List(Y,    N,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,Y,WA_RD,WB_X,  PCR.F,N,N,N,Y,N),
    MTPCR->     List(Y,    N,N,BR_N,  N,Y,N,A2_RTYPE,DW_XPR,FN_OP2,   N,M_X,      MT_X, N,N,Y,WA_RD,WB_ALU,PCR.T,N,N,N,Y,N),
    RDTIME->    List(Y,    N,N,BR_N,  N,N,N,A2_X,    DW_XPR,FN_X,     N,M_X,      MT_X, N,N,Y,WA_RD,WB_TSC,PCR.N,N,N,N,N,N),
    RDCYCLE->   List(Y,    N,N,BR_N,  N,N,N,A2_X,    DW_XPR,FN_X,     N,M_X,      MT_X, N,N,Y,WA_RD,WB_TSC,PCR.N,N,N,N,N,N),
    RDINSTRET-> List(Y,    N,N,BR_N,  N,N,N,A2_X,    DW_XPR,FN_X,     N,M_X,      MT_X, N,N,Y,WA_RD,WB_IRT,PCR.N,N,N,N,N,N))
}

object FDecode extends DecodeConstants
{
  val table = Array(
                //                                                                                               fence.i
                //                    jalr                                              mul_val                  | eret
                //         fp_val     | renx2                                           | div_val                | | syscall
                //         | vec_val  | | renx1                       mem_val           | | wen            pcr   | | | privileged
                //   val   | | brtype | | | s_alu2   dw     alu       | mem_cmd mem_type| | | s_wa  s_wb   |     | | | | replay_next
                //   |     | | |      | | | |        |      |         | |         |     | | | |     |      |     | | | | |
    FCVT_S_D->  List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FCVT_D_S->  List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FSGNJ_S->   List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FSGNJ_D->   List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FSGNJX_S->  List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FSGNJX_D->  List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FSGNJN_S->  List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FSGNJN_D->  List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FMIN_S->    List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FMIN_D->    List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FMAX_S->    List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FMAX_D->    List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FADD_S->    List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FADD_D->    List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FSUB_S->    List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FSUB_D->    List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FMUL_S->    List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FMUL_D->    List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FMADD_S->   List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FMADD_D->   List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FMSUB_S->   List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FMSUB_D->   List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FNMADD_S->  List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FNMADD_D->  List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FNMSUB_S->  List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FNMSUB_D->  List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    MFTX_S->    List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,Y,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    MFTX_D->    List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,Y,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FCVT_W_S->  List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,Y,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FCVT_W_D->  List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,Y,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FCVT_WU_S-> List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,Y,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FCVT_WU_D-> List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,Y,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FCVT_L_S->  List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,Y,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FCVT_L_D->  List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,Y,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FCVT_LU_S-> List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,Y,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FCVT_LU_D-> List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,Y,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FEQ_S->     List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,Y,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FEQ_D->     List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,Y,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FLT_S->     List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,Y,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FLT_D->     List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,Y,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FLE_S->     List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,Y,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FLE_D->     List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,Y,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    MXTF_S->    List(Y,    Y,N,BR_N,  N,N,Y,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    MXTF_D->    List(Y,    Y,N,BR_N,  N,N,Y,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FCVT_S_W->  List(Y,    Y,N,BR_N,  N,N,Y,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FCVT_D_W->  List(Y,    Y,N,BR_N,  N,N,Y,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FCVT_S_WU-> List(Y,    Y,N,BR_N,  N,N,Y,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FCVT_D_WU-> List(Y,    Y,N,BR_N,  N,N,Y,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FCVT_S_L->  List(Y,    Y,N,BR_N,  N,N,Y,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FCVT_D_L->  List(Y,    Y,N,BR_N,  N,N,Y,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FCVT_S_LU-> List(Y,    Y,N,BR_N,  N,N,Y,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FCVT_D_LU-> List(Y,    Y,N,BR_N,  N,N,Y,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    MFFSR->     List(Y,    Y,N,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,Y,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    MTFSR->     List(Y,    Y,N,BR_N,  N,N,Y,A2_X,    DW_X,  FN_X,     N,M_X,      MT_X, N,N,Y,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FLW->       List(Y,    Y,N,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_ADD,   Y,M_XRD,    MT_W, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    FLD->       List(Y,    Y,N,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_ADD,   Y,M_XRD,    MT_D, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    FSW->       List(Y,    Y,N,BR_N,  N,N,Y,A2_BTYPE,DW_XPR,FN_ADD,   Y,M_XWR,    MT_W, N,N,N,WA_X, WB_ALU,PCR.N,N,N,N,N,N),
    FSD->       List(Y,    Y,N,BR_N,  N,N,Y,A2_BTYPE,DW_XPR,FN_ADD,   Y,M_XWR,    MT_D, N,N,N,WA_X, WB_ALU,PCR.N,N,N,N,N,N))
}

object VDecode extends DecodeConstants
{
  val table = Array(
                //                                                                                             fence.i
                //                    jalr                                            mul_val                  | eret
                //         fp_val     | renx2                                         | div_val                | | syscall
                //         | vec_val  | | renx1                     mem_val           | | wen            pcr   | | | privileged
                //   val   | | brtype | | | s_alu2   dw     alu     | mem_cmd mem_type| | | s_wa  s_wb   |     | | | | replay_next
                //   |     | | |      | | | |        |      |       | |         |     | | | |     |      |     | | | | |
    VVCFGIVL->  List(Y,    N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,Y),
    VVCFG->     List(Y,    N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,N,Y),
    VSETVL->    List(Y,    N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,N,Y,WA_RD,WB_ALU,PCR.N,N,N,N,N,Y),
    VF->        List(Y,    N,Y,BR_N,  N,N,Y,A2_ITYPE,DW_XPR,FN_ADD, N,M_X,      MT_X, N,N,N,WA_X, WB_ALU,PCR.N,N,N,N,N,N),
    VMVV->      List(Y,    N,Y,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    VMSV->      List(Y,    N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    VFMVV->     List(Y,    N,Y,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,N,N,WA_RD,WB_X,  PCR.N,N,N,N,N,N),
    FENCE_V_L-> List(Y,    N,Y,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,N,N,WA_X, WB_X,  PCR.N,N,N,N,N,N),
    FENCE_V_G-> List(Y,    N,Y,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   Y,M_FENCE,  MT_X, N,N,N,WA_X, WB_X,  PCR.N,N,N,N,N,N),
    VLD->       List(Y,    N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    VLW->       List(Y,    N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    VLWU->      List(Y,    N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    VLH->       List(Y,    N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    VLHU->      List(Y,    N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    VLB->       List(Y,    N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    VLBU->      List(Y,    N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    VSD->       List(Y,    N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    VSW->       List(Y,    N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    VSH->       List(Y,    N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    VSB->       List(Y,    N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    VFLD->      List(Y,    N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    VFLW->      List(Y,    N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    VFSD->      List(Y,    N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    VFSW->      List(Y,    N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    VLSTD->     List(Y,    N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_D, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    VLSTW->     List(Y,    N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_D, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    VLSTWU->    List(Y,    N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_D, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    VLSTH->     List(Y,    N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_D, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    VLSTHU->    List(Y,    N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_D, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    VLSTB->     List(Y,    N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_D, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    VLSTBU->    List(Y,    N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_D, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    VSSTD->     List(Y,    N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_D, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    VSSTW->     List(Y,    N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_D, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    VSSTH->     List(Y,    N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_D, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    VSSTB->     List(Y,    N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_D, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    VFLSTD->    List(Y,    N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_D, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    VFLSTW->    List(Y,    N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_D, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    VFSSTD->    List(Y,    N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_D, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
    VFSSTW->    List(Y,    N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_D, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,N,N),
                                    
    VENQCMD->   List(Y,    N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,Y,N),
    VENQIMM1->  List(Y,    N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,Y,N),
    VENQIMM2->  List(Y,    N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,Y,N),
    VENQCNT->   List(Y,    N,Y,BR_N,  N,Y,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,Y,N),
    VXCPTEVAC-> List(Y,    N,Y,BR_N,  N,N,Y,A2_ZERO, DW_XPR,FN_ADD, N,M_X,      MT_X, N,N,N,WA_RD,WB_ALU,PCR.N,N,N,N,Y,N),
    VXCPTKILL-> List(Y,    N,Y,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,N,N,WA_X, WB_X,  PCR.N,N,N,N,Y,N),
    VXCPTHOLD-> List(Y,    N,Y,BR_N,  N,N,N,A2_X,    DW_X,  FN_X,   N,M_X,      MT_X, N,N,N,WA_X, WB_X,  PCR.N,N,N,N,Y,N))
}

class Control(implicit conf: RocketConfiguration) extends Module
{
  val io = new Bundle {
    val dpath   = new CtrlDpathIO
    val imem = new CPUFrontendIO()(conf.icache)
    val dmem = new HellaCacheIO()(conf.dcache)
    val dtlb_val = Bool(OUTPUT)
    val dtlb_kill = Bool(OUTPUT)
    val dtlb_rdy = Bool(INPUT)
    val dtlb_miss = Bool(INPUT)
    val xcpt_dtlb_ld = Bool(INPUT)
    val xcpt_dtlb_st = Bool(INPUT)
    val fpu = new CtrlFPUIO
    val vec_dpath = new CtrlDpathVecIO
    val vec_iface = new CtrlVecInterfaceIO
  }

  var decode_table = XDecode.table
  if (conf.fpu) decode_table ++= FDecode.table
  if (conf.vec) decode_table ++= VDecode.table

  val logic = DecodeLogic(io.dpath.inst, XDecode.decode_default, decode_table)
  val cs = logic.map { 
    case b if b.inputs.head.getClass == classOf[Bool] => b.toBool
    case u => u 
  }
  
  val (id_int_val: Bool) :: (id_fp_val: Bool) :: (id_vec_val: Bool) :: id_br_type :: (id_jalr: Bool) :: (id_renx2: Bool) :: (id_renx1: Bool) :: id_sel_alu2 :: (id_fn_dw: Bool) :: id_fn_alu :: cs0 = cs 
  val (id_mem_val: Bool) :: id_mem_cmd :: id_mem_type :: (id_mul_val: Bool) :: (id_div_val: Bool) :: (id_wen: Bool) :: id_sel_wa :: id_sel_wb :: cs1 = cs0
  val id_pcr :: (id_fence_i: Bool) :: (id_eret: Bool) :: (id_syscall: Bool) :: (id_privileged: Bool) :: (id_replay_next: Bool) :: Nil = cs1

  val id_raddr3 = io.dpath.inst(16,12);
  val id_raddr2 = io.dpath.inst(21,17);
  val id_raddr1 = io.dpath.inst(26,22);
  val id_waddr  = Mux(id_sel_wa === WA_RA, RA, io.dpath.inst(31,27));
  val id_load_use = Bool();
  
  val ex_reg_xcpt_interrupt  = RegReset(Bool(false))
  val ex_reg_valid           = RegReset(Bool(false))
  val ex_reg_eret            = RegReset(Bool(false))
  val ex_reg_wen             = RegReset(Bool(false))
  val ex_reg_fp_wen          = RegReset(Bool(false))
  val ex_reg_flush_inst      = RegReset(Bool(false))
  val ex_reg_jalr            = RegReset(Bool(false))
  val ex_reg_btb_hit         = RegReset(Bool(false))
  val ex_reg_div_mul_val     = RegReset(Bool(false))
  val ex_reg_mem_val         = RegReset(Bool(false))
  val ex_reg_xcpt            = RegReset(Bool(false))
  val ex_reg_fp_val          = RegReset(Bool(false))
  val ex_reg_vec_val         = RegReset(Bool(false))
  val ex_reg_replay_next     = RegReset(Bool(false))
  val ex_reg_load_use        = RegReset(Bool(false))
  val ex_reg_pcr             = RegReset(PCR.N)
  val ex_reg_br_type         = RegReset(BR_N)
  val ex_reg_mem_cmd         = Reg(Bits())
  val ex_reg_mem_type        = Reg(Bits())
  val ex_reg_cause           = Reg(UInt())

  val mem_reg_xcpt_interrupt  = RegReset(Bool(false))
  val mem_reg_valid           = RegReset(Bool(false))
  val mem_reg_eret            = RegReset(Bool(false))
  val mem_reg_wen             = RegReset(Bool(false))
  val mem_reg_fp_wen          = RegReset(Bool(false))
  val mem_reg_flush_inst      = RegReset(Bool(false))
  val mem_reg_div_mul_val     = RegReset(Bool(false))
  val mem_reg_mem_val         = RegReset(Bool(false))
  val mem_reg_xcpt            = RegReset(Bool(false))
  val mem_reg_fp_val          = RegReset(Bool(false))
  val mem_reg_vec_val         = RegReset(Bool(false))
  val mem_reg_replay          = RegReset(Bool(false))
  val mem_reg_replay_next     = RegReset(Bool(false))
  val mem_reg_pcr             = RegReset(PCR.N)
  val mem_reg_cause           = Reg(UInt())
  val mem_reg_slow_bypass     = Reg(Bool())

  val wb_reg_valid           = RegReset(Bool(false))
  val wb_reg_pcr             = RegReset(PCR.N)
  val wb_reg_wen             = RegReset(Bool(false))
  val wb_reg_fp_wen          = RegReset(Bool(false))
  val wb_reg_flush_inst      = RegReset(Bool(false))
  val wb_reg_mem_val         = RegReset(Bool(false))
  val wb_reg_eret            = RegReset(Bool(false))
  val wb_reg_xcpt            = RegReset(Bool(false))
  val wb_reg_replay          = RegReset(Bool(false))
  val wb_reg_cause           = Reg(UInt())
  val wb_reg_fp_val          = RegReset(Bool(false))
  val wb_reg_div_mul_val     = RegReset(Bool(false))

  val take_pc = Bool()
  val pc_taken = Reg(update = take_pc, reset = Bool(false))
  val take_pc_wb = Bool()
  val ctrl_killd = Bool()
  val ctrl_killx = Bool()
  val ctrl_killm = Bool()

  val sr = io.dpath.status
  var id_interrupts = (0 until sr.ip.getWidth).map(i => (sr.im(i) && sr.ip(i), UInt(CAUSE_INTERRUPT+i)))

  val (vec_replay, vec_stalld) = if (conf.vec) {
    // vector control
    val vec = Module(new rocketCtrlVec)

    io.vec_dpath <> vec.io.dpath
    io.vec_iface <> vec.io.iface

    vec.io.valid := wb_reg_valid
    vec.io.s := io.dpath.status.s
    vec.io.sr_ev := io.dpath.status.ev
    vec.io.exception := wb_reg_xcpt
    vec.io.eret := wb_reg_eret

    val vec_dec = Module(new rocketCtrlVecDecoder)
    vec_dec.io.inst := io.dpath.inst

    val s = io.dpath.status.s
    val mask_cmdq_ready = !vec_dec.io.sigs.enq_cmdq || s && io.vec_iface.vcmdq.ready || !s && io.vec_iface.vcmdq_user_ready
    val mask_ximm1q_ready = !vec_dec.io.sigs.enq_ximm1q || s && io.vec_iface.vximm1q.ready || !s && io.vec_iface.vximm1q_user_ready
    val mask_ximm2q_ready = !vec_dec.io.sigs.enq_ximm2q || s && io.vec_iface.vximm2q.ready || !s && io.vec_iface.vximm2q_user_ready
    val mask_cntq_ready = !vec_dec.io.sigs.enq_cntq || io.vec_iface.vcntq.ready
    val mask_pfcmdq_ready = !vec_dec.io.sigs.enq_pfcmdq || io.vec_iface.vpfcmdq.ready
    val mask_pfximm1q_ready = !vec_dec.io.sigs.enq_pfximm1q || io.vec_iface.vpfximm1q.ready
    val mask_pfximm2q_ready = !vec_dec.io.sigs.enq_pfximm2q || io.vec_iface.vpfximm2q.ready
    val mask_pfcntq_ready = !vec_dec.io.sigs.enq_pfcntq || io.vec_iface.vpfcntq.ready

    id_interrupts = id_interrupts :+ (vec.io.irq, vec.io.irq_cause)

    val stalld =
      id_vec_val && (
        !mask_cmdq_ready || !mask_ximm1q_ready || !mask_ximm2q_ready || !mask_cntq_ready ||
        !mask_pfcmdq_ready || !mask_pfximm1q_ready || !mask_pfximm2q_ready || !mask_pfcntq_ready ||
        vec_dec.io.sigs.vfence && !vec.io.vfence_ready)

    (vec.io.replay, stalld)
  } else (Bool(false), Bool(false))

  val (id_interrupt_unmasked, id_interrupt_cause) = checkExceptions(id_interrupts)
  val id_interrupt = io.dpath.status.et && id_interrupt_unmasked

  def checkExceptions(x: Seq[(Bool, UInt)]) =
    (x.map(_._1).reduce(_||_), PriorityMux(x))

  // executing ERET when traps are enabled causes an illegal instruction exception
  val illegal_inst = !id_int_val.toBool || (id_eret.toBool && io.dpath.status.et)
  // flush pipeline on PCR writes that may have side effects
  val id_pcr_flush = id_pcr != PCR.N && id_pcr != PCR.F &&
    id_raddr1 != PCR.K0 && id_raddr1 != PCR.K1 && id_raddr1 != PCR.EPC

  val (id_xcpt, id_cause) = checkExceptions(List(
    (id_interrupt,                            id_interrupt_cause),
    (io.imem.resp.bits.xcpt_ma,               UInt(0)),
    (io.imem.resp.bits.xcpt_if,               UInt(1)),
    (illegal_inst,                            UInt(2)),
    (id_privileged && !io.dpath.status.s,     UInt(3)),
    (id_fp_val && !io.dpath.status.ef,        UInt(4)),
    (id_syscall,                              UInt(6)),
    (id_vec_val && !io.dpath.status.ev,       UInt(12))))

  ex_reg_xcpt_interrupt := id_interrupt && !take_pc && io.imem.resp.valid
  when (id_xcpt) { ex_reg_cause := id_cause }

  when (ctrl_killd) {
    ex_reg_jalr        := Bool(false)
    ex_reg_btb_hit     := Bool(false);
    ex_reg_div_mul_val := Bool(false)
    ex_reg_mem_val     := Bool(false);
    ex_reg_valid       := Bool(false);
    ex_reg_wen         := Bool(false);
    ex_reg_fp_wen      := Bool(false);
    ex_reg_eret        := Bool(false);
    ex_reg_flush_inst  := Bool(false);  
    ex_reg_fp_val           := Bool(false);
    ex_reg_vec_val          := Bool(false);
    ex_reg_replay_next      := Bool(false);
    ex_reg_load_use         := Bool(false);
    ex_reg_pcr         := PCR.N
    ex_reg_br_type     := BR_N
    ex_reg_xcpt := Bool(false)
  } 
  .otherwise {
    ex_reg_br_type     := id_br_type;
    ex_reg_jalr        := id_jalr
    ex_reg_btb_hit     := io.imem.resp.bits.taken
    ex_reg_div_mul_val := id_mul_val || id_div_val
    ex_reg_mem_val     := id_mem_val.toBool;
    ex_reg_valid       := Bool(true)
    ex_reg_pcr         := id_pcr
    ex_reg_wen         := id_wen && id_waddr != UInt(0)
    ex_reg_fp_wen      := id_fp_val && io.fpu.dec.wen
    ex_reg_eret        := id_eret.toBool;
    ex_reg_flush_inst  := id_fence_i
    ex_reg_fp_val           := id_fp_val
    ex_reg_vec_val          := id_vec_val.toBool
    ex_reg_replay_next      := id_replay_next || id_pcr_flush
    ex_reg_load_use         := id_load_use;
    ex_reg_mem_cmd := id_mem_cmd
    ex_reg_mem_type := id_mem_type.toUInt
    ex_reg_xcpt := id_xcpt
  }

  // replay inst in ex stage
  val wb_dcache_miss = wb_reg_mem_val && !io.dmem.resp.valid
  val replay_ex_structural = ex_reg_mem_val && !io.dmem.req.ready ||
                             ex_reg_div_mul_val && !io.dpath.div_mul_rdy
  val replay_ex_other = wb_dcache_miss && ex_reg_load_use || mem_reg_replay_next
  val replay_ex = replay_ex_structural || replay_ex_other
  ctrl_killx := take_pc_wb || replay_ex
  val take_pc_ex = Mux(ex_reg_jalr,
    !(ex_reg_btb_hit && io.dpath.jalr_eq) && !replay_ex_other,
    ex_reg_btb_hit != io.dpath.ex_br_taken)
  // detect 2-cycle load-use delay for LB/LH/SC
  val ex_slow_bypass = ex_reg_mem_cmd === M_XSC || AVec(MT_B, MT_BU, MT_H, MT_HU).contains(ex_reg_mem_type)

  val (ex_xcpt, ex_cause) = checkExceptions(List(
    (ex_reg_xcpt_interrupt || ex_reg_xcpt, ex_reg_cause),
    (ex_reg_fp_val && io.fpu.illegal_rm,   UInt(2))))
  
  mem_reg_replay := replay_ex && !take_pc_wb;
  mem_reg_xcpt_interrupt := ex_reg_xcpt_interrupt && !take_pc_wb && !mem_reg_replay_next
  when (ex_xcpt) { mem_reg_cause := ex_cause }
  mem_reg_div_mul_val := ex_reg_div_mul_val && io.dpath.div_mul_rdy

  when (ctrl_killx) {
    mem_reg_valid       := Bool(false);
    mem_reg_pcr         := PCR.N
    mem_reg_wen         := Bool(false);
    mem_reg_fp_wen      := Bool(false);
    mem_reg_eret        := Bool(false);
    mem_reg_mem_val     := Bool(false);
    mem_reg_flush_inst  := Bool(false);
    mem_reg_fp_val := Bool(false)
    mem_reg_vec_val := Bool(false)
    mem_reg_replay_next := Bool(false)
    mem_reg_xcpt := Bool(false)
  }
  .otherwise {
    mem_reg_valid       := ex_reg_valid
    mem_reg_pcr         := ex_reg_pcr
    mem_reg_wen         := ex_reg_wen;
    mem_reg_fp_wen      := ex_reg_fp_wen;
    mem_reg_eret        := ex_reg_eret;
    mem_reg_mem_val     := ex_reg_mem_val;
    mem_reg_flush_inst  := ex_reg_flush_inst;
    mem_reg_fp_val := ex_reg_fp_val
    mem_reg_vec_val := ex_reg_vec_val
    mem_reg_replay_next := ex_reg_replay_next
    mem_reg_slow_bypass := ex_slow_bypass
    mem_reg_xcpt := ex_xcpt
  }

  val (mem_xcpt, mem_cause) = checkExceptions(List(
    (mem_reg_xcpt_interrupt || mem_reg_xcpt, mem_reg_cause),
    (mem_reg_mem_val && io.dmem.xcpt.ma.ld,  UInt( 8)),
    (mem_reg_mem_val && io.dmem.xcpt.ma.st,  UInt( 9)),
    (mem_reg_mem_val && io.dmem.xcpt.pf.ld,  UInt(10)),
    (mem_reg_mem_val && io.dmem.xcpt.pf.st,  UInt(11))))

  val fpu_kill_mem = mem_reg_fp_val && io.fpu.nack_mem
  val ll_wb_kill_mem = io.dpath.mem_ll_wb && (mem_reg_wen || mem_reg_fp_wen || mem_reg_vec_val || mem_reg_pcr != PCR.N)
  val replay_mem  = ll_wb_kill_mem || mem_reg_replay || fpu_kill_mem
  val killm_common = ll_wb_kill_mem || take_pc_wb || mem_reg_xcpt || !mem_reg_valid
  ctrl_killm := killm_common || mem_xcpt || fpu_kill_mem

  wb_reg_replay := replay_mem && !take_pc_wb
  wb_reg_xcpt := mem_xcpt && !take_pc_wb
  when (mem_xcpt) { wb_reg_cause := mem_cause }

  when (ctrl_killm) {
    wb_reg_valid       := Bool(false)
    wb_reg_pcr         := PCR.N
    wb_reg_wen         := Bool(false);
    wb_reg_fp_wen      := Bool(false);
    wb_reg_eret        := Bool(false);
    wb_reg_flush_inst  := Bool(false);
    wb_reg_mem_val     := Bool(false)
    wb_reg_div_mul_val := Bool(false);
    wb_reg_fp_val      := Bool(false)
  }
  .otherwise {
    wb_reg_valid       := mem_reg_valid
    wb_reg_pcr         := mem_reg_pcr
    wb_reg_wen         := mem_reg_wen;
    wb_reg_fp_wen      := mem_reg_fp_wen;
    wb_reg_eret        := mem_reg_eret && !mem_reg_replay
    wb_reg_flush_inst  := mem_reg_flush_inst;
    wb_reg_mem_val     := mem_reg_mem_val
    wb_reg_div_mul_val := mem_reg_div_mul_val
    wb_reg_fp_val      := mem_reg_fp_val
  }

  val replay_wb = io.dmem.resp.bits.nack || wb_reg_replay || vec_replay || io.dpath.pcr_replay

  class Scoreboard(n: Int)
  {
    val r = RegReset(Bits(0, n))
    var next = r
    var ens = Bool(false)
    def apply(addr: UInt) = r(addr)
    def set(en: Bool, addr: UInt): Unit = update(en, next | mask(en, addr))
    def clear(en: Bool, addr: UInt): Unit = update(en, next & ~mask(en, addr))
    private def mask(en: Bool, addr: UInt) = Mux(en, UInt(1) << addr, UInt(0))
    private def update(en: Bool, update: UInt) = {
      next = update
      ens = ens || en
      when (ens) { r := next }
    }
  }

  val sboard = new Scoreboard(32)
  sboard.set((wb_reg_div_mul_val || wb_dcache_miss) && io.dpath.wb_wen, io.dpath.wb_waddr)
  sboard.clear(io.dpath.mem_ll_wb, io.dpath.mem_ll_waddr)

  val id_stall_fpu = if (conf.fpu) {
    val fp_sboard = new Scoreboard(32)
    fp_sboard.set((wb_dcache_miss && wb_reg_fp_wen || io.fpu.sboard_set) && !replay_wb, io.dpath.wb_waddr)
    fp_sboard.clear(io.dpath.fp_sboard_clr, io.dpath.fp_sboard_clra)
    fp_sboard.clear(io.fpu.sboard_clr, io.fpu.sboard_clra)

    io.fpu.dec.ren1 && fp_sboard(id_raddr1) ||
    io.fpu.dec.ren2 && fp_sboard(id_raddr2) ||
    io.fpu.dec.ren3 && fp_sboard(id_raddr3) ||
    io.fpu.dec.wen  && fp_sboard(id_waddr)
  } else Bool(false)

	// write cause to PCR on an exception
	io.dpath.exception := wb_reg_xcpt
	io.dpath.cause := wb_reg_cause
	io.dpath.badvaddr_wen := wb_reg_xcpt && (wb_reg_cause === UInt(10) || wb_reg_cause === UInt(11))
  io.dpath.vec_irq_aux_wen := wb_reg_xcpt && wb_reg_cause >= UInt(24) && wb_reg_cause < UInt(32)

  // control transfer from ex/wb
  take_pc_wb := replay_wb || wb_reg_xcpt || wb_reg_eret
  take_pc := take_pc_ex || take_pc_wb;

  io.dpath.sel_pc :=
    Mux(wb_reg_xcpt,      PC_PCR, // exception
    Mux(wb_reg_eret,      PC_PCR, // eret instruction
    Mux(replay_wb,        PC_WB,  // replay
    Mux(ex_reg_jalr,      PC_EX,  // JALR
    Mux(!ex_reg_btb_hit,  PC_EX,  // mispredicted taken branch
        PC_EX4)))))               // mispredicted not taken branch

  io.imem.req.bits.mispredict := !take_pc_wb && take_pc_ex
  io.imem.req.bits.taken := !ex_reg_btb_hit || ex_reg_jalr
  io.imem.req.valid  := take_pc

  // stall for RAW/WAW hazards on PCRs, loads, AMOs, and mul/div in execute stage.
  val data_hazard_ex = ex_reg_wen &&
    (id_renx1.toBool && id_raddr1 === io.dpath.ex_waddr ||
     id_renx2.toBool && id_raddr2 === io.dpath.ex_waddr ||
     id_wen.toBool   && id_waddr  === io.dpath.ex_waddr)
  val fp_data_hazard_ex = ex_reg_fp_wen &&
    (io.fpu.dec.ren1 && id_raddr1 === io.dpath.ex_waddr ||
     io.fpu.dec.ren2 && id_raddr2 === io.dpath.ex_waddr ||
     io.fpu.dec.ren3 && id_raddr3 === io.dpath.ex_waddr ||
     io.fpu.dec.wen  && id_waddr  === io.dpath.ex_waddr)
  val id_ex_hazard = data_hazard_ex && (ex_reg_pcr != PCR.N || ex_reg_mem_val || ex_reg_div_mul_val || ex_reg_fp_val) ||
                     fp_data_hazard_ex && (ex_reg_mem_val || ex_reg_fp_val)
    
  // stall for RAW/WAW hazards on PCRs, LB/LH, and mul/div in memory stage.
  val mem_mem_cmd_bh =
    if (conf.fastLoadWord) Bool(!conf.fastLoadByte) && mem_reg_slow_bypass
    else Bool(true)
  val data_hazard_mem = mem_reg_wen &&
    (id_raddr1 != UInt(0) && id_renx1 && id_raddr1 === io.dpath.mem_waddr ||
     id_raddr2 != UInt(0) && id_renx2 && id_raddr2 === io.dpath.mem_waddr ||
     id_waddr  != UInt(0) && id_wen   && id_waddr  === io.dpath.mem_waddr)
  val fp_data_hazard_mem = mem_reg_fp_wen &&
    (io.fpu.dec.ren1 && id_raddr1 === io.dpath.mem_waddr ||
     io.fpu.dec.ren2 && id_raddr2 === io.dpath.mem_waddr ||
     io.fpu.dec.ren3 && id_raddr3 === io.dpath.mem_waddr ||
     io.fpu.dec.wen  && id_waddr  === io.dpath.mem_waddr)
  val id_mem_hazard = data_hazard_mem && (mem_reg_pcr != PCR.N || mem_reg_mem_val && mem_mem_cmd_bh || mem_reg_div_mul_val || mem_reg_fp_val) ||
                      fp_data_hazard_mem && mem_reg_fp_val
  id_load_use := mem_reg_mem_val && (data_hazard_mem || fp_data_hazard_mem)

  // stall for RAW/WAW hazards on load/AMO misses and mul/div in writeback.
  val data_hazard_wb = wb_reg_wen &&
    (id_raddr1 != UInt(0) && id_renx1 && (id_raddr1 === io.dpath.wb_waddr) ||
     id_raddr2 != UInt(0) && id_renx2 && (id_raddr2 === io.dpath.wb_waddr) ||
     id_waddr  != UInt(0) && id_wen   && (id_waddr  === io.dpath.wb_waddr))
  val fp_data_hazard_wb = wb_reg_fp_wen &&
    (io.fpu.dec.ren1 && id_raddr1 === io.dpath.wb_waddr ||
     io.fpu.dec.ren2 && id_raddr2 === io.dpath.wb_waddr ||
     io.fpu.dec.ren3 && id_raddr3 === io.dpath.wb_waddr ||
     io.fpu.dec.wen  && id_waddr  === io.dpath.wb_waddr)
  val id_wb_hazard = data_hazard_wb && (wb_dcache_miss || wb_reg_div_mul_val) ||
                     fp_data_hazard_wb && (wb_dcache_miss || wb_reg_fp_val)

  val id_sboard_hazard =
    (id_raddr1 != UInt(0) && id_renx1 && sboard(id_raddr1) ||
     id_raddr2 != UInt(0) && id_renx2 && sboard(id_raddr2) ||
     id_waddr  != UInt(0) && id_wen   && sboard(id_waddr))

  val ctrl_stalld =
    id_ex_hazard || id_mem_hazard || id_wb_hazard || id_sboard_hazard ||
    id_fp_val && id_stall_fpu ||
    id_mem_val && !io.dmem.req.ready ||
    vec_stalld
  val ctrl_draind = id_interrupt || ex_reg_replay_next
  ctrl_killd := !io.imem.resp.valid || take_pc || ctrl_stalld || ctrl_draind

  io.dpath.killd := take_pc || ctrl_stalld && !ctrl_draind
  io.imem.resp.ready := pc_taken || !ctrl_stalld || ctrl_draind
  io.imem.invalidate := wb_reg_flush_inst

  io.dpath.mem_load := mem_reg_mem_val && mem_reg_wen
  io.dpath.wb_load  := wb_reg_mem_val && wb_reg_wen
  io.dpath.ren2     := id_renx2.toBool;
  io.dpath.ren1     := id_renx1.toBool;
  io.dpath.sel_alu2 := id_sel_alu2.toUInt
  io.dpath.fn_dw    := id_fn_dw.toBool;
  io.dpath.fn_alu   := id_fn_alu.toUInt
  io.dpath.div_mul_val  := ex_reg_div_mul_val
  io.dpath.div_mul_kill := mem_reg_div_mul_val && killm_common
  io.dpath.ex_fp_val:= ex_reg_fp_val;
  io.dpath.mem_fp_val:= mem_reg_fp_val;
  io.dpath.ex_jalr  := ex_reg_jalr
  io.dpath.ex_wen   := ex_reg_wen;
  io.dpath.mem_wen  := mem_reg_wen;
  io.dpath.wb_wen   := wb_reg_wen && !replay_wb
  io.dpath.wb_valid := wb_reg_valid && !replay_wb
  io.dpath.sel_wa   := id_sel_wa.toBool;
  io.dpath.sel_wb   := id_sel_wb.toUInt
  io.dpath.pcr      := wb_reg_pcr.toUInt
  io.dpath.eret := wb_reg_eret
  io.dpath.ex_mem_type := ex_reg_mem_type
  io.dpath.ex_br_type := ex_reg_br_type
  io.dpath.ex_rs2_val := ex_reg_mem_val && isWrite(ex_reg_mem_cmd) || ex_reg_vec_val
  io.dpath.mem_rs2_val := mem_reg_vec_val

  io.fpu.valid := !ctrl_killd && id_fp_val
  io.fpu.killx := ctrl_killx
  io.fpu.killm := killm_common

  io.dmem.req.valid     := ex_reg_mem_val
  io.dmem.req.bits.kill := killm_common || mem_xcpt
  io.dmem.req.bits.cmd  := ex_reg_mem_cmd
  io.dmem.req.bits.typ  := ex_reg_mem_type
  io.dmem.req.bits.phys := Bool(false)
}
