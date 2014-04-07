package rocket

import Chisel._
import Instructions._
import uncore.constants.MemoryOpConstants._
import ALU._
import Util._

class CtrlDpathIO(implicit conf: RocketConfiguration) extends Bundle
{
  // outputs to datapath
  val sel_pc   = UInt(OUTPUT, 3)
  val killd    = Bool(OUTPUT)
  val ren      = Vec.fill(2)(Bool(OUTPUT))
  val sel_alu2 = UInt(OUTPUT, 3)
  val sel_alu1 = UInt(OUTPUT, 2)
  val sel_imm  = UInt(OUTPUT, 3)
  val fn_dw    = Bool(OUTPUT)
  val fn_alu   = UInt(OUTPUT, SZ_ALU_FN)
  val div_mul_val = Bool(OUTPUT)
  val div_mul_kill = Bool(OUTPUT)
  val div_val  = Bool(OUTPUT)
  val div_kill = Bool(OUTPUT)
  val csr      = UInt(OUTPUT, 3)
  val sret  = Bool(OUTPUT)
  val mem_load = Bool(OUTPUT)
  val wb_load = Bool(OUTPUT)
  val ex_fp_val= Bool(OUTPUT)
  val mem_fp_val= Bool(OUTPUT)
  val ex_wen = Bool(OUTPUT)
  val ex_valid = Bool(OUTPUT)
  val mem_jalr = Bool(OUTPUT)
  val mem_branch = Bool(OUTPUT)
  val mem_wen  = Bool(OUTPUT)
  val wb_wen   = Bool(OUTPUT)
  val ex_mem_type = Bits(OUTPUT, 3)
  val ex_rs2_val = Bool(OUTPUT)
  val ex_rocc_val = Bool(OUTPUT)
  val mem_rocc_val = Bool(OUTPUT)
  val bypass = Vec.fill(2)(Bool(OUTPUT))
  val bypass_src = Vec.fill(2)(Bits(OUTPUT, SZ_BYP))
  val ll_ready = Bool(OUTPUT)
  // exception handling
  val retire = Bool(OUTPUT)
  val exception = Bool(OUTPUT)
  val cause    = UInt(OUTPUT, conf.xprlen)
  val badvaddr_wen = Bool(OUTPUT) // high for a load/store access fault
  // inputs from datapath
  val inst    = Bits(INPUT, 32)
  val jalr_eq = Bool(INPUT)
  val mem_br_taken = Bool(INPUT)
  val mem_misprediction  = Bool(INPUT)
  val div_mul_rdy = Bool(INPUT)
  val ll_wen = Bool(INPUT)
  val ll_waddr = UInt(INPUT, 5)
  val ex_waddr = UInt(INPUT, 5)
  val mem_rs1_ra = Bool(INPUT)
  val mem_waddr = UInt(INPUT, 5)
  val wb_waddr = UInt(INPUT, 5)
  val status = new Status().asInput
  val fp_sboard_clr  = Bool(INPUT)
  val fp_sboard_clra = UInt(INPUT, 5)
  val csr_replay = Bool(INPUT)
}

abstract trait DecodeConstants
{
  val xpr64 = Y

  val decode_default =
                //               jal                                                                           fence.i
                //               | jalr                                                            mul_val     | sret
                //         fp_val| | renx2                                                         | div_val   | | syscall
                //         | rocc| | | renx1     s_alu1                          mem_val           | | wen     | | |
                //   val   | | br| | | | s_alu2  |       imm    dw     alu       | mem_cmd mem_type| | | csr   | | | replay_next
                //   |     | | | | | | | |       |       |      |      |         | |         |     | | | |     | | | | fence
                //   |     | | | | | | | |       |       |      |      |         | |         |     | | | |     | | | | | amo
                //   |     | | | | | | | |       |       |      |      |         | |         |     | | | |     | | | | | |
                List(N,    X,X,X,X,X,X,X,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, X,X,X,CSR.X,N,X,X,X,X,X)
                                        
  val table: Array[(UInt, List[UInt])]
}

object XDecode extends DecodeConstants
{
  val table = Array(
                //               jal                                                                           fence.i
                //               | jalr                                                            mul_val     | sret
                //         fp_val| | renx2                                                         | div_val   | | syscall
                //         | rocc| | | renx1     s_alu1                          mem_val           | | wen     | | |
                //   val   | | br| | | | s_alu2  |       imm    dw     alu       | mem_cmd mem_type| | | csr   | | | replay_next
                //   |     | | | | | | | |       |       |      |      |         | |         |     | | | |     | | | | fence
                //   |     | | | | | | | |       |       |      |      |         | |         |     | | | |     | | | | | amo
                //   |     | | | | | | | |       |       |      |      |         | |         |     | | | |     | | | | | |
    BNE->       List(Y,    N,N,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SNE,   N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    BEQ->       List(Y,    N,N,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SEQ,   N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    BLT->       List(Y,    N,N,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SLT,   N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    BLTU->      List(Y,    N,N,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SLTU,  N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    BGE->       List(Y,    N,N,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SGE,   N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    BGEU->      List(Y,    N,N,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SGEU,  N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),

    JAL->       List(Y,    N,N,N,Y,N,N,N,A2_FOUR,A1_PC,  IMM_UJ,DW_X,  FN_ADD,   N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    JALR->      List(Y,    N,N,N,N,Y,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    AUIPC->     List(Y,    N,N,N,N,N,N,N,A2_IMM, A1_PC,  IMM_U, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),

    LB->        List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,    MT_B, N,N,Y,CSR.N,N,N,N,N,N,N),
    LH->        List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,    MT_H, N,N,Y,CSR.N,N,N,N,N,N,N),
    LW->        List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,    MT_W, N,N,Y,CSR.N,N,N,N,N,N,N),
    LD->        List(xpr64,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,    MT_D, N,N,Y,CSR.N,N,N,N,N,N,N),
    LBU->       List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,    MT_BU,N,N,Y,CSR.N,N,N,N,N,N,N),
    LHU->       List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,    MT_HU,N,N,Y,CSR.N,N,N,N,N,N,N),
    LWU->       List(xpr64,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,    MT_WU,N,N,Y,CSR.N,N,N,N,N,N,N),
    SB->        List(Y,    N,N,N,N,N,Y,Y,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,    MT_B, N,N,N,CSR.N,N,N,N,N,N,N),
    SH->        List(Y,    N,N,N,N,N,Y,Y,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,    MT_H, N,N,N,CSR.N,N,N,N,N,N,N),
    SW->        List(Y,    N,N,N,N,N,Y,Y,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,    MT_W, N,N,N,CSR.N,N,N,N,N,N,N),
    SD->        List(xpr64,N,N,N,N,N,Y,Y,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,    MT_D, N,N,N,CSR.N,N,N,N,N,N,N),

    AMOADD_W->  List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_ADD, MT_W, N,N,Y,CSR.N,N,N,N,N,N,Y),
    AMOXOR_W->  List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_XOR, MT_W, N,N,Y,CSR.N,N,N,N,N,N,Y),
    AMOSWAP_W-> List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_SWAP,MT_W, N,N,Y,CSR.N,N,N,N,N,N,Y),
    AMOAND_W->  List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_AND, MT_W, N,N,Y,CSR.N,N,N,N,N,N,Y),
    AMOOR_W->   List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_OR,  MT_W, N,N,Y,CSR.N,N,N,N,N,N,Y),
    AMOMIN_W->  List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MIN, MT_W, N,N,Y,CSR.N,N,N,N,N,N,Y),
    AMOMINU_W-> List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MINU,MT_W, N,N,Y,CSR.N,N,N,N,N,N,Y),
    AMOMAX_W->  List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MAX, MT_W, N,N,Y,CSR.N,N,N,N,N,N,Y),
    AMOMAXU_W-> List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MAXU,MT_W, N,N,Y,CSR.N,N,N,N,N,N,Y),
    AMOADD_D->  List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_ADD, MT_D, N,N,Y,CSR.N,N,N,N,N,N,Y),
    AMOSWAP_D-> List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_SWAP,MT_D, N,N,Y,CSR.N,N,N,N,N,N,Y),
    AMOXOR_D->  List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_XOR, MT_D, N,N,Y,CSR.N,N,N,N,N,N,Y),
    AMOAND_D->  List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_AND, MT_D, N,N,Y,CSR.N,N,N,N,N,N,Y),
    AMOOR_D->   List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_OR,  MT_D, N,N,Y,CSR.N,N,N,N,N,N,Y),
    AMOMIN_D->  List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MIN, MT_D, N,N,Y,CSR.N,N,N,N,N,N,Y),
    AMOMINU_D-> List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MINU,MT_D, N,N,Y,CSR.N,N,N,N,N,N,Y),
    AMOMAX_D->  List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MAX, MT_D, N,N,Y,CSR.N,N,N,N,N,N,Y),
    AMOMAXU_D-> List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MAXU,MT_D, N,N,Y,CSR.N,N,N,N,N,N,Y),

    LR_W->      List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XLR,    MT_W, N,N,Y,CSR.N,N,N,N,N,N,Y),
    LR_D->      List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XLR,    MT_D, N,N,Y,CSR.N,N,N,N,N,N,Y),
    SC_W->      List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XSC,    MT_W, N,N,Y,CSR.N,N,N,N,N,N,Y),
    SC_D->      List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XSC,    MT_D, N,N,Y,CSR.N,N,N,N,N,N,Y),

    LUI->       List(Y,    N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_U, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    ADDI->      List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    SLTI ->     List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SLT,   N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    SLTIU->     List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SLTU,  N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    ANDI->      List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_AND,   N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    ORI->       List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_OR,    N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    XORI->      List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_XOR,   N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    SLLI->      List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SL,    N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    SRLI->      List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SR,    N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    SRAI->      List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SRA,   N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    ADD->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    SUB->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SUB,   N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    SLT->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SLT,   N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    SLTU->      List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SLTU,  N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    AND->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_AND,   N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    OR->        List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_OR,    N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    XOR->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_XOR,   N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    SLL->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SL,    N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    SRL->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SR,    N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    SRA->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SRA,   N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),

    ADDIW->     List(xpr64,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_32,FN_ADD,    N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),   
    SLLIW->     List(xpr64,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_32,FN_SL,     N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    SRLIW->     List(xpr64,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_32,FN_SR,     N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    SRAIW->     List(xpr64,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_32,FN_SRA,    N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    ADDW->      List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32,FN_ADD,    N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    SUBW->      List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32,FN_SUB,    N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    SLLW->      List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32,FN_SL,     N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    SRLW->      List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32,FN_SR,     N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    SRAW->      List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32,FN_SRA,    N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),

    MUL->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_MUL,   N,M_X,      MT_X, Y,N,Y,CSR.N,N,N,N,N,N,N),
    MULH->      List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_MULH,  N,M_X,      MT_X, Y,N,Y,CSR.N,N,N,N,N,N,N),
    MULHU->     List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_MULHU, N,M_X,      MT_X, Y,N,Y,CSR.N,N,N,N,N,N,N),
    MULHSU->    List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_MULHSU,N,M_X,      MT_X, Y,N,Y,CSR.N,N,N,N,N,N,N),
    MULW->      List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32, FN_MUL,   N,M_X,      MT_X, Y,N,Y,CSR.N,N,N,N,N,N,N),

    DIV->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_DIV,   N,M_X,      MT_X, N,Y,Y,CSR.N,N,N,N,N,N,N),
    DIVU->      List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_DIVU,  N,M_X,      MT_X, N,Y,Y,CSR.N,N,N,N,N,N,N),
    REM->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_REM,   N,M_X,      MT_X, N,Y,Y,CSR.N,N,N,N,N,N,N),
    REMU->      List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_REMU,  N,M_X,      MT_X, N,Y,Y,CSR.N,N,N,N,N,N,N),
    DIVW->      List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32, FN_DIV,   N,M_X,      MT_X, N,Y,Y,CSR.N,N,N,N,N,N,N),
    DIVUW->     List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32, FN_DIVU,  N,M_X,      MT_X, N,Y,Y,CSR.N,N,N,N,N,N,N),
    REMW->      List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32, FN_REM,   N,M_X,      MT_X, N,Y,Y,CSR.N,N,N,N,N,N,N),
    REMUW->     List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32, FN_REMU,  N,M_X,      MT_X, N,Y,Y,CSR.N,N,N,N,N,N,N),

    SCALL->     List(Y,    N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,Y,N,N,N),
    SRET->      List(Y,    N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,Y,N,N,N,N),
    FENCE->     List(Y,    N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,Y,N),
    FENCE_I->   List(Y,    N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,Y,N,N,Y,N,N),
    CSRRW->     List(Y,    N,N,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,Y,CSR.W,N,N,N,N,N,N),
    CSRRS->     List(Y,    N,N,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,Y,CSR.S,N,N,N,N,N,N),
    CSRRC->     List(Y,    N,N,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,Y,CSR.C,N,N,N,N,N,N),
    CSRRWI->    List(Y,    N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,Y,CSR.W,N,N,N,N,N,N),
    CSRRSI->    List(Y,    N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,Y,CSR.S,N,N,N,N,N,N),
    CSRRCI->    List(Y,    N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,Y,CSR.C,N,N,N,N,N,N))
}

object FDecode extends DecodeConstants
{
  val table = Array(
                //               jal                                                                           fence.i
                //               | jalr                                                            mul_val     | sret
                //         fp_val| | renx2                                                         | div_val   | | syscall
                //         | rocc| | | renx1     s_alu1                          mem_val           | | wen     | | |
                //   val   | | br| | | | s_alu2  |       imm    dw     alu       | mem_cmd mem_type| | | csr   | | | replay_next
                //   |     | | | | | | | |       |       |      |      |         | |         |     | | | |     | | | | fence
                //   |     | | | | | | | |       |       |      |      |         | |         |     | | | |     | | | | | amo
                //   |     | | | | | | | |       |       |      |      |         | |         |     | | | |     | | | | | |
    FCVT_S_D->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    FCVT_D_S->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    FSGNJ_S->   List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    FSGNJ_D->   List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    FSGNJX_S->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    FSGNJX_D->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    FSGNJN_S->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    FSGNJN_D->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    FMIN_S->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    FMIN_D->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    FMAX_S->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    FMAX_D->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    FADD_S->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    FADD_D->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    FSUB_S->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    FSUB_D->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    FMUL_S->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    FMUL_D->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    FMADD_S->   List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    FMADD_D->   List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    FMSUB_S->   List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    FMSUB_D->   List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    FNMADD_S->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    FNMADD_D->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    FNMSUB_S->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    FNMSUB_D->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    FCLASS_S->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    FCLASS_D->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    FMV_X_S->   List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    FMV_X_D->   List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    FCVT_W_S->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    FCVT_W_D->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    FCVT_WU_S-> List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    FCVT_WU_D-> List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    FCVT_L_S->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    FCVT_L_D->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    FCVT_LU_S-> List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    FCVT_LU_D-> List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    FEQ_S->     List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    FEQ_D->     List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    FLT_S->     List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    FLT_D->     List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    FLE_S->     List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    FLE_D->     List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    FMV_S_X->   List(Y,    Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    FMV_D_X->   List(Y,    Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    FCVT_S_W->  List(Y,    Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    FCVT_D_W->  List(Y,    Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    FCVT_S_WU-> List(Y,    Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    FCVT_D_WU-> List(Y,    Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    FCVT_S_L->  List(Y,    Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    FCVT_D_L->  List(Y,    Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    FCVT_S_LU-> List(Y,    Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    FCVT_D_LU-> List(Y,    Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    FLW->       List(Y,    Y,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,    MT_W, N,N,N,CSR.N,N,N,N,N,N,N),
    FLD->       List(Y,    Y,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,    MT_D, N,N,N,CSR.N,N,N,N,N,N,N),
    FSW->       List(Y,    Y,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,    MT_W, N,N,N,CSR.N,N,N,N,N,N,N),
    FSD->       List(Y,    Y,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,    MT_D, N,N,N,CSR.N,N,N,N,N,N,N))
}

object RoCCDecode extends DecodeConstants
{
  val table = Array(
                        //               jal                                                                           fence.i
                        //               | jalr                                                            mul_val     | sret
                        //         fp_val| | renx2                                                         | div_val   | | syscall
                        //         | rocc| | | renx1     s_alu1                          mem_val           | | wen     | | |
                        //   val   | | br| | | | s_alu2  |       imm    dw     alu       | mem_cmd mem_type| | | csr   | | | replay_next
                        //   |     | | | | | | | |       |       |      |      |         | |         |     | | | |     | | | | fence
                        //   |     | | | | | | | |       |       |      |      |         | |         |     | | | |     | | | | | amo
                        //   |     | | | | | | | |       |       |      |      |         | |         |     | | | |     | | | | | |
    CUSTOM0->           List(Y,    N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    CUSTOM0_RS1->       List(Y,    N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    CUSTOM0_RS1_RS2->   List(Y,    N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    CUSTOM0_RD->        List(Y,    N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    CUSTOM0_RD_RS1->    List(Y,    N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    CUSTOM0_RD_RS1_RS2->List(Y,    N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    CUSTOM1->           List(Y,    N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    CUSTOM1_RS1->       List(Y,    N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    CUSTOM1_RS1_RS2->   List(Y,    N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    CUSTOM1_RD->        List(Y,    N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    CUSTOM1_RD_RS1->    List(Y,    N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    CUSTOM1_RD_RS1_RS2->List(Y,    N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    CUSTOM2->           List(Y,    N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    CUSTOM2_RS1->       List(Y,    N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    CUSTOM2_RS1_RS2->   List(Y,    N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    CUSTOM2_RD->        List(Y,    N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    CUSTOM2_RD_RS1->    List(Y,    N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    CUSTOM2_RD_RS1_RS2->List(Y,    N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    CUSTOM3->           List(Y,    N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    CUSTOM3_RS1->       List(Y,    N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    CUSTOM3_RS1_RS2->   List(Y,    N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,CSR.N,N,N,N,N,N,N),
    CUSTOM3_RD->        List(Y,    N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    CUSTOM3_RD_RS1->    List(Y,    N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N),
    CUSTOM3_RD_RS1_RS2->List(Y,    N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,Y,CSR.N,N,N,N,N,N,N))
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
    val rocc = new RoCCInterface().flip
  }

  var decode_table = XDecode.table
  if (!conf.fpu.isEmpty) decode_table ++= FDecode.table
  if (!conf.rocc.isEmpty) decode_table ++= RoCCDecode.table

  val cs = DecodeLogic(io.dpath.inst, XDecode.decode_default, decode_table)
  
  val (id_int_val: Bool) :: (id_fp_val: Bool) :: (id_rocc_val: Bool) :: (id_branch: Bool) :: (id_jal: Bool) :: (id_jalr: Bool) :: (id_renx2: Bool) :: (id_renx1: Bool) :: cs0 = cs
  val id_sel_alu2 :: id_sel_alu1 :: id_sel_imm :: (id_fn_dw: Bool) :: id_fn_alu :: cs1 = cs0
  val (id_mem_val: Bool) :: id_mem_cmd :: id_mem_type :: (id_mul_val: Bool) :: (id_div_val: Bool) :: (id_wen: Bool) :: cs2 = cs1
  val id_csr :: (id_fence_i: Bool) :: (id_sret: Bool) :: (id_syscall: Bool) :: (id_replay_next: Bool) :: (id_fence: Bool) :: (id_amo: Bool) :: Nil = cs2
  
  val ex_reg_xcpt_interrupt  = Reg(Bool())
  val ex_reg_valid           = Reg(Bool())
  val ex_reg_branch          = Reg(Bool())
  val ex_reg_jal             = Reg(Bool())
  val ex_reg_jalr            = Reg(Bool())
  val ex_reg_btb_hit         = Reg(Bool())
  val ex_reg_btb_resp        = Reg(io.imem.btb_resp.bits.clone)
  val ex_reg_sret            = Reg(Bool())
  val ex_reg_wen             = Reg(Bool())
  val ex_reg_fp_wen          = Reg(Bool())
  val ex_reg_flush_inst      = Reg(Bool())
  val ex_reg_div_mul_val     = Reg(Bool())
  val ex_reg_mem_val         = Reg(Bool())
  val ex_reg_xcpt            = Reg(Bool())
  val ex_reg_fp_val          = Reg(Bool())
  val ex_reg_rocc_val        = Reg(Bool())
  val ex_reg_replay_next     = Reg(Bool())
  val ex_reg_load_use        = Reg(Bool())
  val ex_reg_csr             = Reg(UInt())
  val ex_reg_mem_cmd         = Reg(Bits())
  val ex_reg_mem_type        = Reg(Bits())
  val ex_reg_cause           = Reg(UInt())

  val mem_reg_xcpt_interrupt  = Reg(Bool())
  val mem_reg_valid           = Reg(Bool())
  val mem_reg_branch          = Reg(Bool())
  val mem_reg_jal             = Reg(Bool())
  val mem_reg_jalr            = Reg(Bool())
  val mem_reg_btb_hit         = Reg(Bool())
  val mem_reg_btb_resp        = Reg(io.imem.btb_resp.bits.clone)
  val mem_reg_sret            = Reg(Bool())
  val mem_reg_wen             = Reg(Bool())
  val mem_reg_fp_wen          = Reg(Bool())
  val mem_reg_flush_inst      = Reg(Bool())
  val mem_reg_div_mul_val     = Reg(Bool())
  val mem_reg_mem_val         = Reg(Bool())
  val mem_reg_xcpt            = Reg(Bool())
  val mem_reg_fp_val          = Reg(Bool())
  val mem_reg_rocc_val        = Reg(Bool())
  val mem_reg_replay          = Reg(Bool())
  val mem_reg_replay_next     = Reg(Bool())
  val mem_reg_csr             = Reg(UInt())
  val mem_reg_cause           = Reg(UInt())
  val mem_reg_slow_bypass     = Reg(Bool())

  val wb_reg_valid           = Reg(Bool())
  val wb_reg_csr             = Reg(UInt())
  val wb_reg_wen             = Reg(Bool())
  val wb_reg_fp_wen          = Reg(Bool())
  val wb_reg_rocc_val        = Reg(Bool())
  val wb_reg_flush_inst      = Reg(Bool())
  val wb_reg_mem_val         = Reg(Bool())
  val wb_reg_sret            = Reg(Bool())
  val wb_reg_xcpt            = Reg(Bool())
  val wb_reg_replay          = Reg(Bool())
  val wb_reg_cause           = Reg(UInt())
  val wb_reg_fp_val          = Reg(Bool())
  val wb_reg_div_mul_val     = Reg(Bool())

  val take_pc_wb = Bool()
  val take_pc_mem = io.dpath.mem_misprediction && (mem_reg_branch || mem_reg_jalr || mem_reg_jal)
  val take_pc_mem_wb = take_pc_wb || take_pc_mem
  val take_pc = take_pc_mem_wb
  val ctrl_killd = Bool()
  val ctrl_killx = Bool()
  val ctrl_killm = Bool()

  val id_raddr3 = io.dpath.inst(31,27)
  val id_raddr2 = io.dpath.inst(24,20)
  val id_raddr1 = io.dpath.inst(19,15)
  val id_waddr  = io.dpath.inst(11,7)
  val id_load_use = Bool()
  val id_reg_fence = Reg(init=Bool(false))

  val sr = io.dpath.status
  var id_interrupts = (0 until sr.ip.getWidth).map(i => (sr.im(i) && sr.ip(i), UInt(BigInt(1) << (conf.xprlen-1) | i)))

  val (id_interrupt_unmasked, id_interrupt_cause) = checkExceptions(id_interrupts)
  val id_interrupt = io.dpath.status.ei && id_interrupt_unmasked

  def checkExceptions(x: Seq[(Bool, UInt)]) =
    (x.map(_._1).reduce(_||_), PriorityMux(x))

  val fp_csrs = CSRs.fcsr :: CSRs.frm :: CSRs.fflags :: Nil
  val legal_csrs = collection.mutable.LinkedHashSet(CSRs.all:_*)
  if (conf.fpu.isEmpty)
    legal_csrs --= fp_csrs

  val id_csr_addr = io.dpath.inst(31,20)
  val id_csr_en = id_csr != CSR.N
  val id_csr_fp = Bool(!conf.fpu.isEmpty) && id_csr_en && DecodeLogic(id_csr_addr, fp_csrs, CSRs.all.toSet -- fp_csrs)
  val id_csr_wen = id_raddr1 != UInt(0) || !Vec(CSR.S, CSR.C).contains(id_csr)
  val id_csr_invalid = id_csr_en && !Vec(legal_csrs.map(UInt(_))).contains(id_csr_addr)
  val id_csr_privileged = id_csr_en &&
    (id_csr_addr(11,10) === UInt(3) && id_csr_wen ||
     id_csr_addr(11,10) === UInt(2) ||
     id_csr_addr(11,10) === UInt(1) && !io.dpath.status.s ||
     id_csr_addr(9,8) >= UInt(2) ||
     id_csr_addr(9,8) === UInt(1) && !io.dpath.status.s && id_csr_wen)
  // flush pipeline on CSR writes that may have side effects
  val id_csr_flush = {
    val safe_csrs = CSRs.sup0 :: CSRs.sup1 :: CSRs.epc :: Nil
    id_csr_en && id_csr_wen && !DecodeLogic(id_csr_addr, safe_csrs, legal_csrs -- safe_csrs)
  }

  // stall decode for fences (now, for AMO.aq; later, for AMO.rl and FENCE)
  val id_amo_aq = io.dpath.inst(26)
  val id_amo_rl = io.dpath.inst(25)
  val id_fence_next = id_fence || id_amo && id_amo_rl
  val id_mem_busy = !io.dmem.ordered || ex_reg_mem_val
  val id_rocc_busy = Bool(!conf.rocc.isEmpty) &&
    (io.rocc.busy || ex_reg_rocc_val || mem_reg_rocc_val || wb_reg_rocc_val)
  id_reg_fence := id_fence_next || id_reg_fence && id_mem_busy
  val id_do_fence = id_rocc_busy && id_fence ||
    id_mem_busy && (id_amo && id_amo_aq || id_fence_i || id_reg_fence && (id_mem_val || id_rocc_val) || id_csr_flush)

  val (id_xcpt, id_cause) = checkExceptions(List(
    (id_interrupt,                                    id_interrupt_cause),
    (io.imem.resp.bits.xcpt_ma,                       UInt(Causes.misaligned_fetch)),
    (io.imem.resp.bits.xcpt_if,                       UInt(Causes.fault_fetch)),
    (!id_int_val || id_csr_invalid,                   UInt(Causes.illegal_instruction)),
    (id_csr_privileged,                               UInt(Causes.privileged_instruction)),
    (id_sret && !io.dpath.status.s,                   UInt(Causes.privileged_instruction)),
    ((id_fp_val || id_csr_fp) && !io.dpath.status.ef, UInt(Causes.fp_disabled)),
    (id_syscall,                                      UInt(Causes.syscall)),
    (id_rocc_val && !io.dpath.status.er,              UInt(Causes.accelerator_disabled))))

  ex_reg_xcpt_interrupt := id_interrupt && !take_pc && io.imem.resp.valid
  when (id_xcpt) { ex_reg_cause := id_cause }

  when (ctrl_killd) {
    ex_reg_branch := false
    ex_reg_jal := false
    ex_reg_jalr := false
    ex_reg_btb_hit := false
    ex_reg_div_mul_val := Bool(false)
    ex_reg_mem_val     := Bool(false)
    ex_reg_valid       := Bool(false)
    ex_reg_wen         := Bool(false)
    ex_reg_fp_wen      := Bool(false)
    ex_reg_sret        := Bool(false)
    ex_reg_flush_inst  := Bool(false)  
    ex_reg_fp_val := Bool(false)
    ex_reg_rocc_val := Bool(false)
    ex_reg_replay_next := Bool(false)
    ex_reg_load_use := Bool(false)
    ex_reg_csr := CSR.N
    ex_reg_xcpt := Bool(false)
  } 
  .otherwise {
    ex_reg_branch := id_branch
    ex_reg_jal := id_jal
    ex_reg_jalr := id_jalr
    ex_reg_btb_hit := io.imem.btb_resp.valid
    when (io.imem.btb_resp.valid) { ex_reg_btb_resp := io.imem.btb_resp.bits }
    ex_reg_div_mul_val := id_mul_val || id_div_val
    ex_reg_mem_val     := id_mem_val.toBool
    ex_reg_valid       := Bool(true)
    ex_reg_csr         := id_csr
    ex_reg_wen         := id_wen
    ex_reg_fp_wen      := id_fp_val && io.fpu.dec.wen
    ex_reg_sret        := id_sret
    ex_reg_flush_inst  := id_fence_i
    ex_reg_fp_val := id_fp_val
    ex_reg_rocc_val := id_rocc_val.toBool
    ex_reg_replay_next := id_replay_next || id_csr_flush
    ex_reg_load_use := id_load_use
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
  ctrl_killx := take_pc_mem_wb || replay_ex
  // detect 2-cycle load-use delay for LB/LH/SC
  val ex_slow_bypass = ex_reg_mem_cmd === M_XSC || AVec(MT_B, MT_BU, MT_H, MT_HU).contains(ex_reg_mem_type)

  val (ex_xcpt, ex_cause) = checkExceptions(List(
    (ex_reg_xcpt_interrupt || ex_reg_xcpt, ex_reg_cause),
    (ex_reg_fp_val && io.fpu.illegal_rm,   UInt(Causes.illegal_instruction))))
  
  mem_reg_replay := !take_pc_mem_wb && replay_ex
  mem_reg_xcpt_interrupt := !take_pc_mem_wb && ex_reg_xcpt_interrupt && !mem_reg_replay_next
  when (ex_xcpt) { mem_reg_cause := ex_cause }
  mem_reg_div_mul_val := ex_reg_div_mul_val && io.dpath.div_mul_rdy

  when (ctrl_killx) {
    mem_reg_valid := false
    mem_reg_branch := false
    mem_reg_jal := false
    mem_reg_jalr := false
    mem_reg_csr         := CSR.N
    mem_reg_wen         := Bool(false)
    mem_reg_fp_wen      := Bool(false)
    mem_reg_sret        := Bool(false)
    mem_reg_mem_val     := Bool(false)
    mem_reg_flush_inst  := Bool(false)
    mem_reg_fp_val := Bool(false)
    mem_reg_rocc_val := Bool(false)
    mem_reg_replay_next := Bool(false)
    mem_reg_xcpt := Bool(false)
  }
  .otherwise {
    mem_reg_valid := ex_reg_valid
    mem_reg_branch := ex_reg_branch
    mem_reg_jal := ex_reg_jal
    mem_reg_jalr := ex_reg_jalr
    mem_reg_btb_hit := ex_reg_btb_hit
    when (ex_reg_btb_hit) { mem_reg_btb_resp := ex_reg_btb_resp }
    mem_reg_csr         := ex_reg_csr
    mem_reg_wen         := ex_reg_wen
    mem_reg_fp_wen      := ex_reg_fp_wen
    mem_reg_sret        := ex_reg_sret
    mem_reg_mem_val     := ex_reg_mem_val
    mem_reg_flush_inst  := ex_reg_flush_inst
    mem_reg_fp_val := ex_reg_fp_val
    mem_reg_rocc_val := ex_reg_rocc_val
    mem_reg_replay_next := ex_reg_replay_next
    mem_reg_slow_bypass := ex_slow_bypass
    mem_reg_xcpt := ex_xcpt
  }

  val (mem_xcpt, mem_cause) = checkExceptions(List(
    (mem_reg_xcpt_interrupt || mem_reg_xcpt, mem_reg_cause),
    (mem_reg_mem_val && io.dmem.xcpt.ma.ld,  UInt(Causes.misaligned_load)),
    (mem_reg_mem_val && io.dmem.xcpt.ma.st,  UInt(Causes.misaligned_store)),
    (mem_reg_mem_val && io.dmem.xcpt.pf.ld,  UInt(Causes.fault_load)),
    (mem_reg_mem_val && io.dmem.xcpt.pf.st,  UInt(Causes.fault_store))))

  val dcache_kill_mem = mem_reg_wen && io.dmem.replay_next.valid // structural hazard on writeback port
  val fpu_kill_mem = mem_reg_fp_val && io.fpu.nack_mem
  val replay_mem  = dcache_kill_mem || mem_reg_replay || fpu_kill_mem
  val killm_common = dcache_kill_mem || take_pc_wb || mem_reg_xcpt || !mem_reg_valid
  ctrl_killm := killm_common || mem_xcpt || fpu_kill_mem

  wb_reg_replay := replay_mem && !take_pc_wb
  wb_reg_xcpt := mem_xcpt && !take_pc_wb
  when (mem_xcpt) { wb_reg_cause := mem_cause }

  when (ctrl_killm) {
    wb_reg_valid       := Bool(false)
    wb_reg_csr         := CSR.N
    wb_reg_wen         := Bool(false)
    wb_reg_fp_wen      := Bool(false)
    wb_reg_sret        := Bool(false)
    wb_reg_flush_inst  := Bool(false)
    wb_reg_mem_val     := Bool(false)
    wb_reg_div_mul_val := Bool(false)
    wb_reg_fp_val      := Bool(false)
    wb_reg_rocc_val := Bool(false)
  }
  .otherwise {
    wb_reg_valid       := mem_reg_valid
    wb_reg_csr         := mem_reg_csr
    wb_reg_wen         := mem_reg_wen
    wb_reg_fp_wen      := mem_reg_fp_wen
    wb_reg_sret        := mem_reg_sret && !mem_reg_replay
    wb_reg_flush_inst  := mem_reg_flush_inst
    wb_reg_mem_val     := mem_reg_mem_val
    wb_reg_div_mul_val := mem_reg_div_mul_val
    wb_reg_fp_val      := mem_reg_fp_val
    wb_reg_rocc_val := mem_reg_rocc_val
  }

  val replay_wb_common = 
    io.dmem.resp.bits.nack || wb_reg_replay || io.dpath.csr_replay
  val wb_rocc_val = wb_reg_rocc_val && !replay_wb_common
  val replay_wb = replay_wb_common || wb_reg_rocc_val && !io.rocc.cmd.ready

  class Scoreboard(n: Int)
  {
    def set(en: Bool, addr: UInt): Unit = update(en, _next | mask(en, addr))
    def clear(en: Bool, addr: UInt): Unit = update(en, _next & ~mask(en, addr))
    def read(addr: UInt): Bool = r(addr)
    def readBypassed(addr: UInt): Bool = _next(addr)

    private val r = Reg(init=Bits(0, n))
    private var _next = r
    private var ens = Bool(false)
    private def mask(en: Bool, addr: UInt) = Mux(en, UInt(1) << addr, UInt(0))
    private def update(en: Bool, update: UInt) = {
      _next = update
      ens = ens || en
      when (ens) { r := _next }
    }
  }

  val sboard = new Scoreboard(32)
  sboard.set((wb_reg_div_mul_val || wb_dcache_miss || wb_reg_rocc_val) && io.dpath.wb_wen, io.dpath.wb_waddr)
  sboard.clear(io.dpath.ll_wen, io.dpath.ll_waddr)

  val id_stall_fpu = if (!conf.fpu.isEmpty) {
    val fp_sboard = new Scoreboard(32)
    fp_sboard.set((wb_dcache_miss && wb_reg_fp_wen || io.fpu.sboard_set) && !replay_wb, io.dpath.wb_waddr)
    fp_sboard.clear(io.dpath.fp_sboard_clr, io.dpath.fp_sboard_clra)
    fp_sboard.clear(io.fpu.sboard_clr, io.fpu.sboard_clra)

    id_csr_en && !io.fpu.fcsr_rdy ||
    io.fpu.dec.ren1 && fp_sboard.read(id_raddr1) ||
    io.fpu.dec.ren2 && fp_sboard.read(id_raddr2) ||
    io.fpu.dec.ren3 && fp_sboard.read(id_raddr3) ||
    io.fpu.dec.wen  && fp_sboard.read(id_waddr)
  } else Bool(false)

  // write CAUSE CSR on an exception
  io.dpath.exception := wb_reg_xcpt
  io.dpath.cause := wb_reg_cause
  io.dpath.badvaddr_wen := wb_reg_xcpt // don't care for non-memory exceptions

  // control transfer from ex/wb
  take_pc_wb := replay_wb || wb_reg_xcpt || wb_reg_sret

  io.dpath.sel_pc :=
    Mux(wb_reg_xcpt,      PC_PCR, // exception
    Mux(wb_reg_sret,      PC_PCR, // sret instruction
    Mux(replay_wb,        PC_WB,  // replay
                          PC_MEM)))

  io.imem.btb_update.valid := (mem_reg_branch || mem_reg_jal || mem_reg_jalr) && !take_pc_wb && !mem_reg_xcpt
  io.imem.btb_update.bits.prediction.valid := mem_reg_btb_hit
  io.imem.btb_update.bits.prediction.bits := mem_reg_btb_resp
  io.imem.btb_update.bits.taken := mem_reg_jal || mem_reg_branch && io.dpath.mem_br_taken
  io.imem.btb_update.bits.incorrectTarget := take_pc_mem
  io.imem.btb_update.bits.isJump := mem_reg_jal || mem_reg_jalr
  io.imem.btb_update.bits.isCall := mem_reg_wen && io.dpath.mem_waddr(0)
  io.imem.btb_update.bits.isReturn := mem_reg_jalr && io.dpath.mem_rs1_ra
  io.imem.req.valid  := take_pc

  val bypassDst = Array(id_raddr1, id_raddr2)
  val bypassSrc = Array.fill(NBYP)((Bool(true), UInt(0)))
  bypassSrc(BYP_EX) = (ex_reg_wen, io.dpath.ex_waddr)
  bypassSrc(BYP_MEM) = (mem_reg_wen && !mem_reg_mem_val, io.dpath.mem_waddr)
  bypassSrc(BYP_DC) = (mem_reg_wen, io.dpath.mem_waddr)

  val doBypass = bypassDst.map(d => bypassSrc.map(s => s._1 && s._2 === d))
  for (i <- 0 until io.dpath.bypass.size) {
    io.dpath.bypass(i) := doBypass(i).reduce(_||_)
    io.dpath.bypass_src(i) := PriorityEncoder(doBypass(i))
  }

  // stall for RAW/WAW hazards on PCRs, loads, AMOs, and mul/div in execute stage.
  val id_renx1_not0 = id_renx1 && id_raddr1 != UInt(0)
  val id_renx2_not0 = id_renx2 && id_raddr2 != UInt(0)
  val id_wen_not0 = id_wen && id_waddr != UInt(0)
  val data_hazard_ex = ex_reg_wen &&
    (id_renx1_not0 && id_raddr1 === io.dpath.ex_waddr ||
     id_renx2_not0 && id_raddr2 === io.dpath.ex_waddr ||
     id_wen_not0   && id_waddr  === io.dpath.ex_waddr)
  val fp_data_hazard_ex = ex_reg_fp_wen &&
    (io.fpu.dec.ren1 && id_raddr1 === io.dpath.ex_waddr ||
     io.fpu.dec.ren2 && id_raddr2 === io.dpath.ex_waddr ||
     io.fpu.dec.ren3 && id_raddr3 === io.dpath.ex_waddr ||
     io.fpu.dec.wen  && id_waddr  === io.dpath.ex_waddr)
  val id_ex_hazard = data_hazard_ex && (ex_reg_csr != CSR.N || ex_reg_jalr || ex_reg_mem_val || ex_reg_div_mul_val || ex_reg_fp_val || ex_reg_rocc_val) ||
                     fp_data_hazard_ex && (ex_reg_mem_val || ex_reg_fp_val)
    
  // stall for RAW/WAW hazards on PCRs, LB/LH, and mul/div in memory stage.
  val mem_mem_cmd_bh =
    if (conf.fastLoadWord) Bool(!conf.fastLoadByte) && mem_reg_slow_bypass
    else Bool(true)
  val data_hazard_mem = mem_reg_wen &&
    (id_renx1_not0 && id_raddr1 === io.dpath.mem_waddr ||
     id_renx2_not0 && id_raddr2 === io.dpath.mem_waddr ||
     id_wen_not0   && id_waddr  === io.dpath.mem_waddr)
  val fp_data_hazard_mem = mem_reg_fp_wen &&
    (io.fpu.dec.ren1 && id_raddr1 === io.dpath.mem_waddr ||
     io.fpu.dec.ren2 && id_raddr2 === io.dpath.mem_waddr ||
     io.fpu.dec.ren3 && id_raddr3 === io.dpath.mem_waddr ||
     io.fpu.dec.wen  && id_waddr  === io.dpath.mem_waddr)
  val id_mem_hazard = data_hazard_mem && (mem_reg_csr != CSR.N || mem_reg_mem_val && mem_mem_cmd_bh || mem_reg_div_mul_val || mem_reg_fp_val || mem_reg_rocc_val) ||
                      fp_data_hazard_mem && mem_reg_fp_val
  id_load_use := mem_reg_mem_val && (data_hazard_mem || fp_data_hazard_mem)

  // stall for RAW/WAW hazards on load/AMO misses and mul/div in writeback.
  val fp_data_hazard_wb = wb_reg_fp_wen &&
    (io.fpu.dec.ren1 && id_raddr1 === io.dpath.wb_waddr ||
     io.fpu.dec.ren2 && id_raddr2 === io.dpath.wb_waddr ||
     io.fpu.dec.ren3 && id_raddr3 === io.dpath.wb_waddr ||
     io.fpu.dec.wen  && id_waddr  === io.dpath.wb_waddr)
  val id_wb_hazard = fp_data_hazard_wb && (wb_dcache_miss || wb_reg_fp_val)

  val id_sboard_hazard =
    (id_renx1_not0 && sboard.readBypassed(id_raddr1) ||
     id_renx2_not0 && sboard.readBypassed(id_raddr2) ||
     id_wen_not0   && sboard.readBypassed(id_waddr))

  val ctrl_stalld =
    id_ex_hazard || id_mem_hazard || id_wb_hazard || id_sboard_hazard ||
    id_fp_val && id_stall_fpu ||
    id_mem_val && !io.dmem.req.ready ||
    id_do_fence
  val ctrl_draind = id_interrupt || ex_reg_replay_next
  ctrl_killd := !io.imem.resp.valid || take_pc || ctrl_stalld || ctrl_draind

  io.dpath.killd := take_pc || ctrl_stalld && !ctrl_draind
  io.imem.resp.ready := !ctrl_stalld || ctrl_draind
  io.imem.invalidate := wb_reg_flush_inst

  io.dpath.mem_load := mem_reg_mem_val && mem_reg_wen
  io.dpath.wb_load  := wb_reg_mem_val && wb_reg_wen
  io.dpath.ren(1) := id_renx2
  io.dpath.ren(0) := id_renx1
  io.dpath.sel_alu2 := id_sel_alu2.toUInt
  io.dpath.sel_alu1 := id_sel_alu1.toUInt
  io.dpath.sel_imm  := id_sel_imm.toUInt
  io.dpath.fn_dw    := id_fn_dw.toBool
  io.dpath.fn_alu   := id_fn_alu.toUInt
  io.dpath.div_mul_val  := ex_reg_div_mul_val
  io.dpath.div_mul_kill := mem_reg_div_mul_val && killm_common
  io.dpath.ex_fp_val:= ex_reg_fp_val
  io.dpath.mem_fp_val:= mem_reg_fp_val
  io.dpath.mem_jalr := mem_reg_jalr
  io.dpath.mem_branch := mem_reg_branch
  io.dpath.ex_wen   := ex_reg_wen
  io.dpath.ex_valid := ex_reg_valid
  io.dpath.mem_wen  := mem_reg_wen
  io.dpath.ll_ready := !wb_reg_wen
  io.dpath.wb_wen   := wb_reg_wen && !replay_wb
  io.dpath.retire := wb_reg_valid && !replay_wb
  io.dpath.csr := wb_reg_csr
  io.dpath.sret := wb_reg_sret
  io.dpath.ex_mem_type := ex_reg_mem_type
  io.dpath.ex_rs2_val := ex_reg_mem_val && isWrite(ex_reg_mem_cmd) || ex_reg_rocc_val
  io.dpath.ex_rocc_val := ex_reg_rocc_val
  io.dpath.mem_rocc_val := mem_reg_rocc_val

  io.fpu.valid := !ctrl_killd && id_fp_val
  io.fpu.killx := ctrl_killx
  io.fpu.killm := killm_common

  io.dmem.req.valid     := ex_reg_mem_val
  io.dmem.req.bits.kill := killm_common || mem_xcpt
  io.dmem.req.bits.cmd  := ex_reg_mem_cmd
  io.dmem.req.bits.typ  := ex_reg_mem_type
  io.dmem.req.bits.phys := Bool(false)

  io.rocc.cmd.valid := wb_rocc_val
  io.rocc.exception := wb_reg_xcpt && sr.er
  io.rocc.s := sr.s
}
