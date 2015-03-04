// See LICENSE for license details.

package rocket

import Chisel._
import Instructions._
import uncore.constants.MemoryOpConstants._
import ALU._
import Util._

class CtrlDpathIO extends CoreBundle
{
  // outputs to datapath
  val sel_pc   = UInt(OUTPUT, 3)
  val killd = Bool(OUTPUT)
  val killm = Bool(OUTPUT)
  val ren      = Vec.fill(2)(Bool(OUTPUT))
  val ex_ctrl = new IntCtrlSigs().asOutput
  val mem_ctrl = new IntCtrlSigs().asOutput
  val csr      = UInt(OUTPUT, 3)
  val sret  = Bool(OUTPUT)
  val ex_valid = Bool(OUTPUT)
  val wb_wen   = Bool(OUTPUT)
  val bypass = Vec.fill(2)(Bool(OUTPUT))
  val bypass_src = Vec.fill(2)(Bits(OUTPUT, SZ_BYP))
  val ll_ready = Bool(OUTPUT)
  // exception handling
  val retire = Bool(OUTPUT)
  val exception = Bool(OUTPUT)
  val cause    = UInt(OUTPUT, xLen)
  val badvaddr_wen = Bool(OUTPUT) // high for a load/store access fault
  // inputs from datapath
  val inst    = Bits(INPUT, 32)
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
                //               jal                                                               renf1             fence.i
                //               | jalr                                                            | renf2           | sret
                //         fp_val| | renx2                                                         | | renf3         | | syscall
                //         | rocc| | | renx1     s_alu1                          mem_val           | | | wfd         | | |
                //   val   | | br| | | | s_alu2  |       imm    dw     alu       | mem_cmd mem_type| | | | div       | | |
                //   |     | | | | | | | |       |       |      |      |         | |         |     | | | | | wxd     | | | fence
                //   |     | | | | | | | |       |       |      |      |         | |         |     | | | | | | csr   | | | | amo
                //   |     | | | | | | | |       |       |      |      |         | |         |     | | | | | | |     | | | | |
                List(N,    X,X,X,X,X,X,X,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, X,X,X,X,X,X,CSR.X,X,X,X,X,X)

  val table: Array[(UInt, List[UInt])]
}

class IntCtrlSigs extends Bundle {
  val legal = Bool()
  val fp = Bool()
  val rocc = Bool()
  val branch = Bool()
  val jal = Bool()
  val jalr = Bool()
  val rxs2 = Bool()
  val rxs1 = Bool()
  val sel_alu2 = Bits(width = A2_X.getWidth)
  val sel_alu1 = Bits(width = A1_X.getWidth)
  val sel_imm = Bits(width = IMM_X.getWidth)
  val alu_dw = Bool()
  val alu_fn = Bits(width = FN_X.getWidth)
  val mem = Bool()
  val mem_cmd = Bits(width = M_SZ)
  val mem_type = Bits(width = MT_SZ)
  val rfs1 = Bool()
  val rfs2 = Bool()
  val rfs3 = Bool()
  val wfd = Bool()
  val div = Bool()
  val wxd = Bool()
  val csr = Bits(width = CSR.SZ)
  val fence_i = Bool()
  val sret = Bool()
  val scall = Bool()
  val fence = Bool()
  val amo = Bool()

  def decode(inst: UInt, table: Iterable[(UInt, List[UInt])]) = {
    val decoder = DecodeLogic(inst, XDecode.decode_default, table)
    Vec(legal, fp, rocc, branch, jal, jalr, rxs2, rxs1, sel_alu2, sel_alu1,
        sel_imm, alu_dw, alu_fn, mem, mem_cmd, mem_type,
        rfs1, rfs2, rfs3, wfd, div, wxd,
        csr, fence_i, sret, scall, fence, amo) := decoder
    this
  }
}

object XDecode extends DecodeConstants
{
  val table = Array(
                //               jal                                                               renf1             fence.i
                //               | jalr                                                            | renf2           | sret
                //         fp_val| | renx2                                                         | | renf3         | | syscall
                //         | rocc| | | renx1     s_alu1                          mem_val           | | | wfd         | | |
                //   val   | | br| | | | s_alu2  |       imm    dw     alu       | mem_cmd mem_type| | | | div       | | |
                //   |     | | | | | | | |       |       |      |      |         | |         |     | | | | | wxd     | | | fence
                //   |     | | | | | | | |       |       |      |      |         | |         |     | | | | | | csr   | | | | amo
                //   |     | | | | | | | |       |       |      |      |         | |         |     | | | | | | |     | | | | |
    BNE->       List(Y,    N,N,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SNE,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N,N,N),
    BEQ->       List(Y,    N,N,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SEQ,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N,N,N),
    BLT->       List(Y,    N,N,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SLT,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N,N,N),
    BLTU->      List(Y,    N,N,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SLTU,  N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N,N,N),
    BGE->       List(Y,    N,N,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SGE,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N,N,N),
    BGEU->      List(Y,    N,N,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SGEU,  N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N,N,N),

    JAL->       List(Y,    N,N,N,Y,N,N,N,A2_FOUR,A1_PC,  IMM_UJ,DW_X,  FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    JALR->      List(Y,    N,N,N,N,Y,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    AUIPC->     List(Y,    N,N,N,N,N,N,N,A2_IMM, A1_PC,  IMM_U, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),

    LB->        List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,    MT_B, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    LH->        List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,    MT_H, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    LW->        List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,    MT_W, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    LD->        List(xpr64,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,    MT_D, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    LBU->       List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,    MT_BU,N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    LHU->       List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,    MT_HU,N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    LWU->       List(xpr64,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,    MT_WU,N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    SB->        List(Y,    N,N,N,N,N,Y,Y,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,    MT_B, N,N,N,N,N,N,CSR.N,N,N,N,N,N),
    SH->        List(Y,    N,N,N,N,N,Y,Y,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,    MT_H, N,N,N,N,N,N,CSR.N,N,N,N,N,N),
    SW->        List(Y,    N,N,N,N,N,Y,Y,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,    MT_W, N,N,N,N,N,N,CSR.N,N,N,N,N,N),
    SD->        List(xpr64,N,N,N,N,N,Y,Y,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,    MT_D, N,N,N,N,N,N,CSR.N,N,N,N,N,N),

    AMOADD_W->  List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_ADD, MT_W, N,N,N,N,N,Y,CSR.N,N,N,N,N,Y),
    AMOXOR_W->  List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_XOR, MT_W, N,N,N,N,N,Y,CSR.N,N,N,N,N,Y),
    AMOSWAP_W-> List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_SWAP,MT_W, N,N,N,N,N,Y,CSR.N,N,N,N,N,Y),
    AMOAND_W->  List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_AND, MT_W, N,N,N,N,N,Y,CSR.N,N,N,N,N,Y),
    AMOOR_W->   List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_OR,  MT_W, N,N,N,N,N,Y,CSR.N,N,N,N,N,Y),
    AMOMIN_W->  List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MIN, MT_W, N,N,N,N,N,Y,CSR.N,N,N,N,N,Y),
    AMOMINU_W-> List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MINU,MT_W, N,N,N,N,N,Y,CSR.N,N,N,N,N,Y),
    AMOMAX_W->  List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MAX, MT_W, N,N,N,N,N,Y,CSR.N,N,N,N,N,Y),
    AMOMAXU_W-> List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MAXU,MT_W, N,N,N,N,N,Y,CSR.N,N,N,N,N,Y),
    AMOADD_D->  List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_ADD, MT_D, N,N,N,N,N,Y,CSR.N,N,N,N,N,Y),
    AMOSWAP_D-> List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_SWAP,MT_D, N,N,N,N,N,Y,CSR.N,N,N,N,N,Y),
    AMOXOR_D->  List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_XOR, MT_D, N,N,N,N,N,Y,CSR.N,N,N,N,N,Y),
    AMOAND_D->  List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_AND, MT_D, N,N,N,N,N,Y,CSR.N,N,N,N,N,Y),
    AMOOR_D->   List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_OR,  MT_D, N,N,N,N,N,Y,CSR.N,N,N,N,N,Y),
    AMOMIN_D->  List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MIN, MT_D, N,N,N,N,N,Y,CSR.N,N,N,N,N,Y),
    AMOMINU_D-> List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MINU,MT_D, N,N,N,N,N,Y,CSR.N,N,N,N,N,Y),
    AMOMAX_D->  List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MAX, MT_D, N,N,N,N,N,Y,CSR.N,N,N,N,N,Y),
    AMOMAXU_D-> List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MAXU,MT_D, N,N,N,N,N,Y,CSR.N,N,N,N,N,Y),

    LR_W->      List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XLR,    MT_W, N,N,N,N,N,Y,CSR.N,N,N,N,N,Y),
    LR_D->      List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XLR,    MT_D, N,N,N,N,N,Y,CSR.N,N,N,N,N,Y),
    SC_W->      List(Y,    N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XSC,    MT_W, N,N,N,N,N,Y,CSR.N,N,N,N,N,Y),
    SC_D->      List(xpr64,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XSC,    MT_D, N,N,N,N,N,Y,CSR.N,N,N,N,N,Y),

    LUI->       List(Y,    N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_U, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    ADDI->      List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    SLTI ->     List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SLT,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    SLTIU->     List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SLTU,  N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    ANDI->      List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_AND,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    ORI->       List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_OR,    N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    XORI->      List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_XOR,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    SLLI->      List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SL,    N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    SRLI->      List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SR,    N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    SRAI->      List(Y,    N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SRA,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    ADD->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    SUB->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SUB,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    SLT->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SLT,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    SLTU->      List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SLTU,  N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    AND->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_AND,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    OR->        List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_OR,    N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    XOR->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_XOR,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    SLL->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SL,    N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    SRL->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SR,    N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    SRA->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SRA,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),

    ADDIW->     List(xpr64,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_32,FN_ADD,    N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    SLLIW->     List(xpr64,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_32,FN_SL,     N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    SRLIW->     List(xpr64,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_32,FN_SR,     N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    SRAIW->     List(xpr64,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_32,FN_SRA,    N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    ADDW->      List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32,FN_ADD,    N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    SUBW->      List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32,FN_SUB,    N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    SLLW->      List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32,FN_SL,     N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    SRLW->      List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32,FN_SR,     N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    SRAW->      List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32,FN_SRA,    N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),

    MUL->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_MUL,   N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N,N,N),
    MULH->      List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_MULH,  N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N,N,N),
    MULHU->     List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_MULHU, N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N,N,N),
    MULHSU->    List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_MULHSU,N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N,N,N),
    MULW->      List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32, FN_MUL,   N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N,N,N),

    DIV->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_DIV,   N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N,N,N),
    DIVU->      List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_DIVU,  N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N,N,N),
    REM->       List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_REM,   N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N,N,N),
    REMU->      List(Y,    N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_REMU,  N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N,N,N),
    DIVW->      List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32, FN_DIV,   N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N,N,N),
    DIVUW->     List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32, FN_DIVU,  N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N,N,N),
    REMW->      List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32, FN_REM,   N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N,N,N),
    REMUW->     List(xpr64,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32, FN_REMU,  N,M_X,      MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N,N,N),

    SCALL->     List(Y,    N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,Y,N,N),
    SRET->      List(Y,    N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,Y,N,N,N),
    FENCE->     List(Y,    N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N,Y,N),
    FENCE_I->   List(Y,    N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,Y,N,N,N,N),
    CSRRW->     List(Y,    N,N,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.W,N,N,N,N,N),
    CSRRS->     List(Y,    N,N,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.S,N,N,N,N,N),
    CSRRC->     List(Y,    N,N,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.C,N,N,N,N,N),
    CSRRWI->    List(Y,    N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.W,N,N,N,N,N),
    CSRRSI->    List(Y,    N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.S,N,N,N,N,N),
    CSRRCI->    List(Y,    N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.C,N,N,N,N,N))
}

object FDecode extends DecodeConstants
{
  val table = Array(
                //               jal                                                               renf1             fence.i
                //               | jalr                                                            | renf2           | sret
                //         fp_val| | renx2                                                         | | renf3         | | syscall
                //         | rocc| | | renx1     s_alu1                          mem_val           | | | wfd         | | |
                //   val   | | br| | | | s_alu2  |       imm    dw     alu       | mem_cmd mem_type| | | | div       | | |
                //   |     | | | | | | | |       |       |      |      |         | |         |     | | | | | wxd     | | | fence
                //   |     | | | | | | | |       |       |      |      |         | |         |     | | | | | | csr   | | | | amo
                //   |     | | | | | | | |       |       |      |      |         | |         |     | | | | | | |     | | | | |
    FCVT_S_D->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,Y,N,N,CSR.N,N,N,N,N,N),
    FCVT_D_S->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,Y,N,N,CSR.N,N,N,N,N,N),
    FSGNJ_S->   List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N,N,N),
    FSGNJ_D->   List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N,N,N),
    FSGNJX_S->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N,N,N),
    FSGNJX_D->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N,N,N),
    FSGNJN_S->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N,N,N),
    FSGNJN_D->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N,N,N),
    FMIN_S->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N,N,N),
    FMIN_D->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N,N,N),
    FMAX_S->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N,N,N),
    FMAX_D->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N,N,N),
    FADD_S->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N,N,N),
    FADD_D->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N,N,N),
    FSUB_S->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N,N,N),
    FSUB_D->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N,N,N),
    FMUL_S->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N,N,N),
    FMUL_D->    List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N,N,N),
    FMADD_S->   List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,Y,Y,N,N,CSR.N,N,N,N,N,N),
    FMADD_D->   List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,Y,Y,N,N,CSR.N,N,N,N,N,N),
    FMSUB_S->   List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,Y,Y,N,N,CSR.N,N,N,N,N,N),
    FMSUB_D->   List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,Y,Y,N,N,CSR.N,N,N,N,N,N),
    FNMADD_S->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,Y,Y,N,N,CSR.N,N,N,N,N,N),
    FNMADD_D->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,Y,Y,N,N,CSR.N,N,N,N,N,N),
    FNMSUB_S->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,Y,Y,N,N,CSR.N,N,N,N,N,N),
    FNMSUB_D->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,Y,Y,N,N,CSR.N,N,N,N,N,N),
    FCLASS_S->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    FCLASS_D->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    FMV_X_S->   List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    FMV_X_D->   List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    FCVT_W_S->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    FCVT_W_D->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    FCVT_WU_S-> List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    FCVT_WU_D-> List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    FCVT_L_S->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    FCVT_L_D->  List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    FCVT_LU_S-> List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    FCVT_LU_D-> List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    FEQ_S->     List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,N,N,Y,CSR.N,N,N,N,N,N),
    FEQ_D->     List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,N,N,Y,CSR.N,N,N,N,N,N),
    FLT_S->     List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,N,N,Y,CSR.N,N,N,N,N,N),
    FLT_D->     List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,N,N,Y,CSR.N,N,N,N,N,N),
    FLE_S->     List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,N,N,Y,CSR.N,N,N,N,N,N),
    FLE_D->     List(Y,    Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, Y,Y,N,N,N,Y,CSR.N,N,N,N,N,N),
    FMV_S_X->   List(Y,    Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,Y,N,N,CSR.N,N,N,N,N,N),
    FMV_D_X->   List(Y,    Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,Y,N,N,CSR.N,N,N,N,N,N),
    FCVT_S_W->  List(Y,    Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,Y,N,N,CSR.N,N,N,N,N,N),
    FCVT_D_W->  List(Y,    Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,Y,N,N,CSR.N,N,N,N,N,N),
    FCVT_S_WU-> List(Y,    Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,Y,N,N,CSR.N,N,N,N,N,N),
    FCVT_D_WU-> List(Y,    Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,Y,N,N,CSR.N,N,N,N,N,N),
    FCVT_S_L->  List(Y,    Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,Y,N,N,CSR.N,N,N,N,N,N),
    FCVT_D_L->  List(Y,    Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,Y,N,N,CSR.N,N,N,N,N,N),
    FCVT_S_LU-> List(Y,    Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,Y,N,N,CSR.N,N,N,N,N,N),
    FCVT_D_LU-> List(Y,    Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,      MT_X, N,N,N,Y,N,N,CSR.N,N,N,N,N,N),
    FLW->       List(Y,    Y,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,    MT_W, N,N,N,Y,N,N,CSR.N,N,N,N,N,N),
    FLD->       List(Y,    Y,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,    MT_D, N,N,N,Y,N,N,CSR.N,N,N,N,N,N),
    FSW->       List(Y,    Y,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,    MT_W, N,Y,N,N,N,N,CSR.N,N,N,N,N,N),
    FSD->       List(Y,    Y,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,    MT_D, N,Y,N,N,N,N,CSR.N,N,N,N,N,N))
}

object RoCCDecode extends DecodeConstants
{
  val table = Array(
                        //               jal                                                               renf1             fence.i
                        //               | jalr                                                            | renf2           | sret
                        //         fp_val| | renx2                                                         | | renf3         | | syscall
                        //         | rocc| | | renx1     s_alu1                          mem_val           | | | wfd         | | |
                        //   val   | | br| | | | s_alu2  |       imm    dw     alu       | mem_cmd mem_type| | | | div       | | |
                        //   |     | | | | | | | |       |       |      |      |         | |         |     | | | | | wxd     | | | fence
                        //   |     | | | | | | | |       |       |      |      |         | |         |     | | | | | | csr   | | | | amo
                        //   |     | | | | | | | |       |       |      |      |         | |         |     | | | | | | |     | | | | |
    CUSTOM0->           List(Y,    N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N,N,N),
    CUSTOM0_RS1->       List(Y,    N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N,N,N),
    CUSTOM0_RS1_RS2->   List(Y,    N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N,N,N),
    CUSTOM0_RD->        List(Y,    N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    CUSTOM0_RD_RS1->    List(Y,    N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    CUSTOM0_RD_RS1_RS2->List(Y,    N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    CUSTOM1->           List(Y,    N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N,N,N),
    CUSTOM1_RS1->       List(Y,    N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N,N,N),
    CUSTOM1_RS1_RS2->   List(Y,    N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N,N,N),
    CUSTOM1_RD->        List(Y,    N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    CUSTOM1_RD_RS1->    List(Y,    N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    CUSTOM1_RD_RS1_RS2->List(Y,    N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    CUSTOM2->           List(Y,    N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N,N,N),
    CUSTOM2_RS1->       List(Y,    N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N,N,N),
    CUSTOM2_RS1_RS2->   List(Y,    N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N,N,N),
    CUSTOM2_RD->        List(Y,    N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    CUSTOM2_RD_RS1->    List(Y,    N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    CUSTOM2_RD_RS1_RS2->List(Y,    N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    CUSTOM3->           List(Y,    N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N,N,N),
    CUSTOM3_RS1->       List(Y,    N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N,N,N),
    CUSTOM3_RS1_RS2->   List(Y,    N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,N,CSR.N,N,N,N,N,N),
    CUSTOM3_RD->        List(Y,    N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    CUSTOM3_RD_RS1->    List(Y,    N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N),
    CUSTOM3_RD_RS1_RS2->List(Y,    N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,      MT_X, N,N,N,N,N,Y,CSR.N,N,N,N,N,N))
}

class Control extends CoreModule
{
  val io = new Bundle {
    val dpath   = new CtrlDpathIO
    val imem = new CPUFrontendIO
    val dmem = new HellaCacheIO
    val fpu = new CtrlFPUIO
    val rocc = new RoCCInterface().flip
  }

  var decode_table = XDecode.table
  if (!params(BuildFPU).isEmpty) decode_table ++= FDecode.table
  if (!params(BuildRoCC).isEmpty) decode_table ++= RoCCDecode.table

  val id_ctrl = new IntCtrlSigs().decode(io.dpath.inst, decode_table)
  val ex_ctrl = Reg(new IntCtrlSigs)
  val mem_ctrl = Reg(new IntCtrlSigs)
  val wb_ctrl = Reg(new IntCtrlSigs)

  val ex_reg_xcpt_interrupt  = Reg(Bool())
  val ex_reg_valid           = Reg(Bool())
  val ex_reg_btb_hit         = Reg(Bool())
  val ex_reg_btb_resp        = Reg(io.imem.btb_resp.bits.clone)
  val ex_reg_xcpt            = Reg(Bool())
  val ex_reg_flush_pipe      = Reg(Bool())
  val ex_reg_load_use        = Reg(Bool())
  val ex_reg_cause           = Reg(UInt())

  val mem_reg_xcpt_interrupt  = Reg(Bool())
  val mem_reg_valid           = Reg(Bool())
  val mem_reg_btb_hit         = Reg(Bool())
  val mem_reg_btb_resp        = Reg(io.imem.btb_resp.bits.clone)
  val mem_reg_xcpt            = Reg(Bool())
  val mem_reg_replay          = Reg(Bool())
  val mem_reg_flush_pipe      = Reg(Bool())
  val mem_reg_cause           = Reg(UInt())
  val mem_reg_slow_bypass     = Reg(Bool())

  val wb_reg_valid           = Reg(Bool())
  val wb_reg_xcpt            = Reg(Bool())
  val wb_reg_replay          = Reg(Bool())
  val wb_reg_cause           = Reg(UInt())

  val take_pc_wb = Bool()
  val mem_misprediction = io.dpath.mem_misprediction && mem_reg_valid && (mem_ctrl.branch || mem_ctrl.jalr || mem_ctrl.jal)
  val take_pc_mem = mem_reg_valid && (mem_misprediction || mem_reg_flush_pipe)
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
  var id_interrupts = (0 until sr.ip.getWidth).map(i => (sr.im(i) && sr.ip(i), UInt(BigInt(1) << (xLen-1) | i)))

  val (id_interrupt_unmasked, id_interrupt_cause) = checkExceptions(id_interrupts)
  val id_interrupt = io.dpath.status.ei && id_interrupt_unmasked

  def checkExceptions(x: Seq[(Bool, UInt)]) =
    (x.map(_._1).reduce(_||_), PriorityMux(x))

  val fp_csrs = CSRs.fcsr :: CSRs.frm :: CSRs.fflags :: Nil
  val legal_csrs = collection.mutable.LinkedHashSet(CSRs.all:_*)
  if(params(BuildFPU).isEmpty)
    legal_csrs --= fp_csrs

  val id_csr_addr = io.dpath.inst(31,20)
  val isLegalCSR = Vec.tabulate(1 << id_csr_addr.getWidth)(i => Bool(legal_csrs contains i))
  val id_csr_en = id_ctrl.csr != CSR.N
  val id_csr_fp = Bool(!params(BuildFPU).isEmpty) && id_csr_en && DecodeLogic(id_csr_addr, fp_csrs, CSRs.all.toSet -- fp_csrs)
  val id_csr_wen = id_raddr1 != UInt(0) || !Vec(CSR.S, CSR.C).contains(id_ctrl.csr)
  val id_csr_invalid = id_csr_en && !isLegalCSR(id_csr_addr)
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
  val id_fence_next = id_ctrl.fence || id_ctrl.amo && id_amo_rl
  val id_mem_busy = !io.dmem.ordered || io.dmem.req.valid
  val id_rocc_busy = Bool(!params(BuildRoCC).isEmpty) &&
    (io.rocc.busy || ex_reg_valid && ex_ctrl.rocc ||
     mem_reg_valid && mem_ctrl.rocc || wb_reg_valid && wb_ctrl.rocc)
  id_reg_fence := id_fence_next || id_reg_fence && id_mem_busy
  val id_do_fence = id_rocc_busy && id_ctrl.fence ||
    id_mem_busy && (id_ctrl.amo && id_amo_aq || id_ctrl.fence_i || id_reg_fence && (id_ctrl.mem || id_ctrl.rocc) || id_csr_flush)

  val (id_xcpt, id_cause) = checkExceptions(List(
    (id_interrupt,                                    id_interrupt_cause),
    (io.imem.resp.bits.xcpt_ma,                       UInt(Causes.misaligned_fetch)),
    (io.imem.resp.bits.xcpt_if,                       UInt(Causes.fault_fetch)),
    (!id_ctrl.legal || id_csr_invalid,                UInt(Causes.illegal_instruction)),
    (id_csr_privileged,                               UInt(Causes.privileged_instruction)),
    (id_ctrl.sret && !io.dpath.status.s,              UInt(Causes.privileged_instruction)),
    ((id_ctrl.fp || id_csr_fp) && !io.dpath.status.ef,UInt(Causes.fp_disabled)),
    (id_ctrl.scall,                                   UInt(Causes.syscall)),
    (id_ctrl.rocc && !io.dpath.status.er,             UInt(Causes.accelerator_disabled))))

  ex_reg_valid := !ctrl_killd
  ex_reg_xcpt := !ctrl_killd && id_xcpt
  ex_reg_xcpt_interrupt := id_interrupt && !take_pc && io.imem.resp.valid
  when (id_xcpt) { ex_reg_cause := id_cause }

  when (!ctrl_killd) {
    ex_ctrl := id_ctrl
    ex_reg_btb_hit := io.imem.btb_resp.valid
    when (io.imem.btb_resp.valid) { ex_reg_btb_resp := io.imem.btb_resp.bits }
    ex_reg_flush_pipe := id_ctrl.fence_i || id_csr_flush
    ex_reg_load_use := id_load_use
    ex_reg_xcpt := id_xcpt
  }

  // replay inst in ex stage
  val wb_dcache_miss = wb_ctrl.mem && !io.dmem.resp.valid
  val replay_ex_structural = ex_ctrl.mem && !io.dmem.req.ready ||
                             ex_ctrl.div && !io.dpath.div_mul_rdy
  val replay_ex_load_use = wb_dcache_miss && ex_reg_load_use
  val replay_ex = ex_reg_valid && (replay_ex_structural || replay_ex_load_use)
  ctrl_killx := take_pc_mem_wb || replay_ex || !ex_reg_valid
  // detect 2-cycle load-use delay for LB/LH/SC
  val ex_slow_bypass = ex_ctrl.mem_cmd === M_XSC || Vec(MT_B, MT_BU, MT_H, MT_HU).contains(ex_ctrl.mem_type)

  val (ex_xcpt, ex_cause) = checkExceptions(List(
    (ex_reg_xcpt_interrupt || ex_reg_xcpt, ex_reg_cause),
    (ex_ctrl.fp && io.fpu.illegal_rm,      UInt(Causes.illegal_instruction))))

  mem_reg_valid := !ctrl_killx
  mem_reg_replay := !take_pc_mem_wb && replay_ex
  mem_reg_xcpt := !ctrl_killx && ex_xcpt
  mem_reg_xcpt_interrupt := !take_pc_mem_wb && ex_reg_xcpt_interrupt
  when (ex_xcpt) { mem_reg_cause := ex_cause }

  when (!ctrl_killx) {
    mem_ctrl := ex_ctrl
    mem_reg_btb_hit := ex_reg_btb_hit
    when (ex_reg_btb_hit) { mem_reg_btb_resp := ex_reg_btb_resp }
    mem_reg_flush_pipe := ex_reg_flush_pipe
    mem_reg_slow_bypass := ex_slow_bypass
    mem_reg_xcpt := ex_xcpt
  }

  val (mem_xcpt, mem_cause) = checkExceptions(List(
    (mem_reg_xcpt_interrupt || mem_reg_xcpt,              mem_reg_cause),
    (mem_reg_valid && mem_ctrl.mem && io.dmem.xcpt.ma.st, UInt(Causes.misaligned_store)),
    (mem_reg_valid && mem_ctrl.mem && io.dmem.xcpt.ma.ld, UInt(Causes.misaligned_load)),
    (mem_reg_valid && mem_ctrl.mem && io.dmem.xcpt.pf.st, UInt(Causes.fault_store)),
    (mem_reg_valid && mem_ctrl.mem && io.dmem.xcpt.pf.ld, UInt(Causes.fault_load))))

  val dcache_kill_mem = mem_reg_valid && mem_ctrl.wxd && io.dmem.replay_next.valid // structural hazard on writeback port
  val fpu_kill_mem = mem_reg_valid && mem_ctrl.fp && io.fpu.nack_mem
  val replay_mem  = dcache_kill_mem || mem_reg_replay || fpu_kill_mem
  val killm_common = dcache_kill_mem || take_pc_wb || mem_reg_xcpt || !mem_reg_valid
  ctrl_killm := killm_common || mem_xcpt || fpu_kill_mem

  wb_reg_valid := !ctrl_killm
  when (!ctrl_killm) { wb_ctrl := mem_ctrl }
  wb_reg_replay := replay_mem && !take_pc_wb
  wb_reg_xcpt := mem_xcpt && !take_pc_wb
  when (mem_xcpt) { wb_reg_cause := mem_cause }

  val wb_set_sboard = wb_ctrl.div || wb_dcache_miss || wb_ctrl.rocc
  val replay_wb_common =
    io.dmem.resp.bits.nack || wb_reg_replay || io.dpath.csr_replay
  val wb_rocc_val = wb_reg_valid && wb_ctrl.rocc && !replay_wb_common
  val replay_wb = replay_wb_common || wb_reg_valid && wb_ctrl.rocc && !io.rocc.cmd.ready

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
  sboard.clear(io.dpath.ll_wen, io.dpath.ll_waddr)

  val id_stall_fpu = if (!params(BuildFPU).isEmpty) {
    val fp_sboard = new Scoreboard(32)
    fp_sboard.set((wb_dcache_miss && wb_ctrl.wfd || io.fpu.sboard_set) && io.dpath.retire, io.dpath.wb_waddr)
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
  take_pc_wb := replay_wb || wb_reg_xcpt || io.dpath.sret

  io.dpath.sel_pc :=
    Mux(wb_reg_xcpt,                  PC_PCR, // exception
    Mux(replay_wb,                    PC_WB,  // replay
    Mux(wb_reg_valid && wb_ctrl.sret, PC_PCR, // sret instruction
                                      PC_MEM)))

  io.imem.btb_update.valid := mem_reg_valid && io.dpath.mem_misprediction && ((mem_ctrl.branch && io.dpath.mem_br_taken) || mem_ctrl.jalr || mem_ctrl.jal) && !take_pc_wb
  io.imem.btb_update.bits.prediction.valid := mem_reg_btb_hit
  io.imem.btb_update.bits.prediction.bits := mem_reg_btb_resp
  io.imem.btb_update.bits.isJump := mem_ctrl.jal || mem_ctrl.jalr
  io.imem.btb_update.bits.isReturn := mem_ctrl.jalr && io.dpath.mem_rs1_ra

  io.imem.bht_update.valid := mem_reg_valid && mem_ctrl.branch && !take_pc_wb
  io.imem.bht_update.bits.taken := io.dpath.mem_br_taken
  io.imem.bht_update.bits.mispredict := io.dpath.mem_misprediction
  io.imem.bht_update.bits.prediction.valid := mem_reg_btb_hit
  io.imem.bht_update.bits.prediction.bits := mem_reg_btb_resp

  io.imem.ras_update.valid := io.imem.btb_update.bits.isJump && !take_pc_wb
  io.imem.ras_update.bits.isCall := mem_ctrl.wxd && io.dpath.mem_waddr(0)
  io.imem.ras_update.bits.isReturn := mem_ctrl.jalr && io.dpath.mem_rs1_ra
  io.imem.ras_update.bits.prediction.valid := mem_reg_btb_hit
  io.imem.ras_update.bits.prediction.bits := mem_reg_btb_resp

  io.imem.req.valid := take_pc

  val bypassDst = Array(id_raddr1, id_raddr2)
  val bypassSrc = Array.fill(NBYP)((Bool(true), UInt(0)))
  bypassSrc(BYP_EX) = (ex_reg_valid && ex_ctrl.wxd, io.dpath.ex_waddr)
  bypassSrc(BYP_MEM) = (mem_reg_valid && mem_ctrl.wxd && !mem_ctrl.mem, io.dpath.mem_waddr)
  bypassSrc(BYP_DC) = (mem_reg_valid && mem_ctrl.wxd, io.dpath.mem_waddr)

  val doBypass = bypassDst.map(d => bypassSrc.map(s => s._1 && s._2 === d))
  for (i <- 0 until io.dpath.bypass.size) {
    io.dpath.bypass(i) := doBypass(i).reduce(_||_)
    io.dpath.bypass_src(i) := PriorityEncoder(doBypass(i))
  }

  // stall for RAW/WAW hazards on PCRs, loads, AMOs, and mul/div in execute stage.
  val id_renx1_not0 = id_ctrl.rxs1 && id_raddr1 != UInt(0)
  val id_renx2_not0 = id_ctrl.rxs2 && id_raddr2 != UInt(0)
  val id_wen_not0 = id_ctrl.wxd && id_waddr != UInt(0)
  val ex_cannot_bypass = ex_ctrl.csr != CSR.N || ex_ctrl.jalr || ex_ctrl.mem || ex_ctrl.div || ex_ctrl.fp || ex_ctrl.rocc
  val data_hazard_ex = ex_ctrl.wxd &&
    (id_renx1_not0 && id_raddr1 === io.dpath.ex_waddr ||
     id_renx2_not0 && id_raddr2 === io.dpath.ex_waddr ||
     id_wen_not0   && id_waddr  === io.dpath.ex_waddr)
  val fp_data_hazard_ex = ex_ctrl.wfd &&
    (io.fpu.dec.ren1 && id_raddr1 === io.dpath.ex_waddr ||
     io.fpu.dec.ren2 && id_raddr2 === io.dpath.ex_waddr ||
     io.fpu.dec.ren3 && id_raddr3 === io.dpath.ex_waddr ||
     io.fpu.dec.wen  && id_waddr  === io.dpath.ex_waddr)
  val id_ex_hazard = ex_reg_valid && (data_hazard_ex && ex_cannot_bypass || fp_data_hazard_ex)

  // stall for RAW/WAW hazards on PCRs, LB/LH, and mul/div in memory stage.
  val mem_mem_cmd_bh =
    if (params(FastLoadWord)) Bool(!params(FastLoadByte)) && mem_reg_slow_bypass
    else Bool(true)
  val mem_cannot_bypass = mem_ctrl.csr != CSR.N || mem_ctrl.mem && mem_mem_cmd_bh || mem_ctrl.div || mem_ctrl.fp || mem_ctrl.rocc
  val data_hazard_mem = mem_ctrl.wxd &&
    (id_renx1_not0 && id_raddr1 === io.dpath.mem_waddr ||
     id_renx2_not0 && id_raddr2 === io.dpath.mem_waddr ||
     id_wen_not0   && id_waddr  === io.dpath.mem_waddr)
  val fp_data_hazard_mem = mem_ctrl.wfd &&
    (io.fpu.dec.ren1 && id_raddr1 === io.dpath.mem_waddr ||
     io.fpu.dec.ren2 && id_raddr2 === io.dpath.mem_waddr ||
     io.fpu.dec.ren3 && id_raddr3 === io.dpath.mem_waddr ||
     io.fpu.dec.wen  && id_waddr  === io.dpath.mem_waddr)
  val id_mem_hazard = mem_reg_valid && (data_hazard_mem && mem_cannot_bypass || fp_data_hazard_mem)
  id_load_use := mem_reg_valid && data_hazard_mem && mem_ctrl.mem

  // stall for RAW/WAW hazards on load/AMO misses and mul/div in writeback.
  val data_hazard_wb = wb_ctrl.wxd &&
    (id_renx1_not0 && id_raddr1 === io.dpath.wb_waddr ||
     id_renx2_not0 && id_raddr2 === io.dpath.wb_waddr ||
     id_wen_not0   && id_waddr  === io.dpath.wb_waddr)
  val fp_data_hazard_wb = wb_ctrl.wfd &&
    (io.fpu.dec.ren1 && id_raddr1 === io.dpath.wb_waddr ||
     io.fpu.dec.ren2 && id_raddr2 === io.dpath.wb_waddr ||
     io.fpu.dec.ren3 && id_raddr3 === io.dpath.wb_waddr ||
     io.fpu.dec.wen  && id_waddr  === io.dpath.wb_waddr)
  val id_wb_hazard = wb_reg_valid && (data_hazard_wb && wb_set_sboard || fp_data_hazard_wb)

  val id_sboard_hazard =
    (id_renx1_not0 && sboard.readBypassed(id_raddr1) ||
     id_renx2_not0 && sboard.readBypassed(id_raddr2) ||
     id_wen_not0   && sboard.readBypassed(id_waddr))

  sboard.set(wb_set_sboard && io.dpath.wb_wen, io.dpath.wb_waddr)

  val ctrl_stalld =
    id_ex_hazard || id_mem_hazard || id_wb_hazard || id_sboard_hazard ||
    id_ctrl.fp && id_stall_fpu ||
    id_ctrl.mem && !io.dmem.req.ready ||
    id_do_fence
  val ctrl_draind = id_interrupt
  ctrl_killd := !io.imem.resp.valid || take_pc || ctrl_stalld || ctrl_draind

  io.dpath.killd := take_pc || ctrl_stalld && !ctrl_draind
  io.imem.resp.ready := !ctrl_stalld || ctrl_draind
  io.imem.invalidate := wb_reg_valid && wb_ctrl.fence_i

  io.dpath.ren(1) := id_ctrl.rxs2
  io.dpath.ren(0) := id_ctrl.rxs1
  io.dpath.ex_ctrl := ex_ctrl
  io.dpath.mem_ctrl := mem_ctrl
  io.dpath.ex_valid := ex_reg_valid
  io.dpath.ll_ready := !(wb_reg_valid && wb_ctrl.wxd)
  io.dpath.retire := wb_reg_valid && !replay_wb
  io.dpath.wb_wen := io.dpath.retire && wb_ctrl.wxd
  io.dpath.csr := Mux(wb_reg_valid, wb_ctrl.csr, CSR.N)
  io.dpath.sret := wb_reg_valid && wb_ctrl.sret && !replay_wb
  io.dpath.killm := killm_common

  io.fpu.valid := !ctrl_killd && id_ctrl.fp
  io.fpu.killx := ctrl_killx
  io.fpu.killm := killm_common

  io.dmem.req.valid     := ex_reg_valid && ex_ctrl.mem
  io.dmem.req.bits.kill := killm_common || mem_xcpt
  io.dmem.req.bits.cmd  := ex_ctrl.mem_cmd
  io.dmem.req.bits.typ  := ex_ctrl.mem_type
  io.dmem.req.bits.phys := Bool(false)
  io.dmem.sret          := io.dpath.sret

  io.rocc.cmd.valid := wb_rocc_val
  io.rocc.exception := wb_reg_xcpt && sr.er
  io.rocc.s := sr.s
}
