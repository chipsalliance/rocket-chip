// See LICENSE for license details

package rocket

import Chisel._
import Instructions._
import uncore.constants.MemoryOpConstants._
import ALU._
import cde.Parameters
import Util._

abstract trait DecodeConstants extends HasCoreParameters
{
  val table: Array[(BitPat, List[BitPat])]
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
  val fence = Bool()
  val amo = Bool()

  def default: List[BitPat] =
                //           jal                                                                 renf1             fence.i
                //   val     | jalr                                                              | renf2           |
                //   | fp_val| | renx2                                                           | | renf3         |
                //   | | rocc| | | renx1     s_alu1                          mem_val             | | | wfd         | 
                //   | | | br| | | | s_alu2  |       imm    dw     alu       | mem_cmd   mem_type| | | | div       | 
                //   | | | | | | | | |       |       |      |      |         | |           |     | | | | | wxd     | fence
                //   | | | | | | | | |       |       |      |      |         | |           |     | | | | | | csr   | | amo
                //   | | | | | | | | |       |       |      |      |         | |           |     | | | | | | |     | | |
                List(N,X,X,X,X,X,X,X,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, X,X,X,X,X,X,CSR.X,X,X,X)

  def decode(inst: UInt, table: Iterable[(BitPat, List[BitPat])]) = {
    val decoder = DecodeLogic(inst, default, table)
    val sigs = Seq(legal, fp, rocc, branch, jal, jalr, rxs2, rxs1, sel_alu2,
                   sel_alu1, sel_imm, alu_dw, alu_fn, mem, mem_cmd, mem_type,
                   rfs1, rfs2, rfs3, wfd, div, wxd, csr, fence_i, fence, amo)
    sigs zip decoder map {case(s,d) => s := d}
    this
  }
}

class IDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    BNE->       List(Y,N,N,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SNE,   N,M_X,        MT_X, N,N,N,N,N,N,CSR.N,N,N,N),
    BEQ->       List(Y,N,N,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SEQ,   N,M_X,        MT_X, N,N,N,N,N,N,CSR.N,N,N,N),
    BLT->       List(Y,N,N,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SLT,   N,M_X,        MT_X, N,N,N,N,N,N,CSR.N,N,N,N),
    BLTU->      List(Y,N,N,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SLTU,  N,M_X,        MT_X, N,N,N,N,N,N,CSR.N,N,N,N),
    BGE->       List(Y,N,N,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SGE,   N,M_X,        MT_X, N,N,N,N,N,N,CSR.N,N,N,N),
    BGEU->      List(Y,N,N,Y,N,N,Y,Y,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SGEU,  N,M_X,        MT_X, N,N,N,N,N,N,CSR.N,N,N,N),

    JAL->       List(Y,N,N,N,Y,N,N,N,A2_SIZE,A1_PC,  IMM_UJ,DW_XPR,FN_ADD,   N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    JALR->      List(Y,N,N,N,N,Y,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    AUIPC->     List(Y,N,N,N,N,N,N,N,A2_IMM, A1_PC,  IMM_U, DW_XPR,FN_ADD,   N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),

    LB->        List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      MT_B, N,N,N,N,N,Y,CSR.N,N,N,N),
    LH->        List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      MT_H, N,N,N,N,N,Y,CSR.N,N,N,N),
    LW->        List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      MT_W, N,N,N,N,N,Y,CSR.N,N,N,N),
    LBU->       List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      MT_BU,N,N,N,N,N,Y,CSR.N,N,N,N),
    LHU->       List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      MT_HU,N,N,N,N,N,Y,CSR.N,N,N,N),
    SB->        List(Y,N,N,N,N,N,Y,Y,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,      MT_B, N,N,N,N,N,N,CSR.N,N,N,N),
    SH->        List(Y,N,N,N,N,N,Y,Y,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,      MT_H, N,N,N,N,N,N,CSR.N,N,N,N),
    SW->        List(Y,N,N,N,N,N,Y,Y,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,      MT_W, N,N,N,N,N,N,CSR.N,N,N,N),

    LUI->       List(Y,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_U, DW_XPR,FN_ADD,   N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    ADDI->      List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    SLTI ->     List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SLT,   N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    SLTIU->     List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SLTU,  N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    ANDI->      List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_AND,   N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    ORI->       List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_OR,    N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    XORI->      List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_XOR,   N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    SLLI->      List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SL,    N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    SRLI->      List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SR,    N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    SRAI->      List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SRA,   N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    ADD->       List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    SUB->       List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SUB,   N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    SLT->       List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SLT,   N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    SLTU->      List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SLTU,  N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    AND->       List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_AND,   N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    OR->        List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_OR,    N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    XOR->       List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_XOR,   N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    SLL->       List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SL,    N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    SRL->       List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SR,    N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    SRA->       List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SRA,   N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),

    FENCE->     List(Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, N,N,N,N,N,N,CSR.N,N,Y,N),
    FENCE_I->   List(Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     Y,M_FLUSH_ALL,MT_X, N,N,N,N,N,N,CSR.N,Y,N,N),

    SCALL->     List(Y,N,N,N,N,N,N,X,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, N,N,N,N,N,N,CSR.I,N,N,N),
    SBREAK->    List(Y,N,N,N,N,N,N,X,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, N,N,N,N,N,N,CSR.I,N,N,N),
    MRET->      List(Y,N,N,N,N,N,N,X,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, N,N,N,N,N,N,CSR.I,N,N,N),
    WFI->       List(Y,N,N,N,N,N,N,X,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, N,N,N,N,N,N,CSR.I,N,N,N),
    CSRRW->     List(Y,N,N,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        MT_X, N,N,N,N,N,Y,CSR.W,N,N,N),
    CSRRS->     List(Y,N,N,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        MT_X, N,N,N,N,N,Y,CSR.S,N,N,N),
    CSRRC->     List(Y,N,N,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        MT_X, N,N,N,N,N,Y,CSR.C,N,N,N),
    CSRRWI->    List(Y,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD,   N,M_X,        MT_X, N,N,N,N,N,Y,CSR.W,N,N,N),
    CSRRSI->    List(Y,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD,   N,M_X,        MT_X, N,N,N,N,N,Y,CSR.S,N,N,N),
    CSRRCI->    List(Y,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD,   N,M_X,        MT_X, N,N,N,N,N,Y,CSR.C,N,N,N))
}

class SDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    SFENCE_VM-> List(Y,N,N,N,N,N,N,X,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, N,N,N,N,N,N,CSR.I,N,N,N),
    SRET->      List(Y,N,N,N,N,N,N,X,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, N,N,N,N,N,N,CSR.I,N,N,N))
}

class DebugDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    DRET->      List(Y,N,N,N,N,N,N,X,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, N,N,N,N,N,N,CSR.I,N,N,N))
}

class I64Decode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    LD->        List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      MT_D, N,N,N,N,N,Y,CSR.N,N,N,N),
    LWU->       List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      MT_WU,N,N,N,N,N,Y,CSR.N,N,N,N),
    SD->        List(Y,N,N,N,N,N,Y,Y,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,      MT_D, N,N,N,N,N,N,CSR.N,N,N,N),

    ADDIW->     List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_32,FN_ADD,    N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    SLLIW->     List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_32,FN_SL,     N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    SRLIW->     List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_32,FN_SR,     N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    SRAIW->     List(Y,N,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_32,FN_SRA,    N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    ADDW->      List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32,FN_ADD,    N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    SUBW->      List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32,FN_SUB,    N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    SLLW->      List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32,FN_SL,     N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    SRLW->      List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32,FN_SR,     N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    SRAW->      List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32,FN_SRA,    N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N))
}

class MDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    MUL->       List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_MUL,   N,M_X,        MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N),
    MULH->      List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_MULH,  N,M_X,        MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N),
    MULHU->     List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_MULHU, N,M_X,        MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N),
    MULHSU->    List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_MULHSU,N,M_X,        MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N),

    DIV->       List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_DIV,   N,M_X,        MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N),
    DIVU->      List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_DIVU,  N,M_X,        MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N),
    REM->       List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_REM,   N,M_X,        MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N),
    REMU->      List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_REMU,  N,M_X,        MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N))
}

class M64Decode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    MULW->      List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32, FN_MUL,   N,M_X,        MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N),

    DIVW->      List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32, FN_DIV,   N,M_X,        MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N),
    DIVUW->     List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32, FN_DIVU,  N,M_X,        MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N),
    REMW->      List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32, FN_REM,   N,M_X,        MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N),
    REMUW->     List(Y,N,N,N,N,N,Y,Y,A2_RS2, A1_RS1, IMM_X, DW_32, FN_REMU,  N,M_X,        MT_X, N,N,N,N,Y,Y,CSR.N,N,N,N))
}

class ADecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    AMOADD_W->  List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_ADD,   MT_W, N,N,N,N,N,Y,CSR.N,N,N,Y),
    AMOXOR_W->  List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_XOR,   MT_W, N,N,N,N,N,Y,CSR.N,N,N,Y),
    AMOSWAP_W-> List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_SWAP,  MT_W, N,N,N,N,N,Y,CSR.N,N,N,Y),
    AMOAND_W->  List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_AND,   MT_W, N,N,N,N,N,Y,CSR.N,N,N,Y),
    AMOOR_W->   List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_OR,    MT_W, N,N,N,N,N,Y,CSR.N,N,N,Y),
    AMOMIN_W->  List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MIN,   MT_W, N,N,N,N,N,Y,CSR.N,N,N,Y),
    AMOMINU_W-> List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MINU,  MT_W, N,N,N,N,N,Y,CSR.N,N,N,Y),
    AMOMAX_W->  List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MAX,   MT_W, N,N,N,N,N,Y,CSR.N,N,N,Y),
    AMOMAXU_W-> List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MAXU,  MT_W, N,N,N,N,N,Y,CSR.N,N,N,Y),

    LR_W->      List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XLR,      MT_W, N,N,N,N,N,Y,CSR.N,N,N,Y),
    SC_W->      List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XSC,      MT_W, N,N,N,N,N,Y,CSR.N,N,N,Y))
}

class A64Decode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    AMOADD_D->  List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_ADD,   MT_D, N,N,N,N,N,Y,CSR.N,N,N,Y),
    AMOSWAP_D-> List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_SWAP,  MT_D, N,N,N,N,N,Y,CSR.N,N,N,Y),
    AMOXOR_D->  List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_XOR,   MT_D, N,N,N,N,N,Y,CSR.N,N,N,Y),
    AMOAND_D->  List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_AND,   MT_D, N,N,N,N,N,Y,CSR.N,N,N,Y),
    AMOOR_D->   List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_OR,    MT_D, N,N,N,N,N,Y,CSR.N,N,N,Y),
    AMOMIN_D->  List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MIN,   MT_D, N,N,N,N,N,Y,CSR.N,N,N,Y),
    AMOMINU_D-> List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MINU,  MT_D, N,N,N,N,N,Y,CSR.N,N,N,Y),
    AMOMAX_D->  List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MAX,   MT_D, N,N,N,N,N,Y,CSR.N,N,N,Y),
    AMOMAXU_D-> List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MAXU,  MT_D, N,N,N,N,N,Y,CSR.N,N,N,Y),

    LR_D->      List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XLR,      MT_D, N,N,N,N,N,Y,CSR.N,N,N,Y),
    SC_D->      List(Y,N,N,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XSC,      MT_D, N,N,N,N,N,Y,CSR.N,N,N,Y))
}

class FDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FCVT_S_D->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,N,N,Y,N,N,CSR.N,N,N,N),
    FCVT_D_S->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,N,N,Y,N,N,CSR.N,N,N,N),
    FSGNJ_S->   List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FSGNJ_D->   List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FSGNJX_S->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FSGNJX_D->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FSGNJN_S->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FSGNJN_D->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FMIN_S->    List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FMIN_D->    List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FMAX_S->    List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FMAX_D->    List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FADD_S->    List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FADD_D->    List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FSUB_S->    List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FSUB_D->    List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FMUL_S->    List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FMUL_D->    List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FMADD_S->   List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,Y,Y,Y,N,N,CSR.N,N,N,N),
    FMADD_D->   List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,Y,Y,Y,N,N,CSR.N,N,N,N),
    FMSUB_S->   List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,Y,Y,Y,N,N,CSR.N,N,N,N),
    FMSUB_D->   List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,Y,Y,Y,N,N,CSR.N,N,N,N),
    FNMADD_S->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,Y,Y,Y,N,N,CSR.N,N,N,N),
    FNMADD_D->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,Y,Y,Y,N,N,CSR.N,N,N,N),
    FNMSUB_S->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,Y,Y,Y,N,N,CSR.N,N,N,N),
    FNMSUB_D->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,Y,Y,Y,N,N,CSR.N,N,N,N),
    FCLASS_S->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N),
    FCLASS_D->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N),
    FMV_X_S->   List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N),
    FCVT_W_S->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N),
    FCVT_W_D->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N),
    FCVT_WU_S-> List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N),
    FCVT_WU_D-> List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N),
    FEQ_S->     List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,Y,N,N,N,Y,CSR.N,N,N,N),
    FEQ_D->     List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,Y,N,N,N,Y,CSR.N,N,N,N),
    FLT_S->     List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,Y,N,N,N,Y,CSR.N,N,N,N),
    FLT_D->     List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,Y,N,N,N,Y,CSR.N,N,N,N),
    FLE_S->     List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,Y,N,N,N,Y,CSR.N,N,N,N),
    FLE_D->     List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,Y,N,N,N,Y,CSR.N,N,N,N),
    FMV_S_X->   List(Y,Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, N,N,N,Y,N,N,CSR.N,N,N,N),
    FCVT_S_W->  List(Y,Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, N,N,N,Y,N,N,CSR.N,N,N,N),
    FCVT_D_W->  List(Y,Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, N,N,N,Y,N,N,CSR.N,N,N,N),
    FCVT_S_WU-> List(Y,Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, N,N,N,Y,N,N,CSR.N,N,N,N),
    FCVT_D_WU-> List(Y,Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, N,N,N,Y,N,N,CSR.N,N,N,N),
    FLW->       List(Y,Y,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      MT_W, N,N,N,Y,N,N,CSR.N,N,N,N),
    FLD->       List(Y,Y,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      MT_D, N,N,N,Y,N,N,CSR.N,N,N,N),
    FSW->       List(Y,Y,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,      MT_W, N,Y,N,N,N,N,CSR.N,N,N,N),
    FSD->       List(Y,Y,N,N,N,N,N,Y,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,      MT_D, N,Y,N,N,N,N,CSR.N,N,N,N))
}

class F64Decode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FMV_X_D->   List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N),
    FCVT_L_S->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N),
    FCVT_L_D->  List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N),
    FCVT_LU_S-> List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N),
    FCVT_LU_D-> List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,N,N,N,N,Y,CSR.N,N,N,N),
    FMV_D_X->   List(Y,Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, N,N,N,Y,N,N,CSR.N,N,N,N),
    FCVT_S_L->  List(Y,Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, N,N,N,Y,N,N,CSR.N,N,N,N),
    FCVT_D_L->  List(Y,Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, N,N,N,Y,N,N,CSR.N,N,N,N),
    FCVT_S_LU-> List(Y,Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, N,N,N,Y,N,N,CSR.N,N,N,N),
    FCVT_D_LU-> List(Y,Y,N,N,N,N,N,Y,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, N,N,N,Y,N,N,CSR.N,N,N,N))
}

class FDivSqrtDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FDIV_S->    List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FDIV_D->    List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FSQRT_S->   List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N),
    FSQRT_D->   List(Y,Y,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        MT_X, Y,Y,N,Y,N,N,CSR.N,N,N,N))
}

class RoCCDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    CUSTOM0->           List(Y,N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        MT_X, N,N,N,N,N,N,CSR.N,N,N,N),
    CUSTOM0_RS1->       List(Y,N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        MT_X, N,N,N,N,N,N,CSR.N,N,N,N),
    CUSTOM0_RS1_RS2->   List(Y,N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        MT_X, N,N,N,N,N,N,CSR.N,N,N,N),
    CUSTOM0_RD->        List(Y,N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    CUSTOM0_RD_RS1->    List(Y,N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    CUSTOM0_RD_RS1_RS2->List(Y,N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    CUSTOM1->           List(Y,N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        MT_X, N,N,N,N,N,N,CSR.N,N,N,N),
    CUSTOM1_RS1->       List(Y,N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        MT_X, N,N,N,N,N,N,CSR.N,N,N,N),
    CUSTOM1_RS1_RS2->   List(Y,N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        MT_X, N,N,N,N,N,N,CSR.N,N,N,N),
    CUSTOM1_RD->        List(Y,N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    CUSTOM1_RD_RS1->    List(Y,N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    CUSTOM1_RD_RS1_RS2->List(Y,N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    CUSTOM2->           List(Y,N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        MT_X, N,N,N,N,N,N,CSR.N,N,N,N),
    CUSTOM2_RS1->       List(Y,N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        MT_X, N,N,N,N,N,N,CSR.N,N,N,N),
    CUSTOM2_RS1_RS2->   List(Y,N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        MT_X, N,N,N,N,N,N,CSR.N,N,N,N),
    CUSTOM2_RD->        List(Y,N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    CUSTOM2_RD_RS1->    List(Y,N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    CUSTOM2_RD_RS1_RS2->List(Y,N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    CUSTOM3->           List(Y,N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        MT_X, N,N,N,N,N,N,CSR.N,N,N,N),
    CUSTOM3_RS1->       List(Y,N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        MT_X, N,N,N,N,N,N,CSR.N,N,N,N),
    CUSTOM3_RS1_RS2->   List(Y,N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        MT_X, N,N,N,N,N,N,CSR.N,N,N,N),
    CUSTOM3_RD->        List(Y,N,Y,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    CUSTOM3_RD_RS1->    List(Y,N,Y,N,N,N,N,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N),
    CUSTOM3_RD_RS1_RS2->List(Y,N,Y,N,N,N,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        MT_X, N,N,N,N,N,Y,CSR.N,N,N,N))
}
