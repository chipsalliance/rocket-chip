// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.rocket

import Chisel._
import Chisel.ImplicitConversions._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.HasCoreParameters
import freechips.rocketchip.util._
import freechips.rocketchip.scie.SCIE
import Instructions._
import ALU._

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
  val scie = Bool()
  val sel_alu2 = Bits(width = A2_X.getWidth)
  val sel_alu1 = Bits(width = A1_X.getWidth)
  val sel_imm = Bits(width = IMM_X.getWidth)
  val alu_dw = Bool()
  val alu_fn = Bits(width = FN_X.getWidth)
  val mem = Bool()
  val mem_cmd = Bits(width = M_SZ)
  val rfs1 = Bool()
  val rfs2 = Bool()
  val rfs3 = Bool()
  val wfd = Bool()
  val mul = Bool()
  val div = Bool()
  val wxd = Bool()
  val csr = Bits(width = CSR.SZ)
  val fence_i = Bool()
  val fence = Bool()
  val amo = Bool()
  val dp = Bool()

  def default: List[BitPat] =
                //           jal                                                             renf1               fence.i
                //   val     | jalr                                                          | renf2             |
                //   | fp_val| | renx2                                                       | | renf3           |
                //   | | rocc| | | renx1       s_alu1                          mem_val       | | | wfd           |
                //   | | | br| | | |   s_alu2  |       imm    dw     alu       | mem_cmd     | | | | mul         |
                //   | | | | | | | |   |       |       |      |      |         | |           | | | | | div       | fence
                //   | | | | | | | |   |       |       |      |      |         | |           | | | | | | wxd     | | amo
                //   | | | | | | | | scie      |       |      |      |         | |           | | | | | | |       | | | dp
                List(N,X,X,X,X,X,X,X,X,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        X,X,X,X,X,X,X,CSR.X,X,X,X,X)

  def decode(inst: UInt, table: Iterable[(BitPat, List[BitPat])]) = {
    val decoder = DecodeLogic(inst, default, table)
    val sigs = Seq(legal, fp, rocc, branch, jal, jalr, rxs2, rxs1, scie, sel_alu2,
                   sel_alu1, sel_imm, alu_dw, alu_fn, mem, mem_cmd,
                   rfs1, rfs2, rfs3, wfd, mul, div, wxd, csr, fence_i, fence, amo, dp)
    sigs zip decoder map {case(s,d) => s := d}
    this
  }
}

class IDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    BNE->       List(Y,N,N,Y,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SNE,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    BEQ->       List(Y,N,N,Y,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SEQ,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    BLT->       List(Y,N,N,Y,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SLT,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    BLTU->      List(Y,N,N,Y,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SLTU,  N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    BGE->       List(Y,N,N,Y,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SGE,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    BGEU->      List(Y,N,N,Y,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_SB,DW_X,  FN_SGEU,  N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),

    JAL->       List(Y,N,N,N,Y,N,N,N,N,A2_SIZE,A1_PC,  IMM_UJ,DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    JALR->      List(Y,N,N,N,N,Y,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    AUIPC->     List(Y,N,N,N,N,N,N,N,N,A2_IMM, A1_PC,  IMM_U, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),

    LB->        List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    LH->        List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    LW->        List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    LBU->       List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    LHU->       List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SB->        List(Y,N,N,N,N,N,Y,Y,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,      N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    SH->        List(Y,N,N,N,N,N,Y,Y,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,      N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    SW->        List(Y,N,N,N,N,N,Y,Y,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,      N,N,N,N,N,N,N,CSR.N,N,N,N,N),

    LUI->       List(Y,N,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_U, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    ADDI->      List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SLTI ->     List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SLT,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SLTIU->     List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SLTU,  N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    ANDI->      List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_AND,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    ORI->       List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_OR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    XORI->      List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_XOR,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    ADD->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SUB->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SUB,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SLT->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SLT,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SLTU->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SLTU,  N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    AND->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_AND,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    OR->        List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_OR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    XOR->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_XOR,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SLL->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SL,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SRL->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SRA->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_SRA,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),

    FENCE->     List(Y,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.N,N,Y,N,N),

    SCALL->     List(Y,N,N,N,N,N,N,X,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N),
    SBREAK->    List(Y,N,N,N,N,N,N,X,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N),
    MRET->      List(Y,N,N,N,N,N,N,X,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N),
    WFI->       List(Y,N,N,N,N,N,N,X,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N),
    CEASE->     List(Y,N,N,N,N,N,N,X,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N),
    CSRRW->     List(Y,N,N,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.W,N,N,N,N),
    CSRRS->     List(Y,N,N,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.S,N,N,N,N),
    CSRRC->     List(Y,N,N,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.C,N,N,N,N),
    CSRRWI->    List(Y,N,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.W,N,N,N,N),
    CSRRSI->    List(Y,N,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.S,N,N,N,N),
    CSRRCI->    List(Y,N,N,N,N,N,N,N,N,A2_IMM, A1_ZERO,IMM_Z, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.C,N,N,N,N))
}

class FenceIDecode(flushDCache: Boolean)(implicit val p: Parameters) extends DecodeConstants
{
  private val (v, cmd) = if (flushDCache) (Y, BitPat(M_FLUSH_ALL)) else (N, M_X)

  val table: Array[(BitPat, List[BitPat])] = Array(
    FENCE_I->   List(Y,N,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     v,cmd,        N,N,N,N,N,N,N,CSR.N,Y,Y,N,N))
}

class CFlushDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    CFLUSH_D_L1->
                List(Y,N,N,N,N,N,N,Y,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     Y,M_FLUSH_ALL,N,N,N,N,N,N,N,CSR.I,N,N,N,N))
}

class SDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    SFENCE_VMA->List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_SFENCE,   N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    SRET->      List(Y,N,N,N,N,N,N,X,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N))
}

class DebugDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    DRET->      List(Y,N,N,N,N,N,N,X,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,N,N,N,N,CSR.I,N,N,N,N))
}

class I32Decode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    SLLI_RV32-> List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SL,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SRLI_RV32-> List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SRAI_RV32-> List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SRA,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N))
}

class I64Decode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    LD->        List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    LWU->       List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SD->        List(Y,N,N,N,N,N,Y,Y,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,      N,N,N,N,N,N,N,CSR.N,N,N,N,N),

    SLLI->      List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SL,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SRLI->      List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SR,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SRAI->      List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_SRA,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),

    ADDIW->     List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_32,FN_ADD,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SLLIW->     List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_32,FN_SL,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SRLIW->     List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_32,FN_SR,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SRAIW->     List(Y,N,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_32,FN_SRA,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    ADDW->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_32,FN_ADD,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SUBW->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_32,FN_SUB,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SLLW->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_32,FN_SL,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SRLW->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_32,FN_SR,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    SRAW->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_32,FN_SRA,    N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N))
}

class MDecode(pipelinedMul: Boolean)(implicit val p: Parameters) extends DecodeConstants
{
  val M = if (pipelinedMul) Y else N
  val D = if (pipelinedMul) N else Y
  val table: Array[(BitPat, List[BitPat])] = Array(
    MUL->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_MUL,   N,M_X,        N,N,N,N,M,D,Y,CSR.N,N,N,N,N),
    MULH->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_MULH,  N,M_X,        N,N,N,N,M,D,Y,CSR.N,N,N,N,N),
    MULHU->     List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_MULHU, N,M_X,        N,N,N,N,M,D,Y,CSR.N,N,N,N,N),
    MULHSU->    List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_MULHSU,N,M_X,        N,N,N,N,M,D,Y,CSR.N,N,N,N,N),

    DIV->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_DIV,   N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N),
    DIVU->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_DIVU,  N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N),
    REM->       List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_REM,   N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N),
    REMU->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_XPR,FN_REMU,  N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N))
}

class M64Decode(pipelinedMul: Boolean)(implicit val p: Parameters) extends DecodeConstants
{
  val M = if (pipelinedMul) Y else N
  val D = if (pipelinedMul) N else Y
  val table: Array[(BitPat, List[BitPat])] = Array(
    MULW->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_32, FN_MUL,   N,M_X,        N,N,N,N,M,D,Y,CSR.N,N,N,N,N),

    DIVW->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_32, FN_DIV,   N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N),
    DIVUW->     List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_32, FN_DIVU,  N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N),
    REMW->      List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_32, FN_REM,   N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N),
    REMUW->     List(Y,N,N,N,N,N,Y,Y,N,A2_RS2, A1_RS1, IMM_X, DW_32, FN_REMU,  N,M_X,        N,N,N,N,N,Y,Y,CSR.N,N,N,N,N))
}

class ADecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    AMOADD_W->  List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_ADD,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N),
    AMOXOR_W->  List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_XOR,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N),
    AMOSWAP_W-> List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_SWAP,  N,N,N,N,N,N,Y,CSR.N,N,N,Y,N),
    AMOAND_W->  List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_AND,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N),
    AMOOR_W->   List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_OR,    N,N,N,N,N,N,Y,CSR.N,N,N,Y,N),
    AMOMIN_W->  List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MIN,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N),
    AMOMINU_W-> List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MINU,  N,N,N,N,N,N,Y,CSR.N,N,N,Y,N),
    AMOMAX_W->  List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MAX,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N),
    AMOMAXU_W-> List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MAXU,  N,N,N,N,N,N,Y,CSR.N,N,N,Y,N),

    LR_W->      List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XLR,      N,N,N,N,N,N,Y,CSR.N,N,N,Y,N),
    SC_W->      List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XSC,      N,N,N,N,N,N,Y,CSR.N,N,N,Y,N))
}

class A64Decode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    AMOADD_D->  List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_ADD,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N),
    AMOSWAP_D-> List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_SWAP,  N,N,N,N,N,N,Y,CSR.N,N,N,Y,N),
    AMOXOR_D->  List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_XOR,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N),
    AMOAND_D->  List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_AND,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N),
    AMOOR_D->   List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_OR,    N,N,N,N,N,N,Y,CSR.N,N,N,Y,N),
    AMOMIN_D->  List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MIN,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N),
    AMOMINU_D-> List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MINU,  N,N,N,N,N,N,Y,CSR.N,N,N,Y,N),
    AMOMAX_D->  List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MAX,   N,N,N,N,N,N,Y,CSR.N,N,N,Y,N),
    AMOMAXU_D-> List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XA_MAXU,  N,N,N,N,N,N,Y,CSR.N,N,N,Y,N),

    LR_D->      List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XLR,      N,N,N,N,N,N,Y,CSR.N,N,N,Y,N),
    SC_D->      List(Y,N,N,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   Y,M_XSC,      N,N,N,N,N,N,Y,CSR.N,N,N,Y,N))
}

class FDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FSGNJ_S->   List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N),
    FSGNJX_S->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N),
    FSGNJN_S->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N),
    FMIN_S->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N),
    FMAX_S->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N),
    FADD_S->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N),
    FSUB_S->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N),
    FMUL_S->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N),
    FMADD_S->   List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N),
    FMSUB_S->   List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N),
    FNMADD_S->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N),
    FNMSUB_S->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,N),
    FCLASS_S->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    FMV_X_S->   List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    FCVT_W_S->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    FCVT_WU_S-> List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    FEQ_S->     List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,N),
    FLT_S->     List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,N),
    FLE_S->     List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,N),
    FMV_S_X->   List(Y,Y,N,N,N,N,N,Y,N,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N),
    FCVT_S_W->  List(Y,Y,N,N,N,N,N,Y,N,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N),
    FCVT_S_WU-> List(Y,Y,N,N,N,N,N,Y,N,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N),
    FLW->       List(Y,Y,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,Y,N,N,N,CSR.N,N,N,N,N),
    FSW->       List(Y,Y,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,      N,Y,N,N,N,N,N,CSR.N,N,N,N,N),
    FDIV_S->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N),
    FSQRT_S->   List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,N))
}

class DDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FCVT_S_D->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,Y,N,N,N,CSR.N,N,N,N,Y),
    FCVT_D_S->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,Y,N,N,N,CSR.N,N,N,N,Y),
    FSGNJ_D->   List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y),
    FSGNJX_D->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y),
    FSGNJN_D->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y),
    FMIN_D->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y),
    FMAX_D->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y),
    FADD_D->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y),
    FSUB_D->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y),
    FMUL_D->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y),
    FMADD_D->   List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,Y),
    FMSUB_D->   List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,Y),
    FNMADD_D->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,Y),
    FNMSUB_D->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,Y,Y,N,N,N,CSR.N,N,N,N,Y),
    FCLASS_D->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,Y),
    FCVT_W_D->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,Y),
    FCVT_WU_D-> List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,Y),
    FEQ_D->     List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,Y),
    FLT_D->     List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,Y),
    FLE_D->     List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,N,N,N,Y,CSR.N,N,N,N,Y),
    FCVT_D_W->  List(Y,Y,N,N,N,N,N,Y,N,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,Y),
    FCVT_D_WU-> List(Y,Y,N,N,N,N,N,Y,N,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,Y),
    FLD->       List(Y,Y,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_I, DW_XPR,FN_ADD,   Y,M_XRD,      N,N,N,Y,N,N,N,CSR.N,N,N,N,Y),
    FSD->       List(Y,Y,N,N,N,N,N,Y,N,A2_IMM, A1_RS1, IMM_S, DW_XPR,FN_ADD,   Y,M_XWR,      N,Y,N,N,N,N,N,CSR.N,N,N,N,Y),
    FDIV_D->    List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y),
    FSQRT_D->   List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,Y,N,Y,N,N,N,CSR.N,N,N,N,Y))
}

class F64Decode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FCVT_L_S->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    FCVT_LU_S-> List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    FCVT_S_L->  List(Y,Y,N,N,N,N,N,Y,N,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N),
    FCVT_S_LU-> List(Y,Y,N,N,N,N,N,Y,N,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,N))
}

class D64Decode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    FMV_X_D->   List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,Y),
    FCVT_L_D->  List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,Y),
    FCVT_LU_D-> List(Y,Y,N,N,N,N,N,N,N,A2_X,   A1_X,   IMM_X, DW_X,  FN_X,     N,M_X,        Y,N,N,N,N,N,Y,CSR.N,N,N,N,Y),
    FMV_D_X->   List(Y,Y,N,N,N,N,N,Y,N,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,Y),
    FCVT_D_L->  List(Y,Y,N,N,N,N,N,Y,N,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,Y),
    FCVT_D_LU-> List(Y,Y,N,N,N,N,N,Y,N,A2_X,   A1_RS1, IMM_X, DW_X,  FN_X,     N,M_X,        N,N,N,Y,N,N,N,CSR.N,N,N,N,Y))
}

class SCIEDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    SCIE.opcode->       List(Y,N,N,N,N,N,Y,Y,Y,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_X,     N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N))
}

class RoCCDecode(implicit val p: Parameters) extends DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])] = Array(
    CUSTOM0->           List(Y,N,Y,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    CUSTOM0_RS1->       List(Y,N,Y,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    CUSTOM0_RS1_RS2->   List(Y,N,Y,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    CUSTOM0_RD->        List(Y,N,Y,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    CUSTOM0_RD_RS1->    List(Y,N,Y,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    CUSTOM0_RD_RS1_RS2->List(Y,N,Y,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    CUSTOM1->           List(Y,N,Y,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    CUSTOM1_RS1->       List(Y,N,Y,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    CUSTOM1_RS1_RS2->   List(Y,N,Y,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    CUSTOM1_RD->        List(Y,N,Y,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    CUSTOM1_RD_RS1->    List(Y,N,Y,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    CUSTOM1_RD_RS1_RS2->List(Y,N,Y,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    CUSTOM2->           List(Y,N,Y,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    CUSTOM2_RS1->       List(Y,N,Y,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    CUSTOM2_RS1_RS2->   List(Y,N,Y,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    CUSTOM2_RD->        List(Y,N,Y,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    CUSTOM2_RD_RS1->    List(Y,N,Y,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    CUSTOM2_RD_RS1_RS2->List(Y,N,Y,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    CUSTOM3->           List(Y,N,Y,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    CUSTOM3_RS1->       List(Y,N,Y,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    CUSTOM3_RS1_RS2->   List(Y,N,Y,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,N,CSR.N,N,N,N,N),
    CUSTOM3_RD->        List(Y,N,Y,N,N,N,N,N,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    CUSTOM3_RD_RS1->    List(Y,N,Y,N,N,N,N,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N),
    CUSTOM3_RD_RS1_RS2->List(Y,N,Y,N,N,N,Y,Y,N,A2_ZERO,A1_RS1, IMM_X, DW_XPR,FN_ADD,   N,M_X,        N,N,N,N,N,N,Y,CSR.N,N,N,N,N))
}
