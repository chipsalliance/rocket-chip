// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.rocket

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util._
import freechips.rocketchip.scie.SCIE
import Instructions._
import CustomInstructions._

abstract trait DecodeConstants
{
  val table: Array[(BitPat, List[BitPat])]
}

class IntCtrlSigs(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends Bundle {

  val legal = Bool()
  val fp = Bool()
  val rocc = Bool()
  val branch = Bool()
  val jal = Bool()
  val jalr = Bool()
  val rxs2 = Bool()
  val rxs1 = Bool()
  val scie = Bool()
  val zbk = Bool()
  val zkn = Bool()
  val zks = Bool()
  val sel_alu2 = Bits(A2_X.getWidth.W)
  val sel_alu1 = Bits(A1_X.getWidth.W)
  val sel_imm = Bits(IMM_X.getWidth.W)
  val alu_dw = Bool()
  val alu_fn = Bits(aluFn.FN_X.getWidth.W)
  val mem = Bool()
  val mem_cmd = Bits(M_SZ.W)
  val rfs1 = Bool()
  val rfs2 = Bool()
  val rfs3 = Bool()
  val wfd = Bool()
  val mul = Bool()
  val div = Bool()
  val wxd = Bool()
  val csr = Bits(CSR.SZ.W)
  val fence_i = Bool()
  val fence = Bool()
  val amo = Bool()
  val dp = Bool()

  def default: List[BitPat] =
                //           jal                                                                         renf1               fence.i
                //   val     | jalr                                                                      | renf2             |
                //   | fp_val| | renx2                                                                   | | renf3           |
                //   | | rocc| | | renx1               s_alu1                              mem_val       | | | wfd           |
                //   | | | br| | | | scie      s_alu2  |       imm    dw     alu           | mem_cmd     | | | | mul         |
                //   | | | | | | | | | zbk     |       |       |      |      |             | |           | | | | | div       | fence
                //   | | | | | | | | | | zkn   |       |       |      |      |             | |           | | | | | | wxd     | | amo
                //   | | | | | | | | | | | zks |       |       |      |      |             | |           | | | | | | |       | | | dp
                List(N,X,X,X,X,X,X,X,X,X,X,X,  A2_X,   A1_X,   IMM_X, DW_X,  aluFn.FN_X,   N,M_X,        X,X,X,X,X,X,X,CSR.X,X,X,X,X)

  def decode(inst: UInt, table: Iterable[(BitPat, List[BitPat])]) = {
    val decoder = DecodeLogic(inst, default, table)
    val sigs = Seq(legal, fp, rocc, branch, jal, jalr, rxs2, rxs1, scie, zbk, zkn, zks, sel_alu2,
                   sel_alu1, sel_imm, alu_dw, alu_fn, mem, mem_cmd,
                   rfs1, rfs2, rfs3, wfd, mul, div, wxd, csr, fence_i, fence, amo, dp)
    sigs zip decoder map {case(s,d) => s := d}
    this
  }
}

class IDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.BNE, Instructions.BEQ, Instructions.BLT, Instructions.BLTU, Instructions.BGE, Instructions.BGEU, Instructions.JAL, Instructions.JALR, Instructions.AUIPC, Instructions.LB, Instructions.LH, Instructions.LW, Instructions.LBU, Instructions.LHU, Instructions.SB, Instructions.SH, Instructions.SW, Instructions.LUI, Instructions.ADDI, Instructions.SLTI, Instructions.SLTIU, Instructions.ANDI, Instructions.ORI, Instructions.XORI, Instructions.ADD, Instructions.SUB, Instructions.SLT, Instructions.SLTU, Instructions.AND, Instructions.OR, Instructions.XOR, Instructions.SLL, Instructions.SRL, Instructions.SRA, Instructions.FENCE, Instructions.ECALL, Instructions.EBREAK, Instructions.MRET, Instructions.WFI, Instructions.CEASE, Instructions.CSRRW, Instructions.CSRRS, Instructions.CSRRC, Instructions.CSRRWI, Instructions.CSRRSI, Instructions.CSRRCI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BNE, Instructions.BEQ, Instructions.BLT, Instructions.BLTU, Instructions.BGE, Instructions.BGEU, Instructions.JAL, Instructions.JALR, Instructions.AUIPC, Instructions.LB, Instructions.LH, Instructions.LW, Instructions.LBU, Instructions.LHU, Instructions.SB, Instructions.SH, Instructions.SW, Instructions.LUI, Instructions.ADDI, Instructions.SLTI, Instructions.SLTIU, Instructions.ANDI, Instructions.ORI, Instructions.XORI, Instructions.ADD, Instructions.SUB, Instructions.SLT, Instructions.SLTU, Instructions.AND, Instructions.OR, Instructions.XOR, Instructions.SLL, Instructions.SRL, Instructions.SRA, Instructions.FENCE, Instructions.ECALL, Instructions.EBREAK, Instructions.MRET, Instructions.WFI, Instructions.CEASE, Instructions.CSRRW, Instructions.CSRRS, Instructions.CSRRC, Instructions.CSRRWI, Instructions.CSRRSI, Instructions.CSRRCI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BNE, Instructions.BEQ, Instructions.BLT, Instructions.BLTU, Instructions.BGE, Instructions.BGEU, Instructions.JAL, Instructions.JALR, Instructions.AUIPC, Instructions.LB, Instructions.LH, Instructions.LW, Instructions.LBU, Instructions.LHU, Instructions.SB, Instructions.SH, Instructions.SW, Instructions.LUI, Instructions.ADDI, Instructions.SLTI, Instructions.SLTIU, Instructions.ANDI, Instructions.ORI, Instructions.XORI, Instructions.ADD, Instructions.SUB, Instructions.SLT, Instructions.SLTU, Instructions.AND, Instructions.OR, Instructions.XOR, Instructions.SLL, Instructions.SRL, Instructions.SRA, Instructions.FENCE, Instructions.ECALL, Instructions.EBREAK, Instructions.MRET, Instructions.WFI, Instructions.CEASE, Instructions.CSRRW, Instructions.CSRRS, Instructions.CSRRC, Instructions.CSRRWI, Instructions.CSRRSI, Instructions.CSRRCI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.BNE, Instructions.BEQ, Instructions.BLT, Instructions.BLTU, Instructions.BGE, Instructions.BGEU, 
  )
  val insns_N: Seq[BitPat] = Seq(
    Instructions.JAL, Instructions.JALR, Instructions.AUIPC, Instructions.LB, Instructions.LH, Instructions.LW, Instructions.LBU, Instructions.LHU, Instructions.SB, Instructions.SH, Instructions.SW, Instructions.LUI, Instructions.ADDI, Instructions.SLTI, Instructions.SLTIU, Instructions.ANDI, Instructions.ORI, Instructions.XORI, Instructions.ADD, Instructions.SUB, Instructions.SLT, Instructions.SLTU, Instructions.AND, Instructions.OR, Instructions.XOR, Instructions.SLL, Instructions.SRL, Instructions.SRA, Instructions.FENCE, Instructions.ECALL, Instructions.EBREAK, Instructions.MRET, Instructions.WFI, Instructions.CEASE, Instructions.CSRRW, Instructions.CSRRS, Instructions.CSRRC, Instructions.CSRRWI, Instructions.CSRRSI, Instructions.CSRRCI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BNE, Instructions.BEQ, Instructions.BLT, Instructions.BLTU, Instructions.BGE, Instructions.BGEU, Instructions.JALR, Instructions.AUIPC, Instructions.LB, Instructions.LH, Instructions.LW, Instructions.LBU, Instructions.LHU, Instructions.SB, Instructions.SH, Instructions.SW, Instructions.LUI, Instructions.ADDI, Instructions.SLTI, Instructions.SLTIU, Instructions.ANDI, Instructions.ORI, Instructions.XORI, Instructions.ADD, Instructions.SUB, Instructions.SLT, Instructions.SLTU, Instructions.AND, Instructions.OR, Instructions.XOR, Instructions.SLL, Instructions.SRL, Instructions.SRA, Instructions.FENCE, Instructions.ECALL, Instructions.EBREAK, Instructions.MRET, Instructions.WFI, Instructions.CEASE, Instructions.CSRRW, Instructions.CSRRS, Instructions.CSRRC, Instructions.CSRRWI, Instructions.CSRRSI, Instructions.CSRRCI, 
  )
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.JAL, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BNE, Instructions.BEQ, Instructions.BLT, Instructions.BLTU, Instructions.BGE, Instructions.BGEU, Instructions.JAL, Instructions.AUIPC, Instructions.LB, Instructions.LH, Instructions.LW, Instructions.LBU, Instructions.LHU, Instructions.SB, Instructions.SH, Instructions.SW, Instructions.LUI, Instructions.ADDI, Instructions.SLTI, Instructions.SLTIU, Instructions.ANDI, Instructions.ORI, Instructions.XORI, Instructions.ADD, Instructions.SUB, Instructions.SLT, Instructions.SLTU, Instructions.AND, Instructions.OR, Instructions.XOR, Instructions.SLL, Instructions.SRL, Instructions.SRA, Instructions.FENCE, Instructions.ECALL, Instructions.EBREAK, Instructions.MRET, Instructions.WFI, Instructions.CEASE, Instructions.CSRRW, Instructions.CSRRS, Instructions.CSRRC, Instructions.CSRRWI, Instructions.CSRRSI, Instructions.CSRRCI, 
  )
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.JALR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object rxs2 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.BNE, Instructions.BEQ, Instructions.BLT, Instructions.BLTU, Instructions.BGE, Instructions.BGEU, Instructions.SB, Instructions.SH, Instructions.SW, Instructions.ADD, Instructions.SUB, Instructions.SLT, Instructions.SLTU, Instructions.AND, Instructions.OR, Instructions.XOR, Instructions.SLL, Instructions.SRL, Instructions.SRA, 
  )
  val insns_N: Seq[BitPat] = Seq(
    Instructions.JAL, Instructions.JALR, Instructions.AUIPC, Instructions.LB, Instructions.LH, Instructions.LW, Instructions.LBU, Instructions.LHU, Instructions.LUI, Instructions.ADDI, Instructions.SLTI, Instructions.SLTIU, Instructions.ANDI, Instructions.ORI, Instructions.XORI, Instructions.FENCE, Instructions.ECALL, Instructions.EBREAK, Instructions.MRET, Instructions.WFI, Instructions.CEASE, Instructions.CSRRW, Instructions.CSRRS, Instructions.CSRRC, Instructions.CSRRWI, Instructions.CSRRSI, Instructions.CSRRCI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.BNE, Instructions.BEQ, Instructions.BLT, Instructions.BLTU, Instructions.BGE, Instructions.BGEU, Instructions.JALR, Instructions.LB, Instructions.LH, Instructions.LW, Instructions.LBU, Instructions.LHU, Instructions.SB, Instructions.SH, Instructions.SW, Instructions.ADDI, Instructions.SLTI, Instructions.SLTIU, Instructions.ANDI, Instructions.ORI, Instructions.XORI, Instructions.ADD, Instructions.SUB, Instructions.SLT, Instructions.SLTU, Instructions.AND, Instructions.OR, Instructions.XOR, Instructions.SLL, Instructions.SRL, Instructions.SRA, Instructions.CSRRW, Instructions.CSRRS, Instructions.CSRRC, 
  )
  val insns_N: Seq[BitPat] = Seq(
    Instructions.JAL, Instructions.AUIPC, Instructions.LUI, Instructions.FENCE, Instructions.CSRRWI, Instructions.CSRRSI, Instructions.CSRRCI, 
  )
  val insns_X: Seq[BitPat] = Seq(
    Instructions.ECALL, Instructions.EBREAK, Instructions.MRET, Instructions.WFI, Instructions.CEASE, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else if (insns_N.exists(op.bitPat)) N else if (insns_X.exists(op.bitPat)) X else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BNE, Instructions.BEQ, Instructions.BLT, Instructions.BLTU, Instructions.BGE, Instructions.BGEU, Instructions.JAL, Instructions.JALR, Instructions.AUIPC, Instructions.LB, Instructions.LH, Instructions.LW, Instructions.LBU, Instructions.LHU, Instructions.SB, Instructions.SH, Instructions.SW, Instructions.LUI, Instructions.ADDI, Instructions.SLTI, Instructions.SLTIU, Instructions.ANDI, Instructions.ORI, Instructions.XORI, Instructions.ADD, Instructions.SUB, Instructions.SLT, Instructions.SLTU, Instructions.AND, Instructions.OR, Instructions.XOR, Instructions.SLL, Instructions.SRL, Instructions.SRA, Instructions.FENCE, Instructions.ECALL, Instructions.EBREAK, Instructions.MRET, Instructions.WFI, Instructions.CEASE, Instructions.CSRRW, Instructions.CSRRS, Instructions.CSRRC, Instructions.CSRRWI, Instructions.CSRRSI, Instructions.CSRRCI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BNE, Instructions.BEQ, Instructions.BLT, Instructions.BLTU, Instructions.BGE, Instructions.BGEU, Instructions.JAL, Instructions.JALR, Instructions.AUIPC, Instructions.LB, Instructions.LH, Instructions.LW, Instructions.LBU, Instructions.LHU, Instructions.SB, Instructions.SH, Instructions.SW, Instructions.LUI, Instructions.ADDI, Instructions.SLTI, Instructions.SLTIU, Instructions.ANDI, Instructions.ORI, Instructions.XORI, Instructions.ADD, Instructions.SUB, Instructions.SLT, Instructions.SLTU, Instructions.AND, Instructions.OR, Instructions.XOR, Instructions.SLL, Instructions.SRL, Instructions.SRA, Instructions.FENCE, Instructions.ECALL, Instructions.EBREAK, Instructions.MRET, Instructions.WFI, Instructions.CEASE, Instructions.CSRRW, Instructions.CSRRS, Instructions.CSRRC, Instructions.CSRRWI, Instructions.CSRRSI, Instructions.CSRRCI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BNE, Instructions.BEQ, Instructions.BLT, Instructions.BLTU, Instructions.BGE, Instructions.BGEU, Instructions.JAL, Instructions.JALR, Instructions.AUIPC, Instructions.LB, Instructions.LH, Instructions.LW, Instructions.LBU, Instructions.LHU, Instructions.SB, Instructions.SH, Instructions.SW, Instructions.LUI, Instructions.ADDI, Instructions.SLTI, Instructions.SLTIU, Instructions.ANDI, Instructions.ORI, Instructions.XORI, Instructions.ADD, Instructions.SUB, Instructions.SLT, Instructions.SLTU, Instructions.AND, Instructions.OR, Instructions.XOR, Instructions.SLL, Instructions.SRL, Instructions.SRA, Instructions.FENCE, Instructions.ECALL, Instructions.EBREAK, Instructions.MRET, Instructions.WFI, Instructions.CEASE, Instructions.CSRRW, Instructions.CSRRS, Instructions.CSRRC, Instructions.CSRRWI, Instructions.CSRRSI, Instructions.CSRRCI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BNE, Instructions.BEQ, Instructions.BLT, Instructions.BLTU, Instructions.BGE, Instructions.BGEU, Instructions.JAL, Instructions.JALR, Instructions.AUIPC, Instructions.LB, Instructions.LH, Instructions.LW, Instructions.LBU, Instructions.LHU, Instructions.SB, Instructions.SH, Instructions.SW, Instructions.LUI, Instructions.ADDI, Instructions.SLTI, Instructions.SLTIU, Instructions.ANDI, Instructions.ORI, Instructions.XORI, Instructions.ADD, Instructions.SUB, Instructions.SLT, Instructions.SLTU, Instructions.AND, Instructions.OR, Instructions.XOR, Instructions.SLL, Instructions.SRL, Instructions.SRA, Instructions.FENCE, Instructions.ECALL, Instructions.EBREAK, Instructions.MRET, Instructions.WFI, Instructions.CEASE, Instructions.CSRRW, Instructions.CSRRS, Instructions.CSRRC, Instructions.CSRRWI, Instructions.CSRRSI, Instructions.CSRRCI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_RS2: Seq[BitPat] = Seq(
    Instructions.BNE, Instructions.BEQ, Instructions.BLT, Instructions.BLTU, Instructions.BGE, Instructions.BGEU, Instructions.ADD, Instructions.SUB, Instructions.SLT, Instructions.SLTU, Instructions.AND, Instructions.OR, Instructions.XOR, Instructions.SLL, Instructions.SRL, Instructions.SRA, 
  )
  val insns_A2_SIZE: Seq[BitPat] = Seq(
    Instructions.JAL, 
  )
  val insns_A2_IMM: Seq[BitPat] = Seq(
    Instructions.JALR, Instructions.AUIPC, Instructions.LB, Instructions.LH, Instructions.LW, Instructions.LBU, Instructions.LHU, Instructions.SB, Instructions.SH, Instructions.SW, Instructions.LUI, Instructions.ADDI, Instructions.SLTI, Instructions.SLTIU, Instructions.ANDI, Instructions.ORI, Instructions.XORI, Instructions.CSRRWI, Instructions.CSRRSI, Instructions.CSRRCI, 
  )
  val insns_A2_X: Seq[BitPat] = Seq(
    Instructions.FENCE, Instructions.ECALL, Instructions.EBREAK, Instructions.MRET, Instructions.WFI, Instructions.CEASE, 
  )
  val insns_A2_ZERO: Seq[BitPat] = Seq(
    Instructions.CSRRW, Instructions.CSRRS, Instructions.CSRRC, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_RS2.exists(op.bitPat)) A2_RS2 else if (insns_A2_SIZE.exists(op.bitPat)) A2_SIZE else if (insns_A2_IMM.exists(op.bitPat)) A2_IMM else if (insns_A2_X.exists(op.bitPat)) A2_X else if (insns_A2_ZERO.exists(op.bitPat)) A2_ZERO else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.BNE, Instructions.BEQ, Instructions.BLT, Instructions.BLTU, Instructions.BGE, Instructions.BGEU, Instructions.JALR, Instructions.LB, Instructions.LH, Instructions.LW, Instructions.LBU, Instructions.LHU, Instructions.SB, Instructions.SH, Instructions.SW, Instructions.ADDI, Instructions.SLTI, Instructions.SLTIU, Instructions.ANDI, Instructions.ORI, Instructions.XORI, Instructions.ADD, Instructions.SUB, Instructions.SLT, Instructions.SLTU, Instructions.AND, Instructions.OR, Instructions.XOR, Instructions.SLL, Instructions.SRL, Instructions.SRA, Instructions.CSRRW, Instructions.CSRRS, Instructions.CSRRC, 
  )
  val insns_A1_PC: Seq[BitPat] = Seq(
    Instructions.JAL, Instructions.AUIPC, 
  )
  val insns_A1_ZERO: Seq[BitPat] = Seq(
    Instructions.LUI, Instructions.CSRRWI, Instructions.CSRRSI, Instructions.CSRRCI, 
  )
  val insns_A1_X: Seq[BitPat] = Seq(
    Instructions.FENCE, Instructions.ECALL, Instructions.EBREAK, Instructions.MRET, Instructions.WFI, Instructions.CEASE, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else if (insns_A1_PC.exists(op.bitPat)) A1_PC else if (insns_A1_ZERO.exists(op.bitPat)) A1_ZERO else if (insns_A1_X.exists(op.bitPat)) A1_X else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_SB: Seq[BitPat] = Seq(
    Instructions.BNE, Instructions.BEQ, Instructions.BLT, Instructions.BLTU, Instructions.BGE, Instructions.BGEU, 
  )
  val insns_IMM_UJ: Seq[BitPat] = Seq(
    Instructions.JAL, 
  )
  val insns_IMM_I: Seq[BitPat] = Seq(
    Instructions.JALR, Instructions.LB, Instructions.LH, Instructions.LW, Instructions.LBU, Instructions.LHU, Instructions.ADDI, Instructions.SLTI, Instructions.SLTIU, Instructions.ANDI, Instructions.ORI, Instructions.XORI, 
  )
  val insns_IMM_U: Seq[BitPat] = Seq(
    Instructions.AUIPC, Instructions.LUI, 
  )
  val insns_IMM_S: Seq[BitPat] = Seq(
    Instructions.SB, Instructions.SH, Instructions.SW, 
  )
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.ADD, Instructions.SUB, Instructions.SLT, Instructions.SLTU, Instructions.AND, Instructions.OR, Instructions.XOR, Instructions.SLL, Instructions.SRL, Instructions.SRA, Instructions.FENCE, Instructions.ECALL, Instructions.EBREAK, Instructions.MRET, Instructions.WFI, Instructions.CEASE, Instructions.CSRRW, Instructions.CSRRS, Instructions.CSRRC, 
  )
  val insns_IMM_Z: Seq[BitPat] = Seq(
    Instructions.CSRRWI, Instructions.CSRRSI, Instructions.CSRRCI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_SB.exists(op.bitPat)) IMM_SB else if (insns_IMM_UJ.exists(op.bitPat)) IMM_UJ else if (insns_IMM_I.exists(op.bitPat)) IMM_I else if (insns_IMM_U.exists(op.bitPat)) IMM_U else if (insns_IMM_S.exists(op.bitPat)) IMM_S else if (insns_IMM_X.exists(op.bitPat)) IMM_X else if (insns_IMM_Z.exists(op.bitPat)) IMM_Z else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.BNE, Instructions.BEQ, Instructions.BLT, Instructions.BLTU, Instructions.BGE, Instructions.BGEU, Instructions.JAL, Instructions.JALR, Instructions.AUIPC, Instructions.LB, Instructions.LH, Instructions.LW, Instructions.LBU, Instructions.LHU, Instructions.SB, Instructions.SH, Instructions.SW, Instructions.LUI, Instructions.ADDI, Instructions.SLTI, Instructions.SLTIU, Instructions.ANDI, Instructions.ORI, Instructions.XORI, Instructions.ADD, Instructions.SUB, Instructions.SLT, Instructions.SLTU, Instructions.AND, Instructions.OR, Instructions.XOR, Instructions.SLL, Instructions.SRL, Instructions.SRA, Instructions.CSRRW, Instructions.CSRRS, Instructions.CSRRC, Instructions.CSRRWI, Instructions.CSRRSI, Instructions.CSRRCI, 
  )
  val insns_DW_X: Seq[BitPat] = Seq(
    Instructions.FENCE, Instructions.ECALL, Instructions.EBREAK, Instructions.MRET, Instructions.WFI, Instructions.CEASE, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else if (insns_DW_X.exists(op.bitPat)) DW_X else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_SNE: Seq[BitPat] = Seq(
    Instructions.BNE, 
  )
  val insns_aluFn_FN_SEQ: Seq[BitPat] = Seq(
    Instructions.BEQ, 
  )
  val insns_aluFn_FN_SLT: Seq[BitPat] = Seq(
    Instructions.BLT, Instructions.SLTI, Instructions.SLT, 
  )
  val insns_aluFn_FN_SLTU: Seq[BitPat] = Seq(
    Instructions.BLTU, Instructions.SLTIU, Instructions.SLTU, 
  )
  val insns_aluFn_FN_SGE: Seq[BitPat] = Seq(
    Instructions.BGE, 
  )
  val insns_aluFn_FN_SGEU: Seq[BitPat] = Seq(
    Instructions.BGEU, 
  )
  val insns_aluFn_FN_ADD: Seq[BitPat] = Seq(
    Instructions.JAL, Instructions.JALR, Instructions.AUIPC, Instructions.LB, Instructions.LH, Instructions.LW, Instructions.LBU, Instructions.LHU, Instructions.SB, Instructions.SH, Instructions.SW, Instructions.LUI, Instructions.ADDI, Instructions.ADD, Instructions.CSRRW, Instructions.CSRRS, Instructions.CSRRC, Instructions.CSRRWI, Instructions.CSRRSI, Instructions.CSRRCI, 
  )
  val insns_aluFn_FN_AND: Seq[BitPat] = Seq(
    Instructions.ANDI, Instructions.AND, 
  )
  val insns_aluFn_FN_OR: Seq[BitPat] = Seq(
    Instructions.ORI, Instructions.OR, 
  )
  val insns_aluFn_FN_XOR: Seq[BitPat] = Seq(
    Instructions.XORI, Instructions.XOR, 
  )
  val insns_aluFn_FN_SUB: Seq[BitPat] = Seq(
    Instructions.SUB, 
  )
  val insns_aluFn_FN_SL: Seq[BitPat] = Seq(
    Instructions.SLL, 
  )
  val insns_aluFn_FN_SR: Seq[BitPat] = Seq(
    Instructions.SRL, 
  )
  val insns_aluFn_FN_SRA: Seq[BitPat] = Seq(
    Instructions.SRA, 
  )
  val insns_aluFn_FN_X: Seq[BitPat] = Seq(
    Instructions.FENCE, Instructions.ECALL, Instructions.EBREAK, Instructions.MRET, Instructions.WFI, Instructions.CEASE, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_SNE.exists(op.bitPat)) aluFn.FN_SNE else if (insns_aluFn_FN_SEQ.exists(op.bitPat)) aluFn.FN_SEQ else if (insns_aluFn_FN_SLT.exists(op.bitPat)) aluFn.FN_SLT else if (insns_aluFn_FN_SLTU.exists(op.bitPat)) aluFn.FN_SLTU else if (insns_aluFn_FN_SGE.exists(op.bitPat)) aluFn.FN_SGE else if (insns_aluFn_FN_SGEU.exists(op.bitPat)) aluFn.FN_SGEU else if (insns_aluFn_FN_ADD.exists(op.bitPat)) aluFn.FN_ADD else if (insns_aluFn_FN_AND.exists(op.bitPat)) aluFn.FN_AND else if (insns_aluFn_FN_OR.exists(op.bitPat)) aluFn.FN_OR else if (insns_aluFn_FN_XOR.exists(op.bitPat)) aluFn.FN_XOR else if (insns_aluFn_FN_SUB.exists(op.bitPat)) aluFn.FN_SUB else if (insns_aluFn_FN_SL.exists(op.bitPat)) aluFn.FN_SL else if (insns_aluFn_FN_SR.exists(op.bitPat)) aluFn.FN_SR else if (insns_aluFn_FN_SRA.exists(op.bitPat)) aluFn.FN_SRA else if (insns_aluFn_FN_X.exists(op.bitPat)) aluFn.FN_X else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BNE, Instructions.BEQ, Instructions.BLT, Instructions.BLTU, Instructions.BGE, Instructions.BGEU, Instructions.JAL, Instructions.JALR, Instructions.AUIPC, Instructions.LUI, Instructions.ADDI, Instructions.SLTI, Instructions.SLTIU, Instructions.ANDI, Instructions.ORI, Instructions.XORI, Instructions.ADD, Instructions.SUB, Instructions.SLT, Instructions.SLTU, Instructions.AND, Instructions.OR, Instructions.XOR, Instructions.SLL, Instructions.SRL, Instructions.SRA, Instructions.FENCE, Instructions.ECALL, Instructions.EBREAK, Instructions.MRET, Instructions.WFI, Instructions.CEASE, Instructions.CSRRW, Instructions.CSRRS, Instructions.CSRRC, Instructions.CSRRWI, Instructions.CSRRSI, Instructions.CSRRCI, 
  )
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.LB, Instructions.LH, Instructions.LW, Instructions.LBU, Instructions.LHU, Instructions.SB, Instructions.SH, Instructions.SW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.BNE, Instructions.BEQ, Instructions.BLT, Instructions.BLTU, Instructions.BGE, Instructions.BGEU, Instructions.JAL, Instructions.JALR, Instructions.AUIPC, Instructions.LUI, Instructions.ADDI, Instructions.SLTI, Instructions.SLTIU, Instructions.ANDI, Instructions.ORI, Instructions.XORI, Instructions.ADD, Instructions.SUB, Instructions.SLT, Instructions.SLTU, Instructions.AND, Instructions.OR, Instructions.XOR, Instructions.SLL, Instructions.SRL, Instructions.SRA, Instructions.FENCE, Instructions.ECALL, Instructions.EBREAK, Instructions.MRET, Instructions.WFI, Instructions.CEASE, Instructions.CSRRW, Instructions.CSRRS, Instructions.CSRRC, Instructions.CSRRWI, Instructions.CSRRSI, Instructions.CSRRCI, 
  )
  val insns_M_XRD: Seq[BitPat] = Seq(
    Instructions.LB, Instructions.LH, Instructions.LW, Instructions.LBU, Instructions.LHU, 
  )
  val insns_M_XWR: Seq[BitPat] = Seq(
    Instructions.SB, Instructions.SH, Instructions.SW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else if (insns_M_XRD.exists(op.bitPat)) M_XRD else if (insns_M_XWR.exists(op.bitPat)) M_XWR else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BNE, Instructions.BEQ, Instructions.BLT, Instructions.BLTU, Instructions.BGE, Instructions.BGEU, Instructions.JAL, Instructions.JALR, Instructions.AUIPC, Instructions.LB, Instructions.LH, Instructions.LW, Instructions.LBU, Instructions.LHU, Instructions.SB, Instructions.SH, Instructions.SW, Instructions.LUI, Instructions.ADDI, Instructions.SLTI, Instructions.SLTIU, Instructions.ANDI, Instructions.ORI, Instructions.XORI, Instructions.ADD, Instructions.SUB, Instructions.SLT, Instructions.SLTU, Instructions.AND, Instructions.OR, Instructions.XOR, Instructions.SLL, Instructions.SRL, Instructions.SRA, Instructions.FENCE, Instructions.ECALL, Instructions.EBREAK, Instructions.MRET, Instructions.WFI, Instructions.CEASE, Instructions.CSRRW, Instructions.CSRRS, Instructions.CSRRC, Instructions.CSRRWI, Instructions.CSRRSI, Instructions.CSRRCI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BNE, Instructions.BEQ, Instructions.BLT, Instructions.BLTU, Instructions.BGE, Instructions.BGEU, Instructions.JAL, Instructions.JALR, Instructions.AUIPC, Instructions.LB, Instructions.LH, Instructions.LW, Instructions.LBU, Instructions.LHU, Instructions.SB, Instructions.SH, Instructions.SW, Instructions.LUI, Instructions.ADDI, Instructions.SLTI, Instructions.SLTIU, Instructions.ANDI, Instructions.ORI, Instructions.XORI, Instructions.ADD, Instructions.SUB, Instructions.SLT, Instructions.SLTU, Instructions.AND, Instructions.OR, Instructions.XOR, Instructions.SLL, Instructions.SRL, Instructions.SRA, Instructions.FENCE, Instructions.ECALL, Instructions.EBREAK, Instructions.MRET, Instructions.WFI, Instructions.CEASE, Instructions.CSRRW, Instructions.CSRRS, Instructions.CSRRC, Instructions.CSRRWI, Instructions.CSRRSI, Instructions.CSRRCI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BNE, Instructions.BEQ, Instructions.BLT, Instructions.BLTU, Instructions.BGE, Instructions.BGEU, Instructions.JAL, Instructions.JALR, Instructions.AUIPC, Instructions.LB, Instructions.LH, Instructions.LW, Instructions.LBU, Instructions.LHU, Instructions.SB, Instructions.SH, Instructions.SW, Instructions.LUI, Instructions.ADDI, Instructions.SLTI, Instructions.SLTIU, Instructions.ANDI, Instructions.ORI, Instructions.XORI, Instructions.ADD, Instructions.SUB, Instructions.SLT, Instructions.SLTU, Instructions.AND, Instructions.OR, Instructions.XOR, Instructions.SLL, Instructions.SRL, Instructions.SRA, Instructions.FENCE, Instructions.ECALL, Instructions.EBREAK, Instructions.MRET, Instructions.WFI, Instructions.CEASE, Instructions.CSRRW, Instructions.CSRRS, Instructions.CSRRC, Instructions.CSRRWI, Instructions.CSRRSI, Instructions.CSRRCI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BNE, Instructions.BEQ, Instructions.BLT, Instructions.BLTU, Instructions.BGE, Instructions.BGEU, Instructions.JAL, Instructions.JALR, Instructions.AUIPC, Instructions.LB, Instructions.LH, Instructions.LW, Instructions.LBU, Instructions.LHU, Instructions.SB, Instructions.SH, Instructions.SW, Instructions.LUI, Instructions.ADDI, Instructions.SLTI, Instructions.SLTIU, Instructions.ANDI, Instructions.ORI, Instructions.XORI, Instructions.ADD, Instructions.SUB, Instructions.SLT, Instructions.SLTU, Instructions.AND, Instructions.OR, Instructions.XOR, Instructions.SLL, Instructions.SRL, Instructions.SRA, Instructions.FENCE, Instructions.ECALL, Instructions.EBREAK, Instructions.MRET, Instructions.WFI, Instructions.CEASE, Instructions.CSRRW, Instructions.CSRRS, Instructions.CSRRC, Instructions.CSRRWI, Instructions.CSRRSI, Instructions.CSRRCI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BNE, Instructions.BEQ, Instructions.BLT, Instructions.BLTU, Instructions.BGE, Instructions.BGEU, Instructions.JAL, Instructions.JALR, Instructions.AUIPC, Instructions.LB, Instructions.LH, Instructions.LW, Instructions.LBU, Instructions.LHU, Instructions.SB, Instructions.SH, Instructions.SW, Instructions.LUI, Instructions.ADDI, Instructions.SLTI, Instructions.SLTIU, Instructions.ANDI, Instructions.ORI, Instructions.XORI, Instructions.ADD, Instructions.SUB, Instructions.SLT, Instructions.SLTU, Instructions.AND, Instructions.OR, Instructions.XOR, Instructions.SLL, Instructions.SRL, Instructions.SRA, Instructions.FENCE, Instructions.ECALL, Instructions.EBREAK, Instructions.MRET, Instructions.WFI, Instructions.CEASE, Instructions.CSRRW, Instructions.CSRRS, Instructions.CSRRC, Instructions.CSRRWI, Instructions.CSRRSI, Instructions.CSRRCI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BNE, Instructions.BEQ, Instructions.BLT, Instructions.BLTU, Instructions.BGE, Instructions.BGEU, Instructions.JAL, Instructions.JALR, Instructions.AUIPC, Instructions.LB, Instructions.LH, Instructions.LW, Instructions.LBU, Instructions.LHU, Instructions.SB, Instructions.SH, Instructions.SW, Instructions.LUI, Instructions.ADDI, Instructions.SLTI, Instructions.SLTIU, Instructions.ANDI, Instructions.ORI, Instructions.XORI, Instructions.ADD, Instructions.SUB, Instructions.SLT, Instructions.SLTU, Instructions.AND, Instructions.OR, Instructions.XOR, Instructions.SLL, Instructions.SRL, Instructions.SRA, Instructions.FENCE, Instructions.ECALL, Instructions.EBREAK, Instructions.MRET, Instructions.WFI, Instructions.CEASE, Instructions.CSRRW, Instructions.CSRRS, Instructions.CSRRC, Instructions.CSRRWI, Instructions.CSRRSI, Instructions.CSRRCI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BNE, Instructions.BEQ, Instructions.BLT, Instructions.BLTU, Instructions.BGE, Instructions.BGEU, Instructions.SB, Instructions.SH, Instructions.SW, Instructions.FENCE, Instructions.ECALL, Instructions.EBREAK, Instructions.MRET, Instructions.WFI, Instructions.CEASE, 
  )
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.JAL, Instructions.JALR, Instructions.AUIPC, Instructions.LB, Instructions.LH, Instructions.LW, Instructions.LBU, Instructions.LHU, Instructions.LUI, Instructions.ADDI, Instructions.SLTI, Instructions.SLTIU, Instructions.ANDI, Instructions.ORI, Instructions.XORI, Instructions.ADD, Instructions.SUB, Instructions.SLT, Instructions.SLTU, Instructions.AND, Instructions.OR, Instructions.XOR, Instructions.SLL, Instructions.SRL, Instructions.SRA, Instructions.CSRRW, Instructions.CSRRS, Instructions.CSRRC, Instructions.CSRRWI, Instructions.CSRRSI, Instructions.CSRRCI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.BNE, Instructions.BEQ, Instructions.BLT, Instructions.BLTU, Instructions.BGE, Instructions.BGEU, Instructions.JAL, Instructions.JALR, Instructions.AUIPC, Instructions.LB, Instructions.LH, Instructions.LW, Instructions.LBU, Instructions.LHU, Instructions.SB, Instructions.SH, Instructions.SW, Instructions.LUI, Instructions.ADDI, Instructions.SLTI, Instructions.SLTIU, Instructions.ANDI, Instructions.ORI, Instructions.XORI, Instructions.ADD, Instructions.SUB, Instructions.SLT, Instructions.SLTU, Instructions.AND, Instructions.OR, Instructions.XOR, Instructions.SLL, Instructions.SRL, Instructions.SRA, Instructions.FENCE, 
  )
  val insns_CSR_I: Seq[BitPat] = Seq(
    Instructions.ECALL, Instructions.EBREAK, Instructions.MRET, Instructions.WFI, Instructions.CEASE, 
  )
  val insns_CSR_W: Seq[BitPat] = Seq(
    Instructions.CSRRW, Instructions.CSRRWI, 
  )
  val insns_CSR_S: Seq[BitPat] = Seq(
    Instructions.CSRRS, Instructions.CSRRSI, 
  )
  val insns_CSR_C: Seq[BitPat] = Seq(
    Instructions.CSRRC, Instructions.CSRRCI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else if (insns_CSR_I.exists(op.bitPat)) CSR.I else if (insns_CSR_W.exists(op.bitPat)) CSR.W else if (insns_CSR_S.exists(op.bitPat)) CSR.S else if (insns_CSR_C.exists(op.bitPat)) CSR.C else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BNE, Instructions.BEQ, Instructions.BLT, Instructions.BLTU, Instructions.BGE, Instructions.BGEU, Instructions.JAL, Instructions.JALR, Instructions.AUIPC, Instructions.LB, Instructions.LH, Instructions.LW, Instructions.LBU, Instructions.LHU, Instructions.SB, Instructions.SH, Instructions.SW, Instructions.LUI, Instructions.ADDI, Instructions.SLTI, Instructions.SLTIU, Instructions.ANDI, Instructions.ORI, Instructions.XORI, Instructions.ADD, Instructions.SUB, Instructions.SLT, Instructions.SLTU, Instructions.AND, Instructions.OR, Instructions.XOR, Instructions.SLL, Instructions.SRL, Instructions.SRA, Instructions.FENCE, Instructions.ECALL, Instructions.EBREAK, Instructions.MRET, Instructions.WFI, Instructions.CEASE, Instructions.CSRRW, Instructions.CSRRS, Instructions.CSRRC, Instructions.CSRRWI, Instructions.CSRRSI, Instructions.CSRRCI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BNE, Instructions.BEQ, Instructions.BLT, Instructions.BLTU, Instructions.BGE, Instructions.BGEU, Instructions.JAL, Instructions.JALR, Instructions.AUIPC, Instructions.LB, Instructions.LH, Instructions.LW, Instructions.LBU, Instructions.LHU, Instructions.SB, Instructions.SH, Instructions.SW, Instructions.LUI, Instructions.ADDI, Instructions.SLTI, Instructions.SLTIU, Instructions.ANDI, Instructions.ORI, Instructions.XORI, Instructions.ADD, Instructions.SUB, Instructions.SLT, Instructions.SLTU, Instructions.AND, Instructions.OR, Instructions.XOR, Instructions.SLL, Instructions.SRL, Instructions.SRA, Instructions.ECALL, Instructions.EBREAK, Instructions.MRET, Instructions.WFI, Instructions.CEASE, Instructions.CSRRW, Instructions.CSRRS, Instructions.CSRRC, Instructions.CSRRWI, Instructions.CSRRSI, Instructions.CSRRCI, 
  )
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FENCE, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BNE, Instructions.BEQ, Instructions.BLT, Instructions.BLTU, Instructions.BGE, Instructions.BGEU, Instructions.JAL, Instructions.JALR, Instructions.AUIPC, Instructions.LB, Instructions.LH, Instructions.LW, Instructions.LBU, Instructions.LHU, Instructions.SB, Instructions.SH, Instructions.SW, Instructions.LUI, Instructions.ADDI, Instructions.SLTI, Instructions.SLTIU, Instructions.ANDI, Instructions.ORI, Instructions.XORI, Instructions.ADD, Instructions.SUB, Instructions.SLT, Instructions.SLTU, Instructions.AND, Instructions.OR, Instructions.XOR, Instructions.SLL, Instructions.SRL, Instructions.SRA, Instructions.FENCE, Instructions.ECALL, Instructions.EBREAK, Instructions.MRET, Instructions.WFI, Instructions.CEASE, Instructions.CSRRW, Instructions.CSRRS, Instructions.CSRRC, Instructions.CSRRWI, Instructions.CSRRSI, Instructions.CSRRCI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BNE, Instructions.BEQ, Instructions.BLT, Instructions.BLTU, Instructions.BGE, Instructions.BGEU, Instructions.JAL, Instructions.JALR, Instructions.AUIPC, Instructions.LB, Instructions.LH, Instructions.LW, Instructions.LBU, Instructions.LHU, Instructions.SB, Instructions.SH, Instructions.SW, Instructions.LUI, Instructions.ADDI, Instructions.SLTI, Instructions.SLTIU, Instructions.ANDI, Instructions.ORI, Instructions.XORI, Instructions.ADD, Instructions.SUB, Instructions.SLT, Instructions.SLTU, Instructions.AND, Instructions.OR, Instructions.XOR, Instructions.SLL, Instructions.SRL, Instructions.SRA, Instructions.FENCE, Instructions.ECALL, Instructions.EBREAK, Instructions.MRET, Instructions.WFI, Instructions.CEASE, Instructions.CSRRW, Instructions.CSRRS, Instructions.CSRRC, Instructions.CSRRWI, Instructions.CSRRSI, Instructions.CSRRCI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

class FenceIDecode(flushDCache: Boolean, aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  private val (v, cmd) = if (flushDCache) (Y, BitPat(M_FLUSH_ALL)) else (N, M_X)
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FENCE_I, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FENCE_I, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FENCE_I, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FENCE_I, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FENCE_I, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FENCE_I, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FENCE_I, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FENCE_I, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FENCE_I, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FENCE_I, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FENCE_I, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FENCE_I, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_X: Seq[BitPat] = Seq(
    Instructions.FENCE_I, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_X.exists(op.bitPat)) A2_X else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_X: Seq[BitPat] = Seq(
    Instructions.FENCE_I, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_X.exists(op.bitPat)) A1_X else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.FENCE_I, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_X: Seq[BitPat] = Seq(
    Instructions.FENCE_I, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_X.exists(op.bitPat)) DW_X else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_X: Seq[BitPat] = Seq(
    Instructions.FENCE_I, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_X.exists(op.bitPat)) aluFn.FN_X else FN_X
  }
}
object mem extends BoolField {
  val insns_v: Seq[BitPat] = Seq(
    Instructions.FENCE_I, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_v.exists(op.bitPat)) v else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_cmd: Seq[BitPat] = Seq(
    Instructions.FENCE_I, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_cmd.exists(op.bitPat)) cmd else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FENCE_I, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FENCE_I, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FENCE_I, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FENCE_I, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FENCE_I, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FENCE_I, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FENCE_I, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.FENCE_I, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FENCE_I, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fence extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FENCE_I, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FENCE_I, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FENCE_I, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

class CFlushDecode(supportsFlushLine: Boolean, aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  private def zapRs1(x: BitPat) = if (supportsFlushLine) x else BitPat(x.value.U)
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.zapRs1(CFLUSH_D_L1), Instructions.zapRs1(CDISCARD_D_L1), 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.zapRs1(CFLUSH_D_L1), Instructions.zapRs1(CDISCARD_D_L1), 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.zapRs1(CFLUSH_D_L1), Instructions.zapRs1(CDISCARD_D_L1), 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.zapRs1(CFLUSH_D_L1), Instructions.zapRs1(CDISCARD_D_L1), 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.zapRs1(CFLUSH_D_L1), Instructions.zapRs1(CDISCARD_D_L1), 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.zapRs1(CFLUSH_D_L1), Instructions.zapRs1(CDISCARD_D_L1), 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.zapRs1(CFLUSH_D_L1), Instructions.zapRs1(CDISCARD_D_L1), 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.zapRs1(CFLUSH_D_L1), Instructions.zapRs1(CDISCARD_D_L1), 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.zapRs1(CFLUSH_D_L1), Instructions.zapRs1(CDISCARD_D_L1), 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.zapRs1(CFLUSH_D_L1), Instructions.zapRs1(CDISCARD_D_L1), 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.zapRs1(CFLUSH_D_L1), Instructions.zapRs1(CDISCARD_D_L1), 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.zapRs1(CFLUSH_D_L1), Instructions.zapRs1(CDISCARD_D_L1), 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_ZERO: Seq[BitPat] = Seq(
    Instructions.zapRs1(CFLUSH_D_L1), Instructions.zapRs1(CDISCARD_D_L1), 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_ZERO.exists(op.bitPat)) A2_ZERO else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.zapRs1(CFLUSH_D_L1), Instructions.zapRs1(CDISCARD_D_L1), 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.zapRs1(CFLUSH_D_L1), Instructions.zapRs1(CDISCARD_D_L1), 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.zapRs1(CFLUSH_D_L1), Instructions.zapRs1(CDISCARD_D_L1), 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_ADD: Seq[BitPat] = Seq(
    Instructions.zapRs1(CFLUSH_D_L1), Instructions.zapRs1(CDISCARD_D_L1), 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_ADD.exists(op.bitPat)) aluFn.FN_ADD else FN_X
  }
}
object mem extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.zapRs1(CFLUSH_D_L1), Instructions.zapRs1(CDISCARD_D_L1), 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_FLUSH_ALL: Seq[BitPat] = Seq(
    Instructions.zapRs1(CFLUSH_D_L1), Instructions.zapRs1(CDISCARD_D_L1), 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_FLUSH_ALL.exists(op.bitPat)) M_FLUSH_ALL else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.zapRs1(CFLUSH_D_L1), Instructions.zapRs1(CDISCARD_D_L1), 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.zapRs1(CFLUSH_D_L1), Instructions.zapRs1(CDISCARD_D_L1), 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.zapRs1(CFLUSH_D_L1), Instructions.zapRs1(CDISCARD_D_L1), 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.zapRs1(CFLUSH_D_L1), Instructions.zapRs1(CDISCARD_D_L1), 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.zapRs1(CFLUSH_D_L1), Instructions.zapRs1(CDISCARD_D_L1), 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.zapRs1(CFLUSH_D_L1), Instructions.zapRs1(CDISCARD_D_L1), 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.zapRs1(CFLUSH_D_L1), Instructions.zapRs1(CDISCARD_D_L1), 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_I: Seq[BitPat] = Seq(
    Instructions.zapRs1(CFLUSH_D_L1), Instructions.zapRs1(CDISCARD_D_L1), 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_I.exists(op.bitPat)) CSR.I else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.zapRs1(CFLUSH_D_L1), Instructions.zapRs1(CDISCARD_D_L1), 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.zapRs1(CFLUSH_D_L1), Instructions.zapRs1(CDISCARD_D_L1), 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.zapRs1(CFLUSH_D_L1), Instructions.zapRs1(CDISCARD_D_L1), 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.zapRs1(CFLUSH_D_L1), Instructions.zapRs1(CDISCARD_D_L1), 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

class SVMDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.SFENCE_VMA, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SFENCE_VMA, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SFENCE_VMA, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SFENCE_VMA, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SFENCE_VMA, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SFENCE_VMA, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.SFENCE_VMA, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.SFENCE_VMA, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SFENCE_VMA, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SFENCE_VMA, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SFENCE_VMA, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SFENCE_VMA, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_ZERO: Seq[BitPat] = Seq(
    Instructions.SFENCE_VMA, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_ZERO.exists(op.bitPat)) A2_ZERO else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.SFENCE_VMA, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.SFENCE_VMA, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.SFENCE_VMA, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_ADD: Seq[BitPat] = Seq(
    Instructions.SFENCE_VMA, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_ADD.exists(op.bitPat)) aluFn.FN_ADD else FN_X
  }
}
object mem extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.SFENCE_VMA, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_SFENCE: Seq[BitPat] = Seq(
    Instructions.SFENCE_VMA, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_SFENCE.exists(op.bitPat)) M_SFENCE else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SFENCE_VMA, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SFENCE_VMA, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SFENCE_VMA, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SFENCE_VMA, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SFENCE_VMA, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SFENCE_VMA, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SFENCE_VMA, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_I: Seq[BitPat] = Seq(
    Instructions.SFENCE_VMA, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_I.exists(op.bitPat)) CSR.I else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SFENCE_VMA, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SFENCE_VMA, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SFENCE_VMA, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SFENCE_VMA, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

class SDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.SRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs1 extends BoolField {
  val insns_X: Seq[BitPat] = Seq(
    Instructions.SRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_X.exists(op.bitPat)) X else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_X: Seq[BitPat] = Seq(
    Instructions.SRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_X.exists(op.bitPat)) A2_X else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_X: Seq[BitPat] = Seq(
    Instructions.SRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_X.exists(op.bitPat)) A1_X else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.SRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_X: Seq[BitPat] = Seq(
    Instructions.SRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_X.exists(op.bitPat)) DW_X else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_X: Seq[BitPat] = Seq(
    Instructions.SRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_X.exists(op.bitPat)) aluFn.FN_X else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.SRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_I: Seq[BitPat] = Seq(
    Instructions.SRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_I.exists(op.bitPat)) CSR.I else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

class HypervisorDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.HFENCE_VVMA, Instructions.HFENCE_GVMA, Instructions.HLV_B, Instructions.HLV_BU, Instructions.HLV_H, Instructions.HLV_HU, Instructions.HLVX_HU, Instructions.HLV_W, Instructions.HLVX_WU, Instructions.HSV_B, Instructions.HSV_H, Instructions.HSV_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HFENCE_VVMA, Instructions.HFENCE_GVMA, Instructions.HLV_B, Instructions.HLV_BU, Instructions.HLV_H, Instructions.HLV_HU, Instructions.HLVX_HU, Instructions.HLV_W, Instructions.HLVX_WU, Instructions.HSV_B, Instructions.HSV_H, Instructions.HSV_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HFENCE_VVMA, Instructions.HFENCE_GVMA, Instructions.HLV_B, Instructions.HLV_BU, Instructions.HLV_H, Instructions.HLV_HU, Instructions.HLVX_HU, Instructions.HLV_W, Instructions.HLVX_WU, Instructions.HSV_B, Instructions.HSV_H, Instructions.HSV_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HFENCE_VVMA, Instructions.HFENCE_GVMA, Instructions.HLV_B, Instructions.HLV_BU, Instructions.HLV_H, Instructions.HLV_HU, Instructions.HLVX_HU, Instructions.HLV_W, Instructions.HLVX_WU, Instructions.HSV_B, Instructions.HSV_H, Instructions.HSV_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HFENCE_VVMA, Instructions.HFENCE_GVMA, Instructions.HLV_B, Instructions.HLV_BU, Instructions.HLV_H, Instructions.HLV_HU, Instructions.HLVX_HU, Instructions.HLV_W, Instructions.HLVX_WU, Instructions.HSV_B, Instructions.HSV_H, Instructions.HSV_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HFENCE_VVMA, Instructions.HFENCE_GVMA, Instructions.HLV_B, Instructions.HLV_BU, Instructions.HLV_H, Instructions.HLV_HU, Instructions.HLVX_HU, Instructions.HLV_W, Instructions.HLVX_WU, Instructions.HSV_B, Instructions.HSV_H, Instructions.HSV_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.HFENCE_VVMA, Instructions.HFENCE_GVMA, Instructions.HSV_B, Instructions.HSV_H, Instructions.HSV_W, 
  )
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HLV_B, Instructions.HLV_BU, Instructions.HLV_H, Instructions.HLV_HU, Instructions.HLVX_HU, Instructions.HLV_W, Instructions.HLVX_WU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.HFENCE_VVMA, Instructions.HFENCE_GVMA, Instructions.HLV_B, Instructions.HLV_BU, Instructions.HLV_H, Instructions.HLV_HU, Instructions.HLVX_HU, Instructions.HLV_W, Instructions.HLVX_WU, Instructions.HSV_B, Instructions.HSV_H, Instructions.HSV_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HFENCE_VVMA, Instructions.HFENCE_GVMA, Instructions.HLV_B, Instructions.HLV_BU, Instructions.HLV_H, Instructions.HLV_HU, Instructions.HLVX_HU, Instructions.HLV_W, Instructions.HLVX_WU, Instructions.HSV_B, Instructions.HSV_H, Instructions.HSV_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HFENCE_VVMA, Instructions.HFENCE_GVMA, Instructions.HLV_B, Instructions.HLV_BU, Instructions.HLV_H, Instructions.HLV_HU, Instructions.HLVX_HU, Instructions.HLV_W, Instructions.HLVX_WU, Instructions.HSV_B, Instructions.HSV_H, Instructions.HSV_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HFENCE_VVMA, Instructions.HFENCE_GVMA, Instructions.HLV_B, Instructions.HLV_BU, Instructions.HLV_H, Instructions.HLV_HU, Instructions.HLVX_HU, Instructions.HLV_W, Instructions.HLVX_WU, Instructions.HSV_B, Instructions.HSV_H, Instructions.HSV_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HFENCE_VVMA, Instructions.HFENCE_GVMA, Instructions.HLV_B, Instructions.HLV_BU, Instructions.HLV_H, Instructions.HLV_HU, Instructions.HLVX_HU, Instructions.HLV_W, Instructions.HLVX_WU, Instructions.HSV_B, Instructions.HSV_H, Instructions.HSV_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_ZERO: Seq[BitPat] = Seq(
    Instructions.HFENCE_VVMA, Instructions.HFENCE_GVMA, Instructions.HLV_B, Instructions.HLV_BU, Instructions.HLV_H, Instructions.HLV_HU, Instructions.HLVX_HU, Instructions.HLV_W, Instructions.HLVX_WU, Instructions.HSV_B, Instructions.HSV_H, Instructions.HSV_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_ZERO.exists(op.bitPat)) A2_ZERO else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.HFENCE_VVMA, Instructions.HFENCE_GVMA, Instructions.HLV_B, Instructions.HLV_BU, Instructions.HLV_H, Instructions.HLV_HU, Instructions.HLVX_HU, Instructions.HLV_W, Instructions.HLVX_WU, Instructions.HSV_B, Instructions.HSV_H, Instructions.HSV_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.HFENCE_VVMA, Instructions.HFENCE_GVMA, Instructions.HLV_B, Instructions.HLV_BU, Instructions.HLV_H, Instructions.HLV_HU, Instructions.HLVX_HU, Instructions.HLV_W, Instructions.HLVX_WU, 
  )
  val insns_IMM_I: Seq[BitPat] = Seq(
    Instructions.HSV_B, Instructions.HSV_H, Instructions.HSV_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else if (insns_IMM_I.exists(op.bitPat)) IMM_I else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.HFENCE_VVMA, Instructions.HFENCE_GVMA, Instructions.HLV_B, Instructions.HLV_BU, Instructions.HLV_H, Instructions.HLV_HU, Instructions.HLVX_HU, Instructions.HLV_W, Instructions.HLVX_WU, Instructions.HSV_B, Instructions.HSV_H, Instructions.HSV_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_ADD: Seq[BitPat] = Seq(
    Instructions.HFENCE_VVMA, Instructions.HFENCE_GVMA, Instructions.HLV_B, Instructions.HLV_BU, Instructions.HLV_H, Instructions.HLV_HU, Instructions.HLVX_HU, Instructions.HLV_W, Instructions.HLVX_WU, Instructions.HSV_B, Instructions.HSV_H, Instructions.HSV_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_ADD.exists(op.bitPat)) aluFn.FN_ADD else FN_X
  }
}
object mem extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.HFENCE_VVMA, Instructions.HFENCE_GVMA, Instructions.HLV_B, Instructions.HLV_BU, Instructions.HLV_H, Instructions.HLV_HU, Instructions.HLVX_HU, Instructions.HLV_W, Instructions.HLVX_WU, Instructions.HSV_B, Instructions.HSV_H, Instructions.HSV_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_HFENCEV: Seq[BitPat] = Seq(
    Instructions.HFENCE_VVMA, 
  )
  val insns_M_HFENCEG: Seq[BitPat] = Seq(
    Instructions.HFENCE_GVMA, 
  )
  val insns_M_XRD: Seq[BitPat] = Seq(
    Instructions.HLV_B, Instructions.HLV_BU, Instructions.HLV_H, Instructions.HLV_HU, Instructions.HLV_W, 
  )
  val insns_M_HLVX: Seq[BitPat] = Seq(
    Instructions.HLVX_HU, Instructions.HLVX_WU, 
  )
  val insns_M_XWR: Seq[BitPat] = Seq(
    Instructions.HSV_B, Instructions.HSV_H, Instructions.HSV_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_HFENCEV.exists(op.bitPat)) M_HFENCEV else if (insns_M_HFENCEG.exists(op.bitPat)) M_HFENCEG else if (insns_M_XRD.exists(op.bitPat)) M_XRD else if (insns_M_HLVX.exists(op.bitPat)) M_HLVX else if (insns_M_XWR.exists(op.bitPat)) M_XWR else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HFENCE_VVMA, Instructions.HFENCE_GVMA, Instructions.HLV_B, Instructions.HLV_BU, Instructions.HLV_H, Instructions.HLV_HU, Instructions.HLVX_HU, Instructions.HLV_W, Instructions.HLVX_WU, Instructions.HSV_B, Instructions.HSV_H, Instructions.HSV_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HFENCE_VVMA, Instructions.HFENCE_GVMA, Instructions.HLV_B, Instructions.HLV_BU, Instructions.HLV_H, Instructions.HLV_HU, Instructions.HLVX_HU, Instructions.HLV_W, Instructions.HLVX_WU, Instructions.HSV_B, Instructions.HSV_H, Instructions.HSV_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HFENCE_VVMA, Instructions.HFENCE_GVMA, Instructions.HLV_B, Instructions.HLV_BU, Instructions.HLV_H, Instructions.HLV_HU, Instructions.HLVX_HU, Instructions.HLV_W, Instructions.HLVX_WU, Instructions.HSV_B, Instructions.HSV_H, Instructions.HSV_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HFENCE_VVMA, Instructions.HFENCE_GVMA, Instructions.HLV_B, Instructions.HLV_BU, Instructions.HLV_H, Instructions.HLV_HU, Instructions.HLVX_HU, Instructions.HLV_W, Instructions.HLVX_WU, Instructions.HSV_B, Instructions.HSV_H, Instructions.HSV_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HFENCE_VVMA, Instructions.HFENCE_GVMA, Instructions.HLV_B, Instructions.HLV_BU, Instructions.HLV_H, Instructions.HLV_HU, Instructions.HLVX_HU, Instructions.HLV_W, Instructions.HLVX_WU, Instructions.HSV_B, Instructions.HSV_H, Instructions.HSV_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HFENCE_VVMA, Instructions.HFENCE_GVMA, Instructions.HLV_B, Instructions.HLV_BU, Instructions.HLV_H, Instructions.HLV_HU, Instructions.HLVX_HU, Instructions.HLV_W, Instructions.HLVX_WU, Instructions.HSV_B, Instructions.HSV_H, Instructions.HSV_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HFENCE_VVMA, Instructions.HFENCE_GVMA, Instructions.HSV_B, Instructions.HSV_H, Instructions.HSV_W, 
  )
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.HLV_B, Instructions.HLV_BU, Instructions.HLV_H, Instructions.HLV_HU, Instructions.HLVX_HU, Instructions.HLV_W, Instructions.HLVX_WU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_I: Seq[BitPat] = Seq(
    Instructions.HFENCE_VVMA, Instructions.HFENCE_GVMA, Instructions.HLV_B, Instructions.HLV_BU, Instructions.HLV_H, Instructions.HLV_HU, Instructions.HLVX_HU, Instructions.HLV_W, Instructions.HLVX_WU, Instructions.HSV_B, Instructions.HSV_H, Instructions.HSV_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_I.exists(op.bitPat)) CSR.I else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HFENCE_VVMA, Instructions.HFENCE_GVMA, Instructions.HLV_B, Instructions.HLV_BU, Instructions.HLV_H, Instructions.HLV_HU, Instructions.HLVX_HU, Instructions.HLV_W, Instructions.HLVX_WU, Instructions.HSV_B, Instructions.HSV_H, Instructions.HSV_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HFENCE_VVMA, Instructions.HFENCE_GVMA, Instructions.HLV_B, Instructions.HLV_BU, Instructions.HLV_H, Instructions.HLV_HU, Instructions.HLVX_HU, Instructions.HLV_W, Instructions.HLVX_WU, Instructions.HSV_B, Instructions.HSV_H, Instructions.HSV_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HFENCE_VVMA, Instructions.HFENCE_GVMA, Instructions.HLV_B, Instructions.HLV_BU, Instructions.HLV_H, Instructions.HLV_HU, Instructions.HLVX_HU, Instructions.HLV_W, Instructions.HLVX_WU, Instructions.HSV_B, Instructions.HSV_H, Instructions.HSV_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HFENCE_VVMA, Instructions.HFENCE_GVMA, Instructions.HLV_B, Instructions.HLV_BU, Instructions.HLV_H, Instructions.HLV_HU, Instructions.HLVX_HU, Instructions.HLV_W, Instructions.HLVX_WU, Instructions.HSV_B, Instructions.HSV_H, Instructions.HSV_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

class DebugDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.DRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.DRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.DRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.DRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.DRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.DRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.DRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs1 extends BoolField {
  val insns_X: Seq[BitPat] = Seq(
    Instructions.DRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_X.exists(op.bitPat)) X else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.DRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.DRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.DRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.DRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_X: Seq[BitPat] = Seq(
    Instructions.DRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_X.exists(op.bitPat)) A2_X else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_X: Seq[BitPat] = Seq(
    Instructions.DRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_X.exists(op.bitPat)) A1_X else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.DRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_X: Seq[BitPat] = Seq(
    Instructions.DRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_X.exists(op.bitPat)) DW_X else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_X: Seq[BitPat] = Seq(
    Instructions.DRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_X.exists(op.bitPat)) aluFn.FN_X else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.DRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.DRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.DRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.DRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.DRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.DRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.DRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.DRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.DRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_I: Seq[BitPat] = Seq(
    Instructions.DRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_I.exists(op.bitPat)) CSR.I else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.DRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.DRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.DRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.DRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

class NMIDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.MNRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MNRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MNRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MNRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MNRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MNRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MNRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs1 extends BoolField {
  val insns_X: Seq[BitPat] = Seq(
    Instructions.MNRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_X.exists(op.bitPat)) X else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MNRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MNRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MNRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MNRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_X: Seq[BitPat] = Seq(
    Instructions.MNRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_X.exists(op.bitPat)) A2_X else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_X: Seq[BitPat] = Seq(
    Instructions.MNRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_X.exists(op.bitPat)) A1_X else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.MNRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_X: Seq[BitPat] = Seq(
    Instructions.MNRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_X.exists(op.bitPat)) DW_X else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_X: Seq[BitPat] = Seq(
    Instructions.MNRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_X.exists(op.bitPat)) aluFn.FN_X else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MNRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.MNRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MNRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MNRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MNRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MNRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MNRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MNRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MNRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_I: Seq[BitPat] = Seq(
    Instructions.MNRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_I.exists(op.bitPat)) CSR.I else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MNRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MNRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MNRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MNRET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

class I32Decode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.Instructions32.SLLI, Instructions.Instructions32.SRLI, Instructions.Instructions32.SRAI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SLLI, Instructions.Instructions32.SRLI, Instructions.Instructions32.SRAI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SLLI, Instructions.Instructions32.SRLI, Instructions.Instructions32.SRAI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SLLI, Instructions.Instructions32.SRLI, Instructions.Instructions32.SRAI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SLLI, Instructions.Instructions32.SRLI, Instructions.Instructions32.SRAI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SLLI, Instructions.Instructions32.SRLI, Instructions.Instructions32.SRAI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SLLI, Instructions.Instructions32.SRLI, Instructions.Instructions32.SRAI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.Instructions32.SLLI, Instructions.Instructions32.SRLI, Instructions.Instructions32.SRAI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SLLI, Instructions.Instructions32.SRLI, Instructions.Instructions32.SRAI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SLLI, Instructions.Instructions32.SRLI, Instructions.Instructions32.SRAI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SLLI, Instructions.Instructions32.SRLI, Instructions.Instructions32.SRAI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SLLI, Instructions.Instructions32.SRLI, Instructions.Instructions32.SRAI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_IMM: Seq[BitPat] = Seq(
    Instructions.Instructions32.SLLI, Instructions.Instructions32.SRLI, Instructions.Instructions32.SRAI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_IMM.exists(op.bitPat)) A2_IMM else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.Instructions32.SLLI, Instructions.Instructions32.SRLI, Instructions.Instructions32.SRAI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_I: Seq[BitPat] = Seq(
    Instructions.Instructions32.SLLI, Instructions.Instructions32.SRLI, Instructions.Instructions32.SRAI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_I.exists(op.bitPat)) IMM_I else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.Instructions32.SLLI, Instructions.Instructions32.SRLI, Instructions.Instructions32.SRAI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_SL: Seq[BitPat] = Seq(
    Instructions.Instructions32.SLLI, 
  )
  val insns_aluFn_FN_SR: Seq[BitPat] = Seq(
    Instructions.Instructions32.SRLI, 
  )
  val insns_aluFn_FN_SRA: Seq[BitPat] = Seq(
    Instructions.Instructions32.SRAI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_SL.exists(op.bitPat)) aluFn.FN_SL else if (insns_aluFn_FN_SR.exists(op.bitPat)) aluFn.FN_SR else if (insns_aluFn_FN_SRA.exists(op.bitPat)) aluFn.FN_SRA else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SLLI, Instructions.Instructions32.SRLI, Instructions.Instructions32.SRAI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.Instructions32.SLLI, Instructions.Instructions32.SRLI, Instructions.Instructions32.SRAI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SLLI, Instructions.Instructions32.SRLI, Instructions.Instructions32.SRAI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SLLI, Instructions.Instructions32.SRLI, Instructions.Instructions32.SRAI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SLLI, Instructions.Instructions32.SRLI, Instructions.Instructions32.SRAI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SLLI, Instructions.Instructions32.SRLI, Instructions.Instructions32.SRAI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SLLI, Instructions.Instructions32.SRLI, Instructions.Instructions32.SRAI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SLLI, Instructions.Instructions32.SRLI, Instructions.Instructions32.SRAI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.Instructions32.SLLI, Instructions.Instructions32.SRLI, Instructions.Instructions32.SRAI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SLLI, Instructions.Instructions32.SRLI, Instructions.Instructions32.SRAI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SLLI, Instructions.Instructions32.SRLI, Instructions.Instructions32.SRAI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SLLI, Instructions.Instructions32.SRLI, Instructions.Instructions32.SRAI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SLLI, Instructions.Instructions32.SRLI, Instructions.Instructions32.SRAI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SLLI, Instructions.Instructions32.SRLI, Instructions.Instructions32.SRAI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

class I64Decode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.LD, Instructions.LWU, Instructions.SD, Instructions.SLLI, Instructions.SRLI, Instructions.SRAI, Instructions.ADDIW, Instructions.SLLIW, Instructions.SRLIW, Instructions.SRAIW, Instructions.ADDW, Instructions.SUBW, Instructions.SLLW, Instructions.SRLW, Instructions.SRAW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.LD, Instructions.LWU, Instructions.SD, Instructions.SLLI, Instructions.SRLI, Instructions.SRAI, Instructions.ADDIW, Instructions.SLLIW, Instructions.SRLIW, Instructions.SRAIW, Instructions.ADDW, Instructions.SUBW, Instructions.SLLW, Instructions.SRLW, Instructions.SRAW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.LD, Instructions.LWU, Instructions.SD, Instructions.SLLI, Instructions.SRLI, Instructions.SRAI, Instructions.ADDIW, Instructions.SLLIW, Instructions.SRLIW, Instructions.SRAIW, Instructions.ADDW, Instructions.SUBW, Instructions.SLLW, Instructions.SRLW, Instructions.SRAW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.LD, Instructions.LWU, Instructions.SD, Instructions.SLLI, Instructions.SRLI, Instructions.SRAI, Instructions.ADDIW, Instructions.SLLIW, Instructions.SRLIW, Instructions.SRAIW, Instructions.ADDW, Instructions.SUBW, Instructions.SLLW, Instructions.SRLW, Instructions.SRAW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.LD, Instructions.LWU, Instructions.SD, Instructions.SLLI, Instructions.SRLI, Instructions.SRAI, Instructions.ADDIW, Instructions.SLLIW, Instructions.SRLIW, Instructions.SRAIW, Instructions.ADDW, Instructions.SUBW, Instructions.SLLW, Instructions.SRLW, Instructions.SRAW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.LD, Instructions.LWU, Instructions.SD, Instructions.SLLI, Instructions.SRLI, Instructions.SRAI, Instructions.ADDIW, Instructions.SLLIW, Instructions.SRLIW, Instructions.SRAIW, Instructions.ADDW, Instructions.SUBW, Instructions.SLLW, Instructions.SRLW, Instructions.SRAW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.LD, Instructions.LWU, Instructions.SLLI, Instructions.SRLI, Instructions.SRAI, Instructions.ADDIW, Instructions.SLLIW, Instructions.SRLIW, Instructions.SRAIW, 
  )
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.SD, Instructions.ADDW, Instructions.SUBW, Instructions.SLLW, Instructions.SRLW, Instructions.SRAW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.LD, Instructions.LWU, Instructions.SD, Instructions.SLLI, Instructions.SRLI, Instructions.SRAI, Instructions.ADDIW, Instructions.SLLIW, Instructions.SRLIW, Instructions.SRAIW, Instructions.ADDW, Instructions.SUBW, Instructions.SLLW, Instructions.SRLW, Instructions.SRAW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.LD, Instructions.LWU, Instructions.SD, Instructions.SLLI, Instructions.SRLI, Instructions.SRAI, Instructions.ADDIW, Instructions.SLLIW, Instructions.SRLIW, Instructions.SRAIW, Instructions.ADDW, Instructions.SUBW, Instructions.SLLW, Instructions.SRLW, Instructions.SRAW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.LD, Instructions.LWU, Instructions.SD, Instructions.SLLI, Instructions.SRLI, Instructions.SRAI, Instructions.ADDIW, Instructions.SLLIW, Instructions.SRLIW, Instructions.SRAIW, Instructions.ADDW, Instructions.SUBW, Instructions.SLLW, Instructions.SRLW, Instructions.SRAW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.LD, Instructions.LWU, Instructions.SD, Instructions.SLLI, Instructions.SRLI, Instructions.SRAI, Instructions.ADDIW, Instructions.SLLIW, Instructions.SRLIW, Instructions.SRAIW, Instructions.ADDW, Instructions.SUBW, Instructions.SLLW, Instructions.SRLW, Instructions.SRAW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.LD, Instructions.LWU, Instructions.SD, Instructions.SLLI, Instructions.SRLI, Instructions.SRAI, Instructions.ADDIW, Instructions.SLLIW, Instructions.SRLIW, Instructions.SRAIW, Instructions.ADDW, Instructions.SUBW, Instructions.SLLW, Instructions.SRLW, Instructions.SRAW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_IMM: Seq[BitPat] = Seq(
    Instructions.LD, Instructions.LWU, Instructions.SD, Instructions.SLLI, Instructions.SRLI, Instructions.SRAI, Instructions.ADDIW, Instructions.SLLIW, Instructions.SRLIW, Instructions.SRAIW, 
  )
  val insns_A2_RS2: Seq[BitPat] = Seq(
    Instructions.ADDW, Instructions.SUBW, Instructions.SLLW, Instructions.SRLW, Instructions.SRAW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_IMM.exists(op.bitPat)) A2_IMM else if (insns_A2_RS2.exists(op.bitPat)) A2_RS2 else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.LD, Instructions.LWU, Instructions.SD, Instructions.SLLI, Instructions.SRLI, Instructions.SRAI, Instructions.ADDIW, Instructions.SLLIW, Instructions.SRLIW, Instructions.SRAIW, Instructions.ADDW, Instructions.SUBW, Instructions.SLLW, Instructions.SRLW, Instructions.SRAW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_I: Seq[BitPat] = Seq(
    Instructions.LD, Instructions.LWU, Instructions.SLLI, Instructions.SRLI, Instructions.SRAI, Instructions.ADDIW, Instructions.SLLIW, Instructions.SRLIW, Instructions.SRAIW, 
  )
  val insns_IMM_S: Seq[BitPat] = Seq(
    Instructions.SD, 
  )
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.ADDW, Instructions.SUBW, Instructions.SLLW, Instructions.SRLW, Instructions.SRAW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_I.exists(op.bitPat)) IMM_I else if (insns_IMM_S.exists(op.bitPat)) IMM_S else if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.LD, Instructions.LWU, Instructions.SD, Instructions.SLLI, Instructions.SRLI, Instructions.SRAI, 
  )
  val insns_DW_32: Seq[BitPat] = Seq(
    Instructions.ADDIW, Instructions.SLLIW, Instructions.SRLIW, Instructions.SRAIW, Instructions.ADDW, Instructions.SUBW, Instructions.SLLW, Instructions.SRLW, Instructions.SRAW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else if (insns_DW_32.exists(op.bitPat)) DW_32 else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_ADD: Seq[BitPat] = Seq(
    Instructions.LD, Instructions.LWU, Instructions.SD, Instructions.ADDIW, Instructions.ADDW, 
  )
  val insns_aluFn_FN_SL: Seq[BitPat] = Seq(
    Instructions.SLLI, Instructions.SLLIW, Instructions.SLLW, 
  )
  val insns_aluFn_FN_SR: Seq[BitPat] = Seq(
    Instructions.SRLI, Instructions.SRLIW, Instructions.SRLW, 
  )
  val insns_aluFn_FN_SRA: Seq[BitPat] = Seq(
    Instructions.SRAI, Instructions.SRAIW, Instructions.SRAW, 
  )
  val insns_aluFn_FN_SUB: Seq[BitPat] = Seq(
    Instructions.SUBW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_ADD.exists(op.bitPat)) aluFn.FN_ADD else if (insns_aluFn_FN_SL.exists(op.bitPat)) aluFn.FN_SL else if (insns_aluFn_FN_SR.exists(op.bitPat)) aluFn.FN_SR else if (insns_aluFn_FN_SRA.exists(op.bitPat)) aluFn.FN_SRA else if (insns_aluFn_FN_SUB.exists(op.bitPat)) aluFn.FN_SUB else FN_X
  }
}
object mem extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.LD, Instructions.LWU, Instructions.SD, 
  )
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SLLI, Instructions.SRLI, Instructions.SRAI, Instructions.ADDIW, Instructions.SLLIW, Instructions.SRLIW, Instructions.SRAIW, Instructions.ADDW, Instructions.SUBW, Instructions.SLLW, Instructions.SRLW, Instructions.SRAW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_XRD: Seq[BitPat] = Seq(
    Instructions.LD, Instructions.LWU, 
  )
  val insns_M_XWR: Seq[BitPat] = Seq(
    Instructions.SD, 
  )
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.SLLI, Instructions.SRLI, Instructions.SRAI, Instructions.ADDIW, Instructions.SLLIW, Instructions.SRLIW, Instructions.SRAIW, Instructions.ADDW, Instructions.SUBW, Instructions.SLLW, Instructions.SRLW, Instructions.SRAW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_XRD.exists(op.bitPat)) M_XRD else if (insns_M_XWR.exists(op.bitPat)) M_XWR else if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.LD, Instructions.LWU, Instructions.SD, Instructions.SLLI, Instructions.SRLI, Instructions.SRAI, Instructions.ADDIW, Instructions.SLLIW, Instructions.SRLIW, Instructions.SRAIW, Instructions.ADDW, Instructions.SUBW, Instructions.SLLW, Instructions.SRLW, Instructions.SRAW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.LD, Instructions.LWU, Instructions.SD, Instructions.SLLI, Instructions.SRLI, Instructions.SRAI, Instructions.ADDIW, Instructions.SLLIW, Instructions.SRLIW, Instructions.SRAIW, Instructions.ADDW, Instructions.SUBW, Instructions.SLLW, Instructions.SRLW, Instructions.SRAW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.LD, Instructions.LWU, Instructions.SD, Instructions.SLLI, Instructions.SRLI, Instructions.SRAI, Instructions.ADDIW, Instructions.SLLIW, Instructions.SRLIW, Instructions.SRAIW, Instructions.ADDW, Instructions.SUBW, Instructions.SLLW, Instructions.SRLW, Instructions.SRAW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.LD, Instructions.LWU, Instructions.SD, Instructions.SLLI, Instructions.SRLI, Instructions.SRAI, Instructions.ADDIW, Instructions.SLLIW, Instructions.SRLIW, Instructions.SRAIW, Instructions.ADDW, Instructions.SUBW, Instructions.SLLW, Instructions.SRLW, Instructions.SRAW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.LD, Instructions.LWU, Instructions.SD, Instructions.SLLI, Instructions.SRLI, Instructions.SRAI, Instructions.ADDIW, Instructions.SLLIW, Instructions.SRLIW, Instructions.SRAIW, Instructions.ADDW, Instructions.SUBW, Instructions.SLLW, Instructions.SRLW, Instructions.SRAW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.LD, Instructions.LWU, Instructions.SD, Instructions.SLLI, Instructions.SRLI, Instructions.SRAI, Instructions.ADDIW, Instructions.SLLIW, Instructions.SRLIW, Instructions.SRAIW, Instructions.ADDW, Instructions.SUBW, Instructions.SLLW, Instructions.SRLW, Instructions.SRAW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.LD, Instructions.LWU, Instructions.SLLI, Instructions.SRLI, Instructions.SRAI, Instructions.ADDIW, Instructions.SLLIW, Instructions.SRLIW, Instructions.SRAIW, Instructions.ADDW, Instructions.SUBW, Instructions.SLLW, Instructions.SRLW, Instructions.SRAW, 
  )
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else if (insns_N.exists(op.bitPat)) N else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.LD, Instructions.LWU, Instructions.SD, Instructions.SLLI, Instructions.SRLI, Instructions.SRAI, Instructions.ADDIW, Instructions.SLLIW, Instructions.SRLIW, Instructions.SRAIW, Instructions.ADDW, Instructions.SUBW, Instructions.SLLW, Instructions.SRLW, Instructions.SRAW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.LD, Instructions.LWU, Instructions.SD, Instructions.SLLI, Instructions.SRLI, Instructions.SRAI, Instructions.ADDIW, Instructions.SLLIW, Instructions.SRLIW, Instructions.SRAIW, Instructions.ADDW, Instructions.SUBW, Instructions.SLLW, Instructions.SRLW, Instructions.SRAW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.LD, Instructions.LWU, Instructions.SD, Instructions.SLLI, Instructions.SRLI, Instructions.SRAI, Instructions.ADDIW, Instructions.SLLIW, Instructions.SRLIW, Instructions.SRAIW, Instructions.ADDW, Instructions.SUBW, Instructions.SLLW, Instructions.SRLW, Instructions.SRAW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.LD, Instructions.LWU, Instructions.SD, Instructions.SLLI, Instructions.SRLI, Instructions.SRAI, Instructions.ADDIW, Instructions.SLLIW, Instructions.SRLIW, Instructions.SRAIW, Instructions.ADDW, Instructions.SUBW, Instructions.SLLW, Instructions.SRLW, Instructions.SRAW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.LD, Instructions.LWU, Instructions.SD, Instructions.SLLI, Instructions.SRLI, Instructions.SRAI, Instructions.ADDIW, Instructions.SLLIW, Instructions.SRLIW, Instructions.SRAIW, Instructions.ADDW, Instructions.SUBW, Instructions.SLLW, Instructions.SRLW, Instructions.SRAW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

class Hypervisor64Decode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.HLV_D, Instructions.HSV_D, Instructions.HLV_WU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HLV_D, Instructions.HSV_D, Instructions.HLV_WU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HLV_D, Instructions.HSV_D, Instructions.HLV_WU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HLV_D, Instructions.HSV_D, Instructions.HLV_WU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HLV_D, Instructions.HSV_D, Instructions.HLV_WU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HLV_D, Instructions.HSV_D, Instructions.HLV_WU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HLV_D, Instructions.HLV_WU, 
  )
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.HSV_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.HLV_D, Instructions.HSV_D, Instructions.HLV_WU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HLV_D, Instructions.HSV_D, Instructions.HLV_WU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HLV_D, Instructions.HSV_D, Instructions.HLV_WU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HLV_D, Instructions.HSV_D, Instructions.HLV_WU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HLV_D, Instructions.HSV_D, Instructions.HLV_WU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_ZERO: Seq[BitPat] = Seq(
    Instructions.HLV_D, Instructions.HSV_D, Instructions.HLV_WU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_ZERO.exists(op.bitPat)) A2_ZERO else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.HLV_D, Instructions.HSV_D, Instructions.HLV_WU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.HLV_D, Instructions.HLV_WU, 
  )
  val insns_IMM_I: Seq[BitPat] = Seq(
    Instructions.HSV_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else if (insns_IMM_I.exists(op.bitPat)) IMM_I else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.HLV_D, Instructions.HSV_D, Instructions.HLV_WU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_ADD: Seq[BitPat] = Seq(
    Instructions.HLV_D, Instructions.HSV_D, Instructions.HLV_WU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_ADD.exists(op.bitPat)) aluFn.FN_ADD else FN_X
  }
}
object mem extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.HLV_D, Instructions.HSV_D, Instructions.HLV_WU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_XRD: Seq[BitPat] = Seq(
    Instructions.HLV_D, Instructions.HLV_WU, 
  )
  val insns_M_XWR: Seq[BitPat] = Seq(
    Instructions.HSV_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_XRD.exists(op.bitPat)) M_XRD else if (insns_M_XWR.exists(op.bitPat)) M_XWR else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HLV_D, Instructions.HSV_D, Instructions.HLV_WU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HLV_D, Instructions.HSV_D, Instructions.HLV_WU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HLV_D, Instructions.HSV_D, Instructions.HLV_WU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HLV_D, Instructions.HSV_D, Instructions.HLV_WU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HLV_D, Instructions.HSV_D, Instructions.HLV_WU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HLV_D, Instructions.HSV_D, Instructions.HLV_WU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.HLV_D, Instructions.HLV_WU, 
  )
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HSV_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else if (insns_N.exists(op.bitPat)) N else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_I: Seq[BitPat] = Seq(
    Instructions.HLV_D, Instructions.HSV_D, Instructions.HLV_WU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_I.exists(op.bitPat)) CSR.I else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HLV_D, Instructions.HSV_D, Instructions.HLV_WU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HLV_D, Instructions.HSV_D, Instructions.HLV_WU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HLV_D, Instructions.HSV_D, Instructions.HLV_WU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.HLV_D, Instructions.HSV_D, Instructions.HLV_WU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

class MDecode(pipelinedMul: Boolean, aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val M = if (pipelinedMul) Y else N
  val D = if (pipelinedMul) N else Y
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.MUL, Instructions.MULH, Instructions.MULHU, Instructions.MULHSU, Instructions.DIV, Instructions.DIVU, Instructions.REM, Instructions.REMU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MUL, Instructions.MULH, Instructions.MULHU, Instructions.MULHSU, Instructions.DIV, Instructions.DIVU, Instructions.REM, Instructions.REMU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MUL, Instructions.MULH, Instructions.MULHU, Instructions.MULHSU, Instructions.DIV, Instructions.DIVU, Instructions.REM, Instructions.REMU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MUL, Instructions.MULH, Instructions.MULHU, Instructions.MULHSU, Instructions.DIV, Instructions.DIVU, Instructions.REM, Instructions.REMU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MUL, Instructions.MULH, Instructions.MULHU, Instructions.MULHSU, Instructions.DIV, Instructions.DIVU, Instructions.REM, Instructions.REMU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MUL, Instructions.MULH, Instructions.MULHU, Instructions.MULHSU, Instructions.DIV, Instructions.DIVU, Instructions.REM, Instructions.REMU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.MUL, Instructions.MULH, Instructions.MULHU, Instructions.MULHSU, Instructions.DIV, Instructions.DIVU, Instructions.REM, Instructions.REMU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.MUL, Instructions.MULH, Instructions.MULHU, Instructions.MULHSU, Instructions.DIV, Instructions.DIVU, Instructions.REM, Instructions.REMU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MUL, Instructions.MULH, Instructions.MULHU, Instructions.MULHSU, Instructions.DIV, Instructions.DIVU, Instructions.REM, Instructions.REMU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MUL, Instructions.MULH, Instructions.MULHU, Instructions.MULHSU, Instructions.DIV, Instructions.DIVU, Instructions.REM, Instructions.REMU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MUL, Instructions.MULH, Instructions.MULHU, Instructions.MULHSU, Instructions.DIV, Instructions.DIVU, Instructions.REM, Instructions.REMU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MUL, Instructions.MULH, Instructions.MULHU, Instructions.MULHSU, Instructions.DIV, Instructions.DIVU, Instructions.REM, Instructions.REMU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_RS2: Seq[BitPat] = Seq(
    Instructions.MUL, Instructions.MULH, Instructions.MULHU, Instructions.MULHSU, Instructions.DIV, Instructions.DIVU, Instructions.REM, Instructions.REMU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_RS2.exists(op.bitPat)) A2_RS2 else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.MUL, Instructions.MULH, Instructions.MULHU, Instructions.MULHSU, Instructions.DIV, Instructions.DIVU, Instructions.REM, Instructions.REMU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.MUL, Instructions.MULH, Instructions.MULHU, Instructions.MULHSU, Instructions.DIV, Instructions.DIVU, Instructions.REM, Instructions.REMU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.MUL, Instructions.MULH, Instructions.MULHU, Instructions.MULHSU, Instructions.DIV, Instructions.DIVU, Instructions.REM, Instructions.REMU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_MUL: Seq[BitPat] = Seq(
    Instructions.MUL, 
  )
  val insns_aluFn_FN_MULH: Seq[BitPat] = Seq(
    Instructions.MULH, 
  )
  val insns_aluFn_FN_MULHU: Seq[BitPat] = Seq(
    Instructions.MULHU, 
  )
  val insns_aluFn_FN_MULHSU: Seq[BitPat] = Seq(
    Instructions.MULHSU, 
  )
  val insns_aluFn_FN_DIV: Seq[BitPat] = Seq(
    Instructions.DIV, 
  )
  val insns_aluFn_FN_DIVU: Seq[BitPat] = Seq(
    Instructions.DIVU, 
  )
  val insns_aluFn_FN_REM: Seq[BitPat] = Seq(
    Instructions.REM, 
  )
  val insns_aluFn_FN_REMU: Seq[BitPat] = Seq(
    Instructions.REMU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_MUL.exists(op.bitPat)) aluFn.FN_MUL else if (insns_aluFn_FN_MULH.exists(op.bitPat)) aluFn.FN_MULH else if (insns_aluFn_FN_MULHU.exists(op.bitPat)) aluFn.FN_MULHU else if (insns_aluFn_FN_MULHSU.exists(op.bitPat)) aluFn.FN_MULHSU else if (insns_aluFn_FN_DIV.exists(op.bitPat)) aluFn.FN_DIV else if (insns_aluFn_FN_DIVU.exists(op.bitPat)) aluFn.FN_DIVU else if (insns_aluFn_FN_REM.exists(op.bitPat)) aluFn.FN_REM else if (insns_aluFn_FN_REMU.exists(op.bitPat)) aluFn.FN_REMU else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MUL, Instructions.MULH, Instructions.MULHU, Instructions.MULHSU, Instructions.DIV, Instructions.DIVU, Instructions.REM, Instructions.REMU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.MUL, Instructions.MULH, Instructions.MULHU, Instructions.MULHSU, Instructions.DIV, Instructions.DIVU, Instructions.REM, Instructions.REMU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MUL, Instructions.MULH, Instructions.MULHU, Instructions.MULHSU, Instructions.DIV, Instructions.DIVU, Instructions.REM, Instructions.REMU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MUL, Instructions.MULH, Instructions.MULHU, Instructions.MULHSU, Instructions.DIV, Instructions.DIVU, Instructions.REM, Instructions.REMU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MUL, Instructions.MULH, Instructions.MULHU, Instructions.MULHSU, Instructions.DIV, Instructions.DIVU, Instructions.REM, Instructions.REMU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MUL, Instructions.MULH, Instructions.MULHU, Instructions.MULHSU, Instructions.DIV, Instructions.DIVU, Instructions.REM, Instructions.REMU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_M: Seq[BitPat] = Seq(
    Instructions.MUL, Instructions.MULH, Instructions.MULHU, Instructions.MULHSU, 
  )
  val insns_N: Seq[BitPat] = Seq(
    Instructions.DIV, Instructions.DIVU, Instructions.REM, Instructions.REMU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M.exists(op.bitPat)) M else if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_D: Seq[BitPat] = Seq(
    Instructions.MUL, Instructions.MULH, Instructions.MULHU, Instructions.MULHSU, 
  )
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.DIV, Instructions.DIVU, Instructions.REM, Instructions.REMU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_D.exists(op.bitPat)) D else if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.MUL, Instructions.MULH, Instructions.MULHU, Instructions.MULHSU, Instructions.DIV, Instructions.DIVU, Instructions.REM, Instructions.REMU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.MUL, Instructions.MULH, Instructions.MULHU, Instructions.MULHSU, Instructions.DIV, Instructions.DIVU, Instructions.REM, Instructions.REMU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MUL, Instructions.MULH, Instructions.MULHU, Instructions.MULHSU, Instructions.DIV, Instructions.DIVU, Instructions.REM, Instructions.REMU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MUL, Instructions.MULH, Instructions.MULHU, Instructions.MULHSU, Instructions.DIV, Instructions.DIVU, Instructions.REM, Instructions.REMU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MUL, Instructions.MULH, Instructions.MULHU, Instructions.MULHSU, Instructions.DIV, Instructions.DIVU, Instructions.REM, Instructions.REMU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MUL, Instructions.MULH, Instructions.MULHU, Instructions.MULHSU, Instructions.DIV, Instructions.DIVU, Instructions.REM, Instructions.REMU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

class M64Decode(pipelinedMul: Boolean, aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
  val M = if (pipelinedMul) Y else N
  val D = if (pipelinedMul) N else Y
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.MULW, Instructions.DIVW, Instructions.DIVUW, Instructions.REMW, Instructions.REMUW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MULW, Instructions.DIVW, Instructions.DIVUW, Instructions.REMW, Instructions.REMUW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MULW, Instructions.DIVW, Instructions.DIVUW, Instructions.REMW, Instructions.REMUW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MULW, Instructions.DIVW, Instructions.DIVUW, Instructions.REMW, Instructions.REMUW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MULW, Instructions.DIVW, Instructions.DIVUW, Instructions.REMW, Instructions.REMUW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MULW, Instructions.DIVW, Instructions.DIVUW, Instructions.REMW, Instructions.REMUW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.MULW, Instructions.DIVW, Instructions.DIVUW, Instructions.REMW, Instructions.REMUW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.MULW, Instructions.DIVW, Instructions.DIVUW, Instructions.REMW, Instructions.REMUW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MULW, Instructions.DIVW, Instructions.DIVUW, Instructions.REMW, Instructions.REMUW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MULW, Instructions.DIVW, Instructions.DIVUW, Instructions.REMW, Instructions.REMUW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MULW, Instructions.DIVW, Instructions.DIVUW, Instructions.REMW, Instructions.REMUW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MULW, Instructions.DIVW, Instructions.DIVUW, Instructions.REMW, Instructions.REMUW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_RS2: Seq[BitPat] = Seq(
    Instructions.MULW, Instructions.DIVW, Instructions.DIVUW, Instructions.REMW, Instructions.REMUW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_RS2.exists(op.bitPat)) A2_RS2 else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.MULW, Instructions.DIVW, Instructions.DIVUW, Instructions.REMW, Instructions.REMUW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.MULW, Instructions.DIVW, Instructions.DIVUW, Instructions.REMW, Instructions.REMUW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_32: Seq[BitPat] = Seq(
    Instructions.MULW, Instructions.DIVW, Instructions.DIVUW, Instructions.REMW, Instructions.REMUW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_32.exists(op.bitPat)) DW_32 else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_MUL: Seq[BitPat] = Seq(
    Instructions.MULW, 
  )
  val insns_aluFn_FN_DIV: Seq[BitPat] = Seq(
    Instructions.DIVW, 
  )
  val insns_aluFn_FN_DIVU: Seq[BitPat] = Seq(
    Instructions.DIVUW, 
  )
  val insns_aluFn_FN_REM: Seq[BitPat] = Seq(
    Instructions.REMW, 
  )
  val insns_aluFn_FN_REMU: Seq[BitPat] = Seq(
    Instructions.REMUW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_MUL.exists(op.bitPat)) aluFn.FN_MUL else if (insns_aluFn_FN_DIV.exists(op.bitPat)) aluFn.FN_DIV else if (insns_aluFn_FN_DIVU.exists(op.bitPat)) aluFn.FN_DIVU else if (insns_aluFn_FN_REM.exists(op.bitPat)) aluFn.FN_REM else if (insns_aluFn_FN_REMU.exists(op.bitPat)) aluFn.FN_REMU else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MULW, Instructions.DIVW, Instructions.DIVUW, Instructions.REMW, Instructions.REMUW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.MULW, Instructions.DIVW, Instructions.DIVUW, Instructions.REMW, Instructions.REMUW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MULW, Instructions.DIVW, Instructions.DIVUW, Instructions.REMW, Instructions.REMUW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MULW, Instructions.DIVW, Instructions.DIVUW, Instructions.REMW, Instructions.REMUW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MULW, Instructions.DIVW, Instructions.DIVUW, Instructions.REMW, Instructions.REMUW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MULW, Instructions.DIVW, Instructions.DIVUW, Instructions.REMW, Instructions.REMUW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_M: Seq[BitPat] = Seq(
    Instructions.MULW, 
  )
  val insns_N: Seq[BitPat] = Seq(
    Instructions.DIVW, Instructions.DIVUW, Instructions.REMW, Instructions.REMUW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M.exists(op.bitPat)) M else if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_D: Seq[BitPat] = Seq(
    Instructions.MULW, 
  )
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.DIVW, Instructions.DIVUW, Instructions.REMW, Instructions.REMUW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_D.exists(op.bitPat)) D else if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.MULW, Instructions.DIVW, Instructions.DIVUW, Instructions.REMW, Instructions.REMUW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.MULW, Instructions.DIVW, Instructions.DIVUW, Instructions.REMW, Instructions.REMUW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MULW, Instructions.DIVW, Instructions.DIVUW, Instructions.REMW, Instructions.REMUW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MULW, Instructions.DIVW, Instructions.DIVUW, Instructions.REMW, Instructions.REMUW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MULW, Instructions.DIVW, Instructions.DIVUW, Instructions.REMW, Instructions.REMUW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MULW, Instructions.DIVW, Instructions.DIVUW, Instructions.REMW, Instructions.REMUW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

class ADecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.AMOADD_W, Instructions.AMOXOR_W, Instructions.AMOSWAP_W, Instructions.AMOAND_W, Instructions.AMOOR_W, Instructions.AMOMIN_W, Instructions.AMOMINU_W, Instructions.AMOMAX_W, Instructions.AMOMAXU_W, Instructions.LR_W, Instructions.SC_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_W, Instructions.AMOXOR_W, Instructions.AMOSWAP_W, Instructions.AMOAND_W, Instructions.AMOOR_W, Instructions.AMOMIN_W, Instructions.AMOMINU_W, Instructions.AMOMAX_W, Instructions.AMOMAXU_W, Instructions.LR_W, Instructions.SC_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_W, Instructions.AMOXOR_W, Instructions.AMOSWAP_W, Instructions.AMOAND_W, Instructions.AMOOR_W, Instructions.AMOMIN_W, Instructions.AMOMINU_W, Instructions.AMOMAX_W, Instructions.AMOMAXU_W, Instructions.LR_W, Instructions.SC_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_W, Instructions.AMOXOR_W, Instructions.AMOSWAP_W, Instructions.AMOAND_W, Instructions.AMOOR_W, Instructions.AMOMIN_W, Instructions.AMOMINU_W, Instructions.AMOMAX_W, Instructions.AMOMAXU_W, Instructions.LR_W, Instructions.SC_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_W, Instructions.AMOXOR_W, Instructions.AMOSWAP_W, Instructions.AMOAND_W, Instructions.AMOOR_W, Instructions.AMOMIN_W, Instructions.AMOMINU_W, Instructions.AMOMAX_W, Instructions.AMOMAXU_W, Instructions.LR_W, Instructions.SC_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_W, Instructions.AMOXOR_W, Instructions.AMOSWAP_W, Instructions.AMOAND_W, Instructions.AMOOR_W, Instructions.AMOMIN_W, Instructions.AMOMINU_W, Instructions.AMOMAX_W, Instructions.AMOMAXU_W, Instructions.LR_W, Instructions.SC_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.AMOADD_W, Instructions.AMOXOR_W, Instructions.AMOSWAP_W, Instructions.AMOAND_W, Instructions.AMOOR_W, Instructions.AMOMIN_W, Instructions.AMOMINU_W, Instructions.AMOMAX_W, Instructions.AMOMAXU_W, Instructions.LR_W, Instructions.SC_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.AMOADD_W, Instructions.AMOXOR_W, Instructions.AMOSWAP_W, Instructions.AMOAND_W, Instructions.AMOOR_W, Instructions.AMOMIN_W, Instructions.AMOMINU_W, Instructions.AMOMAX_W, Instructions.AMOMAXU_W, Instructions.LR_W, Instructions.SC_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_W, Instructions.AMOXOR_W, Instructions.AMOSWAP_W, Instructions.AMOAND_W, Instructions.AMOOR_W, Instructions.AMOMIN_W, Instructions.AMOMINU_W, Instructions.AMOMAX_W, Instructions.AMOMAXU_W, Instructions.LR_W, Instructions.SC_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_W, Instructions.AMOXOR_W, Instructions.AMOSWAP_W, Instructions.AMOAND_W, Instructions.AMOOR_W, Instructions.AMOMIN_W, Instructions.AMOMINU_W, Instructions.AMOMAX_W, Instructions.AMOMAXU_W, Instructions.LR_W, Instructions.SC_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_W, Instructions.AMOXOR_W, Instructions.AMOSWAP_W, Instructions.AMOAND_W, Instructions.AMOOR_W, Instructions.AMOMIN_W, Instructions.AMOMINU_W, Instructions.AMOMAX_W, Instructions.AMOMAXU_W, Instructions.LR_W, Instructions.SC_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_W, Instructions.AMOXOR_W, Instructions.AMOSWAP_W, Instructions.AMOAND_W, Instructions.AMOOR_W, Instructions.AMOMIN_W, Instructions.AMOMINU_W, Instructions.AMOMAX_W, Instructions.AMOMAXU_W, Instructions.LR_W, Instructions.SC_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_ZERO: Seq[BitPat] = Seq(
    Instructions.AMOADD_W, Instructions.AMOXOR_W, Instructions.AMOSWAP_W, Instructions.AMOAND_W, Instructions.AMOOR_W, Instructions.AMOMIN_W, Instructions.AMOMINU_W, Instructions.AMOMAX_W, Instructions.AMOMAXU_W, Instructions.LR_W, Instructions.SC_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_ZERO.exists(op.bitPat)) A2_ZERO else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.AMOADD_W, Instructions.AMOXOR_W, Instructions.AMOSWAP_W, Instructions.AMOAND_W, Instructions.AMOOR_W, Instructions.AMOMIN_W, Instructions.AMOMINU_W, Instructions.AMOMAX_W, Instructions.AMOMAXU_W, Instructions.LR_W, Instructions.SC_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.AMOADD_W, Instructions.AMOXOR_W, Instructions.AMOSWAP_W, Instructions.AMOAND_W, Instructions.AMOOR_W, Instructions.AMOMIN_W, Instructions.AMOMINU_W, Instructions.AMOMAX_W, Instructions.AMOMAXU_W, Instructions.LR_W, Instructions.SC_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.AMOADD_W, Instructions.AMOXOR_W, Instructions.AMOSWAP_W, Instructions.AMOAND_W, Instructions.AMOOR_W, Instructions.AMOMIN_W, Instructions.AMOMINU_W, Instructions.AMOMAX_W, Instructions.AMOMAXU_W, Instructions.LR_W, Instructions.SC_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_ADD: Seq[BitPat] = Seq(
    Instructions.AMOADD_W, Instructions.AMOXOR_W, Instructions.AMOSWAP_W, Instructions.AMOAND_W, Instructions.AMOOR_W, Instructions.AMOMIN_W, Instructions.AMOMINU_W, Instructions.AMOMAX_W, Instructions.AMOMAXU_W, Instructions.LR_W, Instructions.SC_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_ADD.exists(op.bitPat)) aluFn.FN_ADD else FN_X
  }
}
object mem extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.AMOADD_W, Instructions.AMOXOR_W, Instructions.AMOSWAP_W, Instructions.AMOAND_W, Instructions.AMOOR_W, Instructions.AMOMIN_W, Instructions.AMOMINU_W, Instructions.AMOMAX_W, Instructions.AMOMAXU_W, Instructions.LR_W, Instructions.SC_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_XA_ADD: Seq[BitPat] = Seq(
    Instructions.AMOADD_W, 
  )
  val insns_M_XA_XOR: Seq[BitPat] = Seq(
    Instructions.AMOXOR_W, 
  )
  val insns_M_XA_SWAP: Seq[BitPat] = Seq(
    Instructions.AMOSWAP_W, 
  )
  val insns_M_XA_AND: Seq[BitPat] = Seq(
    Instructions.AMOAND_W, 
  )
  val insns_M_XA_OR: Seq[BitPat] = Seq(
    Instructions.AMOOR_W, 
  )
  val insns_M_XA_MIN: Seq[BitPat] = Seq(
    Instructions.AMOMIN_W, 
  )
  val insns_M_XA_MINU: Seq[BitPat] = Seq(
    Instructions.AMOMINU_W, 
  )
  val insns_M_XA_MAX: Seq[BitPat] = Seq(
    Instructions.AMOMAX_W, 
  )
  val insns_M_XA_MAXU: Seq[BitPat] = Seq(
    Instructions.AMOMAXU_W, 
  )
  val insns_M_XLR: Seq[BitPat] = Seq(
    Instructions.LR_W, 
  )
  val insns_M_XSC: Seq[BitPat] = Seq(
    Instructions.SC_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_XA_ADD.exists(op.bitPat)) M_XA_ADD else if (insns_M_XA_XOR.exists(op.bitPat)) M_XA_XOR else if (insns_M_XA_SWAP.exists(op.bitPat)) M_XA_SWAP else if (insns_M_XA_AND.exists(op.bitPat)) M_XA_AND else if (insns_M_XA_OR.exists(op.bitPat)) M_XA_OR else if (insns_M_XA_MIN.exists(op.bitPat)) M_XA_MIN else if (insns_M_XA_MINU.exists(op.bitPat)) M_XA_MINU else if (insns_M_XA_MAX.exists(op.bitPat)) M_XA_MAX else if (insns_M_XA_MAXU.exists(op.bitPat)) M_XA_MAXU else if (insns_M_XLR.exists(op.bitPat)) M_XLR else if (insns_M_XSC.exists(op.bitPat)) M_XSC else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_W, Instructions.AMOXOR_W, Instructions.AMOSWAP_W, Instructions.AMOAND_W, Instructions.AMOOR_W, Instructions.AMOMIN_W, Instructions.AMOMINU_W, Instructions.AMOMAX_W, Instructions.AMOMAXU_W, Instructions.LR_W, Instructions.SC_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_W, Instructions.AMOXOR_W, Instructions.AMOSWAP_W, Instructions.AMOAND_W, Instructions.AMOOR_W, Instructions.AMOMIN_W, Instructions.AMOMINU_W, Instructions.AMOMAX_W, Instructions.AMOMAXU_W, Instructions.LR_W, Instructions.SC_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_W, Instructions.AMOXOR_W, Instructions.AMOSWAP_W, Instructions.AMOAND_W, Instructions.AMOOR_W, Instructions.AMOMIN_W, Instructions.AMOMINU_W, Instructions.AMOMAX_W, Instructions.AMOMAXU_W, Instructions.LR_W, Instructions.SC_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_W, Instructions.AMOXOR_W, Instructions.AMOSWAP_W, Instructions.AMOAND_W, Instructions.AMOOR_W, Instructions.AMOMIN_W, Instructions.AMOMINU_W, Instructions.AMOMAX_W, Instructions.AMOMAXU_W, Instructions.LR_W, Instructions.SC_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_W, Instructions.AMOXOR_W, Instructions.AMOSWAP_W, Instructions.AMOAND_W, Instructions.AMOOR_W, Instructions.AMOMIN_W, Instructions.AMOMINU_W, Instructions.AMOMAX_W, Instructions.AMOMAXU_W, Instructions.LR_W, Instructions.SC_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_W, Instructions.AMOXOR_W, Instructions.AMOSWAP_W, Instructions.AMOAND_W, Instructions.AMOOR_W, Instructions.AMOMIN_W, Instructions.AMOMINU_W, Instructions.AMOMAX_W, Instructions.AMOMAXU_W, Instructions.LR_W, Instructions.SC_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.AMOADD_W, Instructions.AMOXOR_W, Instructions.AMOSWAP_W, Instructions.AMOAND_W, Instructions.AMOOR_W, Instructions.AMOMIN_W, Instructions.AMOMINU_W, Instructions.AMOMAX_W, Instructions.AMOMAXU_W, Instructions.LR_W, Instructions.SC_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_W, Instructions.AMOXOR_W, Instructions.AMOSWAP_W, Instructions.AMOAND_W, Instructions.AMOOR_W, Instructions.AMOMIN_W, Instructions.AMOMINU_W, Instructions.AMOMAX_W, Instructions.AMOMAXU_W, Instructions.LR_W, Instructions.SC_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_W, Instructions.AMOXOR_W, Instructions.AMOSWAP_W, Instructions.AMOAND_W, Instructions.AMOOR_W, Instructions.AMOMIN_W, Instructions.AMOMINU_W, Instructions.AMOMAX_W, Instructions.AMOMAXU_W, Instructions.LR_W, Instructions.SC_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_W, Instructions.AMOXOR_W, Instructions.AMOSWAP_W, Instructions.AMOAND_W, Instructions.AMOOR_W, Instructions.AMOMIN_W, Instructions.AMOMINU_W, Instructions.AMOMAX_W, Instructions.AMOMAXU_W, Instructions.LR_W, Instructions.SC_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.AMOADD_W, Instructions.AMOXOR_W, Instructions.AMOSWAP_W, Instructions.AMOAND_W, Instructions.AMOOR_W, Instructions.AMOMIN_W, Instructions.AMOMINU_W, Instructions.AMOMAX_W, Instructions.AMOMAXU_W, Instructions.LR_W, Instructions.SC_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_W, Instructions.AMOXOR_W, Instructions.AMOSWAP_W, Instructions.AMOAND_W, Instructions.AMOOR_W, Instructions.AMOMIN_W, Instructions.AMOMINU_W, Instructions.AMOMAX_W, Instructions.AMOMAXU_W, Instructions.LR_W, Instructions.SC_W, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

class A64Decode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.AMOADD_D, Instructions.AMOSWAP_D, Instructions.AMOXOR_D, Instructions.AMOAND_D, Instructions.AMOOR_D, Instructions.AMOMIN_D, Instructions.AMOMINU_D, Instructions.AMOMAX_D, Instructions.AMOMAXU_D, Instructions.LR_D, Instructions.SC_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_D, Instructions.AMOSWAP_D, Instructions.AMOXOR_D, Instructions.AMOAND_D, Instructions.AMOOR_D, Instructions.AMOMIN_D, Instructions.AMOMINU_D, Instructions.AMOMAX_D, Instructions.AMOMAXU_D, Instructions.LR_D, Instructions.SC_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_D, Instructions.AMOSWAP_D, Instructions.AMOXOR_D, Instructions.AMOAND_D, Instructions.AMOOR_D, Instructions.AMOMIN_D, Instructions.AMOMINU_D, Instructions.AMOMAX_D, Instructions.AMOMAXU_D, Instructions.LR_D, Instructions.SC_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_D, Instructions.AMOSWAP_D, Instructions.AMOXOR_D, Instructions.AMOAND_D, Instructions.AMOOR_D, Instructions.AMOMIN_D, Instructions.AMOMINU_D, Instructions.AMOMAX_D, Instructions.AMOMAXU_D, Instructions.LR_D, Instructions.SC_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_D, Instructions.AMOSWAP_D, Instructions.AMOXOR_D, Instructions.AMOAND_D, Instructions.AMOOR_D, Instructions.AMOMIN_D, Instructions.AMOMINU_D, Instructions.AMOMAX_D, Instructions.AMOMAXU_D, Instructions.LR_D, Instructions.SC_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_D, Instructions.AMOSWAP_D, Instructions.AMOXOR_D, Instructions.AMOAND_D, Instructions.AMOOR_D, Instructions.AMOMIN_D, Instructions.AMOMINU_D, Instructions.AMOMAX_D, Instructions.AMOMAXU_D, Instructions.LR_D, Instructions.SC_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.AMOADD_D, Instructions.AMOSWAP_D, Instructions.AMOXOR_D, Instructions.AMOAND_D, Instructions.AMOOR_D, Instructions.AMOMIN_D, Instructions.AMOMINU_D, Instructions.AMOMAX_D, Instructions.AMOMAXU_D, Instructions.LR_D, Instructions.SC_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.AMOADD_D, Instructions.AMOSWAP_D, Instructions.AMOXOR_D, Instructions.AMOAND_D, Instructions.AMOOR_D, Instructions.AMOMIN_D, Instructions.AMOMINU_D, Instructions.AMOMAX_D, Instructions.AMOMAXU_D, Instructions.LR_D, Instructions.SC_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_D, Instructions.AMOSWAP_D, Instructions.AMOXOR_D, Instructions.AMOAND_D, Instructions.AMOOR_D, Instructions.AMOMIN_D, Instructions.AMOMINU_D, Instructions.AMOMAX_D, Instructions.AMOMAXU_D, Instructions.LR_D, Instructions.SC_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_D, Instructions.AMOSWAP_D, Instructions.AMOXOR_D, Instructions.AMOAND_D, Instructions.AMOOR_D, Instructions.AMOMIN_D, Instructions.AMOMINU_D, Instructions.AMOMAX_D, Instructions.AMOMAXU_D, Instructions.LR_D, Instructions.SC_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_D, Instructions.AMOSWAP_D, Instructions.AMOXOR_D, Instructions.AMOAND_D, Instructions.AMOOR_D, Instructions.AMOMIN_D, Instructions.AMOMINU_D, Instructions.AMOMAX_D, Instructions.AMOMAXU_D, Instructions.LR_D, Instructions.SC_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_D, Instructions.AMOSWAP_D, Instructions.AMOXOR_D, Instructions.AMOAND_D, Instructions.AMOOR_D, Instructions.AMOMIN_D, Instructions.AMOMINU_D, Instructions.AMOMAX_D, Instructions.AMOMAXU_D, Instructions.LR_D, Instructions.SC_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_ZERO: Seq[BitPat] = Seq(
    Instructions.AMOADD_D, Instructions.AMOSWAP_D, Instructions.AMOXOR_D, Instructions.AMOAND_D, Instructions.AMOOR_D, Instructions.AMOMIN_D, Instructions.AMOMINU_D, Instructions.AMOMAX_D, Instructions.AMOMAXU_D, Instructions.LR_D, Instructions.SC_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_ZERO.exists(op.bitPat)) A2_ZERO else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.AMOADD_D, Instructions.AMOSWAP_D, Instructions.AMOXOR_D, Instructions.AMOAND_D, Instructions.AMOOR_D, Instructions.AMOMIN_D, Instructions.AMOMINU_D, Instructions.AMOMAX_D, Instructions.AMOMAXU_D, Instructions.LR_D, Instructions.SC_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.AMOADD_D, Instructions.AMOSWAP_D, Instructions.AMOXOR_D, Instructions.AMOAND_D, Instructions.AMOOR_D, Instructions.AMOMIN_D, Instructions.AMOMINU_D, Instructions.AMOMAX_D, Instructions.AMOMAXU_D, Instructions.LR_D, Instructions.SC_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.AMOADD_D, Instructions.AMOSWAP_D, Instructions.AMOXOR_D, Instructions.AMOAND_D, Instructions.AMOOR_D, Instructions.AMOMIN_D, Instructions.AMOMINU_D, Instructions.AMOMAX_D, Instructions.AMOMAXU_D, Instructions.LR_D, Instructions.SC_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_ADD: Seq[BitPat] = Seq(
    Instructions.AMOADD_D, Instructions.AMOSWAP_D, Instructions.AMOXOR_D, Instructions.AMOAND_D, Instructions.AMOOR_D, Instructions.AMOMIN_D, Instructions.AMOMINU_D, Instructions.AMOMAX_D, Instructions.AMOMAXU_D, Instructions.LR_D, Instructions.SC_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_ADD.exists(op.bitPat)) aluFn.FN_ADD else FN_X
  }
}
object mem extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.AMOADD_D, Instructions.AMOSWAP_D, Instructions.AMOXOR_D, Instructions.AMOAND_D, Instructions.AMOOR_D, Instructions.AMOMIN_D, Instructions.AMOMINU_D, Instructions.AMOMAX_D, Instructions.AMOMAXU_D, Instructions.LR_D, Instructions.SC_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_XA_ADD: Seq[BitPat] = Seq(
    Instructions.AMOADD_D, 
  )
  val insns_M_XA_SWAP: Seq[BitPat] = Seq(
    Instructions.AMOSWAP_D, 
  )
  val insns_M_XA_XOR: Seq[BitPat] = Seq(
    Instructions.AMOXOR_D, 
  )
  val insns_M_XA_AND: Seq[BitPat] = Seq(
    Instructions.AMOAND_D, 
  )
  val insns_M_XA_OR: Seq[BitPat] = Seq(
    Instructions.AMOOR_D, 
  )
  val insns_M_XA_MIN: Seq[BitPat] = Seq(
    Instructions.AMOMIN_D, 
  )
  val insns_M_XA_MINU: Seq[BitPat] = Seq(
    Instructions.AMOMINU_D, 
  )
  val insns_M_XA_MAX: Seq[BitPat] = Seq(
    Instructions.AMOMAX_D, 
  )
  val insns_M_XA_MAXU: Seq[BitPat] = Seq(
    Instructions.AMOMAXU_D, 
  )
  val insns_M_XLR: Seq[BitPat] = Seq(
    Instructions.LR_D, 
  )
  val insns_M_XSC: Seq[BitPat] = Seq(
    Instructions.SC_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_XA_ADD.exists(op.bitPat)) M_XA_ADD else if (insns_M_XA_SWAP.exists(op.bitPat)) M_XA_SWAP else if (insns_M_XA_XOR.exists(op.bitPat)) M_XA_XOR else if (insns_M_XA_AND.exists(op.bitPat)) M_XA_AND else if (insns_M_XA_OR.exists(op.bitPat)) M_XA_OR else if (insns_M_XA_MIN.exists(op.bitPat)) M_XA_MIN else if (insns_M_XA_MINU.exists(op.bitPat)) M_XA_MINU else if (insns_M_XA_MAX.exists(op.bitPat)) M_XA_MAX else if (insns_M_XA_MAXU.exists(op.bitPat)) M_XA_MAXU else if (insns_M_XLR.exists(op.bitPat)) M_XLR else if (insns_M_XSC.exists(op.bitPat)) M_XSC else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_D, Instructions.AMOSWAP_D, Instructions.AMOXOR_D, Instructions.AMOAND_D, Instructions.AMOOR_D, Instructions.AMOMIN_D, Instructions.AMOMINU_D, Instructions.AMOMAX_D, Instructions.AMOMAXU_D, Instructions.LR_D, Instructions.SC_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_D, Instructions.AMOSWAP_D, Instructions.AMOXOR_D, Instructions.AMOAND_D, Instructions.AMOOR_D, Instructions.AMOMIN_D, Instructions.AMOMINU_D, Instructions.AMOMAX_D, Instructions.AMOMAXU_D, Instructions.LR_D, Instructions.SC_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_D, Instructions.AMOSWAP_D, Instructions.AMOXOR_D, Instructions.AMOAND_D, Instructions.AMOOR_D, Instructions.AMOMIN_D, Instructions.AMOMINU_D, Instructions.AMOMAX_D, Instructions.AMOMAXU_D, Instructions.LR_D, Instructions.SC_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_D, Instructions.AMOSWAP_D, Instructions.AMOXOR_D, Instructions.AMOAND_D, Instructions.AMOOR_D, Instructions.AMOMIN_D, Instructions.AMOMINU_D, Instructions.AMOMAX_D, Instructions.AMOMAXU_D, Instructions.LR_D, Instructions.SC_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_D, Instructions.AMOSWAP_D, Instructions.AMOXOR_D, Instructions.AMOAND_D, Instructions.AMOOR_D, Instructions.AMOMIN_D, Instructions.AMOMINU_D, Instructions.AMOMAX_D, Instructions.AMOMAXU_D, Instructions.LR_D, Instructions.SC_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_D, Instructions.AMOSWAP_D, Instructions.AMOXOR_D, Instructions.AMOAND_D, Instructions.AMOOR_D, Instructions.AMOMIN_D, Instructions.AMOMINU_D, Instructions.AMOMAX_D, Instructions.AMOMAXU_D, Instructions.LR_D, Instructions.SC_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.AMOADD_D, Instructions.AMOSWAP_D, Instructions.AMOXOR_D, Instructions.AMOAND_D, Instructions.AMOOR_D, Instructions.AMOMIN_D, Instructions.AMOMINU_D, Instructions.AMOMAX_D, Instructions.AMOMAXU_D, Instructions.LR_D, Instructions.SC_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_D, Instructions.AMOSWAP_D, Instructions.AMOXOR_D, Instructions.AMOAND_D, Instructions.AMOOR_D, Instructions.AMOMIN_D, Instructions.AMOMINU_D, Instructions.AMOMAX_D, Instructions.AMOMAXU_D, Instructions.LR_D, Instructions.SC_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_D, Instructions.AMOSWAP_D, Instructions.AMOXOR_D, Instructions.AMOAND_D, Instructions.AMOOR_D, Instructions.AMOMIN_D, Instructions.AMOMINU_D, Instructions.AMOMAX_D, Instructions.AMOMAXU_D, Instructions.LR_D, Instructions.SC_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_D, Instructions.AMOSWAP_D, Instructions.AMOXOR_D, Instructions.AMOAND_D, Instructions.AMOOR_D, Instructions.AMOMIN_D, Instructions.AMOMINU_D, Instructions.AMOMAX_D, Instructions.AMOMAXU_D, Instructions.LR_D, Instructions.SC_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.AMOADD_D, Instructions.AMOSWAP_D, Instructions.AMOXOR_D, Instructions.AMOAND_D, Instructions.AMOOR_D, Instructions.AMOMIN_D, Instructions.AMOMINU_D, Instructions.AMOMAX_D, Instructions.AMOMAXU_D, Instructions.LR_D, Instructions.SC_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AMOADD_D, Instructions.AMOSWAP_D, Instructions.AMOXOR_D, Instructions.AMOAND_D, Instructions.AMOOR_D, Instructions.AMOMIN_D, Instructions.AMOMINU_D, Instructions.AMOMAX_D, Instructions.AMOMAXU_D, Instructions.LR_D, Instructions.SC_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

class HDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FCVT_S_H, Instructions.FCVT_H_S, Instructions.FSGNJ_H, Instructions.FSGNJX_H, Instructions.FSGNJN_H, Instructions.FMIN_H, Instructions.FMAX_H, Instructions.FADD_H, Instructions.FSUB_H, Instructions.FMUL_H, Instructions.FMADD_H, Instructions.FMSUB_H, Instructions.FNMADD_H, Instructions.FNMSUB_H, Instructions.FCLASS_H, Instructions.FMV_X_H, Instructions.FCVT_W_H, Instructions.FCVT_WU_H, Instructions.FEQ_H, Instructions.FLT_H, Instructions.FLE_H, Instructions.FMV_H_X, Instructions.FCVT_H_W, Instructions.FCVT_H_WU, Instructions.FLH, Instructions.FSH, Instructions.FDIV_H, Instructions.FSQRT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FCVT_S_H, Instructions.FCVT_H_S, Instructions.FSGNJ_H, Instructions.FSGNJX_H, Instructions.FSGNJN_H, Instructions.FMIN_H, Instructions.FMAX_H, Instructions.FADD_H, Instructions.FSUB_H, Instructions.FMUL_H, Instructions.FMADD_H, Instructions.FMSUB_H, Instructions.FNMADD_H, Instructions.FNMSUB_H, Instructions.FCLASS_H, Instructions.FMV_X_H, Instructions.FCVT_W_H, Instructions.FCVT_WU_H, Instructions.FEQ_H, Instructions.FLT_H, Instructions.FLE_H, Instructions.FMV_H_X, Instructions.FCVT_H_W, Instructions.FCVT_H_WU, Instructions.FLH, Instructions.FSH, Instructions.FDIV_H, Instructions.FSQRT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_H, Instructions.FCVT_H_S, Instructions.FSGNJ_H, Instructions.FSGNJX_H, Instructions.FSGNJN_H, Instructions.FMIN_H, Instructions.FMAX_H, Instructions.FADD_H, Instructions.FSUB_H, Instructions.FMUL_H, Instructions.FMADD_H, Instructions.FMSUB_H, Instructions.FNMADD_H, Instructions.FNMSUB_H, Instructions.FCLASS_H, Instructions.FMV_X_H, Instructions.FCVT_W_H, Instructions.FCVT_WU_H, Instructions.FEQ_H, Instructions.FLT_H, Instructions.FLE_H, Instructions.FMV_H_X, Instructions.FCVT_H_W, Instructions.FCVT_H_WU, Instructions.FLH, Instructions.FSH, Instructions.FDIV_H, Instructions.FSQRT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_H, Instructions.FCVT_H_S, Instructions.FSGNJ_H, Instructions.FSGNJX_H, Instructions.FSGNJN_H, Instructions.FMIN_H, Instructions.FMAX_H, Instructions.FADD_H, Instructions.FSUB_H, Instructions.FMUL_H, Instructions.FMADD_H, Instructions.FMSUB_H, Instructions.FNMADD_H, Instructions.FNMSUB_H, Instructions.FCLASS_H, Instructions.FMV_X_H, Instructions.FCVT_W_H, Instructions.FCVT_WU_H, Instructions.FEQ_H, Instructions.FLT_H, Instructions.FLE_H, Instructions.FMV_H_X, Instructions.FCVT_H_W, Instructions.FCVT_H_WU, Instructions.FLH, Instructions.FSH, Instructions.FDIV_H, Instructions.FSQRT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_H, Instructions.FCVT_H_S, Instructions.FSGNJ_H, Instructions.FSGNJX_H, Instructions.FSGNJN_H, Instructions.FMIN_H, Instructions.FMAX_H, Instructions.FADD_H, Instructions.FSUB_H, Instructions.FMUL_H, Instructions.FMADD_H, Instructions.FMSUB_H, Instructions.FNMADD_H, Instructions.FNMSUB_H, Instructions.FCLASS_H, Instructions.FMV_X_H, Instructions.FCVT_W_H, Instructions.FCVT_WU_H, Instructions.FEQ_H, Instructions.FLT_H, Instructions.FLE_H, Instructions.FMV_H_X, Instructions.FCVT_H_W, Instructions.FCVT_H_WU, Instructions.FLH, Instructions.FSH, Instructions.FDIV_H, Instructions.FSQRT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_H, Instructions.FCVT_H_S, Instructions.FSGNJ_H, Instructions.FSGNJX_H, Instructions.FSGNJN_H, Instructions.FMIN_H, Instructions.FMAX_H, Instructions.FADD_H, Instructions.FSUB_H, Instructions.FMUL_H, Instructions.FMADD_H, Instructions.FMSUB_H, Instructions.FNMADD_H, Instructions.FNMSUB_H, Instructions.FCLASS_H, Instructions.FMV_X_H, Instructions.FCVT_W_H, Instructions.FCVT_WU_H, Instructions.FEQ_H, Instructions.FLT_H, Instructions.FLE_H, Instructions.FMV_H_X, Instructions.FCVT_H_W, Instructions.FCVT_H_WU, Instructions.FLH, Instructions.FSH, Instructions.FDIV_H, Instructions.FSQRT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_H, Instructions.FCVT_H_S, Instructions.FSGNJ_H, Instructions.FSGNJX_H, Instructions.FSGNJN_H, Instructions.FMIN_H, Instructions.FMAX_H, Instructions.FADD_H, Instructions.FSUB_H, Instructions.FMUL_H, Instructions.FMADD_H, Instructions.FMSUB_H, Instructions.FNMADD_H, Instructions.FNMSUB_H, Instructions.FCLASS_H, Instructions.FMV_X_H, Instructions.FCVT_W_H, Instructions.FCVT_WU_H, Instructions.FEQ_H, Instructions.FLT_H, Instructions.FLE_H, Instructions.FMV_H_X, Instructions.FCVT_H_W, Instructions.FCVT_H_WU, Instructions.FLH, Instructions.FSH, Instructions.FDIV_H, Instructions.FSQRT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_H, Instructions.FCVT_H_S, Instructions.FSGNJ_H, Instructions.FSGNJX_H, Instructions.FSGNJN_H, Instructions.FMIN_H, Instructions.FMAX_H, Instructions.FADD_H, Instructions.FSUB_H, Instructions.FMUL_H, Instructions.FMADD_H, Instructions.FMSUB_H, Instructions.FNMADD_H, Instructions.FNMSUB_H, Instructions.FCLASS_H, Instructions.FMV_X_H, Instructions.FCVT_W_H, Instructions.FCVT_WU_H, Instructions.FEQ_H, Instructions.FLT_H, Instructions.FLE_H, Instructions.FDIV_H, Instructions.FSQRT_H, 
  )
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FMV_H_X, Instructions.FCVT_H_W, Instructions.FCVT_H_WU, Instructions.FLH, Instructions.FSH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_H, Instructions.FCVT_H_S, Instructions.FSGNJ_H, Instructions.FSGNJX_H, Instructions.FSGNJN_H, Instructions.FMIN_H, Instructions.FMAX_H, Instructions.FADD_H, Instructions.FSUB_H, Instructions.FMUL_H, Instructions.FMADD_H, Instructions.FMSUB_H, Instructions.FNMADD_H, Instructions.FNMSUB_H, Instructions.FCLASS_H, Instructions.FMV_X_H, Instructions.FCVT_W_H, Instructions.FCVT_WU_H, Instructions.FEQ_H, Instructions.FLT_H, Instructions.FLE_H, Instructions.FMV_H_X, Instructions.FCVT_H_W, Instructions.FCVT_H_WU, Instructions.FLH, Instructions.FSH, Instructions.FDIV_H, Instructions.FSQRT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_H, Instructions.FCVT_H_S, Instructions.FSGNJ_H, Instructions.FSGNJX_H, Instructions.FSGNJN_H, Instructions.FMIN_H, Instructions.FMAX_H, Instructions.FADD_H, Instructions.FSUB_H, Instructions.FMUL_H, Instructions.FMADD_H, Instructions.FMSUB_H, Instructions.FNMADD_H, Instructions.FNMSUB_H, Instructions.FCLASS_H, Instructions.FMV_X_H, Instructions.FCVT_W_H, Instructions.FCVT_WU_H, Instructions.FEQ_H, Instructions.FLT_H, Instructions.FLE_H, Instructions.FMV_H_X, Instructions.FCVT_H_W, Instructions.FCVT_H_WU, Instructions.FLH, Instructions.FSH, Instructions.FDIV_H, Instructions.FSQRT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_H, Instructions.FCVT_H_S, Instructions.FSGNJ_H, Instructions.FSGNJX_H, Instructions.FSGNJN_H, Instructions.FMIN_H, Instructions.FMAX_H, Instructions.FADD_H, Instructions.FSUB_H, Instructions.FMUL_H, Instructions.FMADD_H, Instructions.FMSUB_H, Instructions.FNMADD_H, Instructions.FNMSUB_H, Instructions.FCLASS_H, Instructions.FMV_X_H, Instructions.FCVT_W_H, Instructions.FCVT_WU_H, Instructions.FEQ_H, Instructions.FLT_H, Instructions.FLE_H, Instructions.FMV_H_X, Instructions.FCVT_H_W, Instructions.FCVT_H_WU, Instructions.FLH, Instructions.FSH, Instructions.FDIV_H, Instructions.FSQRT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_H, Instructions.FCVT_H_S, Instructions.FSGNJ_H, Instructions.FSGNJX_H, Instructions.FSGNJN_H, Instructions.FMIN_H, Instructions.FMAX_H, Instructions.FADD_H, Instructions.FSUB_H, Instructions.FMUL_H, Instructions.FMADD_H, Instructions.FMSUB_H, Instructions.FNMADD_H, Instructions.FNMSUB_H, Instructions.FCLASS_H, Instructions.FMV_X_H, Instructions.FCVT_W_H, Instructions.FCVT_WU_H, Instructions.FEQ_H, Instructions.FLT_H, Instructions.FLE_H, Instructions.FMV_H_X, Instructions.FCVT_H_W, Instructions.FCVT_H_WU, Instructions.FLH, Instructions.FSH, Instructions.FDIV_H, Instructions.FSQRT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_X: Seq[BitPat] = Seq(
    Instructions.FCVT_S_H, Instructions.FCVT_H_S, Instructions.FSGNJ_H, Instructions.FSGNJX_H, Instructions.FSGNJN_H, Instructions.FMIN_H, Instructions.FMAX_H, Instructions.FADD_H, Instructions.FSUB_H, Instructions.FMUL_H, Instructions.FMADD_H, Instructions.FMSUB_H, Instructions.FNMADD_H, Instructions.FNMSUB_H, Instructions.FCLASS_H, Instructions.FMV_X_H, Instructions.FCVT_W_H, Instructions.FCVT_WU_H, Instructions.FEQ_H, Instructions.FLT_H, Instructions.FLE_H, Instructions.FMV_H_X, Instructions.FCVT_H_W, Instructions.FCVT_H_WU, Instructions.FDIV_H, Instructions.FSQRT_H, 
  )
  val insns_A2_IMM: Seq[BitPat] = Seq(
    Instructions.FLH, Instructions.FSH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_X.exists(op.bitPat)) A2_X else if (insns_A2_IMM.exists(op.bitPat)) A2_IMM else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_X: Seq[BitPat] = Seq(
    Instructions.FCVT_S_H, Instructions.FCVT_H_S, Instructions.FSGNJ_H, Instructions.FSGNJX_H, Instructions.FSGNJN_H, Instructions.FMIN_H, Instructions.FMAX_H, Instructions.FADD_H, Instructions.FSUB_H, Instructions.FMUL_H, Instructions.FMADD_H, Instructions.FMSUB_H, Instructions.FNMADD_H, Instructions.FNMSUB_H, Instructions.FCLASS_H, Instructions.FMV_X_H, Instructions.FCVT_W_H, Instructions.FCVT_WU_H, Instructions.FEQ_H, Instructions.FLT_H, Instructions.FLE_H, Instructions.FDIV_H, Instructions.FSQRT_H, 
  )
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.FMV_H_X, Instructions.FCVT_H_W, Instructions.FCVT_H_WU, Instructions.FLH, Instructions.FSH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_X.exists(op.bitPat)) A1_X else if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.FCVT_S_H, Instructions.FCVT_H_S, Instructions.FSGNJ_H, Instructions.FSGNJX_H, Instructions.FSGNJN_H, Instructions.FMIN_H, Instructions.FMAX_H, Instructions.FADD_H, Instructions.FSUB_H, Instructions.FMUL_H, Instructions.FMADD_H, Instructions.FMSUB_H, Instructions.FNMADD_H, Instructions.FNMSUB_H, Instructions.FCLASS_H, Instructions.FMV_X_H, Instructions.FCVT_W_H, Instructions.FCVT_WU_H, Instructions.FEQ_H, Instructions.FLT_H, Instructions.FLE_H, Instructions.FMV_H_X, Instructions.FCVT_H_W, Instructions.FCVT_H_WU, Instructions.FDIV_H, Instructions.FSQRT_H, 
  )
  val insns_IMM_I: Seq[BitPat] = Seq(
    Instructions.FLH, 
  )
  val insns_IMM_S: Seq[BitPat] = Seq(
    Instructions.FSH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else if (insns_IMM_I.exists(op.bitPat)) IMM_I else if (insns_IMM_S.exists(op.bitPat)) IMM_S else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_X: Seq[BitPat] = Seq(
    Instructions.FCVT_S_H, Instructions.FCVT_H_S, Instructions.FSGNJ_H, Instructions.FSGNJX_H, Instructions.FSGNJN_H, Instructions.FMIN_H, Instructions.FMAX_H, Instructions.FADD_H, Instructions.FSUB_H, Instructions.FMUL_H, Instructions.FMADD_H, Instructions.FMSUB_H, Instructions.FNMADD_H, Instructions.FNMSUB_H, Instructions.FCLASS_H, Instructions.FMV_X_H, Instructions.FCVT_W_H, Instructions.FCVT_WU_H, Instructions.FEQ_H, Instructions.FLT_H, Instructions.FLE_H, Instructions.FMV_H_X, Instructions.FCVT_H_W, Instructions.FCVT_H_WU, Instructions.FDIV_H, Instructions.FSQRT_H, 
  )
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.FLH, Instructions.FSH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_X.exists(op.bitPat)) DW_X else if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_X: Seq[BitPat] = Seq(
    Instructions.FCVT_S_H, Instructions.FCVT_H_S, Instructions.FSGNJ_H, Instructions.FSGNJX_H, Instructions.FSGNJN_H, Instructions.FMIN_H, Instructions.FMAX_H, Instructions.FADD_H, Instructions.FSUB_H, Instructions.FMUL_H, Instructions.FMADD_H, Instructions.FMSUB_H, Instructions.FNMADD_H, Instructions.FNMSUB_H, Instructions.FCLASS_H, Instructions.FMV_X_H, Instructions.FCVT_W_H, Instructions.FCVT_WU_H, Instructions.FEQ_H, Instructions.FLT_H, Instructions.FLE_H, Instructions.FMV_H_X, Instructions.FCVT_H_W, Instructions.FCVT_H_WU, Instructions.FDIV_H, Instructions.FSQRT_H, 
  )
  val insns_aluFn_FN_ADD: Seq[BitPat] = Seq(
    Instructions.FLH, Instructions.FSH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_X.exists(op.bitPat)) aluFn.FN_X else if (insns_aluFn_FN_ADD.exists(op.bitPat)) aluFn.FN_ADD else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_H, Instructions.FCVT_H_S, Instructions.FSGNJ_H, Instructions.FSGNJX_H, Instructions.FSGNJN_H, Instructions.FMIN_H, Instructions.FMAX_H, Instructions.FADD_H, Instructions.FSUB_H, Instructions.FMUL_H, Instructions.FMADD_H, Instructions.FMSUB_H, Instructions.FNMADD_H, Instructions.FNMSUB_H, Instructions.FCLASS_H, Instructions.FMV_X_H, Instructions.FCVT_W_H, Instructions.FCVT_WU_H, Instructions.FEQ_H, Instructions.FLT_H, Instructions.FLE_H, Instructions.FMV_H_X, Instructions.FCVT_H_W, Instructions.FCVT_H_WU, Instructions.FDIV_H, Instructions.FSQRT_H, 
  )
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FLH, Instructions.FSH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.FCVT_S_H, Instructions.FCVT_H_S, Instructions.FSGNJ_H, Instructions.FSGNJX_H, Instructions.FSGNJN_H, Instructions.FMIN_H, Instructions.FMAX_H, Instructions.FADD_H, Instructions.FSUB_H, Instructions.FMUL_H, Instructions.FMADD_H, Instructions.FMSUB_H, Instructions.FNMADD_H, Instructions.FNMSUB_H, Instructions.FCLASS_H, Instructions.FMV_X_H, Instructions.FCVT_W_H, Instructions.FCVT_WU_H, Instructions.FEQ_H, Instructions.FLT_H, Instructions.FLE_H, Instructions.FMV_H_X, Instructions.FCVT_H_W, Instructions.FCVT_H_WU, Instructions.FDIV_H, Instructions.FSQRT_H, 
  )
  val insns_M_XRD: Seq[BitPat] = Seq(
    Instructions.FLH, 
  )
  val insns_M_XWR: Seq[BitPat] = Seq(
    Instructions.FSH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else if (insns_M_XRD.exists(op.bitPat)) M_XRD else if (insns_M_XWR.exists(op.bitPat)) M_XWR else M_X
  }
}
object rfs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FCVT_S_H, Instructions.FCVT_H_S, Instructions.FSGNJ_H, Instructions.FSGNJX_H, Instructions.FSGNJN_H, Instructions.FMIN_H, Instructions.FMAX_H, Instructions.FADD_H, Instructions.FSUB_H, Instructions.FMUL_H, Instructions.FMADD_H, Instructions.FMSUB_H, Instructions.FNMADD_H, Instructions.FNMSUB_H, Instructions.FCLASS_H, Instructions.FMV_X_H, Instructions.FCVT_W_H, Instructions.FCVT_WU_H, Instructions.FEQ_H, Instructions.FLT_H, Instructions.FLE_H, Instructions.FDIV_H, Instructions.FSQRT_H, 
  )
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FMV_H_X, Instructions.FCVT_H_W, Instructions.FCVT_H_WU, Instructions.FLH, Instructions.FSH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_H, Instructions.FCVT_H_S, Instructions.FCLASS_H, Instructions.FMV_X_H, Instructions.FCVT_W_H, Instructions.FCVT_WU_H, Instructions.FMV_H_X, Instructions.FCVT_H_W, Instructions.FCVT_H_WU, Instructions.FLH, 
  )
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FSGNJ_H, Instructions.FSGNJX_H, Instructions.FSGNJN_H, Instructions.FMIN_H, Instructions.FMAX_H, Instructions.FADD_H, Instructions.FSUB_H, Instructions.FMUL_H, Instructions.FMADD_H, Instructions.FMSUB_H, Instructions.FNMADD_H, Instructions.FNMSUB_H, Instructions.FEQ_H, Instructions.FLT_H, Instructions.FLE_H, Instructions.FSH, Instructions.FDIV_H, Instructions.FSQRT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_H, Instructions.FCVT_H_S, Instructions.FSGNJ_H, Instructions.FSGNJX_H, Instructions.FSGNJN_H, Instructions.FMIN_H, Instructions.FMAX_H, Instructions.FADD_H, Instructions.FSUB_H, Instructions.FMUL_H, Instructions.FCLASS_H, Instructions.FMV_X_H, Instructions.FCVT_W_H, Instructions.FCVT_WU_H, Instructions.FEQ_H, Instructions.FLT_H, Instructions.FLE_H, Instructions.FMV_H_X, Instructions.FCVT_H_W, Instructions.FCVT_H_WU, Instructions.FLH, Instructions.FSH, Instructions.FDIV_H, Instructions.FSQRT_H, 
  )
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FMADD_H, Instructions.FMSUB_H, Instructions.FNMADD_H, Instructions.FNMSUB_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object wfd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FCVT_S_H, Instructions.FCVT_H_S, Instructions.FSGNJ_H, Instructions.FSGNJX_H, Instructions.FSGNJN_H, Instructions.FMIN_H, Instructions.FMAX_H, Instructions.FADD_H, Instructions.FSUB_H, Instructions.FMUL_H, Instructions.FMADD_H, Instructions.FMSUB_H, Instructions.FNMADD_H, Instructions.FNMSUB_H, Instructions.FMV_H_X, Instructions.FCVT_H_W, Instructions.FCVT_H_WU, Instructions.FLH, Instructions.FDIV_H, Instructions.FSQRT_H, 
  )
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCLASS_H, Instructions.FMV_X_H, Instructions.FCVT_W_H, Instructions.FCVT_WU_H, Instructions.FEQ_H, Instructions.FLT_H, Instructions.FLE_H, Instructions.FSH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_H, Instructions.FCVT_H_S, Instructions.FSGNJ_H, Instructions.FSGNJX_H, Instructions.FSGNJN_H, Instructions.FMIN_H, Instructions.FMAX_H, Instructions.FADD_H, Instructions.FSUB_H, Instructions.FMUL_H, Instructions.FMADD_H, Instructions.FMSUB_H, Instructions.FNMADD_H, Instructions.FNMSUB_H, Instructions.FCLASS_H, Instructions.FMV_X_H, Instructions.FCVT_W_H, Instructions.FCVT_WU_H, Instructions.FEQ_H, Instructions.FLT_H, Instructions.FLE_H, Instructions.FMV_H_X, Instructions.FCVT_H_W, Instructions.FCVT_H_WU, Instructions.FLH, Instructions.FSH, Instructions.FDIV_H, Instructions.FSQRT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_H, Instructions.FCVT_H_S, Instructions.FSGNJ_H, Instructions.FSGNJX_H, Instructions.FSGNJN_H, Instructions.FMIN_H, Instructions.FMAX_H, Instructions.FADD_H, Instructions.FSUB_H, Instructions.FMUL_H, Instructions.FMADD_H, Instructions.FMSUB_H, Instructions.FNMADD_H, Instructions.FNMSUB_H, Instructions.FCLASS_H, Instructions.FMV_X_H, Instructions.FCVT_W_H, Instructions.FCVT_WU_H, Instructions.FEQ_H, Instructions.FLT_H, Instructions.FLE_H, Instructions.FMV_H_X, Instructions.FCVT_H_W, Instructions.FCVT_H_WU, Instructions.FLH, Instructions.FSH, Instructions.FDIV_H, Instructions.FSQRT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_H, Instructions.FCVT_H_S, Instructions.FSGNJ_H, Instructions.FSGNJX_H, Instructions.FSGNJN_H, Instructions.FMIN_H, Instructions.FMAX_H, Instructions.FADD_H, Instructions.FSUB_H, Instructions.FMUL_H, Instructions.FMADD_H, Instructions.FMSUB_H, Instructions.FNMADD_H, Instructions.FNMSUB_H, Instructions.FMV_H_X, Instructions.FCVT_H_W, Instructions.FCVT_H_WU, Instructions.FLH, Instructions.FSH, Instructions.FDIV_H, Instructions.FSQRT_H, 
  )
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FCLASS_H, Instructions.FMV_X_H, Instructions.FCVT_W_H, Instructions.FCVT_WU_H, Instructions.FEQ_H, Instructions.FLT_H, Instructions.FLE_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_H, Instructions.FCVT_H_S, Instructions.FSGNJ_H, Instructions.FSGNJX_H, Instructions.FSGNJN_H, Instructions.FMIN_H, Instructions.FMAX_H, Instructions.FADD_H, Instructions.FSUB_H, Instructions.FMUL_H, Instructions.FMADD_H, Instructions.FMSUB_H, Instructions.FNMADD_H, Instructions.FNMSUB_H, Instructions.FCLASS_H, Instructions.FMV_X_H, Instructions.FCVT_W_H, Instructions.FCVT_WU_H, Instructions.FEQ_H, Instructions.FLT_H, Instructions.FLE_H, Instructions.FMV_H_X, Instructions.FCVT_H_W, Instructions.FCVT_H_WU, Instructions.FLH, Instructions.FSH, Instructions.FDIV_H, Instructions.FSQRT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_H, Instructions.FCVT_H_S, Instructions.FSGNJ_H, Instructions.FSGNJX_H, Instructions.FSGNJN_H, Instructions.FMIN_H, Instructions.FMAX_H, Instructions.FADD_H, Instructions.FSUB_H, Instructions.FMUL_H, Instructions.FMADD_H, Instructions.FMSUB_H, Instructions.FNMADD_H, Instructions.FNMSUB_H, Instructions.FCLASS_H, Instructions.FMV_X_H, Instructions.FCVT_W_H, Instructions.FCVT_WU_H, Instructions.FEQ_H, Instructions.FLT_H, Instructions.FLE_H, Instructions.FMV_H_X, Instructions.FCVT_H_W, Instructions.FCVT_H_WU, Instructions.FLH, Instructions.FSH, Instructions.FDIV_H, Instructions.FSQRT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_H, Instructions.FCVT_H_S, Instructions.FSGNJ_H, Instructions.FSGNJX_H, Instructions.FSGNJN_H, Instructions.FMIN_H, Instructions.FMAX_H, Instructions.FADD_H, Instructions.FSUB_H, Instructions.FMUL_H, Instructions.FMADD_H, Instructions.FMSUB_H, Instructions.FNMADD_H, Instructions.FNMSUB_H, Instructions.FCLASS_H, Instructions.FMV_X_H, Instructions.FCVT_W_H, Instructions.FCVT_WU_H, Instructions.FEQ_H, Instructions.FLT_H, Instructions.FLE_H, Instructions.FMV_H_X, Instructions.FCVT_H_W, Instructions.FCVT_H_WU, Instructions.FLH, Instructions.FSH, Instructions.FDIV_H, Instructions.FSQRT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_H, Instructions.FCVT_H_S, Instructions.FSGNJ_H, Instructions.FSGNJX_H, Instructions.FSGNJN_H, Instructions.FMIN_H, Instructions.FMAX_H, Instructions.FADD_H, Instructions.FSUB_H, Instructions.FMUL_H, Instructions.FMADD_H, Instructions.FMSUB_H, Instructions.FNMADD_H, Instructions.FNMSUB_H, Instructions.FCLASS_H, Instructions.FMV_X_H, Instructions.FCVT_W_H, Instructions.FCVT_WU_H, Instructions.FEQ_H, Instructions.FLT_H, Instructions.FLE_H, Instructions.FMV_H_X, Instructions.FCVT_H_W, Instructions.FCVT_H_WU, Instructions.FLH, Instructions.FSH, Instructions.FDIV_H, Instructions.FSQRT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_H, Instructions.FCVT_H_S, Instructions.FSGNJ_H, Instructions.FSGNJX_H, Instructions.FSGNJN_H, Instructions.FMIN_H, Instructions.FMAX_H, Instructions.FADD_H, Instructions.FSUB_H, Instructions.FMUL_H, Instructions.FMADD_H, Instructions.FMSUB_H, Instructions.FNMADD_H, Instructions.FNMSUB_H, Instructions.FCLASS_H, Instructions.FMV_X_H, Instructions.FCVT_W_H, Instructions.FCVT_WU_H, Instructions.FEQ_H, Instructions.FLT_H, Instructions.FLE_H, Instructions.FMV_H_X, Instructions.FCVT_H_W, Instructions.FCVT_H_WU, Instructions.FLH, Instructions.FSH, Instructions.FDIV_H, Instructions.FSQRT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

class FDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FSGNJ_S, Instructions.FSGNJX_S, Instructions.FSGNJN_S, Instructions.FMIN_S, Instructions.FMAX_S, Instructions.FADD_S, Instructions.FSUB_S, Instructions.FMUL_S, Instructions.FMADD_S, Instructions.FMSUB_S, Instructions.FNMADD_S, Instructions.FNMSUB_S, Instructions.FCLASS_S, Instructions.FMV_X_W, Instructions.FCVT_W_S, Instructions.FCVT_WU_S, Instructions.FEQ_S, Instructions.FLT_S, Instructions.FLE_S, Instructions.FMV_W_X, Instructions.FCVT_S_W, Instructions.FCVT_S_WU, Instructions.FLW, Instructions.FSW, Instructions.FDIV_S, Instructions.FSQRT_S, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FSGNJ_S, Instructions.FSGNJX_S, Instructions.FSGNJN_S, Instructions.FMIN_S, Instructions.FMAX_S, Instructions.FADD_S, Instructions.FSUB_S, Instructions.FMUL_S, Instructions.FMADD_S, Instructions.FMSUB_S, Instructions.FNMADD_S, Instructions.FNMSUB_S, Instructions.FCLASS_S, Instructions.FMV_X_W, Instructions.FCVT_W_S, Instructions.FCVT_WU_S, Instructions.FEQ_S, Instructions.FLT_S, Instructions.FLE_S, Instructions.FMV_W_X, Instructions.FCVT_S_W, Instructions.FCVT_S_WU, Instructions.FLW, Instructions.FSW, Instructions.FDIV_S, Instructions.FSQRT_S, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FSGNJ_S, Instructions.FSGNJX_S, Instructions.FSGNJN_S, Instructions.FMIN_S, Instructions.FMAX_S, Instructions.FADD_S, Instructions.FSUB_S, Instructions.FMUL_S, Instructions.FMADD_S, Instructions.FMSUB_S, Instructions.FNMADD_S, Instructions.FNMSUB_S, Instructions.FCLASS_S, Instructions.FMV_X_W, Instructions.FCVT_W_S, Instructions.FCVT_WU_S, Instructions.FEQ_S, Instructions.FLT_S, Instructions.FLE_S, Instructions.FMV_W_X, Instructions.FCVT_S_W, Instructions.FCVT_S_WU, Instructions.FLW, Instructions.FSW, Instructions.FDIV_S, Instructions.FSQRT_S, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FSGNJ_S, Instructions.FSGNJX_S, Instructions.FSGNJN_S, Instructions.FMIN_S, Instructions.FMAX_S, Instructions.FADD_S, Instructions.FSUB_S, Instructions.FMUL_S, Instructions.FMADD_S, Instructions.FMSUB_S, Instructions.FNMADD_S, Instructions.FNMSUB_S, Instructions.FCLASS_S, Instructions.FMV_X_W, Instructions.FCVT_W_S, Instructions.FCVT_WU_S, Instructions.FEQ_S, Instructions.FLT_S, Instructions.FLE_S, Instructions.FMV_W_X, Instructions.FCVT_S_W, Instructions.FCVT_S_WU, Instructions.FLW, Instructions.FSW, Instructions.FDIV_S, Instructions.FSQRT_S, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FSGNJ_S, Instructions.FSGNJX_S, Instructions.FSGNJN_S, Instructions.FMIN_S, Instructions.FMAX_S, Instructions.FADD_S, Instructions.FSUB_S, Instructions.FMUL_S, Instructions.FMADD_S, Instructions.FMSUB_S, Instructions.FNMADD_S, Instructions.FNMSUB_S, Instructions.FCLASS_S, Instructions.FMV_X_W, Instructions.FCVT_W_S, Instructions.FCVT_WU_S, Instructions.FEQ_S, Instructions.FLT_S, Instructions.FLE_S, Instructions.FMV_W_X, Instructions.FCVT_S_W, Instructions.FCVT_S_WU, Instructions.FLW, Instructions.FSW, Instructions.FDIV_S, Instructions.FSQRT_S, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FSGNJ_S, Instructions.FSGNJX_S, Instructions.FSGNJN_S, Instructions.FMIN_S, Instructions.FMAX_S, Instructions.FADD_S, Instructions.FSUB_S, Instructions.FMUL_S, Instructions.FMADD_S, Instructions.FMSUB_S, Instructions.FNMADD_S, Instructions.FNMSUB_S, Instructions.FCLASS_S, Instructions.FMV_X_W, Instructions.FCVT_W_S, Instructions.FCVT_WU_S, Instructions.FEQ_S, Instructions.FLT_S, Instructions.FLE_S, Instructions.FMV_W_X, Instructions.FCVT_S_W, Instructions.FCVT_S_WU, Instructions.FLW, Instructions.FSW, Instructions.FDIV_S, Instructions.FSQRT_S, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FSGNJ_S, Instructions.FSGNJX_S, Instructions.FSGNJN_S, Instructions.FMIN_S, Instructions.FMAX_S, Instructions.FADD_S, Instructions.FSUB_S, Instructions.FMUL_S, Instructions.FMADD_S, Instructions.FMSUB_S, Instructions.FNMADD_S, Instructions.FNMSUB_S, Instructions.FCLASS_S, Instructions.FMV_X_W, Instructions.FCVT_W_S, Instructions.FCVT_WU_S, Instructions.FEQ_S, Instructions.FLT_S, Instructions.FLE_S, Instructions.FMV_W_X, Instructions.FCVT_S_W, Instructions.FCVT_S_WU, Instructions.FLW, Instructions.FSW, Instructions.FDIV_S, Instructions.FSQRT_S, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FSGNJ_S, Instructions.FSGNJX_S, Instructions.FSGNJN_S, Instructions.FMIN_S, Instructions.FMAX_S, Instructions.FADD_S, Instructions.FSUB_S, Instructions.FMUL_S, Instructions.FMADD_S, Instructions.FMSUB_S, Instructions.FNMADD_S, Instructions.FNMSUB_S, Instructions.FCLASS_S, Instructions.FMV_X_W, Instructions.FCVT_W_S, Instructions.FCVT_WU_S, Instructions.FEQ_S, Instructions.FLT_S, Instructions.FLE_S, Instructions.FDIV_S, Instructions.FSQRT_S, 
  )
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FMV_W_X, Instructions.FCVT_S_W, Instructions.FCVT_S_WU, Instructions.FLW, Instructions.FSW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FSGNJ_S, Instructions.FSGNJX_S, Instructions.FSGNJN_S, Instructions.FMIN_S, Instructions.FMAX_S, Instructions.FADD_S, Instructions.FSUB_S, Instructions.FMUL_S, Instructions.FMADD_S, Instructions.FMSUB_S, Instructions.FNMADD_S, Instructions.FNMSUB_S, Instructions.FCLASS_S, Instructions.FMV_X_W, Instructions.FCVT_W_S, Instructions.FCVT_WU_S, Instructions.FEQ_S, Instructions.FLT_S, Instructions.FLE_S, Instructions.FMV_W_X, Instructions.FCVT_S_W, Instructions.FCVT_S_WU, Instructions.FLW, Instructions.FSW, Instructions.FDIV_S, Instructions.FSQRT_S, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FSGNJ_S, Instructions.FSGNJX_S, Instructions.FSGNJN_S, Instructions.FMIN_S, Instructions.FMAX_S, Instructions.FADD_S, Instructions.FSUB_S, Instructions.FMUL_S, Instructions.FMADD_S, Instructions.FMSUB_S, Instructions.FNMADD_S, Instructions.FNMSUB_S, Instructions.FCLASS_S, Instructions.FMV_X_W, Instructions.FCVT_W_S, Instructions.FCVT_WU_S, Instructions.FEQ_S, Instructions.FLT_S, Instructions.FLE_S, Instructions.FMV_W_X, Instructions.FCVT_S_W, Instructions.FCVT_S_WU, Instructions.FLW, Instructions.FSW, Instructions.FDIV_S, Instructions.FSQRT_S, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FSGNJ_S, Instructions.FSGNJX_S, Instructions.FSGNJN_S, Instructions.FMIN_S, Instructions.FMAX_S, Instructions.FADD_S, Instructions.FSUB_S, Instructions.FMUL_S, Instructions.FMADD_S, Instructions.FMSUB_S, Instructions.FNMADD_S, Instructions.FNMSUB_S, Instructions.FCLASS_S, Instructions.FMV_X_W, Instructions.FCVT_W_S, Instructions.FCVT_WU_S, Instructions.FEQ_S, Instructions.FLT_S, Instructions.FLE_S, Instructions.FMV_W_X, Instructions.FCVT_S_W, Instructions.FCVT_S_WU, Instructions.FLW, Instructions.FSW, Instructions.FDIV_S, Instructions.FSQRT_S, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FSGNJ_S, Instructions.FSGNJX_S, Instructions.FSGNJN_S, Instructions.FMIN_S, Instructions.FMAX_S, Instructions.FADD_S, Instructions.FSUB_S, Instructions.FMUL_S, Instructions.FMADD_S, Instructions.FMSUB_S, Instructions.FNMADD_S, Instructions.FNMSUB_S, Instructions.FCLASS_S, Instructions.FMV_X_W, Instructions.FCVT_W_S, Instructions.FCVT_WU_S, Instructions.FEQ_S, Instructions.FLT_S, Instructions.FLE_S, Instructions.FMV_W_X, Instructions.FCVT_S_W, Instructions.FCVT_S_WU, Instructions.FLW, Instructions.FSW, Instructions.FDIV_S, Instructions.FSQRT_S, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_X: Seq[BitPat] = Seq(
    Instructions.FSGNJ_S, Instructions.FSGNJX_S, Instructions.FSGNJN_S, Instructions.FMIN_S, Instructions.FMAX_S, Instructions.FADD_S, Instructions.FSUB_S, Instructions.FMUL_S, Instructions.FMADD_S, Instructions.FMSUB_S, Instructions.FNMADD_S, Instructions.FNMSUB_S, Instructions.FCLASS_S, Instructions.FMV_X_W, Instructions.FCVT_W_S, Instructions.FCVT_WU_S, Instructions.FEQ_S, Instructions.FLT_S, Instructions.FLE_S, Instructions.FMV_W_X, Instructions.FCVT_S_W, Instructions.FCVT_S_WU, Instructions.FDIV_S, Instructions.FSQRT_S, 
  )
  val insns_A2_IMM: Seq[BitPat] = Seq(
    Instructions.FLW, Instructions.FSW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_X.exists(op.bitPat)) A2_X else if (insns_A2_IMM.exists(op.bitPat)) A2_IMM else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_X: Seq[BitPat] = Seq(
    Instructions.FSGNJ_S, Instructions.FSGNJX_S, Instructions.FSGNJN_S, Instructions.FMIN_S, Instructions.FMAX_S, Instructions.FADD_S, Instructions.FSUB_S, Instructions.FMUL_S, Instructions.FMADD_S, Instructions.FMSUB_S, Instructions.FNMADD_S, Instructions.FNMSUB_S, Instructions.FCLASS_S, Instructions.FMV_X_W, Instructions.FCVT_W_S, Instructions.FCVT_WU_S, Instructions.FEQ_S, Instructions.FLT_S, Instructions.FLE_S, Instructions.FDIV_S, Instructions.FSQRT_S, 
  )
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.FMV_W_X, Instructions.FCVT_S_W, Instructions.FCVT_S_WU, Instructions.FLW, Instructions.FSW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_X.exists(op.bitPat)) A1_X else if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.FSGNJ_S, Instructions.FSGNJX_S, Instructions.FSGNJN_S, Instructions.FMIN_S, Instructions.FMAX_S, Instructions.FADD_S, Instructions.FSUB_S, Instructions.FMUL_S, Instructions.FMADD_S, Instructions.FMSUB_S, Instructions.FNMADD_S, Instructions.FNMSUB_S, Instructions.FCLASS_S, Instructions.FMV_X_W, Instructions.FCVT_W_S, Instructions.FCVT_WU_S, Instructions.FEQ_S, Instructions.FLT_S, Instructions.FLE_S, Instructions.FMV_W_X, Instructions.FCVT_S_W, Instructions.FCVT_S_WU, Instructions.FDIV_S, Instructions.FSQRT_S, 
  )
  val insns_IMM_I: Seq[BitPat] = Seq(
    Instructions.FLW, 
  )
  val insns_IMM_S: Seq[BitPat] = Seq(
    Instructions.FSW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else if (insns_IMM_I.exists(op.bitPat)) IMM_I else if (insns_IMM_S.exists(op.bitPat)) IMM_S else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_X: Seq[BitPat] = Seq(
    Instructions.FSGNJ_S, Instructions.FSGNJX_S, Instructions.FSGNJN_S, Instructions.FMIN_S, Instructions.FMAX_S, Instructions.FADD_S, Instructions.FSUB_S, Instructions.FMUL_S, Instructions.FMADD_S, Instructions.FMSUB_S, Instructions.FNMADD_S, Instructions.FNMSUB_S, Instructions.FCLASS_S, Instructions.FMV_X_W, Instructions.FCVT_W_S, Instructions.FCVT_WU_S, Instructions.FEQ_S, Instructions.FLT_S, Instructions.FLE_S, Instructions.FMV_W_X, Instructions.FCVT_S_W, Instructions.FCVT_S_WU, Instructions.FDIV_S, Instructions.FSQRT_S, 
  )
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.FLW, Instructions.FSW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_X.exists(op.bitPat)) DW_X else if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_X: Seq[BitPat] = Seq(
    Instructions.FSGNJ_S, Instructions.FSGNJX_S, Instructions.FSGNJN_S, Instructions.FMIN_S, Instructions.FMAX_S, Instructions.FADD_S, Instructions.FSUB_S, Instructions.FMUL_S, Instructions.FMADD_S, Instructions.FMSUB_S, Instructions.FNMADD_S, Instructions.FNMSUB_S, Instructions.FCLASS_S, Instructions.FMV_X_W, Instructions.FCVT_W_S, Instructions.FCVT_WU_S, Instructions.FEQ_S, Instructions.FLT_S, Instructions.FLE_S, Instructions.FMV_W_X, Instructions.FCVT_S_W, Instructions.FCVT_S_WU, Instructions.FDIV_S, Instructions.FSQRT_S, 
  )
  val insns_aluFn_FN_ADD: Seq[BitPat] = Seq(
    Instructions.FLW, Instructions.FSW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_X.exists(op.bitPat)) aluFn.FN_X else if (insns_aluFn_FN_ADD.exists(op.bitPat)) aluFn.FN_ADD else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FSGNJ_S, Instructions.FSGNJX_S, Instructions.FSGNJN_S, Instructions.FMIN_S, Instructions.FMAX_S, Instructions.FADD_S, Instructions.FSUB_S, Instructions.FMUL_S, Instructions.FMADD_S, Instructions.FMSUB_S, Instructions.FNMADD_S, Instructions.FNMSUB_S, Instructions.FCLASS_S, Instructions.FMV_X_W, Instructions.FCVT_W_S, Instructions.FCVT_WU_S, Instructions.FEQ_S, Instructions.FLT_S, Instructions.FLE_S, Instructions.FMV_W_X, Instructions.FCVT_S_W, Instructions.FCVT_S_WU, Instructions.FDIV_S, Instructions.FSQRT_S, 
  )
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FLW, Instructions.FSW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.FSGNJ_S, Instructions.FSGNJX_S, Instructions.FSGNJN_S, Instructions.FMIN_S, Instructions.FMAX_S, Instructions.FADD_S, Instructions.FSUB_S, Instructions.FMUL_S, Instructions.FMADD_S, Instructions.FMSUB_S, Instructions.FNMADD_S, Instructions.FNMSUB_S, Instructions.FCLASS_S, Instructions.FMV_X_W, Instructions.FCVT_W_S, Instructions.FCVT_WU_S, Instructions.FEQ_S, Instructions.FLT_S, Instructions.FLE_S, Instructions.FMV_W_X, Instructions.FCVT_S_W, Instructions.FCVT_S_WU, Instructions.FDIV_S, Instructions.FSQRT_S, 
  )
  val insns_M_XRD: Seq[BitPat] = Seq(
    Instructions.FLW, 
  )
  val insns_M_XWR: Seq[BitPat] = Seq(
    Instructions.FSW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else if (insns_M_XRD.exists(op.bitPat)) M_XRD else if (insns_M_XWR.exists(op.bitPat)) M_XWR else M_X
  }
}
object rfs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FSGNJ_S, Instructions.FSGNJX_S, Instructions.FSGNJN_S, Instructions.FMIN_S, Instructions.FMAX_S, Instructions.FADD_S, Instructions.FSUB_S, Instructions.FMUL_S, Instructions.FMADD_S, Instructions.FMSUB_S, Instructions.FNMADD_S, Instructions.FNMSUB_S, Instructions.FCLASS_S, Instructions.FMV_X_W, Instructions.FCVT_W_S, Instructions.FCVT_WU_S, Instructions.FEQ_S, Instructions.FLT_S, Instructions.FLE_S, Instructions.FDIV_S, Instructions.FSQRT_S, 
  )
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FMV_W_X, Instructions.FCVT_S_W, Instructions.FCVT_S_WU, Instructions.FLW, Instructions.FSW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FSGNJ_S, Instructions.FSGNJX_S, Instructions.FSGNJN_S, Instructions.FMIN_S, Instructions.FMAX_S, Instructions.FADD_S, Instructions.FSUB_S, Instructions.FMUL_S, Instructions.FMADD_S, Instructions.FMSUB_S, Instructions.FNMADD_S, Instructions.FNMSUB_S, Instructions.FEQ_S, Instructions.FLT_S, Instructions.FLE_S, Instructions.FSW, Instructions.FDIV_S, Instructions.FSQRT_S, 
  )
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCLASS_S, Instructions.FMV_X_W, Instructions.FCVT_W_S, Instructions.FCVT_WU_S, Instructions.FMV_W_X, Instructions.FCVT_S_W, Instructions.FCVT_S_WU, Instructions.FLW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FSGNJ_S, Instructions.FSGNJX_S, Instructions.FSGNJN_S, Instructions.FMIN_S, Instructions.FMAX_S, Instructions.FADD_S, Instructions.FSUB_S, Instructions.FMUL_S, Instructions.FCLASS_S, Instructions.FMV_X_W, Instructions.FCVT_W_S, Instructions.FCVT_WU_S, Instructions.FEQ_S, Instructions.FLT_S, Instructions.FLE_S, Instructions.FMV_W_X, Instructions.FCVT_S_W, Instructions.FCVT_S_WU, Instructions.FLW, Instructions.FSW, Instructions.FDIV_S, Instructions.FSQRT_S, 
  )
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FMADD_S, Instructions.FMSUB_S, Instructions.FNMADD_S, Instructions.FNMSUB_S, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object wfd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FSGNJ_S, Instructions.FSGNJX_S, Instructions.FSGNJN_S, Instructions.FMIN_S, Instructions.FMAX_S, Instructions.FADD_S, Instructions.FSUB_S, Instructions.FMUL_S, Instructions.FMADD_S, Instructions.FMSUB_S, Instructions.FNMADD_S, Instructions.FNMSUB_S, Instructions.FMV_W_X, Instructions.FCVT_S_W, Instructions.FCVT_S_WU, Instructions.FLW, Instructions.FDIV_S, Instructions.FSQRT_S, 
  )
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCLASS_S, Instructions.FMV_X_W, Instructions.FCVT_W_S, Instructions.FCVT_WU_S, Instructions.FEQ_S, Instructions.FLT_S, Instructions.FLE_S, Instructions.FSW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FSGNJ_S, Instructions.FSGNJX_S, Instructions.FSGNJN_S, Instructions.FMIN_S, Instructions.FMAX_S, Instructions.FADD_S, Instructions.FSUB_S, Instructions.FMUL_S, Instructions.FMADD_S, Instructions.FMSUB_S, Instructions.FNMADD_S, Instructions.FNMSUB_S, Instructions.FCLASS_S, Instructions.FMV_X_W, Instructions.FCVT_W_S, Instructions.FCVT_WU_S, Instructions.FEQ_S, Instructions.FLT_S, Instructions.FLE_S, Instructions.FMV_W_X, Instructions.FCVT_S_W, Instructions.FCVT_S_WU, Instructions.FLW, Instructions.FSW, Instructions.FDIV_S, Instructions.FSQRT_S, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FSGNJ_S, Instructions.FSGNJX_S, Instructions.FSGNJN_S, Instructions.FMIN_S, Instructions.FMAX_S, Instructions.FADD_S, Instructions.FSUB_S, Instructions.FMUL_S, Instructions.FMADD_S, Instructions.FMSUB_S, Instructions.FNMADD_S, Instructions.FNMSUB_S, Instructions.FCLASS_S, Instructions.FMV_X_W, Instructions.FCVT_W_S, Instructions.FCVT_WU_S, Instructions.FEQ_S, Instructions.FLT_S, Instructions.FLE_S, Instructions.FMV_W_X, Instructions.FCVT_S_W, Instructions.FCVT_S_WU, Instructions.FLW, Instructions.FSW, Instructions.FDIV_S, Instructions.FSQRT_S, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FSGNJ_S, Instructions.FSGNJX_S, Instructions.FSGNJN_S, Instructions.FMIN_S, Instructions.FMAX_S, Instructions.FADD_S, Instructions.FSUB_S, Instructions.FMUL_S, Instructions.FMADD_S, Instructions.FMSUB_S, Instructions.FNMADD_S, Instructions.FNMSUB_S, Instructions.FMV_W_X, Instructions.FCVT_S_W, Instructions.FCVT_S_WU, Instructions.FLW, Instructions.FSW, Instructions.FDIV_S, Instructions.FSQRT_S, 
  )
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FCLASS_S, Instructions.FMV_X_W, Instructions.FCVT_W_S, Instructions.FCVT_WU_S, Instructions.FEQ_S, Instructions.FLT_S, Instructions.FLE_S, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.FSGNJ_S, Instructions.FSGNJX_S, Instructions.FSGNJN_S, Instructions.FMIN_S, Instructions.FMAX_S, Instructions.FADD_S, Instructions.FSUB_S, Instructions.FMUL_S, Instructions.FMADD_S, Instructions.FMSUB_S, Instructions.FNMADD_S, Instructions.FNMSUB_S, Instructions.FCLASS_S, Instructions.FMV_X_W, Instructions.FCVT_W_S, Instructions.FCVT_WU_S, Instructions.FEQ_S, Instructions.FLT_S, Instructions.FLE_S, Instructions.FMV_W_X, Instructions.FCVT_S_W, Instructions.FCVT_S_WU, Instructions.FLW, Instructions.FSW, Instructions.FDIV_S, Instructions.FSQRT_S, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FSGNJ_S, Instructions.FSGNJX_S, Instructions.FSGNJN_S, Instructions.FMIN_S, Instructions.FMAX_S, Instructions.FADD_S, Instructions.FSUB_S, Instructions.FMUL_S, Instructions.FMADD_S, Instructions.FMSUB_S, Instructions.FNMADD_S, Instructions.FNMSUB_S, Instructions.FCLASS_S, Instructions.FMV_X_W, Instructions.FCVT_W_S, Instructions.FCVT_WU_S, Instructions.FEQ_S, Instructions.FLT_S, Instructions.FLE_S, Instructions.FMV_W_X, Instructions.FCVT_S_W, Instructions.FCVT_S_WU, Instructions.FLW, Instructions.FSW, Instructions.FDIV_S, Instructions.FSQRT_S, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FSGNJ_S, Instructions.FSGNJX_S, Instructions.FSGNJN_S, Instructions.FMIN_S, Instructions.FMAX_S, Instructions.FADD_S, Instructions.FSUB_S, Instructions.FMUL_S, Instructions.FMADD_S, Instructions.FMSUB_S, Instructions.FNMADD_S, Instructions.FNMSUB_S, Instructions.FCLASS_S, Instructions.FMV_X_W, Instructions.FCVT_W_S, Instructions.FCVT_WU_S, Instructions.FEQ_S, Instructions.FLT_S, Instructions.FLE_S, Instructions.FMV_W_X, Instructions.FCVT_S_W, Instructions.FCVT_S_WU, Instructions.FLW, Instructions.FSW, Instructions.FDIV_S, Instructions.FSQRT_S, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FSGNJ_S, Instructions.FSGNJX_S, Instructions.FSGNJN_S, Instructions.FMIN_S, Instructions.FMAX_S, Instructions.FADD_S, Instructions.FSUB_S, Instructions.FMUL_S, Instructions.FMADD_S, Instructions.FMSUB_S, Instructions.FNMADD_S, Instructions.FNMSUB_S, Instructions.FCLASS_S, Instructions.FMV_X_W, Instructions.FCVT_W_S, Instructions.FCVT_WU_S, Instructions.FEQ_S, Instructions.FLT_S, Instructions.FLE_S, Instructions.FMV_W_X, Instructions.FCVT_S_W, Instructions.FCVT_S_WU, Instructions.FLW, Instructions.FSW, Instructions.FDIV_S, Instructions.FSQRT_S, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FSGNJ_S, Instructions.FSGNJX_S, Instructions.FSGNJN_S, Instructions.FMIN_S, Instructions.FMAX_S, Instructions.FADD_S, Instructions.FSUB_S, Instructions.FMUL_S, Instructions.FMADD_S, Instructions.FMSUB_S, Instructions.FNMADD_S, Instructions.FNMSUB_S, Instructions.FCLASS_S, Instructions.FMV_X_W, Instructions.FCVT_W_S, Instructions.FCVT_WU_S, Instructions.FEQ_S, Instructions.FLT_S, Instructions.FLE_S, Instructions.FMV_W_X, Instructions.FCVT_S_W, Instructions.FCVT_S_WU, Instructions.FLW, Instructions.FSW, Instructions.FDIV_S, Instructions.FSQRT_S, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

class DDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FCVT_S_D, Instructions.FCVT_D_S, Instructions.FSGNJ_D, Instructions.FSGNJX_D, Instructions.FSGNJN_D, Instructions.FMIN_D, Instructions.FMAX_D, Instructions.FADD_D, Instructions.FSUB_D, Instructions.FMUL_D, Instructions.FMADD_D, Instructions.FMSUB_D, Instructions.FNMADD_D, Instructions.FNMSUB_D, Instructions.FCLASS_D, Instructions.FCVT_W_D, Instructions.FCVT_WU_D, Instructions.FEQ_D, Instructions.FLT_D, Instructions.FLE_D, Instructions.FCVT_D_W, Instructions.FCVT_D_WU, Instructions.FLD, Instructions.FSD, Instructions.FDIV_D, Instructions.FSQRT_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FCVT_S_D, Instructions.FCVT_D_S, Instructions.FSGNJ_D, Instructions.FSGNJX_D, Instructions.FSGNJN_D, Instructions.FMIN_D, Instructions.FMAX_D, Instructions.FADD_D, Instructions.FSUB_D, Instructions.FMUL_D, Instructions.FMADD_D, Instructions.FMSUB_D, Instructions.FNMADD_D, Instructions.FNMSUB_D, Instructions.FCLASS_D, Instructions.FCVT_W_D, Instructions.FCVT_WU_D, Instructions.FEQ_D, Instructions.FLT_D, Instructions.FLE_D, Instructions.FCVT_D_W, Instructions.FCVT_D_WU, Instructions.FLD, Instructions.FSD, Instructions.FDIV_D, Instructions.FSQRT_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_D, Instructions.FCVT_D_S, Instructions.FSGNJ_D, Instructions.FSGNJX_D, Instructions.FSGNJN_D, Instructions.FMIN_D, Instructions.FMAX_D, Instructions.FADD_D, Instructions.FSUB_D, Instructions.FMUL_D, Instructions.FMADD_D, Instructions.FMSUB_D, Instructions.FNMADD_D, Instructions.FNMSUB_D, Instructions.FCLASS_D, Instructions.FCVT_W_D, Instructions.FCVT_WU_D, Instructions.FEQ_D, Instructions.FLT_D, Instructions.FLE_D, Instructions.FCVT_D_W, Instructions.FCVT_D_WU, Instructions.FLD, Instructions.FSD, Instructions.FDIV_D, Instructions.FSQRT_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_D, Instructions.FCVT_D_S, Instructions.FSGNJ_D, Instructions.FSGNJX_D, Instructions.FSGNJN_D, Instructions.FMIN_D, Instructions.FMAX_D, Instructions.FADD_D, Instructions.FSUB_D, Instructions.FMUL_D, Instructions.FMADD_D, Instructions.FMSUB_D, Instructions.FNMADD_D, Instructions.FNMSUB_D, Instructions.FCLASS_D, Instructions.FCVT_W_D, Instructions.FCVT_WU_D, Instructions.FEQ_D, Instructions.FLT_D, Instructions.FLE_D, Instructions.FCVT_D_W, Instructions.FCVT_D_WU, Instructions.FLD, Instructions.FSD, Instructions.FDIV_D, Instructions.FSQRT_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_D, Instructions.FCVT_D_S, Instructions.FSGNJ_D, Instructions.FSGNJX_D, Instructions.FSGNJN_D, Instructions.FMIN_D, Instructions.FMAX_D, Instructions.FADD_D, Instructions.FSUB_D, Instructions.FMUL_D, Instructions.FMADD_D, Instructions.FMSUB_D, Instructions.FNMADD_D, Instructions.FNMSUB_D, Instructions.FCLASS_D, Instructions.FCVT_W_D, Instructions.FCVT_WU_D, Instructions.FEQ_D, Instructions.FLT_D, Instructions.FLE_D, Instructions.FCVT_D_W, Instructions.FCVT_D_WU, Instructions.FLD, Instructions.FSD, Instructions.FDIV_D, Instructions.FSQRT_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_D, Instructions.FCVT_D_S, Instructions.FSGNJ_D, Instructions.FSGNJX_D, Instructions.FSGNJN_D, Instructions.FMIN_D, Instructions.FMAX_D, Instructions.FADD_D, Instructions.FSUB_D, Instructions.FMUL_D, Instructions.FMADD_D, Instructions.FMSUB_D, Instructions.FNMADD_D, Instructions.FNMSUB_D, Instructions.FCLASS_D, Instructions.FCVT_W_D, Instructions.FCVT_WU_D, Instructions.FEQ_D, Instructions.FLT_D, Instructions.FLE_D, Instructions.FCVT_D_W, Instructions.FCVT_D_WU, Instructions.FLD, Instructions.FSD, Instructions.FDIV_D, Instructions.FSQRT_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_D, Instructions.FCVT_D_S, Instructions.FSGNJ_D, Instructions.FSGNJX_D, Instructions.FSGNJN_D, Instructions.FMIN_D, Instructions.FMAX_D, Instructions.FADD_D, Instructions.FSUB_D, Instructions.FMUL_D, Instructions.FMADD_D, Instructions.FMSUB_D, Instructions.FNMADD_D, Instructions.FNMSUB_D, Instructions.FCLASS_D, Instructions.FCVT_W_D, Instructions.FCVT_WU_D, Instructions.FEQ_D, Instructions.FLT_D, Instructions.FLE_D, Instructions.FCVT_D_W, Instructions.FCVT_D_WU, Instructions.FLD, Instructions.FSD, Instructions.FDIV_D, Instructions.FSQRT_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_D, Instructions.FCVT_D_S, Instructions.FSGNJ_D, Instructions.FSGNJX_D, Instructions.FSGNJN_D, Instructions.FMIN_D, Instructions.FMAX_D, Instructions.FADD_D, Instructions.FSUB_D, Instructions.FMUL_D, Instructions.FMADD_D, Instructions.FMSUB_D, Instructions.FNMADD_D, Instructions.FNMSUB_D, Instructions.FCLASS_D, Instructions.FCVT_W_D, Instructions.FCVT_WU_D, Instructions.FEQ_D, Instructions.FLT_D, Instructions.FLE_D, Instructions.FDIV_D, Instructions.FSQRT_D, 
  )
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FCVT_D_W, Instructions.FCVT_D_WU, Instructions.FLD, Instructions.FSD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_D, Instructions.FCVT_D_S, Instructions.FSGNJ_D, Instructions.FSGNJX_D, Instructions.FSGNJN_D, Instructions.FMIN_D, Instructions.FMAX_D, Instructions.FADD_D, Instructions.FSUB_D, Instructions.FMUL_D, Instructions.FMADD_D, Instructions.FMSUB_D, Instructions.FNMADD_D, Instructions.FNMSUB_D, Instructions.FCLASS_D, Instructions.FCVT_W_D, Instructions.FCVT_WU_D, Instructions.FEQ_D, Instructions.FLT_D, Instructions.FLE_D, Instructions.FCVT_D_W, Instructions.FCVT_D_WU, Instructions.FLD, Instructions.FSD, Instructions.FDIV_D, Instructions.FSQRT_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_D, Instructions.FCVT_D_S, Instructions.FSGNJ_D, Instructions.FSGNJX_D, Instructions.FSGNJN_D, Instructions.FMIN_D, Instructions.FMAX_D, Instructions.FADD_D, Instructions.FSUB_D, Instructions.FMUL_D, Instructions.FMADD_D, Instructions.FMSUB_D, Instructions.FNMADD_D, Instructions.FNMSUB_D, Instructions.FCLASS_D, Instructions.FCVT_W_D, Instructions.FCVT_WU_D, Instructions.FEQ_D, Instructions.FLT_D, Instructions.FLE_D, Instructions.FCVT_D_W, Instructions.FCVT_D_WU, Instructions.FLD, Instructions.FSD, Instructions.FDIV_D, Instructions.FSQRT_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_D, Instructions.FCVT_D_S, Instructions.FSGNJ_D, Instructions.FSGNJX_D, Instructions.FSGNJN_D, Instructions.FMIN_D, Instructions.FMAX_D, Instructions.FADD_D, Instructions.FSUB_D, Instructions.FMUL_D, Instructions.FMADD_D, Instructions.FMSUB_D, Instructions.FNMADD_D, Instructions.FNMSUB_D, Instructions.FCLASS_D, Instructions.FCVT_W_D, Instructions.FCVT_WU_D, Instructions.FEQ_D, Instructions.FLT_D, Instructions.FLE_D, Instructions.FCVT_D_W, Instructions.FCVT_D_WU, Instructions.FLD, Instructions.FSD, Instructions.FDIV_D, Instructions.FSQRT_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_D, Instructions.FCVT_D_S, Instructions.FSGNJ_D, Instructions.FSGNJX_D, Instructions.FSGNJN_D, Instructions.FMIN_D, Instructions.FMAX_D, Instructions.FADD_D, Instructions.FSUB_D, Instructions.FMUL_D, Instructions.FMADD_D, Instructions.FMSUB_D, Instructions.FNMADD_D, Instructions.FNMSUB_D, Instructions.FCLASS_D, Instructions.FCVT_W_D, Instructions.FCVT_WU_D, Instructions.FEQ_D, Instructions.FLT_D, Instructions.FLE_D, Instructions.FCVT_D_W, Instructions.FCVT_D_WU, Instructions.FLD, Instructions.FSD, Instructions.FDIV_D, Instructions.FSQRT_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_X: Seq[BitPat] = Seq(
    Instructions.FCVT_S_D, Instructions.FCVT_D_S, Instructions.FSGNJ_D, Instructions.FSGNJX_D, Instructions.FSGNJN_D, Instructions.FMIN_D, Instructions.FMAX_D, Instructions.FADD_D, Instructions.FSUB_D, Instructions.FMUL_D, Instructions.FMADD_D, Instructions.FMSUB_D, Instructions.FNMADD_D, Instructions.FNMSUB_D, Instructions.FCLASS_D, Instructions.FCVT_W_D, Instructions.FCVT_WU_D, Instructions.FEQ_D, Instructions.FLT_D, Instructions.FLE_D, Instructions.FCVT_D_W, Instructions.FCVT_D_WU, Instructions.FDIV_D, Instructions.FSQRT_D, 
  )
  val insns_A2_IMM: Seq[BitPat] = Seq(
    Instructions.FLD, Instructions.FSD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_X.exists(op.bitPat)) A2_X else if (insns_A2_IMM.exists(op.bitPat)) A2_IMM else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_X: Seq[BitPat] = Seq(
    Instructions.FCVT_S_D, Instructions.FCVT_D_S, Instructions.FSGNJ_D, Instructions.FSGNJX_D, Instructions.FSGNJN_D, Instructions.FMIN_D, Instructions.FMAX_D, Instructions.FADD_D, Instructions.FSUB_D, Instructions.FMUL_D, Instructions.FMADD_D, Instructions.FMSUB_D, Instructions.FNMADD_D, Instructions.FNMSUB_D, Instructions.FCLASS_D, Instructions.FCVT_W_D, Instructions.FCVT_WU_D, Instructions.FEQ_D, Instructions.FLT_D, Instructions.FLE_D, Instructions.FDIV_D, Instructions.FSQRT_D, 
  )
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.FCVT_D_W, Instructions.FCVT_D_WU, Instructions.FLD, Instructions.FSD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_X.exists(op.bitPat)) A1_X else if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.FCVT_S_D, Instructions.FCVT_D_S, Instructions.FSGNJ_D, Instructions.FSGNJX_D, Instructions.FSGNJN_D, Instructions.FMIN_D, Instructions.FMAX_D, Instructions.FADD_D, Instructions.FSUB_D, Instructions.FMUL_D, Instructions.FMADD_D, Instructions.FMSUB_D, Instructions.FNMADD_D, Instructions.FNMSUB_D, Instructions.FCLASS_D, Instructions.FCVT_W_D, Instructions.FCVT_WU_D, Instructions.FEQ_D, Instructions.FLT_D, Instructions.FLE_D, Instructions.FCVT_D_W, Instructions.FCVT_D_WU, Instructions.FDIV_D, Instructions.FSQRT_D, 
  )
  val insns_IMM_I: Seq[BitPat] = Seq(
    Instructions.FLD, 
  )
  val insns_IMM_S: Seq[BitPat] = Seq(
    Instructions.FSD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else if (insns_IMM_I.exists(op.bitPat)) IMM_I else if (insns_IMM_S.exists(op.bitPat)) IMM_S else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_X: Seq[BitPat] = Seq(
    Instructions.FCVT_S_D, Instructions.FCVT_D_S, Instructions.FSGNJ_D, Instructions.FSGNJX_D, Instructions.FSGNJN_D, Instructions.FMIN_D, Instructions.FMAX_D, Instructions.FADD_D, Instructions.FSUB_D, Instructions.FMUL_D, Instructions.FMADD_D, Instructions.FMSUB_D, Instructions.FNMADD_D, Instructions.FNMSUB_D, Instructions.FCLASS_D, Instructions.FCVT_W_D, Instructions.FCVT_WU_D, Instructions.FEQ_D, Instructions.FLT_D, Instructions.FLE_D, Instructions.FCVT_D_W, Instructions.FCVT_D_WU, Instructions.FDIV_D, Instructions.FSQRT_D, 
  )
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.FLD, Instructions.FSD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_X.exists(op.bitPat)) DW_X else if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_X: Seq[BitPat] = Seq(
    Instructions.FCVT_S_D, Instructions.FCVT_D_S, Instructions.FSGNJ_D, Instructions.FSGNJX_D, Instructions.FSGNJN_D, Instructions.FMIN_D, Instructions.FMAX_D, Instructions.FADD_D, Instructions.FSUB_D, Instructions.FMUL_D, Instructions.FMADD_D, Instructions.FMSUB_D, Instructions.FNMADD_D, Instructions.FNMSUB_D, Instructions.FCLASS_D, Instructions.FCVT_W_D, Instructions.FCVT_WU_D, Instructions.FEQ_D, Instructions.FLT_D, Instructions.FLE_D, Instructions.FCVT_D_W, Instructions.FCVT_D_WU, Instructions.FDIV_D, Instructions.FSQRT_D, 
  )
  val insns_aluFn_FN_ADD: Seq[BitPat] = Seq(
    Instructions.FLD, Instructions.FSD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_X.exists(op.bitPat)) aluFn.FN_X else if (insns_aluFn_FN_ADD.exists(op.bitPat)) aluFn.FN_ADD else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_D, Instructions.FCVT_D_S, Instructions.FSGNJ_D, Instructions.FSGNJX_D, Instructions.FSGNJN_D, Instructions.FMIN_D, Instructions.FMAX_D, Instructions.FADD_D, Instructions.FSUB_D, Instructions.FMUL_D, Instructions.FMADD_D, Instructions.FMSUB_D, Instructions.FNMADD_D, Instructions.FNMSUB_D, Instructions.FCLASS_D, Instructions.FCVT_W_D, Instructions.FCVT_WU_D, Instructions.FEQ_D, Instructions.FLT_D, Instructions.FLE_D, Instructions.FCVT_D_W, Instructions.FCVT_D_WU, Instructions.FDIV_D, Instructions.FSQRT_D, 
  )
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FLD, Instructions.FSD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.FCVT_S_D, Instructions.FCVT_D_S, Instructions.FSGNJ_D, Instructions.FSGNJX_D, Instructions.FSGNJN_D, Instructions.FMIN_D, Instructions.FMAX_D, Instructions.FADD_D, Instructions.FSUB_D, Instructions.FMUL_D, Instructions.FMADD_D, Instructions.FMSUB_D, Instructions.FNMADD_D, Instructions.FNMSUB_D, Instructions.FCLASS_D, Instructions.FCVT_W_D, Instructions.FCVT_WU_D, Instructions.FEQ_D, Instructions.FLT_D, Instructions.FLE_D, Instructions.FCVT_D_W, Instructions.FCVT_D_WU, Instructions.FDIV_D, Instructions.FSQRT_D, 
  )
  val insns_M_XRD: Seq[BitPat] = Seq(
    Instructions.FLD, 
  )
  val insns_M_XWR: Seq[BitPat] = Seq(
    Instructions.FSD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else if (insns_M_XRD.exists(op.bitPat)) M_XRD else if (insns_M_XWR.exists(op.bitPat)) M_XWR else M_X
  }
}
object rfs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FCVT_S_D, Instructions.FCVT_D_S, Instructions.FSGNJ_D, Instructions.FSGNJX_D, Instructions.FSGNJN_D, Instructions.FMIN_D, Instructions.FMAX_D, Instructions.FADD_D, Instructions.FSUB_D, Instructions.FMUL_D, Instructions.FMADD_D, Instructions.FMSUB_D, Instructions.FNMADD_D, Instructions.FNMSUB_D, Instructions.FCLASS_D, Instructions.FCVT_W_D, Instructions.FCVT_WU_D, Instructions.FEQ_D, Instructions.FLT_D, Instructions.FLE_D, Instructions.FDIV_D, Instructions.FSQRT_D, 
  )
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_D_W, Instructions.FCVT_D_WU, Instructions.FLD, Instructions.FSD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_D, Instructions.FCVT_D_S, Instructions.FCLASS_D, Instructions.FCVT_W_D, Instructions.FCVT_WU_D, Instructions.FCVT_D_W, Instructions.FCVT_D_WU, Instructions.FLD, 
  )
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FSGNJ_D, Instructions.FSGNJX_D, Instructions.FSGNJN_D, Instructions.FMIN_D, Instructions.FMAX_D, Instructions.FADD_D, Instructions.FSUB_D, Instructions.FMUL_D, Instructions.FMADD_D, Instructions.FMSUB_D, Instructions.FNMADD_D, Instructions.FNMSUB_D, Instructions.FEQ_D, Instructions.FLT_D, Instructions.FLE_D, Instructions.FSD, Instructions.FDIV_D, Instructions.FSQRT_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_D, Instructions.FCVT_D_S, Instructions.FSGNJ_D, Instructions.FSGNJX_D, Instructions.FSGNJN_D, Instructions.FMIN_D, Instructions.FMAX_D, Instructions.FADD_D, Instructions.FSUB_D, Instructions.FMUL_D, Instructions.FCLASS_D, Instructions.FCVT_W_D, Instructions.FCVT_WU_D, Instructions.FEQ_D, Instructions.FLT_D, Instructions.FLE_D, Instructions.FCVT_D_W, Instructions.FCVT_D_WU, Instructions.FLD, Instructions.FSD, Instructions.FDIV_D, Instructions.FSQRT_D, 
  )
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FMADD_D, Instructions.FMSUB_D, Instructions.FNMADD_D, Instructions.FNMSUB_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object wfd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FCVT_S_D, Instructions.FCVT_D_S, Instructions.FSGNJ_D, Instructions.FSGNJX_D, Instructions.FSGNJN_D, Instructions.FMIN_D, Instructions.FMAX_D, Instructions.FADD_D, Instructions.FSUB_D, Instructions.FMUL_D, Instructions.FMADD_D, Instructions.FMSUB_D, Instructions.FNMADD_D, Instructions.FNMSUB_D, Instructions.FCVT_D_W, Instructions.FCVT_D_WU, Instructions.FLD, Instructions.FDIV_D, Instructions.FSQRT_D, 
  )
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCLASS_D, Instructions.FCVT_W_D, Instructions.FCVT_WU_D, Instructions.FEQ_D, Instructions.FLT_D, Instructions.FLE_D, Instructions.FSD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_D, Instructions.FCVT_D_S, Instructions.FSGNJ_D, Instructions.FSGNJX_D, Instructions.FSGNJN_D, Instructions.FMIN_D, Instructions.FMAX_D, Instructions.FADD_D, Instructions.FSUB_D, Instructions.FMUL_D, Instructions.FMADD_D, Instructions.FMSUB_D, Instructions.FNMADD_D, Instructions.FNMSUB_D, Instructions.FCLASS_D, Instructions.FCVT_W_D, Instructions.FCVT_WU_D, Instructions.FEQ_D, Instructions.FLT_D, Instructions.FLE_D, Instructions.FCVT_D_W, Instructions.FCVT_D_WU, Instructions.FLD, Instructions.FSD, Instructions.FDIV_D, Instructions.FSQRT_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_D, Instructions.FCVT_D_S, Instructions.FSGNJ_D, Instructions.FSGNJX_D, Instructions.FSGNJN_D, Instructions.FMIN_D, Instructions.FMAX_D, Instructions.FADD_D, Instructions.FSUB_D, Instructions.FMUL_D, Instructions.FMADD_D, Instructions.FMSUB_D, Instructions.FNMADD_D, Instructions.FNMSUB_D, Instructions.FCLASS_D, Instructions.FCVT_W_D, Instructions.FCVT_WU_D, Instructions.FEQ_D, Instructions.FLT_D, Instructions.FLE_D, Instructions.FCVT_D_W, Instructions.FCVT_D_WU, Instructions.FLD, Instructions.FSD, Instructions.FDIV_D, Instructions.FSQRT_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_D, Instructions.FCVT_D_S, Instructions.FSGNJ_D, Instructions.FSGNJX_D, Instructions.FSGNJN_D, Instructions.FMIN_D, Instructions.FMAX_D, Instructions.FADD_D, Instructions.FSUB_D, Instructions.FMUL_D, Instructions.FMADD_D, Instructions.FMSUB_D, Instructions.FNMADD_D, Instructions.FNMSUB_D, Instructions.FCVT_D_W, Instructions.FCVT_D_WU, Instructions.FLD, Instructions.FSD, Instructions.FDIV_D, Instructions.FSQRT_D, 
  )
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FCLASS_D, Instructions.FCVT_W_D, Instructions.FCVT_WU_D, Instructions.FEQ_D, Instructions.FLT_D, Instructions.FLE_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_D, Instructions.FCVT_D_S, Instructions.FSGNJ_D, Instructions.FSGNJX_D, Instructions.FSGNJN_D, Instructions.FMIN_D, Instructions.FMAX_D, Instructions.FADD_D, Instructions.FSUB_D, Instructions.FMUL_D, Instructions.FMADD_D, Instructions.FMSUB_D, Instructions.FNMADD_D, Instructions.FNMSUB_D, Instructions.FCLASS_D, Instructions.FCVT_W_D, Instructions.FCVT_WU_D, Instructions.FEQ_D, Instructions.FLT_D, Instructions.FLE_D, Instructions.FCVT_D_W, Instructions.FCVT_D_WU, Instructions.FLD, Instructions.FSD, Instructions.FDIV_D, Instructions.FSQRT_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_D, Instructions.FCVT_D_S, Instructions.FSGNJ_D, Instructions.FSGNJX_D, Instructions.FSGNJN_D, Instructions.FMIN_D, Instructions.FMAX_D, Instructions.FADD_D, Instructions.FSUB_D, Instructions.FMUL_D, Instructions.FMADD_D, Instructions.FMSUB_D, Instructions.FNMADD_D, Instructions.FNMSUB_D, Instructions.FCLASS_D, Instructions.FCVT_W_D, Instructions.FCVT_WU_D, Instructions.FEQ_D, Instructions.FLT_D, Instructions.FLE_D, Instructions.FCVT_D_W, Instructions.FCVT_D_WU, Instructions.FLD, Instructions.FSD, Instructions.FDIV_D, Instructions.FSQRT_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_D, Instructions.FCVT_D_S, Instructions.FSGNJ_D, Instructions.FSGNJX_D, Instructions.FSGNJN_D, Instructions.FMIN_D, Instructions.FMAX_D, Instructions.FADD_D, Instructions.FSUB_D, Instructions.FMUL_D, Instructions.FMADD_D, Instructions.FMSUB_D, Instructions.FNMADD_D, Instructions.FNMSUB_D, Instructions.FCLASS_D, Instructions.FCVT_W_D, Instructions.FCVT_WU_D, Instructions.FEQ_D, Instructions.FLT_D, Instructions.FLE_D, Instructions.FCVT_D_W, Instructions.FCVT_D_WU, Instructions.FLD, Instructions.FSD, Instructions.FDIV_D, Instructions.FSQRT_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_D, Instructions.FCVT_D_S, Instructions.FSGNJ_D, Instructions.FSGNJX_D, Instructions.FSGNJN_D, Instructions.FMIN_D, Instructions.FMAX_D, Instructions.FADD_D, Instructions.FSUB_D, Instructions.FMUL_D, Instructions.FMADD_D, Instructions.FMSUB_D, Instructions.FNMADD_D, Instructions.FNMSUB_D, Instructions.FCLASS_D, Instructions.FCVT_W_D, Instructions.FCVT_WU_D, Instructions.FEQ_D, Instructions.FLT_D, Instructions.FLE_D, Instructions.FCVT_D_W, Instructions.FCVT_D_WU, Instructions.FLD, Instructions.FSD, Instructions.FDIV_D, Instructions.FSQRT_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FCVT_S_D, Instructions.FCVT_D_S, Instructions.FSGNJ_D, Instructions.FSGNJX_D, Instructions.FSGNJN_D, Instructions.FMIN_D, Instructions.FMAX_D, Instructions.FADD_D, Instructions.FSUB_D, Instructions.FMUL_D, Instructions.FMADD_D, Instructions.FMSUB_D, Instructions.FNMADD_D, Instructions.FNMSUB_D, Instructions.FCLASS_D, Instructions.FCVT_W_D, Instructions.FCVT_WU_D, Instructions.FEQ_D, Instructions.FLT_D, Instructions.FLE_D, Instructions.FCVT_D_W, Instructions.FCVT_D_WU, Instructions.FLD, Instructions.FSD, Instructions.FDIV_D, Instructions.FSQRT_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}

}

class HDDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FCVT_D_H, Instructions.FCVT_H_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FCVT_D_H, Instructions.FCVT_H_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_D_H, Instructions.FCVT_H_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_D_H, Instructions.FCVT_H_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_D_H, Instructions.FCVT_H_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_D_H, Instructions.FCVT_H_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_D_H, Instructions.FCVT_H_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_D_H, Instructions.FCVT_H_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_D_H, Instructions.FCVT_H_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_D_H, Instructions.FCVT_H_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_D_H, Instructions.FCVT_H_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_D_H, Instructions.FCVT_H_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_X: Seq[BitPat] = Seq(
    Instructions.FCVT_D_H, Instructions.FCVT_H_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_X.exists(op.bitPat)) A2_X else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_X: Seq[BitPat] = Seq(
    Instructions.FCVT_D_H, Instructions.FCVT_H_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_X.exists(op.bitPat)) A1_X else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.FCVT_D_H, Instructions.FCVT_H_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_X: Seq[BitPat] = Seq(
    Instructions.FCVT_D_H, Instructions.FCVT_H_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_X.exists(op.bitPat)) DW_X else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_X: Seq[BitPat] = Seq(
    Instructions.FCVT_D_H, Instructions.FCVT_H_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_X.exists(op.bitPat)) aluFn.FN_X else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_D_H, Instructions.FCVT_H_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.FCVT_D_H, Instructions.FCVT_H_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FCVT_D_H, Instructions.FCVT_H_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_D_H, Instructions.FCVT_H_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_D_H, Instructions.FCVT_H_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FCVT_D_H, Instructions.FCVT_H_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_D_H, Instructions.FCVT_H_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_D_H, Instructions.FCVT_H_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_D_H, Instructions.FCVT_H_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.FCVT_D_H, Instructions.FCVT_H_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_D_H, Instructions.FCVT_H_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_D_H, Instructions.FCVT_H_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_D_H, Instructions.FCVT_H_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FCVT_D_H, Instructions.FCVT_H_D, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}

}

class H64Decode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, 
  )
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_X: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_X.exists(op.bitPat)) A2_X else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_X: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, 
  )
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_X.exists(op.bitPat)) A1_X else if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_X: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_X.exists(op.bitPat)) DW_X else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_X: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_X.exists(op.bitPat)) aluFn.FN_X else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, 
  )
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, 
  )
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, 
  )
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else if (insns_N.exists(op.bitPat)) N else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

class F64Decode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, 
  )
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_X: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_X.exists(op.bitPat)) A2_X else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_X: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, 
  )
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_X.exists(op.bitPat)) A1_X else if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_X: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_X.exists(op.bitPat)) DW_X else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_X: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_X.exists(op.bitPat)) aluFn.FN_X else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, 
  )
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, 
  )
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, 
  )
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else if (insns_N.exists(op.bitPat)) N else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FCVT_L_S, Instructions.FCVT_LU_S, Instructions.FCVT_S_L, Instructions.FCVT_S_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

class D64Decode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FMV_X_D, Instructions.FCVT_L_D, Instructions.FCVT_LU_D, Instructions.FMV_D_X, Instructions.FCVT_D_L, Instructions.FCVT_D_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FMV_X_D, Instructions.FCVT_L_D, Instructions.FCVT_LU_D, Instructions.FMV_D_X, Instructions.FCVT_D_L, Instructions.FCVT_D_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FMV_X_D, Instructions.FCVT_L_D, Instructions.FCVT_LU_D, Instructions.FMV_D_X, Instructions.FCVT_D_L, Instructions.FCVT_D_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FMV_X_D, Instructions.FCVT_L_D, Instructions.FCVT_LU_D, Instructions.FMV_D_X, Instructions.FCVT_D_L, Instructions.FCVT_D_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FMV_X_D, Instructions.FCVT_L_D, Instructions.FCVT_LU_D, Instructions.FMV_D_X, Instructions.FCVT_D_L, Instructions.FCVT_D_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FMV_X_D, Instructions.FCVT_L_D, Instructions.FCVT_LU_D, Instructions.FMV_D_X, Instructions.FCVT_D_L, Instructions.FCVT_D_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FMV_X_D, Instructions.FCVT_L_D, Instructions.FCVT_LU_D, Instructions.FMV_D_X, Instructions.FCVT_D_L, Instructions.FCVT_D_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FMV_X_D, Instructions.FCVT_L_D, Instructions.FCVT_LU_D, 
  )
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FMV_D_X, Instructions.FCVT_D_L, Instructions.FCVT_D_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FMV_X_D, Instructions.FCVT_L_D, Instructions.FCVT_LU_D, Instructions.FMV_D_X, Instructions.FCVT_D_L, Instructions.FCVT_D_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FMV_X_D, Instructions.FCVT_L_D, Instructions.FCVT_LU_D, Instructions.FMV_D_X, Instructions.FCVT_D_L, Instructions.FCVT_D_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FMV_X_D, Instructions.FCVT_L_D, Instructions.FCVT_LU_D, Instructions.FMV_D_X, Instructions.FCVT_D_L, Instructions.FCVT_D_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FMV_X_D, Instructions.FCVT_L_D, Instructions.FCVT_LU_D, Instructions.FMV_D_X, Instructions.FCVT_D_L, Instructions.FCVT_D_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_X: Seq[BitPat] = Seq(
    Instructions.FMV_X_D, Instructions.FCVT_L_D, Instructions.FCVT_LU_D, Instructions.FMV_D_X, Instructions.FCVT_D_L, Instructions.FCVT_D_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_X.exists(op.bitPat)) A2_X else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_X: Seq[BitPat] = Seq(
    Instructions.FMV_X_D, Instructions.FCVT_L_D, Instructions.FCVT_LU_D, 
  )
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.FMV_D_X, Instructions.FCVT_D_L, Instructions.FCVT_D_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_X.exists(op.bitPat)) A1_X else if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.FMV_X_D, Instructions.FCVT_L_D, Instructions.FCVT_LU_D, Instructions.FMV_D_X, Instructions.FCVT_D_L, Instructions.FCVT_D_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_X: Seq[BitPat] = Seq(
    Instructions.FMV_X_D, Instructions.FCVT_L_D, Instructions.FCVT_LU_D, Instructions.FMV_D_X, Instructions.FCVT_D_L, Instructions.FCVT_D_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_X.exists(op.bitPat)) DW_X else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_X: Seq[BitPat] = Seq(
    Instructions.FMV_X_D, Instructions.FCVT_L_D, Instructions.FCVT_LU_D, Instructions.FMV_D_X, Instructions.FCVT_D_L, Instructions.FCVT_D_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_X.exists(op.bitPat)) aluFn.FN_X else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FMV_X_D, Instructions.FCVT_L_D, Instructions.FCVT_LU_D, Instructions.FMV_D_X, Instructions.FCVT_D_L, Instructions.FCVT_D_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.FMV_X_D, Instructions.FCVT_L_D, Instructions.FCVT_LU_D, Instructions.FMV_D_X, Instructions.FCVT_D_L, Instructions.FCVT_D_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FMV_X_D, Instructions.FCVT_L_D, Instructions.FCVT_LU_D, 
  )
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FMV_D_X, Instructions.FCVT_D_L, Instructions.FCVT_D_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FMV_X_D, Instructions.FCVT_L_D, Instructions.FCVT_LU_D, Instructions.FMV_D_X, Instructions.FCVT_D_L, Instructions.FCVT_D_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FMV_X_D, Instructions.FCVT_L_D, Instructions.FCVT_LU_D, Instructions.FMV_D_X, Instructions.FCVT_D_L, Instructions.FCVT_D_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FMV_X_D, Instructions.FCVT_L_D, Instructions.FCVT_LU_D, 
  )
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FMV_D_X, Instructions.FCVT_D_L, Instructions.FCVT_D_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FMV_X_D, Instructions.FCVT_L_D, Instructions.FCVT_LU_D, Instructions.FMV_D_X, Instructions.FCVT_D_L, Instructions.FCVT_D_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FMV_X_D, Instructions.FCVT_L_D, Instructions.FCVT_LU_D, Instructions.FMV_D_X, Instructions.FCVT_D_L, Instructions.FCVT_D_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FMV_X_D, Instructions.FCVT_L_D, Instructions.FCVT_LU_D, 
  )
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FMV_D_X, Instructions.FCVT_D_L, Instructions.FCVT_D_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else if (insns_N.exists(op.bitPat)) N else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.FMV_X_D, Instructions.FCVT_L_D, Instructions.FCVT_LU_D, Instructions.FMV_D_X, Instructions.FCVT_D_L, Instructions.FCVT_D_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FMV_X_D, Instructions.FCVT_L_D, Instructions.FCVT_LU_D, Instructions.FMV_D_X, Instructions.FCVT_D_L, Instructions.FCVT_D_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FMV_X_D, Instructions.FCVT_L_D, Instructions.FCVT_LU_D, Instructions.FMV_D_X, Instructions.FCVT_D_L, Instructions.FCVT_D_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.FMV_X_D, Instructions.FCVT_L_D, Instructions.FCVT_LU_D, Instructions.FMV_D_X, Instructions.FCVT_D_L, Instructions.FCVT_D_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.FMV_X_D, Instructions.FCVT_L_D, Instructions.FCVT_LU_D, Instructions.FMV_D_X, Instructions.FCVT_D_L, Instructions.FCVT_D_LU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}

}

class SCIEDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.SCIE.opcode, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SCIE.opcode, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SCIE.opcode, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SCIE.opcode, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SCIE.opcode, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SCIE.opcode, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.SCIE.opcode, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.SCIE.opcode, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.SCIE.opcode, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SCIE.opcode, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SCIE.opcode, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SCIE.opcode, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_ZERO: Seq[BitPat] = Seq(
    Instructions.SCIE.opcode, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_ZERO.exists(op.bitPat)) A2_ZERO else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.SCIE.opcode, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.SCIE.opcode, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.SCIE.opcode, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_X: Seq[BitPat] = Seq(
    Instructions.SCIE.opcode, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_X.exists(op.bitPat)) aluFn.FN_X else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SCIE.opcode, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.SCIE.opcode, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SCIE.opcode, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SCIE.opcode, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SCIE.opcode, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SCIE.opcode, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SCIE.opcode, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SCIE.opcode, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.SCIE.opcode, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.SCIE.opcode, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SCIE.opcode, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SCIE.opcode, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SCIE.opcode, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SCIE.opcode, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

trait UsesABLUFN {
  val aluFn = ABLUFN()
}

class ZBADecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.SH1ADD, Instructions.SH2ADD, Instructions.SH3ADD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SH1ADD, Instructions.SH2ADD, Instructions.SH3ADD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SH1ADD, Instructions.SH2ADD, Instructions.SH3ADD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SH1ADD, Instructions.SH2ADD, Instructions.SH3ADD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SH1ADD, Instructions.SH2ADD, Instructions.SH3ADD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SH1ADD, Instructions.SH2ADD, Instructions.SH3ADD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.SH1ADD, Instructions.SH2ADD, Instructions.SH3ADD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.SH1ADD, Instructions.SH2ADD, Instructions.SH3ADD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SH1ADD, Instructions.SH2ADD, Instructions.SH3ADD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SH1ADD, Instructions.SH2ADD, Instructions.SH3ADD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SH1ADD, Instructions.SH2ADD, Instructions.SH3ADD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SH1ADD, Instructions.SH2ADD, Instructions.SH3ADD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_RS2: Seq[BitPat] = Seq(
    Instructions.SH1ADD, Instructions.SH2ADD, Instructions.SH3ADD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_RS2.exists(op.bitPat)) A2_RS2 else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.SH1ADD, Instructions.SH2ADD, Instructions.SH3ADD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.SH1ADD, Instructions.SH2ADD, Instructions.SH3ADD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.SH1ADD, Instructions.SH2ADD, Instructions.SH3ADD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_SH1ADD: Seq[BitPat] = Seq(
    Instructions.SH1ADD, 
  )
  val insns_aluFn_FN_SH2ADD: Seq[BitPat] = Seq(
    Instructions.SH2ADD, 
  )
  val insns_aluFn_FN_SH3ADD: Seq[BitPat] = Seq(
    Instructions.SH3ADD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_SH1ADD.exists(op.bitPat)) aluFn.FN_SH1ADD else if (insns_aluFn_FN_SH2ADD.exists(op.bitPat)) aluFn.FN_SH2ADD else if (insns_aluFn_FN_SH3ADD.exists(op.bitPat)) aluFn.FN_SH3ADD else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SH1ADD, Instructions.SH2ADD, Instructions.SH3ADD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.SH1ADD, Instructions.SH2ADD, Instructions.SH3ADD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SH1ADD, Instructions.SH2ADD, Instructions.SH3ADD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SH1ADD, Instructions.SH2ADD, Instructions.SH3ADD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SH1ADD, Instructions.SH2ADD, Instructions.SH3ADD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SH1ADD, Instructions.SH2ADD, Instructions.SH3ADD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SH1ADD, Instructions.SH2ADD, Instructions.SH3ADD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SH1ADD, Instructions.SH2ADD, Instructions.SH3ADD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.SH1ADD, Instructions.SH2ADD, Instructions.SH3ADD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.SH1ADD, Instructions.SH2ADD, Instructions.SH3ADD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SH1ADD, Instructions.SH2ADD, Instructions.SH3ADD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SH1ADD, Instructions.SH2ADD, Instructions.SH3ADD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SH1ADD, Instructions.SH2ADD, Instructions.SH3ADD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SH1ADD, Instructions.SH2ADD, Instructions.SH3ADD, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

class ZBA64Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_RS2: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_RS2.exists(op.bitPat)) A2_RS2 else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_ANDN: Seq[BitPat] = Seq(
    Instructions.ANDN, 
  )
  val insns_aluFn_FN_ORN: Seq[BitPat] = Seq(
    Instructions.ORN, 
  )
  val insns_aluFn_FN_XNOR: Seq[BitPat] = Seq(
    Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_ANDN.exists(op.bitPat)) aluFn.FN_ANDN else if (insns_aluFn_FN_ORN.exists(op.bitPat)) aluFn.FN_ORN else if (insns_aluFn_FN_XNOR.exists(op.bitPat)) aluFn.FN_XNOR else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

// In both Zbb and Zbkb
class ZBBNDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_RS2: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_RS2.exists(op.bitPat)) A2_RS2 else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_ANDN: Seq[BitPat] = Seq(
    Instructions.ANDN, 
  )
  val insns_aluFn_FN_ORN: Seq[BitPat] = Seq(
    Instructions.ORN, 
  )
  val insns_aluFn_FN_XNOR: Seq[BitPat] = Seq(
    Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_ANDN.exists(op.bitPat)) aluFn.FN_ANDN else if (insns_aluFn_FN_ORN.exists(op.bitPat)) aluFn.FN_ORN else if (insns_aluFn_FN_XNOR.exists(op.bitPat)) aluFn.FN_XNOR else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ANDN, Instructions.ORN, Instructions.XNOR, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

// In both Zbb and Zbkb
class ZBBRDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.ROR, Instructions.ROL, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ROR, Instructions.ROL, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ROR, Instructions.ROL, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ROR, Instructions.ROL, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ROR, Instructions.ROL, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ROR, Instructions.ROL, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.ROR, Instructions.ROL, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.ROR, Instructions.ROL, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ROR, Instructions.ROL, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ROR, Instructions.ROL, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ROR, Instructions.ROL, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ROR, Instructions.ROL, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_RS2: Seq[BitPat] = Seq(
    Instructions.ROR, Instructions.ROL, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_RS2.exists(op.bitPat)) A2_RS2 else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.ROR, Instructions.ROL, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.ROR, Instructions.ROL, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.ROR, Instructions.ROL, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_ROR: Seq[BitPat] = Seq(
    Instructions.ROR, 
  )
  val insns_aluFn_FN_ROL: Seq[BitPat] = Seq(
    Instructions.ROL, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_ROR.exists(op.bitPat)) aluFn.FN_ROR else if (insns_aluFn_FN_ROL.exists(op.bitPat)) aluFn.FN_ROL else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ROR, Instructions.ROL, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.ROR, Instructions.ROL, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ROR, Instructions.ROL, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ROR, Instructions.ROL, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ROR, Instructions.ROL, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ROR, Instructions.ROL, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ROR, Instructions.ROL, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ROR, Instructions.ROL, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.ROR, Instructions.ROL, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.ROR, Instructions.ROL, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ROR, Instructions.ROL, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ROR, Instructions.ROL, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ROR, Instructions.ROL, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ROR, Instructions.ROL, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

class ZBBR32Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.Instructions32.RORI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.RORI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.RORI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.RORI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.RORI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.RORI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.RORI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.Instructions32.RORI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.RORI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.RORI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.RORI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.RORI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_IMM: Seq[BitPat] = Seq(
    Instructions.Instructions32.RORI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_IMM.exists(op.bitPat)) A2_IMM else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.Instructions32.RORI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_I: Seq[BitPat] = Seq(
    Instructions.Instructions32.RORI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_I.exists(op.bitPat)) IMM_I else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.Instructions32.RORI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_ROR: Seq[BitPat] = Seq(
    Instructions.Instructions32.RORI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_ROR.exists(op.bitPat)) aluFn.FN_ROR else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.RORI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.Instructions32.RORI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.RORI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.RORI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.RORI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.RORI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.RORI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.RORI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.Instructions32.RORI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.RORI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.RORI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.RORI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.RORI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.RORI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

class ZBBR64Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.RORI, Instructions.RORW, Instructions.ROLW, Instructions.RORIW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.RORI, Instructions.RORW, Instructions.ROLW, Instructions.RORIW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.RORI, Instructions.RORW, Instructions.ROLW, Instructions.RORIW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.RORI, Instructions.RORW, Instructions.ROLW, Instructions.RORIW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.RORI, Instructions.RORW, Instructions.ROLW, Instructions.RORIW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.RORI, Instructions.RORW, Instructions.ROLW, Instructions.RORIW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.RORI, Instructions.RORIW, 
  )
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.RORW, Instructions.ROLW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.RORI, Instructions.RORW, Instructions.ROLW, Instructions.RORIW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.RORI, Instructions.RORW, Instructions.ROLW, Instructions.RORIW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.RORI, Instructions.RORW, Instructions.ROLW, Instructions.RORIW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.RORI, Instructions.RORW, Instructions.ROLW, Instructions.RORIW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.RORI, Instructions.RORW, Instructions.ROLW, Instructions.RORIW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_IMM: Seq[BitPat] = Seq(
    Instructions.RORI, Instructions.RORIW, 
  )
  val insns_A2_RS2: Seq[BitPat] = Seq(
    Instructions.RORW, Instructions.ROLW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_IMM.exists(op.bitPat)) A2_IMM else if (insns_A2_RS2.exists(op.bitPat)) A2_RS2 else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.RORI, Instructions.RORW, Instructions.ROLW, Instructions.RORIW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_I: Seq[BitPat] = Seq(
    Instructions.RORI, Instructions.RORIW, 
  )
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.RORW, Instructions.ROLW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_I.exists(op.bitPat)) IMM_I else if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.RORI, 
  )
  val insns_DW_32: Seq[BitPat] = Seq(
    Instructions.RORW, Instructions.ROLW, Instructions.RORIW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else if (insns_DW_32.exists(op.bitPat)) DW_32 else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_ROR: Seq[BitPat] = Seq(
    Instructions.RORI, Instructions.RORW, Instructions.RORIW, 
  )
  val insns_aluFn_FN_ROL: Seq[BitPat] = Seq(
    Instructions.ROLW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_ROR.exists(op.bitPat)) aluFn.FN_ROR else if (insns_aluFn_FN_ROL.exists(op.bitPat)) aluFn.FN_ROL else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.RORI, Instructions.RORW, Instructions.ROLW, Instructions.RORIW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.RORI, Instructions.RORW, Instructions.ROLW, Instructions.RORIW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.RORI, Instructions.RORW, Instructions.ROLW, Instructions.RORIW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.RORI, Instructions.RORW, Instructions.ROLW, Instructions.RORIW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.RORI, Instructions.RORW, Instructions.ROLW, Instructions.RORIW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.RORI, Instructions.RORW, Instructions.ROLW, Instructions.RORIW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.RORI, Instructions.RORW, Instructions.ROLW, Instructions.RORIW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.RORI, Instructions.RORW, Instructions.ROLW, Instructions.RORIW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.RORI, Instructions.RORW, Instructions.ROLW, Instructions.RORIW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.RORI, Instructions.RORW, Instructions.ROLW, Instructions.RORIW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.RORI, Instructions.RORW, Instructions.ROLW, Instructions.RORIW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.RORI, Instructions.RORW, Instructions.ROLW, Instructions.RORIW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.RORI, Instructions.RORW, Instructions.ROLW, Instructions.RORIW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.RORI, Instructions.RORW, Instructions.ROLW, Instructions.RORIW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

// Only in Zbb
class ZBBCDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.CLZ, Instructions.CTZ, Instructions.CPOP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZ, Instructions.CTZ, Instructions.CPOP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZ, Instructions.CTZ, Instructions.CPOP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZ, Instructions.CTZ, Instructions.CPOP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZ, Instructions.CTZ, Instructions.CPOP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZ, Instructions.CTZ, Instructions.CPOP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZ, Instructions.CTZ, Instructions.CPOP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.CLZ, Instructions.CTZ, Instructions.CPOP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZ, Instructions.CTZ, Instructions.CPOP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZ, Instructions.CTZ, Instructions.CPOP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZ, Instructions.CTZ, Instructions.CPOP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZ, Instructions.CTZ, Instructions.CPOP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_X: Seq[BitPat] = Seq(
    Instructions.CLZ, Instructions.CTZ, Instructions.CPOP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_X.exists(op.bitPat)) A2_X else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.CLZ, Instructions.CTZ, Instructions.CPOP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.CLZ, Instructions.CTZ, Instructions.CPOP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.CLZ, Instructions.CTZ, Instructions.CPOP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_CLZ: Seq[BitPat] = Seq(
    Instructions.CLZ, 
  )
  val insns_aluFn_FN_CTZ: Seq[BitPat] = Seq(
    Instructions.CTZ, 
  )
  val insns_aluFn_FN_CPOP: Seq[BitPat] = Seq(
    Instructions.CPOP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_CLZ.exists(op.bitPat)) aluFn.FN_CLZ else if (insns_aluFn_FN_CTZ.exists(op.bitPat)) aluFn.FN_CTZ else if (insns_aluFn_FN_CPOP.exists(op.bitPat)) aluFn.FN_CPOP else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZ, Instructions.CTZ, Instructions.CPOP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.CLZ, Instructions.CTZ, Instructions.CPOP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZ, Instructions.CTZ, Instructions.CPOP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZ, Instructions.CTZ, Instructions.CPOP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZ, Instructions.CTZ, Instructions.CPOP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZ, Instructions.CTZ, Instructions.CPOP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZ, Instructions.CTZ, Instructions.CPOP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZ, Instructions.CTZ, Instructions.CPOP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.CLZ, Instructions.CTZ, Instructions.CPOP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.CLZ, Instructions.CTZ, Instructions.CPOP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZ, Instructions.CTZ, Instructions.CPOP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZ, Instructions.CTZ, Instructions.CPOP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZ, Instructions.CTZ, Instructions.CPOP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZ, Instructions.CTZ, Instructions.CPOP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

class ZBBC64Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.CLZW, Instructions.CTZW, Instructions.CPOPW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZW, Instructions.CTZW, Instructions.CPOPW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZW, Instructions.CTZW, Instructions.CPOPW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZW, Instructions.CTZW, Instructions.CPOPW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZW, Instructions.CTZW, Instructions.CPOPW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZW, Instructions.CTZW, Instructions.CPOPW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZW, Instructions.CTZW, Instructions.CPOPW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.CLZW, Instructions.CTZW, Instructions.CPOPW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZW, Instructions.CTZW, Instructions.CPOPW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZW, Instructions.CTZW, Instructions.CPOPW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZW, Instructions.CTZW, Instructions.CPOPW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZW, Instructions.CTZW, Instructions.CPOPW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_X: Seq[BitPat] = Seq(
    Instructions.CLZW, Instructions.CTZW, Instructions.CPOPW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_X.exists(op.bitPat)) A2_X else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.CLZW, Instructions.CTZW, Instructions.CPOPW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.CLZW, Instructions.CTZW, Instructions.CPOPW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_32: Seq[BitPat] = Seq(
    Instructions.CLZW, Instructions.CTZW, Instructions.CPOPW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_32.exists(op.bitPat)) DW_32 else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_CLZ: Seq[BitPat] = Seq(
    Instructions.CLZW, 
  )
  val insns_aluFn_FN_CTZ: Seq[BitPat] = Seq(
    Instructions.CTZW, 
  )
  val insns_aluFn_FN_CPOP: Seq[BitPat] = Seq(
    Instructions.CPOPW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_CLZ.exists(op.bitPat)) aluFn.FN_CLZ else if (insns_aluFn_FN_CTZ.exists(op.bitPat)) aluFn.FN_CTZ else if (insns_aluFn_FN_CPOP.exists(op.bitPat)) aluFn.FN_CPOP else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZW, Instructions.CTZW, Instructions.CPOPW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.CLZW, Instructions.CTZW, Instructions.CPOPW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZW, Instructions.CTZW, Instructions.CPOPW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZW, Instructions.CTZW, Instructions.CPOPW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZW, Instructions.CTZW, Instructions.CPOPW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZW, Instructions.CTZW, Instructions.CPOPW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZW, Instructions.CTZW, Instructions.CPOPW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZW, Instructions.CTZW, Instructions.CPOPW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.CLZW, Instructions.CTZW, Instructions.CPOPW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.CLZW, Instructions.CTZW, Instructions.CPOPW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZW, Instructions.CTZW, Instructions.CPOPW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZW, Instructions.CTZW, Instructions.CPOPW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZW, Instructions.CTZW, Instructions.CPOPW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLZW, Instructions.CTZW, Instructions.CPOPW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

// Only in Zbb
class ZBBMDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.MAX, Instructions.MAXU, Instructions.MIN, Instructions.MINU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MAX, Instructions.MAXU, Instructions.MIN, Instructions.MINU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MAX, Instructions.MAXU, Instructions.MIN, Instructions.MINU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MAX, Instructions.MAXU, Instructions.MIN, Instructions.MINU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MAX, Instructions.MAXU, Instructions.MIN, Instructions.MINU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MAX, Instructions.MAXU, Instructions.MIN, Instructions.MINU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.MAX, Instructions.MAXU, Instructions.MIN, Instructions.MINU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.MAX, Instructions.MAXU, Instructions.MIN, Instructions.MINU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MAX, Instructions.MAXU, Instructions.MIN, Instructions.MINU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MAX, Instructions.MAXU, Instructions.MIN, Instructions.MINU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MAX, Instructions.MAXU, Instructions.MIN, Instructions.MINU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MAX, Instructions.MAXU, Instructions.MIN, Instructions.MINU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_RS2: Seq[BitPat] = Seq(
    Instructions.MAX, Instructions.MAXU, Instructions.MIN, Instructions.MINU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_RS2.exists(op.bitPat)) A2_RS2 else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.MAX, Instructions.MAXU, Instructions.MIN, Instructions.MINU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.MAX, Instructions.MAXU, Instructions.MIN, Instructions.MINU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.MAX, Instructions.MAXU, Instructions.MIN, Instructions.MINU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_MAX: Seq[BitPat] = Seq(
    Instructions.MAX, 
  )
  val insns_aluFn_FN_MAXU: Seq[BitPat] = Seq(
    Instructions.MAXU, 
  )
  val insns_aluFn_FN_MIN: Seq[BitPat] = Seq(
    Instructions.MIN, 
  )
  val insns_aluFn_FN_MINU: Seq[BitPat] = Seq(
    Instructions.MINU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_MAX.exists(op.bitPat)) aluFn.FN_MAX else if (insns_aluFn_FN_MAXU.exists(op.bitPat)) aluFn.FN_MAXU else if (insns_aluFn_FN_MIN.exists(op.bitPat)) aluFn.FN_MIN else if (insns_aluFn_FN_MINU.exists(op.bitPat)) aluFn.FN_MINU else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MAX, Instructions.MAXU, Instructions.MIN, Instructions.MINU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.MAX, Instructions.MAXU, Instructions.MIN, Instructions.MINU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MAX, Instructions.MAXU, Instructions.MIN, Instructions.MINU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MAX, Instructions.MAXU, Instructions.MIN, Instructions.MINU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MAX, Instructions.MAXU, Instructions.MIN, Instructions.MINU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MAX, Instructions.MAXU, Instructions.MIN, Instructions.MINU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MAX, Instructions.MAXU, Instructions.MIN, Instructions.MINU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MAX, Instructions.MAXU, Instructions.MIN, Instructions.MINU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.MAX, Instructions.MAXU, Instructions.MIN, Instructions.MINU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.MAX, Instructions.MAXU, Instructions.MIN, Instructions.MINU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MAX, Instructions.MAXU, Instructions.MIN, Instructions.MINU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MAX, Instructions.MAXU, Instructions.MIN, Instructions.MINU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MAX, Instructions.MAXU, Instructions.MIN, Instructions.MINU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.MAX, Instructions.MAXU, Instructions.MIN, Instructions.MINU, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

// Only in Zbb
class ZBBSEDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.SEXT_H, Instructions.SEXT_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SEXT_H, Instructions.SEXT_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SEXT_H, Instructions.SEXT_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SEXT_H, Instructions.SEXT_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SEXT_H, Instructions.SEXT_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SEXT_H, Instructions.SEXT_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SEXT_H, Instructions.SEXT_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.SEXT_H, Instructions.SEXT_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SEXT_H, Instructions.SEXT_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SEXT_H, Instructions.SEXT_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SEXT_H, Instructions.SEXT_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SEXT_H, Instructions.SEXT_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_X: Seq[BitPat] = Seq(
    Instructions.SEXT_H, Instructions.SEXT_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_X.exists(op.bitPat)) A2_X else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.SEXT_H, Instructions.SEXT_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.SEXT_H, Instructions.SEXT_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.SEXT_H, Instructions.SEXT_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_SEXTH: Seq[BitPat] = Seq(
    Instructions.SEXT_H, 
  )
  val insns_aluFn_FN_SEXTB: Seq[BitPat] = Seq(
    Instructions.SEXT_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_SEXTH.exists(op.bitPat)) aluFn.FN_SEXTH else if (insns_aluFn_FN_SEXTB.exists(op.bitPat)) aluFn.FN_SEXTB else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SEXT_H, Instructions.SEXT_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.SEXT_H, Instructions.SEXT_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SEXT_H, Instructions.SEXT_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SEXT_H, Instructions.SEXT_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SEXT_H, Instructions.SEXT_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SEXT_H, Instructions.SEXT_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SEXT_H, Instructions.SEXT_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SEXT_H, Instructions.SEXT_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.SEXT_H, Instructions.SEXT_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.SEXT_H, Instructions.SEXT_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SEXT_H, Instructions.SEXT_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SEXT_H, Instructions.SEXT_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SEXT_H, Instructions.SEXT_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SEXT_H, Instructions.SEXT_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

class ZBBZE64Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_X: Seq[BitPat] = Seq(
    Instructions.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_X.exists(op.bitPat)) A2_X else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_ZEXTH: Seq[BitPat] = Seq(
    Instructions.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_ZEXTH.exists(op.bitPat)) aluFn.FN_ZEXTH else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

class ZBBZE32Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_X: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_X.exists(op.bitPat)) A2_X else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_ZEXTH: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_ZEXTH.exists(op.bitPat)) aluFn.FN_ZEXTH else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZEXT_H, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

class ZBBORCBDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.ORC_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ORC_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ORC_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ORC_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ORC_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ORC_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ORC_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.ORC_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ORC_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ORC_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ORC_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ORC_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_X: Seq[BitPat] = Seq(
    Instructions.ORC_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_X.exists(op.bitPat)) A2_X else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.ORC_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.ORC_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.ORC_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_ORCB: Seq[BitPat] = Seq(
    Instructions.ORC_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_ORCB.exists(op.bitPat)) aluFn.FN_ORCB else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ORC_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.ORC_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ORC_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ORC_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ORC_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ORC_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ORC_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ORC_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.ORC_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.ORC_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ORC_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ORC_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ORC_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.ORC_B, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

// In both Zbb and Zbkb
class ZBBREV864Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_X: Seq[BitPat] = Seq(
    Instructions.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_X.exists(op.bitPat)) A2_X else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_REV8: Seq[BitPat] = Seq(
    Instructions.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_REV8.exists(op.bitPat)) aluFn.FN_REV8 else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

// In both Zbb and Zbkb
class ZBBREV832Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.Instructions32.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.Instructions32.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_X: Seq[BitPat] = Seq(
    Instructions.Instructions32.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_X.exists(op.bitPat)) A2_X else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.Instructions32.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.Instructions32.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.Instructions32.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_REV8: Seq[BitPat] = Seq(
    Instructions.Instructions32.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_REV8.exists(op.bitPat)) aluFn.FN_REV8 else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.Instructions32.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.Instructions32.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.REV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

// Only in Zbkb
class ZBKBDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.PACK, Instructions.PACKH, Instructions.BREV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACK, Instructions.PACKH, Instructions.BREV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACK, Instructions.PACKH, Instructions.BREV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACK, Instructions.PACKH, Instructions.BREV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACK, Instructions.PACKH, Instructions.BREV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACK, Instructions.PACKH, Instructions.BREV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.PACK, Instructions.PACKH, 
  )
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BREV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.PACK, Instructions.PACKH, Instructions.BREV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACK, Instructions.PACKH, Instructions.BREV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACK, Instructions.PACKH, Instructions.BREV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACK, Instructions.PACKH, Instructions.BREV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACK, Instructions.PACKH, Instructions.BREV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_RS2: Seq[BitPat] = Seq(
    Instructions.PACK, Instructions.PACKH, 
  )
  val insns_A2_X: Seq[BitPat] = Seq(
    Instructions.BREV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_RS2.exists(op.bitPat)) A2_RS2 else if (insns_A2_X.exists(op.bitPat)) A2_X else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.PACK, Instructions.PACKH, Instructions.BREV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.PACK, Instructions.PACKH, Instructions.BREV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.PACK, Instructions.PACKH, Instructions.BREV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_PACK: Seq[BitPat] = Seq(
    Instructions.PACK, 
  )
  val insns_aluFn_FN_PACKH: Seq[BitPat] = Seq(
    Instructions.PACKH, 
  )
  val insns_aluFn_FN_BREV8: Seq[BitPat] = Seq(
    Instructions.BREV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_PACK.exists(op.bitPat)) aluFn.FN_PACK else if (insns_aluFn_FN_PACKH.exists(op.bitPat)) aluFn.FN_PACKH else if (insns_aluFn_FN_BREV8.exists(op.bitPat)) aluFn.FN_BREV8 else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACK, Instructions.PACKH, Instructions.BREV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.PACK, Instructions.PACKH, Instructions.BREV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACK, Instructions.PACKH, Instructions.BREV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACK, Instructions.PACKH, Instructions.BREV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACK, Instructions.PACKH, Instructions.BREV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACK, Instructions.PACKH, Instructions.BREV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACK, Instructions.PACKH, Instructions.BREV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACK, Instructions.PACKH, Instructions.BREV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.PACK, Instructions.PACKH, Instructions.BREV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.PACK, Instructions.PACKH, Instructions.BREV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACK, Instructions.PACKH, Instructions.BREV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACK, Instructions.PACKH, Instructions.BREV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACK, Instructions.PACKH, Instructions.BREV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACK, Instructions.PACKH, Instructions.BREV8, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

class ZBKB64Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.PACKW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACKW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACKW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACKW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACKW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACKW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.PACKW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.PACKW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACKW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACKW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACKW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACKW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_RS2: Seq[BitPat] = Seq(
    Instructions.PACKW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_RS2.exists(op.bitPat)) A2_RS2 else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.PACKW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.PACKW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_32: Seq[BitPat] = Seq(
    Instructions.PACKW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_32.exists(op.bitPat)) DW_32 else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_PACK: Seq[BitPat] = Seq(
    Instructions.PACKW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_PACK.exists(op.bitPat)) aluFn.FN_PACK else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACKW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.PACKW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACKW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACKW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACKW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACKW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACKW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACKW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.PACKW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.PACKW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACKW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACKW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACKW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.PACKW, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

class ZBKB32Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZIP, Instructions.Instructions32.UNZIP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZIP, Instructions.Instructions32.UNZIP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZIP, Instructions.Instructions32.UNZIP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZIP, Instructions.Instructions32.UNZIP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZIP, Instructions.Instructions32.UNZIP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZIP, Instructions.Instructions32.UNZIP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZIP, Instructions.Instructions32.UNZIP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZIP, Instructions.Instructions32.UNZIP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZIP, Instructions.Instructions32.UNZIP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZIP, Instructions.Instructions32.UNZIP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZIP, Instructions.Instructions32.UNZIP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZIP, Instructions.Instructions32.UNZIP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_X: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZIP, Instructions.Instructions32.UNZIP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_X.exists(op.bitPat)) A2_X else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZIP, Instructions.Instructions32.UNZIP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZIP, Instructions.Instructions32.UNZIP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_X: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZIP, Instructions.Instructions32.UNZIP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_X.exists(op.bitPat)) DW_X else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_ZIP: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZIP, 
  )
  val insns_aluFn_FN_UNZIP: Seq[BitPat] = Seq(
    Instructions.Instructions32.UNZIP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_ZIP.exists(op.bitPat)) aluFn.FN_ZIP else if (insns_aluFn_FN_UNZIP.exists(op.bitPat)) aluFn.FN_UNZIP else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZIP, Instructions.Instructions32.UNZIP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZIP, Instructions.Instructions32.UNZIP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZIP, Instructions.Instructions32.UNZIP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZIP, Instructions.Instructions32.UNZIP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZIP, Instructions.Instructions32.UNZIP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZIP, Instructions.Instructions32.UNZIP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZIP, Instructions.Instructions32.UNZIP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZIP, Instructions.Instructions32.UNZIP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZIP, Instructions.Instructions32.UNZIP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZIP, Instructions.Instructions32.UNZIP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZIP, Instructions.Instructions32.UNZIP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZIP, Instructions.Instructions32.UNZIP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZIP, Instructions.Instructions32.UNZIP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.ZIP, Instructions.Instructions32.UNZIP, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

// also in Zbkc but Zbkc does not have CLMULR
class ZBCDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.CLMUL, Instructions.CLMULH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLMUL, Instructions.CLMULH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLMUL, Instructions.CLMULH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLMUL, Instructions.CLMULH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLMUL, Instructions.CLMULH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLMUL, Instructions.CLMULH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.CLMUL, Instructions.CLMULH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.CLMUL, Instructions.CLMULH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLMUL, Instructions.CLMULH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.CLMUL, Instructions.CLMULH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLMUL, Instructions.CLMULH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLMUL, Instructions.CLMULH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_RS2: Seq[BitPat] = Seq(
    Instructions.CLMUL, Instructions.CLMULH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_RS2.exists(op.bitPat)) A2_RS2 else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.CLMUL, Instructions.CLMULH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.CLMUL, Instructions.CLMULH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.CLMUL, Instructions.CLMULH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_CLMUL: Seq[BitPat] = Seq(
    Instructions.CLMUL, 
  )
  val insns_aluFn_FN_CLMULH: Seq[BitPat] = Seq(
    Instructions.CLMULH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_CLMUL.exists(op.bitPat)) aluFn.FN_CLMUL else if (insns_aluFn_FN_CLMULH.exists(op.bitPat)) aluFn.FN_CLMULH else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLMUL, Instructions.CLMULH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.CLMUL, Instructions.CLMULH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLMUL, Instructions.CLMULH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLMUL, Instructions.CLMULH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLMUL, Instructions.CLMULH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLMUL, Instructions.CLMULH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLMUL, Instructions.CLMULH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLMUL, Instructions.CLMULH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.CLMUL, Instructions.CLMULH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.CLMUL, Instructions.CLMULH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLMUL, Instructions.CLMULH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLMUL, Instructions.CLMULH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLMUL, Instructions.CLMULH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CLMUL, Instructions.CLMULH, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

class ZBCRDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_RS2: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_RS2.exists(op.bitPat)) A2_RS2 else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_XPERM8: Seq[BitPat] = Seq(
    Instructions.XPERM8, 
  )
  val insns_aluFn_FN_XPERM4: Seq[BitPat] = Seq(
    Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_XPERM8.exists(op.bitPat)) aluFn.FN_XPERM8 else if (insns_aluFn_FN_XPERM4.exists(op.bitPat)) aluFn.FN_XPERM4 else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

class ZBKXDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_RS2: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_RS2.exists(op.bitPat)) A2_RS2 else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_XPERM8: Seq[BitPat] = Seq(
    Instructions.XPERM8, 
  )
  val insns_aluFn_FN_XPERM4: Seq[BitPat] = Seq(
    Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_XPERM8.exists(op.bitPat)) aluFn.FN_XPERM8 else if (insns_aluFn_FN_XPERM4.exists(op.bitPat)) aluFn.FN_XPERM4 else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.XPERM8, Instructions.XPERM4, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

class ZBSDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.BCLR, Instructions.BEXT, Instructions.BINV, Instructions.BSET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLR, Instructions.BEXT, Instructions.BINV, Instructions.BSET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLR, Instructions.BEXT, Instructions.BINV, Instructions.BSET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLR, Instructions.BEXT, Instructions.BINV, Instructions.BSET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLR, Instructions.BEXT, Instructions.BINV, Instructions.BSET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLR, Instructions.BEXT, Instructions.BINV, Instructions.BSET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.BCLR, Instructions.BEXT, Instructions.BINV, Instructions.BSET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.BCLR, Instructions.BEXT, Instructions.BINV, Instructions.BSET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLR, Instructions.BEXT, Instructions.BINV, Instructions.BSET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLR, Instructions.BEXT, Instructions.BINV, Instructions.BSET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLR, Instructions.BEXT, Instructions.BINV, Instructions.BSET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLR, Instructions.BEXT, Instructions.BINV, Instructions.BSET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_RS2: Seq[BitPat] = Seq(
    Instructions.BCLR, Instructions.BEXT, Instructions.BINV, Instructions.BSET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_RS2.exists(op.bitPat)) A2_RS2 else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.BCLR, Instructions.BEXT, Instructions.BINV, Instructions.BSET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.BCLR, Instructions.BEXT, Instructions.BINV, Instructions.BSET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.BCLR, Instructions.BEXT, Instructions.BINV, Instructions.BSET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_BCLR: Seq[BitPat] = Seq(
    Instructions.BCLR, 
  )
  val insns_aluFn_FN_BEXT: Seq[BitPat] = Seq(
    Instructions.BEXT, 
  )
  val insns_aluFn_FN_BINV: Seq[BitPat] = Seq(
    Instructions.BINV, 
  )
  val insns_aluFn_FN_BSET: Seq[BitPat] = Seq(
    Instructions.BSET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_BCLR.exists(op.bitPat)) aluFn.FN_BCLR else if (insns_aluFn_FN_BEXT.exists(op.bitPat)) aluFn.FN_BEXT else if (insns_aluFn_FN_BINV.exists(op.bitPat)) aluFn.FN_BINV else if (insns_aluFn_FN_BSET.exists(op.bitPat)) aluFn.FN_BSET else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLR, Instructions.BEXT, Instructions.BINV, Instructions.BSET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.BCLR, Instructions.BEXT, Instructions.BINV, Instructions.BSET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLR, Instructions.BEXT, Instructions.BINV, Instructions.BSET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLR, Instructions.BEXT, Instructions.BINV, Instructions.BSET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLR, Instructions.BEXT, Instructions.BINV, Instructions.BSET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLR, Instructions.BEXT, Instructions.BINV, Instructions.BSET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLR, Instructions.BEXT, Instructions.BINV, Instructions.BSET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLR, Instructions.BEXT, Instructions.BINV, Instructions.BSET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.BCLR, Instructions.BEXT, Instructions.BINV, Instructions.BSET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.BCLR, Instructions.BEXT, Instructions.BINV, Instructions.BSET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLR, Instructions.BEXT, Instructions.BINV, Instructions.BSET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLR, Instructions.BEXT, Instructions.BINV, Instructions.BSET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLR, Instructions.BEXT, Instructions.BINV, Instructions.BSET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLR, Instructions.BEXT, Instructions.BINV, Instructions.BSET, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

class ZBS32Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.Instructions32.BCLRI, Instructions.Instructions32.BEXTI, Instructions.Instructions32.BINVI, Instructions.Instructions32.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.BCLRI, Instructions.Instructions32.BEXTI, Instructions.Instructions32.BINVI, Instructions.Instructions32.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.BCLRI, Instructions.Instructions32.BEXTI, Instructions.Instructions32.BINVI, Instructions.Instructions32.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.BCLRI, Instructions.Instructions32.BEXTI, Instructions.Instructions32.BINVI, Instructions.Instructions32.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.BCLRI, Instructions.Instructions32.BEXTI, Instructions.Instructions32.BINVI, Instructions.Instructions32.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.BCLRI, Instructions.Instructions32.BEXTI, Instructions.Instructions32.BINVI, Instructions.Instructions32.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.BCLRI, Instructions.Instructions32.BEXTI, Instructions.Instructions32.BINVI, Instructions.Instructions32.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.Instructions32.BCLRI, Instructions.Instructions32.BEXTI, Instructions.Instructions32.BINVI, Instructions.Instructions32.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.BCLRI, Instructions.Instructions32.BEXTI, Instructions.Instructions32.BINVI, Instructions.Instructions32.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.BCLRI, Instructions.Instructions32.BEXTI, Instructions.Instructions32.BINVI, Instructions.Instructions32.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.BCLRI, Instructions.Instructions32.BEXTI, Instructions.Instructions32.BINVI, Instructions.Instructions32.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.BCLRI, Instructions.Instructions32.BEXTI, Instructions.Instructions32.BINVI, Instructions.Instructions32.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_IMM: Seq[BitPat] = Seq(
    Instructions.Instructions32.BCLRI, Instructions.Instructions32.BEXTI, Instructions.Instructions32.BINVI, Instructions.Instructions32.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_IMM.exists(op.bitPat)) A2_IMM else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.Instructions32.BCLRI, Instructions.Instructions32.BEXTI, Instructions.Instructions32.BINVI, Instructions.Instructions32.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_I: Seq[BitPat] = Seq(
    Instructions.Instructions32.BCLRI, Instructions.Instructions32.BEXTI, Instructions.Instructions32.BINVI, Instructions.Instructions32.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_I.exists(op.bitPat)) IMM_I else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.Instructions32.BCLRI, Instructions.Instructions32.BEXTI, Instructions.Instructions32.BINVI, Instructions.Instructions32.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_BCLR: Seq[BitPat] = Seq(
    Instructions.Instructions32.BCLRI, 
  )
  val insns_aluFn_FN_BEXT: Seq[BitPat] = Seq(
    Instructions.Instructions32.BEXTI, 
  )
  val insns_aluFn_FN_BINV: Seq[BitPat] = Seq(
    Instructions.Instructions32.BINVI, 
  )
  val insns_aluFn_FN_BSET: Seq[BitPat] = Seq(
    Instructions.Instructions32.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_BCLR.exists(op.bitPat)) aluFn.FN_BCLR else if (insns_aluFn_FN_BEXT.exists(op.bitPat)) aluFn.FN_BEXT else if (insns_aluFn_FN_BINV.exists(op.bitPat)) aluFn.FN_BINV else if (insns_aluFn_FN_BSET.exists(op.bitPat)) aluFn.FN_BSET else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.BCLRI, Instructions.Instructions32.BEXTI, Instructions.Instructions32.BINVI, Instructions.Instructions32.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.Instructions32.BCLRI, Instructions.Instructions32.BEXTI, Instructions.Instructions32.BINVI, Instructions.Instructions32.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.BCLRI, Instructions.Instructions32.BEXTI, Instructions.Instructions32.BINVI, Instructions.Instructions32.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.BCLRI, Instructions.Instructions32.BEXTI, Instructions.Instructions32.BINVI, Instructions.Instructions32.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.BCLRI, Instructions.Instructions32.BEXTI, Instructions.Instructions32.BINVI, Instructions.Instructions32.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.BCLRI, Instructions.Instructions32.BEXTI, Instructions.Instructions32.BINVI, Instructions.Instructions32.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.BCLRI, Instructions.Instructions32.BEXTI, Instructions.Instructions32.BINVI, Instructions.Instructions32.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.BCLRI, Instructions.Instructions32.BEXTI, Instructions.Instructions32.BINVI, Instructions.Instructions32.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.Instructions32.BCLRI, Instructions.Instructions32.BEXTI, Instructions.Instructions32.BINVI, Instructions.Instructions32.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.BCLRI, Instructions.Instructions32.BEXTI, Instructions.Instructions32.BINVI, Instructions.Instructions32.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.BCLRI, Instructions.Instructions32.BEXTI, Instructions.Instructions32.BINVI, Instructions.Instructions32.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.BCLRI, Instructions.Instructions32.BEXTI, Instructions.Instructions32.BINVI, Instructions.Instructions32.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.BCLRI, Instructions.Instructions32.BEXTI, Instructions.Instructions32.BINVI, Instructions.Instructions32.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.BCLRI, Instructions.Instructions32.BEXTI, Instructions.Instructions32.BINVI, Instructions.Instructions32.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

class ZBS64Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.BCLRI, Instructions.BEXTI, Instructions.BINVI, Instructions.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLRI, Instructions.BEXTI, Instructions.BINVI, Instructions.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLRI, Instructions.BEXTI, Instructions.BINVI, Instructions.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLRI, Instructions.BEXTI, Instructions.BINVI, Instructions.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLRI, Instructions.BEXTI, Instructions.BINVI, Instructions.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLRI, Instructions.BEXTI, Instructions.BINVI, Instructions.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLRI, Instructions.BEXTI, Instructions.BINVI, Instructions.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.BCLRI, Instructions.BEXTI, Instructions.BINVI, Instructions.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLRI, Instructions.BEXTI, Instructions.BINVI, Instructions.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLRI, Instructions.BEXTI, Instructions.BINVI, Instructions.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLRI, Instructions.BEXTI, Instructions.BINVI, Instructions.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLRI, Instructions.BEXTI, Instructions.BINVI, Instructions.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_IMM: Seq[BitPat] = Seq(
    Instructions.BCLRI, Instructions.BEXTI, Instructions.BINVI, Instructions.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_IMM.exists(op.bitPat)) A2_IMM else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.BCLRI, Instructions.BEXTI, Instructions.BINVI, Instructions.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_I: Seq[BitPat] = Seq(
    Instructions.BCLRI, Instructions.BEXTI, Instructions.BINVI, Instructions.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_I.exists(op.bitPat)) IMM_I else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.BCLRI, Instructions.BEXTI, Instructions.BINVI, Instructions.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_BCLR: Seq[BitPat] = Seq(
    Instructions.BCLRI, 
  )
  val insns_aluFn_FN_BEXT: Seq[BitPat] = Seq(
    Instructions.BEXTI, 
  )
  val insns_aluFn_FN_BINV: Seq[BitPat] = Seq(
    Instructions.BINVI, 
  )
  val insns_aluFn_FN_BSET: Seq[BitPat] = Seq(
    Instructions.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_BCLR.exists(op.bitPat)) aluFn.FN_BCLR else if (insns_aluFn_FN_BEXT.exists(op.bitPat)) aluFn.FN_BEXT else if (insns_aluFn_FN_BINV.exists(op.bitPat)) aluFn.FN_BINV else if (insns_aluFn_FN_BSET.exists(op.bitPat)) aluFn.FN_BSET else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLRI, Instructions.BEXTI, Instructions.BINVI, Instructions.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.BCLRI, Instructions.BEXTI, Instructions.BINVI, Instructions.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLRI, Instructions.BEXTI, Instructions.BINVI, Instructions.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLRI, Instructions.BEXTI, Instructions.BINVI, Instructions.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLRI, Instructions.BEXTI, Instructions.BINVI, Instructions.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLRI, Instructions.BEXTI, Instructions.BINVI, Instructions.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLRI, Instructions.BEXTI, Instructions.BINVI, Instructions.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLRI, Instructions.BEXTI, Instructions.BINVI, Instructions.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.BCLRI, Instructions.BEXTI, Instructions.BINVI, Instructions.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.BCLRI, Instructions.BEXTI, Instructions.BINVI, Instructions.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLRI, Instructions.BEXTI, Instructions.BINVI, Instructions.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLRI, Instructions.BEXTI, Instructions.BINVI, Instructions.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLRI, Instructions.BEXTI, Instructions.BINVI, Instructions.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.BCLRI, Instructions.BEXTI, Instructions.BINVI, Instructions.BSETI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

class ZKND32Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32DSI, Instructions.Instructions32.AES32DSMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32DSI, Instructions.Instructions32.AES32DSMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32DSI, Instructions.Instructions32.AES32DSMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32DSI, Instructions.Instructions32.AES32DSMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32DSI, Instructions.Instructions32.AES32DSMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32DSI, Instructions.Instructions32.AES32DSMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32DSI, Instructions.Instructions32.AES32DSMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32DSI, Instructions.Instructions32.AES32DSMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32DSI, Instructions.Instructions32.AES32DSMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32DSI, Instructions.Instructions32.AES32DSMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32DSI, Instructions.Instructions32.AES32DSMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32DSI, Instructions.Instructions32.AES32DSMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_RS2: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32DSI, Instructions.Instructions32.AES32DSMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_RS2.exists(op.bitPat)) A2_RS2 else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32DSI, Instructions.Instructions32.AES32DSMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32DSI, Instructions.Instructions32.AES32DSMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32DSI, Instructions.Instructions32.AES32DSMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_AES_DS: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32DSI, 
  )
  val insns_aluFn_FN_AES_DSM: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32DSMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_AES_DS.exists(op.bitPat)) aluFn.FN_AES_DS else if (insns_aluFn_FN_AES_DSM.exists(op.bitPat)) aluFn.FN_AES_DSM else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32DSI, Instructions.Instructions32.AES32DSMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32DSI, Instructions.Instructions32.AES32DSMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32DSI, Instructions.Instructions32.AES32DSMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32DSI, Instructions.Instructions32.AES32DSMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32DSI, Instructions.Instructions32.AES32DSMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32DSI, Instructions.Instructions32.AES32DSMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32DSI, Instructions.Instructions32.AES32DSMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32DSI, Instructions.Instructions32.AES32DSMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32DSI, Instructions.Instructions32.AES32DSMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32DSI, Instructions.Instructions32.AES32DSMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32DSI, Instructions.Instructions32.AES32DSMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32DSI, Instructions.Instructions32.AES32DSMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32DSI, Instructions.Instructions32.AES32DSMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32DSI, Instructions.Instructions32.AES32DSMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}
class ZKND64Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.AES64DS, Instructions.AES64DSM, Instructions.AES64IM, Instructions.AES64KS1I, Instructions.AES64KS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64DS, Instructions.AES64DSM, Instructions.AES64IM, Instructions.AES64KS1I, Instructions.AES64KS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64DS, Instructions.AES64DSM, Instructions.AES64IM, Instructions.AES64KS1I, Instructions.AES64KS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64DS, Instructions.AES64DSM, Instructions.AES64IM, Instructions.AES64KS1I, Instructions.AES64KS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64DS, Instructions.AES64DSM, Instructions.AES64IM, Instructions.AES64KS1I, Instructions.AES64KS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64DS, Instructions.AES64DSM, Instructions.AES64IM, Instructions.AES64KS1I, Instructions.AES64KS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.AES64DS, Instructions.AES64DSM, Instructions.AES64IM, Instructions.AES64KS2, 
  )
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64KS1I, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.AES64DS, Instructions.AES64DSM, Instructions.AES64IM, Instructions.AES64KS1I, Instructions.AES64KS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64DS, Instructions.AES64DSM, Instructions.AES64IM, Instructions.AES64KS1I, Instructions.AES64KS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64DS, Instructions.AES64DSM, Instructions.AES64IM, Instructions.AES64KS1I, Instructions.AES64KS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.AES64DS, Instructions.AES64DSM, Instructions.AES64IM, Instructions.AES64KS1I, Instructions.AES64KS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64DS, Instructions.AES64DSM, Instructions.AES64IM, Instructions.AES64KS1I, Instructions.AES64KS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_RS2: Seq[BitPat] = Seq(
    Instructions.AES64DS, Instructions.AES64DSM, Instructions.AES64IM, Instructions.AES64KS2, 
  )
  val insns_A2_IMM: Seq[BitPat] = Seq(
    Instructions.AES64KS1I, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_RS2.exists(op.bitPat)) A2_RS2 else if (insns_A2_IMM.exists(op.bitPat)) A2_IMM else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.AES64DS, Instructions.AES64DSM, Instructions.AES64IM, Instructions.AES64KS1I, Instructions.AES64KS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.AES64DS, Instructions.AES64DSM, Instructions.AES64IM, Instructions.AES64KS2, 
  )
  val insns_IMM_I: Seq[BitPat] = Seq(
    Instructions.AES64KS1I, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else if (insns_IMM_I.exists(op.bitPat)) IMM_I else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.AES64DS, Instructions.AES64DSM, Instructions.AES64IM, Instructions.AES64KS1I, Instructions.AES64KS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_AES_DS: Seq[BitPat] = Seq(
    Instructions.AES64DS, 
  )
  val insns_aluFn_FN_AES_DSM: Seq[BitPat] = Seq(
    Instructions.AES64DSM, 
  )
  val insns_aluFn_FN_AES_IM: Seq[BitPat] = Seq(
    Instructions.AES64IM, 
  )
  val insns_aluFn_FN_AES_KS1: Seq[BitPat] = Seq(
    Instructions.AES64KS1I, 
  )
  val insns_aluFn_FN_AES_KS2: Seq[BitPat] = Seq(
    Instructions.AES64KS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_AES_DS.exists(op.bitPat)) aluFn.FN_AES_DS else if (insns_aluFn_FN_AES_DSM.exists(op.bitPat)) aluFn.FN_AES_DSM else if (insns_aluFn_FN_AES_IM.exists(op.bitPat)) aluFn.FN_AES_IM else if (insns_aluFn_FN_AES_KS1.exists(op.bitPat)) aluFn.FN_AES_KS1 else if (insns_aluFn_FN_AES_KS2.exists(op.bitPat)) aluFn.FN_AES_KS2 else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64DS, Instructions.AES64DSM, Instructions.AES64IM, Instructions.AES64KS1I, Instructions.AES64KS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.AES64DS, Instructions.AES64DSM, Instructions.AES64IM, Instructions.AES64KS1I, Instructions.AES64KS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64DS, Instructions.AES64DSM, Instructions.AES64IM, Instructions.AES64KS1I, Instructions.AES64KS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64DS, Instructions.AES64DSM, Instructions.AES64IM, Instructions.AES64KS1I, Instructions.AES64KS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64DS, Instructions.AES64DSM, Instructions.AES64IM, Instructions.AES64KS1I, Instructions.AES64KS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64DS, Instructions.AES64DSM, Instructions.AES64IM, Instructions.AES64KS1I, Instructions.AES64KS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64DS, Instructions.AES64DSM, Instructions.AES64IM, Instructions.AES64KS1I, Instructions.AES64KS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64DS, Instructions.AES64DSM, Instructions.AES64IM, Instructions.AES64KS1I, Instructions.AES64KS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.AES64DS, Instructions.AES64DSM, Instructions.AES64IM, Instructions.AES64KS1I, Instructions.AES64KS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.AES64DS, Instructions.AES64DSM, Instructions.AES64IM, Instructions.AES64KS1I, Instructions.AES64KS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64DS, Instructions.AES64DSM, Instructions.AES64IM, Instructions.AES64KS1I, Instructions.AES64KS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64DS, Instructions.AES64DSM, Instructions.AES64IM, Instructions.AES64KS1I, Instructions.AES64KS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64DS, Instructions.AES64DSM, Instructions.AES64IM, Instructions.AES64KS1I, Instructions.AES64KS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64DS, Instructions.AES64DSM, Instructions.AES64IM, Instructions.AES64KS1I, Instructions.AES64KS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}
class ZKNE32Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32ESI, Instructions.Instructions32.AES32ESMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32ESI, Instructions.Instructions32.AES32ESMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32ESI, Instructions.Instructions32.AES32ESMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32ESI, Instructions.Instructions32.AES32ESMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32ESI, Instructions.Instructions32.AES32ESMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32ESI, Instructions.Instructions32.AES32ESMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32ESI, Instructions.Instructions32.AES32ESMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32ESI, Instructions.Instructions32.AES32ESMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32ESI, Instructions.Instructions32.AES32ESMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32ESI, Instructions.Instructions32.AES32ESMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32ESI, Instructions.Instructions32.AES32ESMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32ESI, Instructions.Instructions32.AES32ESMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_RS2: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32ESI, Instructions.Instructions32.AES32ESMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_RS2.exists(op.bitPat)) A2_RS2 else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32ESI, Instructions.Instructions32.AES32ESMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32ESI, Instructions.Instructions32.AES32ESMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32ESI, Instructions.Instructions32.AES32ESMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_AES_ES: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32ESI, 
  )
  val insns_aluFn_FN_AES_ESM: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32ESMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_AES_ES.exists(op.bitPat)) aluFn.FN_AES_ES else if (insns_aluFn_FN_AES_ESM.exists(op.bitPat)) aluFn.FN_AES_ESM else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32ESI, Instructions.Instructions32.AES32ESMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32ESI, Instructions.Instructions32.AES32ESMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32ESI, Instructions.Instructions32.AES32ESMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32ESI, Instructions.Instructions32.AES32ESMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32ESI, Instructions.Instructions32.AES32ESMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32ESI, Instructions.Instructions32.AES32ESMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32ESI, Instructions.Instructions32.AES32ESMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32ESI, Instructions.Instructions32.AES32ESMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32ESI, Instructions.Instructions32.AES32ESMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32ESI, Instructions.Instructions32.AES32ESMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32ESI, Instructions.Instructions32.AES32ESMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32ESI, Instructions.Instructions32.AES32ESMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32ESI, Instructions.Instructions32.AES32ESMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.AES32ESI, Instructions.Instructions32.AES32ESMI, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}
class ZKNE64Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.AES64ES, Instructions.AES64ESM, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64ES, Instructions.AES64ESM, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64ES, Instructions.AES64ESM, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64ES, Instructions.AES64ESM, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64ES, Instructions.AES64ESM, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64ES, Instructions.AES64ESM, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.AES64ES, Instructions.AES64ESM, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.AES64ES, Instructions.AES64ESM, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64ES, Instructions.AES64ESM, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64ES, Instructions.AES64ESM, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.AES64ES, Instructions.AES64ESM, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64ES, Instructions.AES64ESM, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_RS2: Seq[BitPat] = Seq(
    Instructions.AES64ES, Instructions.AES64ESM, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_RS2.exists(op.bitPat)) A2_RS2 else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.AES64ES, Instructions.AES64ESM, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.AES64ES, Instructions.AES64ESM, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.AES64ES, Instructions.AES64ESM, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_AES_ES: Seq[BitPat] = Seq(
    Instructions.AES64ES, 
  )
  val insns_aluFn_FN_AES_ESM: Seq[BitPat] = Seq(
    Instructions.AES64ESM, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_AES_ES.exists(op.bitPat)) aluFn.FN_AES_ES else if (insns_aluFn_FN_AES_ESM.exists(op.bitPat)) aluFn.FN_AES_ESM else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64ES, Instructions.AES64ESM, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.AES64ES, Instructions.AES64ESM, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64ES, Instructions.AES64ESM, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64ES, Instructions.AES64ESM, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64ES, Instructions.AES64ESM, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64ES, Instructions.AES64ESM, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64ES, Instructions.AES64ESM, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64ES, Instructions.AES64ESM, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.AES64ES, Instructions.AES64ESM, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.AES64ES, Instructions.AES64ESM, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64ES, Instructions.AES64ESM, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64ES, Instructions.AES64ESM, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64ES, Instructions.AES64ESM, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.AES64ES, Instructions.AES64ESM, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

class ZKNHDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.SHA256SIG0, Instructions.SHA256SIG1, Instructions.SHA256SUM0, Instructions.SHA256SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA256SIG0, Instructions.SHA256SIG1, Instructions.SHA256SUM0, Instructions.SHA256SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA256SIG0, Instructions.SHA256SIG1, Instructions.SHA256SUM0, Instructions.SHA256SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA256SIG0, Instructions.SHA256SIG1, Instructions.SHA256SUM0, Instructions.SHA256SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA256SIG0, Instructions.SHA256SIG1, Instructions.SHA256SUM0, Instructions.SHA256SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA256SIG0, Instructions.SHA256SIG1, Instructions.SHA256SUM0, Instructions.SHA256SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA256SIG0, Instructions.SHA256SIG1, Instructions.SHA256SUM0, Instructions.SHA256SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.SHA256SIG0, Instructions.SHA256SIG1, Instructions.SHA256SUM0, Instructions.SHA256SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA256SIG0, Instructions.SHA256SIG1, Instructions.SHA256SUM0, Instructions.SHA256SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA256SIG0, Instructions.SHA256SIG1, Instructions.SHA256SUM0, Instructions.SHA256SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.SHA256SIG0, Instructions.SHA256SIG1, Instructions.SHA256SUM0, Instructions.SHA256SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA256SIG0, Instructions.SHA256SIG1, Instructions.SHA256SUM0, Instructions.SHA256SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_X: Seq[BitPat] = Seq(
    Instructions.SHA256SIG0, Instructions.SHA256SIG1, Instructions.SHA256SUM0, Instructions.SHA256SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_X.exists(op.bitPat)) A2_X else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.SHA256SIG0, Instructions.SHA256SIG1, Instructions.SHA256SUM0, Instructions.SHA256SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.SHA256SIG0, Instructions.SHA256SIG1, Instructions.SHA256SUM0, Instructions.SHA256SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_X: Seq[BitPat] = Seq(
    Instructions.SHA256SIG0, Instructions.SHA256SIG1, Instructions.SHA256SUM0, Instructions.SHA256SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_X.exists(op.bitPat)) DW_X else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_SHA256_SIG0: Seq[BitPat] = Seq(
    Instructions.SHA256SIG0, 
  )
  val insns_aluFn_FN_SHA256_SIG1: Seq[BitPat] = Seq(
    Instructions.SHA256SIG1, 
  )
  val insns_aluFn_FN_SHA256_SUM0: Seq[BitPat] = Seq(
    Instructions.SHA256SUM0, 
  )
  val insns_aluFn_FN_SHA256_SUM1: Seq[BitPat] = Seq(
    Instructions.SHA256SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_SHA256_SIG0.exists(op.bitPat)) aluFn.FN_SHA256_SIG0 else if (insns_aluFn_FN_SHA256_SIG1.exists(op.bitPat)) aluFn.FN_SHA256_SIG1 else if (insns_aluFn_FN_SHA256_SUM0.exists(op.bitPat)) aluFn.FN_SHA256_SUM0 else if (insns_aluFn_FN_SHA256_SUM1.exists(op.bitPat)) aluFn.FN_SHA256_SUM1 else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA256SIG0, Instructions.SHA256SIG1, Instructions.SHA256SUM0, Instructions.SHA256SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.SHA256SIG0, Instructions.SHA256SIG1, Instructions.SHA256SUM0, Instructions.SHA256SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA256SIG0, Instructions.SHA256SIG1, Instructions.SHA256SUM0, Instructions.SHA256SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA256SIG0, Instructions.SHA256SIG1, Instructions.SHA256SUM0, Instructions.SHA256SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA256SIG0, Instructions.SHA256SIG1, Instructions.SHA256SUM0, Instructions.SHA256SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA256SIG0, Instructions.SHA256SIG1, Instructions.SHA256SUM0, Instructions.SHA256SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA256SIG0, Instructions.SHA256SIG1, Instructions.SHA256SUM0, Instructions.SHA256SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA256SIG0, Instructions.SHA256SIG1, Instructions.SHA256SUM0, Instructions.SHA256SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.SHA256SIG0, Instructions.SHA256SIG1, Instructions.SHA256SUM0, Instructions.SHA256SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.SHA256SIG0, Instructions.SHA256SIG1, Instructions.SHA256SUM0, Instructions.SHA256SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA256SIG0, Instructions.SHA256SIG1, Instructions.SHA256SUM0, Instructions.SHA256SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA256SIG0, Instructions.SHA256SIG1, Instructions.SHA256SUM0, Instructions.SHA256SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA256SIG0, Instructions.SHA256SIG1, Instructions.SHA256SUM0, Instructions.SHA256SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA256SIG0, Instructions.SHA256SIG1, Instructions.SHA256SUM0, Instructions.SHA256SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}
class ZKNH32Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.Instructions32.SHA512SIG0L, Instructions.Instructions32.SHA512SIG1L, Instructions.Instructions32.SHA512SIG0H, Instructions.Instructions32.SHA512SIG1H, Instructions.Instructions32.SHA512SUM0R, Instructions.Instructions32.SHA512SUM1R, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SHA512SIG0L, Instructions.Instructions32.SHA512SIG1L, Instructions.Instructions32.SHA512SIG0H, Instructions.Instructions32.SHA512SIG1H, Instructions.Instructions32.SHA512SUM0R, Instructions.Instructions32.SHA512SUM1R, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SHA512SIG0L, Instructions.Instructions32.SHA512SIG1L, Instructions.Instructions32.SHA512SIG0H, Instructions.Instructions32.SHA512SIG1H, Instructions.Instructions32.SHA512SUM0R, Instructions.Instructions32.SHA512SUM1R, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SHA512SIG0L, Instructions.Instructions32.SHA512SIG1L, Instructions.Instructions32.SHA512SIG0H, Instructions.Instructions32.SHA512SIG1H, Instructions.Instructions32.SHA512SUM0R, Instructions.Instructions32.SHA512SUM1R, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SHA512SIG0L, Instructions.Instructions32.SHA512SIG1L, Instructions.Instructions32.SHA512SIG0H, Instructions.Instructions32.SHA512SIG1H, Instructions.Instructions32.SHA512SUM0R, Instructions.Instructions32.SHA512SUM1R, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SHA512SIG0L, Instructions.Instructions32.SHA512SIG1L, Instructions.Instructions32.SHA512SIG0H, Instructions.Instructions32.SHA512SIG1H, Instructions.Instructions32.SHA512SUM0R, Instructions.Instructions32.SHA512SUM1R, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.Instructions32.SHA512SIG0L, Instructions.Instructions32.SHA512SIG1L, Instructions.Instructions32.SHA512SIG0H, Instructions.Instructions32.SHA512SIG1H, Instructions.Instructions32.SHA512SUM0R, Instructions.Instructions32.SHA512SUM1R, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.Instructions32.SHA512SIG0L, Instructions.Instructions32.SHA512SIG1L, Instructions.Instructions32.SHA512SIG0H, Instructions.Instructions32.SHA512SIG1H, Instructions.Instructions32.SHA512SUM0R, Instructions.Instructions32.SHA512SUM1R, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SHA512SIG0L, Instructions.Instructions32.SHA512SIG1L, Instructions.Instructions32.SHA512SIG0H, Instructions.Instructions32.SHA512SIG1H, Instructions.Instructions32.SHA512SUM0R, Instructions.Instructions32.SHA512SUM1R, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SHA512SIG0L, Instructions.Instructions32.SHA512SIG1L, Instructions.Instructions32.SHA512SIG0H, Instructions.Instructions32.SHA512SIG1H, Instructions.Instructions32.SHA512SUM0R, Instructions.Instructions32.SHA512SUM1R, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.Instructions32.SHA512SIG0L, Instructions.Instructions32.SHA512SIG1L, Instructions.Instructions32.SHA512SIG0H, Instructions.Instructions32.SHA512SIG1H, Instructions.Instructions32.SHA512SUM0R, Instructions.Instructions32.SHA512SUM1R, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SHA512SIG0L, Instructions.Instructions32.SHA512SIG1L, Instructions.Instructions32.SHA512SIG0H, Instructions.Instructions32.SHA512SIG1H, Instructions.Instructions32.SHA512SUM0R, Instructions.Instructions32.SHA512SUM1R, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_RS2: Seq[BitPat] = Seq(
    Instructions.Instructions32.SHA512SIG0L, Instructions.Instructions32.SHA512SIG1L, Instructions.Instructions32.SHA512SIG0H, Instructions.Instructions32.SHA512SIG1H, Instructions.Instructions32.SHA512SUM0R, Instructions.Instructions32.SHA512SUM1R, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_RS2.exists(op.bitPat)) A2_RS2 else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.Instructions32.SHA512SIG0L, Instructions.Instructions32.SHA512SIG1L, Instructions.Instructions32.SHA512SIG0H, Instructions.Instructions32.SHA512SIG1H, Instructions.Instructions32.SHA512SUM0R, Instructions.Instructions32.SHA512SUM1R, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.Instructions32.SHA512SIG0L, Instructions.Instructions32.SHA512SIG1L, Instructions.Instructions32.SHA512SIG0H, Instructions.Instructions32.SHA512SIG1H, Instructions.Instructions32.SHA512SUM0R, Instructions.Instructions32.SHA512SUM1R, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_X: Seq[BitPat] = Seq(
    Instructions.Instructions32.SHA512SIG0L, Instructions.Instructions32.SHA512SIG1L, Instructions.Instructions32.SHA512SIG0H, Instructions.Instructions32.SHA512SIG1H, Instructions.Instructions32.SHA512SUM0R, Instructions.Instructions32.SHA512SUM1R, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_X.exists(op.bitPat)) DW_X else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_SHA512_SIG0: Seq[BitPat] = Seq(
    Instructions.Instructions32.SHA512SIG0L, Instructions.Instructions32.SHA512SIG0H, 
  )
  val insns_aluFn_FN_SHA512_SIG1: Seq[BitPat] = Seq(
    Instructions.Instructions32.SHA512SIG1L, Instructions.Instructions32.SHA512SIG1H, 
  )
  val insns_aluFn_FN_SHA512_SUM0: Seq[BitPat] = Seq(
    Instructions.Instructions32.SHA512SUM0R, 
  )
  val insns_aluFn_FN_SHA512_SUM1: Seq[BitPat] = Seq(
    Instructions.Instructions32.SHA512SUM1R, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_SHA512_SIG0.exists(op.bitPat)) aluFn.FN_SHA512_SIG0 else if (insns_aluFn_FN_SHA512_SIG1.exists(op.bitPat)) aluFn.FN_SHA512_SIG1 else if (insns_aluFn_FN_SHA512_SUM0.exists(op.bitPat)) aluFn.FN_SHA512_SUM0 else if (insns_aluFn_FN_SHA512_SUM1.exists(op.bitPat)) aluFn.FN_SHA512_SUM1 else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SHA512SIG0L, Instructions.Instructions32.SHA512SIG1L, Instructions.Instructions32.SHA512SIG0H, Instructions.Instructions32.SHA512SIG1H, Instructions.Instructions32.SHA512SUM0R, Instructions.Instructions32.SHA512SUM1R, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.Instructions32.SHA512SIG0L, Instructions.Instructions32.SHA512SIG1L, Instructions.Instructions32.SHA512SIG0H, Instructions.Instructions32.SHA512SIG1H, Instructions.Instructions32.SHA512SUM0R, Instructions.Instructions32.SHA512SUM1R, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SHA512SIG0L, Instructions.Instructions32.SHA512SIG1L, Instructions.Instructions32.SHA512SIG0H, Instructions.Instructions32.SHA512SIG1H, Instructions.Instructions32.SHA512SUM0R, Instructions.Instructions32.SHA512SUM1R, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SHA512SIG0L, Instructions.Instructions32.SHA512SIG1L, Instructions.Instructions32.SHA512SIG0H, Instructions.Instructions32.SHA512SIG1H, Instructions.Instructions32.SHA512SUM0R, Instructions.Instructions32.SHA512SUM1R, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SHA512SIG0L, Instructions.Instructions32.SHA512SIG1L, Instructions.Instructions32.SHA512SIG0H, Instructions.Instructions32.SHA512SIG1H, Instructions.Instructions32.SHA512SUM0R, Instructions.Instructions32.SHA512SUM1R, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SHA512SIG0L, Instructions.Instructions32.SHA512SIG1L, Instructions.Instructions32.SHA512SIG0H, Instructions.Instructions32.SHA512SIG1H, Instructions.Instructions32.SHA512SUM0R, Instructions.Instructions32.SHA512SUM1R, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SHA512SIG0L, Instructions.Instructions32.SHA512SIG1L, Instructions.Instructions32.SHA512SIG0H, Instructions.Instructions32.SHA512SIG1H, Instructions.Instructions32.SHA512SUM0R, Instructions.Instructions32.SHA512SUM1R, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SHA512SIG0L, Instructions.Instructions32.SHA512SIG1L, Instructions.Instructions32.SHA512SIG0H, Instructions.Instructions32.SHA512SIG1H, Instructions.Instructions32.SHA512SUM0R, Instructions.Instructions32.SHA512SUM1R, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.Instructions32.SHA512SIG0L, Instructions.Instructions32.SHA512SIG1L, Instructions.Instructions32.SHA512SIG0H, Instructions.Instructions32.SHA512SIG1H, Instructions.Instructions32.SHA512SUM0R, Instructions.Instructions32.SHA512SUM1R, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SHA512SIG0L, Instructions.Instructions32.SHA512SIG1L, Instructions.Instructions32.SHA512SIG0H, Instructions.Instructions32.SHA512SIG1H, Instructions.Instructions32.SHA512SUM0R, Instructions.Instructions32.SHA512SUM1R, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SHA512SIG0L, Instructions.Instructions32.SHA512SIG1L, Instructions.Instructions32.SHA512SIG0H, Instructions.Instructions32.SHA512SIG1H, Instructions.Instructions32.SHA512SUM0R, Instructions.Instructions32.SHA512SUM1R, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SHA512SIG0L, Instructions.Instructions32.SHA512SIG1L, Instructions.Instructions32.SHA512SIG0H, Instructions.Instructions32.SHA512SIG1H, Instructions.Instructions32.SHA512SUM0R, Instructions.Instructions32.SHA512SUM1R, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SHA512SIG0L, Instructions.Instructions32.SHA512SIG1L, Instructions.Instructions32.SHA512SIG0H, Instructions.Instructions32.SHA512SIG1H, Instructions.Instructions32.SHA512SUM0R, Instructions.Instructions32.SHA512SUM1R, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.Instructions32.SHA512SIG0L, Instructions.Instructions32.SHA512SIG1L, Instructions.Instructions32.SHA512SIG0H, Instructions.Instructions32.SHA512SIG1H, Instructions.Instructions32.SHA512SUM0R, Instructions.Instructions32.SHA512SUM1R, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}
class ZKNH64Decode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.SHA512SIG0, Instructions.SHA512SIG1, Instructions.SHA512SUM0, Instructions.SHA512SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA512SIG0, Instructions.SHA512SIG1, Instructions.SHA512SUM0, Instructions.SHA512SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA512SIG0, Instructions.SHA512SIG1, Instructions.SHA512SUM0, Instructions.SHA512SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA512SIG0, Instructions.SHA512SIG1, Instructions.SHA512SUM0, Instructions.SHA512SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA512SIG0, Instructions.SHA512SIG1, Instructions.SHA512SUM0, Instructions.SHA512SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA512SIG0, Instructions.SHA512SIG1, Instructions.SHA512SUM0, Instructions.SHA512SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA512SIG0, Instructions.SHA512SIG1, Instructions.SHA512SUM0, Instructions.SHA512SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.SHA512SIG0, Instructions.SHA512SIG1, Instructions.SHA512SUM0, Instructions.SHA512SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA512SIG0, Instructions.SHA512SIG1, Instructions.SHA512SUM0, Instructions.SHA512SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA512SIG0, Instructions.SHA512SIG1, Instructions.SHA512SUM0, Instructions.SHA512SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.SHA512SIG0, Instructions.SHA512SIG1, Instructions.SHA512SUM0, Instructions.SHA512SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA512SIG0, Instructions.SHA512SIG1, Instructions.SHA512SUM0, Instructions.SHA512SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_X: Seq[BitPat] = Seq(
    Instructions.SHA512SIG0, Instructions.SHA512SIG1, Instructions.SHA512SUM0, Instructions.SHA512SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_X.exists(op.bitPat)) A2_X else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.SHA512SIG0, Instructions.SHA512SIG1, Instructions.SHA512SUM0, Instructions.SHA512SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.SHA512SIG0, Instructions.SHA512SIG1, Instructions.SHA512SUM0, Instructions.SHA512SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_X: Seq[BitPat] = Seq(
    Instructions.SHA512SIG0, Instructions.SHA512SIG1, Instructions.SHA512SUM0, Instructions.SHA512SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_X.exists(op.bitPat)) DW_X else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_SHA512_SIG0: Seq[BitPat] = Seq(
    Instructions.SHA512SIG0, 
  )
  val insns_aluFn_FN_SHA512_SIG1: Seq[BitPat] = Seq(
    Instructions.SHA512SIG1, 
  )
  val insns_aluFn_FN_SHA512_SUM0: Seq[BitPat] = Seq(
    Instructions.SHA512SUM0, 
  )
  val insns_aluFn_FN_SHA512_SUM1: Seq[BitPat] = Seq(
    Instructions.SHA512SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_SHA512_SIG0.exists(op.bitPat)) aluFn.FN_SHA512_SIG0 else if (insns_aluFn_FN_SHA512_SIG1.exists(op.bitPat)) aluFn.FN_SHA512_SIG1 else if (insns_aluFn_FN_SHA512_SUM0.exists(op.bitPat)) aluFn.FN_SHA512_SUM0 else if (insns_aluFn_FN_SHA512_SUM1.exists(op.bitPat)) aluFn.FN_SHA512_SUM1 else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA512SIG0, Instructions.SHA512SIG1, Instructions.SHA512SUM0, Instructions.SHA512SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.SHA512SIG0, Instructions.SHA512SIG1, Instructions.SHA512SUM0, Instructions.SHA512SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA512SIG0, Instructions.SHA512SIG1, Instructions.SHA512SUM0, Instructions.SHA512SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA512SIG0, Instructions.SHA512SIG1, Instructions.SHA512SUM0, Instructions.SHA512SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA512SIG0, Instructions.SHA512SIG1, Instructions.SHA512SUM0, Instructions.SHA512SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA512SIG0, Instructions.SHA512SIG1, Instructions.SHA512SUM0, Instructions.SHA512SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA512SIG0, Instructions.SHA512SIG1, Instructions.SHA512SUM0, Instructions.SHA512SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA512SIG0, Instructions.SHA512SIG1, Instructions.SHA512SUM0, Instructions.SHA512SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.SHA512SIG0, Instructions.SHA512SIG1, Instructions.SHA512SUM0, Instructions.SHA512SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.SHA512SIG0, Instructions.SHA512SIG1, Instructions.SHA512SUM0, Instructions.SHA512SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA512SIG0, Instructions.SHA512SIG1, Instructions.SHA512SUM0, Instructions.SHA512SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA512SIG0, Instructions.SHA512SIG1, Instructions.SHA512SUM0, Instructions.SHA512SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA512SIG0, Instructions.SHA512SIG1, Instructions.SHA512SUM0, Instructions.SHA512SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SHA512SIG0, Instructions.SHA512SIG1, Instructions.SHA512SUM0, Instructions.SHA512SUM1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

class ZKSDecode(implicit val p: Parameters) extends DecodeConstants with UsesABLUFN
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.SM4ED, Instructions.SM4KS, Instructions.SM3P0, Instructions.SM3P1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SM4ED, Instructions.SM4KS, Instructions.SM3P0, Instructions.SM3P1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SM4ED, Instructions.SM4KS, Instructions.SM3P0, Instructions.SM3P1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SM4ED, Instructions.SM4KS, Instructions.SM3P0, Instructions.SM3P1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SM4ED, Instructions.SM4KS, Instructions.SM3P0, Instructions.SM3P1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SM4ED, Instructions.SM4KS, Instructions.SM3P0, Instructions.SM3P1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.SM4ED, Instructions.SM4KS, 
  )
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SM3P0, Instructions.SM3P1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs1 extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.SM4ED, Instructions.SM4KS, Instructions.SM3P0, Instructions.SM3P1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SM4ED, Instructions.SM4KS, Instructions.SM3P0, Instructions.SM3P1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SM4ED, Instructions.SM4KS, Instructions.SM3P0, Instructions.SM3P1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SM4ED, Instructions.SM4KS, Instructions.SM3P0, Instructions.SM3P1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.SM4ED, Instructions.SM4KS, Instructions.SM3P0, Instructions.SM3P1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_RS2: Seq[BitPat] = Seq(
    Instructions.SM4ED, Instructions.SM4KS, 
  )
  val insns_A2_X: Seq[BitPat] = Seq(
    Instructions.SM3P0, Instructions.SM3P1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_RS2.exists(op.bitPat)) A2_RS2 else if (insns_A2_X.exists(op.bitPat)) A2_X else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.SM4ED, Instructions.SM4KS, Instructions.SM3P0, Instructions.SM3P1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.SM4ED, Instructions.SM4KS, Instructions.SM3P0, Instructions.SM3P1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_X: Seq[BitPat] = Seq(
    Instructions.SM4ED, Instructions.SM4KS, Instructions.SM3P0, Instructions.SM3P1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_X.exists(op.bitPat)) DW_X else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_SM4ED: Seq[BitPat] = Seq(
    Instructions.SM4ED, 
  )
  val insns_aluFn_FN_SM4KS: Seq[BitPat] = Seq(
    Instructions.SM4KS, 
  )
  val insns_aluFn_FN_SM3P0: Seq[BitPat] = Seq(
    Instructions.SM3P0, 
  )
  val insns_aluFn_FN_SM3P1: Seq[BitPat] = Seq(
    Instructions.SM3P1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_SM4ED.exists(op.bitPat)) aluFn.FN_SM4ED else if (insns_aluFn_FN_SM4KS.exists(op.bitPat)) aluFn.FN_SM4KS else if (insns_aluFn_FN_SM3P0.exists(op.bitPat)) aluFn.FN_SM3P0 else if (insns_aluFn_FN_SM3P1.exists(op.bitPat)) aluFn.FN_SM3P1 else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SM4ED, Instructions.SM4KS, Instructions.SM3P0, Instructions.SM3P1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.SM4ED, Instructions.SM4KS, Instructions.SM3P0, Instructions.SM3P1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SM4ED, Instructions.SM4KS, Instructions.SM3P0, Instructions.SM3P1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SM4ED, Instructions.SM4KS, Instructions.SM3P0, Instructions.SM3P1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SM4ED, Instructions.SM4KS, Instructions.SM3P0, Instructions.SM3P1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SM4ED, Instructions.SM4KS, Instructions.SM3P0, Instructions.SM3P1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SM4ED, Instructions.SM4KS, Instructions.SM3P0, Instructions.SM3P1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SM4ED, Instructions.SM4KS, Instructions.SM3P0, Instructions.SM3P1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.SM4ED, Instructions.SM4KS, Instructions.SM3P0, Instructions.SM3P1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.SM4ED, Instructions.SM4KS, Instructions.SM3P0, Instructions.SM3P1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SM4ED, Instructions.SM4KS, Instructions.SM3P0, Instructions.SM3P1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SM4ED, Instructions.SM4KS, Instructions.SM3P0, Instructions.SM3P1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SM4ED, Instructions.SM4KS, Instructions.SM3P0, Instructions.SM3P1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.SM4ED, Instructions.SM4KS, Instructions.SM3P0, Instructions.SM3P1, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}

class RoCCDecode(aluFn: ALUFN = ALUFN())(implicit val p: Parameters) extends DecodeConstants
{
object legal extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.CUSTOM0, Instructions.CUSTOM0_RS1, Instructions.CUSTOM0_RS1_RS2, Instructions.CUSTOM0_RD, Instructions.CUSTOM0_RD_RS1, Instructions.CUSTOM0_RD_RS1_RS2, Instructions.CUSTOM1, Instructions.CUSTOM1_RS1, Instructions.CUSTOM1_RS1_RS2, Instructions.CUSTOM1_RD, Instructions.CUSTOM1_RD_RS1, Instructions.CUSTOM1_RD_RS1_RS2, Instructions.CUSTOM2, Instructions.CUSTOM2_RS1, Instructions.CUSTOM2_RS1_RS2, Instructions.CUSTOM2_RD, Instructions.CUSTOM2_RD_RS1, Instructions.CUSTOM2_RD_RS1_RS2, Instructions.CUSTOM3, Instructions.CUSTOM3_RS1, Instructions.CUSTOM3_RS1_RS2, Instructions.CUSTOM3_RD, Instructions.CUSTOM3_RD_RS1, Instructions.CUSTOM3_RD_RS1_RS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object fp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CUSTOM0, Instructions.CUSTOM0_RS1, Instructions.CUSTOM0_RS1_RS2, Instructions.CUSTOM0_RD, Instructions.CUSTOM0_RD_RS1, Instructions.CUSTOM0_RD_RS1_RS2, Instructions.CUSTOM1, Instructions.CUSTOM1_RS1, Instructions.CUSTOM1_RS1_RS2, Instructions.CUSTOM1_RD, Instructions.CUSTOM1_RD_RS1, Instructions.CUSTOM1_RD_RS1_RS2, Instructions.CUSTOM2, Instructions.CUSTOM2_RS1, Instructions.CUSTOM2_RS1_RS2, Instructions.CUSTOM2_RD, Instructions.CUSTOM2_RD_RS1, Instructions.CUSTOM2_RD_RS1_RS2, Instructions.CUSTOM3, Instructions.CUSTOM3_RS1, Instructions.CUSTOM3_RS1_RS2, Instructions.CUSTOM3_RD, Instructions.CUSTOM3_RD_RS1, Instructions.CUSTOM3_RD_RS1_RS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rocc extends BoolField {
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.CUSTOM0, Instructions.CUSTOM0_RS1, Instructions.CUSTOM0_RS1_RS2, Instructions.CUSTOM0_RD, Instructions.CUSTOM0_RD_RS1, Instructions.CUSTOM0_RD_RS1_RS2, Instructions.CUSTOM1, Instructions.CUSTOM1_RS1, Instructions.CUSTOM1_RS1_RS2, Instructions.CUSTOM1_RD, Instructions.CUSTOM1_RD_RS1, Instructions.CUSTOM1_RD_RS1_RS2, Instructions.CUSTOM2, Instructions.CUSTOM2_RS1, Instructions.CUSTOM2_RS1_RS2, Instructions.CUSTOM2_RD, Instructions.CUSTOM2_RD_RS1, Instructions.CUSTOM2_RD_RS1_RS2, Instructions.CUSTOM3, Instructions.CUSTOM3_RS1, Instructions.CUSTOM3_RS1_RS2, Instructions.CUSTOM3_RD, Instructions.CUSTOM3_RD_RS1, Instructions.CUSTOM3_RD_RS1_RS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object branch extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CUSTOM0, Instructions.CUSTOM0_RS1, Instructions.CUSTOM0_RS1_RS2, Instructions.CUSTOM0_RD, Instructions.CUSTOM0_RD_RS1, Instructions.CUSTOM0_RD_RS1_RS2, Instructions.CUSTOM1, Instructions.CUSTOM1_RS1, Instructions.CUSTOM1_RS1_RS2, Instructions.CUSTOM1_RD, Instructions.CUSTOM1_RD_RS1, Instructions.CUSTOM1_RD_RS1_RS2, Instructions.CUSTOM2, Instructions.CUSTOM2_RS1, Instructions.CUSTOM2_RS1_RS2, Instructions.CUSTOM2_RD, Instructions.CUSTOM2_RD_RS1, Instructions.CUSTOM2_RD_RS1_RS2, Instructions.CUSTOM3, Instructions.CUSTOM3_RS1, Instructions.CUSTOM3_RS1_RS2, Instructions.CUSTOM3_RD, Instructions.CUSTOM3_RD_RS1, Instructions.CUSTOM3_RD_RS1_RS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jal extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CUSTOM0, Instructions.CUSTOM0_RS1, Instructions.CUSTOM0_RS1_RS2, Instructions.CUSTOM0_RD, Instructions.CUSTOM0_RD_RS1, Instructions.CUSTOM0_RD_RS1_RS2, Instructions.CUSTOM1, Instructions.CUSTOM1_RS1, Instructions.CUSTOM1_RS1_RS2, Instructions.CUSTOM1_RD, Instructions.CUSTOM1_RD_RS1, Instructions.CUSTOM1_RD_RS1_RS2, Instructions.CUSTOM2, Instructions.CUSTOM2_RS1, Instructions.CUSTOM2_RS1_RS2, Instructions.CUSTOM2_RD, Instructions.CUSTOM2_RD_RS1, Instructions.CUSTOM2_RD_RS1_RS2, Instructions.CUSTOM3, Instructions.CUSTOM3_RS1, Instructions.CUSTOM3_RS1_RS2, Instructions.CUSTOM3_RD, Instructions.CUSTOM3_RD_RS1, Instructions.CUSTOM3_RD_RS1_RS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object jalr extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CUSTOM0, Instructions.CUSTOM0_RS1, Instructions.CUSTOM0_RS1_RS2, Instructions.CUSTOM0_RD, Instructions.CUSTOM0_RD_RS1, Instructions.CUSTOM0_RD_RS1_RS2, Instructions.CUSTOM1, Instructions.CUSTOM1_RS1, Instructions.CUSTOM1_RS1_RS2, Instructions.CUSTOM1_RD, Instructions.CUSTOM1_RD_RS1, Instructions.CUSTOM1_RD_RS1_RS2, Instructions.CUSTOM2, Instructions.CUSTOM2_RS1, Instructions.CUSTOM2_RS1_RS2, Instructions.CUSTOM2_RD, Instructions.CUSTOM2_RD_RS1, Instructions.CUSTOM2_RD_RS1_RS2, Instructions.CUSTOM3, Instructions.CUSTOM3_RS1, Instructions.CUSTOM3_RS1_RS2, Instructions.CUSTOM3_RD, Instructions.CUSTOM3_RD_RS1, Instructions.CUSTOM3_RD_RS1_RS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rxs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CUSTOM0, Instructions.CUSTOM0_RS1, Instructions.CUSTOM0_RD, Instructions.CUSTOM0_RD_RS1, Instructions.CUSTOM1, Instructions.CUSTOM1_RS1, Instructions.CUSTOM1_RD, Instructions.CUSTOM1_RD_RS1, Instructions.CUSTOM2, Instructions.CUSTOM2_RS1, Instructions.CUSTOM2_RD, Instructions.CUSTOM2_RD_RS1, Instructions.CUSTOM3, Instructions.CUSTOM3_RS1, Instructions.CUSTOM3_RD, Instructions.CUSTOM3_RD_RS1, 
  )
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.CUSTOM0_RS1_RS2, Instructions.CUSTOM0_RD_RS1_RS2, Instructions.CUSTOM1_RS1_RS2, Instructions.CUSTOM1_RD_RS1_RS2, Instructions.CUSTOM2_RS1_RS2, Instructions.CUSTOM2_RD_RS1_RS2, Instructions.CUSTOM3_RS1_RS2, Instructions.CUSTOM3_RD_RS1_RS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object rxs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CUSTOM0, Instructions.CUSTOM0_RD, Instructions.CUSTOM1, Instructions.CUSTOM1_RD, Instructions.CUSTOM2, Instructions.CUSTOM2_RD, Instructions.CUSTOM3, Instructions.CUSTOM3_RD, 
  )
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.CUSTOM0_RS1, Instructions.CUSTOM0_RS1_RS2, Instructions.CUSTOM0_RD_RS1, Instructions.CUSTOM0_RD_RS1_RS2, Instructions.CUSTOM1_RS1, Instructions.CUSTOM1_RS1_RS2, Instructions.CUSTOM1_RD_RS1, Instructions.CUSTOM1_RD_RS1_RS2, Instructions.CUSTOM2_RS1, Instructions.CUSTOM2_RS1_RS2, Instructions.CUSTOM2_RD_RS1, Instructions.CUSTOM2_RD_RS1_RS2, Instructions.CUSTOM3_RS1, Instructions.CUSTOM3_RS1_RS2, Instructions.CUSTOM3_RD_RS1, Instructions.CUSTOM3_RD_RS1_RS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object scie extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CUSTOM0, Instructions.CUSTOM0_RS1, Instructions.CUSTOM0_RS1_RS2, Instructions.CUSTOM0_RD, Instructions.CUSTOM0_RD_RS1, Instructions.CUSTOM0_RD_RS1_RS2, Instructions.CUSTOM1, Instructions.CUSTOM1_RS1, Instructions.CUSTOM1_RS1_RS2, Instructions.CUSTOM1_RD, Instructions.CUSTOM1_RD_RS1, Instructions.CUSTOM1_RD_RS1_RS2, Instructions.CUSTOM2, Instructions.CUSTOM2_RS1, Instructions.CUSTOM2_RS1_RS2, Instructions.CUSTOM2_RD, Instructions.CUSTOM2_RD_RS1, Instructions.CUSTOM2_RD_RS1_RS2, Instructions.CUSTOM3, Instructions.CUSTOM3_RS1, Instructions.CUSTOM3_RS1_RS2, Instructions.CUSTOM3_RD, Instructions.CUSTOM3_RD_RS1, Instructions.CUSTOM3_RD_RS1_RS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zbk extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CUSTOM0, Instructions.CUSTOM0_RS1, Instructions.CUSTOM0_RS1_RS2, Instructions.CUSTOM0_RD, Instructions.CUSTOM0_RD_RS1, Instructions.CUSTOM0_RD_RS1_RS2, Instructions.CUSTOM1, Instructions.CUSTOM1_RS1, Instructions.CUSTOM1_RS1_RS2, Instructions.CUSTOM1_RD, Instructions.CUSTOM1_RD_RS1, Instructions.CUSTOM1_RD_RS1_RS2, Instructions.CUSTOM2, Instructions.CUSTOM2_RS1, Instructions.CUSTOM2_RS1_RS2, Instructions.CUSTOM2_RD, Instructions.CUSTOM2_RD_RS1, Instructions.CUSTOM2_RD_RS1_RS2, Instructions.CUSTOM3, Instructions.CUSTOM3_RS1, Instructions.CUSTOM3_RS1_RS2, Instructions.CUSTOM3_RD, Instructions.CUSTOM3_RD_RS1, Instructions.CUSTOM3_RD_RS1_RS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zkn extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CUSTOM0, Instructions.CUSTOM0_RS1, Instructions.CUSTOM0_RS1_RS2, Instructions.CUSTOM0_RD, Instructions.CUSTOM0_RD_RS1, Instructions.CUSTOM0_RD_RS1_RS2, Instructions.CUSTOM1, Instructions.CUSTOM1_RS1, Instructions.CUSTOM1_RS1_RS2, Instructions.CUSTOM1_RD, Instructions.CUSTOM1_RD_RS1, Instructions.CUSTOM1_RD_RS1_RS2, Instructions.CUSTOM2, Instructions.CUSTOM2_RS1, Instructions.CUSTOM2_RS1_RS2, Instructions.CUSTOM2_RD, Instructions.CUSTOM2_RD_RS1, Instructions.CUSTOM2_RD_RS1_RS2, Instructions.CUSTOM3, Instructions.CUSTOM3_RS1, Instructions.CUSTOM3_RS1_RS2, Instructions.CUSTOM3_RD, Instructions.CUSTOM3_RD_RS1, Instructions.CUSTOM3_RD_RS1_RS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object zks extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CUSTOM0, Instructions.CUSTOM0_RS1, Instructions.CUSTOM0_RS1_RS2, Instructions.CUSTOM0_RD, Instructions.CUSTOM0_RD_RS1, Instructions.CUSTOM0_RD_RS1_RS2, Instructions.CUSTOM1, Instructions.CUSTOM1_RS1, Instructions.CUSTOM1_RS1_RS2, Instructions.CUSTOM1_RD, Instructions.CUSTOM1_RD_RS1, Instructions.CUSTOM1_RD_RS1_RS2, Instructions.CUSTOM2, Instructions.CUSTOM2_RS1, Instructions.CUSTOM2_RS1_RS2, Instructions.CUSTOM2_RD, Instructions.CUSTOM2_RD_RS1, Instructions.CUSTOM2_RD_RS1_RS2, Instructions.CUSTOM3, Instructions.CUSTOM3_RS1, Instructions.CUSTOM3_RS1_RS2, Instructions.CUSTOM3_RD, Instructions.CUSTOM3_RD_RS1, Instructions.CUSTOM3_RD_RS1_RS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object sel_alu2 extends BitsField(A2_X.getWidth) {
  val insns_A2_ZERO: Seq[BitPat] = Seq(
    Instructions.CUSTOM0, Instructions.CUSTOM0_RS1, Instructions.CUSTOM0_RS1_RS2, Instructions.CUSTOM0_RD, Instructions.CUSTOM0_RD_RS1, Instructions.CUSTOM0_RD_RS1_RS2, Instructions.CUSTOM1, Instructions.CUSTOM1_RS1, Instructions.CUSTOM1_RS1_RS2, Instructions.CUSTOM1_RD, Instructions.CUSTOM1_RD_RS1, Instructions.CUSTOM1_RD_RS1_RS2, Instructions.CUSTOM2, Instructions.CUSTOM2_RS1, Instructions.CUSTOM2_RS1_RS2, Instructions.CUSTOM2_RD, Instructions.CUSTOM2_RD_RS1, Instructions.CUSTOM2_RD_RS1_RS2, Instructions.CUSTOM3, Instructions.CUSTOM3_RS1, Instructions.CUSTOM3_RS1_RS2, Instructions.CUSTOM3_RD, Instructions.CUSTOM3_RD_RS1, Instructions.CUSTOM3_RD_RS1_RS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A2_ZERO.exists(op.bitPat)) A2_ZERO else A2_X
  }
}
object sel_alu1 extends BitsField(A1_X.getWidth) {
  val insns_A1_RS1: Seq[BitPat] = Seq(
    Instructions.CUSTOM0, Instructions.CUSTOM0_RS1, Instructions.CUSTOM0_RS1_RS2, Instructions.CUSTOM0_RD, Instructions.CUSTOM0_RD_RS1, Instructions.CUSTOM0_RD_RS1_RS2, Instructions.CUSTOM1, Instructions.CUSTOM1_RS1, Instructions.CUSTOM1_RS1_RS2, Instructions.CUSTOM1_RD, Instructions.CUSTOM1_RD_RS1, Instructions.CUSTOM1_RD_RS1_RS2, Instructions.CUSTOM2, Instructions.CUSTOM2_RS1, Instructions.CUSTOM2_RS1_RS2, Instructions.CUSTOM2_RD, Instructions.CUSTOM2_RD_RS1, Instructions.CUSTOM2_RD_RS1_RS2, Instructions.CUSTOM3, Instructions.CUSTOM3_RS1, Instructions.CUSTOM3_RS1_RS2, Instructions.CUSTOM3_RD, Instructions.CUSTOM3_RD_RS1, Instructions.CUSTOM3_RD_RS1_RS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_A1_RS1.exists(op.bitPat)) A1_RS1 else A1_X
  }
}
object sel_imm extends BitsField(IMM_X.getWidth) {
  val insns_IMM_X: Seq[BitPat] = Seq(
    Instructions.CUSTOM0, Instructions.CUSTOM0_RS1, Instructions.CUSTOM0_RS1_RS2, Instructions.CUSTOM0_RD, Instructions.CUSTOM0_RD_RS1, Instructions.CUSTOM0_RD_RS1_RS2, Instructions.CUSTOM1, Instructions.CUSTOM1_RS1, Instructions.CUSTOM1_RS1_RS2, Instructions.CUSTOM1_RD, Instructions.CUSTOM1_RD_RS1, Instructions.CUSTOM1_RD_RS1_RS2, Instructions.CUSTOM2, Instructions.CUSTOM2_RS1, Instructions.CUSTOM2_RS1_RS2, Instructions.CUSTOM2_RD, Instructions.CUSTOM2_RD_RS1, Instructions.CUSTOM2_RD_RS1_RS2, Instructions.CUSTOM3, Instructions.CUSTOM3_RS1, Instructions.CUSTOM3_RS1_RS2, Instructions.CUSTOM3_RD, Instructions.CUSTOM3_RD_RS1, Instructions.CUSTOM3_RD_RS1_RS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_IMM_X.exists(op.bitPat)) IMM_X else IMM_X
  }
}
object alu_dw extends BoolField {
  val insns_DW_XPR: Seq[BitPat] = Seq(
    Instructions.CUSTOM0, Instructions.CUSTOM0_RS1, Instructions.CUSTOM0_RS1_RS2, Instructions.CUSTOM0_RD, Instructions.CUSTOM0_RD_RS1, Instructions.CUSTOM0_RD_RS1_RS2, Instructions.CUSTOM1, Instructions.CUSTOM1_RS1, Instructions.CUSTOM1_RS1_RS2, Instructions.CUSTOM1_RD, Instructions.CUSTOM1_RD_RS1, Instructions.CUSTOM1_RD_RS1_RS2, Instructions.CUSTOM2, Instructions.CUSTOM2_RS1, Instructions.CUSTOM2_RS1_RS2, Instructions.CUSTOM2_RD, Instructions.CUSTOM2_RD_RS1, Instructions.CUSTOM2_RD_RS1_RS2, Instructions.CUSTOM3, Instructions.CUSTOM3_RS1, Instructions.CUSTOM3_RS1_RS2, Instructions.CUSTOM3_RD, Instructions.CUSTOM3_RD_RS1, Instructions.CUSTOM3_RD_RS1_RS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_DW_XPR.exists(op.bitPat)) DW_XPR else DW_X
  }
}
object alu_fn extends BitsField(FN_X.getWidth) {
  val insns_aluFn_FN_ADD: Seq[BitPat] = Seq(
    Instructions.CUSTOM0, Instructions.CUSTOM0_RS1, Instructions.CUSTOM0_RS1_RS2, Instructions.CUSTOM0_RD, Instructions.CUSTOM0_RD_RS1, Instructions.CUSTOM0_RD_RS1_RS2, Instructions.CUSTOM1, Instructions.CUSTOM1_RS1, Instructions.CUSTOM1_RS1_RS2, Instructions.CUSTOM1_RD, Instructions.CUSTOM1_RD_RS1, Instructions.CUSTOM1_RD_RS1_RS2, Instructions.CUSTOM2, Instructions.CUSTOM2_RS1, Instructions.CUSTOM2_RS1_RS2, Instructions.CUSTOM2_RD, Instructions.CUSTOM2_RD_RS1, Instructions.CUSTOM2_RD_RS1_RS2, Instructions.CUSTOM3, Instructions.CUSTOM3_RS1, Instructions.CUSTOM3_RS1_RS2, Instructions.CUSTOM3_RD, Instructions.CUSTOM3_RD_RS1, Instructions.CUSTOM3_RD_RS1_RS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_aluFn_FN_ADD.exists(op.bitPat)) aluFn.FN_ADD else FN_X
  }
}
object mem extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CUSTOM0, Instructions.CUSTOM0_RS1, Instructions.CUSTOM0_RS1_RS2, Instructions.CUSTOM0_RD, Instructions.CUSTOM0_RD_RS1, Instructions.CUSTOM0_RD_RS1_RS2, Instructions.CUSTOM1, Instructions.CUSTOM1_RS1, Instructions.CUSTOM1_RS1_RS2, Instructions.CUSTOM1_RD, Instructions.CUSTOM1_RD_RS1, Instructions.CUSTOM1_RD_RS1_RS2, Instructions.CUSTOM2, Instructions.CUSTOM2_RS1, Instructions.CUSTOM2_RS1_RS2, Instructions.CUSTOM2_RD, Instructions.CUSTOM2_RD_RS1, Instructions.CUSTOM2_RD_RS1_RS2, Instructions.CUSTOM3, Instructions.CUSTOM3_RS1, Instructions.CUSTOM3_RS1_RS2, Instructions.CUSTOM3_RD, Instructions.CUSTOM3_RD_RS1, Instructions.CUSTOM3_RD_RS1_RS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mem_cmd extends BitsField(M_SZ) {
  val insns_M_X: Seq[BitPat] = Seq(
    Instructions.CUSTOM0, Instructions.CUSTOM0_RS1, Instructions.CUSTOM0_RS1_RS2, Instructions.CUSTOM0_RD, Instructions.CUSTOM0_RD_RS1, Instructions.CUSTOM0_RD_RS1_RS2, Instructions.CUSTOM1, Instructions.CUSTOM1_RS1, Instructions.CUSTOM1_RS1_RS2, Instructions.CUSTOM1_RD, Instructions.CUSTOM1_RD_RS1, Instructions.CUSTOM1_RD_RS1_RS2, Instructions.CUSTOM2, Instructions.CUSTOM2_RS1, Instructions.CUSTOM2_RS1_RS2, Instructions.CUSTOM2_RD, Instructions.CUSTOM2_RD_RS1, Instructions.CUSTOM2_RD_RS1_RS2, Instructions.CUSTOM3, Instructions.CUSTOM3_RS1, Instructions.CUSTOM3_RS1_RS2, Instructions.CUSTOM3_RD, Instructions.CUSTOM3_RD_RS1, Instructions.CUSTOM3_RD_RS1_RS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_M_X.exists(op.bitPat)) M_X else M_X
  }
}
object rfs1 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CUSTOM0, Instructions.CUSTOM0_RS1, Instructions.CUSTOM0_RS1_RS2, Instructions.CUSTOM0_RD, Instructions.CUSTOM0_RD_RS1, Instructions.CUSTOM0_RD_RS1_RS2, Instructions.CUSTOM1, Instructions.CUSTOM1_RS1, Instructions.CUSTOM1_RS1_RS2, Instructions.CUSTOM1_RD, Instructions.CUSTOM1_RD_RS1, Instructions.CUSTOM1_RD_RS1_RS2, Instructions.CUSTOM2, Instructions.CUSTOM2_RS1, Instructions.CUSTOM2_RS1_RS2, Instructions.CUSTOM2_RD, Instructions.CUSTOM2_RD_RS1, Instructions.CUSTOM2_RD_RS1_RS2, Instructions.CUSTOM3, Instructions.CUSTOM3_RS1, Instructions.CUSTOM3_RS1_RS2, Instructions.CUSTOM3_RD, Instructions.CUSTOM3_RD_RS1, Instructions.CUSTOM3_RD_RS1_RS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs2 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CUSTOM0, Instructions.CUSTOM0_RS1, Instructions.CUSTOM0_RS1_RS2, Instructions.CUSTOM0_RD, Instructions.CUSTOM0_RD_RS1, Instructions.CUSTOM0_RD_RS1_RS2, Instructions.CUSTOM1, Instructions.CUSTOM1_RS1, Instructions.CUSTOM1_RS1_RS2, Instructions.CUSTOM1_RD, Instructions.CUSTOM1_RD_RS1, Instructions.CUSTOM1_RD_RS1_RS2, Instructions.CUSTOM2, Instructions.CUSTOM2_RS1, Instructions.CUSTOM2_RS1_RS2, Instructions.CUSTOM2_RD, Instructions.CUSTOM2_RD_RS1, Instructions.CUSTOM2_RD_RS1_RS2, Instructions.CUSTOM3, Instructions.CUSTOM3_RS1, Instructions.CUSTOM3_RS1_RS2, Instructions.CUSTOM3_RD, Instructions.CUSTOM3_RD_RS1, Instructions.CUSTOM3_RD_RS1_RS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object rfs3 extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CUSTOM0, Instructions.CUSTOM0_RS1, Instructions.CUSTOM0_RS1_RS2, Instructions.CUSTOM0_RD, Instructions.CUSTOM0_RD_RS1, Instructions.CUSTOM0_RD_RS1_RS2, Instructions.CUSTOM1, Instructions.CUSTOM1_RS1, Instructions.CUSTOM1_RS1_RS2, Instructions.CUSTOM1_RD, Instructions.CUSTOM1_RD_RS1, Instructions.CUSTOM1_RD_RS1_RS2, Instructions.CUSTOM2, Instructions.CUSTOM2_RS1, Instructions.CUSTOM2_RS1_RS2, Instructions.CUSTOM2_RD, Instructions.CUSTOM2_RD_RS1, Instructions.CUSTOM2_RD_RS1_RS2, Instructions.CUSTOM3, Instructions.CUSTOM3_RS1, Instructions.CUSTOM3_RS1_RS2, Instructions.CUSTOM3_RD, Instructions.CUSTOM3_RD_RS1, Instructions.CUSTOM3_RD_RS1_RS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wfd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CUSTOM0, Instructions.CUSTOM0_RS1, Instructions.CUSTOM0_RS1_RS2, Instructions.CUSTOM0_RD, Instructions.CUSTOM0_RD_RS1, Instructions.CUSTOM0_RD_RS1_RS2, Instructions.CUSTOM1, Instructions.CUSTOM1_RS1, Instructions.CUSTOM1_RS1_RS2, Instructions.CUSTOM1_RD, Instructions.CUSTOM1_RD_RS1, Instructions.CUSTOM1_RD_RS1_RS2, Instructions.CUSTOM2, Instructions.CUSTOM2_RS1, Instructions.CUSTOM2_RS1_RS2, Instructions.CUSTOM2_RD, Instructions.CUSTOM2_RD_RS1, Instructions.CUSTOM2_RD_RS1_RS2, Instructions.CUSTOM3, Instructions.CUSTOM3_RS1, Instructions.CUSTOM3_RS1_RS2, Instructions.CUSTOM3_RD, Instructions.CUSTOM3_RD_RS1, Instructions.CUSTOM3_RD_RS1_RS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object mul extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CUSTOM0, Instructions.CUSTOM0_RS1, Instructions.CUSTOM0_RS1_RS2, Instructions.CUSTOM0_RD, Instructions.CUSTOM0_RD_RS1, Instructions.CUSTOM0_RD_RS1_RS2, Instructions.CUSTOM1, Instructions.CUSTOM1_RS1, Instructions.CUSTOM1_RS1_RS2, Instructions.CUSTOM1_RD, Instructions.CUSTOM1_RD_RS1, Instructions.CUSTOM1_RD_RS1_RS2, Instructions.CUSTOM2, Instructions.CUSTOM2_RS1, Instructions.CUSTOM2_RS1_RS2, Instructions.CUSTOM2_RD, Instructions.CUSTOM2_RD_RS1, Instructions.CUSTOM2_RD_RS1_RS2, Instructions.CUSTOM3, Instructions.CUSTOM3_RS1, Instructions.CUSTOM3_RS1_RS2, Instructions.CUSTOM3_RD, Instructions.CUSTOM3_RD_RS1, Instructions.CUSTOM3_RD_RS1_RS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object div extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CUSTOM0, Instructions.CUSTOM0_RS1, Instructions.CUSTOM0_RS1_RS2, Instructions.CUSTOM0_RD, Instructions.CUSTOM0_RD_RS1, Instructions.CUSTOM0_RD_RS1_RS2, Instructions.CUSTOM1, Instructions.CUSTOM1_RS1, Instructions.CUSTOM1_RS1_RS2, Instructions.CUSTOM1_RD, Instructions.CUSTOM1_RD_RS1, Instructions.CUSTOM1_RD_RS1_RS2, Instructions.CUSTOM2, Instructions.CUSTOM2_RS1, Instructions.CUSTOM2_RS1_RS2, Instructions.CUSTOM2_RD, Instructions.CUSTOM2_RD_RS1, Instructions.CUSTOM2_RD_RS1_RS2, Instructions.CUSTOM3, Instructions.CUSTOM3_RS1, Instructions.CUSTOM3_RS1_RS2, Instructions.CUSTOM3_RD, Instructions.CUSTOM3_RD_RS1, Instructions.CUSTOM3_RD_RS1_RS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object wxd extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CUSTOM0, Instructions.CUSTOM0_RS1, Instructions.CUSTOM0_RS1_RS2, Instructions.CUSTOM1, Instructions.CUSTOM1_RS1, Instructions.CUSTOM1_RS1_RS2, Instructions.CUSTOM2, Instructions.CUSTOM2_RS1, Instructions.CUSTOM2_RS1_RS2, Instructions.CUSTOM3, Instructions.CUSTOM3_RS1, Instructions.CUSTOM3_RS1_RS2, 
  )
  val insns_Y: Seq[BitPat] = Seq(
    Instructions.CUSTOM0_RD, Instructions.CUSTOM0_RD_RS1, Instructions.CUSTOM0_RD_RS1_RS2, Instructions.CUSTOM1_RD, Instructions.CUSTOM1_RD_RS1, Instructions.CUSTOM1_RD_RS1_RS2, Instructions.CUSTOM2_RD, Instructions.CUSTOM2_RD_RS1, Instructions.CUSTOM2_RD_RS1_RS2, Instructions.CUSTOM3_RD, Instructions.CUSTOM3_RD_RS1, Instructions.CUSTOM3_RD_RS1_RS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else if (insns_Y.exists(op.bitPat)) Y else N
  }
}
object csr extends BitsField(CSR.SZ) {
  val insns_CSR_N: Seq[BitPat] = Seq(
    Instructions.CUSTOM0, Instructions.CUSTOM0_RS1, Instructions.CUSTOM0_RS1_RS2, Instructions.CUSTOM0_RD, Instructions.CUSTOM0_RD_RS1, Instructions.CUSTOM0_RD_RS1_RS2, Instructions.CUSTOM1, Instructions.CUSTOM1_RS1, Instructions.CUSTOM1_RS1_RS2, Instructions.CUSTOM1_RD, Instructions.CUSTOM1_RD_RS1, Instructions.CUSTOM1_RD_RS1_RS2, Instructions.CUSTOM2, Instructions.CUSTOM2_RS1, Instructions.CUSTOM2_RS1_RS2, Instructions.CUSTOM2_RD, Instructions.CUSTOM2_RD_RS1, Instructions.CUSTOM2_RD_RS1_RS2, Instructions.CUSTOM3, Instructions.CUSTOM3_RS1, Instructions.CUSTOM3_RS1_RS2, Instructions.CUSTOM3_RD, Instructions.CUSTOM3_RD_RS1, Instructions.CUSTOM3_RD_RS1_RS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_CSR_N.exists(op.bitPat)) CSR.N else CSR.N
  }
}
object fence_i extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CUSTOM0, Instructions.CUSTOM0_RS1, Instructions.CUSTOM0_RS1_RS2, Instructions.CUSTOM0_RD, Instructions.CUSTOM0_RD_RS1, Instructions.CUSTOM0_RD_RS1_RS2, Instructions.CUSTOM1, Instructions.CUSTOM1_RS1, Instructions.CUSTOM1_RS1_RS2, Instructions.CUSTOM1_RD, Instructions.CUSTOM1_RD_RS1, Instructions.CUSTOM1_RD_RS1_RS2, Instructions.CUSTOM2, Instructions.CUSTOM2_RS1, Instructions.CUSTOM2_RS1_RS2, Instructions.CUSTOM2_RD, Instructions.CUSTOM2_RD_RS1, Instructions.CUSTOM2_RD_RS1_RS2, Instructions.CUSTOM3, Instructions.CUSTOM3_RS1, Instructions.CUSTOM3_RS1_RS2, Instructions.CUSTOM3_RD, Instructions.CUSTOM3_RD_RS1, Instructions.CUSTOM3_RD_RS1_RS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object fence extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CUSTOM0, Instructions.CUSTOM0_RS1, Instructions.CUSTOM0_RS1_RS2, Instructions.CUSTOM0_RD, Instructions.CUSTOM0_RD_RS1, Instructions.CUSTOM0_RD_RS1_RS2, Instructions.CUSTOM1, Instructions.CUSTOM1_RS1, Instructions.CUSTOM1_RS1_RS2, Instructions.CUSTOM1_RD, Instructions.CUSTOM1_RD_RS1, Instructions.CUSTOM1_RD_RS1_RS2, Instructions.CUSTOM2, Instructions.CUSTOM2_RS1, Instructions.CUSTOM2_RS1_RS2, Instructions.CUSTOM2_RD, Instructions.CUSTOM2_RD_RS1, Instructions.CUSTOM2_RD_RS1_RS2, Instructions.CUSTOM3, Instructions.CUSTOM3_RS1, Instructions.CUSTOM3_RS1_RS2, Instructions.CUSTOM3_RD, Instructions.CUSTOM3_RD_RS1, Instructions.CUSTOM3_RD_RS1_RS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object amo extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CUSTOM0, Instructions.CUSTOM0_RS1, Instructions.CUSTOM0_RS1_RS2, Instructions.CUSTOM0_RD, Instructions.CUSTOM0_RD_RS1, Instructions.CUSTOM0_RD_RS1_RS2, Instructions.CUSTOM1, Instructions.CUSTOM1_RS1, Instructions.CUSTOM1_RS1_RS2, Instructions.CUSTOM1_RD, Instructions.CUSTOM1_RD_RS1, Instructions.CUSTOM1_RD_RS1_RS2, Instructions.CUSTOM2, Instructions.CUSTOM2_RS1, Instructions.CUSTOM2_RS1_RS2, Instructions.CUSTOM2_RD, Instructions.CUSTOM2_RD_RS1, Instructions.CUSTOM2_RD_RS1_RS2, Instructions.CUSTOM3, Instructions.CUSTOM3_RS1, Instructions.CUSTOM3_RS1_RS2, Instructions.CUSTOM3_RD, Instructions.CUSTOM3_RD_RS1, Instructions.CUSTOM3_RD_RS1_RS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}
object dp extends BoolField {
  val insns_N: Seq[BitPat] = Seq(
    Instructions.CUSTOM0, Instructions.CUSTOM0_RS1, Instructions.CUSTOM0_RS1_RS2, Instructions.CUSTOM0_RD, Instructions.CUSTOM0_RD_RS1, Instructions.CUSTOM0_RD_RS1_RS2, Instructions.CUSTOM1, Instructions.CUSTOM1_RS1, Instructions.CUSTOM1_RS1_RS2, Instructions.CUSTOM1_RD, Instructions.CUSTOM1_RD_RS1, Instructions.CUSTOM1_RD_RS1_RS2, Instructions.CUSTOM2, Instructions.CUSTOM2_RS1, Instructions.CUSTOM2_RS1_RS2, Instructions.CUSTOM2_RD, Instructions.CUSTOM2_RD_RS1, Instructions.CUSTOM2_RD_RS1_RS2, Instructions.CUSTOM3, Instructions.CUSTOM3_RS1, Instructions.CUSTOM3_RS1_RS2, Instructions.CUSTOM3_RD, Instructions.CUSTOM3_RD_RS1, Instructions.CUSTOM3_RD_RS1_RS2, 
  )
  def genTable(op: Op): BitPat = {
    if (insns_N.exists(op.bitPat)) N else N
  }
}

}
