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

import chisel3.util.BitPat
import chisel3.util.experimental.decode._

trait FieldName {
  def name: String = this.getClass.getSimpleName.replace("$", "")
}
trait BoolField extends BoolDecodeField[Op] with FieldName
trait BitsField_2 extends DecodeField[Op, UInt] with FieldName {
  def chiselType: UInt = UInt(2.W)
}
trait BitsField_3 extends DecodeField[Op, UInt] with FieldName {
  def chiselType: UInt = UInt(3.W)
}
trait BitsField_4 extends DecodeField[Op, UInt] with FieldName {
  def chiselType: UInt = UInt(4.W)
}
trait BitsField_5 extends DecodeField[Op, UInt] with FieldName {
  def chiselType: UInt = UInt(5.W)
}

case class Op(insn: BitPat) extends DecodePattern {
  def bitPat: BitPat = insn
}

class IntCtrlSigs(pipelinedMul: Boolean, supportsFlushLine: Boolean, flushDCache: Boolean, aluFn: ALUFN = ALUFN(), truthTable: Seq[Op])(implicit val p: Parameters) extends Bundle {
  private val (v, cmd) = if (flushDCache) (Y, BitPat(M_FLUSH_ALL)) else (N, M_X)
  val M = if (pipelinedMul) Y else N
  val D = if (pipelinedMul) N else Y

  object decode_legal extends BoolField {
    val insns_Y: Seq[String] = Seq(
      "SCIE.opcode", "AES64DS", "AES64DSM", "AES64IM", "AES64KS1I", "AES64KS2", "PACKW", "SHA512SIG0", "SHA512SIG1", "SHA512SUM0", "SHA512SUM1", "ROR", "ROL", "HLV_D", "HSV_D", "HLV_WU", "FSGNJ_S", "FSGNJX_S", "FSGNJN_S", "FMIN_S", "FMAX_S", "FADD_S", "FSUB_S", "FMUL_S", "FMADD_S", "FMSUB_S", "FNMADD_S", "FNMSUB_S", "FCLASS_S", "FMV_X_W", "FCVT_W_S", "FCVT_WU_S", "FEQ_S", "FLT_S", "FLE_S", "FMV_W_X", "FCVT_S_W", "FCVT_S_WU", "FLW", "FSW", "FDIV_S", "FSQRT_S", "CLZW", "CTZW", "CPOPW", "SRET", "Instructions32.BCLRI", "Instructions32.BEXTI", "Instructions32.BINVI", "Instructions32.BSETI", "Instructions32.RORI", "SH1ADD", "SH2ADD", "SH3ADD", "ORC_B", "CUSTOM0", "CUSTOM0_RS1", "CUSTOM0_RS1_RS2", "CUSTOM0_RD", "CUSTOM0_RD_RS1", "CUSTOM0_RD_RS1_RS2", "CUSTOM1", "CUSTOM1_RS1", "CUSTOM1_RS1_RS2", "CUSTOM1_RD", "CUSTOM1_RD_RS1", "CUSTOM1_RD_RS1_RS2", "CUSTOM2", "CUSTOM2_RS1", "CUSTOM2_RS1_RS2", "CUSTOM2_RD", "CUSTOM2_RD_RS1", "CUSTOM2_RD_RS1_RS2", "CUSTOM3", "CUSTOM3_RS1", "CUSTOM3_RS1_RS2", "CUSTOM3_RD", "CUSTOM3_RD_RS1", "CUSTOM3_RD_RS1_RS2", "Instructions32.AES32ESI", "Instructions32.AES32ESMI", "FCVT_S_D", "FCVT_D_S", "FSGNJ_D", "FSGNJX_D", "FSGNJN_D", "FMIN_D", "FMAX_D", "FADD_D", "FSUB_D", "FMUL_D", "FMADD_D", "FMSUB_D", "FNMADD_D", "FNMSUB_D", "FCLASS_D", "FCVT_W_D", "FCVT_WU_D", "FEQ_D", "FLT_D", "FLE_D", "FCVT_D_W", "FCVT_D_WU", "FLD", "FSD", "FDIV_D", "FSQRT_D", "Instructions32.ZIP", "Instructions32.UNZIP", "FCVT_L_S", "FCVT_LU_S", "FCVT_S_L", "FCVT_S_LU", "Instructions32.AES32DSI", "Instructions32.AES32DSMI", "Instructions32.SHA512SIG0L", "Instructions32.SHA512SIG1L", "Instructions32.SHA512SIG0H", "Instructions32.SHA512SIG1H", "Instructions32.SHA512SUM0R", "Instructions32.SHA512SUM1R", "FCVT_S_H", "FCVT_H_S", "FSGNJ_H", "FSGNJX_H", "FSGNJN_H", "FMIN_H", "FMAX_H", "FADD_H", "FSUB_H", "FMUL_H", "FMADD_H", "FMSUB_H", "FNMADD_H", "FNMSUB_H", "FCLASS_H", "FMV_X_H", "FCVT_W_H", "FCVT_WU_H", "FEQ_H", "FLT_H", "FLE_H", "FMV_H_X", "FCVT_H_W", "FCVT_H_WU", "FLH", "FSH", "FDIV_H", "FSQRT_H", "AES64ES", "AES64ESM", "CLMUL", "CLMULH", "BCLRI", "BEXTI", "BINVI", "BSETI", "RORI", "RORW", "ROLW", "RORIW", "FCVT_D_H", "FCVT_H_D", "CFLUSH_D_L1", "CDISCARD_D_L1", "BNE", "BEQ", "BLT", "BLTU", "BGE", "BGEU", "JAL", "JALR", "AUIPC", "LB", "LH", "LW", "LBU", "LHU", "SB", "SH", "SW", "LUI", "ADDI", "SLTI", "SLTIU", "ANDI", "ORI", "XORI", "ADD", "SUB", "SLT", "SLTU", "AND", "OR", "XOR", "SLL", "SRL", "SRA", "FENCE", "ECALL", "EBREAK", "MRET", "WFI", "CEASE", "CSRRW", "CSRRS", "CSRRC", "CSRRWI", "CSRRSI", "CSRRCI", "SEXT_H", "SEXT_B", "XPERM8", "XPERM4", "FMV_X_D", "FCVT_L_D", "FCVT_LU_D", "FMV_D_X", "FCVT_D_L", "FCVT_D_LU", "ZEXT_H", "MNRET", "FCVT_L_H", "FCVT_LU_H", "FCVT_H_L", "FCVT_H_LU", "FENCE_I", "ANDN", "ORN", "XNOR", "Instructions32.SLLI", "Instructions32.SRLI", "Instructions32.SRAI", "SM4ED", "SM4KS", "SM3P0", "SM3P1", "DRET", "CLZ", "CTZ", "CPOP", "REV8", "ADD_UW", "SLLI_UW", "SH1ADD_UW", "SH2ADD_UW", "SH3ADD_UW", "SFENCE_VMA", "HFENCE_VVMA", "HFENCE_GVMA", "HLV_B", "HLV_BU", "HLV_H", "HLV_HU", "HLVX_HU", "HLV_W", "HLVX_WU", "HSV_B", "HSV_H", "HSV_W", "Instructions32.ZEXT_H", "CLMULR", "PACK", "PACKH", "BREV8", "SHA256SIG0", "SHA256SIG1", "SHA256SUM0", "SHA256SUM1", "BCLR", "BEXT", "BINV", "BSET", "Instructions32.REV8", "LD", "LWU", "SD", "SLLI", "SRLI", "SRAI", "ADDIW", "SLLIW", "SRLIW", "SRAIW", "ADDW", "SUBW", "SLLW", "SRLW", "SRAW", "MULW", "DIVW", "DIVUW", "REMW", "REMUW", "AMOADD_D", "AMOSWAP_D", "AMOXOR_D", "AMOAND_D", "AMOOR_D", "AMOMIN_D", "AMOMINU_D", "AMOMAX_D", "AMOMAXU_D", "LR_D", "SC_D", "MUL", "MULH", "MULHU", "MULHSU", "DIV", "DIVU", "REM", "REMU", "AMOADD_W", "AMOXOR_W", "AMOSWAP_W", "AMOAND_W", "AMOOR_W", "AMOMIN_W", "AMOMINU_W", "AMOMAX_W", "AMOMAXU_W", "LR_W", "SC_W", "MAX", "MAXU", "MIN", "MINU", 
    )
    def genTable(op: Op): BitPat = {
      if (insns_Y.contains(op.bitPat)) Y else N
    }
  }
  object decode_fp extends BoolField {
    val insns_N: Seq[String] = Seq(
      "SCIE.opcode", "AES64DS", "AES64DSM", "AES64IM", "AES64KS1I", "AES64KS2", "PACKW", "SHA512SIG0", "SHA512SIG1", "SHA512SUM0", "SHA512SUM1", "ROR", "ROL", "HLV_D", "HSV_D", "HLV_WU", "CLZW", "CTZW", "CPOPW", "SRET", "Instructions32.BCLRI", "Instructions32.BEXTI", "Instructions32.BINVI", "Instructions32.BSETI", "Instructions32.RORI", "SH1ADD", "SH2ADD", "SH3ADD", "ORC_B", "CUSTOM0", "CUSTOM0_RS1", "CUSTOM0_RS1_RS2", "CUSTOM0_RD", "CUSTOM0_RD_RS1", "CUSTOM0_RD_RS1_RS2", "CUSTOM1", "CUSTOM1_RS1", "CUSTOM1_RS1_RS2", "CUSTOM1_RD", "CUSTOM1_RD_RS1", "CUSTOM1_RD_RS1_RS2", "CUSTOM2", "CUSTOM2_RS1", "CUSTOM2_RS1_RS2", "CUSTOM2_RD", "CUSTOM2_RD_RS1", "CUSTOM2_RD_RS1_RS2", "CUSTOM3", "CUSTOM3_RS1", "CUSTOM3_RS1_RS2", "CUSTOM3_RD", "CUSTOM3_RD_RS1", "CUSTOM3_RD_RS1_RS2", "Instructions32.AES32ESI", "Instructions32.AES32ESMI", "Instructions32.ZIP", "Instructions32.UNZIP", "Instructions32.AES32DSI", "Instructions32.AES32DSMI", "Instructions32.SHA512SIG0L", "Instructions32.SHA512SIG1L", "Instructions32.SHA512SIG0H", "Instructions32.SHA512SIG1H", "Instructions32.SHA512SUM0R", "Instructions32.SHA512SUM1R", "AES64ES", "AES64ESM", "CLMUL", "CLMULH", "BCLRI", "BEXTI", "BINVI", "BSETI", "RORI", "RORW", "ROLW", "RORIW", "CFLUSH_D_L1", "CDISCARD_D_L1", "BNE", "BEQ", "BLT", "BLTU", "BGE", "BGEU", "JAL", "JALR", "AUIPC", "LB", "LH", "LW", "LBU", "LHU", "SB", "SH", "SW", "LUI", "ADDI", "SLTI", "SLTIU", "ANDI", "ORI", "XORI", "ADD", "SUB", "SLT", "SLTU", "AND", "OR", "XOR", "SLL", "SRL", "SRA", "FENCE", "ECALL", "EBREAK", "MRET", "WFI", "CEASE", "CSRRW", "CSRRS", "CSRRC", "CSRRWI", "CSRRSI", "CSRRCI", "SEXT_H", "SEXT_B", "XPERM8", "XPERM4", "ZEXT_H", "MNRET", "FENCE_I", "ANDN", "ORN", "XNOR", "Instructions32.SLLI", "Instructions32.SRLI", "Instructions32.SRAI", "SM4ED", "SM4KS", "SM3P0", "SM3P1", "DRET", "CLZ", "CTZ", "CPOP", "REV8", "ADD_UW", "SLLI_UW", "SH1ADD_UW", "SH2ADD_UW", "SH3ADD_UW", "SFENCE_VMA", "HFENCE_VVMA", "HFENCE_GVMA", "HLV_B", "HLV_BU", "HLV_H", "HLV_HU", "HLVX_HU", "HLV_W", "HLVX_WU", "HSV_B", "HSV_H", "HSV_W", "Instructions32.ZEXT_H", "CLMULR", "PACK", "PACKH", "BREV8", "SHA256SIG0", "SHA256SIG1", "SHA256SUM0", "SHA256SUM1", "BCLR", "BEXT", "BINV", "BSET", "Instructions32.REV8", "LD", "LWU", "SD", "SLLI", "SRLI", "SRAI", "ADDIW", "SLLIW", "SRLIW", "SRAIW", "ADDW", "SUBW", "SLLW", "SRLW", "SRAW", "MULW", "DIVW", "DIVUW", "REMW", "REMUW", "AMOADD_D", "AMOSWAP_D", "AMOXOR_D", "AMOAND_D", "AMOOR_D", "AMOMIN_D", "AMOMINU_D", "AMOMAX_D", "AMOMAXU_D", "LR_D", "SC_D", "MUL", "MULH", "MULHU", "MULHSU", "DIV", "DIVU", "REM", "REMU", "AMOADD_W", "AMOXOR_W", "AMOSWAP_W", "AMOAND_W", "AMOOR_W", "AMOMIN_W", "AMOMINU_W", "AMOMAX_W", "AMOMAXU_W", "LR_W", "SC_W", "MAX", "MAXU", "MIN", "MINU", 
    )
    val insns_Y: Seq[String] = Seq(
      "FSGNJ_S", "FSGNJX_S", "FSGNJN_S", "FMIN_S", "FMAX_S", "FADD_S", "FSUB_S", "FMUL_S", "FMADD_S", "FMSUB_S", "FNMADD_S", "FNMSUB_S", "FCLASS_S", "FMV_X_W", "FCVT_W_S", "FCVT_WU_S", "FEQ_S", "FLT_S", "FLE_S", "FMV_W_X", "FCVT_S_W", "FCVT_S_WU", "FLW", "FSW", "FDIV_S", "FSQRT_S", "FCVT_S_D", "FCVT_D_S", "FSGNJ_D", "FSGNJX_D", "FSGNJN_D", "FMIN_D", "FMAX_D", "FADD_D", "FSUB_D", "FMUL_D", "FMADD_D", "FMSUB_D", "FNMADD_D", "FNMSUB_D", "FCLASS_D", "FCVT_W_D", "FCVT_WU_D", "FEQ_D", "FLT_D", "FLE_D", "FCVT_D_W", "FCVT_D_WU", "FLD", "FSD", "FDIV_D", "FSQRT_D", "FCVT_L_S", "FCVT_LU_S", "FCVT_S_L", "FCVT_S_LU", "FCVT_S_H", "FCVT_H_S", "FSGNJ_H", "FSGNJX_H", "FSGNJN_H", "FMIN_H", "FMAX_H", "FADD_H", "FSUB_H", "FMUL_H", "FMADD_H", "FMSUB_H", "FNMADD_H", "FNMSUB_H", "FCLASS_H", "FMV_X_H", "FCVT_W_H", "FCVT_WU_H", "FEQ_H", "FLT_H", "FLE_H", "FMV_H_X", "FCVT_H_W", "FCVT_H_WU", "FLH", "FSH", "FDIV_H", "FSQRT_H", "FCVT_D_H", "FCVT_H_D", "FMV_X_D", "FCVT_L_D", "FCVT_LU_D", "FMV_D_X", "FCVT_D_L", "FCVT_D_LU", "FCVT_L_H", "FCVT_LU_H", "FCVT_H_L", "FCVT_H_LU", 
    )
    def genTable(op: Op): BitPat = {
      if (insns_N.contains(op.bitPat)) N else if (insns_Y.contains(op.bitPat)) Y else N
    }
  }
  object decode_rocc extends BoolField {
    val insns_N: Seq[String] = Seq(
      "SCIE.opcode", "AES64DS", "AES64DSM", "AES64IM", "AES64KS1I", "AES64KS2", "PACKW", "SHA512SIG0", "SHA512SIG1", "SHA512SUM0", "SHA512SUM1", "ROR", "ROL", "HLV_D", "HSV_D", "HLV_WU", "FSGNJ_S", "FSGNJX_S", "FSGNJN_S", "FMIN_S", "FMAX_S", "FADD_S", "FSUB_S", "FMUL_S", "FMADD_S", "FMSUB_S", "FNMADD_S", "FNMSUB_S", "FCLASS_S", "FMV_X_W", "FCVT_W_S", "FCVT_WU_S", "FEQ_S", "FLT_S", "FLE_S", "FMV_W_X", "FCVT_S_W", "FCVT_S_WU", "FLW", "FSW", "FDIV_S", "FSQRT_S", "CLZW", "CTZW", "CPOPW", "SRET", "Instructions32.BCLRI", "Instructions32.BEXTI", "Instructions32.BINVI", "Instructions32.BSETI", "Instructions32.RORI", "SH1ADD", "SH2ADD", "SH3ADD", "ORC_B", "Instructions32.AES32ESI", "Instructions32.AES32ESMI", "FCVT_S_D", "FCVT_D_S", "FSGNJ_D", "FSGNJX_D", "FSGNJN_D", "FMIN_D", "FMAX_D", "FADD_D", "FSUB_D", "FMUL_D", "FMADD_D", "FMSUB_D", "FNMADD_D", "FNMSUB_D", "FCLASS_D", "FCVT_W_D", "FCVT_WU_D", "FEQ_D", "FLT_D", "FLE_D", "FCVT_D_W", "FCVT_D_WU", "FLD", "FSD", "FDIV_D", "FSQRT_D", "Instructions32.ZIP", "Instructions32.UNZIP", "FCVT_L_S", "FCVT_LU_S", "FCVT_S_L", "FCVT_S_LU", "Instructions32.AES32DSI", "Instructions32.AES32DSMI", "Instructions32.SHA512SIG0L", "Instructions32.SHA512SIG1L", "Instructions32.SHA512SIG0H", "Instructions32.SHA512SIG1H", "Instructions32.SHA512SUM0R", "Instructions32.SHA512SUM1R", "FCVT_S_H", "FCVT_H_S", "FSGNJ_H", "FSGNJX_H", "FSGNJN_H", "FMIN_H", "FMAX_H", "FADD_H", "FSUB_H", "FMUL_H", "FMADD_H", "FMSUB_H", "FNMADD_H", "FNMSUB_H", "FCLASS_H", "FMV_X_H", "FCVT_W_H", "FCVT_WU_H", "FEQ_H", "FLT_H", "FLE_H", "FMV_H_X", "FCVT_H_W", "FCVT_H_WU", "FLH", "FSH", "FDIV_H", "FSQRT_H", "AES64ES", "AES64ESM", "CLMUL", "CLMULH", "BCLRI", "BEXTI", "BINVI", "BSETI", "RORI", "RORW", "ROLW", "RORIW", "FCVT_D_H", "FCVT_H_D", "CFLUSH_D_L1", "CDISCARD_D_L1", "BNE", "BEQ", "BLT", "BLTU", "BGE", "BGEU", "JAL", "JALR", "AUIPC", "LB", "LH", "LW", "LBU", "LHU", "SB", "SH", "SW", "LUI", "ADDI", "SLTI", "SLTIU", "ANDI", "ORI", "XORI", "ADD", "SUB", "SLT", "SLTU", "AND", "OR", "XOR", "SLL", "SRL", "SRA", "FENCE", "ECALL", "EBREAK", "MRET", "WFI", "CEASE", "CSRRW", "CSRRS", "CSRRC", "CSRRWI", "CSRRSI", "CSRRCI", "SEXT_H", "SEXT_B", "XPERM8", "XPERM4", "FMV_X_D", "FCVT_L_D", "FCVT_LU_D", "FMV_D_X", "FCVT_D_L", "FCVT_D_LU", "ZEXT_H", "MNRET", "FCVT_L_H", "FCVT_LU_H", "FCVT_H_L", "FCVT_H_LU", "FENCE_I", "ANDN", "ORN", "XNOR", "Instructions32.SLLI", "Instructions32.SRLI", "Instructions32.SRAI", "SM4ED", "SM4KS", "SM3P0", "SM3P1", "DRET", "CLZ", "CTZ", "CPOP", "REV8", "ADD_UW", "SLLI_UW", "SH1ADD_UW", "SH2ADD_UW", "SH3ADD_UW", "SFENCE_VMA", "HFENCE_VVMA", "HFENCE_GVMA", "HLV_B", "HLV_BU", "HLV_H", "HLV_HU", "HLVX_HU", "HLV_W", "HLVX_WU", "HSV_B", "HSV_H", "HSV_W", "Instructions32.ZEXT_H", "CLMULR", "PACK", "PACKH", "BREV8", "SHA256SIG0", "SHA256SIG1", "SHA256SUM0", "SHA256SUM1", "BCLR", "BEXT", "BINV", "BSET", "Instructions32.REV8", "LD", "LWU", "SD", "SLLI", "SRLI", "SRAI", "ADDIW", "SLLIW", "SRLIW", "SRAIW", "ADDW", "SUBW", "SLLW", "SRLW", "SRAW", "MULW", "DIVW", "DIVUW", "REMW", "REMUW", "AMOADD_D", "AMOSWAP_D", "AMOXOR_D", "AMOAND_D", "AMOOR_D", "AMOMIN_D", "AMOMINU_D", "AMOMAX_D", "AMOMAXU_D", "LR_D", "SC_D", "MUL", "MULH", "MULHU", "MULHSU", "DIV", "DIVU", "REM", "REMU", "AMOADD_W", "AMOXOR_W", "AMOSWAP_W", "AMOAND_W", "AMOOR_W", "AMOMIN_W", "AMOMINU_W", "AMOMAX_W", "AMOMAXU_W", "LR_W", "SC_W", "MAX", "MAXU", "MIN", "MINU", 
    )
    val insns_Y: Seq[String] = Seq(
      "CUSTOM0", "CUSTOM0_RS1", "CUSTOM0_RS1_RS2", "CUSTOM0_RD", "CUSTOM0_RD_RS1", "CUSTOM0_RD_RS1_RS2", "CUSTOM1", "CUSTOM1_RS1", "CUSTOM1_RS1_RS2", "CUSTOM1_RD", "CUSTOM1_RD_RS1", "CUSTOM1_RD_RS1_RS2", "CUSTOM2", "CUSTOM2_RS1", "CUSTOM2_RS1_RS2", "CUSTOM2_RD", "CUSTOM2_RD_RS1", "CUSTOM2_RD_RS1_RS2", "CUSTOM3", "CUSTOM3_RS1", "CUSTOM3_RS1_RS2", "CUSTOM3_RD", "CUSTOM3_RD_RS1", "CUSTOM3_RD_RS1_RS2", 
    )
    def genTable(op: Op): BitPat = {
      if (insns_N.contains(op.bitPat)) N else if (insns_Y.contains(op.bitPat)) Y else N
    }
  }
  object decode_branch extends BoolField {
    val insns_N: Seq[String] = Seq(
      "SCIE.opcode", "AES64DS", "AES64DSM", "AES64IM", "AES64KS1I", "AES64KS2", "PACKW", "SHA512SIG0", "SHA512SIG1", "SHA512SUM0", "SHA512SUM1", "ROR", "ROL", "HLV_D", "HSV_D", "HLV_WU", "FSGNJ_S", "FSGNJX_S", "FSGNJN_S", "FMIN_S", "FMAX_S", "FADD_S", "FSUB_S", "FMUL_S", "FMADD_S", "FMSUB_S", "FNMADD_S", "FNMSUB_S", "FCLASS_S", "FMV_X_W", "FCVT_W_S", "FCVT_WU_S", "FEQ_S", "FLT_S", "FLE_S", "FMV_W_X", "FCVT_S_W", "FCVT_S_WU", "FLW", "FSW", "FDIV_S", "FSQRT_S", "CLZW", "CTZW", "CPOPW", "SRET", "Instructions32.BCLRI", "Instructions32.BEXTI", "Instructions32.BINVI", "Instructions32.BSETI", "Instructions32.RORI", "SH1ADD", "SH2ADD", "SH3ADD", "ORC_B", "CUSTOM0", "CUSTOM0_RS1", "CUSTOM0_RS1_RS2", "CUSTOM0_RD", "CUSTOM0_RD_RS1", "CUSTOM0_RD_RS1_RS2", "CUSTOM1", "CUSTOM1_RS1", "CUSTOM1_RS1_RS2", "CUSTOM1_RD", "CUSTOM1_RD_RS1", "CUSTOM1_RD_RS1_RS2", "CUSTOM2", "CUSTOM2_RS1", "CUSTOM2_RS1_RS2", "CUSTOM2_RD", "CUSTOM2_RD_RS1", "CUSTOM2_RD_RS1_RS2", "CUSTOM3", "CUSTOM3_RS1", "CUSTOM3_RS1_RS2", "CUSTOM3_RD", "CUSTOM3_RD_RS1", "CUSTOM3_RD_RS1_RS2", "Instructions32.AES32ESI", "Instructions32.AES32ESMI", "FCVT_S_D", "FCVT_D_S", "FSGNJ_D", "FSGNJX_D", "FSGNJN_D", "FMIN_D", "FMAX_D", "FADD_D", "FSUB_D", "FMUL_D", "FMADD_D", "FMSUB_D", "FNMADD_D", "FNMSUB_D", "FCLASS_D", "FCVT_W_D", "FCVT_WU_D", "FEQ_D", "FLT_D", "FLE_D", "FCVT_D_W", "FCVT_D_WU", "FLD", "FSD", "FDIV_D", "FSQRT_D", "Instructions32.ZIP", "Instructions32.UNZIP", "FCVT_L_S", "FCVT_LU_S", "FCVT_S_L", "FCVT_S_LU", "Instructions32.AES32DSI", "Instructions32.AES32DSMI", "Instructions32.SHA512SIG0L", "Instructions32.SHA512SIG1L", "Instructions32.SHA512SIG0H", "Instructions32.SHA512SIG1H", "Instructions32.SHA512SUM0R", "Instructions32.SHA512SUM1R", "FCVT_S_H", "FCVT_H_S", "FSGNJ_H", "FSGNJX_H", "FSGNJN_H", "FMIN_H", "FMAX_H", "FADD_H", "FSUB_H", "FMUL_H", "FMADD_H", "FMSUB_H", "FNMADD_H", "FNMSUB_H", "FCLASS_H", "FMV_X_H", "FCVT_W_H", "FCVT_WU_H", "FEQ_H", "FLT_H", "FLE_H", "FMV_H_X", "FCVT_H_W", "FCVT_H_WU", "FLH", "FSH", "FDIV_H", "FSQRT_H", "AES64ES", "AES64ESM", "CLMUL", "CLMULH", "BCLRI", "BEXTI", "BINVI", "BSETI", "RORI", "RORW", "ROLW", "RORIW", "FCVT_D_H", "FCVT_H_D", "CFLUSH_D_L1", "CDISCARD_D_L1", "JAL", "JALR", "AUIPC", "LB", "LH", "LW", "LBU", "LHU", "SB", "SH", "SW", "LUI", "ADDI", "SLTI", "SLTIU", "ANDI", "ORI", "XORI", "ADD", "SUB", "SLT", "SLTU", "AND", "OR", "XOR", "SLL", "SRL", "SRA", "FENCE", "ECALL", "EBREAK", "MRET", "WFI", "CEASE", "CSRRW", "CSRRS", "CSRRC", "CSRRWI", "CSRRSI", "CSRRCI", "SEXT_H", "SEXT_B", "XPERM8", "XPERM4", "FMV_X_D", "FCVT_L_D", "FCVT_LU_D", "FMV_D_X", "FCVT_D_L", "FCVT_D_LU", "ZEXT_H", "MNRET", "FCVT_L_H", "FCVT_LU_H", "FCVT_H_L", "FCVT_H_LU", "FENCE_I", "ANDN", "ORN", "XNOR", "Instructions32.SLLI", "Instructions32.SRLI", "Instructions32.SRAI", "SM4ED", "SM4KS", "SM3P0", "SM3P1", "DRET", "CLZ", "CTZ", "CPOP", "REV8", "ADD_UW", "SLLI_UW", "SH1ADD_UW", "SH2ADD_UW", "SH3ADD_UW", "SFENCE_VMA", "HFENCE_VVMA", "HFENCE_GVMA", "HLV_B", "HLV_BU", "HLV_H", "HLV_HU", "HLVX_HU", "HLV_W", "HLVX_WU", "HSV_B", "HSV_H", "HSV_W", "Instructions32.ZEXT_H", "CLMULR", "PACK", "PACKH", "BREV8", "SHA256SIG0", "SHA256SIG1", "SHA256SUM0", "SHA256SUM1", "BCLR", "BEXT", "BINV", "BSET", "Instructions32.REV8", "LD", "LWU", "SD", "SLLI", "SRLI", "SRAI", "ADDIW", "SLLIW", "SRLIW", "SRAIW", "ADDW", "SUBW", "SLLW", "SRLW", "SRAW", "MULW", "DIVW", "DIVUW", "REMW", "REMUW", "AMOADD_D", "AMOSWAP_D", "AMOXOR_D", "AMOAND_D", "AMOOR_D", "AMOMIN_D", "AMOMINU_D", "AMOMAX_D", "AMOMAXU_D", "LR_D", "SC_D", "MUL", "MULH", "MULHU", "MULHSU", "DIV", "DIVU", "REM", "REMU", "AMOADD_W", "AMOXOR_W", "AMOSWAP_W", "AMOAND_W", "AMOOR_W", "AMOMIN_W", "AMOMINU_W", "AMOMAX_W", "AMOMAXU_W", "LR_W", "SC_W", "MAX", "MAXU", "MIN", "MINU", 
    )
    val insns_Y: Seq[String] = Seq(
      "BNE", "BEQ", "BLT", "BLTU", "BGE", "BGEU", 
    )
    def genTable(op: Op): BitPat = {
      if (insns_N.contains(op.bitPat)) N else if (insns_Y.contains(op.bitPat)) Y else N
    }
  }
  object decode_jal extends BoolField {
    val insns_N: Seq[String] = Seq(
      "SCIE.opcode", "AES64DS", "AES64DSM", "AES64IM", "AES64KS1I", "AES64KS2", "PACKW", "SHA512SIG0", "SHA512SIG1", "SHA512SUM0", "SHA512SUM1", "ROR", "ROL", "HLV_D", "HSV_D", "HLV_WU", "FSGNJ_S", "FSGNJX_S", "FSGNJN_S", "FMIN_S", "FMAX_S", "FADD_S", "FSUB_S", "FMUL_S", "FMADD_S", "FMSUB_S", "FNMADD_S", "FNMSUB_S", "FCLASS_S", "FMV_X_W", "FCVT_W_S", "FCVT_WU_S", "FEQ_S", "FLT_S", "FLE_S", "FMV_W_X", "FCVT_S_W", "FCVT_S_WU", "FLW", "FSW", "FDIV_S", "FSQRT_S", "CLZW", "CTZW", "CPOPW", "SRET", "Instructions32.BCLRI", "Instructions32.BEXTI", "Instructions32.BINVI", "Instructions32.BSETI", "Instructions32.RORI", "SH1ADD", "SH2ADD", "SH3ADD", "ORC_B", "CUSTOM0", "CUSTOM0_RS1", "CUSTOM0_RS1_RS2", "CUSTOM0_RD", "CUSTOM0_RD_RS1", "CUSTOM0_RD_RS1_RS2", "CUSTOM1", "CUSTOM1_RS1", "CUSTOM1_RS1_RS2", "CUSTOM1_RD", "CUSTOM1_RD_RS1", "CUSTOM1_RD_RS1_RS2", "CUSTOM2", "CUSTOM2_RS1", "CUSTOM2_RS1_RS2", "CUSTOM2_RD", "CUSTOM2_RD_RS1", "CUSTOM2_RD_RS1_RS2", "CUSTOM3", "CUSTOM3_RS1", "CUSTOM3_RS1_RS2", "CUSTOM3_RD", "CUSTOM3_RD_RS1", "CUSTOM3_RD_RS1_RS2", "Instructions32.AES32ESI", "Instructions32.AES32ESMI", "FCVT_S_D", "FCVT_D_S", "FSGNJ_D", "FSGNJX_D", "FSGNJN_D", "FMIN_D", "FMAX_D", "FADD_D", "FSUB_D", "FMUL_D", "FMADD_D", "FMSUB_D", "FNMADD_D", "FNMSUB_D", "FCLASS_D", "FCVT_W_D", "FCVT_WU_D", "FEQ_D", "FLT_D", "FLE_D", "FCVT_D_W", "FCVT_D_WU", "FLD", "FSD", "FDIV_D", "FSQRT_D", "Instructions32.ZIP", "Instructions32.UNZIP", "FCVT_L_S", "FCVT_LU_S", "FCVT_S_L", "FCVT_S_LU", "Instructions32.AES32DSI", "Instructions32.AES32DSMI", "Instructions32.SHA512SIG0L", "Instructions32.SHA512SIG1L", "Instructions32.SHA512SIG0H", "Instructions32.SHA512SIG1H", "Instructions32.SHA512SUM0R", "Instructions32.SHA512SUM1R", "FCVT_S_H", "FCVT_H_S", "FSGNJ_H", "FSGNJX_H", "FSGNJN_H", "FMIN_H", "FMAX_H", "FADD_H", "FSUB_H", "FMUL_H", "FMADD_H", "FMSUB_H", "FNMADD_H", "FNMSUB_H", "FCLASS_H", "FMV_X_H", "FCVT_W_H", "FCVT_WU_H", "FEQ_H", "FLT_H", "FLE_H", "FMV_H_X", "FCVT_H_W", "FCVT_H_WU", "FLH", "FSH", "FDIV_H", "FSQRT_H", "AES64ES", "AES64ESM", "CLMUL", "CLMULH", "BCLRI", "BEXTI", "BINVI", "BSETI", "RORI", "RORW", "ROLW", "RORIW", "FCVT_D_H", "FCVT_H_D", "CFLUSH_D_L1", "CDISCARD_D_L1", "BNE", "BEQ", "BLT", "BLTU", "BGE", "BGEU", "JALR", "AUIPC", "LB", "LH", "LW", "LBU", "LHU", "SB", "SH", "SW", "LUI", "ADDI", "SLTI", "SLTIU", "ANDI", "ORI", "XORI", "ADD", "SUB", "SLT", "SLTU", "AND", "OR", "XOR", "SLL", "SRL", "SRA", "FENCE", "ECALL", "EBREAK", "MRET", "WFI", "CEASE", "CSRRW", "CSRRS", "CSRRC", "CSRRWI", "CSRRSI", "CSRRCI", "SEXT_H", "SEXT_B", "XPERM8", "XPERM4", "FMV_X_D", "FCVT_L_D", "FCVT_LU_D", "FMV_D_X", "FCVT_D_L", "FCVT_D_LU", "ZEXT_H", "MNRET", "FCVT_L_H", "FCVT_LU_H", "FCVT_H_L", "FCVT_H_LU", "FENCE_I", "ANDN", "ORN", "XNOR", "Instructions32.SLLI", "Instructions32.SRLI", "Instructions32.SRAI", "SM4ED", "SM4KS", "SM3P0", "SM3P1", "DRET", "CLZ", "CTZ", "CPOP", "REV8", "ADD_UW", "SLLI_UW", "SH1ADD_UW", "SH2ADD_UW", "SH3ADD_UW", "SFENCE_VMA", "HFENCE_VVMA", "HFENCE_GVMA", "HLV_B", "HLV_BU", "HLV_H", "HLV_HU", "HLVX_HU", "HLV_W", "HLVX_WU", "HSV_B", "HSV_H", "HSV_W", "Instructions32.ZEXT_H", "CLMULR", "PACK", "PACKH", "BREV8", "SHA256SIG0", "SHA256SIG1", "SHA256SUM0", "SHA256SUM1", "BCLR", "BEXT", "BINV", "BSET", "Instructions32.REV8", "LD", "LWU", "SD", "SLLI", "SRLI", "SRAI", "ADDIW", "SLLIW", "SRLIW", "SRAIW", "ADDW", "SUBW", "SLLW", "SRLW", "SRAW", "MULW", "DIVW", "DIVUW", "REMW", "REMUW", "AMOADD_D", "AMOSWAP_D", "AMOXOR_D", "AMOAND_D", "AMOOR_D", "AMOMIN_D", "AMOMINU_D", "AMOMAX_D", "AMOMAXU_D", "LR_D", "SC_D", "MUL", "MULH", "MULHU", "MULHSU", "DIV", "DIVU", "REM", "REMU", "AMOADD_W", "AMOXOR_W", "AMOSWAP_W", "AMOAND_W", "AMOOR_W", "AMOMIN_W", "AMOMINU_W", "AMOMAX_W", "AMOMAXU_W", "LR_W", "SC_W", "MAX", "MAXU", "MIN", "MINU", 
    )
    val insns_Y: Seq[String] = Seq(
      "JAL", 
    )
    def genTable(op: Op): BitPat = {
      if (insns_N.contains(op.bitPat)) N else if (insns_Y.contains(op.bitPat)) Y else N
    }
  }
  object decode_jalr extends BoolField {
    val insns_N: Seq[String] = Seq(
      "SCIE.opcode", "AES64DS", "AES64DSM", "AES64IM", "AES64KS1I", "AES64KS2", "PACKW", "SHA512SIG0", "SHA512SIG1", "SHA512SUM0", "SHA512SUM1", "ROR", "ROL", "HLV_D", "HSV_D", "HLV_WU", "FSGNJ_S", "FSGNJX_S", "FSGNJN_S", "FMIN_S", "FMAX_S", "FADD_S", "FSUB_S", "FMUL_S", "FMADD_S", "FMSUB_S", "FNMADD_S", "FNMSUB_S", "FCLASS_S", "FMV_X_W", "FCVT_W_S", "FCVT_WU_S", "FEQ_S", "FLT_S", "FLE_S", "FMV_W_X", "FCVT_S_W", "FCVT_S_WU", "FLW", "FSW", "FDIV_S", "FSQRT_S", "CLZW", "CTZW", "CPOPW", "SRET", "Instructions32.BCLRI", "Instructions32.BEXTI", "Instructions32.BINVI", "Instructions32.BSETI", "Instructions32.RORI", "SH1ADD", "SH2ADD", "SH3ADD", "ORC_B", "CUSTOM0", "CUSTOM0_RS1", "CUSTOM0_RS1_RS2", "CUSTOM0_RD", "CUSTOM0_RD_RS1", "CUSTOM0_RD_RS1_RS2", "CUSTOM1", "CUSTOM1_RS1", "CUSTOM1_RS1_RS2", "CUSTOM1_RD", "CUSTOM1_RD_RS1", "CUSTOM1_RD_RS1_RS2", "CUSTOM2", "CUSTOM2_RS1", "CUSTOM2_RS1_RS2", "CUSTOM2_RD", "CUSTOM2_RD_RS1", "CUSTOM2_RD_RS1_RS2", "CUSTOM3", "CUSTOM3_RS1", "CUSTOM3_RS1_RS2", "CUSTOM3_RD", "CUSTOM3_RD_RS1", "CUSTOM3_RD_RS1_RS2", "Instructions32.AES32ESI", "Instructions32.AES32ESMI", "FCVT_S_D", "FCVT_D_S", "FSGNJ_D", "FSGNJX_D", "FSGNJN_D", "FMIN_D", "FMAX_D", "FADD_D", "FSUB_D", "FMUL_D", "FMADD_D", "FMSUB_D", "FNMADD_D", "FNMSUB_D", "FCLASS_D", "FCVT_W_D", "FCVT_WU_D", "FEQ_D", "FLT_D", "FLE_D", "FCVT_D_W", "FCVT_D_WU", "FLD", "FSD", "FDIV_D", "FSQRT_D", "Instructions32.ZIP", "Instructions32.UNZIP", "FCVT_L_S", "FCVT_LU_S", "FCVT_S_L", "FCVT_S_LU", "Instructions32.AES32DSI", "Instructions32.AES32DSMI", "Instructions32.SHA512SIG0L", "Instructions32.SHA512SIG1L", "Instructions32.SHA512SIG0H", "Instructions32.SHA512SIG1H", "Instructions32.SHA512SUM0R", "Instructions32.SHA512SUM1R", "FCVT_S_H", "FCVT_H_S", "FSGNJ_H", "FSGNJX_H", "FSGNJN_H", "FMIN_H", "FMAX_H", "FADD_H", "FSUB_H", "FMUL_H", "FMADD_H", "FMSUB_H", "FNMADD_H", "FNMSUB_H", "FCLASS_H", "FMV_X_H", "FCVT_W_H", "FCVT_WU_H", "FEQ_H", "FLT_H", "FLE_H", "FMV_H_X", "FCVT_H_W", "FCVT_H_WU", "FLH", "FSH", "FDIV_H", "FSQRT_H", "AES64ES", "AES64ESM", "CLMUL", "CLMULH", "BCLRI", "BEXTI", "BINVI", "BSETI", "RORI", "RORW", "ROLW", "RORIW", "FCVT_D_H", "FCVT_H_D", "CFLUSH_D_L1", "CDISCARD_D_L1", "BNE", "BEQ", "BLT", "BLTU", "BGE", "BGEU", "JAL", "AUIPC", "LB", "LH", "LW", "LBU", "LHU", "SB", "SH", "SW", "LUI", "ADDI", "SLTI", "SLTIU", "ANDI", "ORI", "XORI", "ADD", "SUB", "SLT", "SLTU", "AND", "OR", "XOR", "SLL", "SRL", "SRA", "FENCE", "ECALL", "EBREAK", "MRET", "WFI", "CEASE", "CSRRW", "CSRRS", "CSRRC", "CSRRWI", "CSRRSI", "CSRRCI", "SEXT_H", "SEXT_B", "XPERM8", "XPERM4", "FMV_X_D", "FCVT_L_D", "FCVT_LU_D", "FMV_D_X", "FCVT_D_L", "FCVT_D_LU", "ZEXT_H", "MNRET", "FCVT_L_H", "FCVT_LU_H", "FCVT_H_L", "FCVT_H_LU", "FENCE_I", "ANDN", "ORN", "XNOR", "Instructions32.SLLI", "Instructions32.SRLI", "Instructions32.SRAI", "SM4ED", "SM4KS", "SM3P0", "SM3P1", "DRET", "CLZ", "CTZ", "CPOP", "REV8", "ADD_UW", "SLLI_UW", "SH1ADD_UW", "SH2ADD_UW", "SH3ADD_UW", "SFENCE_VMA", "HFENCE_VVMA", "HFENCE_GVMA", "HLV_B", "HLV_BU", "HLV_H", "HLV_HU", "HLVX_HU", "HLV_W", "HLVX_WU", "HSV_B", "HSV_H", "HSV_W", "Instructions32.ZEXT_H", "CLMULR", "PACK", "PACKH", "BREV8", "SHA256SIG0", "SHA256SIG1", "SHA256SUM0", "SHA256SUM1", "BCLR", "BEXT", "BINV", "BSET", "Instructions32.REV8", "LD", "LWU", "SD", "SLLI", "SRLI", "SRAI", "ADDIW", "SLLIW", "SRLIW", "SRAIW", "ADDW", "SUBW", "SLLW", "SRLW", "SRAW", "MULW", "DIVW", "DIVUW", "REMW", "REMUW", "AMOADD_D", "AMOSWAP_D", "AMOXOR_D", "AMOAND_D", "AMOOR_D", "AMOMIN_D", "AMOMINU_D", "AMOMAX_D", "AMOMAXU_D", "LR_D", "SC_D", "MUL", "MULH", "MULHU", "MULHSU", "DIV", "DIVU", "REM", "REMU", "AMOADD_W", "AMOXOR_W", "AMOSWAP_W", "AMOAND_W", "AMOOR_W", "AMOMIN_W", "AMOMINU_W", "AMOMAX_W", "AMOMAXU_W", "LR_W", "SC_W", "MAX", "MAXU", "MIN", "MINU", 
    )
    val insns_Y: Seq[String] = Seq(
      "JALR", 
    )
    def genTable(op: Op): BitPat = {
      if (insns_N.contains(op.bitPat)) N else if (insns_Y.contains(op.bitPat)) Y else N
    }
  }
  object decode_rxs2 extends BoolField {
    val insns_Y: Seq[String] = Seq(
      "SCIE.opcode", "AES64DS", "AES64DSM", "AES64IM", "AES64KS2", "PACKW", "ROR", "ROL", "HSV_D", "SH1ADD", "SH2ADD", "SH3ADD", "CUSTOM0_RS1_RS2", "CUSTOM0_RD_RS1_RS2", "CUSTOM1_RS1_RS2", "CUSTOM1_RD_RS1_RS2", "CUSTOM2_RS1_RS2", "CUSTOM2_RD_RS1_RS2", "CUSTOM3_RS1_RS2", "CUSTOM3_RD_RS1_RS2", "Instructions32.AES32ESI", "Instructions32.AES32ESMI", "Instructions32.AES32DSI", "Instructions32.AES32DSMI", "Instructions32.SHA512SIG0L", "Instructions32.SHA512SIG1L", "Instructions32.SHA512SIG0H", "Instructions32.SHA512SIG1H", "Instructions32.SHA512SUM0R", "Instructions32.SHA512SUM1R", "AES64ES", "AES64ESM", "CLMUL", "CLMULH", "RORW", "ROLW", "BNE", "BEQ", "BLT", "BLTU", "BGE", "BGEU", "SB", "SH", "SW", "ADD", "SUB", "SLT", "SLTU", "AND", "OR", "XOR", "SLL", "SRL", "SRA", "XPERM8", "XPERM4", "ANDN", "ORN", "XNOR", "SM4ED", "SM4KS", "ADD_UW", "SH1ADD_UW", "SH2ADD_UW", "SH3ADD_UW", "SFENCE_VMA", "HFENCE_VVMA", "HFENCE_GVMA", "HSV_B", "HSV_H", "HSV_W", "CLMULR", "PACK", "PACKH", "BCLR", "BEXT", "BINV", "BSET", "SD", "ADDW", "SUBW", "SLLW", "SRLW", "SRAW", "MULW", "DIVW", "DIVUW", "REMW", "REMUW", "AMOADD_D", "AMOSWAP_D", "AMOXOR_D", "AMOAND_D", "AMOOR_D", "AMOMIN_D", "AMOMINU_D", "AMOMAX_D", "AMOMAXU_D", "LR_D", "SC_D", "MUL", "MULH", "MULHU", "MULHSU", "DIV", "DIVU", "REM", "REMU", "AMOADD_W", "AMOXOR_W", "AMOSWAP_W", "AMOAND_W", "AMOOR_W", "AMOMIN_W", "AMOMINU_W", "AMOMAX_W", "AMOMAXU_W", "LR_W", "SC_W", "MAX", "MAXU", "MIN", "MINU", 
    )
    val insns_N: Seq[String] = Seq(
      "AES64KS1I", "SHA512SIG0", "SHA512SIG1", "SHA512SUM0", "SHA512SUM1", "HLV_D", "HLV_WU", "FSGNJ_S", "FSGNJX_S", "FSGNJN_S", "FMIN_S", "FMAX_S", "FADD_S", "FSUB_S", "FMUL_S", "FMADD_S", "FMSUB_S", "FNMADD_S", "FNMSUB_S", "FCLASS_S", "FMV_X_W", "FCVT_W_S", "FCVT_WU_S", "FEQ_S", "FLT_S", "FLE_S", "FMV_W_X", "FCVT_S_W", "FCVT_S_WU", "FLW", "FSW", "FDIV_S", "FSQRT_S", "CLZW", "CTZW", "CPOPW", "SRET", "Instructions32.BCLRI", "Instructions32.BEXTI", "Instructions32.BINVI", "Instructions32.BSETI", "Instructions32.RORI", "ORC_B", "CUSTOM0", "CUSTOM0_RS1", "CUSTOM0_RD", "CUSTOM0_RD_RS1", "CUSTOM1", "CUSTOM1_RS1", "CUSTOM1_RD", "CUSTOM1_RD_RS1", "CUSTOM2", "CUSTOM2_RS1", "CUSTOM2_RD", "CUSTOM2_RD_RS1", "CUSTOM3", "CUSTOM3_RS1", "CUSTOM3_RD", "CUSTOM3_RD_RS1", "FCVT_S_D", "FCVT_D_S", "FSGNJ_D", "FSGNJX_D", "FSGNJN_D", "FMIN_D", "FMAX_D", "FADD_D", "FSUB_D", "FMUL_D", "FMADD_D", "FMSUB_D", "FNMADD_D", "FNMSUB_D", "FCLASS_D", "FCVT_W_D", "FCVT_WU_D", "FEQ_D", "FLT_D", "FLE_D", "FCVT_D_W", "FCVT_D_WU", "FLD", "FSD", "FDIV_D", "FSQRT_D", "Instructions32.ZIP", "Instructions32.UNZIP", "FCVT_L_S", "FCVT_LU_S", "FCVT_S_L", "FCVT_S_LU", "FCVT_S_H", "FCVT_H_S", "FSGNJ_H", "FSGNJX_H", "FSGNJN_H", "FMIN_H", "FMAX_H", "FADD_H", "FSUB_H", "FMUL_H", "FMADD_H", "FMSUB_H", "FNMADD_H", "FNMSUB_H", "FCLASS_H", "FMV_X_H", "FCVT_W_H", "FCVT_WU_H", "FEQ_H", "FLT_H", "FLE_H", "FMV_H_X", "FCVT_H_W", "FCVT_H_WU", "FLH", "FSH", "FDIV_H", "FSQRT_H", "BCLRI", "BEXTI", "BINVI", "BSETI", "RORI", "RORIW", "FCVT_D_H", "FCVT_H_D", "CFLUSH_D_L1", "CDISCARD_D_L1", "JAL", "JALR", "AUIPC", "LB", "LH", "LW", "LBU", "LHU", "LUI", "ADDI", "SLTI", "SLTIU", "ANDI", "ORI", "XORI", "FENCE", "ECALL", "EBREAK", "MRET", "WFI", "CEASE", "CSRRW", "CSRRS", "CSRRC", "CSRRWI", "CSRRSI", "CSRRCI", "SEXT_H", "SEXT_B", "FMV_X_D", "FCVT_L_D", "FCVT_LU_D", "FMV_D_X", "FCVT_D_L", "FCVT_D_LU", "ZEXT_H", "MNRET", "FCVT_L_H", "FCVT_LU_H", "FCVT_H_L", "FCVT_H_LU", "FENCE_I", "Instructions32.SLLI", "Instructions32.SRLI", "Instructions32.SRAI", "SM3P0", "SM3P1", "DRET", "CLZ", "CTZ", "CPOP", "REV8", "SLLI_UW", "HLV_B", "HLV_BU", "HLV_H", "HLV_HU", "HLVX_HU", "HLV_W", "HLVX_WU", "Instructions32.ZEXT_H", "BREV8", "SHA256SIG0", "SHA256SIG1", "SHA256SUM0", "SHA256SUM1", "Instructions32.REV8", "LD", "LWU", "SLLI", "SRLI", "SRAI", "ADDIW", "SLLIW", "SRLIW", "SRAIW", 
    )
    def genTable(op: Op): BitPat = {
      if (insns_Y.contains(op.bitPat)) Y else if (insns_N.contains(op.bitPat)) N else N
    }
  }
  object decode_rxs1 extends BoolField {
    val insns_Y: Seq[String] = Seq(
      "SCIE.opcode", "AES64DS", "AES64DSM", "AES64IM", "AES64KS1I", "AES64KS2", "PACKW", "SHA512SIG0", "SHA512SIG1", "SHA512SUM0", "SHA512SUM1", "ROR", "ROL", "HLV_D", "HSV_D", "HLV_WU", "FMV_W_X", "FCVT_S_W", "FCVT_S_WU", "FLW", "FSW", "CLZW", "CTZW", "CPOPW", "Instructions32.BCLRI", "Instructions32.BEXTI", "Instructions32.BINVI", "Instructions32.BSETI", "Instructions32.RORI", "SH1ADD", "SH2ADD", "SH3ADD", "ORC_B", "CUSTOM0_RS1", "CUSTOM0_RS1_RS2", "CUSTOM0_RD_RS1", "CUSTOM0_RD_RS1_RS2", "CUSTOM1_RS1", "CUSTOM1_RS1_RS2", "CUSTOM1_RD_RS1", "CUSTOM1_RD_RS1_RS2", "CUSTOM2_RS1", "CUSTOM2_RS1_RS2", "CUSTOM2_RD_RS1", "CUSTOM2_RD_RS1_RS2", "CUSTOM3_RS1", "CUSTOM3_RS1_RS2", "CUSTOM3_RD_RS1", "CUSTOM3_RD_RS1_RS2", "Instructions32.AES32ESI", "Instructions32.AES32ESMI", "FCVT_D_W", "FCVT_D_WU", "FLD", "FSD", "Instructions32.ZIP", "Instructions32.UNZIP", "FCVT_S_L", "FCVT_S_LU", "Instructions32.AES32DSI", "Instructions32.AES32DSMI", "Instructions32.SHA512SIG0L", "Instructions32.SHA512SIG1L", "Instructions32.SHA512SIG0H", "Instructions32.SHA512SIG1H", "Instructions32.SHA512SUM0R", "Instructions32.SHA512SUM1R", "FMV_H_X", "FCVT_H_W", "FCVT_H_WU", "FLH", "FSH", "AES64ES", "AES64ESM", "CLMUL", "CLMULH", "BCLRI", "BEXTI", "BINVI", "BSETI", "RORI", "RORW", "ROLW", "RORIW", "CFLUSH_D_L1", "CDISCARD_D_L1", "BNE", "BEQ", "BLT", "BLTU", "BGE", "BGEU", "JALR", "LB", "LH", "LW", "LBU", "LHU", "SB", "SH", "SW", "ADDI", "SLTI", "SLTIU", "ANDI", "ORI", "XORI", "ADD", "SUB", "SLT", "SLTU", "AND", "OR", "XOR", "SLL", "SRL", "SRA", "CSRRW", "CSRRS", "CSRRC", "SEXT_H", "SEXT_B", "XPERM8", "XPERM4", "FMV_D_X", "FCVT_D_L", "FCVT_D_LU", "ZEXT_H", "FCVT_H_L", "FCVT_H_LU", "ANDN", "ORN", "XNOR", "Instructions32.SLLI", "Instructions32.SRLI", "Instructions32.SRAI", "SM4ED", "SM4KS", "SM3P0", "SM3P1", "CLZ", "CTZ", "CPOP", "REV8", "ADD_UW", "SLLI_UW", "SH1ADD_UW", "SH2ADD_UW", "SH3ADD_UW", "SFENCE_VMA", "HFENCE_VVMA", "HFENCE_GVMA", "HLV_B", "HLV_BU", "HLV_H", "HLV_HU", "HLVX_HU", "HLV_W", "HLVX_WU", "HSV_B", "HSV_H", "HSV_W", "Instructions32.ZEXT_H", "CLMULR", "PACK", "PACKH", "BREV8", "SHA256SIG0", "SHA256SIG1", "SHA256SUM0", "SHA256SUM1", "BCLR", "BEXT", "BINV", "BSET", "Instructions32.REV8", "LD", "LWU", "SD", "SLLI", "SRLI", "SRAI", "ADDIW", "SLLIW", "SRLIW", "SRAIW", "ADDW", "SUBW", "SLLW", "SRLW", "SRAW", "MULW", "DIVW", "DIVUW", "REMW", "REMUW", "AMOADD_D", "AMOSWAP_D", "AMOXOR_D", "AMOAND_D", "AMOOR_D", "AMOMIN_D", "AMOMINU_D", "AMOMAX_D", "AMOMAXU_D", "LR_D", "SC_D", "MUL", "MULH", "MULHU", "MULHSU", "DIV", "DIVU", "REM", "REMU", "AMOADD_W", "AMOXOR_W", "AMOSWAP_W", "AMOAND_W", "AMOOR_W", "AMOMIN_W", "AMOMINU_W", "AMOMAX_W", "AMOMAXU_W", "LR_W", "SC_W", "MAX", "MAXU", "MIN", "MINU", 
    )
    val insns_N: Seq[String] = Seq(
      "FSGNJ_S", "FSGNJX_S", "FSGNJN_S", "FMIN_S", "FMAX_S", "FADD_S", "FSUB_S", "FMUL_S", "FMADD_S", "FMSUB_S", "FNMADD_S", "FNMSUB_S", "FCLASS_S", "FMV_X_W", "FCVT_W_S", "FCVT_WU_S", "FEQ_S", "FLT_S", "FLE_S", "FDIV_S", "FSQRT_S", "CUSTOM0", "CUSTOM0_RD", "CUSTOM1", "CUSTOM1_RD", "CUSTOM2", "CUSTOM2_RD", "CUSTOM3", "CUSTOM3_RD", "FCVT_S_D", "FCVT_D_S", "FSGNJ_D", "FSGNJX_D", "FSGNJN_D", "FMIN_D", "FMAX_D", "FADD_D", "FSUB_D", "FMUL_D", "FMADD_D", "FMSUB_D", "FNMADD_D", "FNMSUB_D", "FCLASS_D", "FCVT_W_D", "FCVT_WU_D", "FEQ_D", "FLT_D", "FLE_D", "FDIV_D", "FSQRT_D", "FCVT_L_S", "FCVT_LU_S", "FCVT_S_H", "FCVT_H_S", "FSGNJ_H", "FSGNJX_H", "FSGNJN_H", "FMIN_H", "FMAX_H", "FADD_H", "FSUB_H", "FMUL_H", "FMADD_H", "FMSUB_H", "FNMADD_H", "FNMSUB_H", "FCLASS_H", "FMV_X_H", "FCVT_W_H", "FCVT_WU_H", "FEQ_H", "FLT_H", "FLE_H", "FDIV_H", "FSQRT_H", "FCVT_D_H", "FCVT_H_D", "JAL", "AUIPC", "LUI", "FENCE", "CSRRWI", "CSRRSI", "CSRRCI", "FMV_X_D", "FCVT_L_D", "FCVT_LU_D", "FCVT_L_H", "FCVT_LU_H", "FENCE_I", 
    )
    val insns_X: Seq[String] = Seq(
      "SRET", "ECALL", "EBREAK", "MRET", "WFI", "CEASE", "MNRET", "DRET", 
    )
    def genTable(op: Op): BitPat = {
      if (insns_Y.contains(op.bitPat)) Y else if (insns_N.contains(op.bitPat)) N else if (insns_X.contains(op.bitPat)) X else N
    }
  }
  object decode_scie extends BoolField {
    val insns_Y: Seq[String] = Seq(
      "SCIE.opcode", 
    )
    val insns_N: Seq[String] = Seq(
      "AES64DS", "AES64DSM", "AES64IM", "AES64KS1I", "AES64KS2", "PACKW", "SHA512SIG0", "SHA512SIG1", "SHA512SUM0", "SHA512SUM1", "ROR", "ROL", "HLV_D", "HSV_D", "HLV_WU", "FSGNJ_S", "FSGNJX_S", "FSGNJN_S", "FMIN_S", "FMAX_S", "FADD_S", "FSUB_S", "FMUL_S", "FMADD_S", "FMSUB_S", "FNMADD_S", "FNMSUB_S", "FCLASS_S", "FMV_X_W", "FCVT_W_S", "FCVT_WU_S", "FEQ_S", "FLT_S", "FLE_S", "FMV_W_X", "FCVT_S_W", "FCVT_S_WU", "FLW", "FSW", "FDIV_S", "FSQRT_S", "CLZW", "CTZW", "CPOPW", "SRET", "Instructions32.BCLRI", "Instructions32.BEXTI", "Instructions32.BINVI", "Instructions32.BSETI", "Instructions32.RORI", "SH1ADD", "SH2ADD", "SH3ADD", "ORC_B", "CUSTOM0", "CUSTOM0_RS1", "CUSTOM0_RS1_RS2", "CUSTOM0_RD", "CUSTOM0_RD_RS1", "CUSTOM0_RD_RS1_RS2", "CUSTOM1", "CUSTOM1_RS1", "CUSTOM1_RS1_RS2", "CUSTOM1_RD", "CUSTOM1_RD_RS1", "CUSTOM1_RD_RS1_RS2", "CUSTOM2", "CUSTOM2_RS1", "CUSTOM2_RS1_RS2", "CUSTOM2_RD", "CUSTOM2_RD_RS1", "CUSTOM2_RD_RS1_RS2", "CUSTOM3", "CUSTOM3_RS1", "CUSTOM3_RS1_RS2", "CUSTOM3_RD", "CUSTOM3_RD_RS1", "CUSTOM3_RD_RS1_RS2", "Instructions32.AES32ESI", "Instructions32.AES32ESMI", "FCVT_S_D", "FCVT_D_S", "FSGNJ_D", "FSGNJX_D", "FSGNJN_D", "FMIN_D", "FMAX_D", "FADD_D", "FSUB_D", "FMUL_D", "FMADD_D", "FMSUB_D", "FNMADD_D", "FNMSUB_D", "FCLASS_D", "FCVT_W_D", "FCVT_WU_D", "FEQ_D", "FLT_D", "FLE_D", "FCVT_D_W", "FCVT_D_WU", "FLD", "FSD", "FDIV_D", "FSQRT_D", "Instructions32.ZIP", "Instructions32.UNZIP", "FCVT_L_S", "FCVT_LU_S", "FCVT_S_L", "FCVT_S_LU", "Instructions32.AES32DSI", "Instructions32.AES32DSMI", "Instructions32.SHA512SIG0L", "Instructions32.SHA512SIG1L", "Instructions32.SHA512SIG0H", "Instructions32.SHA512SIG1H", "Instructions32.SHA512SUM0R", "Instructions32.SHA512SUM1R", "FCVT_S_H", "FCVT_H_S", "FSGNJ_H", "FSGNJX_H", "FSGNJN_H", "FMIN_H", "FMAX_H", "FADD_H", "FSUB_H", "FMUL_H", "FMADD_H", "FMSUB_H", "FNMADD_H", "FNMSUB_H", "FCLASS_H", "FMV_X_H", "FCVT_W_H", "FCVT_WU_H", "FEQ_H", "FLT_H", "FLE_H", "FMV_H_X", "FCVT_H_W", "FCVT_H_WU", "FLH", "FSH", "FDIV_H", "FSQRT_H", "AES64ES", "AES64ESM", "CLMUL", "CLMULH", "BCLRI", "BEXTI", "BINVI", "BSETI", "RORI", "RORW", "ROLW", "RORIW", "FCVT_D_H", "FCVT_H_D", "CFLUSH_D_L1", "CDISCARD_D_L1", "BNE", "BEQ", "BLT", "BLTU", "BGE", "BGEU", "JAL", "JALR", "AUIPC", "LB", "LH", "LW", "LBU", "LHU", "SB", "SH", "SW", "LUI", "ADDI", "SLTI", "SLTIU", "ANDI", "ORI", "XORI", "ADD", "SUB", "SLT", "SLTU", "AND", "OR", "XOR", "SLL", "SRL", "SRA", "FENCE", "ECALL", "EBREAK", "MRET", "WFI", "CEASE", "CSRRW", "CSRRS", "CSRRC", "CSRRWI", "CSRRSI", "CSRRCI", "SEXT_H", "SEXT_B", "XPERM8", "XPERM4", "FMV_X_D", "FCVT_L_D", "FCVT_LU_D", "FMV_D_X", "FCVT_D_L", "FCVT_D_LU", "ZEXT_H", "MNRET", "FCVT_L_H", "FCVT_LU_H", "FCVT_H_L", "FCVT_H_LU", "FENCE_I", "ANDN", "ORN", "XNOR", "Instructions32.SLLI", "Instructions32.SRLI", "Instructions32.SRAI", "SM4ED", "SM4KS", "SM3P0", "SM3P1", "DRET", "CLZ", "CTZ", "CPOP", "REV8", "ADD_UW", "SLLI_UW", "SH1ADD_UW", "SH2ADD_UW", "SH3ADD_UW", "SFENCE_VMA", "HFENCE_VVMA", "HFENCE_GVMA", "HLV_B", "HLV_BU", "HLV_H", "HLV_HU", "HLVX_HU", "HLV_W", "HLVX_WU", "HSV_B", "HSV_H", "HSV_W", "Instructions32.ZEXT_H", "CLMULR", "PACK", "PACKH", "BREV8", "SHA256SIG0", "SHA256SIG1", "SHA256SUM0", "SHA256SUM1", "BCLR", "BEXT", "BINV", "BSET", "Instructions32.REV8", "LD", "LWU", "SD", "SLLI", "SRLI", "SRAI", "ADDIW", "SLLIW", "SRLIW", "SRAIW", "ADDW", "SUBW", "SLLW", "SRLW", "SRAW", "MULW", "DIVW", "DIVUW", "REMW", "REMUW", "AMOADD_D", "AMOSWAP_D", "AMOXOR_D", "AMOAND_D", "AMOOR_D", "AMOMIN_D", "AMOMINU_D", "AMOMAX_D", "AMOMAXU_D", "LR_D", "SC_D", "MUL", "MULH", "MULHU", "MULHSU", "DIV", "DIVU", "REM", "REMU", "AMOADD_W", "AMOXOR_W", "AMOSWAP_W", "AMOAND_W", "AMOOR_W", "AMOMIN_W", "AMOMINU_W", "AMOMAX_W", "AMOMAXU_W", "LR_W", "SC_W", "MAX", "MAXU", "MIN", "MINU", 
    )
    def genTable(op: Op): BitPat = {
      if (insns_Y.contains(op.bitPat)) Y else if (insns_N.contains(op.bitPat)) N else N
    }
  }
  object decode_zbk extends BoolField {
    val insns_N: Seq[String] = Seq(
      "SCIE.opcode", "AES64DS", "AES64DSM", "AES64IM", "AES64KS1I", "AES64KS2", "PACKW", "SHA512SIG0", "SHA512SIG1", "SHA512SUM0", "SHA512SUM1", "ROR", "ROL", "HLV_D", "HSV_D", "HLV_WU", "FSGNJ_S", "FSGNJX_S", "FSGNJN_S", "FMIN_S", "FMAX_S", "FADD_S", "FSUB_S", "FMUL_S", "FMADD_S", "FMSUB_S", "FNMADD_S", "FNMSUB_S", "FCLASS_S", "FMV_X_W", "FCVT_W_S", "FCVT_WU_S", "FEQ_S", "FLT_S", "FLE_S", "FMV_W_X", "FCVT_S_W", "FCVT_S_WU", "FLW", "FSW", "FDIV_S", "FSQRT_S", "CLZW", "CTZW", "CPOPW", "SRET", "Instructions32.BCLRI", "Instructions32.BEXTI", "Instructions32.BINVI", "Instructions32.BSETI", "Instructions32.RORI", "SH1ADD", "SH2ADD", "SH3ADD", "ORC_B", "CUSTOM0", "CUSTOM0_RS1", "CUSTOM0_RS1_RS2", "CUSTOM0_RD", "CUSTOM0_RD_RS1", "CUSTOM0_RD_RS1_RS2", "CUSTOM1", "CUSTOM1_RS1", "CUSTOM1_RS1_RS2", "CUSTOM1_RD", "CUSTOM1_RD_RS1", "CUSTOM1_RD_RS1_RS2", "CUSTOM2", "CUSTOM2_RS1", "CUSTOM2_RS1_RS2", "CUSTOM2_RD", "CUSTOM2_RD_RS1", "CUSTOM2_RD_RS1_RS2", "CUSTOM3", "CUSTOM3_RS1", "CUSTOM3_RS1_RS2", "CUSTOM3_RD", "CUSTOM3_RD_RS1", "CUSTOM3_RD_RS1_RS2", "Instructions32.AES32ESI", "Instructions32.AES32ESMI", "FCVT_S_D", "FCVT_D_S", "FSGNJ_D", "FSGNJX_D", "FSGNJN_D", "FMIN_D", "FMAX_D", "FADD_D", "FSUB_D", "FMUL_D", "FMADD_D", "FMSUB_D", "FNMADD_D", "FNMSUB_D", "FCLASS_D", "FCVT_W_D", "FCVT_WU_D", "FEQ_D", "FLT_D", "FLE_D", "FCVT_D_W", "FCVT_D_WU", "FLD", "FSD", "FDIV_D", "FSQRT_D", "Instructions32.ZIP", "Instructions32.UNZIP", "FCVT_L_S", "FCVT_LU_S", "FCVT_S_L", "FCVT_S_LU", "Instructions32.AES32DSI", "Instructions32.AES32DSMI", "Instructions32.SHA512SIG0L", "Instructions32.SHA512SIG1L", "Instructions32.SHA512SIG0H", "Instructions32.SHA512SIG1H", "Instructions32.SHA512SUM0R", "Instructions32.SHA512SUM1R", "FCVT_S_H", "FCVT_H_S", "FSGNJ_H", "FSGNJX_H", "FSGNJN_H", "FMIN_H", "FMAX_H", "FADD_H", "FSUB_H", "FMUL_H", "FMADD_H", "FMSUB_H", "FNMADD_H", "FNMSUB_H", "FCLASS_H", "FMV_X_H", "FCVT_W_H", "FCVT_WU_H", "FEQ_H", "FLT_H", "FLE_H", "FMV_H_X", "FCVT_H_W", "FCVT_H_WU", "FLH", "FSH", "FDIV_H", "FSQRT_H", "AES64ES", "AES64ESM", "BCLRI", "BEXTI", "BINVI", "BSETI", "RORI", "RORW", "ROLW", "RORIW", "FCVT_D_H", "FCVT_H_D", "CFLUSH_D_L1", "CDISCARD_D_L1", "BNE", "BEQ", "BLT", "BLTU", "BGE", "BGEU", "JAL", "JALR", "AUIPC", "LB", "LH", "LW", "LBU", "LHU", "SB", "SH", "SW", "LUI", "ADDI", "SLTI", "SLTIU", "ANDI", "ORI", "XORI", "ADD", "SUB", "SLT", "SLTU", "AND", "OR", "XOR", "SLL", "SRL", "SRA", "FENCE", "ECALL", "EBREAK", "MRET", "WFI", "CEASE", "CSRRW", "CSRRS", "CSRRC", "CSRRWI", "CSRRSI", "CSRRCI", "SEXT_H", "SEXT_B", "FMV_X_D", "FCVT_L_D", "FCVT_LU_D", "FMV_D_X", "FCVT_D_L", "FCVT_D_LU", "ZEXT_H", "MNRET", "FCVT_L_H", "FCVT_LU_H", "FCVT_H_L", "FCVT_H_LU", "FENCE_I", "ANDN", "ORN", "XNOR", "Instructions32.SLLI", "Instructions32.SRLI", "Instructions32.SRAI", "SM4ED", "SM4KS", "SM3P0", "SM3P1", "DRET", "CLZ", "CTZ", "CPOP", "REV8", "ADD_UW", "SLLI_UW", "SH1ADD_UW", "SH2ADD_UW", "SH3ADD_UW", "SFENCE_VMA", "HFENCE_VVMA", "HFENCE_GVMA", "HLV_B", "HLV_BU", "HLV_H", "HLV_HU", "HLVX_HU", "HLV_W", "HLVX_WU", "HSV_B", "HSV_H", "HSV_W", "Instructions32.ZEXT_H", "PACK", "PACKH", "BREV8", "SHA256SIG0", "SHA256SIG1", "SHA256SUM0", "SHA256SUM1", "BCLR", "BEXT", "BINV", "BSET", "Instructions32.REV8", "LD", "LWU", "SD", "SLLI", "SRLI", "SRAI", "ADDIW", "SLLIW", "SRLIW", "SRAIW", "ADDW", "SUBW", "SLLW", "SRLW", "SRAW", "MULW", "DIVW", "DIVUW", "REMW", "REMUW", "AMOADD_D", "AMOSWAP_D", "AMOXOR_D", "AMOAND_D", "AMOOR_D", "AMOMIN_D", "AMOMINU_D", "AMOMAX_D", "AMOMAXU_D", "LR_D", "SC_D", "MUL", "MULH", "MULHU", "MULHSU", "DIV", "DIVU", "REM", "REMU", "AMOADD_W", "AMOXOR_W", "AMOSWAP_W", "AMOAND_W", "AMOOR_W", "AMOMIN_W", "AMOMINU_W", "AMOMAX_W", "AMOMAXU_W", "LR_W", "SC_W", "MAX", "MAXU", "MIN", "MINU", 
    )
    val insns_Y: Seq[String] = Seq(
      "CLMUL", "CLMULH", "XPERM8", "XPERM4", "CLMULR", 
    )
    def genTable(op: Op): BitPat = {
      if (insns_N.contains(op.bitPat)) N else if (insns_Y.contains(op.bitPat)) Y else N
    }
  }
  object decode_zkn extends BoolField {
    val insns_N: Seq[String] = Seq(
      "SCIE.opcode", "PACKW", "ROR", "ROL", "HLV_D", "HSV_D", "HLV_WU", "FSGNJ_S", "FSGNJX_S", "FSGNJN_S", "FMIN_S", "FMAX_S", "FADD_S", "FSUB_S", "FMUL_S", "FMADD_S", "FMSUB_S", "FNMADD_S", "FNMSUB_S", "FCLASS_S", "FMV_X_W", "FCVT_W_S", "FCVT_WU_S", "FEQ_S", "FLT_S", "FLE_S", "FMV_W_X", "FCVT_S_W", "FCVT_S_WU", "FLW", "FSW", "FDIV_S", "FSQRT_S", "CLZW", "CTZW", "CPOPW", "SRET", "Instructions32.BCLRI", "Instructions32.BEXTI", "Instructions32.BINVI", "Instructions32.BSETI", "Instructions32.RORI", "SH1ADD", "SH2ADD", "SH3ADD", "ORC_B", "CUSTOM0", "CUSTOM0_RS1", "CUSTOM0_RS1_RS2", "CUSTOM0_RD", "CUSTOM0_RD_RS1", "CUSTOM0_RD_RS1_RS2", "CUSTOM1", "CUSTOM1_RS1", "CUSTOM1_RS1_RS2", "CUSTOM1_RD", "CUSTOM1_RD_RS1", "CUSTOM1_RD_RS1_RS2", "CUSTOM2", "CUSTOM2_RS1", "CUSTOM2_RS1_RS2", "CUSTOM2_RD", "CUSTOM2_RD_RS1", "CUSTOM2_RD_RS1_RS2", "CUSTOM3", "CUSTOM3_RS1", "CUSTOM3_RS1_RS2", "CUSTOM3_RD", "CUSTOM3_RD_RS1", "CUSTOM3_RD_RS1_RS2", "FCVT_S_D", "FCVT_D_S", "FSGNJ_D", "FSGNJX_D", "FSGNJN_D", "FMIN_D", "FMAX_D", "FADD_D", "FSUB_D", "FMUL_D", "FMADD_D", "FMSUB_D", "FNMADD_D", "FNMSUB_D", "FCLASS_D", "FCVT_W_D", "FCVT_WU_D", "FEQ_D", "FLT_D", "FLE_D", "FCVT_D_W", "FCVT_D_WU", "FLD", "FSD", "FDIV_D", "FSQRT_D", "Instructions32.ZIP", "Instructions32.UNZIP", "FCVT_L_S", "FCVT_LU_S", "FCVT_S_L", "FCVT_S_LU", "FCVT_S_H", "FCVT_H_S", "FSGNJ_H", "FSGNJX_H", "FSGNJN_H", "FMIN_H", "FMAX_H", "FADD_H", "FSUB_H", "FMUL_H", "FMADD_H", "FMSUB_H", "FNMADD_H", "FNMSUB_H", "FCLASS_H", "FMV_X_H", "FCVT_W_H", "FCVT_WU_H", "FEQ_H", "FLT_H", "FLE_H", "FMV_H_X", "FCVT_H_W", "FCVT_H_WU", "FLH", "FSH", "FDIV_H", "FSQRT_H", "CLMUL", "CLMULH", "BCLRI", "BEXTI", "BINVI", "BSETI", "RORI", "RORW", "ROLW", "RORIW", "FCVT_D_H", "FCVT_H_D", "CFLUSH_D_L1", "CDISCARD_D_L1", "BNE", "BEQ", "BLT", "BLTU", "BGE", "BGEU", "JAL", "JALR", "AUIPC", "LB", "LH", "LW", "LBU", "LHU", "SB", "SH", "SW", "LUI", "ADDI", "SLTI", "SLTIU", "ANDI", "ORI", "XORI", "ADD", "SUB", "SLT", "SLTU", "AND", "OR", "XOR", "SLL", "SRL", "SRA", "FENCE", "ECALL", "EBREAK", "MRET", "WFI", "CEASE", "CSRRW", "CSRRS", "CSRRC", "CSRRWI", "CSRRSI", "CSRRCI", "SEXT_H", "SEXT_B", "XPERM8", "XPERM4", "FMV_X_D", "FCVT_L_D", "FCVT_LU_D", "FMV_D_X", "FCVT_D_L", "FCVT_D_LU", "ZEXT_H", "MNRET", "FCVT_L_H", "FCVT_LU_H", "FCVT_H_L", "FCVT_H_LU", "FENCE_I", "ANDN", "ORN", "XNOR", "Instructions32.SLLI", "Instructions32.SRLI", "Instructions32.SRAI", "SM4ED", "SM4KS", "SM3P0", "SM3P1", "DRET", "CLZ", "CTZ", "CPOP", "REV8", "ADD_UW", "SLLI_UW", "SH1ADD_UW", "SH2ADD_UW", "SH3ADD_UW", "SFENCE_VMA", "HFENCE_VVMA", "HFENCE_GVMA", "HLV_B", "HLV_BU", "HLV_H", "HLV_HU", "HLVX_HU", "HLV_W", "HLVX_WU", "HSV_B", "HSV_H", "HSV_W", "Instructions32.ZEXT_H", "CLMULR", "PACK", "PACKH", "BREV8", "BCLR", "BEXT", "BINV", "BSET", "Instructions32.REV8", "LD", "LWU", "SD", "SLLI", "SRLI", "SRAI", "ADDIW", "SLLIW", "SRLIW", "SRAIW", "ADDW", "SUBW", "SLLW", "SRLW", "SRAW", "MULW", "DIVW", "DIVUW", "REMW", "REMUW", "AMOADD_D", "AMOSWAP_D", "AMOXOR_D", "AMOAND_D", "AMOOR_D", "AMOMIN_D", "AMOMINU_D", "AMOMAX_D", "AMOMAXU_D", "LR_D", "SC_D", "MUL", "MULH", "MULHU", "MULHSU", "DIV", "DIVU", "REM", "REMU", "AMOADD_W", "AMOXOR_W", "AMOSWAP_W", "AMOAND_W", "AMOOR_W", "AMOMIN_W", "AMOMINU_W", "AMOMAX_W", "AMOMAXU_W", "LR_W", "SC_W", "MAX", "MAXU", "MIN", "MINU", 
    )
    val insns_Y: Seq[String] = Seq(
      "AES64DS", "AES64DSM", "AES64IM", "AES64KS1I", "AES64KS2", "SHA512SIG0", "SHA512SIG1", "SHA512SUM0", "SHA512SUM1", "Instructions32.AES32ESI", "Instructions32.AES32ESMI", "Instructions32.AES32DSI", "Instructions32.AES32DSMI", "Instructions32.SHA512SIG0L", "Instructions32.SHA512SIG1L", "Instructions32.SHA512SIG0H", "Instructions32.SHA512SIG1H", "Instructions32.SHA512SUM0R", "Instructions32.SHA512SUM1R", "AES64ES", "AES64ESM", "SHA256SIG0", "SHA256SIG1", "SHA256SUM0", "SHA256SUM1", 
    )
    def genTable(op: Op): BitPat = {
      if (insns_N.contains(op.bitPat)) N else if (insns_Y.contains(op.bitPat)) Y else N
    }
  }
  object decode_zks extends BoolField {
    val insns_N: Seq[String] = Seq(
      "SCIE.opcode", "AES64DS", "AES64DSM", "AES64IM", "AES64KS1I", "AES64KS2", "PACKW", "SHA512SIG0", "SHA512SIG1", "SHA512SUM0", "SHA512SUM1", "ROR", "ROL", "HLV_D", "HSV_D", "HLV_WU", "FSGNJ_S", "FSGNJX_S", "FSGNJN_S", "FMIN_S", "FMAX_S", "FADD_S", "FSUB_S", "FMUL_S", "FMADD_S", "FMSUB_S", "FNMADD_S", "FNMSUB_S", "FCLASS_S", "FMV_X_W", "FCVT_W_S", "FCVT_WU_S", "FEQ_S", "FLT_S", "FLE_S", "FMV_W_X", "FCVT_S_W", "FCVT_S_WU", "FLW", "FSW", "FDIV_S", "FSQRT_S", "CLZW", "CTZW", "CPOPW", "SRET", "Instructions32.BCLRI", "Instructions32.BEXTI", "Instructions32.BINVI", "Instructions32.BSETI", "Instructions32.RORI", "SH1ADD", "SH2ADD", "SH3ADD", "ORC_B", "CUSTOM0", "CUSTOM0_RS1", "CUSTOM0_RS1_RS2", "CUSTOM0_RD", "CUSTOM0_RD_RS1", "CUSTOM0_RD_RS1_RS2", "CUSTOM1", "CUSTOM1_RS1", "CUSTOM1_RS1_RS2", "CUSTOM1_RD", "CUSTOM1_RD_RS1", "CUSTOM1_RD_RS1_RS2", "CUSTOM2", "CUSTOM2_RS1", "CUSTOM2_RS1_RS2", "CUSTOM2_RD", "CUSTOM2_RD_RS1", "CUSTOM2_RD_RS1_RS2", "CUSTOM3", "CUSTOM3_RS1", "CUSTOM3_RS1_RS2", "CUSTOM3_RD", "CUSTOM3_RD_RS1", "CUSTOM3_RD_RS1_RS2", "Instructions32.AES32ESI", "Instructions32.AES32ESMI", "FCVT_S_D", "FCVT_D_S", "FSGNJ_D", "FSGNJX_D", "FSGNJN_D", "FMIN_D", "FMAX_D", "FADD_D", "FSUB_D", "FMUL_D", "FMADD_D", "FMSUB_D", "FNMADD_D", "FNMSUB_D", "FCLASS_D", "FCVT_W_D", "FCVT_WU_D", "FEQ_D", "FLT_D", "FLE_D", "FCVT_D_W", "FCVT_D_WU", "FLD", "FSD", "FDIV_D", "FSQRT_D", "Instructions32.ZIP", "Instructions32.UNZIP", "FCVT_L_S", "FCVT_LU_S", "FCVT_S_L", "FCVT_S_LU", "Instructions32.AES32DSI", "Instructions32.AES32DSMI", "Instructions32.SHA512SIG0L", "Instructions32.SHA512SIG1L", "Instructions32.SHA512SIG0H", "Instructions32.SHA512SIG1H", "Instructions32.SHA512SUM0R", "Instructions32.SHA512SUM1R", "FCVT_S_H", "FCVT_H_S", "FSGNJ_H", "FSGNJX_H", "FSGNJN_H", "FMIN_H", "FMAX_H", "FADD_H", "FSUB_H", "FMUL_H", "FMADD_H", "FMSUB_H", "FNMADD_H", "FNMSUB_H", "FCLASS_H", "FMV_X_H", "FCVT_W_H", "FCVT_WU_H", "FEQ_H", "FLT_H", "FLE_H", "FMV_H_X", "FCVT_H_W", "FCVT_H_WU", "FLH", "FSH", "FDIV_H", "FSQRT_H", "AES64ES", "AES64ESM", "CLMUL", "CLMULH", "BCLRI", "BEXTI", "BINVI", "BSETI", "RORI", "RORW", "ROLW", "RORIW", "FCVT_D_H", "FCVT_H_D", "CFLUSH_D_L1", "CDISCARD_D_L1", "BNE", "BEQ", "BLT", "BLTU", "BGE", "BGEU", "JAL", "JALR", "AUIPC", "LB", "LH", "LW", "LBU", "LHU", "SB", "SH", "SW", "LUI", "ADDI", "SLTI", "SLTIU", "ANDI", "ORI", "XORI", "ADD", "SUB", "SLT", "SLTU", "AND", "OR", "XOR", "SLL", "SRL", "SRA", "FENCE", "ECALL", "EBREAK", "MRET", "WFI", "CEASE", "CSRRW", "CSRRS", "CSRRC", "CSRRWI", "CSRRSI", "CSRRCI", "SEXT_H", "SEXT_B", "XPERM8", "XPERM4", "FMV_X_D", "FCVT_L_D", "FCVT_LU_D", "FMV_D_X", "FCVT_D_L", "FCVT_D_LU", "ZEXT_H", "MNRET", "FCVT_L_H", "FCVT_LU_H", "FCVT_H_L", "FCVT_H_LU", "FENCE_I", "ANDN", "ORN", "XNOR", "Instructions32.SLLI", "Instructions32.SRLI", "Instructions32.SRAI", "DRET", "CLZ", "CTZ", "CPOP", "REV8", "ADD_UW", "SLLI_UW", "SH1ADD_UW", "SH2ADD_UW", "SH3ADD_UW", "SFENCE_VMA", "HFENCE_VVMA", "HFENCE_GVMA", "HLV_B", "HLV_BU", "HLV_H", "HLV_HU", "HLVX_HU", "HLV_W", "HLVX_WU", "HSV_B", "HSV_H", "HSV_W", "Instructions32.ZEXT_H", "CLMULR", "PACK", "PACKH", "BREV8", "SHA256SIG0", "SHA256SIG1", "SHA256SUM0", "SHA256SUM1", "BCLR", "BEXT", "BINV", "BSET", "Instructions32.REV8", "LD", "LWU", "SD", "SLLI", "SRLI", "SRAI", "ADDIW", "SLLIW", "SRLIW", "SRAIW", "ADDW", "SUBW", "SLLW", "SRLW", "SRAW", "MULW", "DIVW", "DIVUW", "REMW", "REMUW", "AMOADD_D", "AMOSWAP_D", "AMOXOR_D", "AMOAND_D", "AMOOR_D", "AMOMIN_D", "AMOMINU_D", "AMOMAX_D", "AMOMAXU_D", "LR_D", "SC_D", "MUL", "MULH", "MULHU", "MULHSU", "DIV", "DIVU", "REM", "REMU", "AMOADD_W", "AMOXOR_W", "AMOSWAP_W", "AMOAND_W", "AMOOR_W", "AMOMIN_W", "AMOMINU_W", "AMOMAX_W", "AMOMAXU_W", "LR_W", "SC_W", "MAX", "MAXU", "MIN", "MINU", 
    )
    val insns_Y: Seq[String] = Seq(
      "SM4ED", "SM4KS", "SM3P0", "SM3P1", 
    )
    def genTable(op: Op): BitPat = {
      if (insns_N.contains(op.bitPat)) N else if (insns_Y.contains(op.bitPat)) Y else N
    }
  }
  object decode_sel_alu2 extends BitsField_2 {
    val insns_A2_ZERO: Seq[String] = Seq(
      "SCIE.opcode", "HLV_D", "HSV_D", "HLV_WU", "CUSTOM0", "CUSTOM0_RS1", "CUSTOM0_RS1_RS2", "CUSTOM0_RD", "CUSTOM0_RD_RS1", "CUSTOM0_RD_RS1_RS2", "CUSTOM1", "CUSTOM1_RS1", "CUSTOM1_RS1_RS2", "CUSTOM1_RD", "CUSTOM1_RD_RS1", "CUSTOM1_RD_RS1_RS2", "CUSTOM2", "CUSTOM2_RS1", "CUSTOM2_RS1_RS2", "CUSTOM2_RD", "CUSTOM2_RD_RS1", "CUSTOM2_RD_RS1_RS2", "CUSTOM3", "CUSTOM3_RS1", "CUSTOM3_RS1_RS2", "CUSTOM3_RD", "CUSTOM3_RD_RS1", "CUSTOM3_RD_RS1_RS2", "CFLUSH_D_L1", "CDISCARD_D_L1", "CSRRW", "CSRRS", "CSRRC", "SFENCE_VMA", "HFENCE_VVMA", "HFENCE_GVMA", "HLV_B", "HLV_BU", "HLV_H", "HLV_HU", "HLVX_HU", "HLV_W", "HLVX_WU", "HSV_B", "HSV_H", "HSV_W", "AMOADD_D", "AMOSWAP_D", "AMOXOR_D", "AMOAND_D", "AMOOR_D", "AMOMIN_D", "AMOMINU_D", "AMOMAX_D", "AMOMAXU_D", "LR_D", "SC_D", "AMOADD_W", "AMOXOR_W", "AMOSWAP_W", "AMOAND_W", "AMOOR_W", "AMOMIN_W", "AMOMINU_W", "AMOMAX_W", "AMOMAXU_W", "LR_W", "SC_W", 
    )
    val insns_A2_RS2: Seq[String] = Seq(
      "AES64DS", "AES64DSM", "AES64IM", "AES64KS2", "PACKW", "ROR", "ROL", "SH1ADD", "SH2ADD", "SH3ADD", "Instructions32.AES32ESI", "Instructions32.AES32ESMI", "Instructions32.AES32DSI", "Instructions32.AES32DSMI", "Instructions32.SHA512SIG0L", "Instructions32.SHA512SIG1L", "Instructions32.SHA512SIG0H", "Instructions32.SHA512SIG1H", "Instructions32.SHA512SUM0R", "Instructions32.SHA512SUM1R", "AES64ES", "AES64ESM", "CLMUL", "CLMULH", "RORW", "ROLW", "BNE", "BEQ", "BLT", "BLTU", "BGE", "BGEU", "ADD", "SUB", "SLT", "SLTU", "AND", "OR", "XOR", "SLL", "SRL", "SRA", "XPERM8", "XPERM4", "ANDN", "ORN", "XNOR", "SM4ED", "SM4KS", "ADD_UW", "SH1ADD_UW", "SH2ADD_UW", "SH3ADD_UW", "CLMULR", "PACK", "PACKH", "BCLR", "BEXT", "BINV", "BSET", "ADDW", "SUBW", "SLLW", "SRLW", "SRAW", "MULW", "DIVW", "DIVUW", "REMW", "REMUW", "MUL", "MULH", "MULHU", "MULHSU", "DIV", "DIVU", "REM", "REMU", "MAX", "MAXU", "MIN", "MINU", 
    )
    val insns_A2_IMM: Seq[String] = Seq(
      "AES64KS1I", "FLW", "FSW", "Instructions32.BCLRI", "Instructions32.BEXTI", "Instructions32.BINVI", "Instructions32.BSETI", "Instructions32.RORI", "FLD", "FSD", "FLH", "FSH", "BCLRI", "BEXTI", "BINVI", "BSETI", "RORI", "RORIW", "JALR", "AUIPC", "LB", "LH", "LW", "LBU", "LHU", "SB", "SH", "SW", "LUI", "ADDI", "SLTI", "SLTIU", "ANDI", "ORI", "XORI", "CSRRWI", "CSRRSI", "CSRRCI", "Instructions32.SLLI", "Instructions32.SRLI", "Instructions32.SRAI", "SLLI_UW", "LD", "LWU", "SD", "SLLI", "SRLI", "SRAI", "ADDIW", "SLLIW", "SRLIW", "SRAIW", 
    )
    val insns_A2_X: Seq[String] = Seq(
      "SHA512SIG0", "SHA512SIG1", "SHA512SUM0", "SHA512SUM1", "FSGNJ_S", "FSGNJX_S", "FSGNJN_S", "FMIN_S", "FMAX_S", "FADD_S", "FSUB_S", "FMUL_S", "FMADD_S", "FMSUB_S", "FNMADD_S", "FNMSUB_S", "FCLASS_S", "FMV_X_W", "FCVT_W_S", "FCVT_WU_S", "FEQ_S", "FLT_S", "FLE_S", "FMV_W_X", "FCVT_S_W", "FCVT_S_WU", "FDIV_S", "FSQRT_S", "CLZW", "CTZW", "CPOPW", "SRET", "ORC_B", "FCVT_S_D", "FCVT_D_S", "FSGNJ_D", "FSGNJX_D", "FSGNJN_D", "FMIN_D", "FMAX_D", "FADD_D", "FSUB_D", "FMUL_D", "FMADD_D", "FMSUB_D", "FNMADD_D", "FNMSUB_D", "FCLASS_D", "FCVT_W_D", "FCVT_WU_D", "FEQ_D", "FLT_D", "FLE_D", "FCVT_D_W", "FCVT_D_WU", "FDIV_D", "FSQRT_D", "Instructions32.ZIP", "Instructions32.UNZIP", "FCVT_L_S", "FCVT_LU_S", "FCVT_S_L", "FCVT_S_LU", "FCVT_S_H", "FCVT_H_S", "FSGNJ_H", "FSGNJX_H", "FSGNJN_H", "FMIN_H", "FMAX_H", "FADD_H", "FSUB_H", "FMUL_H", "FMADD_H", "FMSUB_H", "FNMADD_H", "FNMSUB_H", "FCLASS_H", "FMV_X_H", "FCVT_W_H", "FCVT_WU_H", "FEQ_H", "FLT_H", "FLE_H", "FMV_H_X", "FCVT_H_W", "FCVT_H_WU", "FDIV_H", "FSQRT_H", "FCVT_D_H", "FCVT_H_D", "FENCE", "ECALL", "EBREAK", "MRET", "WFI", "CEASE", "SEXT_H", "SEXT_B", "FMV_X_D", "FCVT_L_D", "FCVT_LU_D", "FMV_D_X", "FCVT_D_L", "FCVT_D_LU", "ZEXT_H", "MNRET", "FCVT_L_H", "FCVT_LU_H", "FCVT_H_L", "FCVT_H_LU", "FENCE_I", "SM3P0", "SM3P1", "DRET", "CLZ", "CTZ", "CPOP", "REV8", "Instructions32.ZEXT_H", "BREV8", "SHA256SIG0", "SHA256SIG1", "SHA256SUM0", "SHA256SUM1", "Instructions32.REV8", 
    )
    val insns_A2_SIZE: Seq[String] = Seq(
      "JAL", 
    )
    def genTable(op: Op): BitPat = {
      if (insns_A2_ZERO.contains(op.bitPat)) A2_ZERO else if (insns_A2_RS2.contains(op.bitPat)) A2_RS2 else if (insns_A2_IMM.contains(op.bitPat)) A2_IMM else if (insns_A2_X.contains(op.bitPat)) A2_X else if (insns_A2_SIZE.contains(op.bitPat)) A2_SIZE else A2_X
    }
  }
  object decode_sel_alu1 extends BitsField_2 {
    val insns_A1_RS1: Seq[String] = Seq(
      "SCIE.opcode", "AES64DS", "AES64DSM", "AES64IM", "AES64KS1I", "AES64KS2", "PACKW", "SHA512SIG0", "SHA512SIG1", "SHA512SUM0", "SHA512SUM1", "ROR", "ROL", "HLV_D", "HSV_D", "HLV_WU", "FMV_W_X", "FCVT_S_W", "FCVT_S_WU", "FLW", "FSW", "CLZW", "CTZW", "CPOPW", "Instructions32.BCLRI", "Instructions32.BEXTI", "Instructions32.BINVI", "Instructions32.BSETI", "Instructions32.RORI", "SH1ADD", "SH2ADD", "SH3ADD", "ORC_B", "CUSTOM0", "CUSTOM0_RS1", "CUSTOM0_RS1_RS2", "CUSTOM0_RD", "CUSTOM0_RD_RS1", "CUSTOM0_RD_RS1_RS2", "CUSTOM1", "CUSTOM1_RS1", "CUSTOM1_RS1_RS2", "CUSTOM1_RD", "CUSTOM1_RD_RS1", "CUSTOM1_RD_RS1_RS2", "CUSTOM2", "CUSTOM2_RS1", "CUSTOM2_RS1_RS2", "CUSTOM2_RD", "CUSTOM2_RD_RS1", "CUSTOM2_RD_RS1_RS2", "CUSTOM3", "CUSTOM3_RS1", "CUSTOM3_RS1_RS2", "CUSTOM3_RD", "CUSTOM3_RD_RS1", "CUSTOM3_RD_RS1_RS2", "Instructions32.AES32ESI", "Instructions32.AES32ESMI", "FCVT_D_W", "FCVT_D_WU", "FLD", "FSD", "Instructions32.ZIP", "Instructions32.UNZIP", "FCVT_S_L", "FCVT_S_LU", "Instructions32.AES32DSI", "Instructions32.AES32DSMI", "Instructions32.SHA512SIG0L", "Instructions32.SHA512SIG1L", "Instructions32.SHA512SIG0H", "Instructions32.SHA512SIG1H", "Instructions32.SHA512SUM0R", "Instructions32.SHA512SUM1R", "FMV_H_X", "FCVT_H_W", "FCVT_H_WU", "FLH", "FSH", "AES64ES", "AES64ESM", "CLMUL", "CLMULH", "BCLRI", "BEXTI", "BINVI", "BSETI", "RORI", "RORW", "ROLW", "RORIW", "CFLUSH_D_L1", "CDISCARD_D_L1", "BNE", "BEQ", "BLT", "BLTU", "BGE", "BGEU", "JALR", "LB", "LH", "LW", "LBU", "LHU", "SB", "SH", "SW", "ADDI", "SLTI", "SLTIU", "ANDI", "ORI", "XORI", "ADD", "SUB", "SLT", "SLTU", "AND", "OR", "XOR", "SLL", "SRL", "SRA", "CSRRW", "CSRRS", "CSRRC", "SEXT_H", "SEXT_B", "XPERM8", "XPERM4", "FMV_D_X", "FCVT_D_L", "FCVT_D_LU", "ZEXT_H", "FCVT_H_L", "FCVT_H_LU", "ANDN", "ORN", "XNOR", "Instructions32.SLLI", "Instructions32.SRLI", "Instructions32.SRAI", "SM4ED", "SM4KS", "SM3P0", "SM3P1", "CLZ", "CTZ", "CPOP", "REV8", "ADD_UW", "SLLI_UW", "SH1ADD_UW", "SH2ADD_UW", "SH3ADD_UW", "SFENCE_VMA", "HFENCE_VVMA", "HFENCE_GVMA", "HLV_B", "HLV_BU", "HLV_H", "HLV_HU", "HLVX_HU", "HLV_W", "HLVX_WU", "HSV_B", "HSV_H", "HSV_W", "Instructions32.ZEXT_H", "CLMULR", "PACK", "PACKH", "BREV8", "SHA256SIG0", "SHA256SIG1", "SHA256SUM0", "SHA256SUM1", "BCLR", "BEXT", "BINV", "BSET", "Instructions32.REV8", "LD", "LWU", "SD", "SLLI", "SRLI", "SRAI", "ADDIW", "SLLIW", "SRLIW", "SRAIW", "ADDW", "SUBW", "SLLW", "SRLW", "SRAW", "MULW", "DIVW", "DIVUW", "REMW", "REMUW", "AMOADD_D", "AMOSWAP_D", "AMOXOR_D", "AMOAND_D", "AMOOR_D", "AMOMIN_D", "AMOMINU_D", "AMOMAX_D", "AMOMAXU_D", "LR_D", "SC_D", "MUL", "MULH", "MULHU", "MULHSU", "DIV", "DIVU", "REM", "REMU", "AMOADD_W", "AMOXOR_W", "AMOSWAP_W", "AMOAND_W", "AMOOR_W", "AMOMIN_W", "AMOMINU_W", "AMOMAX_W", "AMOMAXU_W", "LR_W", "SC_W", "MAX", "MAXU", "MIN", "MINU", 
    )
    val insns_A1_X: Seq[String] = Seq(
      "FSGNJ_S", "FSGNJX_S", "FSGNJN_S", "FMIN_S", "FMAX_S", "FADD_S", "FSUB_S", "FMUL_S", "FMADD_S", "FMSUB_S", "FNMADD_S", "FNMSUB_S", "FCLASS_S", "FMV_X_W", "FCVT_W_S", "FCVT_WU_S", "FEQ_S", "FLT_S", "FLE_S", "FDIV_S", "FSQRT_S", "SRET", "FCVT_S_D", "FCVT_D_S", "FSGNJ_D", "FSGNJX_D", "FSGNJN_D", "FMIN_D", "FMAX_D", "FADD_D", "FSUB_D", "FMUL_D", "FMADD_D", "FMSUB_D", "FNMADD_D", "FNMSUB_D", "FCLASS_D", "FCVT_W_D", "FCVT_WU_D", "FEQ_D", "FLT_D", "FLE_D", "FDIV_D", "FSQRT_D", "FCVT_L_S", "FCVT_LU_S", "FCVT_S_H", "FCVT_H_S", "FSGNJ_H", "FSGNJX_H", "FSGNJN_H", "FMIN_H", "FMAX_H", "FADD_H", "FSUB_H", "FMUL_H", "FMADD_H", "FMSUB_H", "FNMADD_H", "FNMSUB_H", "FCLASS_H", "FMV_X_H", "FCVT_W_H", "FCVT_WU_H", "FEQ_H", "FLT_H", "FLE_H", "FDIV_H", "FSQRT_H", "FCVT_D_H", "FCVT_H_D", "FENCE", "ECALL", "EBREAK", "MRET", "WFI", "CEASE", "FMV_X_D", "FCVT_L_D", "FCVT_LU_D", "MNRET", "FCVT_L_H", "FCVT_LU_H", "FENCE_I", "DRET", 
    )
    val insns_A1_PC: Seq[String] = Seq(
      "JAL", "AUIPC", 
    )
    val insns_A1_ZERO: Seq[String] = Seq(
      "LUI", "CSRRWI", "CSRRSI", "CSRRCI", 
    )
    def genTable(op: Op): BitPat = {
      if (insns_A1_RS1.contains(op.bitPat)) A1_RS1 else if (insns_A1_X.contains(op.bitPat)) A1_X else if (insns_A1_PC.contains(op.bitPat)) A1_PC else if (insns_A1_ZERO.contains(op.bitPat)) A1_ZERO else A1_X
    }
  }
  object decode_sel_imm extends BitsField_3 {
    val insns_IMM_X: Seq[String] = Seq(
      "SCIE.opcode", "AES64DS", "AES64DSM", "AES64IM", "AES64KS2", "PACKW", "SHA512SIG0", "SHA512SIG1", "SHA512SUM0", "SHA512SUM1", "ROR", "ROL", "HLV_D", "HLV_WU", "FSGNJ_S", "FSGNJX_S", "FSGNJN_S", "FMIN_S", "FMAX_S", "FADD_S", "FSUB_S", "FMUL_S", "FMADD_S", "FMSUB_S", "FNMADD_S", "FNMSUB_S", "FCLASS_S", "FMV_X_W", "FCVT_W_S", "FCVT_WU_S", "FEQ_S", "FLT_S", "FLE_S", "FMV_W_X", "FCVT_S_W", "FCVT_S_WU", "FDIV_S", "FSQRT_S", "CLZW", "CTZW", "CPOPW", "SRET", "SH1ADD", "SH2ADD", "SH3ADD", "ORC_B", "CUSTOM0", "CUSTOM0_RS1", "CUSTOM0_RS1_RS2", "CUSTOM0_RD", "CUSTOM0_RD_RS1", "CUSTOM0_RD_RS1_RS2", "CUSTOM1", "CUSTOM1_RS1", "CUSTOM1_RS1_RS2", "CUSTOM1_RD", "CUSTOM1_RD_RS1", "CUSTOM1_RD_RS1_RS2", "CUSTOM2", "CUSTOM2_RS1", "CUSTOM2_RS1_RS2", "CUSTOM2_RD", "CUSTOM2_RD_RS1", "CUSTOM2_RD_RS1_RS2", "CUSTOM3", "CUSTOM3_RS1", "CUSTOM3_RS1_RS2", "CUSTOM3_RD", "CUSTOM3_RD_RS1", "CUSTOM3_RD_RS1_RS2", "Instructions32.AES32ESI", "Instructions32.AES32ESMI", "FCVT_S_D", "FCVT_D_S", "FSGNJ_D", "FSGNJX_D", "FSGNJN_D", "FMIN_D", "FMAX_D", "FADD_D", "FSUB_D", "FMUL_D", "FMADD_D", "FMSUB_D", "FNMADD_D", "FNMSUB_D", "FCLASS_D", "FCVT_W_D", "FCVT_WU_D", "FEQ_D", "FLT_D", "FLE_D", "FCVT_D_W", "FCVT_D_WU", "FDIV_D", "FSQRT_D", "Instructions32.ZIP", "Instructions32.UNZIP", "FCVT_L_S", "FCVT_LU_S", "FCVT_S_L", "FCVT_S_LU", "Instructions32.AES32DSI", "Instructions32.AES32DSMI", "Instructions32.SHA512SIG0L", "Instructions32.SHA512SIG1L", "Instructions32.SHA512SIG0H", "Instructions32.SHA512SIG1H", "Instructions32.SHA512SUM0R", "Instructions32.SHA512SUM1R", "FCVT_S_H", "FCVT_H_S", "FSGNJ_H", "FSGNJX_H", "FSGNJN_H", "FMIN_H", "FMAX_H", "FADD_H", "FSUB_H", "FMUL_H", "FMADD_H", "FMSUB_H", "FNMADD_H", "FNMSUB_H", "FCLASS_H", "FMV_X_H", "FCVT_W_H", "FCVT_WU_H", "FEQ_H", "FLT_H", "FLE_H", "FMV_H_X", "FCVT_H_W", "FCVT_H_WU", "FDIV_H", "FSQRT_H", "AES64ES", "AES64ESM", "CLMUL", "CLMULH", "RORW", "ROLW", "FCVT_D_H", "FCVT_H_D", "CFLUSH_D_L1", "CDISCARD_D_L1", "ADD", "SUB", "SLT", "SLTU", "AND", "OR", "XOR", "SLL", "SRL", "SRA", "FENCE", "ECALL", "EBREAK", "MRET", "WFI", "CEASE", "CSRRW", "CSRRS", "CSRRC", "SEXT_H", "SEXT_B", "XPERM8", "XPERM4", "FMV_X_D", "FCVT_L_D", "FCVT_LU_D", "FMV_D_X", "FCVT_D_L", "FCVT_D_LU", "ZEXT_H", "MNRET", "FCVT_L_H", "FCVT_LU_H", "FCVT_H_L", "FCVT_H_LU", "FENCE_I", "ANDN", "ORN", "XNOR", "SM4ED", "SM4KS", "SM3P0", "SM3P1", "DRET", "CLZ", "CTZ", "CPOP", "REV8", "ADD_UW", "SH1ADD_UW", "SH2ADD_UW", "SH3ADD_UW", "SFENCE_VMA", "HFENCE_VVMA", "HFENCE_GVMA", "HLV_B", "HLV_BU", "HLV_H", "HLV_HU", "HLVX_HU", "HLV_W", "HLVX_WU", "Instructions32.ZEXT_H", "CLMULR", "PACK", "PACKH", "BREV8", "SHA256SIG0", "SHA256SIG1", "SHA256SUM0", "SHA256SUM1", "BCLR", "BEXT", "BINV", "BSET", "Instructions32.REV8", "ADDW", "SUBW", "SLLW", "SRLW", "SRAW", "MULW", "DIVW", "DIVUW", "REMW", "REMUW", "AMOADD_D", "AMOSWAP_D", "AMOXOR_D", "AMOAND_D", "AMOOR_D", "AMOMIN_D", "AMOMINU_D", "AMOMAX_D", "AMOMAXU_D", "LR_D", "SC_D", "MUL", "MULH", "MULHU", "MULHSU", "DIV", "DIVU", "REM", "REMU", "AMOADD_W", "AMOXOR_W", "AMOSWAP_W", "AMOAND_W", "AMOOR_W", "AMOMIN_W", "AMOMINU_W", "AMOMAX_W", "AMOMAXU_W", "LR_W", "SC_W", "MAX", "MAXU", "MIN", "MINU", 
    )
    val insns_IMM_I: Seq[String] = Seq(
      "AES64KS1I", "HSV_D", "FLW", "Instructions32.BCLRI", "Instructions32.BEXTI", "Instructions32.BINVI", "Instructions32.BSETI", "Instructions32.RORI", "FLD", "FLH", "BCLRI", "BEXTI", "BINVI", "BSETI", "RORI", "RORIW", "JALR", "LB", "LH", "LW", "LBU", "LHU", "ADDI", "SLTI", "SLTIU", "ANDI", "ORI", "XORI", "Instructions32.SLLI", "Instructions32.SRLI", "Instructions32.SRAI", "SLLI_UW", "HSV_B", "HSV_H", "HSV_W", "LD", "LWU", "SLLI", "SRLI", "SRAI", "ADDIW", "SLLIW", "SRLIW", "SRAIW", 
    )
    val insns_IMM_S: Seq[String] = Seq(
      "FSW", "FSD", "FSH", "SB", "SH", "SW", "SD", 
    )
    val insns_IMM_SB: Seq[String] = Seq(
      "BNE", "BEQ", "BLT", "BLTU", "BGE", "BGEU", 
    )
    val insns_IMM_UJ: Seq[String] = Seq(
      "JAL", 
    )
    val insns_IMM_U: Seq[String] = Seq(
      "AUIPC", "LUI", 
    )
    val insns_IMM_Z: Seq[String] = Seq(
      "CSRRWI", "CSRRSI", "CSRRCI", 
    )
    def genTable(op: Op): BitPat = {
      if (insns_IMM_X.contains(op.bitPat)) IMM_X else if (insns_IMM_I.contains(op.bitPat)) IMM_I else if (insns_IMM_S.contains(op.bitPat)) IMM_S else if (insns_IMM_SB.contains(op.bitPat)) IMM_SB else if (insns_IMM_UJ.contains(op.bitPat)) IMM_UJ else if (insns_IMM_U.contains(op.bitPat)) IMM_U else if (insns_IMM_Z.contains(op.bitPat)) IMM_Z else IMM_X
    }
  }
  object decode_alu_dw extends BoolField {
    val insns_DW_XPR: Seq[String] = Seq(
      "SCIE.opcode", "AES64DS", "AES64DSM", "AES64IM", "AES64KS1I", "AES64KS2", "ROR", "ROL", "HLV_D", "HSV_D", "HLV_WU", "FLW", "FSW", "Instructions32.BCLRI", "Instructions32.BEXTI", "Instructions32.BINVI", "Instructions32.BSETI", "Instructions32.RORI", "SH1ADD", "SH2ADD", "SH3ADD", "ORC_B", "CUSTOM0", "CUSTOM0_RS1", "CUSTOM0_RS1_RS2", "CUSTOM0_RD", "CUSTOM0_RD_RS1", "CUSTOM0_RD_RS1_RS2", "CUSTOM1", "CUSTOM1_RS1", "CUSTOM1_RS1_RS2", "CUSTOM1_RD", "CUSTOM1_RD_RS1", "CUSTOM1_RD_RS1_RS2", "CUSTOM2", "CUSTOM2_RS1", "CUSTOM2_RS1_RS2", "CUSTOM2_RD", "CUSTOM2_RD_RS1", "CUSTOM2_RD_RS1_RS2", "CUSTOM3", "CUSTOM3_RS1", "CUSTOM3_RS1_RS2", "CUSTOM3_RD", "CUSTOM3_RD_RS1", "CUSTOM3_RD_RS1_RS2", "Instructions32.AES32ESI", "Instructions32.AES32ESMI", "FLD", "FSD", "Instructions32.AES32DSI", "Instructions32.AES32DSMI", "FLH", "FSH", "AES64ES", "AES64ESM", "CLMUL", "CLMULH", "BCLRI", "BEXTI", "BINVI", "BSETI", "RORI", "CFLUSH_D_L1", "CDISCARD_D_L1", "BNE", "BEQ", "BLT", "BLTU", "BGE", "BGEU", "JAL", "JALR", "AUIPC", "LB", "LH", "LW", "LBU", "LHU", "SB", "SH", "SW", "LUI", "ADDI", "SLTI", "SLTIU", "ANDI", "ORI", "XORI", "ADD", "SUB", "SLT", "SLTU", "AND", "OR", "XOR", "SLL", "SRL", "SRA", "CSRRW", "CSRRS", "CSRRC", "CSRRWI", "CSRRSI", "CSRRCI", "SEXT_H", "SEXT_B", "XPERM8", "XPERM4", "ZEXT_H", "ANDN", "ORN", "XNOR", "Instructions32.SLLI", "Instructions32.SRLI", "Instructions32.SRAI", "CLZ", "CTZ", "CPOP", "REV8", "SFENCE_VMA", "HFENCE_VVMA", "HFENCE_GVMA", "HLV_B", "HLV_BU", "HLV_H", "HLV_HU", "HLVX_HU", "HLV_W", "HLVX_WU", "HSV_B", "HSV_H", "HSV_W", "Instructions32.ZEXT_H", "CLMULR", "PACK", "PACKH", "BREV8", "BCLR", "BEXT", "BINV", "BSET", "Instructions32.REV8", "LD", "LWU", "SD", "SLLI", "SRLI", "SRAI", "AMOADD_D", "AMOSWAP_D", "AMOXOR_D", "AMOAND_D", "AMOOR_D", "AMOMIN_D", "AMOMINU_D", "AMOMAX_D", "AMOMAXU_D", "LR_D", "SC_D", "MUL", "MULH", "MULHU", "MULHSU", "DIV", "DIVU", "REM", "REMU", "AMOADD_W", "AMOXOR_W", "AMOSWAP_W", "AMOAND_W", "AMOOR_W", "AMOMIN_W", "AMOMINU_W", "AMOMAX_W", "AMOMAXU_W", "LR_W", "SC_W", "MAX", "MAXU", "MIN", "MINU", 
    )
    val insns_DW_32: Seq[String] = Seq(
      "PACKW", "CLZW", "CTZW", "CPOPW", "RORW", "ROLW", "RORIW", "ADDIW", "SLLIW", "SRLIW", "SRAIW", "ADDW", "SUBW", "SLLW", "SRLW", "SRAW", "MULW", "DIVW", "DIVUW", "REMW", "REMUW", 
    )
    val insns_DW_X: Seq[String] = Seq(
      "SHA512SIG0", "SHA512SIG1", "SHA512SUM0", "SHA512SUM1", "FSGNJ_S", "FSGNJX_S", "FSGNJN_S", "FMIN_S", "FMAX_S", "FADD_S", "FSUB_S", "FMUL_S", "FMADD_S", "FMSUB_S", "FNMADD_S", "FNMSUB_S", "FCLASS_S", "FMV_X_W", "FCVT_W_S", "FCVT_WU_S", "FEQ_S", "FLT_S", "FLE_S", "FMV_W_X", "FCVT_S_W", "FCVT_S_WU", "FDIV_S", "FSQRT_S", "SRET", "FCVT_S_D", "FCVT_D_S", "FSGNJ_D", "FSGNJX_D", "FSGNJN_D", "FMIN_D", "FMAX_D", "FADD_D", "FSUB_D", "FMUL_D", "FMADD_D", "FMSUB_D", "FNMADD_D", "FNMSUB_D", "FCLASS_D", "FCVT_W_D", "FCVT_WU_D", "FEQ_D", "FLT_D", "FLE_D", "FCVT_D_W", "FCVT_D_WU", "FDIV_D", "FSQRT_D", "Instructions32.ZIP", "Instructions32.UNZIP", "FCVT_L_S", "FCVT_LU_S", "FCVT_S_L", "FCVT_S_LU", "Instructions32.SHA512SIG0L", "Instructions32.SHA512SIG1L", "Instructions32.SHA512SIG0H", "Instructions32.SHA512SIG1H", "Instructions32.SHA512SUM0R", "Instructions32.SHA512SUM1R", "FCVT_S_H", "FCVT_H_S", "FSGNJ_H", "FSGNJX_H", "FSGNJN_H", "FMIN_H", "FMAX_H", "FADD_H", "FSUB_H", "FMUL_H", "FMADD_H", "FMSUB_H", "FNMADD_H", "FNMSUB_H", "FCLASS_H", "FMV_X_H", "FCVT_W_H", "FCVT_WU_H", "FEQ_H", "FLT_H", "FLE_H", "FMV_H_X", "FCVT_H_W", "FCVT_H_WU", "FDIV_H", "FSQRT_H", "FCVT_D_H", "FCVT_H_D", "FENCE", "ECALL", "EBREAK", "MRET", "WFI", "CEASE", "FMV_X_D", "FCVT_L_D", "FCVT_LU_D", "FMV_D_X", "FCVT_D_L", "FCVT_D_LU", "MNRET", "FCVT_L_H", "FCVT_LU_H", "FCVT_H_L", "FCVT_H_LU", "FENCE_I", "SM4ED", "SM4KS", "SM3P0", "SM3P1", "DRET", "SHA256SIG0", "SHA256SIG1", "SHA256SUM0", "SHA256SUM1", 
    )
    val insns_DW_64: Seq[String] = Seq(
      "ADD_UW", "SLLI_UW", "SH1ADD_UW", "SH2ADD_UW", "SH3ADD_UW", 
    )
    def genTable(op: Op): BitPat = {
      if (insns_DW_XPR.contains(op.bitPat)) DW_XPR else if (insns_DW_32.contains(op.bitPat)) DW_32 else if (insns_DW_X.contains(op.bitPat)) DW_X else if (insns_DW_64.contains(op.bitPat)) DW_64 else DW_X
    }
  }
  object decode_alu_fn extends BitsField_4 {
    val insns_aluFn_FN_X: Seq[String] = Seq(
      "SCIE.opcode", "FSGNJ_S", "FSGNJX_S", "FSGNJN_S", "FMIN_S", "FMAX_S", "FADD_S", "FSUB_S", "FMUL_S", "FMADD_S", "FMSUB_S", "FNMADD_S", "FNMSUB_S", "FCLASS_S", "FMV_X_W", "FCVT_W_S", "FCVT_WU_S", "FEQ_S", "FLT_S", "FLE_S", "FMV_W_X", "FCVT_S_W", "FCVT_S_WU", "FDIV_S", "FSQRT_S", "SRET", "FCVT_S_D", "FCVT_D_S", "FSGNJ_D", "FSGNJX_D", "FSGNJN_D", "FMIN_D", "FMAX_D", "FADD_D", "FSUB_D", "FMUL_D", "FMADD_D", "FMSUB_D", "FNMADD_D", "FNMSUB_D", "FCLASS_D", "FCVT_W_D", "FCVT_WU_D", "FEQ_D", "FLT_D", "FLE_D", "FCVT_D_W", "FCVT_D_WU", "FDIV_D", "FSQRT_D", "FCVT_L_S", "FCVT_LU_S", "FCVT_S_L", "FCVT_S_LU", "FCVT_S_H", "FCVT_H_S", "FSGNJ_H", "FSGNJX_H", "FSGNJN_H", "FMIN_H", "FMAX_H", "FADD_H", "FSUB_H", "FMUL_H", "FMADD_H", "FMSUB_H", "FNMADD_H", "FNMSUB_H", "FCLASS_H", "FMV_X_H", "FCVT_W_H", "FCVT_WU_H", "FEQ_H", "FLT_H", "FLE_H", "FMV_H_X", "FCVT_H_W", "FCVT_H_WU", "FDIV_H", "FSQRT_H", "FCVT_D_H", "FCVT_H_D", "FENCE", "ECALL", "EBREAK", "MRET", "WFI", "CEASE", "FMV_X_D", "FCVT_L_D", "FCVT_LU_D", "FMV_D_X", "FCVT_D_L", "FCVT_D_LU", "MNRET", "FCVT_L_H", "FCVT_LU_H", "FCVT_H_L", "FCVT_H_LU", "FENCE_I", "DRET", 
    )
    val insns_aluFn_FN_AES_DS: Seq[String] = Seq(
      "AES64DS", "Instructions32.AES32DSI", 
    )
    val insns_aluFn_FN_AES_DSM: Seq[String] = Seq(
      "AES64DSM", "Instructions32.AES32DSMI", 
    )
    val insns_aluFn_FN_AES_IM: Seq[String] = Seq(
      "AES64IM", 
    )
    val insns_aluFn_FN_AES_KS1: Seq[String] = Seq(
      "AES64KS1I", 
    )
    val insns_aluFn_FN_AES_KS2: Seq[String] = Seq(
      "AES64KS2", 
    )
    val insns_aluFn_FN_PACK: Seq[String] = Seq(
      "PACKW", "PACK", 
    )
    val insns_aluFn_FN_SHA512_SIG0: Seq[String] = Seq(
      "SHA512SIG0", "Instructions32.SHA512SIG0L", "Instructions32.SHA512SIG0H", 
    )
    val insns_aluFn_FN_SHA512_SIG1: Seq[String] = Seq(
      "SHA512SIG1", "Instructions32.SHA512SIG1L", "Instructions32.SHA512SIG1H", 
    )
    val insns_aluFn_FN_SHA512_SUM0: Seq[String] = Seq(
      "SHA512SUM0", "Instructions32.SHA512SUM0R", 
    )
    val insns_aluFn_FN_SHA512_SUM1: Seq[String] = Seq(
      "SHA512SUM1", "Instructions32.SHA512SUM1R", 
    )
    val insns_aluFn_FN_ROR: Seq[String] = Seq(
      "ROR", "Instructions32.RORI", "RORI", "RORW", "RORIW", 
    )
    val insns_aluFn_FN_ROL: Seq[String] = Seq(
      "ROL", "ROLW", 
    )
    val insns_aluFn_FN_ADD: Seq[String] = Seq(
      "HLV_D", "HSV_D", "HLV_WU", "FLW", "FSW", "CUSTOM0", "CUSTOM0_RS1", "CUSTOM0_RS1_RS2", "CUSTOM0_RD", "CUSTOM0_RD_RS1", "CUSTOM0_RD_RS1_RS2", "CUSTOM1", "CUSTOM1_RS1", "CUSTOM1_RS1_RS2", "CUSTOM1_RD", "CUSTOM1_RD_RS1", "CUSTOM1_RD_RS1_RS2", "CUSTOM2", "CUSTOM2_RS1", "CUSTOM2_RS1_RS2", "CUSTOM2_RD", "CUSTOM2_RD_RS1", "CUSTOM2_RD_RS1_RS2", "CUSTOM3", "CUSTOM3_RS1", "CUSTOM3_RS1_RS2", "CUSTOM3_RD", "CUSTOM3_RD_RS1", "CUSTOM3_RD_RS1_RS2", "FLD", "FSD", "FLH", "FSH", "CFLUSH_D_L1", "CDISCARD_D_L1", "JAL", "JALR", "AUIPC", "LB", "LH", "LW", "LBU", "LHU", "SB", "SH", "SW", "LUI", "ADDI", "ADD", "CSRRW", "CSRRS", "CSRRC", "CSRRWI", "CSRRSI", "CSRRCI", "SFENCE_VMA", "HFENCE_VVMA", "HFENCE_GVMA", "HLV_B", "HLV_BU", "HLV_H", "HLV_HU", "HLVX_HU", "HLV_W", "HLVX_WU", "HSV_B", "HSV_H", "HSV_W", "LD", "LWU", "SD", "ADDIW", "ADDW", "AMOADD_D", "AMOSWAP_D", "AMOXOR_D", "AMOAND_D", "AMOOR_D", "AMOMIN_D", "AMOMINU_D", "AMOMAX_D", "AMOMAXU_D", "LR_D", "SC_D", "AMOADD_W", "AMOXOR_W", "AMOSWAP_W", "AMOAND_W", "AMOOR_W", "AMOMIN_W", "AMOMINU_W", "AMOMAX_W", "AMOMAXU_W", "LR_W", "SC_W", 
    )
    val insns_aluFn_FN_CLZ: Seq[String] = Seq(
      "CLZW", "CLZ", 
    )
    val insns_aluFn_FN_CTZ: Seq[String] = Seq(
      "CTZW", "CTZ", 
    )
    val insns_aluFn_FN_CPOP: Seq[String] = Seq(
      "CPOPW", "CPOP", 
    )
    val insns_aluFn_FN_BCLR: Seq[String] = Seq(
      "Instructions32.BCLRI", "BCLRI", "BCLR", 
    )
    val insns_aluFn_FN_BEXT: Seq[String] = Seq(
      "Instructions32.BEXTI", "BEXTI", "BEXT", 
    )
    val insns_aluFn_FN_BINV: Seq[String] = Seq(
      "Instructions32.BINVI", "BINVI", "BINV", 
    )
    val insns_aluFn_FN_BSET: Seq[String] = Seq(
      "Instructions32.BSETI", "BSETI", "BSET", 
    )
    val insns_aluFn_FN_SH1ADD: Seq[String] = Seq(
      "SH1ADD", 
    )
    val insns_aluFn_FN_SH2ADD: Seq[String] = Seq(
      "SH2ADD", 
    )
    val insns_aluFn_FN_SH3ADD: Seq[String] = Seq(
      "SH3ADD", 
    )
    val insns_aluFn_FN_ORCB: Seq[String] = Seq(
      "ORC_B", 
    )
    val insns_aluFn_FN_AES_ES: Seq[String] = Seq(
      "Instructions32.AES32ESI", "AES64ES", 
    )
    val insns_aluFn_FN_AES_ESM: Seq[String] = Seq(
      "Instructions32.AES32ESMI", "AES64ESM", 
    )
    val insns_aluFn_FN_ZIP: Seq[String] = Seq(
      "Instructions32.ZIP", 
    )
    val insns_aluFn_FN_UNZIP: Seq[String] = Seq(
      "Instructions32.UNZIP", 
    )
    val insns_aluFn_FN_CLMUL: Seq[String] = Seq(
      "CLMUL", 
    )
    val insns_aluFn_FN_CLMULH: Seq[String] = Seq(
      "CLMULH", 
    )
    val insns_aluFn_FN_SNE: Seq[String] = Seq(
      "BNE", 
    )
    val insns_aluFn_FN_SEQ: Seq[String] = Seq(
      "BEQ", 
    )
    val insns_aluFn_FN_SLT: Seq[String] = Seq(
      "BLT", "SLTI", "SLT", 
    )
    val insns_aluFn_FN_SLTU: Seq[String] = Seq(
      "BLTU", "SLTIU", "SLTU", 
    )
    val insns_aluFn_FN_SGE: Seq[String] = Seq(
      "BGE", 
    )
    val insns_aluFn_FN_SGEU: Seq[String] = Seq(
      "BGEU", 
    )
    val insns_aluFn_FN_AND: Seq[String] = Seq(
      "ANDI", "AND", 
    )
    val insns_aluFn_FN_OR: Seq[String] = Seq(
      "ORI", "OR", 
    )
    val insns_aluFn_FN_XOR: Seq[String] = Seq(
      "XORI", "XOR", 
    )
    val insns_aluFn_FN_SUB: Seq[String] = Seq(
      "SUB", "SUBW", 
    )
    val insns_aluFn_FN_SL: Seq[String] = Seq(
      "SLL", "Instructions32.SLLI", "SLLI", "SLLIW", "SLLW", 
    )
    val insns_aluFn_FN_SR: Seq[String] = Seq(
      "SRL", "Instructions32.SRLI", "SRLI", "SRLIW", "SRLW", 
    )
    val insns_aluFn_FN_SRA: Seq[String] = Seq(
      "SRA", "Instructions32.SRAI", "SRAI", "SRAIW", "SRAW", 
    )
    val insns_aluFn_FN_SEXTH: Seq[String] = Seq(
      "SEXT_H", 
    )
    val insns_aluFn_FN_SEXTB: Seq[String] = Seq(
      "SEXT_B", 
    )
    val insns_aluFn_FN_XPERM8: Seq[String] = Seq(
      "XPERM8", 
    )
    val insns_aluFn_FN_XPERM4: Seq[String] = Seq(
      "XPERM4", 
    )
    val insns_aluFn_FN_ZEXTH: Seq[String] = Seq(
      "ZEXT_H", "Instructions32.ZEXT_H", 
    )
    val insns_aluFn_FN_ANDN: Seq[String] = Seq(
      "ANDN", 
    )
    val insns_aluFn_FN_ORN: Seq[String] = Seq(
      "ORN", 
    )
    val insns_aluFn_FN_XNOR: Seq[String] = Seq(
      "XNOR", 
    )
    val insns_aluFn_FN_SM4ED: Seq[String] = Seq(
      "SM4ED", 
    )
    val insns_aluFn_FN_SM4KS: Seq[String] = Seq(
      "SM4KS", 
    )
    val insns_aluFn_FN_SM3P0: Seq[String] = Seq(
      "SM3P0", 
    )
    val insns_aluFn_FN_SM3P1: Seq[String] = Seq(
      "SM3P1", 
    )
    val insns_aluFn_FN_REV8: Seq[String] = Seq(
      "REV8", "Instructions32.REV8", 
    )
    val insns_aluFn_FN_ADDUW: Seq[String] = Seq(
      "ADD_UW", 
    )
    val insns_aluFn_FN_SLLIUW: Seq[String] = Seq(
      "SLLI_UW", 
    )
    val insns_aluFn_FN_SH1ADDUW: Seq[String] = Seq(
      "SH1ADD_UW", 
    )
    val insns_aluFn_FN_SH2ADDUW: Seq[String] = Seq(
      "SH2ADD_UW", 
    )
    val insns_aluFn_FN_SH3ADDUW: Seq[String] = Seq(
      "SH3ADD_UW", 
    )
    val insns_aluFn_FN_CLMULR: Seq[String] = Seq(
      "CLMULR", 
    )
    val insns_aluFn_FN_PACKH: Seq[String] = Seq(
      "PACKH", 
    )
    val insns_aluFn_FN_BREV8: Seq[String] = Seq(
      "BREV8", 
    )
    val insns_aluFn_FN_SHA256_SIG0: Seq[String] = Seq(
      "SHA256SIG0", 
    )
    val insns_aluFn_FN_SHA256_SIG1: Seq[String] = Seq(
      "SHA256SIG1", 
    )
    val insns_aluFn_FN_SHA256_SUM0: Seq[String] = Seq(
      "SHA256SUM0", 
    )
    val insns_aluFn_FN_SHA256_SUM1: Seq[String] = Seq(
      "SHA256SUM1", 
    )
    val insns_aluFn_FN_MUL: Seq[String] = Seq(
      "MULW", "MUL", 
    )
    val insns_aluFn_FN_DIV: Seq[String] = Seq(
      "DIVW", "DIV", 
    )
    val insns_aluFn_FN_DIVU: Seq[String] = Seq(
      "DIVUW", "DIVU", 
    )
    val insns_aluFn_FN_REM: Seq[String] = Seq(
      "REMW", "REM", 
    )
    val insns_aluFn_FN_REMU: Seq[String] = Seq(
      "REMUW", "REMU", 
    )
    val insns_aluFn_FN_MULH: Seq[String] = Seq(
      "MULH", 
    )
    val insns_aluFn_FN_MULHU: Seq[String] = Seq(
      "MULHU", 
    )
    val insns_aluFn_FN_MULHSU: Seq[String] = Seq(
      "MULHSU", 
    )
    val insns_aluFn_FN_MAX: Seq[String] = Seq(
      "MAX", 
    )
    val insns_aluFn_FN_MAXU: Seq[String] = Seq(
      "MAXU", 
    )
    val insns_aluFn_FN_MIN: Seq[String] = Seq(
      "MIN", 
    )
    val insns_aluFn_FN_MINU: Seq[String] = Seq(
      "MINU", 
    )
    def genTable(op: Op): BitPat = {
      if (insns_aluFn_FN_X.contains(op.bitPat)) aluFn.FN_X else if (insns_aluFn_FN_AES_DS.contains(op.bitPat)) aluFn.FN_AES_DS else if (insns_aluFn_FN_AES_DSM.contains(op.bitPat)) aluFn.FN_AES_DSM else if (insns_aluFn_FN_AES_IM.contains(op.bitPat)) aluFn.FN_AES_IM else if (insns_aluFn_FN_AES_KS1.contains(op.bitPat)) aluFn.FN_AES_KS1 else if (insns_aluFn_FN_AES_KS2.contains(op.bitPat)) aluFn.FN_AES_KS2 else if (insns_aluFn_FN_PACK.contains(op.bitPat)) aluFn.FN_PACK else if (insns_aluFn_FN_SHA512_SIG0.contains(op.bitPat)) aluFn.FN_SHA512_SIG0 else if (insns_aluFn_FN_SHA512_SIG1.contains(op.bitPat)) aluFn.FN_SHA512_SIG1 else if (insns_aluFn_FN_SHA512_SUM0.contains(op.bitPat)) aluFn.FN_SHA512_SUM0 else if (insns_aluFn_FN_SHA512_SUM1.contains(op.bitPat)) aluFn.FN_SHA512_SUM1 else if (insns_aluFn_FN_ROR.contains(op.bitPat)) aluFn.FN_ROR else if (insns_aluFn_FN_ROL.contains(op.bitPat)) aluFn.FN_ROL else if (insns_aluFn_FN_ADD.contains(op.bitPat)) aluFn.FN_ADD else if (insns_aluFn_FN_CLZ.contains(op.bitPat)) aluFn.FN_CLZ else if (insns_aluFn_FN_CTZ.contains(op.bitPat)) aluFn.FN_CTZ else if (insns_aluFn_FN_CPOP.contains(op.bitPat)) aluFn.FN_CPOP else if (insns_aluFn_FN_BCLR.contains(op.bitPat)) aluFn.FN_BCLR else if (insns_aluFn_FN_BEXT.contains(op.bitPat)) aluFn.FN_BEXT else if (insns_aluFn_FN_BINV.contains(op.bitPat)) aluFn.FN_BINV else if (insns_aluFn_FN_BSET.contains(op.bitPat)) aluFn.FN_BSET else if (insns_aluFn_FN_SH1ADD.contains(op.bitPat)) aluFn.FN_SH1ADD else if (insns_aluFn_FN_SH2ADD.contains(op.bitPat)) aluFn.FN_SH2ADD else if (insns_aluFn_FN_SH3ADD.contains(op.bitPat)) aluFn.FN_SH3ADD else if (insns_aluFn_FN_ORCB.contains(op.bitPat)) aluFn.FN_ORCB else if (insns_aluFn_FN_AES_ES.contains(op.bitPat)) aluFn.FN_AES_ES else if (insns_aluFn_FN_AES_ESM.contains(op.bitPat)) aluFn.FN_AES_ESM else if (insns_aluFn_FN_ZIP.contains(op.bitPat)) aluFn.FN_ZIP else if (insns_aluFn_FN_UNZIP.contains(op.bitPat)) aluFn.FN_UNZIP else if (insns_aluFn_FN_CLMUL.contains(op.bitPat)) aluFn.FN_CLMUL else if (insns_aluFn_FN_CLMULH.contains(op.bitPat)) aluFn.FN_CLMULH else if (insns_aluFn_FN_SNE.contains(op.bitPat)) aluFn.FN_SNE else if (insns_aluFn_FN_SEQ.contains(op.bitPat)) aluFn.FN_SEQ else if (insns_aluFn_FN_SLT.contains(op.bitPat)) aluFn.FN_SLT else if (insns_aluFn_FN_SLTU.contains(op.bitPat)) aluFn.FN_SLTU else if (insns_aluFn_FN_SGE.contains(op.bitPat)) aluFn.FN_SGE else if (insns_aluFn_FN_SGEU.contains(op.bitPat)) aluFn.FN_SGEU else if (insns_aluFn_FN_AND.contains(op.bitPat)) aluFn.FN_AND else if (insns_aluFn_FN_OR.contains(op.bitPat)) aluFn.FN_OR else if (insns_aluFn_FN_XOR.contains(op.bitPat)) aluFn.FN_XOR else if (insns_aluFn_FN_SUB.contains(op.bitPat)) aluFn.FN_SUB else if (insns_aluFn_FN_SL.contains(op.bitPat)) aluFn.FN_SL else if (insns_aluFn_FN_SR.contains(op.bitPat)) aluFn.FN_SR else if (insns_aluFn_FN_SRA.contains(op.bitPat)) aluFn.FN_SRA else if (insns_aluFn_FN_SEXTH.contains(op.bitPat)) aluFn.FN_SEXTH else if (insns_aluFn_FN_SEXTB.contains(op.bitPat)) aluFn.FN_SEXTB else if (insns_aluFn_FN_XPERM8.contains(op.bitPat)) aluFn.FN_XPERM8 else if (insns_aluFn_FN_XPERM4.contains(op.bitPat)) aluFn.FN_XPERM4 else if (insns_aluFn_FN_ZEXTH.contains(op.bitPat)) aluFn.FN_ZEXTH else if (insns_aluFn_FN_ANDN.contains(op.bitPat)) aluFn.FN_ANDN else if (insns_aluFn_FN_ORN.contains(op.bitPat)) aluFn.FN_ORN else if (insns_aluFn_FN_XNOR.contains(op.bitPat)) aluFn.FN_XNOR else if (insns_aluFn_FN_SM4ED.contains(op.bitPat)) aluFn.FN_SM4ED else if (insns_aluFn_FN_SM4KS.contains(op.bitPat)) aluFn.FN_SM4KS else if (insns_aluFn_FN_SM3P0.contains(op.bitPat)) aluFn.FN_SM3P0 else if (insns_aluFn_FN_SM3P1.contains(op.bitPat)) aluFn.FN_SM3P1 else if (insns_aluFn_FN_REV8.contains(op.bitPat)) aluFn.FN_REV8 else if (insns_aluFn_FN_ADDUW.contains(op.bitPat)) aluFn.FN_ADDUW else if (insns_aluFn_FN_SLLIUW.contains(op.bitPat)) aluFn.FN_SLLIUW else if (insns_aluFn_FN_SH1ADDUW.contains(op.bitPat)) aluFn.FN_SH1ADDUW else if (insns_aluFn_FN_SH2ADDUW.contains(op.bitPat)) aluFn.FN_SH2ADDUW else if (insns_aluFn_FN_SH3ADDUW.contains(op.bitPat)) aluFn.FN_SH3ADDUW else if (insns_aluFn_FN_CLMULR.contains(op.bitPat)) aluFn.FN_CLMULR else if (insns_aluFn_FN_PACKH.contains(op.bitPat)) aluFn.FN_PACKH else if (insns_aluFn_FN_BREV8.contains(op.bitPat)) aluFn.FN_BREV8 else if (insns_aluFn_FN_SHA256_SIG0.contains(op.bitPat)) aluFn.FN_SHA256_SIG0 else if (insns_aluFn_FN_SHA256_SIG1.contains(op.bitPat)) aluFn.FN_SHA256_SIG1 else if (insns_aluFn_FN_SHA256_SUM0.contains(op.bitPat)) aluFn.FN_SHA256_SUM0 else if (insns_aluFn_FN_SHA256_SUM1.contains(op.bitPat)) aluFn.FN_SHA256_SUM1 else if (insns_aluFn_FN_MUL.contains(op.bitPat)) aluFn.FN_MUL else if (insns_aluFn_FN_DIV.contains(op.bitPat)) aluFn.FN_DIV else if (insns_aluFn_FN_DIVU.contains(op.bitPat)) aluFn.FN_DIVU else if (insns_aluFn_FN_REM.contains(op.bitPat)) aluFn.FN_REM else if (insns_aluFn_FN_REMU.contains(op.bitPat)) aluFn.FN_REMU else if (insns_aluFn_FN_MULH.contains(op.bitPat)) aluFn.FN_MULH else if (insns_aluFn_FN_MULHU.contains(op.bitPat)) aluFn.FN_MULHU else if (insns_aluFn_FN_MULHSU.contains(op.bitPat)) aluFn.FN_MULHSU else if (insns_aluFn_FN_MAX.contains(op.bitPat)) aluFn.FN_MAX else if (insns_aluFn_FN_MAXU.contains(op.bitPat)) aluFn.FN_MAXU else if (insns_aluFn_FN_MIN.contains(op.bitPat)) aluFn.FN_MIN else if (insns_aluFn_FN_MINU.contains(op.bitPat)) aluFn.FN_MINU else aluFn.FN_X
    }
  }
  object decode_mem extends BoolField {
    val insns_N: Seq[String] = Seq(
      "SCIE.opcode", "AES64DS", "AES64DSM", "AES64IM", "AES64KS1I", "AES64KS2", "PACKW", "SHA512SIG0", "SHA512SIG1", "SHA512SUM0", "SHA512SUM1", "ROR", "ROL", "FSGNJ_S", "FSGNJX_S", "FSGNJN_S", "FMIN_S", "FMAX_S", "FADD_S", "FSUB_S", "FMUL_S", "FMADD_S", "FMSUB_S", "FNMADD_S", "FNMSUB_S", "FCLASS_S", "FMV_X_W", "FCVT_W_S", "FCVT_WU_S", "FEQ_S", "FLT_S", "FLE_S", "FMV_W_X", "FCVT_S_W", "FCVT_S_WU", "FDIV_S", "FSQRT_S", "CLZW", "CTZW", "CPOPW", "SRET", "Instructions32.BCLRI", "Instructions32.BEXTI", "Instructions32.BINVI", "Instructions32.BSETI", "Instructions32.RORI", "SH1ADD", "SH2ADD", "SH3ADD", "ORC_B", "CUSTOM0", "CUSTOM0_RS1", "CUSTOM0_RS1_RS2", "CUSTOM0_RD", "CUSTOM0_RD_RS1", "CUSTOM0_RD_RS1_RS2", "CUSTOM1", "CUSTOM1_RS1", "CUSTOM1_RS1_RS2", "CUSTOM1_RD", "CUSTOM1_RD_RS1", "CUSTOM1_RD_RS1_RS2", "CUSTOM2", "CUSTOM2_RS1", "CUSTOM2_RS1_RS2", "CUSTOM2_RD", "CUSTOM2_RD_RS1", "CUSTOM2_RD_RS1_RS2", "CUSTOM3", "CUSTOM3_RS1", "CUSTOM3_RS1_RS2", "CUSTOM3_RD", "CUSTOM3_RD_RS1", "CUSTOM3_RD_RS1_RS2", "Instructions32.AES32ESI", "Instructions32.AES32ESMI", "FCVT_S_D", "FCVT_D_S", "FSGNJ_D", "FSGNJX_D", "FSGNJN_D", "FMIN_D", "FMAX_D", "FADD_D", "FSUB_D", "FMUL_D", "FMADD_D", "FMSUB_D", "FNMADD_D", "FNMSUB_D", "FCLASS_D", "FCVT_W_D", "FCVT_WU_D", "FEQ_D", "FLT_D", "FLE_D", "FCVT_D_W", "FCVT_D_WU", "FDIV_D", "FSQRT_D", "Instructions32.ZIP", "Instructions32.UNZIP", "FCVT_L_S", "FCVT_LU_S", "FCVT_S_L", "FCVT_S_LU", "Instructions32.AES32DSI", "Instructions32.AES32DSMI", "Instructions32.SHA512SIG0L", "Instructions32.SHA512SIG1L", "Instructions32.SHA512SIG0H", "Instructions32.SHA512SIG1H", "Instructions32.SHA512SUM0R", "Instructions32.SHA512SUM1R", "FCVT_S_H", "FCVT_H_S", "FSGNJ_H", "FSGNJX_H", "FSGNJN_H", "FMIN_H", "FMAX_H", "FADD_H", "FSUB_H", "FMUL_H", "FMADD_H", "FMSUB_H", "FNMADD_H", "FNMSUB_H", "FCLASS_H", "FMV_X_H", "FCVT_W_H", "FCVT_WU_H", "FEQ_H", "FLT_H", "FLE_H", "FMV_H_X", "FCVT_H_W", "FCVT_H_WU", "FDIV_H", "FSQRT_H", "AES64ES", "AES64ESM", "CLMUL", "CLMULH", "BCLRI", "BEXTI", "BINVI", "BSETI", "RORI", "RORW", "ROLW", "RORIW", "FCVT_D_H", "FCVT_H_D", "BNE", "BEQ", "BLT", "BLTU", "BGE", "BGEU", "JAL", "JALR", "AUIPC", "LUI", "ADDI", "SLTI", "SLTIU", "ANDI", "ORI", "XORI", "ADD", "SUB", "SLT", "SLTU", "AND", "OR", "XOR", "SLL", "SRL", "SRA", "FENCE", "ECALL", "EBREAK", "MRET", "WFI", "CEASE", "CSRRW", "CSRRS", "CSRRC", "CSRRWI", "CSRRSI", "CSRRCI", "SEXT_H", "SEXT_B", "XPERM8", "XPERM4", "FMV_X_D", "FCVT_L_D", "FCVT_LU_D", "FMV_D_X", "FCVT_D_L", "FCVT_D_LU", "ZEXT_H", "MNRET", "FCVT_L_H", "FCVT_LU_H", "FCVT_H_L", "FCVT_H_LU", "ANDN", "ORN", "XNOR", "Instructions32.SLLI", "Instructions32.SRLI", "Instructions32.SRAI", "SM4ED", "SM4KS", "SM3P0", "SM3P1", "DRET", "CLZ", "CTZ", "CPOP", "REV8", "ADD_UW", "SLLI_UW", "SH1ADD_UW", "SH2ADD_UW", "SH3ADD_UW", "Instructions32.ZEXT_H", "CLMULR", "PACK", "PACKH", "BREV8", "SHA256SIG0", "SHA256SIG1", "SHA256SUM0", "SHA256SUM1", "BCLR", "BEXT", "BINV", "BSET", "Instructions32.REV8", "SLLI", "SRLI", "SRAI", "ADDIW", "SLLIW", "SRLIW", "SRAIW", "ADDW", "SUBW", "SLLW", "SRLW", "SRAW", "MULW", "DIVW", "DIVUW", "REMW", "REMUW", "MUL", "MULH", "MULHU", "MULHSU", "DIV", "DIVU", "REM", "REMU", "MAX", "MAXU", "MIN", "MINU", 
    )
    val insns_Y: Seq[String] = Seq(
      "HLV_D", "HSV_D", "HLV_WU", "FLW", "FSW", "FLD", "FSD", "FLH", "FSH", "CFLUSH_D_L1", "CDISCARD_D_L1", "LB", "LH", "LW", "LBU", "LHU", "SB", "SH", "SW", "SFENCE_VMA", "HFENCE_VVMA", "HFENCE_GVMA", "HLV_B", "HLV_BU", "HLV_H", "HLV_HU", "HLVX_HU", "HLV_W", "HLVX_WU", "HSV_B", "HSV_H", "HSV_W", "LD", "LWU", "SD", "AMOADD_D", "AMOSWAP_D", "AMOXOR_D", "AMOAND_D", "AMOOR_D", "AMOMIN_D", "AMOMINU_D", "AMOMAX_D", "AMOMAXU_D", "LR_D", "SC_D", "AMOADD_W", "AMOXOR_W", "AMOSWAP_W", "AMOAND_W", "AMOOR_W", "AMOMIN_W", "AMOMINU_W", "AMOMAX_W", "AMOMAXU_W", "LR_W", "SC_W", 
    )
    val insns_v: Seq[String] = Seq(
      "FENCE_I", 
    )
    def genTable(op: Op): BitPat = {
      if (insns_N.contains(op.bitPat)) N else if (insns_Y.contains(op.bitPat)) Y else if (insns_v.contains(op.bitPat)) v else N
    }
  }
  object decode_mem_cmd extends BitsField_5 {
    val insns_M_X: Seq[String] = Seq(
      "SCIE.opcode", "AES64DS", "AES64DSM", "AES64IM", "AES64KS1I", "AES64KS2", "PACKW", "SHA512SIG0", "SHA512SIG1", "SHA512SUM0", "SHA512SUM1", "ROR", "ROL", "FSGNJ_S", "FSGNJX_S", "FSGNJN_S", "FMIN_S", "FMAX_S", "FADD_S", "FSUB_S", "FMUL_S", "FMADD_S", "FMSUB_S", "FNMADD_S", "FNMSUB_S", "FCLASS_S", "FMV_X_W", "FCVT_W_S", "FCVT_WU_S", "FEQ_S", "FLT_S", "FLE_S", "FMV_W_X", "FCVT_S_W", "FCVT_S_WU", "FDIV_S", "FSQRT_S", "CLZW", "CTZW", "CPOPW", "SRET", "Instructions32.BCLRI", "Instructions32.BEXTI", "Instructions32.BINVI", "Instructions32.BSETI", "Instructions32.RORI", "SH1ADD", "SH2ADD", "SH3ADD", "ORC_B", "CUSTOM0", "CUSTOM0_RS1", "CUSTOM0_RS1_RS2", "CUSTOM0_RD", "CUSTOM0_RD_RS1", "CUSTOM0_RD_RS1_RS2", "CUSTOM1", "CUSTOM1_RS1", "CUSTOM1_RS1_RS2", "CUSTOM1_RD", "CUSTOM1_RD_RS1", "CUSTOM1_RD_RS1_RS2", "CUSTOM2", "CUSTOM2_RS1", "CUSTOM2_RS1_RS2", "CUSTOM2_RD", "CUSTOM2_RD_RS1", "CUSTOM2_RD_RS1_RS2", "CUSTOM3", "CUSTOM3_RS1", "CUSTOM3_RS1_RS2", "CUSTOM3_RD", "CUSTOM3_RD_RS1", "CUSTOM3_RD_RS1_RS2", "Instructions32.AES32ESI", "Instructions32.AES32ESMI", "FCVT_S_D", "FCVT_D_S", "FSGNJ_D", "FSGNJX_D", "FSGNJN_D", "FMIN_D", "FMAX_D", "FADD_D", "FSUB_D", "FMUL_D", "FMADD_D", "FMSUB_D", "FNMADD_D", "FNMSUB_D", "FCLASS_D", "FCVT_W_D", "FCVT_WU_D", "FEQ_D", "FLT_D", "FLE_D", "FCVT_D_W", "FCVT_D_WU", "FDIV_D", "FSQRT_D", "Instructions32.ZIP", "Instructions32.UNZIP", "FCVT_L_S", "FCVT_LU_S", "FCVT_S_L", "FCVT_S_LU", "Instructions32.AES32DSI", "Instructions32.AES32DSMI", "Instructions32.SHA512SIG0L", "Instructions32.SHA512SIG1L", "Instructions32.SHA512SIG0H", "Instructions32.SHA512SIG1H", "Instructions32.SHA512SUM0R", "Instructions32.SHA512SUM1R", "FCVT_S_H", "FCVT_H_S", "FSGNJ_H", "FSGNJX_H", "FSGNJN_H", "FMIN_H", "FMAX_H", "FADD_H", "FSUB_H", "FMUL_H", "FMADD_H", "FMSUB_H", "FNMADD_H", "FNMSUB_H", "FCLASS_H", "FMV_X_H", "FCVT_W_H", "FCVT_WU_H", "FEQ_H", "FLT_H", "FLE_H", "FMV_H_X", "FCVT_H_W", "FCVT_H_WU", "FDIV_H", "FSQRT_H", "AES64ES", "AES64ESM", "CLMUL", "CLMULH", "BCLRI", "BEXTI", "BINVI", "BSETI", "RORI", "RORW", "ROLW", "RORIW", "FCVT_D_H", "FCVT_H_D", "BNE", "BEQ", "BLT", "BLTU", "BGE", "BGEU", "JAL", "JALR", "AUIPC", "LUI", "ADDI", "SLTI", "SLTIU", "ANDI", "ORI", "XORI", "ADD", "SUB", "SLT", "SLTU", "AND", "OR", "XOR", "SLL", "SRL", "SRA", "FENCE", "ECALL", "EBREAK", "MRET", "WFI", "CEASE", "CSRRW", "CSRRS", "CSRRC", "CSRRWI", "CSRRSI", "CSRRCI", "SEXT_H", "SEXT_B", "XPERM8", "XPERM4", "FMV_X_D", "FCVT_L_D", "FCVT_LU_D", "FMV_D_X", "FCVT_D_L", "FCVT_D_LU", "ZEXT_H", "MNRET", "FCVT_L_H", "FCVT_LU_H", "FCVT_H_L", "FCVT_H_LU", "ANDN", "ORN", "XNOR", "Instructions32.SLLI", "Instructions32.SRLI", "Instructions32.SRAI", "SM4ED", "SM4KS", "SM3P0", "SM3P1", "DRET", "CLZ", "CTZ", "CPOP", "REV8", "ADD_UW", "SLLI_UW", "SH1ADD_UW", "SH2ADD_UW", "SH3ADD_UW", "Instructions32.ZEXT_H", "CLMULR", "PACK", "PACKH", "BREV8", "SHA256SIG0", "SHA256SIG1", "SHA256SUM0", "SHA256SUM1", "BCLR", "BEXT", "BINV", "BSET", "Instructions32.REV8", "SLLI", "SRLI", "SRAI", "ADDIW", "SLLIW", "SRLIW", "SRAIW", "ADDW", "SUBW", "SLLW", "SRLW", "SRAW", "MULW", "DIVW", "DIVUW", "REMW", "REMUW", "MUL", "MULH", "MULHU", "MULHSU", "DIV", "DIVU", "REM", "REMU", "MAX", "MAXU", "MIN", "MINU", 
    )
    val insns_M_XRD: Seq[String] = Seq(
      "HLV_D", "HLV_WU", "FLW", "FLD", "FLH", "LB", "LH", "LW", "LBU", "LHU", "HLV_B", "HLV_BU", "HLV_H", "HLV_HU", "HLV_W", "LD", "LWU", 
    )
    val insns_M_XWR: Seq[String] = Seq(
      "HSV_D", "FSW", "FSD", "FSH", "SB", "SH", "SW", "HSV_B", "HSV_H", "HSV_W", "SD", 
    )
    val insns_M_FLUSH_ALL: Seq[String] = Seq(
      "CFLUSH_D_L1", "CDISCARD_D_L1", 
    )
    val insns_cmd: Seq[String] = Seq(
      "FENCE_I", 
    )
    val insns_M_SFENCE: Seq[String] = Seq(
      "SFENCE_VMA", 
    )
    val insns_M_HFENCEV: Seq[String] = Seq(
      "HFENCE_VVMA", 
    )
    val insns_M_HFENCEG: Seq[String] = Seq(
      "HFENCE_GVMA", 
    )
    val insns_M_HLVX: Seq[String] = Seq(
      "HLVX_HU", "HLVX_WU", 
    )
    val insns_M_XA_ADD: Seq[String] = Seq(
      "AMOADD_D", "AMOADD_W", 
    )
    val insns_M_XA_SWAP: Seq[String] = Seq(
      "AMOSWAP_D", "AMOSWAP_W", 
    )
    val insns_M_XA_XOR: Seq[String] = Seq(
      "AMOXOR_D", "AMOXOR_W", 
    )
    val insns_M_XA_AND: Seq[String] = Seq(
      "AMOAND_D", "AMOAND_W", 
    )
    val insns_M_XA_OR: Seq[String] = Seq(
      "AMOOR_D", "AMOOR_W", 
    )
    val insns_M_XA_MIN: Seq[String] = Seq(
      "AMOMIN_D", "AMOMIN_W", 
    )
    val insns_M_XA_MINU: Seq[String] = Seq(
      "AMOMINU_D", "AMOMINU_W", 
    )
    val insns_M_XA_MAX: Seq[String] = Seq(
      "AMOMAX_D", "AMOMAX_W", 
    )
    val insns_M_XA_MAXU: Seq[String] = Seq(
      "AMOMAXU_D", "AMOMAXU_W", 
    )
    val insns_M_XLR: Seq[String] = Seq(
      "LR_D", "LR_W", 
    )
    val insns_M_XSC: Seq[String] = Seq(
      "SC_D", "SC_W", 
    )
    def genTable(op: Op): BitPat = {
      if (insns_M_X.contains(op.bitPat)) M_X else if (insns_M_XRD.contains(op.bitPat)) M_XRD else if (insns_M_XWR.contains(op.bitPat)) M_XWR else if (insns_M_FLUSH_ALL.contains(op.bitPat)) M_FLUSH_ALL else if (insns_cmd.contains(op.bitPat)) cmd else if (insns_M_SFENCE.contains(op.bitPat)) M_SFENCE else if (insns_M_HFENCEV.contains(op.bitPat)) M_HFENCEV else if (insns_M_HFENCEG.contains(op.bitPat)) M_HFENCEG else if (insns_M_HLVX.contains(op.bitPat)) M_HLVX else if (insns_M_XA_ADD.contains(op.bitPat)) M_XA_ADD else if (insns_M_XA_SWAP.contains(op.bitPat)) M_XA_SWAP else if (insns_M_XA_XOR.contains(op.bitPat)) M_XA_XOR else if (insns_M_XA_AND.contains(op.bitPat)) M_XA_AND else if (insns_M_XA_OR.contains(op.bitPat)) M_XA_OR else if (insns_M_XA_MIN.contains(op.bitPat)) M_XA_MIN else if (insns_M_XA_MINU.contains(op.bitPat)) M_XA_MINU else if (insns_M_XA_MAX.contains(op.bitPat)) M_XA_MAX else if (insns_M_XA_MAXU.contains(op.bitPat)) M_XA_MAXU else if (insns_M_XLR.contains(op.bitPat)) M_XLR else if (insns_M_XSC.contains(op.bitPat)) M_XSC else M_X
    }
  }
  object decode_rfs1 extends BoolField {
    val insns_N: Seq[String] = Seq(
      "SCIE.opcode", "AES64DS", "AES64DSM", "AES64IM", "AES64KS1I", "AES64KS2", "PACKW", "SHA512SIG0", "SHA512SIG1", "SHA512SUM0", "SHA512SUM1", "ROR", "ROL", "HLV_D", "HSV_D", "HLV_WU", "FMV_W_X", "FCVT_S_W", "FCVT_S_WU", "FLW", "FSW", "CLZW", "CTZW", "CPOPW", "SRET", "Instructions32.BCLRI", "Instructions32.BEXTI", "Instructions32.BINVI", "Instructions32.BSETI", "Instructions32.RORI", "SH1ADD", "SH2ADD", "SH3ADD", "ORC_B", "CUSTOM0", "CUSTOM0_RS1", "CUSTOM0_RS1_RS2", "CUSTOM0_RD", "CUSTOM0_RD_RS1", "CUSTOM0_RD_RS1_RS2", "CUSTOM1", "CUSTOM1_RS1", "CUSTOM1_RS1_RS2", "CUSTOM1_RD", "CUSTOM1_RD_RS1", "CUSTOM1_RD_RS1_RS2", "CUSTOM2", "CUSTOM2_RS1", "CUSTOM2_RS1_RS2", "CUSTOM2_RD", "CUSTOM2_RD_RS1", "CUSTOM2_RD_RS1_RS2", "CUSTOM3", "CUSTOM3_RS1", "CUSTOM3_RS1_RS2", "CUSTOM3_RD", "CUSTOM3_RD_RS1", "CUSTOM3_RD_RS1_RS2", "Instructions32.AES32ESI", "Instructions32.AES32ESMI", "FCVT_D_W", "FCVT_D_WU", "FLD", "FSD", "Instructions32.ZIP", "Instructions32.UNZIP", "FCVT_S_L", "FCVT_S_LU", "Instructions32.AES32DSI", "Instructions32.AES32DSMI", "Instructions32.SHA512SIG0L", "Instructions32.SHA512SIG1L", "Instructions32.SHA512SIG0H", "Instructions32.SHA512SIG1H", "Instructions32.SHA512SUM0R", "Instructions32.SHA512SUM1R", "FMV_H_X", "FCVT_H_W", "FCVT_H_WU", "FLH", "FSH", "AES64ES", "AES64ESM", "CLMUL", "CLMULH", "BCLRI", "BEXTI", "BINVI", "BSETI", "RORI", "RORW", "ROLW", "RORIW", "CFLUSH_D_L1", "CDISCARD_D_L1", "BNE", "BEQ", "BLT", "BLTU", "BGE", "BGEU", "JAL", "JALR", "AUIPC", "LB", "LH", "LW", "LBU", "LHU", "SB", "SH", "SW", "LUI", "ADDI", "SLTI", "SLTIU", "ANDI", "ORI", "XORI", "ADD", "SUB", "SLT", "SLTU", "AND", "OR", "XOR", "SLL", "SRL", "SRA", "FENCE", "ECALL", "EBREAK", "MRET", "WFI", "CEASE", "CSRRW", "CSRRS", "CSRRC", "CSRRWI", "CSRRSI", "CSRRCI", "SEXT_H", "SEXT_B", "XPERM8", "XPERM4", "FMV_D_X", "FCVT_D_L", "FCVT_D_LU", "ZEXT_H", "MNRET", "FCVT_H_L", "FCVT_H_LU", "FENCE_I", "ANDN", "ORN", "XNOR", "Instructions32.SLLI", "Instructions32.SRLI", "Instructions32.SRAI", "SM4ED", "SM4KS", "SM3P0", "SM3P1", "DRET", "CLZ", "CTZ", "CPOP", "REV8", "ADD_UW", "SLLI_UW", "SH1ADD_UW", "SH2ADD_UW", "SH3ADD_UW", "SFENCE_VMA", "HFENCE_VVMA", "HFENCE_GVMA", "HLV_B", "HLV_BU", "HLV_H", "HLV_HU", "HLVX_HU", "HLV_W", "HLVX_WU", "HSV_B", "HSV_H", "HSV_W", "Instructions32.ZEXT_H", "CLMULR", "PACK", "PACKH", "BREV8", "SHA256SIG0", "SHA256SIG1", "SHA256SUM0", "SHA256SUM1", "BCLR", "BEXT", "BINV", "BSET", "Instructions32.REV8", "LD", "LWU", "SD", "SLLI", "SRLI", "SRAI", "ADDIW", "SLLIW", "SRLIW", "SRAIW", "ADDW", "SUBW", "SLLW", "SRLW", "SRAW", "MULW", "DIVW", "DIVUW", "REMW", "REMUW", "AMOADD_D", "AMOSWAP_D", "AMOXOR_D", "AMOAND_D", "AMOOR_D", "AMOMIN_D", "AMOMINU_D", "AMOMAX_D", "AMOMAXU_D", "LR_D", "SC_D", "MUL", "MULH", "MULHU", "MULHSU", "DIV", "DIVU", "REM", "REMU", "AMOADD_W", "AMOXOR_W", "AMOSWAP_W", "AMOAND_W", "AMOOR_W", "AMOMIN_W", "AMOMINU_W", "AMOMAX_W", "AMOMAXU_W", "LR_W", "SC_W", "MAX", "MAXU", "MIN", "MINU", 
    )
    val insns_Y: Seq[String] = Seq(
      "FSGNJ_S", "FSGNJX_S", "FSGNJN_S", "FMIN_S", "FMAX_S", "FADD_S", "FSUB_S", "FMUL_S", "FMADD_S", "FMSUB_S", "FNMADD_S", "FNMSUB_S", "FCLASS_S", "FMV_X_W", "FCVT_W_S", "FCVT_WU_S", "FEQ_S", "FLT_S", "FLE_S", "FDIV_S", "FSQRT_S", "FCVT_S_D", "FCVT_D_S", "FSGNJ_D", "FSGNJX_D", "FSGNJN_D", "FMIN_D", "FMAX_D", "FADD_D", "FSUB_D", "FMUL_D", "FMADD_D", "FMSUB_D", "FNMADD_D", "FNMSUB_D", "FCLASS_D", "FCVT_W_D", "FCVT_WU_D", "FEQ_D", "FLT_D", "FLE_D", "FDIV_D", "FSQRT_D", "FCVT_L_S", "FCVT_LU_S", "FCVT_S_H", "FCVT_H_S", "FSGNJ_H", "FSGNJX_H", "FSGNJN_H", "FMIN_H", "FMAX_H", "FADD_H", "FSUB_H", "FMUL_H", "FMADD_H", "FMSUB_H", "FNMADD_H", "FNMSUB_H", "FCLASS_H", "FMV_X_H", "FCVT_W_H", "FCVT_WU_H", "FEQ_H", "FLT_H", "FLE_H", "FDIV_H", "FSQRT_H", "FCVT_D_H", "FCVT_H_D", "FMV_X_D", "FCVT_L_D", "FCVT_LU_D", "FCVT_L_H", "FCVT_LU_H", 
    )
    def genTable(op: Op): BitPat = {
      if (insns_N.contains(op.bitPat)) N else if (insns_Y.contains(op.bitPat)) Y else N
    }
  }
  object decode_rfs2 extends BoolField {
    val insns_N: Seq[String] = Seq(
      "SCIE.opcode", "AES64DS", "AES64DSM", "AES64IM", "AES64KS1I", "AES64KS2", "PACKW", "SHA512SIG0", "SHA512SIG1", "SHA512SUM0", "SHA512SUM1", "ROR", "ROL", "HLV_D", "HSV_D", "HLV_WU", "FCLASS_S", "FMV_X_W", "FCVT_W_S", "FCVT_WU_S", "FMV_W_X", "FCVT_S_W", "FCVT_S_WU", "FLW", "CLZW", "CTZW", "CPOPW", "SRET", "Instructions32.BCLRI", "Instructions32.BEXTI", "Instructions32.BINVI", "Instructions32.BSETI", "Instructions32.RORI", "SH1ADD", "SH2ADD", "SH3ADD", "ORC_B", "CUSTOM0", "CUSTOM0_RS1", "CUSTOM0_RS1_RS2", "CUSTOM0_RD", "CUSTOM0_RD_RS1", "CUSTOM0_RD_RS1_RS2", "CUSTOM1", "CUSTOM1_RS1", "CUSTOM1_RS1_RS2", "CUSTOM1_RD", "CUSTOM1_RD_RS1", "CUSTOM1_RD_RS1_RS2", "CUSTOM2", "CUSTOM2_RS1", "CUSTOM2_RS1_RS2", "CUSTOM2_RD", "CUSTOM2_RD_RS1", "CUSTOM2_RD_RS1_RS2", "CUSTOM3", "CUSTOM3_RS1", "CUSTOM3_RS1_RS2", "CUSTOM3_RD", "CUSTOM3_RD_RS1", "CUSTOM3_RD_RS1_RS2", "Instructions32.AES32ESI", "Instructions32.AES32ESMI", "FCVT_S_D", "FCVT_D_S", "FCLASS_D", "FCVT_W_D", "FCVT_WU_D", "FCVT_D_W", "FCVT_D_WU", "FLD", "Instructions32.ZIP", "Instructions32.UNZIP", "FCVT_L_S", "FCVT_LU_S", "FCVT_S_L", "FCVT_S_LU", "Instructions32.AES32DSI", "Instructions32.AES32DSMI", "Instructions32.SHA512SIG0L", "Instructions32.SHA512SIG1L", "Instructions32.SHA512SIG0H", "Instructions32.SHA512SIG1H", "Instructions32.SHA512SUM0R", "Instructions32.SHA512SUM1R", "FCVT_S_H", "FCVT_H_S", "FCLASS_H", "FMV_X_H", "FCVT_W_H", "FCVT_WU_H", "FMV_H_X", "FCVT_H_W", "FCVT_H_WU", "FLH", "AES64ES", "AES64ESM", "CLMUL", "CLMULH", "BCLRI", "BEXTI", "BINVI", "BSETI", "RORI", "RORW", "ROLW", "RORIW", "FCVT_D_H", "FCVT_H_D", "CFLUSH_D_L1", "CDISCARD_D_L1", "BNE", "BEQ", "BLT", "BLTU", "BGE", "BGEU", "JAL", "JALR", "AUIPC", "LB", "LH", "LW", "LBU", "LHU", "SB", "SH", "SW", "LUI", "ADDI", "SLTI", "SLTIU", "ANDI", "ORI", "XORI", "ADD", "SUB", "SLT", "SLTU", "AND", "OR", "XOR", "SLL", "SRL", "SRA", "FENCE", "ECALL", "EBREAK", "MRET", "WFI", "CEASE", "CSRRW", "CSRRS", "CSRRC", "CSRRWI", "CSRRSI", "CSRRCI", "SEXT_H", "SEXT_B", "XPERM8", "XPERM4", "FMV_X_D", "FCVT_L_D", "FCVT_LU_D", "FMV_D_X", "FCVT_D_L", "FCVT_D_LU", "ZEXT_H", "MNRET", "FCVT_L_H", "FCVT_LU_H", "FCVT_H_L", "FCVT_H_LU", "FENCE_I", "ANDN", "ORN", "XNOR", "Instructions32.SLLI", "Instructions32.SRLI", "Instructions32.SRAI", "SM4ED", "SM4KS", "SM3P0", "SM3P1", "DRET", "CLZ", "CTZ", "CPOP", "REV8", "ADD_UW", "SLLI_UW", "SH1ADD_UW", "SH2ADD_UW", "SH3ADD_UW", "SFENCE_VMA", "HFENCE_VVMA", "HFENCE_GVMA", "HLV_B", "HLV_BU", "HLV_H", "HLV_HU", "HLVX_HU", "HLV_W", "HLVX_WU", "HSV_B", "HSV_H", "HSV_W", "Instructions32.ZEXT_H", "CLMULR", "PACK", "PACKH", "BREV8", "SHA256SIG0", "SHA256SIG1", "SHA256SUM0", "SHA256SUM1", "BCLR", "BEXT", "BINV", "BSET", "Instructions32.REV8", "LD", "LWU", "SD", "SLLI", "SRLI", "SRAI", "ADDIW", "SLLIW", "SRLIW", "SRAIW", "ADDW", "SUBW", "SLLW", "SRLW", "SRAW", "MULW", "DIVW", "DIVUW", "REMW", "REMUW", "AMOADD_D", "AMOSWAP_D", "AMOXOR_D", "AMOAND_D", "AMOOR_D", "AMOMIN_D", "AMOMINU_D", "AMOMAX_D", "AMOMAXU_D", "LR_D", "SC_D", "MUL", "MULH", "MULHU", "MULHSU", "DIV", "DIVU", "REM", "REMU", "AMOADD_W", "AMOXOR_W", "AMOSWAP_W", "AMOAND_W", "AMOOR_W", "AMOMIN_W", "AMOMINU_W", "AMOMAX_W", "AMOMAXU_W", "LR_W", "SC_W", "MAX", "MAXU", "MIN", "MINU", 
    )
    val insns_Y: Seq[String] = Seq(
      "FSGNJ_S", "FSGNJX_S", "FSGNJN_S", "FMIN_S", "FMAX_S", "FADD_S", "FSUB_S", "FMUL_S", "FMADD_S", "FMSUB_S", "FNMADD_S", "FNMSUB_S", "FEQ_S", "FLT_S", "FLE_S", "FSW", "FDIV_S", "FSQRT_S", "FSGNJ_D", "FSGNJX_D", "FSGNJN_D", "FMIN_D", "FMAX_D", "FADD_D", "FSUB_D", "FMUL_D", "FMADD_D", "FMSUB_D", "FNMADD_D", "FNMSUB_D", "FEQ_D", "FLT_D", "FLE_D", "FSD", "FDIV_D", "FSQRT_D", "FSGNJ_H", "FSGNJX_H", "FSGNJN_H", "FMIN_H", "FMAX_H", "FADD_H", "FSUB_H", "FMUL_H", "FMADD_H", "FMSUB_H", "FNMADD_H", "FNMSUB_H", "FEQ_H", "FLT_H", "FLE_H", "FSH", "FDIV_H", "FSQRT_H", 
    )
    def genTable(op: Op): BitPat = {
      if (insns_N.contains(op.bitPat)) N else if (insns_Y.contains(op.bitPat)) Y else N
    }
  }
  object decode_rfs3 extends BoolField {
    val insns_N: Seq[String] = Seq(
      "SCIE.opcode", "AES64DS", "AES64DSM", "AES64IM", "AES64KS1I", "AES64KS2", "PACKW", "SHA512SIG0", "SHA512SIG1", "SHA512SUM0", "SHA512SUM1", "ROR", "ROL", "HLV_D", "HSV_D", "HLV_WU", "FSGNJ_S", "FSGNJX_S", "FSGNJN_S", "FMIN_S", "FMAX_S", "FADD_S", "FSUB_S", "FMUL_S", "FCLASS_S", "FMV_X_W", "FCVT_W_S", "FCVT_WU_S", "FEQ_S", "FLT_S", "FLE_S", "FMV_W_X", "FCVT_S_W", "FCVT_S_WU", "FLW", "FSW", "FDIV_S", "FSQRT_S", "CLZW", "CTZW", "CPOPW", "SRET", "Instructions32.BCLRI", "Instructions32.BEXTI", "Instructions32.BINVI", "Instructions32.BSETI", "Instructions32.RORI", "SH1ADD", "SH2ADD", "SH3ADD", "ORC_B", "CUSTOM0", "CUSTOM0_RS1", "CUSTOM0_RS1_RS2", "CUSTOM0_RD", "CUSTOM0_RD_RS1", "CUSTOM0_RD_RS1_RS2", "CUSTOM1", "CUSTOM1_RS1", "CUSTOM1_RS1_RS2", "CUSTOM1_RD", "CUSTOM1_RD_RS1", "CUSTOM1_RD_RS1_RS2", "CUSTOM2", "CUSTOM2_RS1", "CUSTOM2_RS1_RS2", "CUSTOM2_RD", "CUSTOM2_RD_RS1", "CUSTOM2_RD_RS1_RS2", "CUSTOM3", "CUSTOM3_RS1", "CUSTOM3_RS1_RS2", "CUSTOM3_RD", "CUSTOM3_RD_RS1", "CUSTOM3_RD_RS1_RS2", "Instructions32.AES32ESI", "Instructions32.AES32ESMI", "FCVT_S_D", "FCVT_D_S", "FSGNJ_D", "FSGNJX_D", "FSGNJN_D", "FMIN_D", "FMAX_D", "FADD_D", "FSUB_D", "FMUL_D", "FCLASS_D", "FCVT_W_D", "FCVT_WU_D", "FEQ_D", "FLT_D", "FLE_D", "FCVT_D_W", "FCVT_D_WU", "FLD", "FSD", "FDIV_D", "FSQRT_D", "Instructions32.ZIP", "Instructions32.UNZIP", "FCVT_L_S", "FCVT_LU_S", "FCVT_S_L", "FCVT_S_LU", "Instructions32.AES32DSI", "Instructions32.AES32DSMI", "Instructions32.SHA512SIG0L", "Instructions32.SHA512SIG1L", "Instructions32.SHA512SIG0H", "Instructions32.SHA512SIG1H", "Instructions32.SHA512SUM0R", "Instructions32.SHA512SUM1R", "FCVT_S_H", "FCVT_H_S", "FSGNJ_H", "FSGNJX_H", "FSGNJN_H", "FMIN_H", "FMAX_H", "FADD_H", "FSUB_H", "FMUL_H", "FCLASS_H", "FMV_X_H", "FCVT_W_H", "FCVT_WU_H", "FEQ_H", "FLT_H", "FLE_H", "FMV_H_X", "FCVT_H_W", "FCVT_H_WU", "FLH", "FSH", "FDIV_H", "FSQRT_H", "AES64ES", "AES64ESM", "CLMUL", "CLMULH", "BCLRI", "BEXTI", "BINVI", "BSETI", "RORI", "RORW", "ROLW", "RORIW", "FCVT_D_H", "FCVT_H_D", "CFLUSH_D_L1", "CDISCARD_D_L1", "BNE", "BEQ", "BLT", "BLTU", "BGE", "BGEU", "JAL", "JALR", "AUIPC", "LB", "LH", "LW", "LBU", "LHU", "SB", "SH", "SW", "LUI", "ADDI", "SLTI", "SLTIU", "ANDI", "ORI", "XORI", "ADD", "SUB", "SLT", "SLTU", "AND", "OR", "XOR", "SLL", "SRL", "SRA", "FENCE", "ECALL", "EBREAK", "MRET", "WFI", "CEASE", "CSRRW", "CSRRS", "CSRRC", "CSRRWI", "CSRRSI", "CSRRCI", "SEXT_H", "SEXT_B", "XPERM8", "XPERM4", "FMV_X_D", "FCVT_L_D", "FCVT_LU_D", "FMV_D_X", "FCVT_D_L", "FCVT_D_LU", "ZEXT_H", "MNRET", "FCVT_L_H", "FCVT_LU_H", "FCVT_H_L", "FCVT_H_LU", "FENCE_I", "ANDN", "ORN", "XNOR", "Instructions32.SLLI", "Instructions32.SRLI", "Instructions32.SRAI", "SM4ED", "SM4KS", "SM3P0", "SM3P1", "DRET", "CLZ", "CTZ", "CPOP", "REV8", "ADD_UW", "SLLI_UW", "SH1ADD_UW", "SH2ADD_UW", "SH3ADD_UW", "SFENCE_VMA", "HFENCE_VVMA", "HFENCE_GVMA", "HLV_B", "HLV_BU", "HLV_H", "HLV_HU", "HLVX_HU", "HLV_W", "HLVX_WU", "HSV_B", "HSV_H", "HSV_W", "Instructions32.ZEXT_H", "CLMULR", "PACK", "PACKH", "BREV8", "SHA256SIG0", "SHA256SIG1", "SHA256SUM0", "SHA256SUM1", "BCLR", "BEXT", "BINV", "BSET", "Instructions32.REV8", "LD", "LWU", "SD", "SLLI", "SRLI", "SRAI", "ADDIW", "SLLIW", "SRLIW", "SRAIW", "ADDW", "SUBW", "SLLW", "SRLW", "SRAW", "MULW", "DIVW", "DIVUW", "REMW", "REMUW", "AMOADD_D", "AMOSWAP_D", "AMOXOR_D", "AMOAND_D", "AMOOR_D", "AMOMIN_D", "AMOMINU_D", "AMOMAX_D", "AMOMAXU_D", "LR_D", "SC_D", "MUL", "MULH", "MULHU", "MULHSU", "DIV", "DIVU", "REM", "REMU", "AMOADD_W", "AMOXOR_W", "AMOSWAP_W", "AMOAND_W", "AMOOR_W", "AMOMIN_W", "AMOMINU_W", "AMOMAX_W", "AMOMAXU_W", "LR_W", "SC_W", "MAX", "MAXU", "MIN", "MINU", 
    )
    val insns_Y: Seq[String] = Seq(
      "FMADD_S", "FMSUB_S", "FNMADD_S", "FNMSUB_S", "FMADD_D", "FMSUB_D", "FNMADD_D", "FNMSUB_D", "FMADD_H", "FMSUB_H", "FNMADD_H", "FNMSUB_H", 
    )
    def genTable(op: Op): BitPat = {
      if (insns_N.contains(op.bitPat)) N else if (insns_Y.contains(op.bitPat)) Y else N
    }
  }
  object decode_wfd extends BoolField {
    val insns_N: Seq[String] = Seq(
      "SCIE.opcode", "AES64DS", "AES64DSM", "AES64IM", "AES64KS1I", "AES64KS2", "PACKW", "SHA512SIG0", "SHA512SIG1", "SHA512SUM0", "SHA512SUM1", "ROR", "ROL", "HLV_D", "HSV_D", "HLV_WU", "FCLASS_S", "FMV_X_W", "FCVT_W_S", "FCVT_WU_S", "FEQ_S", "FLT_S", "FLE_S", "FSW", "CLZW", "CTZW", "CPOPW", "SRET", "Instructions32.BCLRI", "Instructions32.BEXTI", "Instructions32.BINVI", "Instructions32.BSETI", "Instructions32.RORI", "SH1ADD", "SH2ADD", "SH3ADD", "ORC_B", "CUSTOM0", "CUSTOM0_RS1", "CUSTOM0_RS1_RS2", "CUSTOM0_RD", "CUSTOM0_RD_RS1", "CUSTOM0_RD_RS1_RS2", "CUSTOM1", "CUSTOM1_RS1", "CUSTOM1_RS1_RS2", "CUSTOM1_RD", "CUSTOM1_RD_RS1", "CUSTOM1_RD_RS1_RS2", "CUSTOM2", "CUSTOM2_RS1", "CUSTOM2_RS1_RS2", "CUSTOM2_RD", "CUSTOM2_RD_RS1", "CUSTOM2_RD_RS1_RS2", "CUSTOM3", "CUSTOM3_RS1", "CUSTOM3_RS1_RS2", "CUSTOM3_RD", "CUSTOM3_RD_RS1", "CUSTOM3_RD_RS1_RS2", "Instructions32.AES32ESI", "Instructions32.AES32ESMI", "FCLASS_D", "FCVT_W_D", "FCVT_WU_D", "FEQ_D", "FLT_D", "FLE_D", "FSD", "Instructions32.ZIP", "Instructions32.UNZIP", "FCVT_L_S", "FCVT_LU_S", "Instructions32.AES32DSI", "Instructions32.AES32DSMI", "Instructions32.SHA512SIG0L", "Instructions32.SHA512SIG1L", "Instructions32.SHA512SIG0H", "Instructions32.SHA512SIG1H", "Instructions32.SHA512SUM0R", "Instructions32.SHA512SUM1R", "FCLASS_H", "FMV_X_H", "FCVT_W_H", "FCVT_WU_H", "FEQ_H", "FLT_H", "FLE_H", "FSH", "AES64ES", "AES64ESM", "CLMUL", "CLMULH", "BCLRI", "BEXTI", "BINVI", "BSETI", "RORI", "RORW", "ROLW", "RORIW", "CFLUSH_D_L1", "CDISCARD_D_L1", "BNE", "BEQ", "BLT", "BLTU", "BGE", "BGEU", "JAL", "JALR", "AUIPC", "LB", "LH", "LW", "LBU", "LHU", "SB", "SH", "SW", "LUI", "ADDI", "SLTI", "SLTIU", "ANDI", "ORI", "XORI", "ADD", "SUB", "SLT", "SLTU", "AND", "OR", "XOR", "SLL", "SRL", "SRA", "FENCE", "ECALL", "EBREAK", "MRET", "WFI", "CEASE", "CSRRW", "CSRRS", "CSRRC", "CSRRWI", "CSRRSI", "CSRRCI", "SEXT_H", "SEXT_B", "XPERM8", "XPERM4", "FMV_X_D", "FCVT_L_D", "FCVT_LU_D", "ZEXT_H", "MNRET", "FCVT_L_H", "FCVT_LU_H", "FENCE_I", "ANDN", "ORN", "XNOR", "Instructions32.SLLI", "Instructions32.SRLI", "Instructions32.SRAI", "SM4ED", "SM4KS", "SM3P0", "SM3P1", "DRET", "CLZ", "CTZ", "CPOP", "REV8", "ADD_UW", "SLLI_UW", "SH1ADD_UW", "SH2ADD_UW", "SH3ADD_UW", "SFENCE_VMA", "HFENCE_VVMA", "HFENCE_GVMA", "HLV_B", "HLV_BU", "HLV_H", "HLV_HU", "HLVX_HU", "HLV_W", "HLVX_WU", "HSV_B", "HSV_H", "HSV_W", "Instructions32.ZEXT_H", "CLMULR", "PACK", "PACKH", "BREV8", "SHA256SIG0", "SHA256SIG1", "SHA256SUM0", "SHA256SUM1", "BCLR", "BEXT", "BINV", "BSET", "Instructions32.REV8", "LD", "LWU", "SD", "SLLI", "SRLI", "SRAI", "ADDIW", "SLLIW", "SRLIW", "SRAIW", "ADDW", "SUBW", "SLLW", "SRLW", "SRAW", "MULW", "DIVW", "DIVUW", "REMW", "REMUW", "AMOADD_D", "AMOSWAP_D", "AMOXOR_D", "AMOAND_D", "AMOOR_D", "AMOMIN_D", "AMOMINU_D", "AMOMAX_D", "AMOMAXU_D", "LR_D", "SC_D", "MUL", "MULH", "MULHU", "MULHSU", "DIV", "DIVU", "REM", "REMU", "AMOADD_W", "AMOXOR_W", "AMOSWAP_W", "AMOAND_W", "AMOOR_W", "AMOMIN_W", "AMOMINU_W", "AMOMAX_W", "AMOMAXU_W", "LR_W", "SC_W", "MAX", "MAXU", "MIN", "MINU", 
    )
    val insns_Y: Seq[String] = Seq(
      "FSGNJ_S", "FSGNJX_S", "FSGNJN_S", "FMIN_S", "FMAX_S", "FADD_S", "FSUB_S", "FMUL_S", "FMADD_S", "FMSUB_S", "FNMADD_S", "FNMSUB_S", "FMV_W_X", "FCVT_S_W", "FCVT_S_WU", "FLW", "FDIV_S", "FSQRT_S", "FCVT_S_D", "FCVT_D_S", "FSGNJ_D", "FSGNJX_D", "FSGNJN_D", "FMIN_D", "FMAX_D", "FADD_D", "FSUB_D", "FMUL_D", "FMADD_D", "FMSUB_D", "FNMADD_D", "FNMSUB_D", "FCVT_D_W", "FCVT_D_WU", "FLD", "FDIV_D", "FSQRT_D", "FCVT_S_L", "FCVT_S_LU", "FCVT_S_H", "FCVT_H_S", "FSGNJ_H", "FSGNJX_H", "FSGNJN_H", "FMIN_H", "FMAX_H", "FADD_H", "FSUB_H", "FMUL_H", "FMADD_H", "FMSUB_H", "FNMADD_H", "FNMSUB_H", "FMV_H_X", "FCVT_H_W", "FCVT_H_WU", "FLH", "FDIV_H", "FSQRT_H", "FCVT_D_H", "FCVT_H_D", "FMV_D_X", "FCVT_D_L", "FCVT_D_LU", "FCVT_H_L", "FCVT_H_LU", 
    )
    def genTable(op: Op): BitPat = {
      if (insns_N.contains(op.bitPat)) N else if (insns_Y.contains(op.bitPat)) Y else N
    }
  }
  object decode_mul extends BoolField {
    val insns_N: Seq[String] = Seq(
      "SCIE.opcode", "AES64DS", "AES64DSM", "AES64IM", "AES64KS1I", "AES64KS2", "PACKW", "SHA512SIG0", "SHA512SIG1", "SHA512SUM0", "SHA512SUM1", "ROR", "ROL", "HLV_D", "HSV_D", "HLV_WU", "FSGNJ_S", "FSGNJX_S", "FSGNJN_S", "FMIN_S", "FMAX_S", "FADD_S", "FSUB_S", "FMUL_S", "FMADD_S", "FMSUB_S", "FNMADD_S", "FNMSUB_S", "FCLASS_S", "FMV_X_W", "FCVT_W_S", "FCVT_WU_S", "FEQ_S", "FLT_S", "FLE_S", "FMV_W_X", "FCVT_S_W", "FCVT_S_WU", "FLW", "FSW", "FDIV_S", "FSQRT_S", "CLZW", "CTZW", "CPOPW", "SRET", "Instructions32.BCLRI", "Instructions32.BEXTI", "Instructions32.BINVI", "Instructions32.BSETI", "Instructions32.RORI", "SH1ADD", "SH2ADD", "SH3ADD", "ORC_B", "CUSTOM0", "CUSTOM0_RS1", "CUSTOM0_RS1_RS2", "CUSTOM0_RD", "CUSTOM0_RD_RS1", "CUSTOM0_RD_RS1_RS2", "CUSTOM1", "CUSTOM1_RS1", "CUSTOM1_RS1_RS2", "CUSTOM1_RD", "CUSTOM1_RD_RS1", "CUSTOM1_RD_RS1_RS2", "CUSTOM2", "CUSTOM2_RS1", "CUSTOM2_RS1_RS2", "CUSTOM2_RD", "CUSTOM2_RD_RS1", "CUSTOM2_RD_RS1_RS2", "CUSTOM3", "CUSTOM3_RS1", "CUSTOM3_RS1_RS2", "CUSTOM3_RD", "CUSTOM3_RD_RS1", "CUSTOM3_RD_RS1_RS2", "Instructions32.AES32ESI", "Instructions32.AES32ESMI", "FCVT_S_D", "FCVT_D_S", "FSGNJ_D", "FSGNJX_D", "FSGNJN_D", "FMIN_D", "FMAX_D", "FADD_D", "FSUB_D", "FMUL_D", "FMADD_D", "FMSUB_D", "FNMADD_D", "FNMSUB_D", "FCLASS_D", "FCVT_W_D", "FCVT_WU_D", "FEQ_D", "FLT_D", "FLE_D", "FCVT_D_W", "FCVT_D_WU", "FLD", "FSD", "FDIV_D", "FSQRT_D", "Instructions32.ZIP", "Instructions32.UNZIP", "FCVT_L_S", "FCVT_LU_S", "FCVT_S_L", "FCVT_S_LU", "Instructions32.AES32DSI", "Instructions32.AES32DSMI", "Instructions32.SHA512SIG0L", "Instructions32.SHA512SIG1L", "Instructions32.SHA512SIG0H", "Instructions32.SHA512SIG1H", "Instructions32.SHA512SUM0R", "Instructions32.SHA512SUM1R", "FCVT_S_H", "FCVT_H_S", "FSGNJ_H", "FSGNJX_H", "FSGNJN_H", "FMIN_H", "FMAX_H", "FADD_H", "FSUB_H", "FMUL_H", "FMADD_H", "FMSUB_H", "FNMADD_H", "FNMSUB_H", "FCLASS_H", "FMV_X_H", "FCVT_W_H", "FCVT_WU_H", "FEQ_H", "FLT_H", "FLE_H", "FMV_H_X", "FCVT_H_W", "FCVT_H_WU", "FLH", "FSH", "FDIV_H", "FSQRT_H", "AES64ES", "AES64ESM", "CLMUL", "CLMULH", "BCLRI", "BEXTI", "BINVI", "BSETI", "RORI", "RORW", "ROLW", "RORIW", "FCVT_D_H", "FCVT_H_D", "CFLUSH_D_L1", "CDISCARD_D_L1", "BNE", "BEQ", "BLT", "BLTU", "BGE", "BGEU", "JAL", "JALR", "AUIPC", "LB", "LH", "LW", "LBU", "LHU", "SB", "SH", "SW", "LUI", "ADDI", "SLTI", "SLTIU", "ANDI", "ORI", "XORI", "ADD", "SUB", "SLT", "SLTU", "AND", "OR", "XOR", "SLL", "SRL", "SRA", "FENCE", "ECALL", "EBREAK", "MRET", "WFI", "CEASE", "CSRRW", "CSRRS", "CSRRC", "CSRRWI", "CSRRSI", "CSRRCI", "SEXT_H", "SEXT_B", "XPERM8", "XPERM4", "FMV_X_D", "FCVT_L_D", "FCVT_LU_D", "FMV_D_X", "FCVT_D_L", "FCVT_D_LU", "ZEXT_H", "MNRET", "FCVT_L_H", "FCVT_LU_H", "FCVT_H_L", "FCVT_H_LU", "FENCE_I", "ANDN", "ORN", "XNOR", "Instructions32.SLLI", "Instructions32.SRLI", "Instructions32.SRAI", "SM4ED", "SM4KS", "SM3P0", "SM3P1", "DRET", "CLZ", "CTZ", "CPOP", "REV8", "ADD_UW", "SLLI_UW", "SH1ADD_UW", "SH2ADD_UW", "SH3ADD_UW", "SFENCE_VMA", "HFENCE_VVMA", "HFENCE_GVMA", "HLV_B", "HLV_BU", "HLV_H", "HLV_HU", "HLVX_HU", "HLV_W", "HLVX_WU", "HSV_B", "HSV_H", "HSV_W", "Instructions32.ZEXT_H", "CLMULR", "PACK", "PACKH", "BREV8", "SHA256SIG0", "SHA256SIG1", "SHA256SUM0", "SHA256SUM1", "BCLR", "BEXT", "BINV", "BSET", "Instructions32.REV8", "LD", "LWU", "SD", "SLLI", "SRLI", "SRAI", "ADDIW", "SLLIW", "SRLIW", "SRAIW", "ADDW", "SUBW", "SLLW", "SRLW", "SRAW", "DIVW", "DIVUW", "REMW", "REMUW", "AMOADD_D", "AMOSWAP_D", "AMOXOR_D", "AMOAND_D", "AMOOR_D", "AMOMIN_D", "AMOMINU_D", "AMOMAX_D", "AMOMAXU_D", "LR_D", "SC_D", "DIV", "DIVU", "REM", "REMU", "AMOADD_W", "AMOXOR_W", "AMOSWAP_W", "AMOAND_W", "AMOOR_W", "AMOMIN_W", "AMOMINU_W", "AMOMAX_W", "AMOMAXU_W", "LR_W", "SC_W", "MAX", "MAXU", "MIN", "MINU", 
    )
    val insns_M: Seq[String] = Seq(
      "MULW", "MUL", "MULH", "MULHU", "MULHSU", 
    )
    def genTable(op: Op): BitPat = {
      if (insns_N.contains(op.bitPat)) N else if (insns_M.contains(op.bitPat)) M else N
    }
  }
  object decode_div extends BoolField {
    val insns_N: Seq[String] = Seq(
      "SCIE.opcode", "AES64DS", "AES64DSM", "AES64IM", "AES64KS1I", "AES64KS2", "PACKW", "SHA512SIG0", "SHA512SIG1", "SHA512SUM0", "SHA512SUM1", "ROR", "ROL", "HLV_D", "HSV_D", "HLV_WU", "FSGNJ_S", "FSGNJX_S", "FSGNJN_S", "FMIN_S", "FMAX_S", "FADD_S", "FSUB_S", "FMUL_S", "FMADD_S", "FMSUB_S", "FNMADD_S", "FNMSUB_S", "FCLASS_S", "FMV_X_W", "FCVT_W_S", "FCVT_WU_S", "FEQ_S", "FLT_S", "FLE_S", "FMV_W_X", "FCVT_S_W", "FCVT_S_WU", "FLW", "FSW", "FDIV_S", "FSQRT_S", "CLZW", "CTZW", "CPOPW", "SRET", "Instructions32.BCLRI", "Instructions32.BEXTI", "Instructions32.BINVI", "Instructions32.BSETI", "Instructions32.RORI", "SH1ADD", "SH2ADD", "SH3ADD", "ORC_B", "CUSTOM0", "CUSTOM0_RS1", "CUSTOM0_RS1_RS2", "CUSTOM0_RD", "CUSTOM0_RD_RS1", "CUSTOM0_RD_RS1_RS2", "CUSTOM1", "CUSTOM1_RS1", "CUSTOM1_RS1_RS2", "CUSTOM1_RD", "CUSTOM1_RD_RS1", "CUSTOM1_RD_RS1_RS2", "CUSTOM2", "CUSTOM2_RS1", "CUSTOM2_RS1_RS2", "CUSTOM2_RD", "CUSTOM2_RD_RS1", "CUSTOM2_RD_RS1_RS2", "CUSTOM3", "CUSTOM3_RS1", "CUSTOM3_RS1_RS2", "CUSTOM3_RD", "CUSTOM3_RD_RS1", "CUSTOM3_RD_RS1_RS2", "Instructions32.AES32ESI", "Instructions32.AES32ESMI", "FCVT_S_D", "FCVT_D_S", "FSGNJ_D", "FSGNJX_D", "FSGNJN_D", "FMIN_D", "FMAX_D", "FADD_D", "FSUB_D", "FMUL_D", "FMADD_D", "FMSUB_D", "FNMADD_D", "FNMSUB_D", "FCLASS_D", "FCVT_W_D", "FCVT_WU_D", "FEQ_D", "FLT_D", "FLE_D", "FCVT_D_W", "FCVT_D_WU", "FLD", "FSD", "FDIV_D", "FSQRT_D", "Instructions32.ZIP", "Instructions32.UNZIP", "FCVT_L_S", "FCVT_LU_S", "FCVT_S_L", "FCVT_S_LU", "Instructions32.AES32DSI", "Instructions32.AES32DSMI", "Instructions32.SHA512SIG0L", "Instructions32.SHA512SIG1L", "Instructions32.SHA512SIG0H", "Instructions32.SHA512SIG1H", "Instructions32.SHA512SUM0R", "Instructions32.SHA512SUM1R", "FCVT_S_H", "FCVT_H_S", "FSGNJ_H", "FSGNJX_H", "FSGNJN_H", "FMIN_H", "FMAX_H", "FADD_H", "FSUB_H", "FMUL_H", "FMADD_H", "FMSUB_H", "FNMADD_H", "FNMSUB_H", "FCLASS_H", "FMV_X_H", "FCVT_W_H", "FCVT_WU_H", "FEQ_H", "FLT_H", "FLE_H", "FMV_H_X", "FCVT_H_W", "FCVT_H_WU", "FLH", "FSH", "FDIV_H", "FSQRT_H", "AES64ES", "AES64ESM", "CLMUL", "CLMULH", "BCLRI", "BEXTI", "BINVI", "BSETI", "RORI", "RORW", "ROLW", "RORIW", "FCVT_D_H", "FCVT_H_D", "CFLUSH_D_L1", "CDISCARD_D_L1", "BNE", "BEQ", "BLT", "BLTU", "BGE", "BGEU", "JAL", "JALR", "AUIPC", "LB", "LH", "LW", "LBU", "LHU", "SB", "SH", "SW", "LUI", "ADDI", "SLTI", "SLTIU", "ANDI", "ORI", "XORI", "ADD", "SUB", "SLT", "SLTU", "AND", "OR", "XOR", "SLL", "SRL", "SRA", "FENCE", "ECALL", "EBREAK", "MRET", "WFI", "CEASE", "CSRRW", "CSRRS", "CSRRC", "CSRRWI", "CSRRSI", "CSRRCI", "SEXT_H", "SEXT_B", "XPERM8", "XPERM4", "FMV_X_D", "FCVT_L_D", "FCVT_LU_D", "FMV_D_X", "FCVT_D_L", "FCVT_D_LU", "ZEXT_H", "MNRET", "FCVT_L_H", "FCVT_LU_H", "FCVT_H_L", "FCVT_H_LU", "FENCE_I", "ANDN", "ORN", "XNOR", "Instructions32.SLLI", "Instructions32.SRLI", "Instructions32.SRAI", "SM4ED", "SM4KS", "SM3P0", "SM3P1", "DRET", "CLZ", "CTZ", "CPOP", "REV8", "ADD_UW", "SLLI_UW", "SH1ADD_UW", "SH2ADD_UW", "SH3ADD_UW", "SFENCE_VMA", "HFENCE_VVMA", "HFENCE_GVMA", "HLV_B", "HLV_BU", "HLV_H", "HLV_HU", "HLVX_HU", "HLV_W", "HLVX_WU", "HSV_B", "HSV_H", "HSV_W", "Instructions32.ZEXT_H", "CLMULR", "PACK", "PACKH", "BREV8", "SHA256SIG0", "SHA256SIG1", "SHA256SUM0", "SHA256SUM1", "BCLR", "BEXT", "BINV", "BSET", "Instructions32.REV8", "LD", "LWU", "SD", "SLLI", "SRLI", "SRAI", "ADDIW", "SLLIW", "SRLIW", "SRAIW", "ADDW", "SUBW", "SLLW", "SRLW", "SRAW", "AMOADD_D", "AMOSWAP_D", "AMOXOR_D", "AMOAND_D", "AMOOR_D", "AMOMIN_D", "AMOMINU_D", "AMOMAX_D", "AMOMAXU_D", "LR_D", "SC_D", "AMOADD_W", "AMOXOR_W", "AMOSWAP_W", "AMOAND_W", "AMOOR_W", "AMOMIN_W", "AMOMINU_W", "AMOMAX_W", "AMOMAXU_W", "LR_W", "SC_W", "MAX", "MAXU", "MIN", "MINU", 
    )
    val insns_D: Seq[String] = Seq(
      "MULW", "MUL", "MULH", "MULHU", "MULHSU", 
    )
    val insns_Y: Seq[String] = Seq(
      "DIVW", "DIVUW", "REMW", "REMUW", "DIV", "DIVU", "REM", "REMU", 
    )
    def genTable(op: Op): BitPat = {
      if (insns_N.contains(op.bitPat)) N else if (insns_D.contains(op.bitPat)) D else if (insns_Y.contains(op.bitPat)) Y else N
    }
  }
  object decode_wxd extends BoolField {
    val insns_Y: Seq[String] = Seq(
      "SCIE.opcode", "AES64DS", "AES64DSM", "AES64IM", "AES64KS1I", "AES64KS2", "PACKW", "SHA512SIG0", "SHA512SIG1", "SHA512SUM0", "SHA512SUM1", "ROR", "ROL", "HLV_D", "HLV_WU", "FCLASS_S", "FMV_X_W", "FCVT_W_S", "FCVT_WU_S", "FEQ_S", "FLT_S", "FLE_S", "CLZW", "CTZW", "CPOPW", "Instructions32.BCLRI", "Instructions32.BEXTI", "Instructions32.BINVI", "Instructions32.BSETI", "Instructions32.RORI", "SH1ADD", "SH2ADD", "SH3ADD", "ORC_B", "CUSTOM0_RD", "CUSTOM0_RD_RS1", "CUSTOM0_RD_RS1_RS2", "CUSTOM1_RD", "CUSTOM1_RD_RS1", "CUSTOM1_RD_RS1_RS2", "CUSTOM2_RD", "CUSTOM2_RD_RS1", "CUSTOM2_RD_RS1_RS2", "CUSTOM3_RD", "CUSTOM3_RD_RS1", "CUSTOM3_RD_RS1_RS2", "Instructions32.AES32ESI", "Instructions32.AES32ESMI", "FCLASS_D", "FCVT_W_D", "FCVT_WU_D", "FEQ_D", "FLT_D", "FLE_D", "Instructions32.ZIP", "Instructions32.UNZIP", "FCVT_L_S", "FCVT_LU_S", "Instructions32.AES32DSI", "Instructions32.AES32DSMI", "Instructions32.SHA512SIG0L", "Instructions32.SHA512SIG1L", "Instructions32.SHA512SIG0H", "Instructions32.SHA512SIG1H", "Instructions32.SHA512SUM0R", "Instructions32.SHA512SUM1R", "FCLASS_H", "FMV_X_H", "FCVT_W_H", "FCVT_WU_H", "FEQ_H", "FLT_H", "FLE_H", "AES64ES", "AES64ESM", "CLMUL", "CLMULH", "BCLRI", "BEXTI", "BINVI", "BSETI", "RORI", "RORW", "ROLW", "RORIW", "JAL", "JALR", "AUIPC", "LB", "LH", "LW", "LBU", "LHU", "LUI", "ADDI", "SLTI", "SLTIU", "ANDI", "ORI", "XORI", "ADD", "SUB", "SLT", "SLTU", "AND", "OR", "XOR", "SLL", "SRL", "SRA", "CSRRW", "CSRRS", "CSRRC", "CSRRWI", "CSRRSI", "CSRRCI", "SEXT_H", "SEXT_B", "XPERM8", "XPERM4", "FMV_X_D", "FCVT_L_D", "FCVT_LU_D", "ZEXT_H", "FCVT_L_H", "FCVT_LU_H", "ANDN", "ORN", "XNOR", "Instructions32.SLLI", "Instructions32.SRLI", "Instructions32.SRAI", "SM4ED", "SM4KS", "SM3P0", "SM3P1", "CLZ", "CTZ", "CPOP", "REV8", "ADD_UW", "SLLI_UW", "SH1ADD_UW", "SH2ADD_UW", "SH3ADD_UW", "HLV_B", "HLV_BU", "HLV_H", "HLV_HU", "HLVX_HU", "HLV_W", "HLVX_WU", "Instructions32.ZEXT_H", "CLMULR", "PACK", "PACKH", "BREV8", "SHA256SIG0", "SHA256SIG1", "SHA256SUM0", "SHA256SUM1", "BCLR", "BEXT", "BINV", "BSET", "Instructions32.REV8", "LD", "LWU", "SLLI", "SRLI", "SRAI", "ADDIW", "SLLIW", "SRLIW", "SRAIW", "ADDW", "SUBW", "SLLW", "SRLW", "SRAW", "MULW", "DIVW", "DIVUW", "REMW", "REMUW", "AMOADD_D", "AMOSWAP_D", "AMOXOR_D", "AMOAND_D", "AMOOR_D", "AMOMIN_D", "AMOMINU_D", "AMOMAX_D", "AMOMAXU_D", "LR_D", "SC_D", "MUL", "MULH", "MULHU", "MULHSU", "DIV", "DIVU", "REM", "REMU", "AMOADD_W", "AMOXOR_W", "AMOSWAP_W", "AMOAND_W", "AMOOR_W", "AMOMIN_W", "AMOMINU_W", "AMOMAX_W", "AMOMAXU_W", "LR_W", "SC_W", "MAX", "MAXU", "MIN", "MINU", 
    )
    val insns_N: Seq[String] = Seq(
      "HSV_D", "FSGNJ_S", "FSGNJX_S", "FSGNJN_S", "FMIN_S", "FMAX_S", "FADD_S", "FSUB_S", "FMUL_S", "FMADD_S", "FMSUB_S", "FNMADD_S", "FNMSUB_S", "FMV_W_X", "FCVT_S_W", "FCVT_S_WU", "FLW", "FSW", "FDIV_S", "FSQRT_S", "SRET", "CUSTOM0", "CUSTOM0_RS1", "CUSTOM0_RS1_RS2", "CUSTOM1", "CUSTOM1_RS1", "CUSTOM1_RS1_RS2", "CUSTOM2", "CUSTOM2_RS1", "CUSTOM2_RS1_RS2", "CUSTOM3", "CUSTOM3_RS1", "CUSTOM3_RS1_RS2", "FCVT_S_D", "FCVT_D_S", "FSGNJ_D", "FSGNJX_D", "FSGNJN_D", "FMIN_D", "FMAX_D", "FADD_D", "FSUB_D", "FMUL_D", "FMADD_D", "FMSUB_D", "FNMADD_D", "FNMSUB_D", "FCVT_D_W", "FCVT_D_WU", "FLD", "FSD", "FDIV_D", "FSQRT_D", "FCVT_S_L", "FCVT_S_LU", "FCVT_S_H", "FCVT_H_S", "FSGNJ_H", "FSGNJX_H", "FSGNJN_H", "FMIN_H", "FMAX_H", "FADD_H", "FSUB_H", "FMUL_H", "FMADD_H", "FMSUB_H", "FNMADD_H", "FNMSUB_H", "FMV_H_X", "FCVT_H_W", "FCVT_H_WU", "FLH", "FSH", "FDIV_H", "FSQRT_H", "FCVT_D_H", "FCVT_H_D", "CFLUSH_D_L1", "CDISCARD_D_L1", "BNE", "BEQ", "BLT", "BLTU", "BGE", "BGEU", "SB", "SH", "SW", "FENCE", "ECALL", "EBREAK", "MRET", "WFI", "CEASE", "FMV_D_X", "FCVT_D_L", "FCVT_D_LU", "MNRET", "FCVT_H_L", "FCVT_H_LU", "FENCE_I", "DRET", "SFENCE_VMA", "HFENCE_VVMA", "HFENCE_GVMA", "HSV_B", "HSV_H", "HSV_W", "SD", 
    )
    def genTable(op: Op): BitPat = {
      if (insns_Y.contains(op.bitPat)) Y else if (insns_N.contains(op.bitPat)) N else N
    }
  }
  object decode_csr extends BitsField_3 {
    val insns_CSR_N: Seq[String] = Seq(
      "SCIE.opcode", "AES64DS", "AES64DSM", "AES64IM", "AES64KS1I", "AES64KS2", "PACKW", "SHA512SIG0", "SHA512SIG1", "SHA512SUM0", "SHA512SUM1", "ROR", "ROL", "FSGNJ_S", "FSGNJX_S", "FSGNJN_S", "FMIN_S", "FMAX_S", "FADD_S", "FSUB_S", "FMUL_S", "FMADD_S", "FMSUB_S", "FNMADD_S", "FNMSUB_S", "FCLASS_S", "FMV_X_W", "FCVT_W_S", "FCVT_WU_S", "FEQ_S", "FLT_S", "FLE_S", "FMV_W_X", "FCVT_S_W", "FCVT_S_WU", "FLW", "FSW", "FDIV_S", "FSQRT_S", "CLZW", "CTZW", "CPOPW", "Instructions32.BCLRI", "Instructions32.BEXTI", "Instructions32.BINVI", "Instructions32.BSETI", "Instructions32.RORI", "SH1ADD", "SH2ADD", "SH3ADD", "ORC_B", "CUSTOM0", "CUSTOM0_RS1", "CUSTOM0_RS1_RS2", "CUSTOM0_RD", "CUSTOM0_RD_RS1", "CUSTOM0_RD_RS1_RS2", "CUSTOM1", "CUSTOM1_RS1", "CUSTOM1_RS1_RS2", "CUSTOM1_RD", "CUSTOM1_RD_RS1", "CUSTOM1_RD_RS1_RS2", "CUSTOM2", "CUSTOM2_RS1", "CUSTOM2_RS1_RS2", "CUSTOM2_RD", "CUSTOM2_RD_RS1", "CUSTOM2_RD_RS1_RS2", "CUSTOM3", "CUSTOM3_RS1", "CUSTOM3_RS1_RS2", "CUSTOM3_RD", "CUSTOM3_RD_RS1", "CUSTOM3_RD_RS1_RS2", "Instructions32.AES32ESI", "Instructions32.AES32ESMI", "FCVT_S_D", "FCVT_D_S", "FSGNJ_D", "FSGNJX_D", "FSGNJN_D", "FMIN_D", "FMAX_D", "FADD_D", "FSUB_D", "FMUL_D", "FMADD_D", "FMSUB_D", "FNMADD_D", "FNMSUB_D", "FCLASS_D", "FCVT_W_D", "FCVT_WU_D", "FEQ_D", "FLT_D", "FLE_D", "FCVT_D_W", "FCVT_D_WU", "FLD", "FSD", "FDIV_D", "FSQRT_D", "Instructions32.ZIP", "Instructions32.UNZIP", "FCVT_L_S", "FCVT_LU_S", "FCVT_S_L", "FCVT_S_LU", "Instructions32.AES32DSI", "Instructions32.AES32DSMI", "Instructions32.SHA512SIG0L", "Instructions32.SHA512SIG1L", "Instructions32.SHA512SIG0H", "Instructions32.SHA512SIG1H", "Instructions32.SHA512SUM0R", "Instructions32.SHA512SUM1R", "FCVT_S_H", "FCVT_H_S", "FSGNJ_H", "FSGNJX_H", "FSGNJN_H", "FMIN_H", "FMAX_H", "FADD_H", "FSUB_H", "FMUL_H", "FMADD_H", "FMSUB_H", "FNMADD_H", "FNMSUB_H", "FCLASS_H", "FMV_X_H", "FCVT_W_H", "FCVT_WU_H", "FEQ_H", "FLT_H", "FLE_H", "FMV_H_X", "FCVT_H_W", "FCVT_H_WU", "FLH", "FSH", "FDIV_H", "FSQRT_H", "AES64ES", "AES64ESM", "CLMUL", "CLMULH", "BCLRI", "BEXTI", "BINVI", "BSETI", "RORI", "RORW", "ROLW", "RORIW", "FCVT_D_H", "FCVT_H_D", "BNE", "BEQ", "BLT", "BLTU", "BGE", "BGEU", "JAL", "JALR", "AUIPC", "LB", "LH", "LW", "LBU", "LHU", "SB", "SH", "SW", "LUI", "ADDI", "SLTI", "SLTIU", "ANDI", "ORI", "XORI", "ADD", "SUB", "SLT", "SLTU", "AND", "OR", "XOR", "SLL", "SRL", "SRA", "FENCE", "SEXT_H", "SEXT_B", "XPERM8", "XPERM4", "FMV_X_D", "FCVT_L_D", "FCVT_LU_D", "FMV_D_X", "FCVT_D_L", "FCVT_D_LU", "ZEXT_H", "FCVT_L_H", "FCVT_LU_H", "FCVT_H_L", "FCVT_H_LU", "FENCE_I", "ANDN", "ORN", "XNOR", "Instructions32.SLLI", "Instructions32.SRLI", "Instructions32.SRAI", "SM4ED", "SM4KS", "SM3P0", "SM3P1", "CLZ", "CTZ", "CPOP", "REV8", "ADD_UW", "SLLI_UW", "SH1ADD_UW", "SH2ADD_UW", "SH3ADD_UW", "Instructions32.ZEXT_H", "CLMULR", "PACK", "PACKH", "BREV8", "SHA256SIG0", "SHA256SIG1", "SHA256SUM0", "SHA256SUM1", "BCLR", "BEXT", "BINV", "BSET", "Instructions32.REV8", "LD", "LWU", "SD", "SLLI", "SRLI", "SRAI", "ADDIW", "SLLIW", "SRLIW", "SRAIW", "ADDW", "SUBW", "SLLW", "SRLW", "SRAW", "MULW", "DIVW", "DIVUW", "REMW", "REMUW", "AMOADD_D", "AMOSWAP_D", "AMOXOR_D", "AMOAND_D", "AMOOR_D", "AMOMIN_D", "AMOMINU_D", "AMOMAX_D", "AMOMAXU_D", "LR_D", "SC_D", "MUL", "MULH", "MULHU", "MULHSU", "DIV", "DIVU", "REM", "REMU", "AMOADD_W", "AMOXOR_W", "AMOSWAP_W", "AMOAND_W", "AMOOR_W", "AMOMIN_W", "AMOMINU_W", "AMOMAX_W", "AMOMAXU_W", "LR_W", "SC_W", "MAX", "MAXU", "MIN", "MINU", 
    )
    val insns_CSR_I: Seq[String] = Seq(
      "HLV_D", "HSV_D", "HLV_WU", "SRET", "CFLUSH_D_L1", "CDISCARD_D_L1", "ECALL", "EBREAK", "MRET", "WFI", "CEASE", "MNRET", "DRET", "SFENCE_VMA", "HFENCE_VVMA", "HFENCE_GVMA", "HLV_B", "HLV_BU", "HLV_H", "HLV_HU", "HLVX_HU", "HLV_W", "HLVX_WU", "HSV_B", "HSV_H", "HSV_W", 
    )
    val insns_CSR_W: Seq[String] = Seq(
      "CSRRW", "CSRRWI", 
    )
    val insns_CSR_S: Seq[String] = Seq(
      "CSRRS", "CSRRSI", 
    )
    val insns_CSR_C: Seq[String] = Seq(
      "CSRRC", "CSRRCI", 
    )
    def genTable(op: Op): BitPat = {
      if (insns_CSR_N.contains(op.bitPat)) CSR.N else if (insns_CSR_I.contains(op.bitPat)) CSR.I else if (insns_CSR_W.contains(op.bitPat)) CSR.W else if (insns_CSR_S.contains(op.bitPat)) CSR.S else if (insns_CSR_C.contains(op.bitPat)) CSR.C else CSR.N
    }
  }
  object decode_fence_i extends BoolField {
    val insns_N: Seq[String] = Seq(
      "SCIE.opcode", "AES64DS", "AES64DSM", "AES64IM", "AES64KS1I", "AES64KS2", "PACKW", "SHA512SIG0", "SHA512SIG1", "SHA512SUM0", "SHA512SUM1", "ROR", "ROL", "HLV_D", "HSV_D", "HLV_WU", "FSGNJ_S", "FSGNJX_S", "FSGNJN_S", "FMIN_S", "FMAX_S", "FADD_S", "FSUB_S", "FMUL_S", "FMADD_S", "FMSUB_S", "FNMADD_S", "FNMSUB_S", "FCLASS_S", "FMV_X_W", "FCVT_W_S", "FCVT_WU_S", "FEQ_S", "FLT_S", "FLE_S", "FMV_W_X", "FCVT_S_W", "FCVT_S_WU", "FLW", "FSW", "FDIV_S", "FSQRT_S", "CLZW", "CTZW", "CPOPW", "SRET", "Instructions32.BCLRI", "Instructions32.BEXTI", "Instructions32.BINVI", "Instructions32.BSETI", "Instructions32.RORI", "SH1ADD", "SH2ADD", "SH3ADD", "ORC_B", "CUSTOM0", "CUSTOM0_RS1", "CUSTOM0_RS1_RS2", "CUSTOM0_RD", "CUSTOM0_RD_RS1", "CUSTOM0_RD_RS1_RS2", "CUSTOM1", "CUSTOM1_RS1", "CUSTOM1_RS1_RS2", "CUSTOM1_RD", "CUSTOM1_RD_RS1", "CUSTOM1_RD_RS1_RS2", "CUSTOM2", "CUSTOM2_RS1", "CUSTOM2_RS1_RS2", "CUSTOM2_RD", "CUSTOM2_RD_RS1", "CUSTOM2_RD_RS1_RS2", "CUSTOM3", "CUSTOM3_RS1", "CUSTOM3_RS1_RS2", "CUSTOM3_RD", "CUSTOM3_RD_RS1", "CUSTOM3_RD_RS1_RS2", "Instructions32.AES32ESI", "Instructions32.AES32ESMI", "FCVT_S_D", "FCVT_D_S", "FSGNJ_D", "FSGNJX_D", "FSGNJN_D", "FMIN_D", "FMAX_D", "FADD_D", "FSUB_D", "FMUL_D", "FMADD_D", "FMSUB_D", "FNMADD_D", "FNMSUB_D", "FCLASS_D", "FCVT_W_D", "FCVT_WU_D", "FEQ_D", "FLT_D", "FLE_D", "FCVT_D_W", "FCVT_D_WU", "FLD", "FSD", "FDIV_D", "FSQRT_D", "Instructions32.ZIP", "Instructions32.UNZIP", "FCVT_L_S", "FCVT_LU_S", "FCVT_S_L", "FCVT_S_LU", "Instructions32.AES32DSI", "Instructions32.AES32DSMI", "Instructions32.SHA512SIG0L", "Instructions32.SHA512SIG1L", "Instructions32.SHA512SIG0H", "Instructions32.SHA512SIG1H", "Instructions32.SHA512SUM0R", "Instructions32.SHA512SUM1R", "FCVT_S_H", "FCVT_H_S", "FSGNJ_H", "FSGNJX_H", "FSGNJN_H", "FMIN_H", "FMAX_H", "FADD_H", "FSUB_H", "FMUL_H", "FMADD_H", "FMSUB_H", "FNMADD_H", "FNMSUB_H", "FCLASS_H", "FMV_X_H", "FCVT_W_H", "FCVT_WU_H", "FEQ_H", "FLT_H", "FLE_H", "FMV_H_X", "FCVT_H_W", "FCVT_H_WU", "FLH", "FSH", "FDIV_H", "FSQRT_H", "AES64ES", "AES64ESM", "CLMUL", "CLMULH", "BCLRI", "BEXTI", "BINVI", "BSETI", "RORI", "RORW", "ROLW", "RORIW", "FCVT_D_H", "FCVT_H_D", "CFLUSH_D_L1", "CDISCARD_D_L1", "BNE", "BEQ", "BLT", "BLTU", "BGE", "BGEU", "JAL", "JALR", "AUIPC", "LB", "LH", "LW", "LBU", "LHU", "SB", "SH", "SW", "LUI", "ADDI", "SLTI", "SLTIU", "ANDI", "ORI", "XORI", "ADD", "SUB", "SLT", "SLTU", "AND", "OR", "XOR", "SLL", "SRL", "SRA", "FENCE", "ECALL", "EBREAK", "MRET", "WFI", "CEASE", "CSRRW", "CSRRS", "CSRRC", "CSRRWI", "CSRRSI", "CSRRCI", "SEXT_H", "SEXT_B", "XPERM8", "XPERM4", "FMV_X_D", "FCVT_L_D", "FCVT_LU_D", "FMV_D_X", "FCVT_D_L", "FCVT_D_LU", "ZEXT_H", "MNRET", "FCVT_L_H", "FCVT_LU_H", "FCVT_H_L", "FCVT_H_LU", "ANDN", "ORN", "XNOR", "Instructions32.SLLI", "Instructions32.SRLI", "Instructions32.SRAI", "SM4ED", "SM4KS", "SM3P0", "SM3P1", "DRET", "CLZ", "CTZ", "CPOP", "REV8", "ADD_UW", "SLLI_UW", "SH1ADD_UW", "SH2ADD_UW", "SH3ADD_UW", "SFENCE_VMA", "HFENCE_VVMA", "HFENCE_GVMA", "HLV_B", "HLV_BU", "HLV_H", "HLV_HU", "HLVX_HU", "HLV_W", "HLVX_WU", "HSV_B", "HSV_H", "HSV_W", "Instructions32.ZEXT_H", "CLMULR", "PACK", "PACKH", "BREV8", "SHA256SIG0", "SHA256SIG1", "SHA256SUM0", "SHA256SUM1", "BCLR", "BEXT", "BINV", "BSET", "Instructions32.REV8", "LD", "LWU", "SD", "SLLI", "SRLI", "SRAI", "ADDIW", "SLLIW", "SRLIW", "SRAIW", "ADDW", "SUBW", "SLLW", "SRLW", "SRAW", "MULW", "DIVW", "DIVUW", "REMW", "REMUW", "AMOADD_D", "AMOSWAP_D", "AMOXOR_D", "AMOAND_D", "AMOOR_D", "AMOMIN_D", "AMOMINU_D", "AMOMAX_D", "AMOMAXU_D", "LR_D", "SC_D", "MUL", "MULH", "MULHU", "MULHSU", "DIV", "DIVU", "REM", "REMU", "AMOADD_W", "AMOXOR_W", "AMOSWAP_W", "AMOAND_W", "AMOOR_W", "AMOMIN_W", "AMOMINU_W", "AMOMAX_W", "AMOMAXU_W", "LR_W", "SC_W", "MAX", "MAXU", "MIN", "MINU", 
    )
    val insns_Y: Seq[String] = Seq(
      "FENCE_I", 
    )
    def genTable(op: Op): BitPat = {
      if (insns_N.contains(op.bitPat)) N else if (insns_Y.contains(op.bitPat)) Y else N
    }
  }
  object decode_fence extends BoolField {
    val insns_N: Seq[String] = Seq(
      "SCIE.opcode", "AES64DS", "AES64DSM", "AES64IM", "AES64KS1I", "AES64KS2", "PACKW", "SHA512SIG0", "SHA512SIG1", "SHA512SUM0", "SHA512SUM1", "ROR", "ROL", "HLV_D", "HSV_D", "HLV_WU", "FSGNJ_S", "FSGNJX_S", "FSGNJN_S", "FMIN_S", "FMAX_S", "FADD_S", "FSUB_S", "FMUL_S", "FMADD_S", "FMSUB_S", "FNMADD_S", "FNMSUB_S", "FCLASS_S", "FMV_X_W", "FCVT_W_S", "FCVT_WU_S", "FEQ_S", "FLT_S", "FLE_S", "FMV_W_X", "FCVT_S_W", "FCVT_S_WU", "FLW", "FSW", "FDIV_S", "FSQRT_S", "CLZW", "CTZW", "CPOPW", "SRET", "Instructions32.BCLRI", "Instructions32.BEXTI", "Instructions32.BINVI", "Instructions32.BSETI", "Instructions32.RORI", "SH1ADD", "SH2ADD", "SH3ADD", "ORC_B", "CUSTOM0", "CUSTOM0_RS1", "CUSTOM0_RS1_RS2", "CUSTOM0_RD", "CUSTOM0_RD_RS1", "CUSTOM0_RD_RS1_RS2", "CUSTOM1", "CUSTOM1_RS1", "CUSTOM1_RS1_RS2", "CUSTOM1_RD", "CUSTOM1_RD_RS1", "CUSTOM1_RD_RS1_RS2", "CUSTOM2", "CUSTOM2_RS1", "CUSTOM2_RS1_RS2", "CUSTOM2_RD", "CUSTOM2_RD_RS1", "CUSTOM2_RD_RS1_RS2", "CUSTOM3", "CUSTOM3_RS1", "CUSTOM3_RS1_RS2", "CUSTOM3_RD", "CUSTOM3_RD_RS1", "CUSTOM3_RD_RS1_RS2", "Instructions32.AES32ESI", "Instructions32.AES32ESMI", "FCVT_S_D", "FCVT_D_S", "FSGNJ_D", "FSGNJX_D", "FSGNJN_D", "FMIN_D", "FMAX_D", "FADD_D", "FSUB_D", "FMUL_D", "FMADD_D", "FMSUB_D", "FNMADD_D", "FNMSUB_D", "FCLASS_D", "FCVT_W_D", "FCVT_WU_D", "FEQ_D", "FLT_D", "FLE_D", "FCVT_D_W", "FCVT_D_WU", "FLD", "FSD", "FDIV_D", "FSQRT_D", "Instructions32.ZIP", "Instructions32.UNZIP", "FCVT_L_S", "FCVT_LU_S", "FCVT_S_L", "FCVT_S_LU", "Instructions32.AES32DSI", "Instructions32.AES32DSMI", "Instructions32.SHA512SIG0L", "Instructions32.SHA512SIG1L", "Instructions32.SHA512SIG0H", "Instructions32.SHA512SIG1H", "Instructions32.SHA512SUM0R", "Instructions32.SHA512SUM1R", "FCVT_S_H", "FCVT_H_S", "FSGNJ_H", "FSGNJX_H", "FSGNJN_H", "FMIN_H", "FMAX_H", "FADD_H", "FSUB_H", "FMUL_H", "FMADD_H", "FMSUB_H", "FNMADD_H", "FNMSUB_H", "FCLASS_H", "FMV_X_H", "FCVT_W_H", "FCVT_WU_H", "FEQ_H", "FLT_H", "FLE_H", "FMV_H_X", "FCVT_H_W", "FCVT_H_WU", "FLH", "FSH", "FDIV_H", "FSQRT_H", "AES64ES", "AES64ESM", "CLMUL", "CLMULH", "BCLRI", "BEXTI", "BINVI", "BSETI", "RORI", "RORW", "ROLW", "RORIW", "FCVT_D_H", "FCVT_H_D", "CFLUSH_D_L1", "CDISCARD_D_L1", "BNE", "BEQ", "BLT", "BLTU", "BGE", "BGEU", "JAL", "JALR", "AUIPC", "LB", "LH", "LW", "LBU", "LHU", "SB", "SH", "SW", "LUI", "ADDI", "SLTI", "SLTIU", "ANDI", "ORI", "XORI", "ADD", "SUB", "SLT", "SLTU", "AND", "OR", "XOR", "SLL", "SRL", "SRA", "ECALL", "EBREAK", "MRET", "WFI", "CEASE", "CSRRW", "CSRRS", "CSRRC", "CSRRWI", "CSRRSI", "CSRRCI", "SEXT_H", "SEXT_B", "XPERM8", "XPERM4", "FMV_X_D", "FCVT_L_D", "FCVT_LU_D", "FMV_D_X", "FCVT_D_L", "FCVT_D_LU", "ZEXT_H", "MNRET", "FCVT_L_H", "FCVT_LU_H", "FCVT_H_L", "FCVT_H_LU", "ANDN", "ORN", "XNOR", "Instructions32.SLLI", "Instructions32.SRLI", "Instructions32.SRAI", "SM4ED", "SM4KS", "SM3P0", "SM3P1", "DRET", "CLZ", "CTZ", "CPOP", "REV8", "ADD_UW", "SLLI_UW", "SH1ADD_UW", "SH2ADD_UW", "SH3ADD_UW", "SFENCE_VMA", "HFENCE_VVMA", "HFENCE_GVMA", "HLV_B", "HLV_BU", "HLV_H", "HLV_HU", "HLVX_HU", "HLV_W", "HLVX_WU", "HSV_B", "HSV_H", "HSV_W", "Instructions32.ZEXT_H", "CLMULR", "PACK", "PACKH", "BREV8", "SHA256SIG0", "SHA256SIG1", "SHA256SUM0", "SHA256SUM1", "BCLR", "BEXT", "BINV", "BSET", "Instructions32.REV8", "LD", "LWU", "SD", "SLLI", "SRLI", "SRAI", "ADDIW", "SLLIW", "SRLIW", "SRAIW", "ADDW", "SUBW", "SLLW", "SRLW", "SRAW", "MULW", "DIVW", "DIVUW", "REMW", "REMUW", "AMOADD_D", "AMOSWAP_D", "AMOXOR_D", "AMOAND_D", "AMOOR_D", "AMOMIN_D", "AMOMINU_D", "AMOMAX_D", "AMOMAXU_D", "LR_D", "SC_D", "MUL", "MULH", "MULHU", "MULHSU", "DIV", "DIVU", "REM", "REMU", "AMOADD_W", "AMOXOR_W", "AMOSWAP_W", "AMOAND_W", "AMOOR_W", "AMOMIN_W", "AMOMINU_W", "AMOMAX_W", "AMOMAXU_W", "LR_W", "SC_W", "MAX", "MAXU", "MIN", "MINU", 
    )
    val insns_Y: Seq[String] = Seq(
      "FENCE", "FENCE_I", 
    )
    def genTable(op: Op): BitPat = {
      if (insns_N.contains(op.bitPat)) N else if (insns_Y.contains(op.bitPat)) Y else N
    }
  }
  object decode_amo extends BoolField {
    val insns_N: Seq[String] = Seq(
      "SCIE.opcode", "AES64DS", "AES64DSM", "AES64IM", "AES64KS1I", "AES64KS2", "PACKW", "SHA512SIG0", "SHA512SIG1", "SHA512SUM0", "SHA512SUM1", "ROR", "ROL", "HLV_D", "HSV_D", "HLV_WU", "FSGNJ_S", "FSGNJX_S", "FSGNJN_S", "FMIN_S", "FMAX_S", "FADD_S", "FSUB_S", "FMUL_S", "FMADD_S", "FMSUB_S", "FNMADD_S", "FNMSUB_S", "FCLASS_S", "FMV_X_W", "FCVT_W_S", "FCVT_WU_S", "FEQ_S", "FLT_S", "FLE_S", "FMV_W_X", "FCVT_S_W", "FCVT_S_WU", "FLW", "FSW", "FDIV_S", "FSQRT_S", "CLZW", "CTZW", "CPOPW", "SRET", "Instructions32.BCLRI", "Instructions32.BEXTI", "Instructions32.BINVI", "Instructions32.BSETI", "Instructions32.RORI", "SH1ADD", "SH2ADD", "SH3ADD", "ORC_B", "CUSTOM0", "CUSTOM0_RS1", "CUSTOM0_RS1_RS2", "CUSTOM0_RD", "CUSTOM0_RD_RS1", "CUSTOM0_RD_RS1_RS2", "CUSTOM1", "CUSTOM1_RS1", "CUSTOM1_RS1_RS2", "CUSTOM1_RD", "CUSTOM1_RD_RS1", "CUSTOM1_RD_RS1_RS2", "CUSTOM2", "CUSTOM2_RS1", "CUSTOM2_RS1_RS2", "CUSTOM2_RD", "CUSTOM2_RD_RS1", "CUSTOM2_RD_RS1_RS2", "CUSTOM3", "CUSTOM3_RS1", "CUSTOM3_RS1_RS2", "CUSTOM3_RD", "CUSTOM3_RD_RS1", "CUSTOM3_RD_RS1_RS2", "Instructions32.AES32ESI", "Instructions32.AES32ESMI", "FCVT_S_D", "FCVT_D_S", "FSGNJ_D", "FSGNJX_D", "FSGNJN_D", "FMIN_D", "FMAX_D", "FADD_D", "FSUB_D", "FMUL_D", "FMADD_D", "FMSUB_D", "FNMADD_D", "FNMSUB_D", "FCLASS_D", "FCVT_W_D", "FCVT_WU_D", "FEQ_D", "FLT_D", "FLE_D", "FCVT_D_W", "FCVT_D_WU", "FLD", "FSD", "FDIV_D", "FSQRT_D", "Instructions32.ZIP", "Instructions32.UNZIP", "FCVT_L_S", "FCVT_LU_S", "FCVT_S_L", "FCVT_S_LU", "Instructions32.AES32DSI", "Instructions32.AES32DSMI", "Instructions32.SHA512SIG0L", "Instructions32.SHA512SIG1L", "Instructions32.SHA512SIG0H", "Instructions32.SHA512SIG1H", "Instructions32.SHA512SUM0R", "Instructions32.SHA512SUM1R", "FCVT_S_H", "FCVT_H_S", "FSGNJ_H", "FSGNJX_H", "FSGNJN_H", "FMIN_H", "FMAX_H", "FADD_H", "FSUB_H", "FMUL_H", "FMADD_H", "FMSUB_H", "FNMADD_H", "FNMSUB_H", "FCLASS_H", "FMV_X_H", "FCVT_W_H", "FCVT_WU_H", "FEQ_H", "FLT_H", "FLE_H", "FMV_H_X", "FCVT_H_W", "FCVT_H_WU", "FLH", "FSH", "FDIV_H", "FSQRT_H", "AES64ES", "AES64ESM", "CLMUL", "CLMULH", "BCLRI", "BEXTI", "BINVI", "BSETI", "RORI", "RORW", "ROLW", "RORIW", "FCVT_D_H", "FCVT_H_D", "CFLUSH_D_L1", "CDISCARD_D_L1", "BNE", "BEQ", "BLT", "BLTU", "BGE", "BGEU", "JAL", "JALR", "AUIPC", "LB", "LH", "LW", "LBU", "LHU", "SB", "SH", "SW", "LUI", "ADDI", "SLTI", "SLTIU", "ANDI", "ORI", "XORI", "ADD", "SUB", "SLT", "SLTU", "AND", "OR", "XOR", "SLL", "SRL", "SRA", "FENCE", "ECALL", "EBREAK", "MRET", "WFI", "CEASE", "CSRRW", "CSRRS", "CSRRC", "CSRRWI", "CSRRSI", "CSRRCI", "SEXT_H", "SEXT_B", "XPERM8", "XPERM4", "FMV_X_D", "FCVT_L_D", "FCVT_LU_D", "FMV_D_X", "FCVT_D_L", "FCVT_D_LU", "ZEXT_H", "MNRET", "FCVT_L_H", "FCVT_LU_H", "FCVT_H_L", "FCVT_H_LU", "FENCE_I", "ANDN", "ORN", "XNOR", "Instructions32.SLLI", "Instructions32.SRLI", "Instructions32.SRAI", "SM4ED", "SM4KS", "SM3P0", "SM3P1", "DRET", "CLZ", "CTZ", "CPOP", "REV8", "ADD_UW", "SLLI_UW", "SH1ADD_UW", "SH2ADD_UW", "SH3ADD_UW", "SFENCE_VMA", "HFENCE_VVMA", "HFENCE_GVMA", "HLV_B", "HLV_BU", "HLV_H", "HLV_HU", "HLVX_HU", "HLV_W", "HLVX_WU", "HSV_B", "HSV_H", "HSV_W", "Instructions32.ZEXT_H", "CLMULR", "PACK", "PACKH", "BREV8", "SHA256SIG0", "SHA256SIG1", "SHA256SUM0", "SHA256SUM1", "BCLR", "BEXT", "BINV", "BSET", "Instructions32.REV8", "LD", "LWU", "SD", "SLLI", "SRLI", "SRAI", "ADDIW", "SLLIW", "SRLIW", "SRAIW", "ADDW", "SUBW", "SLLW", "SRLW", "SRAW", "MULW", "DIVW", "DIVUW", "REMW", "REMUW", "MUL", "MULH", "MULHU", "MULHSU", "DIV", "DIVU", "REM", "REMU", "MAX", "MAXU", "MIN", "MINU", 
    )
    val insns_Y: Seq[String] = Seq(
      "AMOADD_D", "AMOSWAP_D", "AMOXOR_D", "AMOAND_D", "AMOOR_D", "AMOMIN_D", "AMOMINU_D", "AMOMAX_D", "AMOMAXU_D", "LR_D", "SC_D", "AMOADD_W", "AMOXOR_W", "AMOSWAP_W", "AMOAND_W", "AMOOR_W", "AMOMIN_W", "AMOMINU_W", "AMOMAX_W", "AMOMAXU_W", "LR_W", "SC_W", 
    )
    def genTable(op: Op): BitPat = {
      if (insns_N.contains(op.bitPat)) N else if (insns_Y.contains(op.bitPat)) Y else N
    }
  }
  object decode_dp extends BoolField {
    val insns_N: Seq[String] = Seq(
      "SCIE.opcode", "AES64DS", "AES64DSM", "AES64IM", "AES64KS1I", "AES64KS2", "PACKW", "SHA512SIG0", "SHA512SIG1", "SHA512SUM0", "SHA512SUM1", "ROR", "ROL", "HLV_D", "HSV_D", "HLV_WU", "FSGNJ_S", "FSGNJX_S", "FSGNJN_S", "FMIN_S", "FMAX_S", "FADD_S", "FSUB_S", "FMUL_S", "FMADD_S", "FMSUB_S", "FNMADD_S", "FNMSUB_S", "FCLASS_S", "FMV_X_W", "FCVT_W_S", "FCVT_WU_S", "FEQ_S", "FLT_S", "FLE_S", "FMV_W_X", "FCVT_S_W", "FCVT_S_WU", "FLW", "FSW", "FDIV_S", "FSQRT_S", "CLZW", "CTZW", "CPOPW", "SRET", "Instructions32.BCLRI", "Instructions32.BEXTI", "Instructions32.BINVI", "Instructions32.BSETI", "Instructions32.RORI", "SH1ADD", "SH2ADD", "SH3ADD", "ORC_B", "CUSTOM0", "CUSTOM0_RS1", "CUSTOM0_RS1_RS2", "CUSTOM0_RD", "CUSTOM0_RD_RS1", "CUSTOM0_RD_RS1_RS2", "CUSTOM1", "CUSTOM1_RS1", "CUSTOM1_RS1_RS2", "CUSTOM1_RD", "CUSTOM1_RD_RS1", "CUSTOM1_RD_RS1_RS2", "CUSTOM2", "CUSTOM2_RS1", "CUSTOM2_RS1_RS2", "CUSTOM2_RD", "CUSTOM2_RD_RS1", "CUSTOM2_RD_RS1_RS2", "CUSTOM3", "CUSTOM3_RS1", "CUSTOM3_RS1_RS2", "CUSTOM3_RD", "CUSTOM3_RD_RS1", "CUSTOM3_RD_RS1_RS2", "Instructions32.AES32ESI", "Instructions32.AES32ESMI", "Instructions32.ZIP", "Instructions32.UNZIP", "FCVT_L_S", "FCVT_LU_S", "FCVT_S_L", "FCVT_S_LU", "Instructions32.AES32DSI", "Instructions32.AES32DSMI", "Instructions32.SHA512SIG0L", "Instructions32.SHA512SIG1L", "Instructions32.SHA512SIG0H", "Instructions32.SHA512SIG1H", "Instructions32.SHA512SUM0R", "Instructions32.SHA512SUM1R", "FCVT_S_H", "FCVT_H_S", "FSGNJ_H", "FSGNJX_H", "FSGNJN_H", "FMIN_H", "FMAX_H", "FADD_H", "FSUB_H", "FMUL_H", "FMADD_H", "FMSUB_H", "FNMADD_H", "FNMSUB_H", "FCLASS_H", "FMV_X_H", "FCVT_W_H", "FCVT_WU_H", "FEQ_H", "FLT_H", "FLE_H", "FMV_H_X", "FCVT_H_W", "FCVT_H_WU", "FLH", "FSH", "FDIV_H", "FSQRT_H", "AES64ES", "AES64ESM", "CLMUL", "CLMULH", "BCLRI", "BEXTI", "BINVI", "BSETI", "RORI", "RORW", "ROLW", "RORIW", "CFLUSH_D_L1", "CDISCARD_D_L1", "BNE", "BEQ", "BLT", "BLTU", "BGE", "BGEU", "JAL", "JALR", "AUIPC", "LB", "LH", "LW", "LBU", "LHU", "SB", "SH", "SW", "LUI", "ADDI", "SLTI", "SLTIU", "ANDI", "ORI", "XORI", "ADD", "SUB", "SLT", "SLTU", "AND", "OR", "XOR", "SLL", "SRL", "SRA", "FENCE", "ECALL", "EBREAK", "MRET", "WFI", "CEASE", "CSRRW", "CSRRS", "CSRRC", "CSRRWI", "CSRRSI", "CSRRCI", "SEXT_H", "SEXT_B", "XPERM8", "XPERM4", "ZEXT_H", "MNRET", "FCVT_L_H", "FCVT_LU_H", "FCVT_H_L", "FCVT_H_LU", "FENCE_I", "ANDN", "ORN", "XNOR", "Instructions32.SLLI", "Instructions32.SRLI", "Instructions32.SRAI", "SM4ED", "SM4KS", "SM3P0", "SM3P1", "DRET", "CLZ", "CTZ", "CPOP", "REV8", "ADD_UW", "SLLI_UW", "SH1ADD_UW", "SH2ADD_UW", "SH3ADD_UW", "SFENCE_VMA", "HFENCE_VVMA", "HFENCE_GVMA", "HLV_B", "HLV_BU", "HLV_H", "HLV_HU", "HLVX_HU", "HLV_W", "HLVX_WU", "HSV_B", "HSV_H", "HSV_W", "Instructions32.ZEXT_H", "CLMULR", "PACK", "PACKH", "BREV8", "SHA256SIG0", "SHA256SIG1", "SHA256SUM0", "SHA256SUM1", "BCLR", "BEXT", "BINV", "BSET", "Instructions32.REV8", "LD", "LWU", "SD", "SLLI", "SRLI", "SRAI", "ADDIW", "SLLIW", "SRLIW", "SRAIW", "ADDW", "SUBW", "SLLW", "SRLW", "SRAW", "MULW", "DIVW", "DIVUW", "REMW", "REMUW", "AMOADD_D", "AMOSWAP_D", "AMOXOR_D", "AMOAND_D", "AMOOR_D", "AMOMIN_D", "AMOMINU_D", "AMOMAX_D", "AMOMAXU_D", "LR_D", "SC_D", "MUL", "MULH", "MULHU", "MULHSU", "DIV", "DIVU", "REM", "REMU", "AMOADD_W", "AMOXOR_W", "AMOSWAP_W", "AMOAND_W", "AMOOR_W", "AMOMIN_W", "AMOMINU_W", "AMOMAX_W", "AMOMAXU_W", "LR_W", "SC_W", "MAX", "MAXU", "MIN", "MINU", 
    )
    val insns_Y: Seq[String] = Seq(
      "FCVT_S_D", "FCVT_D_S", "FSGNJ_D", "FSGNJX_D", "FSGNJN_D", "FMIN_D", "FMAX_D", "FADD_D", "FSUB_D", "FMUL_D", "FMADD_D", "FMSUB_D", "FNMADD_D", "FNMSUB_D", "FCLASS_D", "FCVT_W_D", "FCVT_WU_D", "FEQ_D", "FLT_D", "FLE_D", "FCVT_D_W", "FCVT_D_WU", "FLD", "FSD", "FDIV_D", "FSQRT_D", "FCVT_D_H", "FCVT_H_D", "FMV_X_D", "FCVT_L_D", "FCVT_LU_D", "FMV_D_X", "FCVT_D_L", "FCVT_D_LU", 
    )
    def genTable(op: Op): BitPat = {
      if (insns_N.contains(op.bitPat)) N else if (insns_Y.contains(op.bitPat)) Y else N
    }
  }


  val all: Seq[DecodeField[Op, _ >: Bool <: UInt]] = Seq(
    decode_legal,
    decode_fp,
    decode_rocc,
    decode_branch,
    decode_jal,
    decode_jalr,
    decode_rxs2,
    decode_rxs1,
    decode_scie,
    decode_zbk,
    decode_zkn,
    decode_zks,
    decode_sel_alu2,
    decode_sel_alu1,
    decode_sel_imm,
    decode_alu_dw,
    decode_alu_fn,
    decode_mem,
    decode_mem_cmd,
    decode_rfs1,
    decode_rfs2,
    decode_rfs3,
    decode_wfd,
    decode_mul,
    decode_div,
    decode_wxd,
    decode_csr,
    decode_fence_i,
    decode_fence,
    decode_amo,
    decode_dp,
  )

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

  private val decodeTable: DecodeTable[Op] = new DecodeTable[Op](truthTable, all)
  def bundle:              DecodeBundle = decodeTable.bundle

  def decode(inst: UInt) = {
    val decoder = decodeTable.decode(inst)
    legal := decoder(decode_legal)
    fp := decoder(decode_fp)
    rocc := decoder(decode_rocc)
    branch := decoder(decode_branch)
    jal := decoder(decode_jal)
    jalr := decoder(decode_jalr)
    rxs2 := decoder(decode_rxs2)
    rxs1 := decoder(decode_rxs1)
    scie := decoder(decode_scie)
    zbk := decoder(decode_zbk)
    zkn := decoder(decode_zkn)
    zks := decoder(decode_zks)
    sel_alu2 := decoder(decode_sel_alu2)
    sel_alu1 := decoder(decode_sel_alu1)
    sel_imm := decoder(decode_sel_imm)
    alu_dw := decoder(decode_alu_dw)
    alu_fn := decoder(decode_alu_fn)
    mem := decoder(decode_mem)
    mem_cmd := decoder(decode_mem_cmd)
    rfs1 := decoder(decode_rfs1)
    rfs2 := decoder(decode_rfs2)
    rfs3 := decoder(decode_rfs3)
    wfd := decoder(decode_wfd)
    mul := decoder(decode_mul)
    div := decoder(decode_div)
    wxd := decoder(decode_wxd)
    csr := decoder(decode_csr)
    fence_i := decoder(decode_fence_i)
    fence := decoder(decode_fence)
    amo := decoder(decode_amo)
    dp := decoder(decode_dp)
    val sigs = Seq(legal, fp, rocc, branch, jal, jalr, rxs2, rxs1, scie, zbk, zkn, zks, sel_alu2,
                   sel_alu1, sel_imm, alu_dw, alu_fn, mem, mem_cmd,
                   rfs1, rfs2, rfs3, wfd, mul, div, wxd, csr, fence_i, fence, amo, dp)
    this
  }
}
