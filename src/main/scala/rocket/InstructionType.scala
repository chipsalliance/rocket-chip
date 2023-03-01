package freechips.rocketchip.rocket

import chisel3.util.BitPat
import chisel3.util.experimental.decode._

case class Op(insn: BitPat) extends DecodePattern {
  def bitPat: BitPat = insn
}
object InstructionType {
  val SCIEType = Map(
    "SCIE.opcode" -> BitPat("b?????????????????????????0?01011"),
  )
  val ZKND64Type = Map(
    "AES64DS" -> BitPat("b0011101??????????000?????0110011"),
    "AES64DSM" -> BitPat("b0011111??????????000?????0110011"),
    "AES64IM" -> BitPat("b001100000000?????001?????0010011"),
    "AES64KS1I" -> BitPat("b00110001?????????001?????0010011"),
    "AES64KS2" -> BitPat("b0111111??????????000?????0110011"),
  )
  val ZBKB64Type = Map(
    "PACKW" -> BitPat("b0000100??????????100?????0111011"),
  )
  val ZKNH64Type = Map(
    "SHA512SIG0" -> BitPat("b000100000110?????001?????0010011"),
    "SHA512SIG1" -> BitPat("b000100000111?????001?????0010011"),
    "SHA512SUM0" -> BitPat("b000100000100?????001?????0010011"),
    "SHA512SUM1" -> BitPat("b000100000101?????001?????0010011"),
  )
  val ZBBRType = Map(
    "ROR" -> BitPat("b0110000??????????101?????0110011"),
    "ROL" -> BitPat("b0110000??????????001?????0110011"),
  )
  val Hypervisor64Type = Map(
    "HLV_D" -> BitPat("b011011000000?????100?????1110011"),
    "HSV_D" -> BitPat("b0110111??????????100000001110011"),
    "HLV_WU" -> BitPat("b011010000001?????100?????1110011"),
  )
  val FType = Map(
    "FSGNJ_S" -> BitPat("b0010000??????????000?????1010011"),
    "FSGNJX_S" -> BitPat("b0010000??????????010?????1010011"),
    "FSGNJN_S" -> BitPat("b0010000??????????001?????1010011"),
    "FMIN_S" -> BitPat("b0010100??????????000?????1010011"),
    "FMAX_S" -> BitPat("b0010100??????????001?????1010011"),
    "FADD_S" -> BitPat("b0000000??????????????????1010011"),
    "FSUB_S" -> BitPat("b0000100??????????????????1010011"),
    "FMUL_S" -> BitPat("b0001000??????????????????1010011"),
    "FMADD_S" -> BitPat("b?????00??????????????????1000011"),
    "FMSUB_S" -> BitPat("b?????00??????????????????1000111"),
    "FNMADD_S" -> BitPat("b?????00??????????????????1001111"),
    "FNMSUB_S" -> BitPat("b?????00??????????????????1001011"),
    "FCLASS_S" -> BitPat("b111000000000?????001?????1010011"),
    "FMV_X_W" -> BitPat("b111000000000?????000?????1010011"),
    "FCVT_W_S" -> BitPat("b110000000000?????????????1010011"),
    "FCVT_WU_S" -> BitPat("b110000000001?????????????1010011"),
    "FEQ_S" -> BitPat("b1010000??????????010?????1010011"),
    "FLT_S" -> BitPat("b1010000??????????001?????1010011"),
    "FLE_S" -> BitPat("b1010000??????????000?????1010011"),
    "FMV_W_X" -> BitPat("b111100000000?????000?????1010011"),
    "FCVT_S_W" -> BitPat("b110100000000?????????????1010011"),
    "FCVT_S_WU" -> BitPat("b110100000001?????????????1010011"),
    "FLW" -> BitPat("b?????????????????010?????0000111"),
    "FSW" -> BitPat("b?????????????????010?????0100111"),
    "FDIV_S" -> BitPat("b0001100??????????????????1010011"),
    "FSQRT_S" -> BitPat("b010110000000?????????????1010011"),
  )
  val ZBBC64Type = Map(
    "CLZW" -> BitPat("b011000000000?????001?????0011011"),
    "CTZW" -> BitPat("b011000000001?????001?????0011011"),
    "CPOPW" -> BitPat("b011000000010?????001?????0011011"),
  )
  val SType = Map(
    "SRET" -> BitPat("b00010000001000000000000001110011"),
  )
  val ZBS32Type = Map(
    "BCLRI" -> BitPat("b010010???????????001?????0010011"),
    "BEXTI" -> BitPat("b010010???????????101?????0010011"),
    "BINVI" -> BitPat("b011010???????????001?????0010011"),
    "BSETI" -> BitPat("b001010???????????001?????0010011"),
  )
  val ZBBR32Type = Map(
    "RORI" -> BitPat("b011000???????????101?????0010011"),
  )
  val ZBAType = Map(
    "SH1ADD" -> BitPat("b0010000??????????010?????0110011"),
    "SH2ADD" -> BitPat("b0010000??????????100?????0110011"),
    "SH3ADD" -> BitPat("b0010000??????????110?????0110011"),
  )
  val ZBBORCBType = Map(
    "ORC_B" -> BitPat("b001010000111?????101?????0010011"),
  )
  val RoCCType = Map(
    "CUSTOM0" -> BitPat("b?????????????????000?????0001011"),
    "CUSTOM0_RS1" -> BitPat("b?????????????????010?????0001011"),
    "CUSTOM0_RS1_RS2" -> BitPat("b?????????????????011?????0001011"),
    "CUSTOM0_RD" -> BitPat("b?????????????????100?????0001011"),
    "CUSTOM0_RD_RS1" -> BitPat("b?????????????????110?????0001011"),
    "CUSTOM0_RD_RS1_RS2" -> BitPat("b?????????????????111?????0001011"),
    "CUSTOM1" -> BitPat("b?????????????????000?????0101011"),
    "CUSTOM1_RS1" -> BitPat("b?????????????????010?????0101011"),
    "CUSTOM1_RS1_RS2" -> BitPat("b?????????????????011?????0101011"),
    "CUSTOM1_RD" -> BitPat("b?????????????????100?????0101011"),
    "CUSTOM1_RD_RS1" -> BitPat("b?????????????????110?????0101011"),
    "CUSTOM1_RD_RS1_RS2" -> BitPat("b?????????????????111?????0101011"),
    "CUSTOM2" -> BitPat("b?????????????????000?????1011011"),
    "CUSTOM2_RS1" -> BitPat("b?????????????????010?????1011011"),
    "CUSTOM2_RS1_RS2" -> BitPat("b?????????????????011?????1011011"),
    "CUSTOM2_RD" -> BitPat("b?????????????????100?????1011011"),
    "CUSTOM2_RD_RS1" -> BitPat("b?????????????????110?????1011011"),
    "CUSTOM2_RD_RS1_RS2" -> BitPat("b?????????????????111?????1011011"),
    "CUSTOM3" -> BitPat("b?????????????????000?????1111011"),
    "CUSTOM3_RS1" -> BitPat("b?????????????????010?????1111011"),
    "CUSTOM3_RS1_RS2" -> BitPat("b?????????????????011?????1111011"),
    "CUSTOM3_RD" -> BitPat("b?????????????????100?????1111011"),
    "CUSTOM3_RD_RS1" -> BitPat("b?????????????????110?????1111011"),
    "CUSTOM3_RD_RS1_RS2" -> BitPat("b?????????????????111?????1111011"),
  )
  val ZKNE32Type = Map(
    "AES32ESI" -> BitPat("b??10001??????????000?????0110011"),
    "AES32ESMI" -> BitPat("b??10011??????????000?????0110011"),
  )
  val DType = Map(
    "FCVT_S_D" -> BitPat("b010000000001?????????????1010011"),
    "FCVT_D_S" -> BitPat("b010000100000?????????????1010011"),
    "FSGNJ_D" -> BitPat("b0010001??????????000?????1010011"),
    "FSGNJX_D" -> BitPat("b0010001??????????010?????1010011"),
    "FSGNJN_D" -> BitPat("b0010001??????????001?????1010011"),
    "FMIN_D" -> BitPat("b0010101??????????000?????1010011"),
    "FMAX_D" -> BitPat("b0010101??????????001?????1010011"),
    "FADD_D" -> BitPat("b0000001??????????????????1010011"),
    "FSUB_D" -> BitPat("b0000101??????????????????1010011"),
    "FMUL_D" -> BitPat("b0001001??????????????????1010011"),
    "FMADD_D" -> BitPat("b?????01??????????????????1000011"),
    "FMSUB_D" -> BitPat("b?????01??????????????????1000111"),
    "FNMADD_D" -> BitPat("b?????01??????????????????1001111"),
    "FNMSUB_D" -> BitPat("b?????01??????????????????1001011"),
    "FCLASS_D" -> BitPat("b111000100000?????001?????1010011"),
    "FCVT_W_D" -> BitPat("b110000100000?????????????1010011"),
    "FCVT_WU_D" -> BitPat("b110000100001?????????????1010011"),
    "FEQ_D" -> BitPat("b1010001??????????010?????1010011"),
    "FLT_D" -> BitPat("b1010001??????????001?????1010011"),
    "FLE_D" -> BitPat("b1010001??????????000?????1010011"),
    "FCVT_D_W" -> BitPat("b110100100000?????????????1010011"),
    "FCVT_D_WU" -> BitPat("b110100100001?????????????1010011"),
    "FLD" -> BitPat("b?????????????????011?????0000111"),
    "FSD" -> BitPat("b?????????????????011?????0100111"),
    "FDIV_D" -> BitPat("b0001101??????????????????1010011"),
    "FSQRT_D" -> BitPat("b010110100000?????????????1010011"),
  )
  val ZBKB32Type = Map(
    "ZIP" -> BitPat("b000010001111?????001?????0010011"),
    "UNZIP" -> BitPat("b000010001111?????101?????0010011"),
  )
  val F64Type = Map(
    "FCVT_L_S" -> BitPat("b110000000010?????????????1010011"),
    "FCVT_LU_S" -> BitPat("b110000000011?????????????1010011"),
    "FCVT_S_L" -> BitPat("b110100000010?????????????1010011"),
    "FCVT_S_LU" -> BitPat("b110100000011?????????????1010011"),
  )
  val ZKND32Type = Map(
    "AES32DSI" -> BitPat("b??10101??????????000?????0110011"),
    "AES32DSMI" -> BitPat("b??10111??????????000?????0110011"),
  )
  val ZKNH32Type = Map(
    "SHA512SIG0L" -> BitPat("b0101010??????????000?????0110011"),
    "SHA512SIG1L" -> BitPat("b0101011??????????000?????0110011"),
    "SHA512SIG0H" -> BitPat("b0101110??????????000?????0110011"),
    "SHA512SIG1H" -> BitPat("b0101111??????????000?????0110011"),
    "SHA512SUM0R" -> BitPat("b0101000??????????000?????0110011"),
    "SHA512SUM1R" -> BitPat("b0101001??????????000?????0110011"),
  )
  val HType = Map(
    "FCVT_S_H" -> BitPat("b010000000010?????????????1010011"),
    "FCVT_H_S" -> BitPat("b010001000000?????????????1010011"),
    "FSGNJ_H" -> BitPat("b0010010??????????000?????1010011"),
    "FSGNJX_H" -> BitPat("b0010010??????????010?????1010011"),
    "FSGNJN_H" -> BitPat("b0010010??????????001?????1010011"),
    "FMIN_H" -> BitPat("b0010110??????????000?????1010011"),
    "FMAX_H" -> BitPat("b0010110??????????001?????1010011"),
    "FADD_H" -> BitPat("b0000010??????????????????1010011"),
    "FSUB_H" -> BitPat("b0000110??????????????????1010011"),
    "FMUL_H" -> BitPat("b0001010??????????????????1010011"),
    "FMADD_H" -> BitPat("b?????10??????????????????1000011"),
    "FMSUB_H" -> BitPat("b?????10??????????????????1000111"),
    "FNMADD_H" -> BitPat("b?????10??????????????????1001111"),
    "FNMSUB_H" -> BitPat("b?????10??????????????????1001011"),
    "FCLASS_H" -> BitPat("b111001000000?????001?????1010011"),
    "FMV_X_H" -> BitPat("b111001000000?????000?????1010011"),
    "FCVT_W_H" -> BitPat("b110001000000?????????????1010011"),
    "FCVT_WU_H" -> BitPat("b110001000001?????????????1010011"),
    "FEQ_H" -> BitPat("b1010010??????????010?????1010011"),
    "FLT_H" -> BitPat("b1010010??????????001?????1010011"),
    "FLE_H" -> BitPat("b1010010??????????000?????1010011"),
    "FMV_H_X" -> BitPat("b111101000000?????000?????1010011"),
    "FCVT_H_W" -> BitPat("b110101000000?????????????1010011"),
    "FCVT_H_WU" -> BitPat("b110101000001?????????????1010011"),
    "FLH" -> BitPat("b?????????????????001?????0000111"),
    "FSH" -> BitPat("b?????????????????001?????0100111"),
    "FDIV_H" -> BitPat("b0001110??????????????????1010011"),
    "FSQRT_H" -> BitPat("b010111000000?????????????1010011"),
  )
  val ZKNE64Type = Map(
    "AES64ES" -> BitPat("b0011001??????????000?????0110011"),
    "AES64ESM" -> BitPat("b0011011??????????000?????0110011"),
  )
  val ZBCType = Map(
    "CLMUL" -> BitPat("b0000101??????????001?????0110011"),
    "CLMULH" -> BitPat("b0000101??????????011?????0110011"),
  )
  val ZBS64Type = Map(
    "BCLRI" -> BitPat("b010010???????????001?????0010011"),
    "BEXTI" -> BitPat("b010010???????????101?????0010011"),
    "BINVI" -> BitPat("b011010???????????001?????0010011"),
    "BSETI" -> BitPat("b001010???????????001?????0010011"),
  )
  val ZBBR64Type = Map(
    "RORI" -> BitPat("b011000???????????101?????0010011"),
    "RORW" -> BitPat("b0110000??????????101?????0111011"),
    "ROLW" -> BitPat("b0110000??????????001?????0111011"),
    "RORIW" -> BitPat("b0110000??????????101?????0011011"),
  )
  val HDType = Map(
    "FCVT_D_H" -> BitPat("b010000100010?????????????1010011"),
    "FCVT_H_D" -> BitPat("b010001000001?????????????1010011"),
  )
  val CFlushType = Map(
    "zapRs1(CFLUSH_D_L1)" -> BitPat("b111111000000?????000000001110011"),
    "zapRs1(CDISCARD_D_L1)" -> BitPat("b111111000010?????000000001110011"),
  )
  val IType = Map(
    "BNE" -> BitPat("b?????????????????001?????1100011"),
    "BEQ" -> BitPat("b?????????????????000?????1100011"),
    "BLT" -> BitPat("b?????????????????100?????1100011"),
    "BLTU" -> BitPat("b?????????????????110?????1100011"),
    "BGE" -> BitPat("b?????????????????101?????1100011"),
    "BGEU" -> BitPat("b?????????????????111?????1100011"),
    "JAL" -> BitPat("b?????????????????????????1101111"),
    "JALR" -> BitPat("b?????????????????000?????1100111"),
    "AUIPC" -> BitPat("b?????????????????????????0010111"),
    "LB" -> BitPat("b?????????????????000?????0000011"),
    "LH" -> BitPat("b?????????????????001?????0000011"),
    "LW" -> BitPat("b?????????????????010?????0000011"),
    "LBU" -> BitPat("b?????????????????100?????0000011"),
    "LHU" -> BitPat("b?????????????????101?????0000011"),
    "SB" -> BitPat("b?????????????????000?????0100011"),
    "SH" -> BitPat("b?????????????????001?????0100011"),
    "SW" -> BitPat("b?????????????????010?????0100011"),
    "LUI" -> BitPat("b?????????????????????????0110111"),
    "ADDI" -> BitPat("b?????????????????000?????0010011"),
    "SLTI" -> BitPat("b?????????????????010?????0010011"),
    "SLTIU" -> BitPat("b?????????????????011?????0010011"),
    "ANDI" -> BitPat("b?????????????????111?????0010011"),
    "ORI" -> BitPat("b?????????????????110?????0010011"),
    "XORI" -> BitPat("b?????????????????100?????0010011"),
    "ADD" -> BitPat("b0000000??????????000?????0110011"),
    "SUB" -> BitPat("b0100000??????????000?????0110011"),
    "SLT" -> BitPat("b0000000??????????010?????0110011"),
    "SLTU" -> BitPat("b0000000??????????011?????0110011"),
    "AND" -> BitPat("b0000000??????????111?????0110011"),
    "OR" -> BitPat("b0000000??????????110?????0110011"),
    "XOR" -> BitPat("b0000000??????????100?????0110011"),
    "SLL" -> BitPat("b0000000??????????001?????0110011"),
    "SRL" -> BitPat("b0000000??????????101?????0110011"),
    "SRA" -> BitPat("b0100000??????????101?????0110011"),
    "FENCE" -> BitPat("b?????????????????000?????0001111"),
    "ECALL" -> BitPat("b00000000000000000000000001110011"),
    "EBREAK" -> BitPat("b00000000000100000000000001110011"),
    "MRET" -> BitPat("b00110000001000000000000001110011"),
    "WFI" -> BitPat("b00010000010100000000000001110011"),
    "CEASE" -> BitPat("b00110000010100000000000001110011"),
    "CSRRW" -> BitPat("b?????????????????001?????1110011"),
    "CSRRS" -> BitPat("b?????????????????010?????1110011"),
    "CSRRC" -> BitPat("b?????????????????011?????1110011"),
    "CSRRWI" -> BitPat("b?????????????????101?????1110011"),
    "CSRRSI" -> BitPat("b?????????????????110?????1110011"),
    "CSRRCI" -> BitPat("b?????????????????111?????1110011"),
  )
  val ZBBSEType = Map(
    "SEXT_H" -> BitPat("b011000000101?????001?????0010011"),
    "SEXT_B" -> BitPat("b011000000100?????001?????0010011"),
  )
  val ZBKXType = Map(
    "XPERM8" -> BitPat("b0010100??????????100?????0110011"),
    "XPERM4" -> BitPat("b0010100??????????010?????0110011"),
  )
  val D64Type = Map(
    "FMV_X_D" -> BitPat("b111000100000?????000?????1010011"),
    "FCVT_L_D" -> BitPat("b110000100010?????????????1010011"),
    "FCVT_LU_D" -> BitPat("b110000100011?????????????1010011"),
    "FMV_D_X" -> BitPat("b111100100000?????000?????1010011"),
    "FCVT_D_L" -> BitPat("b110100100010?????????????1010011"),
    "FCVT_D_LU" -> BitPat("b110100100011?????????????1010011"),
  )
  val ZBBZE64Type = Map(
    "ZEXT_H" -> BitPat("b000010000000?????100?????0111011"),
  )
  val NMIType = Map(
    "MNRET" -> BitPat("b01110000001000000000000001110011"),
  )
  val H64Type = Map(
    "FCVT_L_H" -> BitPat("b110001000010?????????????1010011"),
    "FCVT_LU_H" -> BitPat("b110001000011?????????????1010011"),
    "FCVT_H_L" -> BitPat("b110101000010?????????????1010011"),
    "FCVT_H_LU" -> BitPat("b110101000011?????????????1010011"),
  )
  val FenceIType = Map(
    "FENCE_I" -> BitPat("b?????????????????001?????0001111"),
  )
  val ZBBNType = Map(
    "ANDN" -> BitPat("b0100000??????????111?????0110011"),
    "ORN" -> BitPat("b0100000??????????110?????0110011"),
    "XNOR" -> BitPat("b0100000??????????100?????0110011"),
  )
  val I32Type = Map(
    "SLLI" -> BitPat("b000000???????????001?????0010011"),
    "SRLI" -> BitPat("b000000???????????101?????0010011"),
    "SRAI" -> BitPat("b010000???????????101?????0010011"),
  )
  val ZKSType = Map(
    "SM4ED" -> BitPat("b??11000??????????000?????0110011"),
    "SM4KS" -> BitPat("b??11010??????????000?????0110011"),
    "SM3P0" -> BitPat("b000100001000?????001?????0010011"),
    "SM3P1" -> BitPat("b000100001001?????001?????0010011"),
  )
  val DebugType = Map(
    "DRET" -> BitPat("b01111011001000000000000001110011"),
  )
  val ZBBCType = Map(
    "CLZ" -> BitPat("b011000000000?????001?????0010011"),
    "CTZ" -> BitPat("b011000000001?????001?????0010011"),
    "CPOP" -> BitPat("b011000000010?????001?????0010011"),
  )
  val ZBBREV864Type = Map(
    "REV8" -> BitPat("b011010111000?????101?????0010011"),
  )
  val ZBA64Type = Map(
    "ADD_UW" -> BitPat("b0000100??????????000?????0111011"),
    "SLLI_UW" -> BitPat("b000010???????????001?????0011011"),
    "SH1ADD_UW" -> BitPat("b0010000??????????010?????0111011"),
    "SH2ADD_UW" -> BitPat("b0010000??????????100?????0111011"),
    "SH3ADD_UW" -> BitPat("b0010000??????????110?????0111011"),
  )
  val SVMType = Map(
    "SFENCE_VMA" -> BitPat("b0001001??????????000000001110011"),
  )
  val HypervisorType = Map(
    "HFENCE_VVMA" -> BitPat("b0010001??????????000000001110011"),
    "HFENCE_GVMA" -> BitPat("b0110001??????????000000001110011"),
    "HLV_B" -> BitPat("b011000000000?????100?????1110011"),
    "HLV_BU" -> BitPat("b011000000001?????100?????1110011"),
    "HLV_H" -> BitPat("b011001000000?????100?????1110011"),
    "HLV_HU" -> BitPat("b011001000001?????100?????1110011"),
    "HLVX_HU" -> BitPat("b011001000011?????100?????1110011"),
    "HLV_W" -> BitPat("b011010000000?????100?????1110011"),
    "HLVX_WU" -> BitPat("b011010000011?????100?????1110011"),
    "HSV_B" -> BitPat("b0110001??????????100000001110011"),
    "HSV_H" -> BitPat("b0110011??????????100000001110011"),
    "HSV_W" -> BitPat("b0110101??????????100000001110011"),
  )
  val ZBBZE32Type = Map(
    "ZEXT_H" -> BitPat("b000010000000?????100?????0111011"),
  )
  val ZBCRType = Map(
    "CLMULR" -> BitPat("b0000101??????????010?????0110011"),
  )
  val ZBKBType = Map(
    "PACK" -> BitPat("b0000100??????????100?????0110011"),
    "PACKH" -> BitPat("b0000100??????????111?????0110011"),
    "BREV8" -> BitPat("b011010000111?????101?????0010011"),
  )
  val ZKNHType = Map(
    "SHA256SIG0" -> BitPat("b000100000010?????001?????0010011"),
    "SHA256SIG1" -> BitPat("b000100000011?????001?????0010011"),
    "SHA256SUM0" -> BitPat("b000100000000?????001?????0010011"),
    "SHA256SUM1" -> BitPat("b000100000001?????001?????0010011"),
  )
  val ZBSType = Map(
    "BCLR" -> BitPat("b0100100??????????001?????0110011"),
    "BEXT" -> BitPat("b0100100??????????101?????0110011"),
    "BINV" -> BitPat("b0110100??????????001?????0110011"),
    "BSET" -> BitPat("b0010100??????????001?????0110011"),
  )
  val ZBBREV832Type = Map(
    "REV8" -> BitPat("b011010111000?????101?????0010011"),
  )
  val I64Type = Map(
    "LD" -> BitPat("b?????????????????011?????0000011"),
    "LWU" -> BitPat("b?????????????????110?????0000011"),
    "SD" -> BitPat("b?????????????????011?????0100011"),
    "SLLI" -> BitPat("b000000???????????001?????0010011"),
    "SRLI" -> BitPat("b000000???????????101?????0010011"),
    "SRAI" -> BitPat("b010000???????????101?????0010011"),
    "ADDIW" -> BitPat("b?????????????????000?????0011011"),
    "SLLIW" -> BitPat("b0000000??????????001?????0011011"),
    "SRLIW" -> BitPat("b0000000??????????101?????0011011"),
    "SRAIW" -> BitPat("b0100000??????????101?????0011011"),
    "ADDW" -> BitPat("b0000000??????????000?????0111011"),
    "SUBW" -> BitPat("b0100000??????????000?????0111011"),
    "SLLW" -> BitPat("b0000000??????????001?????0111011"),
    "SRLW" -> BitPat("b0000000??????????101?????0111011"),
    "SRAW" -> BitPat("b0100000??????????101?????0111011"),
  )
  val M64Type = Map(
    "MULW" -> BitPat("b0000001??????????000?????0111011"),
    "DIVW" -> BitPat("b0000001??????????100?????0111011"),
    "DIVUW" -> BitPat("b0000001??????????101?????0111011"),
    "REMW" -> BitPat("b0000001??????????110?????0111011"),
    "REMUW" -> BitPat("b0000001??????????111?????0111011"),
  )
  val A64Type = Map(
    "AMOADD_D" -> BitPat("b00000????????????011?????0101111"),
    "AMOSWAP_D" -> BitPat("b00001????????????011?????0101111"),
    "AMOXOR_D" -> BitPat("b00100????????????011?????0101111"),
    "AMOAND_D" -> BitPat("b01100????????????011?????0101111"),
    "AMOOR_D" -> BitPat("b01000????????????011?????0101111"),
    "AMOMIN_D" -> BitPat("b10000????????????011?????0101111"),
    "AMOMINU_D" -> BitPat("b11000????????????011?????0101111"),
    "AMOMAX_D" -> BitPat("b10100????????????011?????0101111"),
    "AMOMAXU_D" -> BitPat("b11100????????????011?????0101111"),
    "LR_D" -> BitPat("b00010??00000?????011?????0101111"),
    "SC_D" -> BitPat("b00011????????????011?????0101111"),
  )
  val MType = Map(
    "MUL" -> BitPat("b0000001??????????000?????0110011"),
    "MULH" -> BitPat("b0000001??????????001?????0110011"),
    "MULHU" -> BitPat("b0000001??????????011?????0110011"),
    "MULHSU" -> BitPat("b0000001??????????010?????0110011"),
    "DIV" -> BitPat("b0000001??????????100?????0110011"),
    "DIVU" -> BitPat("b0000001??????????101?????0110011"),
    "REM" -> BitPat("b0000001??????????110?????0110011"),
    "REMU" -> BitPat("b0000001??????????111?????0110011"),
  )
  val AType = Map(
    "AMOADD_W" -> BitPat("b00000????????????010?????0101111"),
    "AMOXOR_W" -> BitPat("b00100????????????010?????0101111"),
    "AMOSWAP_W" -> BitPat("b00001????????????010?????0101111"),
    "AMOAND_W" -> BitPat("b01100????????????010?????0101111"),
    "AMOOR_W" -> BitPat("b01000????????????010?????0101111"),
    "AMOMIN_W" -> BitPat("b10000????????????010?????0101111"),
    "AMOMINU_W" -> BitPat("b11000????????????010?????0101111"),
    "AMOMAX_W" -> BitPat("b10100????????????010?????0101111"),
    "AMOMAXU_W" -> BitPat("b11100????????????010?????0101111"),
    "LR_W" -> BitPat("b00010??00000?????010?????0101111"),
    "SC_W" -> BitPat("b00011????????????010?????0101111"),
  )
  val ZBBMType = Map(
    "MAX" -> BitPat("b0000101??????????110?????0110011"),
    "MAXU" -> BitPat("b0000101??????????111?????0110011"),
    "MIN" -> BitPat("b0000101??????????100?????0110011"),
    "MINU" -> BitPat("b0000101??????????101?????0110011"),
  )
  val AllType = SCIEType ++ ZKND64Type ++ ZBKB64Type ++ ZKNH64Type ++ ZBBRType ++ Hypervisor64Type ++ FType ++ ZBBC64Type ++ SType ++ ZBS32Type ++ ZBBR32Type ++ ZBAType ++ ZBBORCBType ++ RoCCType ++ ZKNE32Type ++ DType ++ ZBKB32Type ++ F64Type ++ ZKND32Type ++ ZKNH32Type ++ HType ++ ZKNE64Type ++ ZBCType ++ ZBS64Type ++ ZBBR64Type ++ HDType ++ CFlushType ++ IType ++ ZBBSEType ++ ZBKXType ++ D64Type ++ ZBBZE64Type ++ NMIType ++ H64Type ++ FenceIType ++ ZBBNType ++ I32Type ++ ZKSType ++ DebugType ++ ZBBCType ++ ZBBREV864Type ++ ZBA64Type ++ SVMType ++ HypervisorType ++ ZBBZE32Type ++ ZBCRType ++ ZBKBType ++ ZKNHType ++ ZBSType ++ ZBBREV832Type ++ I64Type ++ M64Type ++ A64Type ++ MType ++ AType ++ ZBBMType
} 

