// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2023 Jiuyang Liu <liu@jiuyang.me>

package org.chipsalliance.rocketcore.decoder

import chisel3._
import chisel3.util.BitPat
import chisel3.util.experimental.decode._
import org.chipsalliance.rvdecoderdb.{Encoding, Instruction, InstructionSet, Utils}

// behave like ChiselEnum, but for compatibility, use UInt for now.
// This is going to be upstreamed to Chisel in the future.
trait UOP {
  def width: Int

  def dontCare: BitPat = BitPat.dontCare(width)

  def chiselType: TPE = UInt(width.W)

  def encode(lit: Int): BitPat = BitPat(lit.U(width.W))

  def encode(strLit: String): BitPat = BitPat(strLit.U(width.W))

  type TPE = UInt
}

trait UOPDecodeField[T <: DecodePattern] extends DecodeField[T, UInt] {
  def uopType: UOP

  def chiselType: UInt = uopType.chiselType
}

/** DecodePattern for an RISC-V instruction */
case class RocketDecodePattern(instruction: Instruction) extends DecodePattern {
  def isRoCC: Boolean = instruction.instructionSet.name == "rv_rocc"

  override def bitPat: BitPat = BitPat("b"+instruction.encoding.toString)
}

object CustomInstructions {
  private def rocket(name: String, encoding: Encoding) =
    Instruction(name, encoding, Seq(), Seq(InstructionSet("rv_rocket")), None, false, true)

  val rocketSet = Seq(
    rocket("c.flush.d.l1", Encoding.fromString("111111000000?????000000001110011")),
    rocket("c.discard.d.l1", Encoding.fromString("111111000010?????000000001110011")),
    rocket("cease", Encoding.fromString("00110000010100000000000001110011"))
  )

  private def rocc(name: String, encoding: Encoding) =
    Instruction(name, encoding, Seq(), Seq(InstructionSet("rv_rocc")), None, false, true)
  val roccSet = Seq(
    rocc("custom0", Encoding.fromString("?????????????????000?????0001011")),
    rocc("custom0.rs1", Encoding.fromString("?????????????????010?????0001011")),
    rocc("custom0.rs1.rs2", Encoding.fromString("?????????????????011?????0001011")),
    rocc("custom0.rd", Encoding.fromString("?????????????????100?????0001011")),
    rocc("custom0.rd.rs1", Encoding.fromString("?????????????????110?????0001011")),
    rocc("custom0.rd.rs1.rs2", Encoding.fromString("?????????????????111?????0001011")),
    rocc("custom1", Encoding.fromString("?????????????????000?????0101011")),
    rocc("custom1.rs1", Encoding.fromString("?????????????????010?????0101011")),
    rocc("custom1.rs1.rs2", Encoding.fromString("?????????????????011?????0101011")),
    rocc("custom1.rd", Encoding.fromString("?????????????????100?????0101011")),
    rocc("custom1.rd.rs1", Encoding.fromString("?????????????????110?????0101011")),
    rocc("custom1.rd.rs1.rs2", Encoding.fromString("?????????????????111?????0101011")),
    rocc("custom2", Encoding.fromString("?????????????????000?????1011011")),
    rocc("custom2.rs1", Encoding.fromString("?????????????????010?????1011011")),
    rocc("custom2.rs1.rs2", Encoding.fromString("?????????????????011?????1011011")),
    rocc("custom2.rd", Encoding.fromString("?????????????????100?????1011011")),
    rocc("custom2.rd.rs1", Encoding.fromString("?????????????????110?????1011011")),
    rocc("custom2.rd.rs1.rs2", Encoding.fromString("?????????????????111?????1011011")),
    rocc("custom3", Encoding.fromString("?????????????????000?????1111011")),
    rocc("custom3.rs1", Encoding.fromString("?????????????????010?????1111011")),
    rocc("custom3.rs1.rs2", Encoding.fromString("?????????????????011?????1111011")),
    rocc("custom3.rd", Encoding.fromString("?????????????????100?????1111011")),
    rocc("custom3.rd.rs1", Encoding.fromString("?????????????????110?????1111011")),
    rocc("custom3.rd.rs1.rs2", Encoding.fromString("?????????????????111?????1111011"))
  )
}

/** Parameter for InstructionDecoder
  * @param instructions Instructions supported by
  * @param pipelinedMul
  * @param fenceIFlushDCache
  */
case class InstructionDecoderParameter(
                                        instructions: Seq[Instruction],
                                        // uarch configurations on rocket decoder
                                        pipelinedMul: Boolean,
                                        fenceIFlushDCache: Boolean
                                      )
/** factory to generate the rocket core decoder. */
class InstructionDecoder(p: InstructionDecoderParameter) {
  private val instructions = p.instructions

  // functions below is my little reminder, which is used for future rocket core refactoring, just keep it, I'll remove it later in the future.
  private def hasAnySetIn(sets: String*): Boolean = sets.exists(set => instructions.flatMap(_.instructionSets.map(_.name)).exists(_.contains(set)))

  private def xLen32: Boolean = instructions.map(_.instructionSet.name).exists(_.startsWith("rv32_"))

  private def xLen64: Boolean = instructions.map(_.instructionSet.name).exists(_.startsWith("rv64_"))

  private def fLen0: Boolean = !fLen32 && !fLen64

  private def fLen32: Boolean = hasAnySetIn("rv_f", "rv32_f", "rv64_f")

  private def fLen64: Boolean = hasAnySetIn("rv_d", "rv32_d", "rv64_d")

  private def zfh: Boolean = hasAnySetIn("rv_zfh", "rv64_zfh", "rv_d_zfh")

  private def usingAtomics: Boolean = hasAnySetIn("rv_a", "rv64_a")

  private def usingBitManip: Boolean = hasAnySetIn("rv_zba", "rv64_zba", "rv_zbb", "rv32_zbb", "rv64_zbb", "rv_zbc", "rv_zbs")

  private def usingBitManipCrypto: Boolean = hasAnySetIn("rv_zbkb", "rv32_zbkb", "rv64_zbkb", "rv_zbkc", "rv_zbkx")

  private def usingCryptoNIST: Boolean = hasAnySetIn("rv32_zknd", "rv64_zknd", "rv32_zkne", "rv64_zkne", "rv_zknh", "rv32_zknh", "rv64_zknh", "rv_zkn", "rv32_zkn", "rv64_zkn")

  private def usingCryptoSM: Boolean = hasAnySetIn("rv_zksed", "rv_zksh", "rv_zks", "rv32_zks", "rv64_zks", "rv_zk")

  private def usingConditionalZero = hasAnySetIn("rv_zicond")

  /** use ablu for ALU which supports zb zk */
  private val useABLU: Boolean = usingBitManip || usingBitManipCrypto || usingCryptoNIST || usingCryptoSM
  private val useFPU = !fLen0
  private val useRoCC = hasAnySetIn("rv_rocc")
  private val useVector = hasAnySetIn("rv_v")
  private val useMulDiv = hasAnySetIn("rv_m", "rv64_m")

  private val instructionDecodePatterns: Seq[RocketDecodePattern] = instructions.map(RocketDecodePattern.apply)
  private val instructionDecodeFields: Seq[DecodeField[RocketDecodePattern, _ <: Data]] = Seq(
    isLegal, isBranch, isJal, isJalr, rxs2, rxs1, selAlu2, selAlu1, selImm, aluDoubleWords, mem, memCommand, wxd, csr, fenceI, fence, amo,
  ) ++
    (if (useABLU) Seq(abluFn, zbk, zkn, zks) else Some(aluFn)) ++
    (if (useFPU) Seq(fp, rfs1, rfs2, rfs3, wfd, dp) else None) ++
    (if (useMulDiv) if (p.pipelinedMul) Seq(mul, div) else Seq(div) else None) ++
    (if (useRoCC) Some(rocc) else None) ++
    (if (useVector) Seq(isVector, vload, vstore, vcsr) else None)

  val table: DecodeTable[RocketDecodePattern] = new DecodeTable[RocketDecodePattern](
    instructionDecodePatterns,
    instructionDecodeFields
  )

  object isLegal extends BoolDecodeField[RocketDecodePattern] {
    override def name: String = "legal"

    override def default: BitPat = n

    // should always be true
    override def genTable(op: RocketDecodePattern): BitPat = y
  }

  object fp extends BoolDecodeField[RocketDecodePattern] {
    override def name: String = "fp"

    override def genTable(op: RocketDecodePattern): BitPat = op.instruction.instructionSet.name match {
      // format: off
      case s if Seq(
        "rv_d", "rv64_d",
        "rv_f", "rv64_f",
        "rv_q", "rv64_q",
        "rv_zfh", "rv64_zfh", "rv_d_zfh", "rv_q_zfh",
      ).contains(s) => y
      case _ => n
      // format: on
    }
  }

  object dp extends BoolDecodeField[RocketDecodePattern] {
    override def name: String = "dp"

    override def genTable(op: RocketDecodePattern): BitPat = op.instruction.instructionSet.name match {
      // format: off
      case s if Seq("rv_d", "rv_d_zfh", "rv64_d").contains(s) => y
      case _ => n
      // format: on
    }
  }

  object isBranch extends BoolDecodeField[RocketDecodePattern] {
    override def name: String = "branch"

    override def genTable(op: RocketDecodePattern): BitPat = op.instruction.name match {
      // format: off
      case i if Seq("bne", "beq", "blt", "bltu", "bge", "bgeu").contains(i) => y
      case _ => n
      // format: on
    }
  }

  object isJal extends BoolDecodeField[RocketDecodePattern] {
    override def name: String = "jal"

    override def genTable(op: RocketDecodePattern): BitPat = op.instruction.name match {
      // format: off
      case i if Seq("jal").contains(i) => y
      case _ => n
      // format: on
    }
  }

  object isJalr extends BoolDecodeField[RocketDecodePattern] {
    override def name: String = "jalr"

    override def genTable(op: RocketDecodePattern): BitPat = op.instruction.name match {
      // format: off
      case i if Seq("jalr").contains(i) => y
      case _ => n
      // format: on
    }
  }

  object rxs2 extends BoolDecodeField[RocketDecodePattern] {
    override def name: String = "rxs2"

    override def genTable(op: RocketDecodePattern): BitPat = op.instruction.name match {
      case i if Seq("bne", "beq", "blt", "bltu", "bge", "bgeu", "sb", "sh", "sw", "add", "sub", "slt", "sltu", "and", "or", "xor", "sll", "srl", "sra", "czero.eqz", "czero.nez", "sfence.vma", "hfence.vvma", "hfence.gvma", "hsv.b", "hsv.h", "hsv.w", "sd", "addw", "subw", "sllw", "srlw", "sraw", "hsv.d", "mul", "mulh", "mulhu", "mulhsu", "div", "divu", "rem", "remu", "mulw", "divw", "divuw", "remw", "remuw", "amoadd.w", "amoxor.w", "amoswap.w", "amoand.w", "amoor.w", "amomin.w", "amominu.w", "amomax.w", "amomaxu.w", "lr.w", "sc.w", "amoadd.d", "amoswap.d", "amoxor.d", "amoand.d", "amoor.d", "amomin.d", "amominu.d", "amomax.d", "amomaxu.d", "lr.d", "sc.d", "sh1add", "sh2add", "sh3add", "add.uw", "sh1add.uw", "sh2add.uw", "sh3add.uw", "andn", "orn", "xnor", "ror", "rol", "rorw", "rolw", "max", "maxu", "min", "minu", "pack", "packh", "packw", "clmul", "clmulh", "clmulr", "xperm8", "xperm4", "bclr", "bext", "binv", "bset", "aes32dsi", "aes32dsmi", "aes64ds", "aes64dsm", "aes64im", "aes64ks2", "aes32esi", "aes32esmi", "aes64es", "aes64esm", "sha512sig0l", "sha512sig1l", "sha512sig0h", "sha512sig1h", "sha512sum0r", "sha512sum1r", "sm4ed", "sm4ks", "custom0.rs1.rs2", "custom0.rd.rs1.rs2", "custom1.rs1.rs2", "custom1.rd.rs1.rs2", "custom2.rs1.rs2", "custom2.rd.rs1.rs2", "custom3.rs1.rs2", "custom3.rd.rs1.rs2").contains(i) => y
      case _ => n
    }
  }

  object rxs1 extends BoolDecodeField[RocketDecodePattern] {
    override def name: String = "rxs1"

    override def genTable(op: RocketDecodePattern): BitPat = op.instruction.name match {
      case i if Seq("bne", "beq", "blt", "bltu", "bge", "bgeu", "jalr", "lb", "lh", "lw", "lbu", "lhu", "sb", "sh", "sw", "addi", "slti", "sltiu", "andi", "ori", "xori", "add", "sub", "slt", "sltu", "and", "or", "xor", "sll", "srl", "sra", "csrrw", "csrrs", "csrrc", "czero.eqz", "czero.nez", "cflush.d.l1", "cdiscard.d.l1", "sfence.vma", "hfence.vvma", "hfence.gvma", "hlv.b", "hlv.bu", "hlv.h", "hlv.hu", "hlvx.hu", "hlv.w", "hlvx.wu", "hsv.b", "hsv.h", "hsv.w", "srli", "srai", "ld", "lwu", "sd", "slli", "srli", "srai", "addiw", "slliw", "srliw", "sraiw", "addw", "subw", "sllw", "srlw", "sraw", "hlv.d", "hsv.d", "hlv.wu", "mul", "mulh", "mulhu", "mulhsu", "div", "divu", "rem", "remu", "mulw", "divw", "divuw", "remw", "remuw", "amoadd.w", "amoxor.w", "amoswap.w", "amoand.w", "amoor.w", "amomin.w", "amominu.w", "amomax.w", "amomaxu.w", "lr.w", "sc.w", "amoadd.d", "amoswap.d", "amoxor.d", "amoand.d", "amoor.d", "amomin.d", "amominu.d", "amomax.d", "amomaxu.d", "lr.d", "sc.d", "fmv.h.x", "fcvt.h.w", "fcvt.h.wu", "flh", "fsh", "fmv.w.x", "fcvt.s.w", "fcvt.s.wu", "flw", "fsw", "fcvt.d.w", "fcvt.d.wu", "fld", "fsd", "fcvt.h.l", "fcvt.h.lu", "fcvt.s.l", "fcvt.s.lu", "fmv.d.x", "fcvt.d.l", "fcvt.d.lu", "sh1add", "sh2add", "sh3add", "add.uw", "slli.uw", "sh1add.uw", "sh2add.uw", "sh3add.uw", "andn", "orn", "xnor", "ror", "rol", "rori", "rori", "rorw", "rolw", "roriw", "clz", "ctz", "cpop", "clzw", "ctzw", "cpopw", "max", "maxu", "min", "minu", "sext.h", "sext.b", "zext.h", "zext.h", "orc.b", "rev8", "rev8", "pack", "packh", "brev8", "packw", "zip", "unzip", "clmul", "clmulh", "clmulr", "xperm8", "xperm4", "bclr", "bext", "binv", "bset", "bclri", "bexti", "binvi", "bseti", "bclri", "bexti", "binvi", "bseti", "aes32dsi", "aes32dsmi", "aes64ds", "aes64dsm", "aes64im", "aes64ks1i", "aes64ks2", "aes32esi", "aes32esmi", "aes64es", "aes64esm", "sha256sig0", "sha256sig1", "sha256sum0", "sha256sum1", "sha512sig0l", "sha512sig1l", "sha512sig0h", "sha512sig1h", "sha512sum0r", "sha512sum1r", "sha512sig0", "sha512sig1", "sha512sum0", "sha512sum1", "sm4ed", "sm4ks", "sm3p0", "sm3p1", "custom0.rs1", "custom0.rs1.rs2", "custom0.rd.rs1", "custom0.rd.rs1.rs2", "custom1.rs1", "custom1.rs1.rs2", "custom1.rd.rs1", "custom1.rd.rs1.rs2", "custom2.rs1", "custom2.rs1.rs2", "custom2.rd.rs1", "custom2.rd.rs1.rs2", "custom3.rs1", "custom3.rs1.rs2", "custom3.rd.rs1", "custom3.rd.rs1.rs2").contains(i) => y
      case i if Seq("ecall", "ebreak", "mret", "wfi", "cease", "sret", "dret", "nmret").contains(i) => dc
      case _ => n
    }
  }

  object fenceI extends BoolDecodeField[RocketDecodePattern] {
    override def name: String = "fence_i"

    override def genTable(op: RocketDecodePattern): BitPat = op.instruction.name match {
      // format: off
      case i if Seq("fence.i").contains(i) => y
      case _ => n
      // format: on
    }
  }

  object fence extends BoolDecodeField[RocketDecodePattern] {
    override def name: String = "fence"

    override def genTable(op: RocketDecodePattern): BitPat = op.instruction.name match {
      // format: off
      case i if Seq("fence").contains(i) => y
      case _ => n
      // format: on
    }
  }

  object amo extends BoolDecodeField[RocketDecodePattern] {
    override def name: String = "amo"

    override def genTable(op: RocketDecodePattern): BitPat = op.instruction.instructionSet.name match {
      // format: off
      case s if Seq("rv_a", "rv64_a").contains(s) => y
      case _ => n
      // format: on
    }
  }


  object zbk extends BoolDecodeField[RocketDecodePattern] {
    override def name: String = "zbk"

    override def genTable(op: RocketDecodePattern): BitPat = op.instruction.instructionSet.name match {
      // format: off
      case s if Seq("rv_zbc", "rv_zbkx").contains(s) => y
      case _ => n
      // format: on
    }
  }

  object zkn extends BoolDecodeField[RocketDecodePattern] {
    override def name: String = "zkn"

    override def genTable(op: RocketDecodePattern): BitPat = op.instruction.instructionSet.name match {
      // format: off
      case s if Seq("rv32_zknd", "rv64_zknd", "rv32_zkne", "rv64_zkne", "rv_zknh", "rv32_zknh", "rv64_zknh", "rv_zksed").contains(s) => y
      case _ => n
      // format: on
    }

  }

  object zks extends BoolDecodeField[RocketDecodePattern] {
    override def name: String = "zks"

    override def genTable(op: RocketDecodePattern): BitPat = op.instruction.instructionSet.name match {
      // format: off
      case s if Seq("rv_zksed").contains(s) => y
      case _ => n
      // format: on
    }
  }

  object aluDoubleWords extends BoolDecodeField[RocketDecodePattern] {
    override def name: String = "alu_dw"

    override def genTable(op: RocketDecodePattern): BitPat = {
      op.instruction.name match {
        // format: off
        case i if Seq("bne", "beq", "blt", "bltu", "bge", "bgeu", "jal", "jalr", "auipc", "lb", "lh", "lw", "lbu", "lhu", "sb", "sh", "sw", "lui", "addi", "slti", "sltiu", "andi", "ori", "xori", "add", "sub", "slt", "sltu", "and", "or", "xor", "sll", "srl", "sra", "csrrw", "csrrs", "csrrc", "csrrwi", "csrrsi", "csrrci", "czero.eqz", "czero.nez", "sfence.vma", "hfence.vvma", "hfence.gvma", "hlv.b", "hlv.bu", "hlv.h", "hlv.hu", "hlvx.hu", "hlv.w", "hlvx.wu", "hsv.b", "hsv.h", "hsv.w", "ld", "lwu", "sd", "slli", "srli", "srai", "hlv.d", "hsv.d", "hlv.wu", "mul", "mulh", "mulhu", "mulhsu", "div", "divu", "rem", "remu", "amoadd.w", "amoxor.w", "amoswap.w", "amoand.w", "amoor.w", "amomin.w", "amominu.w", "amomax.w", "amomaxu.w", "lr.w", "sc.w", "amoadd.d", "amoswap.d", "amoxor.d", "amoand.d", "amoor.d", "amomin.d", "amominu.d", "amomax.d", "amomaxu.d", "lr.d", "sc.d", "flh", "fsh", "flw", "fsw", "fld", "fsd", "sh1add", "sh2add", "sh3add", "andn", "orn", "xnor", "ror", "rol", "rori", "clz", "ctz", "cpop", "max", "maxu", "min", "minu", "sext.h", "sext.b", "zext.h", "orc.b", "rev8", "pack", "packh", "brev8", "clmul", "clmulh", "clmulr", "xperm8", "xperm4", "bclr", "bext", "binv", "bset", "bclri", "bexti", "binvi", "bseti", "aes64ds", "aes64dsm", "aes64im", "aes64ks1i", "aes64ks2", "aes64es", "aes64esm", "add.uw", "slli.uw", "sh1add.uw", "sh2add.uw", "sh3add.uw").contains(i) => y
        case i if Seq("addiw", "slliw", "srliw", "sraiw", "addw", "subw", "sllw", "srlw", "sraw", "mulw", "divw", "divuw", "remw", "remuw", "rorw", "rolw", "roriw", "clzw", "ctzw", "cpopw", "packw").contains(i) => n
        case _ => dc
        // format: on
      }
    }
  }

  object mem extends BoolDecodeField[RocketDecodePattern] {
    override def name: String = "mem"

    override def default: BitPat = n

    override def genTable(op: RocketDecodePattern): BitPat = {
      op.instruction.name match {
        // format: off
        case i if Seq("lb", "lh", "lw", "lbu", "lhu", "sb", "sh", "sw", "hlv.b", "hlv.bu", "hlv.h", "hlv.hu", "hlv.w", "hsv.b", "hsv.h", "hsv.w", "ld", "lwu", "sd", "hlv.d", "hsv.d", "hlv.wu", "amoadd.w", "amoxor.w", "amoswap.w", "amoand.w", "amoor.w", "amomin.w", "amominu.w", "amomax.w", "amomaxu.w", "lr.w", "sc.w", "amoadd.d", "amoswap.d", "amoxor.d", "amoand.d", "amoor.d", "amomin.d", "amominu.d", "amomax.d", "amomaxu.d", "lr.d", "sc.d", "flh", "fsh", "flw", "fsw", "fld", "fsd", "sfence.vma").contains(i) => y
        case i if Seq("fence.i").contains(i) && p.fenceIFlushDCache => y
        case _ => n
        // format: on
      }
    }
  }

  object rfs1 extends BoolDecodeField[RocketDecodePattern] {
    override def name: String = "rfs1"

    override def genTable(op: RocketDecodePattern): BitPat = {
      op.instruction.name match {
        // format: off
        case i if Seq("fcvt.s.h", "fcvt.h.s", "fsgnj.h", "fsgnjx.h", "fsgnjn.h", "fmin.h", "fmax.h", "fadd.h", "fsub.h", "fmul.h", "fmadd.h", "fmsub.h", "fnmadd.h", "fnmsub.h", "fclass.h", "fmv.x.h", "fcvt.w.h", "fcvt.wu.h", "feq.h", "flt.h", "fle.h", "fdiv.h", "fsqrt.h", "fsgnj.s", "fsgnjx.s", "fsgnjn.s", "fmin.s", "fmax.s", "fadd.s", "fsub.s", "fmul.s", "fmadd.s", "fmsub.s", "fnmadd.s", "fnmsub.s", "fclass.s", "fmv.x.w", "fcvt.w.s", "fcvt.wu.s", "feq.s", "flt.s", "fle.s", "fdiv.s", "fsqrt.s", "fcvt.s.d", "fcvt.d.s", "fsgnj.d", "fsgnjx.d", "fsgnjn.d", "fmin.d", "fmax.d", "fadd.d", "fsub.d", "fmul.d", "fmadd.d", "fmsub.d", "fnmadd.d", "fnmsub.d", "fclass.d", "fcvt.w.d", "fcvt.wu.d", "feq.d", "flt.d", "fle.d", "fdiv.d", "fsqrt.d", "fcvt.d.h", "fcvt.h.d", "fcvt.l.h", "fcvt.lu.h", "fcvt.l.s", "fcvt.lu.s", "fmv.x.d", "fcvt.l.d", "fcvt.lu.d").contains(i) => y
        case _ => n
        // format: on
      }
    }
  }

  object rfs2 extends BoolDecodeField[RocketDecodePattern] {
    override def name: String = "rfs2"

    override def genTable(op: RocketDecodePattern): BitPat = {
      op.instruction.name match {
        // format: off
        case i if Seq("fsgnj.h", "fsgnjx.h", "fsgnjn.h", "fmin.h", "fmax.h", "fadd.h", "fsub.h", "fmul.h", "fmadd.h", "fmsub.h", "fnmadd.h", "fnmsub.h", "feq.h", "flt.h", "fle.h", "fdiv.h", "fsqrt.h", "fsgnj.s", "fsgnjx.s", "fsgnjn.s", "fmin.s", "fmax.s", "fadd.s", "fsub.s", "fmul.s", "fmadd.s", "fmsub.s", "fnmadd.s", "fnmsub.s", "feq.s", "flt.s", "fle.s", "fdiv.s", "fsqrt.s", "fsgnj.d", "fsgnjx.d", "fsgnjn.d", "fmin.d", "fmax.d", "fadd.d", "fsub.d", "fmul.d", "fmadd.d", "fmsub.d", "fnmadd.d", "fnmsub.d", "feq.d", "flt.d", "fle.d", "fdiv.d", "fsqrt.d").contains(i) => y
        case _ => n
        // format: on
      }
    }
  }

  object rfs3 extends BoolDecodeField[RocketDecodePattern] {
    override def name: String = "rfs3"

    override def genTable(op: RocketDecodePattern): BitPat =
      op.instruction.name match {
        // format: off
        case i if Seq("fmadd.h", "fmsub.h", "fnmadd.h", "fnmsub.h", "fmadd.s", "fmsub.s", "fnmadd.s", "fnmsub.s", "fmadd.d", "fmsub.d", "fnmadd.d", "fnmsub.d").contains(i) => y
        case _ => n
        // format: on
      }
  }

  object wfd extends BoolDecodeField[RocketDecodePattern] {
    override def name: String = "wfd"

    override def genTable(op: RocketDecodePattern): BitPat = op.instruction.name match {
      // format: off
      case i if Seq("fcvt.s.h", "fcvt.h.s", "fsgnj.h", "fsgnjx.h", "fsgnjn.h", "fmin.h", "fmax.h", "fadd.h", "fsub.h", "fmul.h", "fmadd.h", "fmsub.h", "fnmadd.h", "fnmsub.h", "fmv.h.x", "fcvt.h.w", "fcvt.h.wu", "flh", "fdiv.h", "fsqrt.h", "fsgnj.s", "fsgnjx.s", "fsgnjn.s", "fmin.s", "fmax.s", "fadd.s", "fsub.s", "fmul.s", "fmadd.s", "fmsub.s", "fnmadd.s", "fnmsub.s", "fmv.w.x", "fcvt.s.w", "fcvt.s.wu", "flw", "fdiv.s", "fsqrt.s", "fcvt.s.d", "fcvt.d.s", "fsgnj.d", "fsgnjx.d", "fsgnjn.d", "fmin.d", "fmax.d", "fadd.d", "fsub.d", "fmul.d", "fmadd.d", "fmsub.d", "fnmadd.d", "fnmsub.d", "fcvt.d.w", "fcvt.d.wu", "fld", "fdiv.d", "fsqrt.d", "fcvt.d.h", "fcvt.h.d", "fcvt.h.l", "fcvt.h.lu", "fcvt.s.l", "fcvt.s.lu", "fmv.d.x", "fcvt.d.l", "fcvt.d.lu").contains(i) => y
      case _ => n
      // format: on
    }
  }

  object mul extends BoolDecodeField[RocketDecodePattern] {
    override def name: String = "mul"

    override def genTable(op: RocketDecodePattern): BitPat = op.instruction.name match {
      // format: off
      case i if Seq("mul", "mulh", "mulhu", "mulhsu", "mulw").contains(i) => y
      case _ => n
      // format: on
    }
  }

  object div extends BoolDecodeField[RocketDecodePattern] {
    override def name: String = "div"

    override def genTable(op: RocketDecodePattern): BitPat = op.instruction.name match {
      // format: off
      case i if Seq("mul", "mulh", "mulhu", "mulhsu", "mulw").contains(i) && !p.pipelinedMul => y
      case i if Seq("div", "divu", "rem", "remu", "divw", "divuw", "remw", "remuw").contains(i) => y
      case _ => n
      // format: on
    }
  }

  object wxd extends BoolDecodeField[RocketDecodePattern] {
    override def name: String = "wxd"

    override def genTable(op: RocketDecodePattern): BitPat = op.instruction.name match {
      // format: off
      // TODO: filter out rd
      case i if Seq("jal", "jalr", "auipc", "lb", "lh", "lw", "lbu", "lhu", "lui", "addi", "slti", "sltiu", "andi", "ori", "xori", "add", "sub", "slt", "sltu", "and", "or", "xor", "sll", "srl", "sra", "csrrw", "csrrs", "csrrc", "csrrwi", "csrrsi", "csrrci", "czero.eqz", "czero.nez", "hlv.b", "hlv.bu", "hlv.h", "hlv.hu", "hlvx.hu", "hlv.w", "hlvx.wu", "slli", "srli", "srai", "ld", "lwu", "slli", "srli", "srai", "addiw", "slliw", "srliw", "sraiw", "addw", "subw", "sllw", "srlw", "sraw", "hlv.d", "hlv.wu", "mul", "mulh", "mulhu", "mulhsu", "div", "divu", "rem", "remu", "mulw", "divw", "divuw", "remw", "remuw", "amoadd.w", "amoxor.w", "amoswap.w", "amoand.w", "amoor.w", "amomin.w", "amominu.w", "amomax.w", "amomaxu.w", "lr.w", "sc.w", "amoadd.d", "amoswap.d", "amoxor.d", "amoand.d", "amoor.d", "amomin.d", "amominu.d", "amomax.d", "amomaxu.d", "lr.d", "sc.d", "fclass.h", "fmv.x.h", "fcvt.w.h", "fcvt.wu.h", "feq.h", "flt.h", "fle.h", "fclass.s", "fmv.x.w", "fcvt.w.s", "fcvt.wu.s", "feq.s", "flt.s", "fle.s", "fclass.d", "fcvt.w.d", "fcvt.wu.d", "feq.d", "flt.d", "fle.d", "fcvt.l.h", "fcvt.lu.h", "fcvt.l.s", "fcvt.lu.s", "fmv.x.d", "fcvt.l.d", "fcvt.lu.d", "sh1add", "sh2add", "sh3add", "add.uw", "slli.uw", "sh1add.uw", "sh2add.uw", "sh3add.uw", "andn", "orn", "xnor", "ror", "rol", "rori", "rori", "rorw", "rolw", "roriw", "clz", "ctz", "cpop", "clzw", "ctzw", "cpopw", "max", "maxu", "min", "minu", "sext.h", "sext.b", "zext.h", "zext.h", "orc.b", "rev8", "rev8", "pack", "packh", "brev8", "packw", "zip", "unzip", "clmul", "clmulh", "clmulr", "xperm8", "xperm4", "bclr", "bext", "binv", "bset", "bclri", "bexti", "binvi", "bseti", "bclri", "bexti", "binvi", "bseti", "aes32dsi", "aes32dsmi", "aes64ds", "aes64dsm", "aes64im", "aes64ks1i", "aes64ks2", "aes32esi", "aes32esmi", "aes64es", "aes64esm", "sha256sig0", "sha256sig1", "sha256sum0", "sha256sum1", "sha512sig0l", "sha512sig1l", "sha512sig0h", "sha512sig1h", "sha512sum0r", "sha512sum1r", "sha512sig0", "sha512sig1", "sha512sum0", "sha512sum1", "sm4ed", "sm4ks", "sm3p0", "sm3p1", "custom0.rd", "custom0.rd.rs1", "custom0.rd.rs1.rs2", "custom1.rd", "custom1.rd.rs1", "custom1.rd.rs1.rs2", "custom2.rd", "custom2.rd.rs1", "custom2.rd.rs1.rs2", "custom3.rd", "custom3.rd.rs1", "custom3.rd.rs1.rs2").contains(i) => y
      case _ => n
      // format: on
    }
  }

  // UOPs

  object UOPMEM extends UOP {
    def width = 5

    def xrd: BitPat = encode("b00000")

    def xwr: BitPat = encode("b00001")

    def pfr: BitPat = encode("b00010")

    def pfw: BitPat = encode("b00011")

    def xaSwap: BitPat = encode("b00100")

    def flushAll: BitPat = encode("b00101")

    def xlr: BitPat = encode("b00110")

    def xsc: BitPat = encode("b00111")

    def xaAdd: BitPat = encode("b01000")

    def xaXor: BitPat = encode("b01001")

    def xaOr: BitPat = encode("b01010")

    def xaAnd: BitPat = encode("b01011")

    def xaMin: BitPat = encode("b01100")

    def xaMax: BitPat = encode("b01101")

    def xaMinu: BitPat = encode("b01110")

    def xaMaxu: BitPat = encode("b01111")

    // TODO: unused
    def flush: BitPat = encode("b10000")

    // TODO: unused
    def pwr: BitPat = encode("b10001")

    // TODO: unused
    def produce: BitPat = encode("b10010")

    // TODO: unused
    def clean: BitPat = encode("b10011")

    def sfence: BitPat = encode("b10100")

    def hfencev: BitPat = encode("b10101")

    def hfenceg: BitPat = encode("b10110")

    def wok: BitPat = encode("b10111")

    def hlvx: BitPat = encode("b10000")
  }

  object memCommand extends UOPDecodeField[RocketDecodePattern] {
    override def name: String = "mem_cmd"

    override def genTable(op: RocketDecodePattern): BitPat = {
      op.instruction.name match {
        // format: off
        case i if Seq("fld", "flh", "flw", "hlv.b", "hlv.bu", "hlv.d", "hlv.h", "hlv.hu", "hlv.w", "hlv.wu", "lb", "lbu", "ld", "lh", "lhu", "lw", "lwu").contains(i) => UOPMEM.xrd
        case i if Seq("fsd", "fsh", "fsw", "hsv.b", "hsv.d", "hsv.h", "hsv.w", "sb", "sd", "sh", "sw").contains(i) => UOPMEM.xwr
        case i if Seq("amoswap.d", "amoswap.w").contains(i) => UOPMEM.xaSwap
        case i if Seq("fence.i").contains(i) && p.fenceIFlushDCache => UOPMEM.flushAll
        case i if Seq("lr.d", "lr.w").contains(i) => UOPMEM.xlr
        case i if Seq("sc.d", "sc.w").contains(i) => UOPMEM.xsc
        case i if Seq("amoadd.d", "amoadd.w").contains(i) => UOPMEM.xaAdd
        case i if Seq("amoxor.d", "amoxor.w").contains(i) => UOPMEM.xaXor
        case i if Seq("amoor.d", "amoor.w").contains(i) => UOPMEM.xaOr
        case i if Seq("amoand.d", "amoand.w").contains(i) => UOPMEM.xaAnd
        case i if Seq("amomin.d", "amomin.w").contains(i) => UOPMEM.xaMin
        case i if Seq("amomax.d", "amomax.w").contains(i) => UOPMEM.xaMax
        case i if Seq("amominu.d", "amominu.w").contains(i) => UOPMEM.xaMinu
        case i if Seq("amomaxu.d", "amomaxu.w").contains(i) => UOPMEM.xaMaxu
        case i if Seq("sfence.vma").contains(i) => UOPMEM.sfence
        case i if Seq("hfence.vvma").contains(i) => UOPMEM.hfencev
        case i if Seq("hfence.gvma").contains(i) => UOPMEM.hfenceg
        case i if Seq("hlvx.hu", "hlvx.wu").contains(i) => UOPMEM.hlvx
        case _ => UOPMEM.dontCare
        // format: on
      }
    }

    override def uopType: UOPMEM.type = UOPMEM
  }

  object UOPCSR extends UOP {
    def width = 3

    def n: BitPat = encode(0)

    def r: BitPat = encode(2)

    def i: BitPat = encode(4)

    def w: BitPat = encode(5)

    def s: BitPat = encode(6)

    def c: BitPat = encode(7)
  }

  object csr extends UOPDecodeField[RocketDecodePattern] {
    override def name: String = "csr"

    override def genTable(op: RocketDecodePattern): BitPat = op.instruction.name match {
      // format: off
      // TODO: default should be N?
      case i if Seq("bne", "beq", "blt", "bltu", "bge", "bgeu", "jal", "jalr", "auipc", "lb", "lh", "lw", "lbu", "lhu", "sb", "sh", "sw", "lui", "addi", "slti", "sltiu", "andi", "ori", "xori", "add", "sub", "slt", "sltu", "and", "or", "xor", "sll", "srl", "sra", "fence", "fence.i", "czero.eqz", "czero.nez", "slli", "srli", "srai", "ld", "lwu", "sd", "slli", "srli", "srai", "addiw", "slliw", "srliw", "sraiw", "addw", "subw", "sllw", "srlw", "sraw", "mul", "mulh", "mulhu", "mulhsu", "div", "divu", "rem", "remu", "mulw", "divw", "divuw", "remw", "remuw", "amoadd.w", "amoxor.w", "amoswap.w", "amoand.w", "amoor.w", "amomin.w", "amominu.w", "amomax.w", "amomaxu.w", "lr.w", "sc.w", "amoadd.d", "amoswap.d", "amoxor.d", "amoand.d", "amoor.d", "amomin.d", "amominu.d", "amomax.d", "amomaxu.d", "lr.d", "sc.d", "fcvt.s.h", "fcvt.h.s", "fsgnj.h", "fsgnjx.h", "fsgnjn.h", "fmin.h", "fmax.h", "fadd.h", "fsub.h", "fmul.h", "fmadd.h", "fmsub.h", "fnmadd.h", "fnmsub.h", "fclass.h", "fmv.x.h", "fcvt.w.h", "fcvt.wu.h", "feq.h", "flt.h", "fle.h", "fmv.h.x", "fcvt.h.w", "fcvt.h.wu", "flh", "fsh", "fdiv.h", "fsqrt.h", "fsgnj.s", "fsgnjx.s", "fsgnjn.s", "fmin.s", "fmax.s", "fadd.s", "fsub.s", "fmul.s", "fmadd.s", "fmsub.s", "fnmadd.s", "fnmsub.s", "fclass.s", "fmv.x.w", "fcvt.w.s", "fcvt.wu.s", "feq.s", "flt.s", "fle.s", "fmv.w.x", "fcvt.s.w", "fcvt.s.wu", "flw", "fsw", "fdiv.s", "fsqrt.s", "fcvt.s.d", "fcvt.d.s", "fsgnj.d", "fsgnjx.d", "fsgnjn.d", "fmin.d", "fmax.d", "fadd.d", "fsub.d", "fmul.d", "fmadd.d", "fmsub.d", "fnmadd.d", "fnmsub.d", "fclass.d", "fcvt.w.d", "fcvt.wu.d", "feq.d", "flt.d", "fle.d", "fcvt.d.w", "fcvt.d.wu", "fld", "fsd", "fdiv.d", "fsqrt.d", "fcvt.d.h", "fcvt.h.d", "fcvt.l.h", "fcvt.lu.h", "fcvt.h.l", "fcvt.h.lu", "fcvt.l.s", "fcvt.lu.s", "fcvt.s.l", "fcvt.s.lu", "fmv.x.d", "fcvt.l.d", "fcvt.lu.d", "fmv.d.x", "fcvt.d.l", "fcvt.d.lu", "sh1add", "sh2add", "sh3add", "add.uw", "slli.uw", "sh1add.uw", "sh2add.uw", "sh3add.uw", "andn", "orn", "xnor", "ror", "rol", "rori", "rori", "rorw", "rolw", "roriw", "clz", "ctz", "cpop", "clzw", "ctzw", "cpopw", "max", "maxu", "min", "minu", "sext.h", "sext.b", "zext.h", "zext.h", "orc.b", "rev8", "rev8", "pack", "packh", "brev8", "packw", "zip", "unzip", "clmul", "clmulh", "clmulr", "xperm8", "xperm4", "bclr", "bext", "binv", "bset", "bclri", "bexti", "binvi", "bseti", "bclri", "bexti", "binvi", "bseti", "aes32dsi", "aes32dsmi", "aes64ds", "aes64dsm", "aes64im", "aes64ks1i", "aes64ks2", "aes32esi", "aes32esmi", "aes64es", "aes64esm", "sha256sig0", "sha256sig1", "sha256sum0", "sha256sum1", "sha512sig0l", "sha512sig1l", "sha512sig0h", "sha512sig1h", "sha512sum0r", "sha512sum1r", "sha512sig0", "sha512sig1", "sha512sum0", "sha512sum1", "sm4ed", "sm4ks", "sm3p0", "sm3p1", "custom0", "custom0.rs1", "custom0.rs1.rs2", "custom0.rd", "custom0.rd.rs1", "custom0.rd.rs1.rs2", "custom1", "custom1.rs1", "custom1.rs1.rs2", "custom1.rd", "custom1.rd.rs1", "custom1.rd.rs1.rs2", "custom2", "custom2.rs1", "custom2.rs1.rs2", "custom2.rd", "custom2.rd.rs1", "custom2.rd.rs1.rs2", "custom3", "custom3.rs1", "custom3.rs1.rs2", "custom3.rd", "custom3.rd.rs1", "custom3.rd.rs1.rs2").contains(i) => UOPCSR.n
      case i if Seq("cdiscard.d.l1", "cease", "cflush.d.l1", "dret", "ebreak", "ecall", "hfence.gvma", "hfence.vvma", "hlv.b", "hlv.bu", "hlv.d", "hlv.h", "hlv.hu", "hlv.w", "hlv.wu", "hlvx.hu", "hlvx.wu", "hsv.b", "hsv.d", "hsv.h", "hsv.w", "mnret", "mret", "sfence.vma", "sret", "wfi", "cease").contains(i) => UOPCSR.i
      case i if Seq("csrrw", "csrrwi").contains(i) => UOPCSR.w
      case i if Seq("csrrs", "csrrsi").contains(i) => UOPCSR.s
      case i if Seq("csrrc", "csrrci").contains(i) => UOPCSR.c
      case _ => UOPCSR.dontCare
      // format: on
    }

    override def uopType: UOPCSR.type = UOPCSR
  }

  object UOPALU extends UOP {
    def width = 4

    def add: BitPat = encode(0)

    def sl: BitPat = encode(1)

    def seq: BitPat = encode(2)

    def sne: BitPat = encode(3)

    def xor: BitPat = encode(4)

    def sr: BitPat = encode(5)

    def or: BitPat = encode(6)

    def and: BitPat = encode(7)

    def czeqz: BitPat = encode(8)

    def cznez: BitPat = encode(9)

    def sub: BitPat = encode(10)

    def sra: BitPat = encode(11)

    def slt: BitPat = encode(12)

    def sge: BitPat = encode(13)

    def sltu: BitPat = encode(14)

    def sgeu: BitPat = encode(15)

    def div: BitPat = xor

    def divu: BitPat = sr

    def rem: BitPat = or

    def remu: BitPat = and

    def mul: BitPat = add

    def mulh: BitPat = sl

    def mulhsu: BitPat = seq

    def mulhu: BitPat = sne
  }

  object aluFn extends UOPDecodeField[RocketDecodePattern] {
    override def name: String = "alu_fn"

    override def genTable(op: RocketDecodePattern): BitPat = op.instruction.name match {
      // format: off
      case i if Seq("add", "addi", "addiw", "addw", "amoadd.d", "amoadd.w", "amoand.d", "amoand.w", "amomax.d", "amomax.w", "amomaxu.d", "amomaxu.w", "amomin.d", "amomin.w", "amominu.d", "amominu.w", "amoor.d", "amoor.w", "amoswap.d", "amoswap.w", "amoxor.d", "amoxor.w", "auipc", "cdiscard.d.l1", "cflush.d.l1", "csrrc", "csrrci", "csrrs", "csrrsi", "csrrw", "csrrwi", "fld", "flh", "flw", "fsd", "fsh", "fsw", "hfence.gvma", "hfence.vvma", "hlv.b", "hlv.bu", "hlv.d", "hlv.h", "hlv.hu", "hlv.w", "hlv.wu", "hlvx.hu", "hlvx.wu", "hsv.b", "hsv.d", "hsv.h", "hsv.w", "jal", "jalr", "lb", "lbu", "ld", "lh", "lhu", "lr.d", "lr.w", "lui", "lw", "lwu", "sb", "sc.d", "sc.w", "sd", "sfence.vma", "sh", "sw").contains(i) => UOPALU.add
      case i if Seq("and", "andi").contains(i) => UOPALU.and
      case i if Seq("or", "ori").contains(i) => UOPALU.or
      case i if Seq("beq").contains(i) => UOPALU.seq
      case i if Seq("bge").contains(i) => UOPALU.sge
      case i if Seq("bgeu").contains(i) => UOPALU.sgeu
      case i if Seq("sll", "slli", "slli", "slliw", "sllw").contains(i) => UOPALU.sl
      case i if Seq("blt", "slt", "slti").contains(i) => UOPALU.slt
      case i if Seq("bltu", "sltiu", "sltu").contains(i) => UOPALU.sltu
      case i if Seq("bne").contains(i) => UOPALU.sne
      case i if Seq("srl", "srli", "srli", "srliw", "srlw").contains(i) => UOPALU.sr
      case i if Seq("sra", "srai", "srai", "sraiw", "sraw").contains(i) => UOPALU.sra
      case i if Seq("sub", "subw").contains(i) => UOPALU.sub
      case i if Seq("xor", "xori").contains(i) => UOPALU.xor

      // rv_m
      case i if Seq("mul", "mulw").contains(i) => UOPALU.mul
      case i if Seq("mulh").contains(i) => UOPALU.mulh
      case i if Seq("mulhu").contains(i) => UOPALU.mulhu
      case i if Seq("mulhsu").contains(i) => UOPALU.mulhsu
      case i if Seq("div", "divw").contains(i) => UOPALU.div
      case i if Seq("divu", "divuw").contains(i) => UOPALU.divu
      case i if Seq("rem", "remw").contains(i) => UOPALU.rem
      case i if Seq("remu", "remuw").contains(i) => UOPALU.remu

      case i if Seq("czero.eqz").contains(i) => UOPALU.czeqz
      case i if Seq("czero.nez").contains(i) => UOPALU.cznez
      case _ => UOPALU.dontCare
      // format: on
    }

    override def uopType: UOPALU.type = UOPALU
  }

  object UOPABLU extends UOP {
    def width = 39

    def add: BitPat = encode("b000000000000000000001000000000000000001")

    def sl: BitPat = encode("b000000000110000000001000000000000000010")

    def seq: BitPat = encode("b001000000000000000001000000000001000000")

    def sne: BitPat = encode("b001100000000000000001000000000001000000")

    def xor: BitPat = encode("b000000000000000000001000000000000001000")

    def sr: BitPat = encode("b000000000000000000001000000000000000010")

    def or: BitPat = encode("b000000000000000000001000000000000010000")

    def and: BitPat = encode("b000000000000000000001000000000000000100")

    def czeqz: BitPat = encode(8)

    def cznez: BitPat = encode(9)

    def sub: BitPat = encode("b000000000000000110001000000000000000001")

    def sra: BitPat = encode("b000000000000100000001000000000000000010")

    def slt: BitPat = encode("b000000000000000110001000000000001000000")

    def sge: BitPat = encode("b000100000000000110001000000000001000000")

    def sltu: BitPat = encode("b000010000000000110001000000000001000000")

    def sgeu: BitPat = encode("b000110000000000110001000000000001000000")

    def adduw: BitPat = encode("b000000000000010000001000000000000000001")

    def slliuw: BitPat = encode("b000000000110010000001000000000000000010")

    def sh1add: BitPat = encode("b000000000000000000010000000000000000001")

    def sh1adduw: BitPat = encode("b000000000000010000010000000000000000001")

    def sh2add: BitPat = encode("b000000000000000000100000000000000000001")

    def sh2adduw: BitPat = encode("b000000000000010000100000000000000000001")

    def sh3add: BitPat = encode("b000000000000000001000000000000000000001")

    def sh3adduw: BitPat = encode("b000000000000010001000000000000000000001")

    def ror: BitPat = encode("b000000000001000000001000000000000000010")

    def rol: BitPat = encode("b000000000111000000001000000000000000010")

    def andn: BitPat = encode("b000000000000000100001000000000000000100")

    def orn: BitPat = encode("b000000000000000100001000000000000010000")

    def xnor: BitPat = encode("b000000000000000100001000000000000001000")

    def rev8: BitPat = encode("b000000000000000000001000001000000000000")

    def orcb: BitPat = encode("b100000000000000000001000010000000000000")

    def sextb: BitPat = encode("b000000000000000000001000000100000000000")

    def sexth: BitPat = encode("b010000000000000000001000000010000000000")

    def zexth: BitPat = encode("b000000000000000000001000000010000000000")

    def max: BitPat = encode("b000000000000000110001000000000010000000")

    def maxu: BitPat = encode("b000010000000000110001000000000010000000")

    def min: BitPat = encode("b000100000000000110001000000000010000000")

    def minu: BitPat = encode("b000110000000000110001000000000010000000")

    def cpop: BitPat = encode("b000000000000000000001000000000100000000")

    def clz: BitPat = encode("b000001101111000000001000000001000000000")

    def ctz: BitPat = encode("b000001101000000000001000000001000000000")

    def bclr: BitPat = encode("b000001110100001000001000000000000000100")

    def bext: BitPat = encode("b000001000100001000001000000000000100000")

    def binv: BitPat = encode("b000001000100001000001000000000000001000")

    def bset: BitPat = encode("b000001000100001000001000000000000010000")

    def brev8: BitPat = encode("b000000000000000000001000010000000000000")

    def pack: BitPat = encode("b000000000000000000001000100000000000000")

    def packh: BitPat = encode("b000000000000000000001001000000000000000")

    def zip: BitPat = encode("b000000000000000000001010000000000000000")

    def unzip: BitPat = encode("b000000000000000000001100000000000000000")

    def clmul: BitPat = encode("b00001")

    def clmulr: BitPat = encode("b00010")

    def clmulh: BitPat = encode("b00100")

    def xperm8: BitPat = encode("b01000")

    def xperm4: BitPat = encode("b10000")

    def aesds: BitPat = encode("b00100001000000001")

    def aesdsm: BitPat = encode("b00000010000000001")

    def aeses: BitPat = encode("b00110001000000001")

    def aesesm: BitPat = encode("b00010010000000001")

    def aesim: BitPat = encode("b10000010000000001")

    def aesks1: BitPat = encode("b01010100000000001")

    def aesks2: BitPat = encode("b00001000000000001")

    def sha256sig0: BitPat = encode("b00000001000000010")

    def sha256sig1: BitPat = encode("b00000001000000100")

    def sha256sum0: BitPat = encode("b00000001000001000")

    def sha256sum1: BitPat = encode("b00000001000010000")

    def sha512sig0: BitPat = encode("b00000001000100000")

    def sha512sig1: BitPat = encode("b00000001001000000")

    def sha512sum0: BitPat = encode("b00000001010000000")

    def sha512sum1: BitPat = encode("b00000001100000000")

    def sm4ed: BitPat = encode("b0101")

    def sm4ks: BitPat = encode("b0001")

    def sm3p0: BitPat = encode("b1010")

    def sm3p1: BitPat = encode("b0010")

    def div: BitPat = xor

    def divu: BitPat = sr

    def rem: BitPat = or

    def remu: BitPat = and

    def mul: BitPat = add

    def mulh: BitPat = sl

    def mulhsu: BitPat = seq

    def mulhu: BitPat = sne
  }

  object abluFn extends UOPDecodeField[RocketDecodePattern] {
    override def name: String = "alu_fn"

    override def genTable(op: RocketDecodePattern): BitPat = op.instruction.name match {
      // format: off
      case i if Seq("add", "addi", "addiw", "addw", "amoadd.d", "amoadd.w", "amoand.d", "amoand.w", "amomax.d", "amomax.w", "amomaxu.d", "amomaxu.w", "amomin.d", "amomin.w", "amominu.d", "amominu.w", "amoor.d", "amoor.w", "amoswap.d", "amoswap.w", "amoxor.d", "amoxor.w", "auipc", "cdiscard.d.l1", "cflush.d.l1", "csrrc", "csrrci", "csrrs", "csrrsi", "csrrw", "csrrwi", "fld", "flh", "flw", "fsd", "fsh", "fsw", "hfence.gvma", "hfence.vvma", "hlv.b", "hlv.bu", "hlv.d", "hlv.h", "hlv.hu", "hlv.w", "hlv.wu", "hlvx.hu", "hlvx.wu", "hsv.b", "hsv.d", "hsv.h", "hsv.w", "jal", "jalr", "lb", "lbu", "ld", "lh", "lhu", "lr.d", "lr.w", "lui", "lw", "lwu", "sb", "sc.d", "sc.w", "sd", "sfence.vma", "sh", "sw").contains(i) => UOPABLU.add
      case i if Seq("add.uw").contains(i) => UOPABLU.adduw
      case i if Seq("aes32dsi", "aes64ds").contains(i) => UOPABLU.aesds
      case i if Seq("aes32dsmi", "aes64dsm").contains(i) => UOPABLU.aesdsm
      case i if Seq("aes32esi", "aes64es").contains(i) => UOPABLU.aeses
      case i if Seq("aes32esmi", "aes64esm").contains(i) => UOPABLU.aesesm
      case i if Seq("aes64im").contains(i) => UOPABLU.aesim
      case i if Seq("aes64ks1i", "aes64ks2").contains(i) => UOPABLU.aesks1
      case i if Seq("and", "andi").contains(i) => UOPABLU.and
      case i if Seq("andn").contains(i) => UOPABLU.andn
      case i if Seq("bclr", "bclri", "bclri").contains(i) => UOPABLU.bclr
      case i if Seq("bext", "bexti", "bexti").contains(i) => UOPABLU.bext
      case i if Seq("binv", "binvi", "binvi").contains(i) => UOPABLU.binv
      case i if Seq("brev8").contains(i) => UOPABLU.brev8
      case i if Seq("bset", "bseti", "bseti").contains(i) => UOPABLU.bset
      case i if Seq("clmul").contains(i) => UOPABLU.clmul
      case i if Seq("clmulh").contains(i) => UOPABLU.clmulh
      case i if Seq("clmulr").contains(i) => UOPABLU.clmulr
      case i if Seq("clz", "clzw").contains(i) => UOPABLU.clz
      case i if Seq("cpop", "cpopw").contains(i) => UOPABLU.cpop
      case i if Seq("ctz", "ctzw").contains(i) => UOPABLU.ctz
      case i if Seq("czero.eqz", "czero.nez").contains(i) => UOPABLU.czeqz
      case i if Seq("div", "divw").contains(i) => UOPABLU.div
      case i if Seq("divu", "divuw").contains(i) => UOPABLU.divu
      case i if Seq("max").contains(i) => UOPABLU.max
      case i if Seq("maxu").contains(i) => UOPABLU.maxu
      case i if Seq("min").contains(i) => UOPABLU.min
      case i if Seq("minu").contains(i) => UOPABLU.minu
      case i if Seq("mul", "mulw").contains(i) => UOPABLU.mul
      case i if Seq("mulh").contains(i) => UOPABLU.mulh
      case i if Seq("mulhsu").contains(i) => UOPABLU.mulhsu
      case i if Seq("mulhu").contains(i) => UOPABLU.mulhu
      case i if Seq("or", "ori").contains(i) => UOPABLU.or
      case i if Seq("orc.b").contains(i) => UOPABLU.orcb
      case i if Seq("orn").contains(i) => UOPABLU.orn
      case i if Seq("pack", "packw").contains(i) => UOPABLU.pack
      case i if Seq("packh").contains(i) => UOPABLU.packh
      case i if Seq("rem", "remw").contains(i) => UOPABLU.rem
      case i if Seq("remu", "remuw").contains(i) => UOPABLU.remu
      case i if Seq("rev8", "rev8").contains(i) => UOPABLU.rev8
      case i if Seq("rol", "rolw").contains(i) => UOPABLU.rol
      case i if Seq("ror", "rori", "rori", "roriw", "rorw").contains(i) => UOPABLU.ror
      case i if Seq("beq").contains(i) => UOPABLU.seq
      case i if Seq("sext.b").contains(i) => UOPABLU.sextb
      case i if Seq("sext.h").contains(i) => UOPABLU.sexth
      case i if Seq("bge").contains(i) => UOPABLU.sge
      case i if Seq("bgeu").contains(i) => UOPABLU.sgeu
      case i if Seq("sh1add").contains(i) => UOPABLU.sh1add
      case i if Seq("sh1add.uw").contains(i) => UOPABLU.sh1adduw
      case i if Seq("sh2add").contains(i) => UOPABLU.sh2add
      case i if Seq("sh2add.uw").contains(i) => UOPABLU.sh2adduw
      case i if Seq("sh3add").contains(i) => UOPABLU.sh3add
      case i if Seq("sh3add.uw").contains(i) => UOPABLU.sh3adduw
      case i if Seq("sha256sig0").contains(i) => UOPABLU.sha256sig0
      case i if Seq("sha256sig1").contains(i) => UOPABLU.sha256sig1
      case i if Seq("sha256sum0").contains(i) => UOPABLU.sha256sum0
      case i if Seq("sha256sum1").contains(i) => UOPABLU.sha256sum1
      case i if Seq("sha512sig0", "sha512sig0h", "sha512sig0l").contains(i) => UOPABLU.sha512sig0
      case i if Seq("sha512sig1", "sha512sig1h", "sha512sig1l").contains(i) => UOPABLU.sha512sig1
      case i if Seq("sha512sum0", "sha512sum0r").contains(i) => UOPABLU.sha512sum0
      case i if Seq("sha512sum1", "sha512sum1r").contains(i) => UOPABLU.sha512sum1
      case i if Seq("sll", "slli", "slli", "slliw", "sllw").contains(i) => UOPABLU.sl
      case i if Seq("slli.uw").contains(i) => UOPABLU.slliuw
      case i if Seq("blt", "slt", "slti").contains(i) => UOPABLU.slt
      case i if Seq("bltu", "sltiu", "sltu").contains(i) => UOPABLU.sltu
      case i if Seq("sm3p0").contains(i) => UOPABLU.sm3p0
      case i if Seq("sm3p1").contains(i) => UOPABLU.sm3p1
      case i if Seq("sm4ed").contains(i) => UOPABLU.sm4ed
      case i if Seq("sm4ks").contains(i) => UOPABLU.sm4ks
      case i if Seq("bne").contains(i) => UOPABLU.sne
      case i if Seq("srl", "srli", "srli", "srliw", "srlw").contains(i) => UOPABLU.sr
      case i if Seq("sra", "srai", "srai", "sraiw", "sraw").contains(i) => UOPABLU.sra
      case i if Seq("sub", "subw").contains(i) => UOPABLU.sub
      case i if Seq("unzip").contains(i) => UOPABLU.unzip
      case i if Seq("xnor").contains(i) => UOPABLU.xnor
      case i if Seq("xor", "xori").contains(i) => UOPABLU.xor
      case i if Seq("xperm4").contains(i) => UOPABLU.xperm4
      case i if Seq("xperm8").contains(i) => UOPABLU.xperm8
      case i if Seq("zext.h").contains(i) => UOPABLU.zexth
      case i if Seq("zip").contains(i) => UOPABLU.zip
      case _ => UOPABLU.dontCare
      // format: on
    }

    override def uopType: UOPALU.type = UOPALU
  }

  object UOPIMM extends UOP {
    def width = 3

    def s: BitPat = encode(0)

    def sb: BitPat = encode(1)

    def u: BitPat = encode(2)

    def uj: BitPat = encode(3)

    def i: BitPat = encode(4)

    def z: BitPat = encode(5)
  }

  object selImm extends UOPDecodeField[RocketDecodePattern] {
    override def name: String = "sel_imm"

    override def genTable(op: RocketDecodePattern): BitPat = op.instruction.name match {
      // format: off
      case i if Seq("addi", "addiw", "aes64ks1i", "andi", "bclri", "bclri", "bexti", "bexti", "binvi", "binvi", "bseti", "bseti", "fld", "flh", "flw", "hsv.b", "hsv.d", "hsv.h", "hsv.w", "jalr", "lb", "lbu", "ld", "lh", "lhu", "lw", "lwu", "ori", "rori", "rori", "roriw", "slli", "slli", "slli.uw", "slliw", "slti", "sltiu", "srai", "srai", "sraiw", "srli", "srli", "srliw", "xori").contains(i) => UOPIMM.i
      case i if Seq("fsd", "fsh", "fsw", "sb", "sd", "sh", "sw").contains(i) => UOPIMM.s
      case i if Seq("beq", "bge", "bgeu", "blt", "bltu", "bne").contains(i) => UOPIMM.sb
      case i if Seq("auipc", "lui").contains(i) => UOPIMM.u
      case i if Seq("jal").contains(i) => UOPIMM.uj
      case i if Seq("csrrci", "csrrsi", "csrrwi").contains(i) => UOPIMM.z
      case _ => UOPIMM.dontCare
      // format: on
    }

    override def uopType: UOPIMM.type = UOPIMM
  }

  object UOPA1 extends UOP {
    def width = 2

    def zero: BitPat = encode(0)

    def rs1: BitPat = encode(1)

    def pc: BitPat = encode(2)
  }

  object selAlu1 extends UOPDecodeField[RocketDecodePattern] {
    override def name: String = "sel_alu1"

    override def genTable(op: RocketDecodePattern): BitPat = op.instruction.name match {
      // format: off
      case i if Seq("auipc", "jal").contains(i) => UOPA1.pc
      case i if Seq("add", "add.uw", "addi", "addiw", "addw", "aes32dsi", "aes32dsmi", "aes32esi", "aes32esmi", "aes64ds", "aes64dsm", "aes64es", "aes64esm", "aes64im", "aes64ks1i", "aes64ks2", "amoadd.d", "amoadd.w", "amoand.d", "amoand.w", "amomax.d", "amomax.w", "amomaxu.d", "amomaxu.w", "amomin.d", "amomin.w", "amominu.d", "amominu.w", "amoor.d", "amoor.w", "amoswap.d", "amoswap.w", "amoxor.d", "amoxor.w", "and", "andi", "andn", "bclr", "bclri", "bclri", "beq", "bext", "bexti", "bexti", "bge", "bgeu", "binv", "binvi", "binvi", "blt", "bltu", "bne", "brev8", "bset", "bseti", "bseti", "cdiscard.d.l1", "cflush.d.l1", "clmul", "clmulh", "clmulr", "clz", "clzw", "cpop", "cpopw", "csrrc", "csrrs", "csrrw", "ctz", "ctzw", "czero.eqz", "czero.nez", "div", "divu", "divuw", "divw", "fcvt.d.l", "fcvt.d.lu", "fcvt.d.w", "fcvt.d.wu", "fcvt.h.l", "fcvt.h.lu", "fcvt.h.w", "fcvt.h.wu", "fcvt.s.l", "fcvt.s.lu", "fcvt.s.w", "fcvt.s.wu", "fld", "flh", "flw", "fmv.d.x", "fmv.h.x", "fmv.w.x", "fsd", "fsh", "fsw", "hfence.gvma", "hfence.vvma", "hlv.b", "hlv.bu", "hlv.d", "hlv.h", "hlv.hu", "hlv.w", "hlv.wu", "hlvx.hu", "hlvx.wu", "hsv.b", "hsv.d", "hsv.h", "hsv.w", "jalr", "lb", "lbu", "ld", "lh", "lhu", "lr.d", "lr.w", "lw", "lwu", "max", "maxu", "min", "minu", "mul", "mulh", "mulhsu", "mulhu", "mulw", "or", "orc.b", "ori", "orn", "pack", "packh", "packw", "rem", "remu", "remuw", "remw", "rev8", "rev8", "rol", "rolw", "ror", "rori", "rori", "roriw", "rorw", "sb", "sc.d", "sc.w", "sd", "sext.b", "sext.h", "sfence.vma", "sh", "sh1add", "sh1add.uw", "sh2add", "sh2add.uw", "sh3add", "sh3add.uw", "sha256sig0", "sha256sig1", "sha256sum0", "sha256sum1", "sha512sig0", "sha512sig0h", "sha512sig0l", "sha512sig1", "sha512sig1h", "sha512sig1l", "sha512sum0", "sha512sum0r", "sha512sum1", "sha512sum1r", "sll", "slli", "slli.uw", "slliw", "sllw", "slt", "slti", "sltiu", "sltu", "sm3p0", "sm3p1", "sm4ed", "sm4ks", "sra", "srai", "srai", "sraiw", "sraw", "srl", "srli", "srli", "srliw", "srlw", "sub", "subw", "sw", "unzip", "xnor", "xor", "xori", "xperm4", "xperm8", "zext.h", "zext.h", "zip").contains(i) => UOPA1.rs1
      case i if Seq("csrrci", "csrrsi", "csrrwi", "lui").contains(i) => UOPA1.zero
      case _ => UOPA1.dontCare
    }

    override def uopType: UOPA1.type = UOPA1
  }

  object UOPA2 extends UOP {
    def width = 2

    def zero: BitPat = encode(0)

    def size: BitPat = encode(1)

    def rs2: BitPat = encode(2)

    def imm: BitPat = encode(3)
  }

  object selAlu2 extends UOPDecodeField[RocketDecodePattern] {
    override def name: String = "sel_alu2"

    override def genTable(op: RocketDecodePattern): BitPat = op.instruction.name match {
      // format: off
      case i if Seq("addi", "addiw", "aes64ks1i", "andi", "auipc", "bclri", "bclri", "bexti", "bexti", "binvi", "binvi", "bseti", "bseti", "csrrci", "csrrsi", "csrrwi", "fld", "flh", "flw", "fsd", "fsh", "fsw", "jalr", "lb", "lbu", "ld", "lh", "lhu", "lui", "lw", "lwu", "ori", "rori", "rori", "roriw", "sb", "sd", "sh", "slli", "slli.uw", "slliw", "slti", "sltiu", "srai", "srai", "sraiw", "srli", "srli", "srliw", "sw", "xori").contains(i) => UOPA2.imm
      case i if Seq("add", "add.uw", "addw", "aes32dsi", "aes32dsmi", "aes32esi", "aes32esmi", "aes64ds", "aes64dsm", "aes64es", "aes64esm", "aes64im", "aes64ks2", "and", "andn", "bclr", "beq", "bext", "bge", "bgeu", "binv", "blt", "bltu", "bne", "bset", "clmul", "clmulh", "clmulr", "czero.eqz", "czero.nez", "div", "divu", "divuw", "divw", "max", "maxu", "min", "minu", "mul", "mulh", "mulhsu", "mulhu", "mulw", "or", "orn", "pack", "packh", "packw", "rem", "remu", "remuw", "remw", "rol", "rolw", "ror", "rorw", "sh1add", "sh1add.uw", "sh2add", "sh2add.uw", "sh3add", "sh3add.uw", "sha512sig0h", "sha512sig0l", "sha512sig1h", "sha512sig1l", "sha512sum0r", "sha512sum1r", "sll", "sllw", "slt", "sltu", "sm4ed", "sm4ks", "sra", "sraw", "srl", "srlw", "sub", "subw", "xnor", "xor", "xperm4", "xperm8").contains(i) => UOPA2.rs2
      case i if Seq("jal").contains(i) => UOPA2.size
      case i if Seq("amoadd.d", "amoadd.w", "amoand.d", "amoand.w", "amomax.d", "amomax.w", "amomaxu.d", "amomaxu.w", "amomin.d", "amomin.w", "amominu.d", "amominu.w", "amoor.d", "amoor.w", "amoswap.d", "amoswap.w", "amoxor.d", "amoxor.w", "cdiscard.d.l1", "cflush.d.l1", "csrrc", "csrrs", "csrrw", "hfence.gvma", "hfence.vvma", "hlv.b", "hlv.bu", "hlv.d", "hlv.h", "hlv.hu", "hlv.w", "hlv.wu", "hlvx.hu", "hlvx.wu", "hsv.b", "hsv.d", "hsv.h", "hsv.w", "lr.d", "lr.w", "sc.d", "sc.w", "sfence.vma").contains(i) => UOPA2.zero
      case i => UOPA2.dontCare
    }

    override def uopType: UOPA2.type = UOPA2
  }

  // Custom extensions

  /** Rocket Custom Coprocessor, add FU at commit stage */
  object rocc extends BoolDecodeField[RocketDecodePattern] {
    override def name: String = "rocc"

    override def genTable(op: RocketDecodePattern): BitPat = if (op.isRoCC) y else n
  }

  // instructions need to be take care by scalar for vector
  object isVector extends BoolDecodeField[RocketDecodePattern] {
    override def name: String = "isVector"

    override def genTable(op: RocketDecodePattern): BitPat = n
  }

  object vload extends BoolDecodeField[RocketDecodePattern] {
    override def name: String = "isVectorStore"

    override def genTable(op: RocketDecodePattern): BitPat = n
  }

  object vstore extends BoolDecodeField[RocketDecodePattern] {
    override def name: String = "isVectorLoad"

    override def genTable(op: RocketDecodePattern): BitPat = n
  }

  object vcsr extends BoolDecodeField[RocketDecodePattern] {
    override def name: String = "isVectorCSR"

    override def genTable(op: RocketDecodePattern): BitPat = n
  }
}
