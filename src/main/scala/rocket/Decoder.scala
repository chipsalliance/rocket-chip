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

  private def usingConditionalZero = hasAnySetIn("rv_zicond")

  private val useFPU = !fLen0
  private val useRoCC = hasAnySetIn("rv_rocc")
  private val useMulDiv = hasAnySetIn("rv_m", "rv64_m")

  private val instructionDecodePatterns: Seq[RocketDecodePattern] = instructions.map(RocketDecodePattern.apply)
  private val instructionDecodeFields: Seq[DecodeField[RocketDecodePattern, _ <: Data]] = Seq(
    isLegal, isBranch, isJal, isJalr, rxs2, rxs1, selAlu2, selAlu1, selImm, aluDoubleWords, mem, memCommand, wxd, csr, fenceI, fence, amo, aluFn
  ) ++
    (if (useFPU) Seq(fp, rfs1, rfs2, rfs3, wfd, dp) else None) ++
    (if (useMulDiv) if (p.pipelinedMul) Seq(mul, div) else Seq(div) else None) ++
    (if (useRoCC) Some(rocc) else None)

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
      case i if Seq("amomaxu.w", "amoand.w", "amoor.w", "amoxor.w", "amoswap.w", "lr.w", "amomax.w", "amoadd.w", "amomin.w", "amominu.w", "sc.w", "lr.d", "amomax.d", "amoswap.d", "amoxor.d", "amoand.d", "amomin.d", "amoor.d", "amoadd.d", "amomaxu.d", "amominu.d", "sc.d", "hsv.w", "hsv.b", "hfence.vvma", "hsv.h", "hfence.gvma", "hsv.d", "or", "srl", "sltu", "sra", "sb", "add", "xor", "beq", "bge", "sw", "blt", "bgeu", "bltu", "bne", "sub", "and", "slt", "sh", "sll", "addw", "sd", "sllw", "sraw", "subw", "srlw", "mulhsu", "rem", "div", "mul", "mulhu", "mulh", "remu", "divu", "remuw", "divw", "divuw", "mulw", "remw", "sfence.vma", "czero.nez", "czero.eqz", "custom0.rs1.rs2", "custom0.rd.rs1.rs2", "custom1.rs1.rs2", "custom1.rd.rs1.rs2", "custom2.rs1.rs2", "custom2.rd.rs1.rs2", "custom3.rs1.rs2", "custom3.rd.rs1.rs2").contains(i) => y
      case _ => n
    }
  }

  object rxs1 extends BoolDecodeField[RocketDecodePattern] {
    override def name: String = "rxs1"

    override def genTable(op: RocketDecodePattern): BitPat = op.instruction.name match {
      case i if Seq("amomaxu.w", "amoand.w", "amoor.w", "amoxor.w", "amoswap.w", "lr.w", "amomax.w", "amoadd.w", "amomin.w", "amominu.w", "sc.w", "lr.d", "amomax.d", "amoswap.d", "amoxor.d", "amoand.d", "amomin.d", "amoor.d", "amoadd.d", "amomaxu.d", "amominu.d", "sc.d", "fld", "fcvt.d.wu", "fsd", "fcvt.d.w", "fcvt.d.lu", "fmv.d.x", "fcvt.d.l", "fcvt.s.wu", "fmv.w.x", "fsw", "fcvt.s.w", "flw", "fcvt.s.lu", "fcvt.s.l", "hsv.w", "hsv.b", "hfence.vvma", "hlv.hu", "hlvx.hu", "hlv.b", "hlvx.wu", "hlv.w", "hsv.h", "hlv.h", "hlv.bu", "hfence.gvma", "hsv.d", "hlv.d", "hlv.wu", "or", "srl", "ori", "lhu", "sltu", "sra", "sb", "lw", "add", "xor", "beq", "andi", "bge", "sw", "blt", "bgeu", "sltiu", "lh", "bltu", "jalr", "bne", "lbu", "sub", "and", "xori", "slti", "slt", "addi", "lb", "sh", "sll", "srli", "srai", "slli", "ld", "addw", "sd", "sraiw", "lwu", "sllw", "sraw", "subw", "srlw", "addiw", "srliw", "slliw", "mulhsu", "rem", "div", "mul", "mulhu", "mulh", "remu", "divu", "remuw", "divw", "divuw", "mulw", "remw", "sfence.vma", "fsh", "flh", "fcvt.h.wu", "fcvt.h.w", "fmv.h.x", "fcvt.h.lu", "fcvt.h.l", "csrrc", "csrrs", "csrrw", "czero.nez", "czero.eqz", 
      "cflush.d.l1", "cdiscard.d.l1", "custom0.rs1", "custom0.rs1.rs2", "custom0.rd.rs1", "custom0.rd.rs1.rs2", "custom1.rs1", "custom1.rs1.rs2", "custom1.rd.rs1", "custom1.rd.rs1.rs2", "custom2.rs1", "custom2.rs1.rs2", "custom2.rd.rs1", "custom2.rd.rs1.rs2", "custom3.rs1", "custom3.rs1.rs2", "custom3.rd.rs1", "custom3.rd.rs1.rs2").contains(i) => y
      case i if Seq("ecall", "ebreak", "mret", "wfi", "sret", "dret", "cease", "nmret").contains(i) => dc
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
        case i if Seq("amomaxu.w", "amoand.w", "amoor.w", "amoxor.w", "amoswap.w", "lr.w", "amomax.w", "amoadd.w", "amomin.w", "amominu.w", "sc.w", "lr.d", "amomax.d", "amoswap.d", "amoxor.d", "amoand.d", "amomin.d", "amoor.d", "amoadd.d", "amomaxu.d", "amominu.d", "sc.d", "fld", "fsd", "fsw", "flw", "hsv.w", "hsv.b", "hfence.vvma", "hlv.hu", "hlvx.hu", "hlv.b", "hlvx.wu", "hlv.w", "hsv.h", "hlv.h", "hlv.bu", "hfence.gvma", "hsv.d", "hlv.d", "hlv.wu", "or", "srl", "ori", "lhu", "sltu", "sra", "sb", "lw", "add", "xor", "beq", "andi", "bge", "sw", "blt", "bgeu", "sltiu", "lh", "bltu", "jalr", "lui", "bne", "lbu", "sub", "and", "auipc", "xori", "slti", "slt", "addi", "lb", "jal", "sh", "sll", "srli", "srai", "slli", "ld", "sd", "lwu", "mulhsu", "rem", "div", "mul", "mulhu", "mulh", "remu", "divu", "sfence.vma", "fsh", "flh", "csrrc", "csrrci", "csrrs", "csrrw", "csrrsi", "csrrwi", "czero.nez", "czero.eqz").contains(i) => y
        case i if Seq("addw", "sraiw", "sllw", "sraw", "subw", "srlw", "addiw", "srliw", "slliw", "remuw", "divw", "divuw", "mulw", "remw").contains(i) => n
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
        case i if Seq("amomaxu.w", "amoand.w", "amoor.w", "amoxor.w", "amoswap.w", "lr.w", "amomax.w", "amoadd.w", "amomin.w", "amominu.w", "sc.w", "lr.d", "amomax.d", "amoswap.d", "amoxor.d", "amoand.d", "amomin.d", "amoor.d", "amoadd.d", "amomaxu.d", "amominu.d", "sc.d", "fld", "fsd", "fsw", "flw", "hsv.w", "hsv.b", "hlv.hu", "hlv.b", "hlv.w", "hsv.h", "hlv.h", "hlv.bu", "hsv.d", "hlv.d", "hlv.wu", "lhu", "sb", "lw", "sw", "lh", "lbu", "lb", "sh", "ld", "sd", "lwu", "sfence.vma", "fsh", "flh").contains(i) => y
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
        case i if Seq("fmin.d", "fsgnj.d", "fle.d", "fnmsub.d", "fadd.d", "fcvt.w.d", "fmsub.d", "fmul.d", "fcvt.wu.d", "feq.d", "fmax.d", "fnmadd.d", "fcvt.d.s", "fcvt.s.d", "fmadd.d", "fsgnjx.d", "flt.d", "fsgnjn.d", "fsub.d", "fsqrt.d", "fclass.d", "fdiv.d", "fmv.x.d", "fcvt.lu.d", "fcvt.l.d", "fcvt.d.h", "fcvt.h.d", "fnmsub.s", "fsgnjx.s", "fmsub.s", "fsgnjn.s", "fdiv.s", "fmin.s", "fsqrt.s", "fclass.s", "fcvt.wu.s", "fmax.s", "feq.s", "fle.s", "fmadd.s", "fsgnj.s", "fadd.s", "flt.s", "fmv.x.w", "fnmadd.s", "fmul.s", "fcvt.w.s", "fsub.s", "fcvt.lu.s", "fcvt.l.s", "feq.h", "fsgnjx.h", "fcvt.w.h", "fcvt.h.s", "fdiv.h", "fclass.h", "fsgnj.h", "fmul.h", "fsub.h", "fcvt.wu.h", "fadd.h", "fmax.h", "fsgnjn.h", "fmv.x.h", "fcvt.s.h", "fmsub.h", "fmin.h", "fsqrt.h", "flt.h", "fnmadd.h", "fmadd.h", "fnmsub.h", "fle.h", "fcvt.l.h", "fcvt.lu.h").contains(i) => y
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
        case i if Seq("fmin.d", "fsgnj.d", "fle.d", "fnmsub.d", "fadd.d", "fmsub.d", "fmul.d", "feq.d", "fmax.d", "fnmadd.d", "fmadd.d", "fsgnjx.d", "flt.d", "fsgnjn.d", "fsub.d", "fsqrt.d", "fdiv.d", "fnmsub.s", "fsgnjx.s", "fmsub.s", "fsgnjn.s", "fdiv.s", "fmin.s", "fsqrt.s", "fmax.s", "feq.s", "fle.s", "fmadd.s", "fsgnj.s", "fadd.s", "flt.s", "fnmadd.s", "fmul.s", "fsub.s", "feq.h", "fsgnjx.h", "fdiv.h", "fsgnj.h", "fmul.h", "fsub.h", "fadd.h", "fmax.h", "fsgnjn.h", "fmsub.h", "fmin.h", "fsqrt.h", "flt.h", "fnmadd.h", "fmadd.h", "fnmsub.h", "fle.h").contains(i) => y
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
        case i if Seq("fnmsub.d", "fmsub.d", "fnmadd.d", "fmadd.d", "fnmsub.s", "fmsub.s", "fmadd.s", "fnmadd.s", "fmsub.h", "fnmadd.h", "fmadd.h", "fnmsub.h").contains(i) => y
        case _ => n
        // format: on
      }
  }

  object wfd extends BoolDecodeField[RocketDecodePattern] {
    override def name: String = "wfd"

    override def genTable(op: RocketDecodePattern): BitPat = op.instruction.name match {
      // format: off
      case i if Seq("fmin.d", "fsgnj.d", "fnmsub.d", "fadd.d", "fmsub.d", "fld", "fmul.d", "fmax.d", "fcvt.d.wu", "fnmadd.d", "fcvt.d.s", "fcvt.s.d", "fmadd.d", "fsgnjx.d", "fsgnjn.d", "fsub.d", "fsqrt.d", "fcvt.d.w", "fdiv.d", "fcvt.d.lu", "fmv.d.x", "fcvt.d.l", "fcvt.d.h", "fcvt.h.d", "fnmsub.s", "fsgnjx.s", "fmsub.s", "fsgnjn.s", "fdiv.s", "fmin.s", "fsqrt.s", "fmax.s", "fcvt.s.wu", "fmv.w.x", "fmadd.s", "fsgnj.s", "fadd.s", "fnmadd.s", "fcvt.s.w", "flw", "fmul.s", "fsub.s", "fcvt.s.lu", "fcvt.s.l", "fsgnjx.h", "fcvt.h.s", "fdiv.h", "fsgnj.h", "fmul.h", "fsub.h", "flh", "fadd.h", "fmax.h", "fsgnjn.h", "fcvt.s.h", "fcvt.h.wu", "fcvt.h.w", "fmsub.h", "fmin.h", "fsqrt.h", "fnmadd.h", "fmadd.h", "fnmsub.h", "fmv.h.x", "fcvt.h.lu", "fcvt.h.l").contains(i) => y
      case _ => n
      // format: on
    }
  }

  object mul extends BoolDecodeField[RocketDecodePattern] {
    override def name: String = "mul"

    override def genTable(op: RocketDecodePattern): BitPat = op.instruction.name match {
      // format: off
      case i if Seq("mulhsu", "mul", "mulhu", "mulh", "mulw").contains(i) => y
      case _ => n
      // format: on
    }
  }

  object div extends BoolDecodeField[RocketDecodePattern] {
    override def name: String = "div"

    override def genTable(op: RocketDecodePattern): BitPat = op.instruction.name match {
      // format: off
      case i if Seq("mulhsu", "mul", "mulhu", "mulh", "mulw").contains(i) && !p.pipelinedMul => y
      case i if Seq("rem", "div", "remu", "divu", "remuw", "divw", "divuw", "remw").contains(i) => y
      case _ => n
      // format: on
    }
  }

  object wxd extends BoolDecodeField[RocketDecodePattern] {
    override def name: String = "wxd"

    override def genTable(op: RocketDecodePattern): BitPat = op.instruction.name match {
      // format: off
      // TODO: filter out rd
      case i if Seq("amomaxu.w", "amoand.w", "amoor.w", "amoxor.w", "amoswap.w", "lr.w", "amomax.w", "amoadd.w", "amomin.w", "amominu.w", "sc.w", "lr.d", "amomax.d", "amoswap.d", "amoxor.d", "amoand.d", "amomin.d", "amoor.d", "amoadd.d", "amomaxu.d", "amominu.d", "sc.d", "fle.d", "fcvt.w.d", "fcvt.wu.d", "feq.d", "flt.d", "fclass.d", "fmv.x.d", "fcvt.lu.d", "fcvt.l.d", "fclass.s", "fcvt.wu.s", "feq.s", "fle.s", "flt.s", "fmv.x.w", "fcvt.w.s", "fcvt.lu.s", "fcvt.l.s", "hlv.hu", "hlvx.hu", "hlv.b", "hlvx.wu", "hlv.w", "hlv.h", "hlv.bu", "hlv.d", "hlv.wu", "or", "srl", "ori", "lhu", "sltu", "sra", "lw", "add", "xor", "andi", "sltiu", "lh", "jalr", "lui", "lbu", "sub", "and", "auipc", "xori", "slti", "slt", "addi", "lb", "jal", "sll", "srli", "srai", "slli", "ld", "addw", "sraiw", "lwu", "sllw", "sraw", "subw", "srlw", "addiw", "srliw", "slliw", "mulhsu", "rem", "div", "mul", "mulhu", "mulh", "remu", "divu", "remuw", "divw", "divuw", "mulw", "remw", "feq.h", "fcvt.w.h", "fclass.h", "fcvt.wu.h", "fmv.x.h", "flt.h", "fle.h", "fcvt.l.h", "fcvt.lu.h", "csrrc", "csrrci", "csrrs", "csrrw", "csrrsi", "csrrwi", "czero.nez", "czero.eqz", "custom0.rd", "custom0.rd.rs1", "custom0.rd.rs1.rs2", "custom1.rd", "custom1.rd.rs1", "custom1.rd.rs1.rs2", "custom2.rd", "custom2.rd.rs1", "custom2.rd.rs1.rs2", "custom3.rd", "custom3.rd.rs1", "custom3.rd.rs1.rs2").contains(i) => y
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
      case i if Seq("amomaxu.w", "amoand.w", "amoor.w", "amoxor.w", "amoswap.w", "lr.w", "amomax.w", "amoadd.w", "amomin.w", "amominu.w", "sc.w", "lr.d", "amomax.d", "amoswap.d", "amoxor.d", "amoand.d", "amomin.d", "amoor.d", "amoadd.d", "amomaxu.d", "amominu.d", "sc.d", "fmin.d", "fsgnj.d", "fle.d", "fnmsub.d", "fadd.d", "fcvt.w.d", "fmsub.d", "fld", "fmul.d", "fcvt.wu.d", "feq.d", "fmax.d", "fcvt.d.wu", "fnmadd.d", "fcvt.d.s", "fcvt.s.d", "fsd", "fmadd.d", "fsgnjx.d", "flt.d", "fsgnjn.d", "fsub.d", "fsqrt.d", "fclass.d", "fcvt.d.w", "fdiv.d", "fcvt.d.lu", "fmv.x.d", "fmv.d.x", "fcvt.lu.d", "fcvt.l.d", "fcvt.d.l", "fcvt.d.h", "fcvt.h.d", "fnmsub.s", "fsgnjx.s", "fmsub.s", "fsgnjn.s", "fdiv.s", "fmin.s", "fsqrt.s", "fclass.s", "fcvt.wu.s", "fmax.s", "feq.s", "fcvt.s.wu", "fmv.w.x", "fle.s", "fmadd.s", "fsgnj.s", "fadd.s", "fsw", "flt.s", "fmv.x.w", "fnmadd.s", "fcvt.s.w", "flw", "fmul.s", "fcvt.w.s", "fsub.s", "fcvt.lu.s", "fcvt.s.lu", "fcvt.l.s", "fcvt.s.l", "or", "srl", "fence", "ori", "lhu", "sltu", "sra", "sb", "lw", "add", "xor", "beq", "andi", "bge", "sw", "blt", "bgeu", "sltiu", "lh", "bltu", "jalr", "lui", "bne", "lbu", "sub", "and", "auipc", "xori", "slti", "slt", "addi", "lb", "jal", "sh", "sll", "srli", "srai", "slli", "ld", "addw", "sd", "sraiw", "lwu", "sllw", "sraw", "subw", "srlw", "addiw", "srliw", "slliw", "mulhsu", "rem", "div", "mul", "mulhu", "mulh", "remu", "divu", "remuw", "divw", "divuw", "mulw", "remw", "feq.h", "fsgnjx.h", "fcvt.w.h", "fcvt.h.s", "fdiv.h", "fclass.h", "fsh", "fsgnj.h", "fmul.h", "fsub.h", "flh", "fcvt.wu.h", "fadd.h", "fmax.h", "fsgnjn.h", "fmv.x.h", "fcvt.s.h", "fcvt.h.wu", "fcvt.h.w", "fmsub.h", "fmin.h", "fsqrt.h", "flt.h", "fnmadd.h", "fmadd.h", "fnmsub.h", "fmv.h.x", "fle.h", "fcvt.l.h", "fcvt.lu.h", "fcvt.h.lu", "fcvt.h.l", "fence.i", "czero.nez", "czero.eqz", "custom0", "custom0.rs1", "custom0.rs1.rs2", "custom0.rd", "custom0.rd.rs1", "custom0.rd.rs1.rs2", "custom1", "custom1.rs1", "custom1.rs1.rs2", "custom1.rd", "custom1.rd.rs1", "custom1.rd.rs1.rs2", "custom2", "custom2.rs1", "custom2.rs1.rs2", "custom2.rd", "custom2.rd.rs1", "custom2.rd.rs1.rs2", "custom3", "custom3.rs1", "custom3.rs1.rs2", "custom3.rd", "custom3.rd.rs1", "custom3.rd.rs1.rs2").contains(i) => UOPCSR.n
      case i if Seq("cdiscard.d.l1", "cease", "cflush.d.l1", "hsv.w", "hsv.b", "hfence.vvma", "hlv.hu", "hlvx.hu", "hlv.b", "hlvx.wu", "hlv.w", "hsv.h", "hlv.h", "hlv.bu", "hfence.gvma", "hsv.d", "hlv.d", "hlv.wu", "ebreak", "ecall", "sret", "sfence.vma", "dret", "wfi", "mret", "mnret").contains(i) => UOPCSR.i
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
      case i if Seq("amomaxu.w", "amoand.w", "amoor.w", "amoxor.w", "amoswap.w", "lr.w", "amomax.w", "amoadd.w", "amomin.w", "amominu.w", "sc.w", "lr.d", "amomax.d", "amoswap.d", "amoxor.d", "amoand.d", "amomin.d", "amoor.d", "amoadd.d", "amomaxu.d", "amominu.d", "sc.d", "fld", "fsd", "fsw", "flw", "hsv.w", "hsv.b", "hfence.vvma", "hlv.hu", "hlvx.hu", "hlv.b", "hlvx.wu", "hlv.w", "hsv.h", "hlv.h", "hlv.bu", "hfence.gvma", "hsv.d", "hlv.d", "hlv.wu", "lhu", "sb", "lw", "add", "sw", "lh", "jalr", "lui", "lbu", "auipc", "addi", "lb", "jal", "sh", "ld", "addw", "sd", "lwu", "addiw", "sfence.vma", "fsh", "flh", "csrrc", "csrrci", "csrrs", "csrrw", "csrrsi", "csrrwi", "cdiscard.d.l1", "cflush.d.l1").contains(i) => UOPALU.add
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
      case i if Seq("fld", "flw", "hsv.w", "hsv.b", "hsv.h", "hsv.d", "ori", "lhu", "lw", "andi", "sltiu", "lh", "jalr", "lbu", "xori", "slti", "addi", "lb", "srli", "srai", "slli", "ld", "sraiw", "lwu", "addiw", "srliw", "slliw", "flh").contains(i) => UOPIMM.i
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
      case i if Seq("amomaxu.w", "amoand.w", "amoor.w", "amoxor.w", "amoswap.w", "lr.w", "amomax.w", "amoadd.w", "amomin.w", "amominu.w", "sc.w", "lr.d", "amomax.d", "amoswap.d", "amoxor.d", "amoand.d", "amomin.d", "amoor.d", "amoadd.d", "amomaxu.d", "amominu.d", "sc.d", "fld", "fcvt.d.wu", "fsd", "fcvt.d.w", "fcvt.d.lu", "fmv.d.x", "fcvt.d.l", "fcvt.s.wu", "fmv.w.x", "fsw", "fcvt.s.w", "flw", "fcvt.s.lu", "fcvt.s.l", "hsv.w", "hsv.b", "hfence.vvma", "hlv.hu", "hlvx.hu", "hlv.b", "hlvx.wu", "hlv.w", "hsv.h", "hlv.h", "hlv.bu", "hfence.gvma", "hsv.d", "hlv.d", "hlv.wu", "or", "srl", "ori", "lhu", "sltu", "sra", "sb", "lw", "add", "xor", "beq", "andi", "bge", "sw", "blt", "bgeu", "sltiu", "lh", "bltu", "jalr", "bne", "lbu", "sub", "and", "xori", "slti", "slt", "addi", "lb", "sh", "sll", "srli", "srai", "slli", "ld", "addw", "sd", "sraiw", "lwu", "sllw", "sraw", "subw", "srlw", "addiw", "srliw", "slliw", "mulhsu", "rem", "div", "mul", "mulhu", "mulh", "remu", "divu", "remuw", "divw", "divuw", "mulw", "remw", "sfence.vma", "fsh", "flh", "fcvt.h.wu", "fcvt.h.w", "fmv.h.x", "fcvt.h.lu", "fcvt.h.l", "csrrc", "csrrs", "csrrw", "czero.nez", "czero.eqz", 
      "cdiscard.d.l1", "cflush.d.l1").contains(i) => UOPA1.rs1
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
      case i if Seq("fld", "fsd", "fsw", "flw", "ori", "lhu", "sb", "lw", "andi", "sw", "sltiu", "lh", "jalr", "lui", "lbu", "auipc", "xori", "slti", "addi", "lb", "sh", "srli", "srai", "slli", "ld", "sd", "sraiw", "lwu", "addiw", "srliw", "slliw", "fsh", "flh", "csrrci", "csrrsi", "csrrwi").contains(i) => UOPA2.imm
      case i if Seq("or", "srl", "sltu", "sra", "add", "xor", "beq", "bge", "blt", "bgeu", "bltu", "bne", "sub", "and", "slt", "sll", "addw", "sllw", "sraw", "subw", "srlw", "mulhsu", "rem", "div", "mul", "mulhu", "mulh", "remu", "divu", "remuw", "divw", "divuw", "mulw", "remw", "czero.nez", "czero.eqz").contains(i) => UOPA2.rs2
      case i if Seq("jal").contains(i) => UOPA2.size
      case i if Seq("amomaxu.w", "amoand.w", "amoor.w", "amoxor.w", "amoswap.w", "lr.w", "amomax.w", "amoadd.w", "amomin.w", "amominu.w", "sc.w", "lr.d", "amomax.d", "amoswap.d", "amoxor.d", "amoand.d", "amomin.d", "amoor.d", "amoadd.d", "amomaxu.d", "amominu.d", "sc.d", "hsv.w", "hsv.b", "hfence.vvma", "hlv.hu", "hlvx.hu", "hlv.b", "hlvx.wu", "hlv.w", "hsv.h", "hlv.h", "hlv.bu", "hfence.gvma", "hsv.d", "hlv.d", "hlv.wu", "sfence.vma", "csrrc", "csrrs", "csrrw", "cdiscard.d.l1", "cflush.d.l1").contains(i) => UOPA2.zero
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
}
