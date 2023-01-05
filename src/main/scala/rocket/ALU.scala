// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.rocket

import chisel3._
import chisel3.util.{BitPat, Fill, Cat, Reverse}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.CoreModule

trait ALUFN {
  val SZ_ALU_FN = 4
  def FN_X    = BitPat("b????")
  def FN_ADD  = 0.U
  def FN_SL   = 1.U
  def FN_SEQ  = 2.U
  def FN_SNE  = 3.U
  def FN_XOR  = 4.U
  def FN_SR   = 5.U
  def FN_OR   = 6.U
  def FN_AND  = 7.U
  def FN_SUB  = 10.U
  def FN_SRA  = 11.U
  def FN_SLT  = 12.U
  def FN_SGE  = 13.U
  def FN_SLTU = 14.U
  def FN_SGEU = 15.U

  // from Zb
  // Zba: UW is encoded here becuase it is DW_64
  def FN_ADDUW    = UInt(0)
  def FN_SLLIUW   = UInt(0)
  def FN_SH1ADD   = UInt(0)
  def FN_SH1ADDUW = UInt(0)
  def FN_SH2ADD   = UInt(0)
  def FN_SH2ADDUW = UInt(0)
  def FN_SH3ADD   = UInt(0)
  def FN_SH3ADDUW = UInt(0)
  // Zbb
  def FN_ROR      = UInt(0)
  def FN_ROL      = UInt(0)
  def FN_ANDN     = UInt(0)
  def FN_ORN      = UInt(0)
  def FN_XNOR     = UInt(0)
  def FN_REV8     = UInt(0)
  def FN_ORCB     = UInt(0)
  def FN_SEXTB    = UInt(0)
  def FN_SEXTH    = UInt(0)
  def FN_ZEXTH    = UInt(0)
  def FN_MAX      = UInt(0)
  def FN_MAXU     = UInt(0)
  def FN_MIN      = UInt(0)
  def FN_MINU     = UInt(0)
  def FN_CPOP     = UInt(0)
  def FN_CLZ      = UInt(0)
  def FN_CTZ      = UInt(0)
  // Zbs
  def FN_BCLR     = UInt(0)
  def FN_BEXT     = UInt(0)
  def FN_BINV     = UInt(0)
  def FN_BSET     = UInt(0)
  // Zbk
  def FN_BREV8    = UInt(0)
  def FN_PACK     = UInt(0)
  def FN_PACKH    = UInt(0)
  def FN_ZIP      = UInt(0)
  def FN_UNZIP    = UInt(0)

  def FN_DIV  = FN_XOR
  def FN_DIVU = FN_SR
  def FN_REM  = FN_OR
  def FN_REMU = FN_AND

  def FN_MUL    = FN_ADD
  def FN_MULH   = FN_SL
  def FN_MULHSU = FN_SEQ
  def FN_MULHU  = FN_SNE
}

object ALU extends ALUFN
{
  def isMulFN(fn: UInt, cmp: UInt) = fn(1,0) === cmp(1,0)
  def isSub(cmd: UInt) = cmd(3)
  def isCmp(cmd: UInt) = cmd >= FN_SLT
  def cmpUnsigned(cmd: UInt) = cmd(1)
  def cmpInverted(cmd: UInt) = cmd(0)
  def cmpEq(cmd: UInt) = !cmd(3)
}

import ALU._

trait HasALUIO extends HasRocketCoreParameters {
  val io = new Bundle {
    val dw = Bits(INPUT, SZ_DW)
    val fn = Bits(INPUT, if (usingABLU) ABLU.SZ_ALU_FN else SZ_ALU_FN)
    val in2 = UInt(INPUT, xLen)
    val in1 = UInt(INPUT, xLen)
    val out = UInt(OUTPUT, xLen)
    val adder_out = UInt(OUTPUT, xLen)
    val cmp_out = Bool(OUTPUT)
  }
}

class ALU(implicit p: Parameters) extends CoreModule()(p) with HasALUIO {
  // ADD, SUB
  val in2_inv = Mux(isSub(io.fn), ~io.in2, io.in2)
  val in1_xor_in2 = io.in1 ^ in2_inv
  io.adder_out := io.in1 + in2_inv + isSub(io.fn)

  // SLT, SLTU
  val slt =
    Mux(io.in1(xLen-1) === io.in2(xLen-1), io.adder_out(xLen-1),
    Mux(cmpUnsigned(io.fn), io.in2(xLen-1), io.in1(xLen-1)))
  io.cmp_out := cmpInverted(io.fn) ^ Mux(cmpEq(io.fn), in1_xor_in2 === 0.U, slt)

  // SLL, SRL, SRA
  val (shamt, shin_r) =
    if (xLen == 32) (io.in2(4,0), io.in1)
    else {
      require(xLen == 64)
      val shin_hi_32 = Fill(32, isSub(io.fn) && io.in1(31))
      val shin_hi = Mux(io.dw === DW_64, io.in1(63,32), shin_hi_32)
      val shamt = Cat(io.in2(5) & (io.dw === DW_64), io.in2(4,0))
      (shamt, Cat(shin_hi, io.in1(31,0)))
    }
  val shin = Mux(io.fn === FN_SR  || io.fn === FN_SRA, shin_r, Reverse(shin_r))
  val shout_r = (Cat(isSub(io.fn) & shin(xLen-1), shin).asSInt >> shamt)(xLen-1,0)
  val shout_l = Reverse(shout_r)
  val shout = Mux(io.fn === FN_SR || io.fn === FN_SRA, shout_r, 0.U) |
              Mux(io.fn === FN_SL,                     shout_l, 0.U)

  // AND, OR, XOR
  val logic = Mux(io.fn === FN_XOR || io.fn === FN_OR, in1_xor_in2, 0.U) |
              Mux(io.fn === FN_OR || io.fn === FN_AND, io.in1 & io.in2, 0.U)
  val shift_logic = (isCmp(io.fn) && slt) | logic | shout
  val out = Mux(io.fn === FN_ADD || io.fn === FN_SUB, io.adder_out, shift_logic)

  io.out := out
  if (xLen > 32) {
    require(xLen == 64)
    when (io.dw === DW_32) { io.out := Cat(Fill(32, out(31)), out(31,0)) }
  }
}
