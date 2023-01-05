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
  def FN_ADDUW    = 0.U
  def FN_SLLIUW   = 0.U
  def FN_SH1ADD   = 0.U
  def FN_SH1ADDUW = 0.U
  def FN_SH2ADD   = 0.U
  def FN_SH2ADDUW = 0.U
  def FN_SH3ADD   = 0.U
  def FN_SH3ADDUW = 0.U
  // Zbb
  def FN_ROR      = 0.U
  def FN_ROL      = 0.U
  def FN_ANDN     = 0.U
  def FN_ORN      = 0.U
  def FN_XNOR     = 0.U
  def FN_REV8     = 0.U
  def FN_ORCB     = 0.U
  def FN_SEXTB    = 0.U
  def FN_SEXTH    = 0.U
  def FN_ZEXTH    = 0.U
  def FN_MAX      = 0.U
  def FN_MAXU     = 0.U
  def FN_MIN      = 0.U
  def FN_MINU     = 0.U
  def FN_CPOP     = 0.U
  def FN_CLZ      = 0.U
  def FN_CTZ      = 0.U
  // Zbs
  def FN_BCLR     = 0.U
  def FN_BEXT     = 0.U
  def FN_BINV     = 0.U
  def FN_BSET     = 0.U
  // Zbk
  def FN_BREV8    = 0.U
  def FN_PACK     = 0.U
  def FN_PACKH    = 0.U
  def FN_ZIP      = 0.U
  def FN_UNZIP    = 0.U

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

trait HasALUIO extends Module with HasRocketCoreParameters {
  val io = IO(new Bundle {
    val dw = Input(UInt(SZ_DW.W))
    val fn = Input(UInt(if (usingABLU) ABLU.SZ_ALU_FN.W else SZ_ALU_FN.W))
    val in2 = Input(UInt(xLen.W))
    val in1 = Input(UInt(xLen.W))
    val out = Output(UInt(xLen.W))
    val adder_out = Output(UInt(xLen.W))
    val cmp_out = Output(Bool())
  })
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
