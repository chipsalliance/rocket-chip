// See LICENSE for license details.

package rocket

import Chisel._
import cde.{Parameters, Field}
import Instructions._

object ALU
{
  val SZ_ALU_FN = 4
  val FN_X    = BitPat("b????")
  val FN_ADD  = UInt(0)
  val FN_SL   = UInt(1)
  val FN_XOR  = UInt(4)
  val FN_OR   = UInt(6)
  val FN_AND  = UInt(7)
  val FN_SR   = UInt(5)
  val FN_SEQ  = UInt(8)
  val FN_SNE  = UInt(9)
  val FN_SUB  = UInt(10)
  val FN_SRA  = UInt(11)
  val FN_SLT  = UInt(12)
  val FN_SGE  = UInt(13)
  val FN_SLTU = UInt(14)
  val FN_SGEU = UInt(15)

  val FN_DIV  = FN_XOR
  val FN_DIVU = FN_SR
  val FN_REM  = FN_OR
  val FN_REMU = FN_AND

  val FN_MUL    = FN_ADD
  val FN_MULH   = FN_SL
  val FN_MULHSU = FN_SLT
  val FN_MULHU  = FN_SLTU

  def isMulFN(fn: Bits, cmp: Bits) = fn(1,0) === cmp(1,0)
  def isSub(cmd: Bits) = cmd(3)
  def cmpUnsigned(cmd: Bits) = cmd(1)
  def cmpInverted(cmd: Bits) = cmd(0)
  def cmpEq(cmd: Bits) = !cmd(2)
}
import ALU._

class ALU(implicit p: Parameters) extends CoreModule()(p) {
  val io = new Bundle {
    val dw = Bits(INPUT, SZ_DW)
    val fn = Bits(INPUT, SZ_ALU_FN)
    val in2 = UInt(INPUT, xLen)
    val in1 = UInt(INPUT, xLen)
    val out = UInt(OUTPUT, xLen)
    val adder_out = UInt(OUTPUT, xLen)
  }

  // ADD, SUB
  val sum = io.in1 + Mux(isSub(io.fn), -io.in2, io.in2)

  // SLT, SLTU
  val cmp = cmpInverted(io.fn) ^
    Mux(cmpEq(io.fn), sum === UInt(0),
    Mux(io.in1(xLen-1) === io.in2(xLen-1), sum(xLen-1),
    Mux(cmpUnsigned(io.fn), io.in2(xLen-1), io.in1(xLen-1))))

  // SLL, SRL, SRA
  val full_shamt = io.in2(log2Up(xLen)-1,0)

  val (shamt, shin_r) =
    if (xLen == 32) (full_shamt, io.in1)
    else {
      require(xLen == 64)
      val shin_hi_32 = Fill(32, isSub(io.fn) && io.in1(31))
      val shin_hi = Mux(io.dw === DW_64, io.in1(63,32), shin_hi_32)
      val shamt = Cat(full_shamt(5) & (io.dw === DW_64), full_shamt(4,0))
      (shamt, Cat(shin_hi, io.in1(31,0)))
    }
  val shin = Mux(io.fn === FN_SR  || io.fn === FN_SRA, shin_r, Reverse(shin_r))
  val shout_r = (Cat(isSub(io.fn) & shin(xLen-1), shin).toSInt >> shamt)(xLen-1,0)
  val shout_l = Reverse(shout_r)

  val out =
    Mux(io.fn === FN_ADD || io.fn === FN_SUB,  sum,
    Mux(io.fn === FN_SR  || io.fn === FN_SRA,  shout_r,
    Mux(io.fn === FN_SL,                       shout_l,
    Mux(io.fn === FN_AND,                      io.in1 & io.in2,
    Mux(io.fn === FN_OR,                       io.in1 | io.in2,
    Mux(io.fn === FN_XOR,                      io.in1 ^ io.in2,
                /* all comparisons */          cmp))))))

  io.adder_out := sum
  io.out := out
  if (xLen > 32) {
    require(xLen == 64)
    when (io.dw === DW_32) { io.out := Cat(Fill(32, out(31)), out(31,0)) }
  }
}
