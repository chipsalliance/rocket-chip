// See LICENSE for license details.

package rocket

import Chisel._
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

class ALUIO extends CoreBundle {
  val dw = Bits(INPUT, SZ_DW)
  val fn = Bits(INPUT, SZ_ALU_FN)
  val in2 = UInt(INPUT, xLen)
  val in1 = UInt(INPUT, xLen)
  val out = UInt(OUTPUT, xLen)
  val adder_out = UInt(OUTPUT, xLen)
}

class ALU extends Module
{
  val io = new ALUIO

  // ADD, SUB
  val sum = io.in1 + Mux(isSub(io.fn), -io.in2, io.in2)

  // SLT, SLTU
  val cmp = cmpInverted(io.fn) ^
    Mux(cmpEq(io.fn), sum === UInt(0),
    Mux(io.in1(63) === io.in2(63), sum(63),
    Mux(cmpUnsigned(io.fn), io.in2(63), io.in1(63))))

  // SLL, SRL, SRA
  val shamt = Cat(io.in2(5) & (io.dw === DW_64), io.in2(4,0)).toUInt
  val shin_hi_32 = Mux(isSub(io.fn), Fill(32, io.in1(31)), UInt(0,32))
  val shin_hi = Mux(io.dw === DW_64, io.in1(63,32), shin_hi_32)
  val shin_r = Cat(shin_hi, io.in1(31,0))
  val shin = Mux(io.fn === FN_SR  || io.fn === FN_SRA, shin_r, Reverse(shin_r))
  val shout_r = (Cat(isSub(io.fn) & shin(63), shin).toSInt >> shamt)(63,0)
  val shout_l = Reverse(shout_r)

  val out64 =
    Mux(io.fn === FN_ADD || io.fn === FN_SUB,  sum,
    Mux(io.fn === FN_SR  || io.fn === FN_SRA,  shout_r,
    Mux(io.fn === FN_SL,                       shout_l,
    Mux(io.fn === FN_AND,                      io.in1 & io.in2,
    Mux(io.fn === FN_OR,                       io.in1 | io.in2,
    Mux(io.fn === FN_XOR,                      io.in1 ^ io.in2,
                /* all comparisons */          cmp))))))

  val out_hi = Mux(io.dw === DW_64, out64(63,32), Fill(32, out64(31)))
  io.out := Cat(out_hi, out64(31,0)).toUInt
  io.adder_out := sum
}
