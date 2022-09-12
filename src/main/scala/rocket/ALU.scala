// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.rocket

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.CoreModule

object ALU
{
  val SZ_ALU_FN = 4
  def FN_X: BitPat = BitPat("b????")
  def FN_ADD: UInt = 0.U
  def FN_SL: UInt = 1.U
  def FN_SEQ: UInt = 2.U
  def FN_SNE: UInt = 3.U
  def FN_XOR: UInt = 4.U
  def FN_SR: UInt = 5.U
  def FN_OR: UInt = 6.U
  def FN_AND: UInt = 7.U
  def FN_SUB: UInt = 10.U
  def FN_SRA: UInt = 11.U
  def FN_SLT: UInt = 12.U
  def FN_SGE: UInt = 13.U
  def FN_SLTU: UInt = 14.U
  def FN_SGEU: UInt = 15.U

  def FN_DIV: UInt = FN_XOR
  def FN_DIVU: UInt = FN_SR
  def FN_REM: UInt = FN_OR
  def FN_REMU: UInt = FN_AND

  def FN_MUL: UInt = FN_ADD
  def FN_MULH: UInt = FN_SL
  def FN_MULHSU: UInt = FN_SEQ
  def FN_MULHU: UInt = FN_SNE

  def isMulFN(fn: UInt, cmp: UInt): Bool = fn(1,0) === cmp(1,0)
  def isSub(cmd: UInt): Bool = cmd(3)
  def isCmp(cmd: UInt): Bool = cmd >= FN_SLT
  def cmpUnsigned(cmd: UInt): Bool = cmd(1)
  def cmpInverted(cmd: UInt): Bool = cmd(0)
  def cmpEq(cmd: UInt): Bool = !cmd(3)
}

import ALU._

class ALU(implicit p: Parameters) extends CoreModule()(p) {
  val io = new Bundle {
    val dw: UInt = Input(UInt(SZ_DW.W))
    val fn: UInt = Input(UInt(SZ_ALU_FN.W))
    val in2: UInt = Input(UInt(xLen.W))
    val in1: UInt = Input(UInt(xLen.W))
    val out: UInt = Output(UInt(xLen.W))
    val adder_out: UInt = Output(UInt(xLen.W))
    val cmp_out: Bool = Output(Bool())
  }

  // ADD, SUB
  val in2_inv: UInt = Mux(isSub(io.fn), (~io.in2).asUInt, io.in2)
  val in1_xor_in2: UInt = io.in1 ^ in2_inv
  io.adder_out := io.in1 + in2_inv + isSub(io.fn)

  // SLT, SLTU
  val slt: Bool =
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
  val shin: UInt = Mux(io.fn === FN_SR  || io.fn === FN_SRA, shin_r, Reverse(shin_r))
  val shout_r: UInt = (Cat(isSub(io.fn) & shin(xLen-1), shin).asSInt >> shamt)(xLen-1,0)
  val shout_l: UInt = Reverse(shout_r)
  val shout: UInt = Mux(io.fn === FN_SR || io.fn === FN_SRA, shout_r, 0.U) |
              Mux(io.fn === FN_SL,                     shout_l, 0.U)

  // AND, OR, XOR
  val logic: UInt = Mux(io.fn === FN_XOR || io.fn === FN_OR, in1_xor_in2, 0.U) |
              Mux(io.fn === FN_OR || io.fn === FN_AND, io.in1 & io.in2, 0.U)
  val shift_logic: UInt = (isCmp(io.fn) && slt) | logic | shout
  val out: UInt = Mux(io.fn === FN_ADD || io.fn === FN_SUB, io.adder_out, shift_logic)

  io.out := out
  if (xLen > 32) {
    require(xLen == 64)
    when (io.dw === DW_32) { io.out := Cat(Fill(32, out(31)), out(31,0)) }
  }
}
