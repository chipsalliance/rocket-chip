// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.rocket

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.CoreModule

object ALU
{
  val SZ_ALU_FN = 4
  def FN_X    = BitPat("b????")
  def FN_ADD  = UInt(0)
  def FN_SL   = UInt(1)
  def FN_SEQ  = UInt(2)
  def FN_SNE  = UInt(3)
  def FN_XOR  = UInt(4)
  def FN_SR   = UInt(5)
  def FN_OR   = UInt(6)
  def FN_AND  = UInt(7)
  def FN_SUB  = UInt(10)
  def FN_SRA  = UInt(11)
  def FN_SLT  = UInt(12)
  def FN_SGE  = UInt(13)
  def FN_SLTU = UInt(14)
  def FN_SGEU = UInt(15)

  def FN_DIV  = FN_XOR
  def FN_DIVU = FN_SR
  def FN_REM  = FN_OR
  def FN_REMU = FN_AND

  def FN_MUL    = FN_ADD
  def FN_MULH   = FN_SL
  def FN_MULHSU = FN_SEQ
  def FN_MULHU  = FN_SNE

  def isMulFN(fn: UInt, cmp: UInt) = fn(1,0) === cmp(1,0)
  def isSub(cmd: UInt) = cmd(3)
  def isCmp(cmd: UInt) = cmd >= FN_SLT
  def cmpUnsigned(cmd: UInt) = cmd(1)
  def cmpInverted(cmd: UInt) = cmd(0)
  def cmpEq(cmd: UInt) = !cmd(3)
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
    val cmp_out = Bool(OUTPUT)
  }

  // ADD, SUB
  // FIXME: isSub also for ANDN, ORN, XNOR, CLZ, CTZ
  val adder_in1 = Mux(io.fn === FN_CLZ || io.fn === FN_CTZ, shin, io.in1)
  val adder_in2 = Mux(io.fn === FN_CLZ || io.fn === FN_CTZ, 1.U, io.in2)
  val in2_inv = Mux(isSub(io.fn), ~adder_in2, adder_in2)
  val in1_xor_in2 = adder_in1 ^ in2_inv
  val adder_out = adder_in1 + in2_inv + isSub(io.fn)
  io.adder_out := adder_out

  // CLZ, CPOP, CTZ
  val pop_in = Mux(io.fn === FN_CPOP, io.in1, adder_out)
  val pop_inw =
    if (xLen == 32) pop_in
    else {
      require(xLen == 64)
      Mux(io.dw === DW_64, pop_in, Cat(Fill(32, 0.U), pop_in(31,0)))
    }
  val pop_out = Mux(io.fn === FN_CLZ || io.fn === FN_CPOP || io.fn === FN_CTZ,
    PopCount(pop_inw), 0.U)

  // zext/sext
  val exth = Mux(io.fn === FN_ZEXT || io.fn === FN_SEXT,
    Cat(Fill(xLen-16, Mux(io.fn === FN_SEXT, io.in1(15), 0.U)), io.in1(15,0)),
    0.U)
  val extb = Mux(io.fn === FN_SEXT,
    Cat(Fill(xLen-8, io.in1(7)), io.in1(7,0)),
    0.U)
  // FIXME: impl by dw or fn
  val ext = Mux(isHalfWord, exth, extb)

  // rev/orc
  def asBytes(in: UInt): Vec[UInt] = VecInit(in.asBools.grouped(8).map(VecInit(_).asUInt).toSeq)
  val rs1_bytes = asBytes(io.rs1)
  val rev8 = VecInit(rs1_bytes.reverse.toSeq).asUInt
  // brev8 only in Zbk
  // discard that Mux when not withZBK
  val orc_brev8 = VecInit(rs1_bytes.map(x =>
      Mux(io.fn === FN_ORC,
        Mux(x.orR, 0xFF.U(8.W), 0.U(8.W)),
        Reverse(x))
    ).toSeq).asUInt

  // SLT, SLTU
  val slt =
    Mux(io.in1(xLen-1) === io.in2(xLen-1), io.adder_out(xLen-1),
    Mux(cmpUnsigned(io.fn), io.in2(xLen-1), io.in1(xLen-1)))
  val cmp = cmpInverted(io.fn) ^ Mux(cmpEq(io.fn), in1_xor_in2 === UInt(0), slt)
  io.cmp_out := cmp
  // MAX, MAXU, MIN, MINU
  // FIXME: actually no fn defined now, inverted is min
  //        add withZB option
  val max_min = Mux(io.fn === FN_MAX || io.fn === FN_MIN, Mux(cmp, io.in2, io.in1), UInt(0))

  // SLL, SRL, SRA
  // ROL, ROLW, ROR, RORI, RORW, RORIW
  val (shamt, shin_r) =
    if (xLen == 32) (io.in2(4,0), io.in1)
    else {
      require(xLen == 64)
      // FIXME: add impl of isRotate
      // FIXME: isRotate also for CLZW
      val shin_hi_32 = Mux(isRotate, io.in1(31,0), Fill(32, isSub(io.fn) && io.in1(31)))
      val shin_hi = Mux(io.dw === DW_64, io.in1(63,32), shin_hi_32)
      val shamt = Cat(io.in2(5) & (io.dw === DW_64), io.in2(4,0))
      (shamt, Cat(shin_hi, io.in1(31,0)))
    }
  // FIXME: reverse also for CLZ/CLZW
  val shin = Mux(io.fn === FN_SR  || io.fn === FN_SRA, shin_r, Reverse(shin_r))
  // TODO: Merge shift and rotate (manual barrel)
  val shout_r = (Cat(isSub(io.fn) & shin(xLen-1), shin).asSInt >> shamt)(xLen-1,0)
  val roout_r = shin.rotateRight(shamt)(xLen-1,0)
  // FIXME: add withZB option
  val shro_r = Mux(isRotate, roout_r, shout_r)
  val shro_l = Reverse(shro_r)
  // not sign extended, used by rorw
  val shro = Mux(io.fn === FN_SR || io.fn === FN_SRA, shro_r, UInt(0)) |
             Mux(io.fn === FN_SL,                     shro_l, UInt(0))

  // AND, OR, XOR
  // ANDN, ORN, XNOR
  val logic = Mux(io.fn === FN_XOR || io.fn === FN_OR, in1_xor_in2, UInt(0)) |
              Mux(io.fn === FN_OR || io.fn === FN_AND, io.in1 & in2_inv, UInt(0))
  val shift_logic = (isCmp(io.fn) && slt) | logic | shro | max_min | pop_out | ext
  val out = Mux(io.fn === FN_ADD || io.fn === FN_SUB, io.adder_out, shift_logic)

  io.out :=
    if (xLen == 32) out
    else {
      require(xLen == 64)
      Mux(io.dw === DW_64, out, Cat(Fill(32, out(31)), out(31,0)))
    }
}
