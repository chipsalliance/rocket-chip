// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.rocket

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.CoreModule

object ABLU
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

import ABLU._

class ABLU(implicit p: Parameters) extends CoreModule()(p) {
  val io = new Bundle {
    val dw = Bits(INPUT, SZ_DW)
    val fn = Bits(INPUT, SZ_ALU_FN)
    val in2 = UInt(INPUT, xLen)
    val in1 = UInt(INPUT, xLen)
    val out = UInt(OUTPUT, xLen)
    val adder_out = UInt(OUTPUT, xLen)
    val cmp_out = Bool(OUTPUT)
  }

  // process input
  // used by SUB, ANDN, ORN, XNOR
  val in2_inv = Mux(isSub | isLogicN, ~io.in2, io.in2)
  val shamt =
    if (xLen == 32) io.in2(4,0)
    else {
      require(xLen == 64)
      Cat(io.in2(5) & (io.dw === DW_64), io.in2(4,0))
    }
  val in1_ext =
    if (xLen == 32) Mux(isZBS, 1.U, io.in1)
    else {
      require(xLen == 64)
      val in1_hi_orig = io.in1(63,32)
      val in1_hi_rotate = io.in1(31,0)
      val in1_hi_sext = Fill(32, isSRA & io.in1(31))
      val in1_hi_zext = Fill(32, 0.U)
      val in1_hi = Mux(io.dw === DW_64,
        Mux(isUW, in1_hi_zext, in1_hi_orig),
        Mux(isRotate, in1_hi_rotate, in1_hi_sext))
      Cat(in1_hi, io.in1(31,0))
    }
  val in1 = Mux(isZBS, 1.U(xLen.W), in1_ext)
  // another arm: SR, SRA, ROR, CTZ, ADD, SUB
  // note that CLZW is not included here
  // in1 capable of right hand operation
  val in1_r = Mux(isSL | isROL | isSLLIUW | isZBS | isCLZ, Reverse(in1), in1)

  // shifter
  // TODO: Merge shift and rotate (manual barrel)
  val shout_r = (Cat(isSub & in1_r(xLen-1), in1_r).asSInt >> shamt)(xLen-1,0)
  val roout_r = in1_r.rotateRight(shamt)(xLen-1,0)
  // FIXME: add withZB option
  val shro_r = Mux(isRotate, roout_r, shout_r)
  val shro = Mux(isSR | isSRA | isROR, shro_r, Reverse(shro_r))

  // adder
  val adder_in1_r =
    if (xLen == 32) in1_r
    else {
      require(xLen == 64)
      Mux(io.dw === DW_64, in1_r,
        // CLZW only reverse (31,0) here
        Mux(isCLZ, Cat(in1_ext(63,32), Reverse(in1_ext(31,0))), in1_ext))
    }
  val adder_in1 =
    Mux(isSH1ADD, (in1_ext << 1)(xLen-1,0),
      Mux(isSH2ADD, (in1_ext << 2)(xLen-1,0),
        Mux(isSH3ADD, (in1_ext << 3)(xLen-1,0), adder_in1_r)))
  // out = in1 - 1 when isCLZ/isCTZ
  val adder_in2 = Mux(isCLZ | isCTZ,
    ~0.U(xLen.W), in2_inv)
  val adder_out = adder_in1 + adder_in2 + isSub
  io.adder_out := adder_out

  // logic
  // AND, OR, XOR
  // ANDN, ORN, XNOR
  // BCLR, BEXT, BINV, BSET
  // NOTE: can this be merged into in2_inv?
  val out_inv = Mux(isCLZ | isCTZ | isBCLR, ~Mux(isBCLR, shro, adder_out), shro)
  val logic_in2 = Mux(isCLZ | isCTZ | isZBS, out_inv, in2_inv)
  // also BINV
  val xor = adder_in1 ^ logic_in2
  // also BCLR
  val and = adder_in1 & logic_in2
  // also BSET
  val or = xor | and
  val bext = and.orR

  // SLT, SLTU
  // BEQ, BNE, BLT, BGE
  // MAX, MIN
  val slt =
    Mux(io.in1(xLen-1) === io.in2(xLen-1), adder_out(xLen-1),
    Mux(isUnsigned, io.in2(xLen-1), io.in1(xLen-1)))
  val cmp = isInverted ^ Mux(isSEQSNE, ~xor.orR, slt)
  io.cmp_out := cmp
  // MAX, MAXU, MIN, MINU
  // FIXME: actually no fn defined now, inverted is min
  //        add withZB option
  val max_min = Mux(cmp, io.in2, io.in1)

  // counter
  // CLZ, CPOP, CTZ
  val cpop = PopCount(
    if (xLen == 32) io.in1
    else {
      require(xLen == 64)
      Mux(io.dw === DW_64, io.in1, Cat(Fill(32, 0.U(1.W)), io.in1(31,0)))
    })
  // ctz_in = ~adder_out & adder_in1_r // all zero or one hot
  val ctz_in = and
  val ctz_in_w = // mask higher bits
    if (xLen == 32) ctz_in
    else {
      require(xLen == 64)
      Mux(io.dw === DW_64, ctz_in, Cat(Fill(32, 0.U(1.W)), ctz_in(31,0)))
    }
  val ctz_out = Cat(~ctz_in_w.orR, VecInit((0 to log2Ceil(xLen)-1).map(
    x => {
      val bits = ctz_in_w.asBools.zipWithIndex
      VecInit(
        bits
          filter { case (_, i) => i % (1 << (x + 1)) >= (1 << x) }
          map { case (b, _) => b }
        ).asUInt.orR
    }
  ).toSeq).asUInt)

  // ZEXT/SEXT
  val exth = Mux(isZEXT | isSEXT,
    Cat(Fill(xLen-16, Mux(isSEXT, io.in1(15), 0.U)), io.in1(15,0)),
    0.U)
  val extb = Mux(isSEXT,
    Cat(Fill(xLen-8, io.in1(7)), io.in1(7,0)),
    0.U)

  // REV/ORC
  def asBytes(in: UInt): Vec[UInt] = VecInit(in.asBools.grouped(8).map(VecInit(_).asUInt).toSeq)
  val in1_bytes = asBytes(io.in1)
  val rev8 = VecInit(in1_bytes.reverse.toSeq).asUInt
  // BREV8 only in Zbk
  // discard that Mux when not withZBK
  val orc_brev8 = VecInit(in1_bytes.map(x =>
      Mux(io.fn === FN_ORC,
        Mux(x.orR, 0xFF.U(8.W), 0.U(8.W)),
        Reverse(x))
    ).toSeq).asUInt

  io.out :=
    if (xLen == 32) out
    else {
      require(xLen == 64)
      Mux(io.dw === DW_64, out, Cat(Fill(32, out(31)), out(31,0)))
    }
}
