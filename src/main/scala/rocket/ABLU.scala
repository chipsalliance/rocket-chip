// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.rocket

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.CoreModule

object ABLU
{
  val SZ_ALU_FN = 4
  def FN_X    = BitPat("b????")
  def FN_ADD  = 0.U(SZ_ALU_FN.W)
  def FN_SL   = 1.U(SZ_ALU_FN.W)
  def FN_SEQ  = 2.U(SZ_ALU_FN.W)
  def FN_SNE  = 3.U(SZ_ALU_FN.W)
  def FN_XOR  = 4.U(SZ_ALU_FN.W)
  def FN_SR   = 5.U(SZ_ALU_FN.W)
  def FN_OR   = 6.U(SZ_ALU_FN.W)
  def FN_AND  = 7.U(SZ_ALU_FN.W)
  def FN_SUB  = 10.U(SZ_ALU_FN.W)
  def FN_SRA  = 11.U(SZ_ALU_FN.W)
  def FN_SLT  = 12.U(SZ_ALU_FN.W)
  def FN_SGE  = 13.U(SZ_ALU_FN.W)
  def FN_SLTU = 14.U(SZ_ALU_FN.W)
  def FN_SGEU = 15.U(SZ_ALU_FN.W)

  def FN_DIV  = FN_XOR
  def FN_DIVU = FN_SR
  def FN_REM  = FN_OR
  def FN_REMU = FN_AND

  def FN_MUL    = FN_ADD
  def FN_MULH   = FN_SL
  def FN_MULHSU = FN_SEQ
  def FN_MULHU  = FN_SNE
}

import ABLU._

class ABLU(implicit p: Parameters) extends CoreModule()(p) {
  val io = new Bundle {
    val dw = Input(Bits(SZ_DW.W))
    val fn = Input(Bits(SZ_ALU_FN.W))
    val in2 = Input(UInt(xLen.W))
    val in1 = Input(UInt(xLen.W))
    val out = Output(UInt(xLen.W))
    val adder_out = Output(UInt(xLen.W))
    val cmp_out = Output(Bool())
  }

  val (pla_in, pla_out) = pla(Seq(
    // ctrl signals, shxadd1H out1H
    (BitPat("b0000"),BitPat("b00_000_00000_000_0000 0001 00_0000_0000_0001")),//FN_ADD
    (BitPat("b0001"),BitPat("b00_000_00000_100_0000 0001 00_0000_0000_0010")),//FN_SL
    (BitPat("b0010"),BitPat("b00_100_00000_000_0000 0001 00_0000_0100_0000")),//FN_SEQ
    (BitPat("b0011"),BitPat("b00_110_00000_000_0000 0001 00_0000_0100_0000")),//FN_SNE
    (BitPat("b0100"),BitPat("b00_000_00000_000_0000 0001 00_0000_0000_1000")),//FN_XOR
    (BitPat("b0101"),BitPat("b00_000_00000_000_0000 0001 00_0000_0000_0010")),//FN_SR
    (BitPat("b0110"),BitPat("b00_000_00000_000_0000 0001 00_0000_0001_0000")),//FN_OR
    (BitPat("b0111"),BitPat("b00_000_00000_000_0000 0001 00_0000_0000_0100")),//FN_AND
    (BitPat("b1000"),BitPat("b00_000_00000_000_0000 0000 00_0000_0000_0000")),//UNUSED
    (BitPat("b1001"),BitPat("b00_000_00000_000_0000 0000 00_0000_0000_0000")),//UNUSED
    (BitPat("b1010"),BitPat("b00_000_00000_000_0011 0001 00_0000_0000_0001")),//FN_SUB
    (BitPat("b1011"),BitPat("b00_000_00000_001_0000 0001 00_0000_0000_0010")),//FN_SRA
    (BitPat("b1100"),BitPat("b00_000_00000_000_0011 0001 00_0000_0100_0000")),//FN_SLT
    (BitPat("b1101"),BitPat("b00_010_00000_000_0011 0001 00_0000_0100_0000")),//FN_SGE
    (BitPat("b1110"),BitPat("b00_001_00000_000_0011 0001 00_0000_0100_0000")),//FN_SLTU
    (BitPat("b1111"),BitPat("b00_011_00000_000_0011 0001 00_0000_0100_0000")),//FN_SGEU
  ))

  pla_in := io.fn
  // note that it is inverted
  val isSub :: isIn2Inv :: isZBS :: isUW :: Nil = pla_out(21,18).asBools
  val isSRA :: isRotate :: isLeft :: Nil = pla_out(24,22).asBools
  val isCLZ :: isCZ :: isBCLR :: isCZBCLR :: isCZZBS :: Nil = pla_out(29,25).asBools
  val isUnsigned :: isInverted :: isSEQSNE :: Nil = pla_out(32,30).asBools
  val isSEXT :: isORC :: Nil = pla_out(34,33).asBools
  val shxadd1H = pla_out(17,14) // 4 bit
  val out1H = pla_out(13,0)

  // process input
  // used by SUB, ANDN, ORN, XNOR
  val in2_inv = Mux(isIn2Inv, ~io.in2, io.in2)
  val shamt =
    if (xLen == 32) io.in2(4,0)
    else {
      require(xLen == 64)
      Cat(io.in2(5) & (io.dw === DW_64), io.in2(4,0))
    }
  val in1_ext =
    if (xLen == 32) io.in1
    else {
      require(xLen == 64)
      val in1_hi_orig = io.in1(63,32)
      val in1_hi_rotate = io.in1(31,0)
      // note that sext fills 0 for ADDW/SUBW, but it works
      val in1_hi_sext = Fill(32, isSRA & io.in1(31)) // 31 to 63 then to 64 in shout_r
      val in1_hi_zext = Fill(32, 0.U)
      val in1_hi = Mux(io.dw === DW_64,
        Mux(isUW, in1_hi_zext, in1_hi_orig),
        Mux(isRotate, in1_hi_rotate, in1_hi_sext))
      Cat(in1_hi, io.in1(31,0))
    }
  val in1 = Mux(isZBS, 1.U(xLen.W), in1_ext)
  // one arm: SL, ROL, SLLIUW, ZBS, CLZ
  // another arm: SR, SRA, ROR, CTZ, ADD, SUB
  // note that CLZW is not included here
  // in1 capable of right hand operation
  // isLeft
  val in1_r = Mux(isLeft, Reverse(in1), in1)

  // shifter
  // TODO: Merge shift and rotate (manual barrel)
  val shout_r = (Cat(isSRA & in1_r(xLen-1), in1_r).asSInt >> shamt)(xLen-1,0)
  val roout_r = in1_r.rotateRight(shamt)(xLen-1,0)
  // FIXME: add withZB option
  val shro_r = Mux(isRotate, roout_r, shout_r)
  // one arm: SL, ROL, SLLIUW, ZBS
  // another arm: SR, SRA, ROR
  val shro = Mux(isLeft, Reverse(shro_r), shro_r)

  // adder
  val adder_in1_r =
    if (xLen == 32) in1_r
    else {
      require(xLen == 64)
      // one arm: CLZW
      // another arm: add, addw, add.uw
      // CLZW only reverse (31,0) here
      Mux((io.dw === DW_32) & isCLZ,
        Cat(in1_ext(63,32), Reverse(in1_ext(31,0))),
        in1_ext)
    }
  val adder_in1 =
    Mux1H(shxadd1H, Seq(
      adder_in1_r,
      (in1_ext << 1)(xLen-1,0),
      (in1_ext << 2)(xLen-1,0),
      (in1_ext << 3)(xLen-1,0)))
  // out = in1 - 1 when isCLZ/isCTZ
  val adder_in2 = Mux(isCZ,
    ~0.U(xLen.W), in2_inv)
  val adder_out = adder_in1 + adder_in2 + isSub
  io.adder_out := adder_out

  // logic
  // AND, OR, XOR
  // ANDN, ORN, XNOR
  // BCLR, BEXT, BINV, BSET
  // NOTE: can this be merged into in2_inv?
  val out_inv = Mux(isCZBCLR, ~Mux(isBCLR, shro, adder_out), shro)
  val logic_in2 = Mux(isCZZBS, out_inv, in2_inv)
  // also BINV
  val xor = adder_in1 ^ logic_in2
  // also BCLR
  val and = adder_in1 & logic_in2
  // also BSET
  val or = adder_in1 | logic_in2
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
  val exth = Cat(Fill(xLen-16, Mux(isSEXT, io.in1(15), 0.U)), io.in1(15,0))
  val extb = Cat(Fill(xLen-8, io.in1(7)), io.in1(7,0))

  // REV/ORC
  def asBytes(in: UInt): Vec[UInt] = VecInit(in.asBools.grouped(8).map(VecInit(_).asUInt).toSeq)
  val in1_bytes = asBytes(io.in1)
  val rev8 = VecInit(in1_bytes.reverse.toSeq).asUInt
  // BREV8 only in Zbk
  // discard that Mux when not withZBK
  val orc_brev8 = VecInit(in1_bytes.map(x =>
      Mux(isORC,
        Mux(x.orR, 0xFF.U(8.W), 0.U(8.W)),
        Reverse(x))
    ).toSeq).asUInt

  val out = Mux1H(out1H, Seq(
    adder_out,
    shro,
    and,
    xor,
    //
    or,
    bext,
    cmp,
    max_min,
    //
    cpop,
    ctz_out,
    exth,
    extb,
    //
    rev8,
    orc_brev8))

  val out_w =
    if (xLen == 32) out
    else {
      require(xLen == 64)
      Mux(io.dw === DW_64, out, Cat(Fill(32, out(31)), out(31,0)))
    }
  io.out := out_w
}
