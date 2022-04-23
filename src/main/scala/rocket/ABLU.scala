// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.rocket

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.CoreModule

object ABLU extends ALUFN
{
  override val SZ_ALU_FN = 6
  override def FN_X    = BitPat("b??????")
  override def FN_ADD  = 0.U(SZ_ALU_FN.W)
  override def FN_SL   = 1.U(SZ_ALU_FN.W)
  override def FN_SEQ  = 2.U(SZ_ALU_FN.W)
  override def FN_SNE  = 3.U(SZ_ALU_FN.W)
  override def FN_XOR  = 4.U(SZ_ALU_FN.W)
  override def FN_SR   = 5.U(SZ_ALU_FN.W)
  override def FN_OR   = 6.U(SZ_ALU_FN.W)
  override def FN_AND  = 7.U(SZ_ALU_FN.W)
  override def FN_SUB  = 10.U(SZ_ALU_FN.W)
  override def FN_SRA  = 11.U(SZ_ALU_FN.W)
  override def FN_SLT  = 12.U(SZ_ALU_FN.W)
  override def FN_SGE  = 13.U(SZ_ALU_FN.W)
  override def FN_SLTU = 14.U(SZ_ALU_FN.W)
  override def FN_SGEU = 15.U(SZ_ALU_FN.W)

  // from Zb
  // Zba: UW is encoded here becuase it is DW_64
  override def FN_ADDUW    = 16.U(SZ_ALU_FN.W)
  override def FN_SLLIUW   = 17.U(SZ_ALU_FN.W)
  override def FN_SH1ADD   = 18.U(SZ_ALU_FN.W)
  override def FN_SH1ADDUW = 19.U(SZ_ALU_FN.W)
  override def FN_SH2ADD   = 20.U(SZ_ALU_FN.W)
  override def FN_SH2ADDUW = 21.U(SZ_ALU_FN.W)
  override def FN_SH3ADD   = 22.U(SZ_ALU_FN.W)
  override def FN_SH3ADDUW = 23.U(SZ_ALU_FN.W)
  // Zbb
  override def FN_ROR      = 24.U(SZ_ALU_FN.W)
  override def FN_ROL      = 25.U(SZ_ALU_FN.W)
  override def FN_ANDN     = 26.U(SZ_ALU_FN.W)
  override def FN_ORN      = 27.U(SZ_ALU_FN.W)
  override def FN_XNOR     = 28.U(SZ_ALU_FN.W)
  override def FN_REV8     = 29.U(SZ_ALU_FN.W)
  override def FN_ORCB     = 30.U(SZ_ALU_FN.W)
  override def FN_SEXTB    = 31.U(SZ_ALU_FN.W)
  override def FN_SEXTH    = 32.U(SZ_ALU_FN.W)
  override def FN_ZEXTH    = 33.U(SZ_ALU_FN.W)
  override def FN_MAX      = 34.U(SZ_ALU_FN.W)
  override def FN_MAXU     = 35.U(SZ_ALU_FN.W)
  override def FN_MIN      = 36.U(SZ_ALU_FN.W)
  override def FN_MINU     = 37.U(SZ_ALU_FN.W)
  override def FN_CPOP     = 38.U(SZ_ALU_FN.W)
  override def FN_CLZ      = 39.U(SZ_ALU_FN.W)
  override def FN_CTZ      = 40.U(SZ_ALU_FN.W)
  // Zbs
  override def FN_BCLR     = 41.U(SZ_ALU_FN.W)
  override def FN_BEXT     = 42.U(SZ_ALU_FN.W)
  override def FN_BINV     = 43.U(SZ_ALU_FN.W)
  override def FN_BSET     = 44.U(SZ_ALU_FN.W)
  // Zbk
  override def FN_BREV8    = 45.U(SZ_ALU_FN.W)
  override def FN_PACK     = 46.U(SZ_ALU_FN.W)
  override def FN_PACKH    = 47.U(SZ_ALU_FN.W)
  override def FN_ZIP      = 48.U(SZ_ALU_FN.W)
  override def FN_UNZIP    = 49.U(SZ_ALU_FN.W)

  override def FN_DIV  = FN_XOR
  override def FN_DIVU = FN_SR
  override def FN_REM  = FN_OR
  override def FN_REMU = FN_AND

  override def FN_MUL    = FN_ADD
  override def FN_MULH   = FN_SL
  override def FN_MULHSU = FN_SEQ
  override def FN_MULHU  = FN_SNE
}

import ABLU._

class ABLU(implicit p: Parameters) extends CoreModule()(p) with HasALUIO {
  val (pla_in, pla_out) = pla(Seq(
    // ctrl signals, shxadd1H out1H
    (BitPat("b000000"),BitPat("b00_000_0000_0000_0000 0001 00_0000_0000_0000_0001")),//FN_ADD
    (BitPat("b000001"),BitPat("b00_000_0000_1100_0000 0001 00_0000_0000_0000_0010")),//FN_SL
    (BitPat("b000010"),BitPat("b00_100_0000_0000_0000 0001 00_0000_0000_0100_0000")),//FN_SEQ
    (BitPat("b000011"),BitPat("b00_110_0000_0000_0000 0001 00_0000_0000_0100_0000")),//FN_SNE
    (BitPat("b000100"),BitPat("b00_000_0000_0000_0000 0001 00_0000_0000_0000_1000")),//FN_XOR
    (BitPat("b000101"),BitPat("b00_000_0000_0000_0000 0001 00_0000_0000_0000_0010")),//FN_SR
    (BitPat("b000110"),BitPat("b00_000_0000_0000_0000 0001 00_0000_0000_0001_0000")),//FN_OR
    (BitPat("b000111"),BitPat("b00_000_0000_0000_0000 0001 00_0000_0000_0000_0100")),//FN_AND
    (BitPat("b001000"),BitPat("b00_000_0000_0000_0000 0000 00_0000_0000_0000_0000")),//UNUSED
    (BitPat("b001001"),BitPat("b00_000_0000_0000_0000 0000 00_0000_0000_0000_0000")),//UNUSED
    (BitPat("b001010"),BitPat("b00_000_0000_0000_0011 0001 00_0000_0000_0000_0001")),//FN_SUB
    (BitPat("b001011"),BitPat("b00_000_0000_0001_0000 0001 00_0000_0000_0000_0010")),//FN_SRA
    (BitPat("b001100"),BitPat("b00_000_0000_0000_0011 0001 00_0000_0000_0100_0000")),//FN_SLT
    (BitPat("b001101"),BitPat("b00_010_0000_0000_0011 0001 00_0000_0000_0100_0000")),//FN_SGE
    (BitPat("b001110"),BitPat("b00_001_0000_0000_0011 0001 00_0000_0000_0100_0000")),//FN_SLTU
    (BitPat("b001111"),BitPat("b00_011_0000_0000_0011 0001 00_0000_0000_0100_0000")),//FN_SGEU
    // Zb
    (BitPat("b010000"),BitPat("b00_000_0000_0000_1000 0001 00_0000_0000_0000_0001")),//FN_ADDUW
    (BitPat("b010001"),BitPat("b00_000_0000_1100_1000 0001 00_0000_0000_0000_0010")),//FN_SLLIUW
    (BitPat("b010010"),BitPat("b00_000_0000_0000_0000 0010 00_0000_0000_0000_0001")),//FN_SH1ADD
    (BitPat("b010011"),BitPat("b00_000_0000_0000_1000 0010 00_0000_0000_0000_0001")),//FN_SH1ADDUW
    (BitPat("b010100"),BitPat("b00_000_0000_0000_0000 0100 00_0000_0000_0000_0001")),//FN_SH2ADD
    (BitPat("b010101"),BitPat("b00_000_0000_0000_1000 0100 00_0000_0000_0000_0001")),//FN_SH2ADDUW
    (BitPat("b010110"),BitPat("b00_000_0000_0000_0000 1000 00_0000_0000_0000_0001")),//FN_SH3ADD
    (BitPat("b010111"),BitPat("b00_000_0000_0000_1000 1000 00_0000_0000_0000_0001")),//FN_SH3ADDUW
    (BitPat("b011000"),BitPat("b00_000_0000_0010_0000 0001 00_0000_0000_0000_0010")),//FN_ROR
    (BitPat("b011001"),BitPat("b00_000_0000_1110_0000 0001 00_0000_0000_0000_0010")),//FN_ROL
    (BitPat("b011010"),BitPat("b00_000_0000_0000_0010 0001 00_0000_0000_0000_0100")),//FN_ANDN
    (BitPat("b011011"),BitPat("b00_000_0000_0000_0010 0001 00_0000_0000_0001_0000")),//FN_ORN
    (BitPat("b011100"),BitPat("b00_000_0000_0000_0010 0001 00_0000_0000_0000_1000")),//FN_XNOR
    (BitPat("b011101"),BitPat("b00_000_0000_0000_0000 0001 00_0001_0000_0000_0000")),//FN_REV8
    (BitPat("b011110"),BitPat("b10_000_0000_0000_0000 0001 00_0010_0000_0000_0000")),//FN_ORCB
    (BitPat("b011111"),BitPat("b00_000_0000_0000_0000 0001 00_0000_1000_0000_0000")),//FN_SEXTB
    (BitPat("b100000"),BitPat("b01_000_0000_0000_0000 0001 00_0000_0100_0000_0000")),//FN_SEXTH
    (BitPat("b100001"),BitPat("b00_000_0000_0000_0000 0001 00_0000_0100_0000_0000")),//FN_ZEXTH
    (BitPat("b100010"),BitPat("b00_000_0000_0000_0011 0001 00_0000_0000_1000_0000")),//FN_MAX
    (BitPat("b100011"),BitPat("b00_001_0000_0000_0011 0001 00_0000_0000_1000_0000")),//FN_MAXU
    (BitPat("b100100"),BitPat("b00_010_0000_0000_0011 0001 00_0000_0000_1000_0000")),//FN_MIN
    (BitPat("b100101"),BitPat("b00_011_0000_0000_0011 0001 00_0000_0000_1000_0000")),//FN_MINU
    (BitPat("b100110"),BitPat("b00_000_0000_0000_0000 0001 00_0000_0001_0000_0000")),//FN_CPOP
    (BitPat("b100111"),BitPat("b00_000_1101_1110_0000 0001 00_0000_0010_0000_0000")),//FN_CLZ
    (BitPat("b101000"),BitPat("b00_000_1101_0000_0000 0001 00_0000_0010_0000_0000")),//FN_CTZ
    (BitPat("b101001"),BitPat("b00_000_1110_1000_0100 0001 00_0000_0000_0000_0100")),//FN_BCLR
    (BitPat("b101010"),BitPat("b00_000_1000_1000_0100 0001 00_0000_0000_0010_0000")),//FN_BEXT
    (BitPat("b101011"),BitPat("b00_000_1000_1000_0100 0001 00_0000_0000_0000_1000")),//FN_BINV
    (BitPat("b101100"),BitPat("b00_000_1000_1000_0100 0001 00_0000_0000_0001_0000")),//FN_BSET
    (BitPat("b101101"),BitPat("b00_000_0000_0000_0000 0001 00_0010_0000_0000_0000")),//FN_BREV8
    (BitPat("b101110"),BitPat("b00_000_0000_0000_0000 0001 00_0100_0000_0000_0000")),//FN_PACK
    (BitPat("b101111"),BitPat("b00_000_0000_0000_0000 0001 00_1000_0000_0000_0000")),//FN_PACKH
    (BitPat("b110000"),BitPat("b00_000_0000_0000_0000 0001 01_0000_0000_0000_0000")),//FN_ZIP
    (BitPat("b110001"),BitPat("b00_000_0000_0000_0000 0001 10_0000_0000_0000_0000")),//FN_UNZIP
  ))

  pla_in := io.fn
  // note that it is inverted
  val isSub :: isIn2Inv :: isZBS :: isUW :: Nil = pla_out(25,22).asBools
  val isSRA :: isRotate :: isLeft :: isLeftZBS :: Nil = pla_out(29,26).asBools
  val isCZ :: isBCLR :: isCZBCLR :: isCZZBS :: Nil = pla_out(33,30).asBools
  val isUnsigned :: isInverted :: isSEQSNE :: Nil = pla_out(36,34).asBools
  val isSEXT :: isORC :: Nil = pla_out(38,37).asBools
  val shxadd1H = pla_out(21,18) // 4 bit
  val out1H = pla_out(17,0)

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
      // note that CLZW uses rotate
      val in1_hi_rotate = io.in1(31,0)
      // note that sext fills 0 for ADDW/SUBW, but it works
      val in1_hi_sext = Fill(32, isSRA & io.in1(31)) // 31 to 63 then to 64 in shout_r
      val in1_hi_zext = Fill(32, 0.U)
      val in1_hi = Mux(io.dw === DW_64,
        Mux(isUW, in1_hi_zext, in1_hi_orig),
        Mux(isRotate, in1_hi_rotate, in1_hi_sext))
      Cat(in1_hi, io.in1(31,0))
    }
  // one arm: SL, ROL, SLLIUW, CLZ
  // another arm: SR, SRA, ROR, CTZ, ADD, SUB, ZBS
  // note that CLZW is not included here
  // in1 capable of right hand operation
  // isLeft
  val in1_r = Mux(isLeft, Reverse(in1_ext), in1_ext)

  // shifter
  val shin = Mux(isZBS,
    if (xLen == 32) (BigInt(1) << 31).U(32.W)
    else {
      require(xLen == 64)
      (BigInt(1) << 63).U(64.W)
    }, in1_r)
  // TODO: Merge shift and rotate (manual barrel)
  val shout_r = (Cat(isSRA & shin(xLen-1), shin).asSInt >> shamt)(xLen-1,0)
  val roout_r = shin.rotateRight(shamt)(xLen-1,0)
  // FIXME: add withZB option
  val shro_r = Mux(isRotate, roout_r, shout_r)
  // one arm: SL, ROL, SLLIUW, ZBS
  // another arm: SR, SRA, ROR
  val shro = Mux(isLeftZBS, Reverse(shro_r), shro_r)

  // adder
  val adder_in1 =
    Mux1H(shxadd1H, Seq(
      if (xLen == 32) in1_r
      else {
        require(xLen == 64)
        // for CLZW/CTZW
        Mux(io.dw === DW_64, in1_r, Cat(Fill(32, 1.U(1.W)), in1_r(31,0)))
      },
      (in1_ext << 1)(xLen-1,0),
      (in1_ext << 2)(xLen-1,0),
      (in1_ext << 3)(xLen-1,0)))
  // out = in1 - 1 when isCLZ/isCTZ
  // note that when isCZ, isSub is 0 as ~0 = ~1+1 = -1
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
  val max_min = Mux(cmp, io.in2, io.in1)

  // counter
  // CLZ, CPOP, CTZ
  val cpop = PopCount(
    if (xLen == 32) io.in1
    else {
      require(xLen == 64)
      Mux(io.dw === DW_64, io.in1, Cat(Fill(32, 0.U(1.W)), io.in1(31,0)))
    })
  // ctz_in = ~adder_out & adder_in1 // all zero or one hot
  val ctz_in = and
  val ctz_out = Cat(~ctz_in.orR, VecInit((0 to log2Ceil(xLen)-1).map(
    x => {
      val bits = ctz_in.asBools.zipWithIndex
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

  // pack
  def sext(in: UInt): UInt = {
    val in_hi_32 = Fill(32, in(31))
    Cat(in_hi_32, in)
  }
  val pack =
    if (xLen == 32) Cat(io.in2(xLen/2-1,0), io.in1(xLen/2-1,0))
    else {
      require(xLen == 64)
      Mux(io.dw === DW_64,
        Cat(io.in2(xLen/2-1,0), io.in1(xLen/2-1,0)),
        sext(Cat(io.in2(xLen/4-1,0), io.in1(xLen/4-1,0))))
    }
  val packh = Cat(0.U((xLen-16).W), io.in2(7,0), io.in1(7,0))

  // zip
  // FIXME: use Option here for RV64?
  val zip = if (xLen == 32) {
    val lo = io.in1(15,0).asBools
    val hi = io.in1(31,16).asBools
    VecInit(lo.zip(hi).map { case (l, h) => VecInit(Seq(l, h)).asUInt }).asUInt
  } else 0.U
  val unzip = if (xLen == 32) {
    val bits = io.in1.asBools.zipWithIndex
    val lo = VecInit(bits filter { case (_, i) => i % 2 == 0 } map { case (b, _) => b }).asUInt
    val hi = VecInit(bits filter { case (_, i) => i % 2 != 0 } map { case (b, _) => b }).asUInt
    Cat(hi, lo)
  } else 0.U

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
    orc_brev8,
    pack,
    packh,
    //
    zip,
    unzip))

  val out_w =
    if (xLen == 32) out
    else {
      require(xLen == 64)
      Mux(io.dw === DW_64, out, Cat(Fill(32, out(31)), out(31,0)))
    }
  io.out := out_w
}
