// SPDX-License-Identifier: Apache-2.0

package freechips.rocketchip.rocket

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tile.CoreModule

// These are for the ABLU unit, which uses an alternate function encoding
class ABLUFN extends ALUFN
{
  override val SZ_ALU_FN = 39
  override def FN_X        = BitPat("b??_???_????_????_????__????__??_????_????_????_????")
  override def FN_ADD      = "b00_000_0000_0000_0000__0001__00_0000_0000_0000_0001".U(SZ_ALU_FN.W)
  override def FN_SL       = "b00_000_0000_1100_0000__0001__00_0000_0000_0000_0010".U(SZ_ALU_FN.W)
  override def FN_SEQ      = "b00_100_0000_0000_0000__0001__00_0000_0000_0100_0000".U(SZ_ALU_FN.W)
  override def FN_SNE      = "b00_110_0000_0000_0000__0001__00_0000_0000_0100_0000".U(SZ_ALU_FN.W)
  override def FN_XOR      = "b00_000_0000_0000_0000__0001__00_0000_0000_0000_1000".U(SZ_ALU_FN.W)
  override def FN_SR       = "b00_000_0000_0000_0000__0001__00_0000_0000_0000_0010".U(SZ_ALU_FN.W)
  override def FN_OR       = "b00_000_0000_0000_0000__0001__00_0000_0000_0001_0000".U(SZ_ALU_FN.W)
  override def FN_AND      = "b00_000_0000_0000_0000__0001__00_0000_0000_0000_0100".U(SZ_ALU_FN.W)
  override def FN_SUB      = "b00_000_0000_0000_0011__0001__00_0000_0000_0000_0001".U(SZ_ALU_FN.W)
  override def FN_SRA      = "b00_000_0000_0001_0000__0001__00_0000_0000_0000_0010".U(SZ_ALU_FN.W)
  override def FN_SLT      = "b00_000_0000_0000_0011__0001__00_0000_0000_0100_0000".U(SZ_ALU_FN.W)
  override def FN_SGE      = "b00_010_0000_0000_0011__0001__00_0000_0000_0100_0000".U(SZ_ALU_FN.W)
  override def FN_SLTU     = "b00_001_0000_0000_0011__0001__00_0000_0000_0100_0000".U(SZ_ALU_FN.W)
  override def FN_SGEU     = "b00_011_0000_0000_0011__0001__00_0000_0000_0100_0000".U(SZ_ALU_FN.W)

  // from Zb
  // Zba: UW is encoded here becuase it is DW_64
  override def FN_ADDUW    = "b00_000_0000_0000_1000__0001__00_0000_0000_0000_0001".U(SZ_ALU_FN.W)
  override def FN_SLLIUW   = "b00_000_0000_1100_1000__0001__00_0000_0000_0000_0010".U(SZ_ALU_FN.W)
  override def FN_SH1ADD   = "b00_000_0000_0000_0000__0010__00_0000_0000_0000_0001".U(SZ_ALU_FN.W)
  override def FN_SH1ADDUW = "b00_000_0000_0000_1000__0010__00_0000_0000_0000_0001".U(SZ_ALU_FN.W)
  override def FN_SH2ADD   = "b00_000_0000_0000_0000__0100__00_0000_0000_0000_0001".U(SZ_ALU_FN.W)
  override def FN_SH2ADDUW = "b00_000_0000_0000_1000__0100__00_0000_0000_0000_0001".U(SZ_ALU_FN.W)
  override def FN_SH3ADD   = "b00_000_0000_0000_0000__1000__00_0000_0000_0000_0001".U(SZ_ALU_FN.W)
  override def FN_SH3ADDUW = "b00_000_0000_0000_1000__1000__00_0000_0000_0000_0001".U(SZ_ALU_FN.W)
  // Zbb
  override def FN_ROR      = "b00_000_0000_0010_0000__0001__00_0000_0000_0000_0010".U(SZ_ALU_FN.W)
  override def FN_ROL      = "b00_000_0000_1110_0000__0001__00_0000_0000_0000_0010".U(SZ_ALU_FN.W)
  override def FN_ANDN     = "b00_000_0000_0000_0010__0001__00_0000_0000_0000_0100".U(SZ_ALU_FN.W)
  override def FN_ORN      = "b00_000_0000_0000_0010__0001__00_0000_0000_0001_0000".U(SZ_ALU_FN.W)
  override def FN_XNOR     = "b00_000_0000_0000_0010__0001__00_0000_0000_0000_1000".U(SZ_ALU_FN.W)
  override def FN_REV8     = "b00_000_0000_0000_0000__0001__00_0001_0000_0000_0000".U(SZ_ALU_FN.W)
  override def FN_ORCB     = "b10_000_0000_0000_0000__0001__00_0010_0000_0000_0000".U(SZ_ALU_FN.W)
  override def FN_SEXTB    = "b00_000_0000_0000_0000__0001__00_0000_1000_0000_0000".U(SZ_ALU_FN.W)
  override def FN_SEXTH    = "b01_000_0000_0000_0000__0001__00_0000_0100_0000_0000".U(SZ_ALU_FN.W)
  override def FN_ZEXTH    = "b00_000_0000_0000_0000__0001__00_0000_0100_0000_0000".U(SZ_ALU_FN.W)
  override def FN_MAX      = "b00_000_0000_0000_0011__0001__00_0000_0000_1000_0000".U(SZ_ALU_FN.W)
  override def FN_MAXU     = "b00_001_0000_0000_0011__0001__00_0000_0000_1000_0000".U(SZ_ALU_FN.W)
  override def FN_MIN      = "b00_010_0000_0000_0011__0001__00_0000_0000_1000_0000".U(SZ_ALU_FN.W)
  override def FN_MINU     = "b00_011_0000_0000_0011__0001__00_0000_0000_1000_0000".U(SZ_ALU_FN.W)
  override def FN_CPOP     = "b00_000_0000_0000_0000__0001__00_0000_0001_0000_0000".U(SZ_ALU_FN.W)
  override def FN_CLZ      = "b00_000_1101_1110_0000__0001__00_0000_0010_0000_0000".U(SZ_ALU_FN.W)
  override def FN_CTZ      = "b00_000_1101_0000_0000__0001__00_0000_0010_0000_0000".U(SZ_ALU_FN.W)
  // Zbs
  override def FN_BCLR     = "b00_000_1110_1000_0100__0001__00_0000_0000_0000_0100".U(SZ_ALU_FN.W)
  override def FN_BEXT     = "b00_000_1000_1000_0100__0001__00_0000_0000_0010_0000".U(SZ_ALU_FN.W)
  override def FN_BINV     = "b00_000_1000_1000_0100__0001__00_0000_0000_0000_1000".U(SZ_ALU_FN.W)
  override def FN_BSET     = "b00_000_1000_1000_0100__0001__00_0000_0000_0001_0000".U(SZ_ALU_FN.W)
  // Zbk
  override def FN_BREV8    = "b00_000_0000_0000_0000__0001__00_0010_0000_0000_0000".U(SZ_ALU_FN.W)
  override def FN_PACK     = "b00_000_0000_0000_0000__0001__00_0100_0000_0000_0000".U(SZ_ALU_FN.W)
  override def FN_PACKH    = "b00_000_0000_0000_0000__0001__00_1000_0000_0000_0000".U(SZ_ALU_FN.W)
  override def FN_ZIP      = "b00_000_0000_0000_0000__0001__01_0000_0000_0000_0000".U(SZ_ALU_FN.W)
  override def FN_UNZIP    = "b00_000_0000_0000_0000__0001__10_0000_0000_0000_0000".U(SZ_ALU_FN.W)
  def SZ_ZBK_FN = 5
  override def FN_CLMUL    = "b00001".U(SZ_ZBK_FN.W) // These use the BitManipCrypto FU, not ABLU
  override def FN_CLMULR   = "b00010".U(SZ_ZBK_FN.W)
  override def FN_CLMULH   = "b00100".U(SZ_ZBK_FN.W)
  override def FN_XPERM8   = "b01000".U(SZ_ZBK_FN.W)
  override def FN_XPERM4   = "b10000".U(SZ_ZBK_FN.W)
  // Zkn: These use the CryptoNIST unit
  def SZ_ZKN_FN = 17
  override def FN_AES_DS      = "b0010__0001__0_0000_0001".U(SZ_ZKN_FN.W)
  override def FN_AES_DSM     = "b0000__0010__0_0000_0001".U(SZ_ZKN_FN.W)
  override def FN_AES_ES      = "b0011__0001__0_0000_0001".U(SZ_ZKN_FN.W)
  override def FN_AES_ESM     = "b0001__0010__0_0000_0001".U(SZ_ZKN_FN.W)
  override def FN_AES_IM      = "b1000__0010__0_0000_0001".U(SZ_ZKN_FN.W)
  override def FN_AES_KS1     = "b0101__0100__0_0000_0001".U(SZ_ZKN_FN.W)
  override def FN_AES_KS2     = "b0000__1000__0_0000_0001".U(SZ_ZKN_FN.W)
  override def FN_SHA256_SIG0 = "b0000__0001__0_0000_0010".U(SZ_ZKN_FN.W)
  override def FN_SHA256_SIG1 = "b0000__0001__0_0000_0100".U(SZ_ZKN_FN.W)
  override def FN_SHA256_SUM0 = "b0000__0001__0_0000_1000".U(SZ_ZKN_FN.W)
  override def FN_SHA256_SUM1 = "b0000__0001__0_0001_0000".U(SZ_ZKN_FN.W)
  override def FN_SHA512_SIG0 = "b0000__0001__0_0010_0000".U(SZ_ZKN_FN.W)
  override def FN_SHA512_SIG1 = "b0000__0001__0_0100_0000".U(SZ_ZKN_FN.W)
  override def FN_SHA512_SUM0 = "b0000__0001__0_1000_0000".U(SZ_ZKN_FN.W)
  override def FN_SHA512_SUM1 = "b0000__0001__1_0000_0000".U(SZ_ZKN_FN.W)
  // Zks: Thses use the CryptoSM unit
  def SZ_ZKS_FN = 4
  override def FN_SM4ED = "b01_01".U(SZ_ZKS_FN.W)
  override def FN_SM4KS = "b00_01".U(SZ_ZKS_FN.W)
  override def FN_SM3P0 = "b10_10".U(SZ_ZKS_FN.W)
  override def FN_SM3P1 = "b00_10".U(SZ_ZKS_FN.W)


  override def FN_DIV  = FN_XOR
  override def FN_DIVU = FN_SR
  override def FN_REM  = FN_OR
  override def FN_REMU = FN_AND

  override def FN_MUL    = FN_ADD
  override def FN_MULH   = FN_SL
  override def FN_MULHSU = FN_SEQ
  override def FN_MULHU  = FN_SNE

  // not implemented functions for this fn
  override def isMulFN(fn: UInt, cmp: UInt) = ???
  override def isCmp(cmd: UInt) = ???
  override def cmpUnsigned(cmd: UInt) = ???
  override def cmpInverted(cmd: UInt) = ???
  override def cmpEq(cmd: UInt) = ???

  override def isSub(cmd: UInt) = cmd(22)
  def isIn2Inv(cmd: UInt) = cmd(23)
  def isZBS(cmd: UInt) = cmd(24)
  def isUW(cmd: UInt) = cmd(25)
  def isSRA(cmd: UInt) = cmd(26)
  def isRotate(cmd: UInt) = cmd(27)
  def isLeft(cmd: UInt) = cmd(28)
  def isLeftZBS(cmd: UInt) = cmd(29)
  def isCZ(cmd: UInt) = cmd(30)
  def isBCLR(cmd: UInt) = cmd(31)
  def isCZBCLR(cmd: UInt) = cmd(32)
  def isCZZBS(cmd: UInt) = cmd(33)
  def isUnsigned(cmd: UInt) = cmd(34)
  def isInverted(cmd: UInt) = cmd(35)
  def isSEQSNE(cmd: UInt) = cmd(36)
  def isSEXT(cmd: UInt) = cmd(37)
  def isORC(cmd: UInt) = cmd(38)
  def shxadd1H(cmd: UInt) = cmd(21,18)
  def out1H(cmd: UInt) = cmd(17,0)
  // Zbk
  def isClmul(cmd: UInt) = cmd(0)
  def zbkOut1H(cmd: UInt) = cmd(4,0)
  // Zkn
  def isEnc(cmd: UInt) = cmd(13)
  def isNotMix(cmd: UInt) = cmd(14)
  override def isKs1(cmd: UInt) = cmd(15)
  def isIm(cmd: UInt) = cmd(16)
  def aes1H(cmd: UInt) = cmd(12,9)
  def zknOut1H(cmd: UInt) = cmd(8,0)
  // Zks
  def isEd(cmd: UInt) = cmd(2)
  def isP0(cmd: UInt) = cmd(3)
  def zksOut1H(cmd: UInt) = cmd(1,0)
}

object ABLUFN {
  def apply() = new ABLUFN
}

class ABLU(implicit p: Parameters) extends AbstractALU(new ABLUFN)(p) {
  val isSub = aluFn.isSub(io.fn)
  val isIn2Inv = aluFn.isIn2Inv(io.fn)
  val isZBS = aluFn.isZBS(io.fn)
  val isUW = aluFn.isUW(io.fn)
  val isSRA = aluFn.isSRA(io.fn)
  val isRotate = aluFn.isRotate(io.fn)
  val isLeft = aluFn.isLeft(io.fn)
  val isLeftZBS = aluFn.isLeftZBS(io.fn)
  val isCZ = aluFn.isCZ(io.fn)
  val isBCLR = aluFn.isBCLR(io.fn)
  val isCZBCLR = aluFn.isCZBCLR(io.fn)
  val isCZZBS = aluFn.isCZZBS(io.fn)
  val isUnsigned = aluFn.isUnsigned(io.fn)
  val isInverted = aluFn.isInverted(io.fn)
  val isSEQSNE = aluFn.isSEQSNE(io.fn)
  val isSEXT = aluFn.isSEXT(io.fn)
  val isORC = aluFn.isORC(io.fn)
  val shxadd1H = aluFn.shxadd1H(io.fn)
  val out1H = aluFn.out1H(io.fn)

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
  // TODO: Merge shift and rotate (manual barrel or upstream to Chisel)
  val shout_r = (Cat(isSRA & shin(xLen-1), shin).asSInt >> shamt)(xLen-1,0)
  val roout_r = shin.rotateRight(shamt)(xLen-1,0)
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
  // adder_out = adder_in1 + adder_in2 + isSub
  val adder_out = (Cat(adder_in1, 1.U(1.W)) + Cat(adder_in2, isSub))(xLen,1)
  io.adder_out := adder_out

  // logic
  // AND, OR, XOR
  // ANDN, ORN, XNOR
  // BCLR, BEXT, BINV, BSET
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
  val cmp = isInverted ^ Mux(isSEQSNE, ~(xor.orR), slt)
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
  val orc_brev8 = VecInit(in1_bytes.map(x => {
    val orc = Mux(x.orR, 0xFF.U(8.W), 0.U(8.W))
    // BREV8 only in Zbk
    if (usingBitManipCrypto)
      Mux(isORC, orc, Reverse(x))
    else orc
  }).toSeq).asUInt

  // pack
  def sext(in: UInt): UInt = {
    val in_hi_32 = Fill(32, in(31))
    Cat(in_hi_32, in)
  }
  val pack = if (usingBitManipCrypto) {
    if (xLen == 32) Cat(io.in2(xLen/2-1,0), io.in1(xLen/2-1,0))
    else {
      require(xLen == 64)
      Mux(io.dw === DW_64,
        Cat(io.in2(xLen/2-1,0), io.in1(xLen/2-1,0)),
        sext(Cat(io.in2(xLen/4-1,0), io.in1(xLen/4-1,0))))
    }
  } else 0.U
  val packh = if (usingBitManipCrypto) Cat(0.U((xLen-16).W), io.in2(7,0), io.in1(7,0)) else 0.U

  // zip
  val zip = if (xLen == 32 && usingBitManipCrypto) {
    val lo = io.in1(15,0).asBools
    val hi = io.in1(31,16).asBools
    VecInit(lo.zip(hi).map { case (l, h) => VecInit(Seq(l, h)).asUInt }).asUInt
  } else 0.U
  val unzip = if (xLen == 32 && usingBitManipCrypto) {
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
