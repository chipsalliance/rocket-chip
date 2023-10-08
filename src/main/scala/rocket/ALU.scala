// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.rocket

import chisel3._
import chisel3.util.{BitPat, Fill, Cat, Reverse}
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tile.CoreModule

class ALUFN {
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
  def FN_CZEQZ = 8.U
  def FN_CZNEZ = 9.U
  def FN_SUB  = 10.U
  def FN_SRA  = 11.U
  def FN_SLT  = 12.U
  def FN_SGE  = 13.U
  def FN_SLTU = 14.U
  def FN_SGEU = 15.U

  // The Base ALU does not support any Zb FNs
  // from Zb
  // Zba: UW is encoded here becuase it is DW_64
  def FN_ADDUW    : UInt = ???
  def FN_SLLIUW   : UInt = ???
  def FN_SH1ADD   : UInt = ???
  def FN_SH1ADDUW : UInt = ???
  def FN_SH2ADD   : UInt = ???
  def FN_SH2ADDUW : UInt = ???
  def FN_SH3ADD   : UInt = ???
  def FN_SH3ADDUW : UInt = ???
  // Zbb
  def FN_ROR      : UInt = ???
  def FN_ROL      : UInt = ???
  def FN_ANDN     : UInt = ???
  def FN_ORN      : UInt = ???
  def FN_XNOR     : UInt = ???
  def FN_REV8     : UInt = ???
  def FN_ORCB     : UInt = ???
  def FN_SEXTB    : UInt = ???
  def FN_SEXTH    : UInt = ???
  def FN_ZEXTH    : UInt = ???
  def FN_MAX      : UInt = ???
  def FN_MAXU     : UInt = ???
  def FN_MIN      : UInt = ???
  def FN_MINU     : UInt = ???
  def FN_CPOP     : UInt = ???
  def FN_CLZ      : UInt = ???
  def FN_CTZ      : UInt = ???
  // Zbs
  def FN_BCLR     : UInt = ???
  def FN_BEXT     : UInt = ???
  def FN_BINV     : UInt = ???
  def FN_BSET     : UInt = ???
  // Zbk
  def FN_BREV8    : UInt = ???
  def FN_PACK     : UInt = ???
  def FN_PACKH    : UInt = ???
  def FN_ZIP      : UInt = ???
  def FN_UNZIP    : UInt = ???
  def FN_CLMUL    : UInt = ???
  def FN_CLMULR   : UInt = ???
  def FN_CLMULH   : UInt = ???
  def FN_XPERM8   : UInt = ???
  def FN_XPERM4   : UInt = ???
  // Zkn
  def FN_AES_DS      : UInt = ???
  def FN_AES_DSM     : UInt = ???
  def FN_AES_ES      : UInt = ???
  def FN_AES_ESM     : UInt = ???
  def FN_AES_IM      : UInt = ???
  def FN_AES_KS1     : UInt = ???
  def FN_AES_KS2     : UInt = ???
  def FN_SHA256_SIG0 : UInt = ???
  def FN_SHA256_SIG1 : UInt = ???
  def FN_SHA256_SUM0 : UInt = ???
  def FN_SHA256_SUM1 : UInt = ???
  def FN_SHA512_SIG0 : UInt = ???
  def FN_SHA512_SIG1 : UInt = ???
  def FN_SHA512_SUM0 : UInt = ???
  def FN_SHA512_SUM1 : UInt = ???
  //Zks
  def FN_SM4ED : UInt = ???
  def FN_SM4KS : UInt = ???
  def FN_SM3P0 : UInt = ???
  def FN_SM3P1 : UInt = ???

  // Mul/div reuse some integer FNs
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

  // Only CryptoNIST uses this
  def isKs1(cmd: UInt): Bool = ???
}

object ALUFN {
  def apply() = new ALUFN
}


abstract class AbstractALU[T <: ALUFN](val aluFn: T)(implicit p: Parameters) extends CoreModule()(p) {
  val io = IO(new Bundle {
    val dw = Input(UInt(SZ_DW.W))
    val fn = Input(UInt(aluFn.SZ_ALU_FN.W))
    val in2 = Input(UInt(xLen.W))
    val in1 = Input(UInt(xLen.W))
    val out = Output(UInt(xLen.W))
    val adder_out = Output(UInt(xLen.W))
    val cmp_out = Output(Bool())
  })
}

class ALU(implicit p: Parameters) extends AbstractALU(new ALUFN)(p) {
  // ADD, SUB
  val in2_inv = Mux(aluFn.isSub(io.fn), ~io.in2, io.in2)
  val in1_xor_in2 = io.in1 ^ in2_inv
  io.adder_out := io.in1 + in2_inv + aluFn.isSub(io.fn)

  // SLT, SLTU
  val slt =
    Mux(io.in1(xLen-1) === io.in2(xLen-1), io.adder_out(xLen-1),
    Mux(aluFn.cmpUnsigned(io.fn), io.in2(xLen-1), io.in1(xLen-1)))
  io.cmp_out := aluFn.cmpInverted(io.fn) ^ Mux(aluFn.cmpEq(io.fn), in1_xor_in2 === 0.U, slt)

  // SLL, SRL, SRA
  val (shamt, shin_r) =
    if (xLen == 32) (io.in2(4,0), io.in1)
    else {
      require(xLen == 64)
      val shin_hi_32 = Fill(32, aluFn.isSub(io.fn) && io.in1(31))
      val shin_hi = Mux(io.dw === DW_64, io.in1(63,32), shin_hi_32)
      val shamt = Cat(io.in2(5) & (io.dw === DW_64), io.in2(4,0))
      (shamt, Cat(shin_hi, io.in1(31,0)))
    }
  val shin = Mux(io.fn === aluFn.FN_SR  || io.fn === aluFn.FN_SRA, shin_r, Reverse(shin_r))
  val shout_r = (Cat(aluFn.isSub(io.fn) & shin(xLen-1), shin).asSInt >> shamt)(xLen-1,0)
  val shout_l = Reverse(shout_r)
  val shout = Mux(io.fn === aluFn.FN_SR || io.fn === aluFn.FN_SRA, shout_r, 0.U) |
              Mux(io.fn === aluFn.FN_SL,                           shout_l, 0.U)

  // CZEQZ, CZNEZ
  val in2_not_zero = io.in2.orR
  val cond_out = Option.when(usingConditionalZero)(
    Mux((io.fn === aluFn.FN_CZEQZ && in2_not_zero) || (io.fn === aluFn.FN_CZNEZ && !in2_not_zero), io.in1, 0.U)
  )

  // AND, OR, XOR
  val logic = Mux(io.fn === aluFn.FN_XOR || io.fn === aluFn.FN_OR, in1_xor_in2, 0.U) |
              Mux(io.fn === aluFn.FN_OR || io.fn === aluFn.FN_AND, io.in1 & io.in2, 0.U)

  val shift_logic = (aluFn.isCmp (io.fn) && slt) | logic | shout
  val shift_logic_cond = cond_out match {
    case Some(co) => shift_logic | co
    case _ => shift_logic
  }
  val out = Mux(io.fn === aluFn.FN_ADD || io.fn === aluFn.FN_SUB, io.adder_out, shift_logic_cond)

  io.out := out
  if (xLen > 32) {
    require(xLen == 64)
    when (io.dw === DW_32) { io.out := Cat(Fill(32, out(31)), out(31,0)) }
  }
}
