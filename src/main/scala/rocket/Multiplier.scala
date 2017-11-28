// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket

import chisel3._
import chisel3.util.ImplicitConversions._
import chisel3.util._
import ALU._

class MultiplierReq(dataBits: Int, tagBits: Int) extends Bundle {
  val fn = UInt(SZ_ALU_FN.W)
  val dw = UInt(SZ_DW.W)
  val in1 = UInt(dataBits.W)
  val in2 = UInt(dataBits.W)
  val tag = UInt(tagBits.W)
  override def cloneType = new MultiplierReq(dataBits, tagBits).asInstanceOf[this.type]
}

class MultiplierResp(dataBits: Int, tagBits: Int) extends Bundle {
  val data = UInt(dataBits.W)
  val tag = UInt(tagBits.W)
  override def cloneType = new MultiplierResp(dataBits, tagBits).asInstanceOf[this.type]
}

class MultiplierIO(dataBits: Int, tagBits: Int) extends Bundle {
  val req = Flipped(Decoupled(new MultiplierReq(dataBits, tagBits)))
  val kill = Input(Bool())
  val resp = Decoupled(new MultiplierResp(dataBits, tagBits))
}

case class MulDivParams(
  mulUnroll: Int = 1,
  divUnroll: Int = 1,
  mulEarlyOut: Boolean = false,
  divEarlyOut: Boolean = false
)

class MulDiv(cfg: MulDivParams, width: Int, nXpr: Int = 32) extends Module {
  val io = IO(new MultiplierIO(width, log2Ceil(nXpr)))
  val w = io.req.bits.in1.getWidth
  val mulw = (w + cfg.mulUnroll - 1) / cfg.mulUnroll * cfg.mulUnroll
  val fastMulW = w/2 > cfg.mulUnroll && w % (2*cfg.mulUnroll) == 0
 
  val s_ready :: s_neg_inputs :: s_mul :: s_div :: s_dummy :: s_neg_output :: s_done_mul :: s_done_div :: Nil = Enum(8)
  val state = RegInit(s_ready)
 
  val req = Reg(chiselTypeOf(io.req.bits))
  val count = Reg(UInt(log2Ceil((w/cfg.divUnroll + 1) max (w/cfg.mulUnroll)).W))
  val neg_out = Reg(Bool())
  val isHi = Reg(Bool())
  val resHi = Reg(Bool())
  val divisor = Reg(UInt((w+1).W)) // div only needs w bits
  val remainder = Reg(UInt((2*mulw+2).W)) // div only needs 2*w+1 bits

  val cmdMul :: cmdHi :: lhsSigned :: rhsSigned :: Nil =
    DecodeLogic(io.req.bits.fn, List(X, X, X, X), List(
                   FN_DIV    -> List(N, N, Y, Y),
                   FN_REM    -> List(N, Y, Y, Y),
                   FN_DIVU   -> List(N, N, N, N),
                   FN_REMU   -> List(N, Y, N, N),
                   FN_MUL    -> List(Y, N, X, X),
                   FN_MULH   -> List(Y, Y, Y, Y),
                   FN_MULHU  -> List(Y, Y, N, N),
                   FN_MULHSU -> List(Y, Y, Y, N))).map(_ toBool)

  require(w == 32 || w == 64)
  def halfWidth(req: MultiplierReq) = (w > 32).B && req.dw === DW_32

  def sext(x: Bits, halfW: Bool, signed: Bool) = {
    val sign = signed && Mux(halfW, x(w/2-1), x(w-1))
    val hi = Mux(halfW, Fill(w/2, sign), x(w-1,w/2))
    (Cat(hi, x(w/2-1,0)), sign)
  }
  val (lhs_in, lhs_sign) = sext(io.req.bits.in1, halfWidth(io.req.bits), lhsSigned)
  val (rhs_in, rhs_sign) = sext(io.req.bits.in2, halfWidth(io.req.bits), rhsSigned)
  
  val subtractor = remainder(2*w,w) - divisor
  val result = Mux(resHi, remainder(2*w, w+1), remainder(w-1, 0))
  val negated_remainder = -result

  when (state === s_neg_inputs) {
    when (remainder(w-1)) {
      remainder := negated_remainder
    }
    when (divisor(w-1)) {
      divisor := subtractor
    }
    state := s_div
  }
  when (state === s_neg_output) {
    remainder := negated_remainder
    state := s_done_div
    resHi := false
  }
  when (state === s_mul) {
    val mulReg = Cat(remainder(2*mulw+1,w+1),remainder(w-1,0))
    val mplierSign = remainder(w)
    val mplier = mulReg(mulw-1,0)
    val accum = mulReg(2*mulw,mulw).asSInt
    val mpcand = divisor.asSInt
    val prod = Cat(mplierSign, mplier(cfg.mulUnroll-1, 0)).asSInt * mpcand + accum
    val nextMulReg = Cat(prod, mplier(mulw-1, cfg.mulUnroll))
    val nextMplierSign = count === mulw/cfg.mulUnroll-2 && neg_out

    val eOutMask = (((BigInt(-1) << mulw).S >> (count * cfg.mulUnroll)(log2Ceil(mulw)-1,0))(mulw-1,0))
    val eOut = (cfg.mulEarlyOut).B && count =/= mulw/cfg.mulUnroll-1 && count =/= 0 &&
      !isHi && ((mplier & ~eOutMask) === 0.U)
    val eOutRes = (mulReg >> (mulw - count * cfg.mulUnroll)(log2Ceil(mulw)-1,0))
    val nextMulReg1 = Cat(nextMulReg(2*mulw,mulw), Mux(eOut, eOutRes, nextMulReg)(mulw-1,0))
    remainder := Cat(nextMulReg1 >> w, nextMplierSign, nextMulReg1(w-1,0))

    count := count + 1
    when (eOut || count === mulw/cfg.mulUnroll-1) {
      state := s_done_mul
      resHi := isHi
    }
  }
  when (state === s_div) {
    val unrolls = ((0 until cfg.divUnroll) scanLeft remainder) { case (rem, i) =>
      // the special case for iteration 0 is to save HW, not for correctness
      val difference = if (i == 0) subtractor else rem(2*w,w) - divisor(w-1,0)
      val less = difference(w)
      Cat(Mux(less, rem(2*w-1,w), difference(w-1,0)), rem(w-1,0), !less)
    } tail

    remainder := unrolls.last
    when (count === w/cfg.divUnroll) {
      state := Mux(neg_out, s_neg_output, s_done_div)
      resHi := isHi
      if (w % cfg.divUnroll < cfg.divUnroll - 1)
        remainder := unrolls(w % cfg.divUnroll)
    }
    count := count + 1

    val divby0 = count === 0 && !subtractor(w)
    if (cfg.divEarlyOut) {
      val divisorMSB = Log2(divisor(w-1,0), w)
      val dividendMSB = Log2(remainder(w-1,0), w)
      val eOutPos = (w-1).U + divisorMSB - dividendMSB
      val eOutZero = divisorMSB > dividendMSB
      val eOut = count === 0 && !divby0 && (eOutPos >= cfg.divUnroll || eOutZero)
      when (eOut) {
        val inc = Mux(eOutZero, (w-1).U, eOutPos) >> log2Floor(cfg.divUnroll)
        val shift = (inc << log2Floor(cfg.divUnroll))
        remainder := remainder(w-1,0) << shift
        count := inc
      }
    }
    when (divby0 && !isHi) { neg_out := false }
  }
  when (io.resp.fire() || io.kill) {
    state := s_ready
  }
  when (io.req.fire()) {
    state := Mux(cmdMul, s_mul, Mux(lhs_sign || rhs_sign, s_neg_inputs, s_div))
    isHi := cmdHi
    resHi := false
    count := Mux[UInt](fastMulW.B && cmdMul && halfWidth(io.req.bits), w/cfg.mulUnroll/2, 0)
    neg_out := Mux(cmdHi, lhs_sign, lhs_sign =/= rhs_sign)
    divisor := Cat(rhs_sign, rhs_in)
    remainder := lhs_in
    req := io.req.bits
  }

  val outMul = (state & (s_done_mul ^ s_done_div)) === (s_done_mul & ~s_done_div)
  val loOut = Mux(fastMulW.B && halfWidth(req) && outMul, result(w-1,w/2), result(w/2-1,0))
  val hiOut = Mux(halfWidth(req), Fill(w/2, loOut(w/2-1)), result(w-1,w/2))
  io.resp.bits.tag := req.tag
  io.resp.bits.data := Cat(hiOut, loOut)
  io.resp.valid := (state === s_done_mul || state === s_done_div)
  io.req.ready := state === s_ready
}
