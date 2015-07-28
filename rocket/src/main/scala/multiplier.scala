// See LICENSE for license details.

package rocket

import Chisel._
import ALU._
import Util._

class MultiplierReq extends CoreBundle {
  val fn = Bits(width = SZ_ALU_FN)
  val dw = Bits(width = SZ_DW)
  val in1 = Bits(width = xLen)
  val in2 = Bits(width = xLen)
  val tag = UInt(width = log2Up(params(NMultXpr)))
}

class MultiplierResp extends CoreBundle {
  val data = Bits(width = xLen)
  val tag = UInt(width = log2Up(params(NMultXpr)))
}

class MultiplierIO extends Bundle {
  val req = Decoupled(new MultiplierReq).flip
  val kill = Bool(INPUT)
  val resp = Decoupled(new MultiplierResp)
}

class MulDiv(mulUnroll: Int = 1, earlyOut: Boolean = false) extends Module {
  val io = new MultiplierIO
  val w = io.req.bits.in1.getWidth
  val mulw = (w+mulUnroll-1)/mulUnroll*mulUnroll
 
  val s_ready :: s_neg_inputs :: s_busy :: s_move_rem :: s_neg_output :: s_done :: Nil = Enum(UInt(), 6)
  val state = Reg(init=s_ready)
 
  val req = Reg(io.req.bits)
  val count = Reg(UInt(width = log2Up(w+1)))
  val neg_out = Reg(Bool())
  val isMul = Reg(Bool())
  val isHi = Reg(Bool())
  val divisor = Reg(Bits(width = w+1)) // div only needs w bits
  val remainder = Reg(Bits(width = 2*mulw+2)) // div only needs 2*w+1 bits

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

  def sext(x: Bits, signed: Bool) = {
    val sign = signed && Mux(io.req.bits.dw === DW_64, x(w-1), x(w/2-1))
    val hi = Mux(io.req.bits.dw === DW_64, x(w-1,w/2), Fill(w/2, sign))
    (Cat(hi, x(w/2-1,0)), sign)
  }
  val (lhs_in, lhs_sign) = sext(io.req.bits.in1, lhsSigned)
  val (rhs_in, rhs_sign) = sext(io.req.bits.in2, rhsSigned)
  
  val subtractor = remainder(2*w,w) - divisor(w,0)
  val less = subtractor(w)
  val negated_remainder = -remainder(w-1,0)

  when (state === s_neg_inputs) {
    when (remainder(w-1) || isMul) {
      remainder := negated_remainder
    }
    when (divisor(w-1) || isMul) {
      divisor := subtractor
    }
    state := s_busy
  }

  when (state === s_neg_output) {
    remainder := negated_remainder
    state := s_done
  }
  when (state === s_move_rem) {
    remainder := remainder(2*w, w+1)
    state := Mux(neg_out, s_neg_output, s_done)
  }
  when (state === s_busy && isMul) {
    val mulReg = Cat(remainder(2*mulw+1,w+1),remainder(w-1,0))
    val mplier = mulReg(mulw-1,0)
    val accum = mulReg(2*mulw,mulw).toSInt
    val mpcand = divisor.toSInt
    val prod = mplier(mulUnroll-1,0) * mpcand + accum
    val nextMulReg = Cat(prod, mplier(mulw-1,mulUnroll)).toUInt

    val eOutMask = (SInt(BigInt(-1) << mulw) >> (count * mulUnroll)(log2Up(mulw)-1,0))(mulw-1,0)
    val eOut = Bool(earlyOut) && count != mulw/mulUnroll-1 && count != 0 &&
      !isHi && (mplier & ~eOutMask) === UInt(0)
    val eOutRes = (mulReg >> (mulw - count * mulUnroll)(log2Up(mulw)-1,0))
    val nextMulReg1 = Cat(nextMulReg(2*mulw,mulw), Mux(eOut, eOutRes, nextMulReg)(mulw-1,0))
    remainder := Cat(nextMulReg1 >> w, Bool(false), nextMulReg1(w-1,0)).toSInt

    count := count + 1
    when (eOut || count === mulw/mulUnroll-1) {
      state := Mux(isHi, s_move_rem, s_done)
    }
  }
  when (state === s_busy && !isMul) {
    when (count === w) {
      state := Mux(isHi, s_move_rem, Mux(neg_out, s_neg_output, s_done))
    }
    count := count + 1

    remainder := Cat(Mux(less, remainder(2*w-1,w), subtractor(w-1,0)), remainder(w-1,0), !less)

    val divisorMSB = Log2(divisor(w-1,0), w)
    val dividendMSB = Log2(remainder(w-1,0), w)
    val eOutPos = UInt(w-1) + divisorMSB - dividendMSB
    val eOutZero = divisorMSB > dividendMSB
    val eOut = count === 0 && less /* not divby0 */ && (eOutPos > 0 || eOutZero)
    when (Bool(earlyOut) && eOut) {
      val shift = Mux(eOutZero, UInt(w-1), eOutPos(log2Up(w)-1,0))
      remainder := remainder(w-1,0) << shift
      count := shift
    }
    when (count === 0 && !less /* divby0 */ && !isHi) { neg_out := false }
  }
  when (io.resp.fire() || io.kill) {
    state := s_ready
  }
  when (io.req.fire()) {
    state := Mux(lhs_sign || rhs_sign && !cmdMul, s_neg_inputs, s_busy)
    isMul := cmdMul
    isHi := cmdHi
    count := 0
    neg_out := !cmdMul && Mux(cmdHi, lhs_sign, lhs_sign != rhs_sign)
    divisor := Cat(rhs_sign, rhs_in)
    remainder := lhs_in
    req := io.req.bits
  }

  io.resp.bits := req
  io.resp.bits.data := Mux(req.dw === DW_32, Cat(Fill(w/2, remainder(w/2-1)), remainder(w/2-1,0)), remainder(w-1,0))
  io.resp.valid := state === s_done
  io.req.ready := state === s_ready
}
