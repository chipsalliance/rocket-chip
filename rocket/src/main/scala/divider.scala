package rocket

import Chisel._
import ALU._
import Util._

class MulDiv(mulUnroll: Int = 1, earlyOut: Boolean = false)(implicit conf: RocketConfiguration) extends Module {
  val io = new MultiplierIO
  val w = io.req.bits.in1.getWidth
  val mulw = (w+mulUnroll-1)/mulUnroll*mulUnroll
  
  val s_ready :: s_neg_inputs :: s_mul_busy :: s_div_busy :: s_move_rem :: s_neg_output :: s_done :: Nil = Enum(7) { UInt() };
  val state = Reg(init=s_ready)
  
  val req = Reg(io.req.bits.clone)
  val count = Reg(UInt(width = log2Up(w+1)))
  val divby0 = Reg(Bool())
  val neg_out = Reg(Bool())
  val divisor = Reg(Bits(width = w+1)) // div only needs w bits
  val remainder = Reg(Bits(width = 2*mulw+2)) // div only needs 2*w+1 bits

  def sext(x: Bits, cmds: Vec[Bits]) = {
    val sign = Mux(io.req.bits.dw === DW_64, x(w-1), x(w/2-1)) && cmds.contains(io.req.bits.fn)
    val hi = Mux(io.req.bits.dw === DW_64, x(w-1,w/2), Fill(w/2, sign))
    (Cat(hi, x(w/2-1,0)), sign)
  }
  val (lhs_in, lhs_sign) = sext(io.req.bits.in1, AVec(FN_DIV, FN_REM, FN_MULH, FN_MULHSU))
  val (rhs_in, rhs_sign) = sext(io.req.bits.in2, AVec(FN_DIV, FN_REM, FN_MULH))
        
  val subtractor = remainder(2*w,w) - divisor(w,0)
  val negated_remainder = -remainder(w-1,0)

  when (state === s_neg_inputs) {
    val isMul = AVec(FN_MUL, FN_MULH, FN_MULHU, FN_MULHSU).contains(req.fn)
    state := Mux(isMul, s_mul_busy, s_div_busy)
    when (remainder(w-1) || isMul) {
      remainder := negated_remainder
    }
    when (divisor(w-1) || isMul) {
      divisor := subtractor
    }
  }
  when (state === s_neg_output) {
    remainder := negated_remainder
    state := s_done
  }
  when (state === s_move_rem) {
    remainder := remainder(2*w, w+1)
    state := Mux(neg_out, s_neg_output, s_done)
  }
  when (state === s_mul_busy) {
    val mulReg = Cat(remainder(2*mulw+1,w+1),remainder(w-1,0))
    val mplier = mulReg(mulw-1,0)
    val accum = mulReg(2*mulw,mulw).toSInt
    val mpcand = divisor.toSInt
    val prod = mplier(mulUnroll-1,0) * mpcand + accum
    val eOut = Bool(earlyOut) && count > 0 &&
      (0 until mulw/mulUnroll).map(i => i > mulw/mulUnroll-1-count || mplier((i+1)*mulUnroll-1,i*mulUnroll) === 0).reduce(_&&_)
    val eOutValue = mulReg >> (mulw/mulUnroll-count)(log2Up(mulw/mulUnroll)-1,0)*mulUnroll
    val nextMulReg = Mux(eOut, eOutValue, Cat(prod, mplier(mulw-1,mulUnroll)))
    remainder := Cat(nextMulReg >> w, Bool(false), nextMulReg(w-1,0)).toSInt

    count := count + 1
    when (count === mulw/mulUnroll-1 || eOut) {
      state := s_done
      when (AVec(FN_MULH, FN_MULHU, FN_MULHSU) contains req.fn) {
        state := s_move_rem
      }
    }
  }
  when (state === s_div_busy) {
    when (count === UInt(w)) {
      state := Mux(neg_out && !divby0, s_neg_output, s_done)
      when (AVec(FN_REM, FN_REMU) contains req.fn) {
        state := s_move_rem
      }
    }
    count := count + UInt(1)

    val msb = subtractor(w)
    divby0 := divby0 && !msb
    remainder := Cat(Mux(msb, remainder(2*w-1,w), subtractor(w-1,0)), remainder(w-1,0), !msb)

    val divisorMSB = Log2(divisor(w-1,0), w)
    val dividendMSB = Log2(remainder(w-1,0), w)
    val eOutPos = UInt(w-1, log2Up(2*w)) + divisorMSB - dividendMSB
    val eOut = count === UInt(0) && eOutPos > 0 && (divisorMSB != UInt(0) || divisor(0))
    when (Bool(earlyOut) && eOut) {
      val shift = eOutPos(log2Up(w)-1,0)
      remainder := remainder(w-1,0) << shift
      count := shift
      when (eOutPos(log2Up(w))) {
        remainder := remainder(w-1,0) << w-1
        count := w-1
      }
    }
  }
  when (io.resp.fire() || io.kill) {
    state := s_ready
  }
  when (io.req.fire()) {
    val isMul = AVec(FN_MUL, FN_MULH, FN_MULHU, FN_MULHSU).contains(io.req.bits.fn)
    val isRem = AVec(FN_REM, FN_REMU).contains(io.req.bits.fn)
    val mulState = Mux(lhs_sign, s_neg_inputs, s_mul_busy)
    val divState = Mux(lhs_sign || rhs_sign, s_neg_inputs, s_div_busy)
    state := Mux(isMul, mulState, divState)
    count := UInt(0)
    neg_out := !isMul && Mux(isRem, lhs_sign, lhs_sign != rhs_sign)
    divby0 := true
    divisor := Cat(rhs_sign, rhs_in)
    remainder := lhs_in
    req := io.req.bits
  }

  io.resp.bits := req
  io.resp.bits.data := Mux(req.dw === DW_32, Cat(Fill(w/2, remainder(w/2-1)), remainder(w/2-1,0)), remainder(w-1,0))
  io.resp.valid := state === s_done
  io.req.ready := state === s_ready
}

class Divider(earlyOut: Boolean = false)(implicit conf: RocketConfiguration) extends Module {
  val io = new MultiplierIO
  val w = io.req.bits.in1.getWidth
  
  val s_ready :: s_neg_inputs :: s_busy :: s_move_rem :: s_neg_output :: s_done :: Nil = Enum(6) { UInt() };
  val state = Reg(init=s_ready)
  
  val count = Reg(UInt(width = log2Up(w+1)))
  val divby0 = Reg(Bool())
  val neg_out = Reg(Bool())
  val r_req = Reg(io.req.bits)
  
  val divisor     = Reg(Bits())
  val remainder   = Reg(Bits(width = 2*w+1))
  val subtractor  = remainder(2*w,w) - divisor

  def sext(x: Bits, cmds: Vec[Bits]) = {
    val sign = Mux(io.req.bits.dw === DW_64, x(w-1), x(w/2-1)) && cmds.contains(io.req.bits.fn)
    val hi = Mux(io.req.bits.dw === DW_64, x(w-1,w/2), Fill(w/2, sign))
    (Cat(hi, x(w/2-1,0)), sign)
  }
  val (lhs_in, lhs_sign) = sext(io.req.bits.in1, AVec(FN_DIV, FN_REM))
  val (rhs_in, rhs_sign) = sext(io.req.bits.in2, AVec(FN_DIV, FN_REM))

  val r_isRem = isMulFN(r_req.fn, FN_REM) || isMulFN(r_req.fn, FN_REMU)
        
  when (state === s_neg_inputs) {
    state := s_busy
    when (remainder(w-1)) {
      remainder := -remainder(w-1,0)
    }
    when (divisor(w-1)) {
      divisor := subtractor(w-1,0)
    }
  }
  when (state === s_neg_output) {
    remainder := -remainder(w-1,0)
    state := s_done
  }
  when (state === s_move_rem) {
    remainder := remainder(2*w, w+1)
    state := Mux(neg_out, s_neg_output, s_done)
  }
  when (state === s_busy) {
    when (count === UInt(w)) {
      state := Mux(r_isRem, s_move_rem, Mux(neg_out && !divby0, s_neg_output, s_done))
    }
    count := count + UInt(1)

    val msb = subtractor(w)
    divby0 := divby0 && !msb
    remainder := Cat(Mux(msb, remainder(2*w-1,w), subtractor(w-1,0)), remainder(w-1,0), !msb)

    val divisorMSB = Log2(divisor, w)
    val dividendMSB = Log2(remainder(w-1,0), w)
    val eOutPos = UInt(w-1, log2Up(2*w)) + divisorMSB - dividendMSB
    val eOut = count === UInt(0) && eOutPos > 0 && (divisorMSB != UInt(0) || divisor(0))
    when (Bool(earlyOut) && eOut) {
      val shift = eOutPos(log2Up(w)-1,0)
      remainder := remainder(w-1,0) << shift
      count := shift
      when (eOutPos(log2Up(w))) {
        remainder := remainder(w-1,0) << w-1
        count := w-1
      }
    }
  }
  when (io.resp.fire() || io.kill) {
    state := s_ready
  }
  when (io.req.fire()) {
    state := Mux(lhs_sign || rhs_sign, s_neg_inputs, s_busy)
    count := UInt(0)
    neg_out := Mux(AVec(FN_REM, FN_REMU).contains(io.req.bits.fn), lhs_sign, lhs_sign != rhs_sign)
    divby0 := true
    divisor := rhs_in
    remainder := lhs_in
    r_req := io.req.bits
  }

  io.resp.bits := r_req
  io.resp.bits.data := Mux(r_req.dw === DW_32, Cat(Fill(w/2, remainder(w/2-1)), remainder(w/2-1,0)), remainder(w-1,0))
  io.resp.valid := state === s_done
  io.req.ready := state === s_ready
}
