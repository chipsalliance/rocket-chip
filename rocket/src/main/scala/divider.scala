package rocket

import Chisel._
import Node._
import Constants._
import ALU._

class Divider(earlyOut: Boolean = false)(implicit conf: RocketConfiguration) extends Component {
  val io = new MultiplierIO
  val w = io.req.bits.in1.getWidth
  
  val s_ready :: s_neg_inputs :: s_busy :: s_neg_outputs :: s_done :: Nil = Enum(5) { UFix() };
  val state = Reg(resetVal = s_ready);
  
  val count       = Reg() { UFix(width = log2Up(w+1)) }
  val divby0      = Reg() { Bool() };
  val neg_quo     = Reg() { Bool() };
  val neg_rem     = Reg() { Bool() };
  val rem         = Reg() { Bool() };
  val half        = Reg() { Bool() };
  val r_req = Reg{io.req.bits.clone}
  
  val divisor     = Reg() { Bits() }
  val remainder   = Reg() { Bits(width = 2*w+1) }
  val subtractor  = remainder(2*w,w) - divisor
 
  val dw = io.req.bits.dw
  val fn = io.req.bits.fn
  val tc = isMulFN(fn, FN_DIV) || isMulFN(fn, FN_REM)

  val lhs_sign = tc && Mux(dw === DW_64, io.req.bits.in1(w-1), io.req.bits.in1(w/2-1))
  val lhs_hi = Mux(dw === DW_64, io.req.bits.in1(w-1,w/2), Fill(w/2, lhs_sign))
  val lhs_in = Cat(lhs_hi, io.req.bits.in1(w/2-1,0))

  val rhs_sign = tc && Mux(dw === DW_64, io.req.bits.in2(w-1), io.req.bits.in2(w/2-1))
  val rhs_hi = Mux(dw === DW_64, io.req.bits.in2(w-1,w/2), Fill(w/2, rhs_sign))
  val rhs_in = Cat(rhs_hi, io.req.bits.in2(w/2-1,0))
        
  when (state === s_neg_inputs) {
    state := s_busy
    when (remainder(w-1)) {
      remainder := Cat(remainder(2*w, w), -remainder(w-1,0))
    }
    when (divisor(w-1)) {
      divisor := subtractor(w-1,0)
    }
  }
  when (state === s_neg_outputs) {
    state := s_done
    when (neg_rem && neg_quo && !divby0) {
      remainder := Cat(-remainder(2*w, w+1), remainder(w), -remainder(w-1,0))
    }
    .elsewhen (neg_quo && !divby0) {
      remainder := Cat(remainder(2*w, w), -remainder(w-1,0))
    }
    .elsewhen (neg_rem) {
      remainder := Cat(-remainder(2*w, w+1), remainder(w,0))
    }
  }
  when (state === s_busy) {
    when (count === UFix(w)) {
      state := Mux(neg_quo || neg_rem, s_neg_outputs, s_done)
    }
    count := count + UFix(1)

    val msb = subtractor(w)
    divby0 := divby0 && !msb
    remainder := Cat(Mux(msb, remainder(2*w-1,w), subtractor(w-1,0)), remainder(w-1,0), !msb)

    val divisorMSB = Log2(divisor, w)
    val dividendMSB = Log2(remainder(w-1,0), w)
    val eOutPos = UFix(w-1, log2Up(2*w)) + divisorMSB
    val eOut = count === UFix(0) && eOutPos > dividendMSB && (divisorMSB != UFix(0) || divisor(0))
    when (Bool(earlyOut) && eOut) {
      val eOutDist = eOutPos - dividendMSB
      val shift = Mux(divisorMSB >= dividendMSB, UFix(w-1), eOutDist(log2Up(w)-1,0))
      remainder := remainder(w-1,0) << shift
      count := shift
    }
  }
  when (io.resp.fire() || io.kill) {
    state := s_ready
  }
  when (io.req.fire()) {
    state := Mux(lhs_sign || rhs_sign, s_neg_inputs, s_busy)
    count := UFix(0)
    half := (dw === DW_32);
    neg_quo := lhs_sign != rhs_sign
    neg_rem := lhs_sign
    rem := isMulFN(fn, FN_REM) || isMulFN(fn, FN_REMU)
    divby0 := Bool(true);
    divisor := rhs_in
    remainder := lhs_in
    r_req := io.req.bits
  }

  val result = Mux(rem, remainder(w+w, w+1), remainder(w-1,0))
  
  io.resp.bits := r_req
  io.resp.bits.data := Mux(half, Cat(Fill(w/2, result(w/2-1)), result(w/2-1,0)), result)
  io.resp.valid := state === s_done
  io.req.ready := state === s_ready
}
