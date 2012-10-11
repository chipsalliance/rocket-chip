package rocket

import Chisel._
import Node._
import Constants._

class rocketDivider(earlyOut: Boolean = false) extends Component {
  val io = new ioMultiplier
  val w = io.req.bits.in0.getWidth
  
  val s_ready :: s_neg_inputs :: s_busy :: s_neg_outputs :: s_done :: Nil = Enum(5) { UFix() };
  val state = Reg(resetVal = s_ready);
  
  val count       = Reg() { UFix(width = log2Up(w+1)) }
  val divby0      = Reg() { Bool() };
  val neg_quo     = Reg() { Bool() };
  val neg_rem     = Reg() { Bool() };
  val reg_tag     = Reg() { Bits() };
  val rem         = Reg() { Bool() };
  val half        = Reg() { Bool() };
  
  val divisor     = Reg() { Bits() }
  val remainder   = Reg() { Bits(width = 2*w+1) }
  val subtractor  = remainder(2*w,w) - divisor
 
  val dw = io.req.bits.fn(io.req.bits.fn.width-1)
  val fn = io.req.bits.fn(io.req.bits.fn.width-2,0)
  val tc = (fn === DIV_D) || (fn === DIV_R);

  val lhs_sign = tc && Mux(dw === DW_64, io.req.bits.in0(w-1), io.req.bits.in0(w/2-1))
  val lhs_hi = Mux(dw === DW_64, io.req.bits.in0(w-1,w/2), Fill(w/2, lhs_sign))
  val lhs_in = Cat(lhs_hi, io.req.bits.in0(w/2-1,0))

  val rhs_sign = tc && Mux(dw === DW_64, io.req.bits.in1(w-1), io.req.bits.in1(w/2-1))
  val rhs_hi = Mux(dw === DW_64, io.req.bits.in1(w-1,w/2), Fill(w/2, rhs_sign))
  val rhs_in = Cat(rhs_hi, io.req.bits.in1(w/2-1,0))
        
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
  when (state === s_done && io.resp_rdy || io.req_kill) {
    state := s_ready
  }
  when (io.req.fire()) {
    state := Mux(lhs_sign || rhs_sign, s_neg_inputs, s_busy)
    count := UFix(0)
    half := (dw === DW_32);
    neg_quo := lhs_sign != rhs_sign
    neg_rem := lhs_sign
    rem := (fn === DIV_R) || (fn === DIV_RU);
    reg_tag := io.req_tag;
    divby0 := Bool(true);
    divisor := rhs_in
    remainder := lhs_in
  }

  val result = Mux(rem, remainder(w+w, w+1), remainder(w-1,0))
  
  io.resp_bits := Mux(half, Cat(Fill(w/2, result(w/2-1)), result(w/2-1,0)), result)
  io.resp_tag := reg_tag
  io.resp_val := state === s_done
  io.req.ready := state === s_ready
}
