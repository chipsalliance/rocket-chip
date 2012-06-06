package rocket

import Chisel._
import Node._
import Constants._

class rocketDivider(width: Int) extends Component {
  val io = new ioMultiplier
  
  val s_ready :: s_neg_inputs :: s_busy :: s_neg_outputs :: s_done :: Nil = Enum(5) { UFix() };
  val state = Reg(resetVal = s_ready);
  
  val count       = Reg() { UFix() };
  val divby0      = Reg() { Bool() };
  val neg_quo     = Reg() { Bool() };
  val neg_rem     = Reg() { Bool() };
  val reg_tag     = Reg() { Bits() };
  val rem         = Reg() { Bool() };
  val half        = Reg() { Bool() };
  
  val divisor     = Reg() { UFix() };
  val remainder   = Reg() { UFix() };
  val subtractor  = remainder(2*width, width).toUFix - divisor;
 
  val dw = io.req.bits.fn(io.req.bits.fn.width-1)
  val fn = io.req.bits.fn(io.req.bits.fn.width-2,0)
  val tc = (fn === DIV_D) || (fn === DIV_R);

  val do_kill = io.req_kill && Reg(io.req.ready) // kill on 1st cycle only

  switch (state) {
    is (s_ready) {
      when (io.req.valid) {
        state := Mux(tc, s_neg_inputs, s_busy)
      }
    }
    is (s_neg_inputs) {
      state := Mux(do_kill, s_ready, s_busy)
    }
    is (s_busy) {
      when (do_kill) {
        state := s_ready
      }
      .elsewhen (count === UFix(width)) {
        state := Mux(neg_quo || neg_rem, s_neg_outputs, s_done)
      }
    }
    is (s_neg_outputs) {
      state := s_done
    }
    is (s_done) {
      when (io.resp_rdy) {
        state := s_ready
      }
    }
  }
  
  // state machine

  val lhs_sign = tc && Mux(dw === DW_64, io.req.bits.in0(width-1), io.req.bits.in0(width/2-1)).toBool
  val lhs_hi = Mux(dw === DW_64, io.req.bits.in0(width-1,width/2), Fill(width/2, lhs_sign))
  val lhs_in = Cat(lhs_hi, io.req.bits.in0(width/2-1,0))

  val rhs_sign = tc && Mux(dw === DW_64, io.req.bits.in1(width-1), io.req.bits.in1(width/2-1)).toBool
  val rhs_hi = Mux(dw === DW_64, io.req.bits.in1(width-1,width/2), Fill(width/2, rhs_sign))
  val rhs_in = Cat(rhs_hi, io.req.bits.in1(width/2-1,0))
        
  when ((state === s_ready) && io.req.valid) {
    count := UFix(0, log2Up(width+1));
    half := (dw === DW_32);
    neg_quo := Bool(false);
    neg_rem := Bool(false);
    rem := (fn === DIV_R) || (fn === DIV_RU);
    reg_tag := io.req_tag;
    divby0 := Bool(true);
    divisor := rhs_in.toUFix;
    remainder := Cat(UFix(0,width+1), lhs_in).toUFix;
  }
  when (state === s_neg_inputs) {
    neg_rem := remainder(width-1).toBool;
    neg_quo := (remainder(width-1) != divisor(width-1));
    when (remainder(width-1).toBool) {
      remainder := Cat(remainder(2*width, width), -remainder(width-1,0)).toUFix;
    }
    when (divisor(width-1).toBool) {
      divisor := subtractor(width-1,0);
    }
  }
  when (state === s_neg_outputs) {
    when (neg_rem && neg_quo && !divby0) {
      remainder := Cat(-remainder(2*width, width+1), remainder(width), -remainder(width-1,0)).toUFix;
    }
    .elsewhen (neg_quo && !divby0) {
      remainder := Cat(remainder(2*width, width), -remainder(width-1,0)).toUFix;
    }
    .elsewhen (neg_rem) {
      remainder := Cat(-remainder(2*width, width+1), remainder(width,0)).toUFix;
    }

    when (divisor(width-1).toBool) {
      divisor := subtractor(width-1,0);
    }
  }
  when (state === s_busy) {
    count := count + UFix(1);
    divby0 := divby0 && !subtractor(width).toBool;
    remainder := Mux(subtractor(width).toBool,
                      Cat(remainder(2*width-1, width), remainder(width-1,0), ~subtractor(width)),
                      Cat(subtractor(width-1, 0), remainder(width-1,0), ~subtractor(width))).toUFix;
  }  

  val result = Mux(rem, remainder(2*width, width+1), remainder(width-1,0));
  
  io.resp_bits := Mux(half, Cat(Fill(width/2, result(width/2-1)), result(width/2-1,0)), result);
  io.resp_tag := reg_tag;
  io.resp_val := (state === s_done);

  io.req.ready := (state === s_ready);
}
