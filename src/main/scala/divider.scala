package Top {

import Chisel._
import Node._;
import Constants._;

class ioDivider(width: Int) extends Bundle {
  // requests
  val div_val   = Bool(INPUT);
  val div_kill  = Bool(INPUT);
  val div_rdy   = Bool(OUTPUT);
  val dw        = UFix(1, INPUT);
  val div_fn    = UFix(2, INPUT);
  val div_tag   = UFix(5, INPUT);
  val in0       = Bits(width, INPUT);
  val in1       = Bits(width, INPUT);
  // responses
  val result      = Bits(width, OUTPUT);
  val result_tag  = UFix(5, OUTPUT);
  val result_val  = Bool(OUTPUT);
  val result_rdy  = Bool(INPUT);
}

class rocketDivider(width : Int) extends Component {
  val io = new ioDivider(width);
  
  val s_ready :: s_neg_inputs :: s_busy :: s_neg_outputs :: s_done :: Nil = Enum(5) { UFix() };
  val state = Reg(resetVal = s_ready);
  
  val count       = Reg() { UFix() };
  val divby0      = Reg() { Bool() };
  val neg_quo     = Reg() { Bool() };
  val neg_rem     = Reg() { Bool() };
  val reg_tag     = Reg() { UFix() };
  val rem         = Reg() { Bool() };
  val half        = Reg() { Bool() };
  
  val divisor     = Reg() { UFix() };
  val remainder   = Reg() { UFix() };
  val subtractor  = remainder(2*width, width).toUFix - divisor;
  
  val tc = (io.div_fn === DIV_D) || (io.div_fn === DIV_R);

  val do_kill = io.div_kill && Reg(io.div_rdy) // kill on 1st cycle only

  switch (state) {
    is (s_ready) {
      when (io.div_val) {
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
      when (io.result_rdy) {
        state := s_ready
      }
    }
  }
  
  // state machine

  val lhs_sign = tc && Mux(io.dw === DW_64, io.in0(width-1), io.in0(width/2-1)).toBool
  val lhs_hi = Mux(io.dw === DW_64, io.in0(width-1,width/2), Fill(width/2, lhs_sign))
  val lhs_in = Cat(lhs_hi, io.in0(width/2-1,0))

  val rhs_sign = tc && Mux(io.dw === DW_64, io.in1(width-1), io.in1(width/2-1)).toBool
  val rhs_hi = Mux(io.dw === DW_64, io.in1(width-1,width/2), Fill(width/2, rhs_sign))
  val rhs_in = Cat(rhs_hi, io.in1(width/2-1,0))
        
  when ((state === s_ready) && io.div_val) {
    count := UFix(0, log2up(width+1));
    half := (io.dw === DW_32);
    neg_quo := Bool(false);
    neg_rem := Bool(false);
    rem := (io.div_fn === DIV_R) || (io.div_fn === DIV_RU);
    reg_tag := io.div_tag;
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
  
  io.result := Mux(half, Cat(Fill(width/2, result(width/2-1)), result(width/2-1,0)), result);
  io.result_tag := reg_tag;
  io.result_val := (state === s_done);

  io.div_rdy := (state === s_ready);
}

}
