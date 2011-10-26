package Top {

import Chisel._
import Node._;
import Constants._;

class ioDivider(width: Int) extends Bundle {
  // requests
  val div_val   = Bool('input);
  val div_rdy   = Bool('output);
  val div_fn    = UFix(4, 'input);
  val div_waddr = UFix(5, 'input);
  val dpath_rs1 = Bits(width, 'input);
  val dpath_rs2 = Bits(width, 'input);
  // responses
  val div_result_bits = Bits(width, 'output);
  val div_result_tag  = UFix(5, 'output);
  val div_result_val  = Bool('output);
  val div_result_rdy  = Bool('input);
}

// class ioDivider extends Bundle {
//   // requests
//   val req_val   = Bool('input);
//   val req_rdy   = Bool('output);
//   val req_fn    = UFix(3, 'input);
//   val req_tag = UFix(5, 'input);
//   val req_rs1   = Bits(64, 'input);
//   val req_rs2   = Bits(64, 'input);
//   // responses
//   val resp_val  = Bool('output);
//   val resp_data = Bits(64, 'output);
//   val resp_tag  = UFix(5, 'output);
// }

class rocketDivider(width : Int) extends Component {
  val io = new ioDivider(width);
  
  val s_ready :: s_neg_inputs :: s_busy :: s_neg_outputs :: s_done :: Nil = Enum(5) { UFix() };
  val state = Reg(resetVal = s_ready);
  
  val count_bits  = java.math.BigInteger.valueOf(width).bitLength();
  val count       = Reg(resetVal = UFix(0, count_bits));
  val divby0      = Reg(resetVal = Bool(false));
  val neg_quo     = Reg(resetVal = Bool(false));
  val neg_rem     = Reg(resetVal = Bool(false));
  val reg_waddr   = Reg(resetVal = UFix(0, 5));
  val rem         = Reg(resetVal = Bool(false));
  val half        = Reg(resetVal = Bool(false));
  val tc          = Reg(resetVal = Bool(false));
  
  val divisor     = Reg(resetVal = UFix(0, width));
  val remainder   = Reg(resetVal = UFix(0, 2*width+1));
  val subtractor  = remainder(2*width, width).toUFix - divisor;
  
  val v_tc   = ((io.div_fn === DIV_64D) || (io.div_fn === DIV_64R)) ||
               ((io.div_fn === DIV_32D) || (io.div_fn === DIV_32R));

  val v_rem  = ((io.div_fn === DIV_32R) || (io.div_fn === DIV_32RU)) ||
               ((io.div_fn === DIV_64R) || (io.div_fn === DIV_64RU));
             
  val v_half = ((io.div_fn === DIV_32R) || (io.div_fn === DIV_32RU)) ||
               ((io.div_fn === DIV_32D) || (io.div_fn === DIV_32DU));
  
  // state machine
  switch (state) {
    is (s_ready) {
      when (!io.div_val)  { state <== s_ready; }
      when (v_tc)         { state <== s_neg_inputs };
      otherwise           { state <== s_busy; }
    }
    is (s_neg_inputs)     { state <== s_busy; }
    is (s_busy) {
      when (count != UFix(width))   { state <== s_busy; }
      when (!(neg_quo || neg_rem))  { state <== s_done; }
      otherwise                     { state <== s_neg_outputs; }
    }
    is (s_neg_outputs)              { state <== s_done; }
    is (s_done) {
      when (io.div_result_rdy)      { state <== s_ready; }
    }
  }

  // if we're doing 32-bit unsigned division, then we don't want the 32-bit
  // inputs to be sign-extended.
  val in_lhs = Mux((v_half && !v_tc), 
                   Cat(Fill(width/2, UFix(0,1)), io.dpath_rs1(width/2-1, 0)),
                   io.dpath_rs1).toUFix;

  val in_rhs = Mux((v_half && !v_tc), 
                   Cat(Fill(width/2, UFix(0,1)), io.dpath_rs2(width/2-1, 0)),
                   io.dpath_rs2).toUFix;                   
        
  when ((state === s_ready) && io.div_val) {
    count <== UFix(0, count_bits);
    half <== v_half;
    neg_quo <== Bool(false);
    neg_rem <== Bool(false);
    rem <== v_rem;
    tc <== v_tc;
    reg_waddr <== io.div_waddr;
    divby0 <== Bool(true);
    divisor <== in_rhs;
    remainder <== Cat(Fill(width+1, UFix(0,1)), in_lhs).toUFix;
  }

  when (state === s_neg_inputs) {
    neg_rem <== remainder(width-1).toBool;
    neg_quo <== (remainder(width-1) != divisor(width-1));
    when (remainder(width-1).toBool) {
      remainder <== Cat(remainder(2*width, width), -remainder(width-1,0)).toUFix;
    }
    when (divisor(width-1).toBool) {
      divisor <== subtractor(width-1,0);
    }
  }
  when (state === s_neg_outputs) {
    when (neg_rem && neg_quo && !divby0) {
      remainder <== Cat(-remainder(2*width, width+1), remainder(width), -remainder(width-1,0)).toUFix;
    }
    when (neg_quo && !divby0) {
      remainder <== Cat(remainder(2*width, width), -remainder(width-1,0)).toUFix;
    }
    when (neg_rem) {
      remainder <== Cat(-remainder(2*width, width+1), remainder(width,0)).toUFix;
    }
    when (divisor(width-1).toBool) {
      divisor <== subtractor(width-1,0);
    }
  }
  when (state === s_busy) {
    count <== count + UFix(1);
    divby0 <== divby0 && !subtractor(width).toBool;
    remainder <== Mux(subtractor(width).toBool,
                      Cat(remainder(2*width-1, width), remainder(width-1,0), ~subtractor(width)),
                      Cat(subtractor(width-1, 0), remainder(width-1,0), ~subtractor(width))).toUFix;
  }  

  val result = Mux(rem, remainder(2*width, width+1), remainder(width-1,0));
  
  io.div_result_bits := Mux(half, Cat(Fill(width/2, result(width/2-1)), result(width/2-1,0)), result);
  io.div_rdy := (state === s_ready);
  io.div_result_tag := reg_waddr;
  io.div_result_val := (state === s_done);
}

}
