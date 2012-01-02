package Top {

import Chisel._
import Node._;
import Constants._;

class ioMultiplier(width: Int) extends Bundle {
  // requests
  val mul_val = Bool('input);
  val mul_kill= Bool('input);
  val mul_rdy = Bool('output);
  val dw      = UFix(1, 'input);
  val mul_fn  = UFix(2, 'input);
  val mul_tag = UFix(CPU_TAG_BITS, 'input);
  val in0     = Bits(width, 'input);
  val in1     = Bits(width, 'input);
  
  // responses
  val result     = Bits(width, 'output);
  val result_tag = UFix(CPU_TAG_BITS, 'output);
  val result_val = Bool('output);
  val result_rdy = Bool('input);
}

class rocketMultiplier extends Component {
  val io = new ioMultiplier(64);
  // width must be even (booth).
  // we need an extra bit to handle signed vs. unsigned,
  // so we need to add a second to keep width even.
  val width = 64 + 2
  // unroll must divide width/2
  val unroll = 3

  val cycles = width/unroll/2
  
  val r_val = Reg(resetVal = Bool(false));
  val r_dw  = Reg { UFix() }
  val r_fn  = Reg { UFix() }
  val r_tag = Reg { UFix() }
  val r_lhs = Reg { Bits() }
  val r_prod= Reg { Bits(width = width*2) }
  val r_lsb = Reg { Bits() }
  val r_cnt = Reg { UFix(width = log2up(cycles+1)) }

  val lhs_msb = Mux(io.dw === DW_64, io.in0(63), io.in0(31)).toBool
  val lhs_sign = ((io.mul_fn === MUL_HS) || (io.mul_fn === MUL_HSU)) && lhs_msb
  val lhs_hi = Mux(io.dw === DW_64, io.in0(63,32), Fill(32, lhs_sign))
  val lhs_in = Cat(lhs_sign, lhs_hi, io.in0(31,0))

  val rhs_msb = Mux(io.dw === DW_64, io.in1(63), io.in1(31)).toBool
  val rhs_sign = (io.mul_fn === MUL_HS) && rhs_msb
  val rhs_hi = Mux(io.dw === DW_64, io.in1(63,32), Fill(32, rhs_sign))
  val rhs_in = Cat(rhs_sign, rhs_sign, rhs_hi, io.in1(31,0))
  
  when (io.mul_val && io.mul_rdy) {
    r_val <== Bool(true)
    r_cnt <== UFix(0, log2up(cycles+1))
    r_dw  <== io.dw
    r_fn  <== io.mul_fn
    r_tag <== io.mul_tag
    r_lhs <== lhs_in
    r_prod<== rhs_in
    r_lsb <== Bool(false)
  }
  when (io.result_val && io.result_rdy || io.mul_kill) {
    r_val <== Bool(false)
  }

  val lhs_sext = Cat(r_lhs(width-2), r_lhs(width-2), r_lhs).toUFix
  val lhs_twice = Cat(r_lhs(width-2), r_lhs, Bits(0,1)).toUFix

  var prod = r_prod
  var lsb = r_lsb

  for (i <- 0 until unroll) {
    val addend = Mux(prod(0) != lsb,     lhs_sext,
                 Mux(prod(0) != prod(1), lhs_twice,
                                             UFix(0)));
    val sub = prod(1)
    val adder_lhs = Cat(prod(width*2-1), prod(width*2-1,width)).toUFix
    val adder_rhs = Mux(sub, ~addend, addend)
    val adder_out = (adder_lhs + adder_rhs + sub.toUFix)(width,0)

    lsb = prod(1)
    prod = Cat(adder_out(width), adder_out, prod(width-1,2))
  }

  when (r_val && (r_cnt != UFix(cycles))) {
    r_lsb  <== lsb
    r_prod <== prod
    r_cnt <== r_cnt + UFix(1)
  }

  val mul_output64 = Mux(r_fn === MUL_LO, r_prod(63,0), r_prod(127,64))
  val mul_output32 = Mux(r_fn === MUL_LO, r_prod(31,0), r_prod(63,32))
  val mul_output32_ext = Cat(Fill(32, mul_output32(31)), mul_output32)
  
  val mul_output = Mux(r_dw === DW_64, mul_output64, mul_output32_ext)
 
  io.mul_rdy := !r_val
  io.result := mul_output;
  io.result_tag := r_tag;
  io.result_val := r_val && (r_cnt === UFix(cycles))
}

}
