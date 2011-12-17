package Top {

import Chisel._
import Node._;
import Constants._;

class ioMultiplier(width: Int) extends Bundle {
  // requests
  val mul_val = Bool('input);
  val dw      = UFix(1, 'input);
  val mul_fn  = UFix(2, 'input);
  val mul_tag = UFix(5, 'input);
  val in0     = Bits(width, 'input);
  val in1     = Bits(width, 'input);
  
  // responses
  val result     = Bits(width, 'output);
  val result_tag = UFix(5, 'output);
  val result_val = Bool('output);
}

class rocketMultiplier extends Component {
  val io = new ioMultiplier(64);
  
  val r_val = Reg(resetVal = Bool(false));
  val r_dw  = Reg(resetVal = UFix(0,1));
  val r_fn  = Reg(resetVal = UFix(0,3));
  val r_tag = Reg(resetVal = UFix(0,5));
  val r_lhs = Reg(resetVal = Bits(0,65));
  val r_rhs = Reg(resetVal = Bits(0,65));

  val lhs_msb = Mux(io.dw === DW_64, io.in0(63), io.in0(31)).toBool
  val lhs_sign = ((io.mul_fn === MUL_HS) || (io.mul_fn === MUL_HSU)) && lhs_msb
  val lhs_hi = Mux(io.dw === DW_64, io.in0(63,32), Fill(32, lhs_sign))
  val lhs = Cat(lhs_sign, lhs_hi, io.in0(31,0))

  val rhs_msb = Mux(io.dw === DW_64, io.in1(63), io.in1(31)).toBool
  val rhs_sign = (io.mul_fn === MUL_HS) && rhs_msb
  val rhs_hi = Mux(io.dw === DW_64, io.in1(63,32), Fill(32, rhs_sign))
  val rhs = Cat(rhs_sign, rhs_hi, io.in1(31,0))
  
  r_val <== io.mul_val;
  when (io.mul_val) {
    r_dw  <== io.dw
    r_fn  <== io.mul_fn;
    r_tag <== io.mul_tag;
    r_lhs <== lhs;
    r_rhs <== rhs;
  }
  
  val mul_result = r_lhs.toFix * r_rhs.toFix;

  val mul_output64 = Mux(r_fn === MUL_LO, mul_result(63,0), mul_result(127,64))
  val mul_output32 = Mux(r_fn === MUL_LO, mul_result(31,0), mul_result(63,31))
  val mul_output32_ext = Cat(Fill(32, mul_output32(31)), mul_output32)
  
  val mul_output = Mux(r_dw === DW_64, mul_output64, mul_output32_ext)
  
  // just a hack for now, this should be a parameterized number of stages
  val r_result     = Reg(Reg(Reg(mul_output)));
  val r_result_tag = Reg(Reg(Reg(r_tag))); 
  val r_result_val = Reg(Reg(Reg(r_val)));
  
  io.result := r_result;
  io.result_tag := r_result_tag;
  io.result_val := r_result_val;
  
}

}
