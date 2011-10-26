package Top {

import Chisel._
import Node._;
import Constants._;

class ioMultiplier(width: Int) extends Bundle {
  // requests
  val mul_val = Bool('input);
  val mul_fn  = UFix(3, 'input);
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
  val r_fn  = Reg(resetVal = UFix(0,3));
  val r_tag = Reg(resetVal = UFix(0,5));
  val r_in0 = Reg(resetVal = Bits(0,64));
  val r_in1 = Reg(resetVal = Bits(0,64));
  
  r_val <== io.mul_val;
  when (io.mul_val) {
    r_fn  <== io.mul_fn;
    r_tag <== io.mul_tag;
    r_in0 <== io.in0;
    r_in1 <== io.in1;
  }
  
  val sxl64 = (r_fn === MUL_64H) || (r_fn === MUL_64HSU);
  val sxr64 = (r_fn === MUL_64H);

  val lhs = Cat(r_in0(63) & sxl64, r_in0);
  val rhs = Cat(r_in1(63) & sxr64, r_in1);
  
  val mul_result = lhs.toFix * rhs.toFix;
  
  val mul_output = MuxCase(mul_result(63,0), Array(
    ((r_fn === MUL_64H) || (r_fn === MUL_64HU) || (r_fn === MUL_64HSU)) -> mul_result(127,64),
    (r_fn === MUL_32) -> Cat(Fill(32, mul_result(31)), mul_result(31, 0))));
  
  // just a hack for now, this should be a parameterized number of stages
  val r_result     = Reg(Reg(Reg(mul_output)));
  val r_result_tag = Reg(Reg(Reg(r_tag))); 
  val r_result_val = Reg(Reg(Reg(r_val)));
  
  io.result := r_result;
  io.result_tag := r_result_tag;
  io.result_val := r_result_val;
  
}

}
