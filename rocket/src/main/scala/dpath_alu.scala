package Top {


import Chisel._
import Node._;

import Constants._
import Instructions._

class ioALU extends Bundle(){
  val dw    = UFix(1, 'input);
  val fn    = UFix(4, 'input);
  val shamt = UFix(6, 'input);
  val in2   = UFix(64, 'input);
  val in1   = UFix(64, 'input);
  val out   = UFix(64, 'output);
}

class rocketDpathALU extends Component
{
  override val io = new ioALU();
  val out64 =
    MuxCase(Fix(0, 64), Array(
      (io.fn === FN_ADD) ->                   (io.in1 + io.in2).toFix,
      (io.fn === FN_SUB) ->                   (io.in1 - io.in2).toFix,
      (io.fn === FN_SLT) ->                   (io.in1.toFix < io.in2.toFix), //(io.in1 < io.in2)
      (io.fn === FN_SLTU) ->                  (io.in1 < io.in2).toFix,
      (io.fn === FN_AND) ->                   (io.in1 & io.in2).toFix,
      (io.fn === FN_OR) ->                    (io.in1 | io.in2).toFix,
      (io.fn === FN_XOR) ->                   (io.in1 ^ io.in2).toFix,
      (io.fn === FN_SL) ->                    (io.in1 << io.shamt).toFix,
      (io.fn === FN_SR && io.dw === DW_64) -> (io.in1 >> io.shamt).toFix,
      (io.fn === FN_SR && io.dw === DW_32) -> (Cat(Fix(0, 32),io.in1(31, 0)).toUFix >> io.shamt),
      (io.fn === FN_SRA) ->                   (io.in1.toFix >>> io.shamt)));
      
  io.out := MuxLookup(io.dw, Fix(0, 64), Array(
              DW_64 -> out64,
              DW_32 -> Cat(Fill(32, out64(31)), out64(31,0)).toFix)).toUFix;

}

/*
class IoDpathALU extends Bundle {
  val in0 = Bits(32,'input);
  val in1 = Bits(32,'input);
  val fn = Bits(4,'input);
  val out = Bits(32,'output);
}

class DpathALU extends Component {
  val io = new IoDpathALU();

  val adder_in0 = MuxCase(io.in0,Array(
      ((io.fn === FN_SUB) | (io.fn === FN_SLT) | (io.fn === FN_SLTU)) -> (~io.in0)));

  val adder_in1 = io.in1;
  val adder_cin = MuxCase(Bits(0),Array(
      ((io.fn === FN_SUB) | (io.fn === FN_SLT) | (io.fn === FN_SLTU)) -> Bits(1)));

  // Need to make the same width?
  val adder_out = Cat(Bits(0,1),adder_in1).toUFix + Cat(Bits(0,1),adder_in0).toUFix + adder_cin.toUFix;
  //adder_out := (adder_in1.toUFix + adder_in0.toUFix + adder_cin.toUFix);

  // Determine if there is overflow
  val overflow = (io.in0(31) ^ io.in1(31)) & (adder_out(32) != io.in0(31));

  val compare_yes = MuxLookup(io.fn,Bits(0),Array(
      // If unsigned, do subtraction, and if the result is negative, then slt=true
      FN_SLTU -> ~adder_out(32),
      // If signed, do subtraction, and if the result is negative, then slt=true as well
      // But if there is bad overflow (operands same sign and result is a different sign),
      // then need to flip
      FN_SLT -> ~(adder_out(32) ^ overflow)));

  io.out := MuxLookup(io.fn,Fix(0),Array(
    FN_ADD -> adder_out,
    FN_SUB -> adder_out,
    FN_SLT -> compare_yes,
    FN_SLTU -> compare_yes,
    FN_AND -> (io.in0 & io.in1),
    FN_OR -> (io.in0 | io.in1),
    FN_XOR -> (io.in0 ^ io.in1),
    FN_SL -> (io.in1 << io.in0(4,0).toUFix),
    FN_SR -> (io.in1 >> io.in0(4,0).toUFix),
    FN_SRA -> (io.in1.toFix >> io.in0(4,0).toUFix)
  ));
}
*/

}
