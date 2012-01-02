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
  val adder_out = UFix(64, 'output);
}

class rocketDpathALU extends Component
{
  val io = new ioALU();

  // ADD, SUB
  val sub = (io.fn === FN_SUB) || (io.fn === FN_SLT) || (io.fn === FN_SLTU)
  val adder_rhs = Mux(sub, ~io.in2, io.in2)
  val adder_out = (io.in1 + adder_rhs + sub.toUFix)(63,0)

  // SLT, SLTU
  val less  = Mux(io.in1(63) === io.in2(63), adder_out(63), io.in1(63))
  val lessu = Mux(io.in1(63) === io.in2(63), adder_out(63), io.in2(63))

  // SLL, SRL, SRA
  val sra = (io.fn === FN_SRA)
  val shright = sra || (io.fn === FN_SR)
  val shin_hi_32 = Mux(sra, Fill(32, io.in1(31)), UFix(0,32))
  val shin_hi = Mux(io.dw === DW_64, io.in1(63,32), shin_hi_32)
  val shin_r = Cat(shin_hi, io.in1(31,0))
  val shin = Mux(shright, shin_r, Reverse(shin_r))
  val shout_r = (Cat(sra & shin_r(63), shin).toFix >>> io.shamt)(63,0)

  val out64 = Wire { Bits(64) }
  switch(io.fn)
  {
    is(FN_ADD)  { out64 <== adder_out }
    is(FN_SUB)  { out64 <== adder_out }
    is(FN_SLT)  { out64 <== less }
    is(FN_SLTU) { out64 <== lessu }
    is(FN_AND)  { out64 <== io.in1 & io.in2 }
    is(FN_OR)   { out64 <== io.in1 | io.in2 }
    is(FN_XOR)  { out64 <== io.in1 ^ io.in2 }
    is(FN_SL)   { out64 <== Reverse(shout_r) }
  }
  out64 <== shout_r

  val out_hi = Mux(io.dw === DW_64, out64(63,32), Fill(32, out64(31)))
  io.out := Cat(out_hi, out64(31,0)).toUFix
  io.adder_out := adder_out
}

}
