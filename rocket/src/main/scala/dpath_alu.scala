package rocket

import Chisel._
import Node._;

import Constants._
import Instructions._

class ioALU extends Bundle(){
  val dw    = UFix(INPUT, 1);
  val fn    = UFix(INPUT, 4);
  val in2   = UFix(INPUT, 64);
  val in1   = UFix(INPUT, 64);
  val out   = UFix(OUTPUT, 64);
  val adder_out = UFix(OUTPUT, 64);
}

class rocketDpathALU extends Component
{
  val io = new ioALU();

  // ADD, SUB
  val sub = (io.fn === FN_SUB) || (io.fn === FN_SLT) || (io.fn === FN_SLTU)
  val adder_rhs = Mux(sub, ~io.in2, io.in2)
  val sum = (io.in1 + adder_rhs + sub.toUFix)(63,0)

  // SLT, SLTU
  val less  = Mux(io.in1(63) === io.in2(63), sum(63),
              Mux(io.fn === FN_SLT, io.in1(63), io.in2(63)))

  // SLL, SRL, SRA
  val sra = (io.fn === FN_SRA)
  val shamt = Cat(io.in2(5) & (io.dw === DW_64), io.in2(4,0)).toUFix
  val shright = sra || (io.fn === FN_SR)
  val shin_hi_32 = Mux(sra, Fill(32, io.in1(31)), UFix(0,32))
  val shin_hi = Mux(io.dw === DW_64, io.in1(63,32), shin_hi_32)
  val shin = Cat(shin_hi, io.in1(31,0))
  val shout_r = (Cat(sra & shin(63), shin).toFix >> shamt)(63,0)
  val shout_l = (shin << shamt)(63,0)

  val bitwise_logic =
    Mux(io.fn === FN_AND, io.in1 & io.in2,
    Mux(io.fn === FN_OR,  io.in1 | io.in2,
    Mux(io.fn === FN_XOR, io.in1 ^ io.in2,
        io.in2))) // FN_OP2

  val out64 =
    Mux(io.fn === FN_ADD || io.fn === FN_SUB,  sum,
    Mux(io.fn === FN_SLT || io.fn === FN_SLTU, less,
    Mux(io.fn === FN_SR  || io.fn === FN_SRA,  shout_r,
    Mux(io.fn === FN_SL,                       shout_l,
        bitwise_logic))))

  val out_hi = Mux(io.dw === DW_64, out64(63,32), Fill(32, out64(31)))
  io.out := Cat(out_hi, out64(31,0)).toUFix
  io.adder_out := sum
}
