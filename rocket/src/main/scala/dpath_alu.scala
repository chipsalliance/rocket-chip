package rocket

import Chisel._
import Node._
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

object ALU
{
  val FN_X    = Bits("b????")
  val FN_ADD  = UFix(0)
  val FN_SL   = UFix(1)
  val FN_XOR  = UFix(4)
  val FN_OR   = UFix(6)
  val FN_AND  = UFix(7)
  val FN_SR   = UFix(5)
  val FN_SUB  = UFix(8)
  val FN_SLT  = UFix(10)
  val FN_SLTU = UFix(11)
  val FN_SRA  = UFix(13)
  val FN_OP2  = UFix(15)

  def isSub(cmd: Bits) = cmd(3)
  def isSLTU(cmd: Bits) = cmd(0)
}

class ALU extends Component
{
  import ALU._
  val io = new ioALU();

  // ADD, SUB
  val sub = isSub(io.fn)
  val adder_rhs = Mux(sub, ~io.in2, io.in2)
  val sum = (io.in1 + adder_rhs + sub.toUFix)(63,0)

  // SLT, SLTU
  val less  = Mux(io.in1(63) === io.in2(63), sum(63),
              Mux(isSLTU(io.fn), io.in2(63), io.in1(63)))

  // SLL, SRL, SRA
  val shamt = Cat(io.in2(5) & (io.dw === DW_64), io.in2(4,0)).toUFix
  val shin_hi_32 = Mux(isSub(io.fn), Fill(32, io.in1(31)), UFix(0,32))
  val shin_hi = Mux(io.dw === DW_64, io.in1(63,32), shin_hi_32)
  val shin = Cat(shin_hi, io.in1(31,0))
  val shout_r = (Cat(isSub(io.fn) & shin(63), shin).toFix >> shamt)(63,0)
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
