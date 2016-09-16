package uncore.util

import Chisel._

import cde.{Parameters}

/** This black-boxes an Async Reset
  *  (or Set)
  * Register.
  *  
  * Because Chisel doesn't support
  * parameterized black boxes, 
  * we unfortunately have to 
  * instantiate a number of these.
  *  
  *  We also have to hard-code the set/
  *  reset behavior.
  *  
  *  Do not confuse an asynchronous
  *  reset signal with an asynchronously
  *  reset reg. You should still 
  *  properly synchronize your reset 
  *  deassertion.
  *  
  *  @param d Data input
  *  @param q Data Output
  *  @param clk Clock Input
  *  @param rst Reset Input
  *  @param en Write Enable Input
  *  
  */

abstract class AbstractBBReg extends BlackBox {

  val io = new Bundle {
    val d = Bool(INPUT)
    val q = Bool(OUTPUT)
    val en = Bool(INPUT)

    val clk = Clock(INPUT)
    val rst = Bool(INPUT)
  }

}

class AsyncResetReg  extends AbstractBBReg
class AsyncSetReg extends AbstractBBReg

class SimpleRegIO(val w: Int) extends Bundle{

  val d = UInt(INPUT, width = w)
  val q = UInt(OUTPUT, width = w)

  val en = Bool(INPUT)

}

class AsyncResetRegVec(val w: Int, val init: Int) extends Module {

  val io = new SimpleRegIO(w)

  val bb_q = Wire(UInt(width = w))
  val bb_d = Wire(UInt(width = w))

  val async_regs: List[AbstractBBReg] = List.tabulate(w)(
    i => Module (
      if (((init >> i) % 2) > 0)
        new AsyncSetReg
      else
        new AsyncResetReg)
  )

  bb_q := (async_regs.map(_.io.q)).asUInt()

  io.q := bb_q

  // This mux is not strictly necessary,
  // but makes this work if the underlying black
  // boxes were not enable flops.
  bb_d := Mux(io.en , io.d , bb_q)
  
  for ((reg, idx) <- async_regs.zipWithIndex) {
    reg.io.clk := clock
    reg.io.rst := reset
    reg.io.en  := io.en
    reg.io.d   := bb_d(idx)
  }

}
