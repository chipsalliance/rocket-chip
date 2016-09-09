package uncore.util

import Chisel._

import cde.{Parameters}
import junctions.{ParameterizedBundle}

/** This black-boxes an Async Reset
  * Reg.
  *  
  * Because Chisel doesn't support
  * parameterized black boxes, 
  * we unfortunately have to 
  * instantiate a number of these.
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
  *  
  *  @param init Value to write at Reset. 
  *  This is a constant, 
  *  but this construction
  *  will likely make backend flows
  *  and lint tools unhappy.
  * 
  */

class AsyncResetReg  extends BlackBox {

  val io = new Bundle {
    val d = Bool(INPUT)
    val q = Bool(OUTPUT)

    val clk = Clock(INPUT)
    val rst = Bool(INPUT)

    val init = Bool(INPUT)
  }

}


class SimpleRegIO(val w: Int)(implicit val p: Parameters) extends ParameterizedBundle()(p){

  val d = UInt(INPUT, width = w)
  val q = UInt(OUTPUT, width = w)

  val en = Bool(INPUT)

}

class AsyncResetRegVec(val w: Int, val init: Int)(implicit val p: Parameters) extends Module {

  val io = new SimpleRegIO(w)(p)

  val bb_q = Wire(UInt(width = w))
  val bb_d = Wire(UInt(width = w))

  val init_val = Wire(UInt(width = w))
  init_val := UInt(init, width = w)

  val async_regs = List.fill(w)(Module (new AsyncResetReg))

  bb_q := (async_regs.map(_.io.q)).asUInt()
  bb_d := Mux(io.en , io.d , bb_q)

  io.q := bb_q

  
  for ((reg, idx) <- async_regs.zipWithIndex) {
    reg.io.clk := clock
    reg.io.rst := reset
    reg.io.init := init_val(idx)
    reg.io.d := bb_d(idx)
  }

}
