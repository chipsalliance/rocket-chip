package uncore.util

import Chisel._

import cde.{Parameters}

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

class SimpleRegIO(val w: Int) extends Bundle{

  val d = UInt(INPUT, width = w)
  val q = UInt(OUTPUT, width = w)

  val en = Bool(INPUT)

}

class AsyncResetRegVec(val w: Int, val init: BigInt) extends Module {

  val io = new SimpleRegIO(w)

  val bb_d = Mux(io.en, io.d, io.q)

  val async_regs = List.fill(w)(Module (new AsyncResetReg))

  io.q := async_regs.map(_.io.q).asUInt

  for ((reg, idx) <- async_regs.zipWithIndex) {
    reg.io.clk := clock
    reg.io.rst := reset
    reg.io.init := Bool(((init >> idx) & 1) == 1)
    reg.io.d := bb_d(idx)
  }

}

object AsyncResetReg {
  def apply(d: Bool, clk: Clock, rst: Bool, init: Bool): Bool = {
    val reg = Module(new AsyncResetReg)
    reg.io.d := d
    reg.io.clk := clk
    reg.io.rst := rst
    reg.io.init := init
    reg.io.q
  }

  def apply(d: Bool, clk: Clock, rst: Bool): Bool = apply(d, clk, rst, Bool(false))

  def apply(updateData: UInt, resetData: BigInt, enable: Bool): UInt = {
    val w = updateData.getWidth max resetData.bitLength
    val reg = Module(new AsyncResetRegVec(w, resetData))
    reg.io.d := updateData
    reg.io.en := enable
    reg.io.q
  }

  def apply(updateData: UInt, resetData: BigInt): UInt = apply(updateData, resetData, Bool(true))

  def apply(updateData: UInt, enable: Bool): UInt = apply(updateData, BigInt(0), enable)

  def apply(updateData: UInt): UInt = apply(updateData, BigInt(0), Bool(true))
}
