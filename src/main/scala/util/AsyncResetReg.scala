// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.experimental.{IntParam, withReset}
import chisel3.util.{HasBlackBoxResource, RegEnable}

class SimpleRegIO(val w: Int) extends Bundle {
  val d = Input(UInt(w.W))
  val q = Output(UInt(w.W))
  val en = Input(Bool())
}

class AsyncResetRegVec(val w: Int, val init: BigInt) extends Module {
  val io = IO(new SimpleRegIO(w))

  val reg = withReset(reset.asAsyncReset) {
    RegEnable(io.d, init.U(w.W), io.en)
  }

  io.q := reg

  override def desiredName = s"AsyncResetRegVec_w${w}_i${init}"
}

object AsyncResetReg {
  // Create Single Registers
  def apply(d: Bool, clk: Clock, rst: Bool, init: Boolean, name: Option[String]): Bool = {
    val reg = withReset(rst.asAsyncReset)(RegNext(d, init.B))
    name.foreach(reg.suggestName(_))
    reg
  }

  def apply(d: Bool, clk: Clock, rst: Bool): Bool = apply(d, clk, rst, false, None)
  def apply(d: Bool, clk: Clock, rst: Bool, name: String): Bool = apply(d, clk, rst, false, Some(name))

  // Create Vectors of Registers
  def apply(updateData: UInt, resetData: BigInt, enable: Bool, name: Option[String] = None): UInt = {
    val w = updateData.getWidth max resetData.bitLength
    val reg = withReset(Module.reset.asAsyncReset) {
      RegEnable(updateData, resetData.U(w.W), enable)
    }
    name.foreach(reg.suggestName(_))
    reg
  }
  def apply(updateData: UInt, resetData: BigInt, enable: Bool, name: String): UInt = apply(updateData,
    resetData, enable, Some(name))


  def apply(updateData: UInt, resetData: BigInt): UInt = apply(updateData, resetData, enable=true.B)
  def apply(updateData: UInt, resetData: BigInt, name: String): UInt = apply(updateData, resetData, enable=true.B, Some(name))

  def apply(updateData: UInt, enable: Bool): UInt = apply(updateData, resetData=BigInt(0), enable)
  def apply(updateData: UInt, enable: Bool, name: String): UInt = apply(updateData, resetData=BigInt(0), enable, Some(name))

  def apply(updateData: UInt): UInt = apply(updateData, resetData=BigInt(0), enable=true.B)
  def apply(updateData: UInt, name:String): UInt = apply(updateData, resetData=BigInt(0), enable=true.B, Some(name))
}

