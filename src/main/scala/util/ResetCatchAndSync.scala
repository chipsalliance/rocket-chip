// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import Chisel._

/** Reset: asynchronous assert,
  *  synchronous de-assert
  *
  */

class ResetCatchAndSync (sync: Int = 3) extends Module {

  override def desiredName = s"ResetCatchAndSync_d${sync}"

  val io = new Bundle {
    val sync_reset = Bool(OUTPUT)
    val psd_test_reset = Bool(INPUT)
    val psd_test_mode = Bool(INPUT)
  }

  io.sync_reset := Mux(io.psd_test_mode, io.psd_test_reset,
    ~AsyncResetSynchronizerShiftReg(Bool(true), sync))

}

object ResetCatchAndSync {

  def apply(clk: Clock, rst: Bool, sync: Int = 3, name: Option[String] = None,
    psd_test_mode: Bool = Bool(false), psd_test_reset: Bool = Bool(false)): Bool = {

    val catcher = Module (new ResetCatchAndSync(sync))
    if (name.isDefined) {catcher.suggestName(name.get)}
    catcher.clock := clk
    catcher.reset := rst
    catcher.io.psd_test_mode := psd_test_mode
    catcher.io.psd_test_reset:= psd_test_reset

    catcher.io.sync_reset
  }

  def apply(clk: Clock, rst: Bool, sync: Int, name: String): Bool = apply(clk, rst, sync, Some(name))
  def apply(clk: Clock, rst: Bool, name: String): Bool = apply(clk, rst, name = Some(name))

  def apply(clk: Clock, rst: Bool, sync: Int, name: String, psd_test_mode: Bool, psd_test_reset: Bool): Bool = apply(clk, rst, sync, Some(name),
    psd_test_mode, psd_test_reset)
  def apply(clk: Clock, rst: Bool, name: String, psd_test_mode: Bool, psd_test_reset: Bool): Bool = apply(clk, rst, name = Some(name),
    psd_test_mode = psd_test_mode, psd_test_reset = psd_test_reset)

}
