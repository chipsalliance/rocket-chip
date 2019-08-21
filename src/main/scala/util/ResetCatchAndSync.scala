// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import Chisel._
import chisel3.experimental.{withClockAndReset, withReset}

/** Reset: asynchronous assert,
  *  synchronous de-assert
  *
  */

class ResetCatchAndSync (sync: Int = 3) extends Module {

  override def desiredName = s"ResetCatchAndSync_d${sync}"

  val io = new Bundle {
    val sync_reset = Bool(OUTPUT)
    val psd = new PSDTestMode().asInput
  }

  // Bypass both the resets to the flops themselves (to prevent DFT holes on
  // those flops) and on the output of the synchronizer circuit (to control
  // reset to any flops this circuit drives).

  val post_psd_reset = Mux(io.psd.test_mode, io.psd.test_mode_reset, reset)
  withReset(post_psd_reset) {
    io.sync_reset := Mux(io.psd.test_mode, io.psd.test_mode_reset,
      ~AsyncResetSynchronizerShiftReg(Bool(true), sync))
  }
}

object ResetCatchAndSync {

  def apply(clk: Clock, rst: Bool, sync: Int = 3, name: Option[String] = None,
    psd: Option[PSDTestMode] = None): Bool = {

    withClockAndReset(clk, rst) {
      val catcher = Module (new ResetCatchAndSync(sync))
      if (name.isDefined) {catcher.suggestName(name.get)}
      catcher.io.psd <> psd.getOrElse(Wire(new PSDTestMode()).fromBits(UInt(0)))
      catcher.io.sync_reset
    }
  }

  def apply(clk: Clock, rst: Bool, sync: Int, name: String): Bool = apply(clk, rst, sync, Some(name))
  def apply(clk: Clock, rst: Bool, name: String): Bool = apply(clk, rst, name = Some(name))

  def apply(clk: Clock, rst: Bool, sync: Int, name: String, psd: PSDTestMode): Bool =
    apply(clk, rst, sync, Some(name), Some(psd))
  def apply(clk: Clock, rst: Bool, name: String, psd: PSDTestMode): Bool =
    apply(clk, rst, name = Some(name), psd = Some(psd))
  
}
