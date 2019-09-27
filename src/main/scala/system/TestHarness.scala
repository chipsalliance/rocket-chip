// See LICENSE.SiFive for license details.

package freechips.rocketchip.system

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.devices.debug.Debug
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.util.AsyncResetReg

class TestHarness()(implicit p: Parameters) extends Module {
  val io = new Bundle {
    val success = Bool(OUTPUT)
  }

  val dut = Module(LazyModule(new ExampleRocketSystem).module)

  // Allow the debug ndreset to reset the dut, but not until the initial reset has completed
  val debug_ndreset = AsyncResetReg(dut.debug.ndreset)
  dut.reset := reset | debug_ndreset

  dut.dontTouchPorts()
  dut.tieOffInterrupts()
  dut.connectSimAXIMem()
  dut.connectSimAXIMMIO()
  dut.l2_frontend_bus_axi4.foreach(_.tieoff)
  Debug.connectDebug(dut.debug, clock, reset, io.success)
}
