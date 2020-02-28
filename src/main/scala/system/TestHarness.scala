// See LICENSE.SiFive for license details.

package freechips.rocketchip.system

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.devices.debug.Debug
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.util.AsyncResetReg
import freechips.rocketchip.aoputil._

class TestHarness()(implicit p: Parameters) extends Module {
  val io = new Bundle {
    val success = Bool(OUTPUT)
  }

  val ldut = LazyModule(new ExampleRocketSystem)
  val dut = Module(ldut.module)

  // Allow the debug ndreset to reset the dut, but not until the initial reset has completed
  dut.reset := reset | dut.debug.map { debug => AsyncResetReg(debug.ndreset) }.getOrElse(false.B)

  println(s"SULTAN info TestHarness")
  AopInjModules

  dut.dontTouchPorts()
  dut.tieOffInterrupts()
  SimAXIMem.connectMem(ldut)
  SimAXIMem.connectMMIO(ldut)
  ldut.l2_frontend_bus_axi4.foreach(_.tieoff)
  Debug.connectDebug(dut.debug, dut.psd, clock, reset, io.success)
}

object AopInjModules {

  InjectModules.register(Seq(
    ShowModules,
    ShowModulePorts("DCache"),
    AopInjectMonitor("DCache$".r, "TLMonitor", "HellaCacheBundle")
  ))
}

// AopInjectMonitorChild("DCache$".r, "TLMonitor", "HellaCacheBundle", <child-name>)
// AopInjectMonitorParent("DCache$".r, "TLMonitor", "HellaCacheBundle", <parent-name>)
// AopInjectMonitorAncestor("DCache$".r, "TLMonitor", "HellaCacheBundle", <ancestor-name>)
// AopInjectMonitorDescendant("DCache$".r, "TLMonitor", "HellaCacheBundle".<descendant-name>)

