// See LICENSE.SiFive for license details.

package freechips.rocketchip.groundtest

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.system.SimAXIMem

class TestHarness(implicit p: Parameters) extends Module {
  val io = new Bundle { val success = Bool(OUTPUT) }
  val ldut = LazyModule(new GroundTestSubsystem)
  val dut = Module(ldut.module)
  io.success := dut.success
  SimAXIMem.connectMem(ldut)
}
