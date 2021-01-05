// See LICENSE.SiFive for license details.

package freechips.rocketchip.groundtest

import Chisel._
import diplomacy.config.Parameters
import diplomacy.LazyModule
import freechips.rocketchip.system.SimAXIMem

class TestHarness(implicit p: Parameters) extends Module {
  val io = new Bundle { val success = Bool(OUTPUT) }
  val ldut = LazyModule(new GroundTestSubsystem)
  val dut = Module(ldut.module)
  io.success := dut.success
  SimAXIMem.connectMem(ldut)
}
