// See LICENSE.SiFive for license details.

package freechips.rocketchip.groundtest

import chisel3._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.system.SimAXIMem

class TestHarness(implicit p: Parameters) extends Module {
  val io = IO(new Bundle { val success = Output(Bool()) })
  val ldut = LazyModule(new GroundTestSubsystem)
  val dut = Module(ldut.module)
  io.success := dut.success
  SimAXIMem.connectMem(ldut)
}
