// See LICENSE.SiFive for license details.

package freechips.rocketchip.groundtest

import Chisel._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule

class TestHarness(implicit p: Parameters) extends Module {
  val io = new Bundle { val success = Bool(OUTPUT) }
  val dut = Module(LazyModule(new GroundTestTop).module)
  io.success := dut.io_success
  dut.connectSimAXIMem()
}
