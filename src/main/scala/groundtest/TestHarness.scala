// See LICENSE.SiFive for license details.

package groundtest

import Chisel._
import diplomacy._
import config._
import rocketchip._
import util._

class TestHarness(implicit p: Parameters) extends Module {
  val io = new Bundle { val success = Bool(OUTPUT) }
  val dut = Module(LazyModule(new GroundTestTop).module)
  io.success := dut.io_success
  dut.connectSimAXIMem()
}
