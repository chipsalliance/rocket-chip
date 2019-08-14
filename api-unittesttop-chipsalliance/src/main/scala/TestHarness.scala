// See LICENSE.SiFive for license details.

package freechips.rocketchip.unittest

import Chisel._
import freechips.rocketchip.config.Parameters

class TestHarness(implicit val p: Parameters) extends Module {
  val io = new Bundle { val success = Bool(OUTPUT) }
  io.success := Module(new UnitTestSuite).io.finished
}
