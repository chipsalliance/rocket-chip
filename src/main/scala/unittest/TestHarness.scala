// See LICENSE.SiFive for license details.

package unittest

import Chisel._
import config._

class TestHarness(implicit val p: Parameters) extends Module {
  val io = new Bundle { val success = Bool(OUTPUT) }
  io.success := Module(new UnitTestSuite).io.finished
}
