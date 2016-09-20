// See LICENSE for license details.

package unittest

import Chisel._

class TestHarness(implicit val p: cde.Parameters) extends Module {
  val io = new Bundle { val success = Bool(OUTPUT) }
  io.success := Module(new UnitTestSuite).io.finished
}
