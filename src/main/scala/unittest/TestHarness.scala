// See LICENSE for license details.

package unittest

import Chisel._
import cde.Parameters
import rocketchip._

class TestHarness(implicit val p: Parameters) extends Module {
  val io = new Bundle {
    val success = Bool(OUTPUT)
  }

  val l1params = p.alterPartial({
    case NCoreplexExtClients => 0
    case ConfigString => ""
    case uncore.tilelink.TLId => "L1toL2" })
  val tests = Module(new UnitTestSuite()(l1params))

  io.success := tests.io.finished
}
