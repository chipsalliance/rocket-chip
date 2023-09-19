// See LICENSE.SiFive for license details.

package freechips.rocketchip.unittest

import chisel3._
import org.chipsalliance.cde.config.Parameters

class TestHarness(implicit val p: Parameters) extends Module {
  val io = IO(new Bundle { val success = Output(Bool()) })
  io.success := Module(new UnitTestSuite).io.finished
}
