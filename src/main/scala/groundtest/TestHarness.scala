// See LICENSE.SiFive for license details.

package groundtest

import Chisel._
import diplomacy._
import config._
import rocketchip._
import util._

class TestHarness(implicit p: Parameters) extends Module {
  val io = new Bundle {
    val success = Bool(OUTPUT)
  }

  val dut = Module(LazyModule(new GroundTestTop).module)
  io.success := dut.io.success

  val channels = p(coreplex.BankedL2Config).nMemoryChannels
  if (channels > 0) Module(LazyModule(new SimAXIMem(channels)).module).io.axi4 <> dut.io.mem_axi4
}
