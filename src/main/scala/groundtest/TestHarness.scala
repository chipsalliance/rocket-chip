package groundtest

import Chisel._
import diplomacy._
import cde.Parameters
import rocketchip._
import util._

class TestHarness(q: Parameters) extends Module {
  val io = new Bundle {
    val success = Bool(OUTPUT)
  }
  implicit val p = q

  val dut = Module(LazyModule(new GroundTestTop(new GroundTestCoreplex()(_))).module)
  io.success := dut.io.success

  if (dut.io.mem_axi4.nonEmpty) {
    val memSize = p(ExtMemSize)
    require(memSize % dut.io.mem_axi4.size == 0)
    for (axi <- dut.io.mem_axi4.map(_(0))) {
      val mem = Module(new SimAXIMem(memSize / dut.io.mem_axi4.size))
      mem.io.axi.ar <> axi.ar
      mem.io.axi.aw <> axi.aw
      mem.io.axi.w  <> axi.w
      axi.r <> LatencyPipe(mem.io.axi.r, p(SimMemLatency))
      axi.b <> LatencyPipe(mem.io.axi.b, p(SimMemLatency))
    }
  }
}
