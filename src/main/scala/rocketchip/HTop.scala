// See LICENSE for license details.

package rocketchip

import Chisel._
import cde.{Parameters, Field}
import rocket.Util._
import util._
import testchipip._
import coreplex._
import uncore.tilelink2._
import uncore.tilelink._
import junctions._

case object BuildHTop extends Field[Parameters => HTop]

/* Hurricane Chisel Top */
class HTop(q: Parameters) extends BaseTop(q)
    with PeripheryBootROM // TODOHurricane: Is this neccessary for non-standalone boot
    with PeripheryDebug
    with PeripheryCoreplexLocalInterrupter
    with PeripheryMasterMem
    with PeripheryMasterMMIO
    with PeripherySlave { //TODOHurricane: Do we need this?/What is it for?
  override lazy val module = Module(new HTopModule(p, this, new HTopBundle(p)))
}

class HTopBundle(p: Parameters) extends BaseTopBundle(p)
    with PeripheryBootROMBundle
    with PeripheryDebugBundle
    with PeripheryCoreplexLocalInterrupterBundle
    with PeripheryMasterMemBundle
    with PeripheryMasterMMIOBundle
    with PeripherySlaveBundle
//TODOHurricane: add DRAM I/Os here

class HTopModule[+L <: HTop, +B <: HTopBundle]
    (p: Parameters, l: L, b: => B) extends BaseTopModule(p, l, b) {
  val multiClockCoreplexIO = coreplexIO.asInstanceOf[MultiClockCoreplexBundle]

  coreplex.clock := clock

  multiClockCoreplexIO.tcrs foreach { tcr =>
    tcr.clock := clock
    tcr.reset := reset
  }
  multiClockCoreplexIO.extcr.clock := clock
  multiClockCoreplexIO.extcr.reset := reset
}


class HTestHarness(q: Parameters) extends Module {
  val io = new Bundle {
    val success = Bool(OUTPUT)
  }
  val dut = q(BuildHTop)(q).module
  implicit val p = dut.p

  // This test harness isn't especially flexible yet
  require(dut.io.mem_clk.isEmpty)
  require(dut.io.mem_rst.isEmpty)
  require(dut.io.mem_ahb.isEmpty)
  require(dut.io.mem_tl.isEmpty)
  require(dut.io.bus_clk.isEmpty)
  require(dut.io.bus_rst.isEmpty)
  require(dut.io.mmio_clk.isEmpty)
  require(dut.io.mmio_rst.isEmpty)
  require(dut.io.mmio_ahb.isEmpty)
  require(dut.io.mmio_tl.isEmpty)

  if (dut.io.mem_axi.nonEmpty) {
    val memSize = p(GlobalAddrMap)("mem").size
    require(memSize % dut.io.mem_axi.size == 0)
    for (axi <- dut.io.mem_axi) {
      val mem = Module(new SimAXIMem(memSize / dut.io.mem_axi.size))
      mem.io.axi.ar <> axi.ar
      mem.io.axi.aw <> axi.aw
      mem.io.axi.w  <> axi.w
      axi.r <> LatencyPipe(mem.io.axi.r, p(SimMemLatency))
      axi.b <> LatencyPipe(mem.io.axi.b, p(SimMemLatency))
    }
  }

  if (p(NarrowIF)) {
    val memSize = p(GlobalAddrMap)("mem").size
    val dessert = Module(new ClientUncachedTileLinkIODesser(p(NarrowWidth))(p.alterPartial({case TLId => "Outermost"})))
    //dessert.io.serial <> dut.io.mem_narrow.get // TODOHurricane - Howie says to wire in and out separately for SerialIO (throws GenderCheck errors)
    val sim_axi = Module(new SimAXIMem(memSize))
    // HurricaneTODO - should we convert TL to AXI here, or is there a "SimTLMem"?
  }

  if (!p(IncludeJtagDTM)) {
    // Todo: enable the usage of different clocks
    // to test the synchronizer more aggressively.
    val dtm_clock = clock
    val dtm_reset = reset
    if (dut.io.debug_clk.isDefined) dut.io.debug_clk.get := dtm_clock
    if (dut.io.debug_rst.isDefined) dut.io.debug_rst.get := dtm_reset
    val dtm = Module(new SimDTM).connect(dtm_clock, dtm_reset, dut.io.debug.get,
      dut.io.success, io.success)
  } else {
    val jtag = Module(new JTAGVPI).connect(dut.io.jtag.get, reset, io.success)
  }

  for (bus_axi <- dut.io.bus_axi) {
    bus_axi.ar.valid := Bool(false)
    bus_axi.aw.valid := Bool(false)
    bus_axi.w.valid  := Bool(false)
    bus_axi.r.ready  := Bool(false)
    bus_axi.b.ready  := Bool(false)
  }

  for (mmio_axi <- dut.io.mmio_axi) {
    val slave = Module(new NastiErrorSlave)
    slave.io <> mmio_axi
  }

}


