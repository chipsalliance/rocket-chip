// See LICENSE for license details.

package hurricane 

import Chisel._
import cde.{Parameters, Field}
import rocket.Util._
import util._
import testchipip._
import coreplex._
import uncore.tilelink2._
import uncore.tilelink._
import uncore.agents._
import uncore.devices._
import junctions._
import hbwif._
import rocketchip._

class TestHarness(q: Parameters) extends Module {
  val io = new Bundle {
    val success = Bool(OUTPUT)
  }
  val dut = q(BuildHTop)(q).module
  implicit val p = dut.p

  val memParams = p.alterPartial({case TLId => "LBWIF"})
  val memSize = p(GlobalAddrMap)("mem").size
  val memBytes = p(TLKey(memParams(TLId))).dataBitsPerBeat / 8
  val memDepth = memSize / memBytes
  val sim_tl_mem = Module(new TileLinkTestRAM(memDepth.toInt)(memParams))
  val dessert = Module(new ClientUncachedTileLinkIOBidirectionalSerdes(p(NarrowWidth))(memParams))

  val host_clock = dut.io.host_clock.asClock
  dessert.io.serial.in <> AsyncDecoupledFrom(host_clock, reset, dut.io.serial.out)
  dut.io.serial.in <> AsyncDecoupledTo(host_clock, reset, dessert.io.serial.out)
  sim_tl_mem.io <> dessert.io.tl_client
  // dessert.io.tl_manager <> ...

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
}
