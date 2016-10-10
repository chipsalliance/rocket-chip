// See LICENSE for license details.

package hurricane 

import Chisel._
import cde.{Parameters, Field}
import util._
import testchipip._
import coreplex._
import uncore.tilelink2._
import uncore.tilelink._
import uncore.agents._
import uncore.devices._
import uncore.util._
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
  val adapter = Module(new SerialAdapter()(memParams))
  val serial = Module(new SimSerialWrapper(p(SerialInterfaceWidth)))

  val host_clock = dut.io.host_clock.asClock
  dessert.io.serial.in <> AsyncDecoupledFrom(host_clock, reset, dut.io.serial.out)
  dut.io.serial.in <> AsyncDecoupledTo(host_clock, reset, dessert.io.serial.out)
  sim_tl_mem.io <> dessert.io.tl_client
  dessert.io.tl_manager <> adapter.io.mem
  serial.io.serial <> adapter.io.serial

  // We don't need this for simulation
  dut.io.debug.map { dbg =>
    dbg.req.valid := Bool(false)
    dbg.resp.ready := Bool(false)
  }

  io.success := serial.io.exit
}
