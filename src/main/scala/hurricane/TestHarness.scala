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

  val hbwifNumLanes = p(HbwifKey).numLanes

  val sim_tl_mem = Module(new TileLinkTestRAM(memDepth.toInt)(memParams))
  val dessert = Module(new ClientUncachedTileLinkIOBidirectionalSerdes(p(NarrowWidth))(memParams))
  val adapter = Module(new SerialAdapter()(memParams))
  val serial = Module(new SimSerialWrapper(p(SerialInterfaceWidth)))
  val arbiter = Module(new ClientUncachedTileLinkIOArbiter(hbwifNumLanes+1)(memParams))
  val fiwbh = Module(new Fiwbh)

  fiwbh.io.fastClk := clock
  fiwbh.io.loopback := Bool(false)
  fiwbh.io.rx <> dut.io.hbwifTx
  dut.io.hbwifRx <> fiwbh.io.tx
  dut.io.hbwifIref.get := Bool(true)
  arbiter.io.in.tail.zip(fiwbh.io.mem).foreach { case (h, f) => h <> f }

  val host_clock = dut.io.host_clock.asClock
  val host_clock_toggle = dut.io.host_clock =/= RegNext(dut.io.host_clock)
  val (_, host_clock_wrap) = Counter(host_clock_toggle, 4)
  val host_reset = Reg(init = Bool(true))
  when(host_clock_wrap) { host_reset := Bool(false) }
  dessert.io.serial.in <> AsyncDecoupledFrom(host_clock, host_reset, dut.io.serial.out)
  dut.io.serial.in <> AsyncDecoupledTo(host_clock, host_reset, dessert.io.serial.out)
  sim_tl_mem.io <> dessert.io.tl_client
  dessert.io.tl_manager <> arbiter.io.out
  arbiter.io.in.head <> adapter.io.mem
  serial.io.serial <> adapter.io.serial

  // We don't need this for simulation
  dut.io.debug.map { dbg =>
    dbg.req.valid := Bool(false)
    dbg.resp.ready := Bool(false)
  }

  io.success := serial.io.exit
}
