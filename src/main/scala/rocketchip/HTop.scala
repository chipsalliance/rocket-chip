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
import uncore.agents._
import junctions._
import hbwif._

case object BuildHTop extends Field[Parameters => HTop]

/* Hurricane Chisel Top */
class HTop(q: Parameters) extends BaseTop(q)
    with PeripheryBootROM // TODOHurricane: Is this neccessary for non-standalone boot
    with PeripheryDebug
    with PeripheryCoreplexLocalInterrupter
    with HurricaneIF
    with HurricaneExtraTopLevel
    with Hbwif
    with PeripheryMasterMMIO
    with PeripherySlave { //TODOHurricane: Do we need this?/What is it for?
  override lazy val module = Module(new HTopModule(p, this, new HTopBundle(p)))
}

class HTopBundle(p: Parameters) extends BaseTopBundle(p)
    with PeripheryBootROMBundle
    with PeripheryDebugBundle
    with PeripheryCoreplexLocalInterrupterBundle
    with HurricaneIFBundle
    with HbwifBundle
    with PeripheryMasterMMIOBundle
    with PeripherySlaveBundle
//TODOHurricane: add DRAM I/Os here

class HTopModule[+L <: HTop, +B <: HTopBundle]
    (p: Parameters, l: L, b: => B) extends BaseTopModule(p, l, b)
    with PeripheryBootROMModule
    with PeripheryDebugModule
    with PeripheryCoreplexLocalInterrupterModule
    with HurricaneIFModule
    with HurricaneExtraTopLevelModule
    with HbwifModule
    with PeripheryMasterMMIOModule
    with PeripherySlaveModule
    with HardwiredResetVector {
  val multiClockCoreplexIO = coreplexIO.asInstanceOf[MultiClockCoreplexBundle]

  coreplex.clock := clock
  coreplex.reset := ResetSync(topLevelSCRBuilder.control("coreplex_reset", UInt(1))(0).toBool, coreplex.clock)

  multiClockCoreplexIO.tcrs.dropRight(1).zipWithIndex foreach { case (tcr, i) =>
    tcr.clock := clock
    tcr.reset := ResetSync(topLevelSCRBuilder.control(s"core_${i}_reset", UInt(1))(0).toBool, tcr.clock)
  }
  multiClockCoreplexIO.tcrs.last.reset := topLevelSCRBuilder.control(s"pmu_reset", UInt(1))(0).toBool

  //TODOHurricane: if we can't assign to toplevel clock assign to tcr.last
  multiClockCoreplexIO.extcr.clock := clock
  multiClockCoreplexIO.extcr.reset := reset

  // Hbwif connections
  hbwifFastClock := clock

  //SCR file generation
  val scrTL = topLevelSCRBuilder.generate(outermostMMIOParams)

  scrTL <> pBus.port("HSCRFile")
}
/////
trait HurricaneExtraTopLevel extends LazyModule {
  implicit val p: Parameters
  val pDevices: ResourceManager[AddrMapEntry]

  pDevices.add(AddrMapEntry(s"HSCRFile", MemSize(BigInt(p(HSCRFileSize)), MemAttr(AddrMapProt.RW))))
}

trait HurricaneExtraTopLevelModule {
  implicit val p: Parameters
  val hbwifFastClock: Clock = Wire(Clock())
  val topLevelSCRBuilder: SCRBuilder = new SCRBuilder
}

class HTestHarness(q: Parameters) extends Module {
  val io = new Bundle {
    val success = Bool(OUTPUT)
  }
  val dut = q(BuildHTop)(q).module
  implicit val p = dut.p

  // This test harness isn't especially flexible yet
  require(dut.io.bus_clk.isEmpty)
  require(dut.io.bus_rst.isEmpty)
  require(dut.io.mmio_clk.isEmpty)
  require(dut.io.mmio_rst.isEmpty)
  require(dut.io.mmio_ahb.isEmpty)
  require(dut.io.mmio_tl.isEmpty)

  val memSize = p(GlobalAddrMap)("mem").size
  val dessert = Module(new ClientUncachedTileLinkIODesser(p(NarrowWidth))(p.alterPartial({case TLId => "Outermost"})))
  //dessert.io.serial <> dut.io.mem_narrow.get // TODOHurricane - Howie says to wire in and out separately for SerialIO (throws GenderCheck errors)
  val sim_axi = Module(new SimAXIMem(memSize))
  // HurricaneTODO - should we convert TL to AXI here, or is there a "SimTLMem"?

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

/////

trait HurricaneIF extends LazyModule {
  implicit val p: Parameters
}

trait HurricaneIFBundle extends HasPeripheryParameters {
  implicit val p: Parameters
  val narrowIO = new SerialIO(p(NarrowWidth)) //TODOHurricane - this should be NarrowIO, not SerialIO
}

trait HurricaneIFModule extends HasPeripheryParameters {
  implicit val p: Parameters
  val numLanes = p(HbwifKey).numLanes

  val outer: HurricaneIF
  val io: HurricaneIFBundle
  val coreplexIO: BaseCoreplexBundle
  val hbwifIO: Vec[ClientUncachedTileLinkIO] = Wire(Vec(numLanes,
    new ClientUncachedTileLinkIO()(p.alterPartial({case TLId => "Outermost"}))))
  // TODOHurricane - implement the TL master/slave combined version
  require(p(NAcquireTransactors) > 2 || numLanes < 8)
  // TODOHurricane - why doesn't the switcher handle this gracefully
  val nBanks = nMemChannels*p(NBanksPerMemoryChannel)
  val switcher = Module(new ClientUncachedTileLinkIOSwitcher(nBanks, numLanes+1)
      (p.alterPartial({case TLId => "Outermost"})))
  switcher.io.in <> coreplexIO.master.mem
  val lbwif = Module(new ClientUncachedTileLinkIOSerdes(p(NarrowWidth))(p.alterPartial({case TLId => "Outermost"})))

  lbwif.io.tl <> switcher.io.out(0)
  io.narrowIO <> lbwif.io.serial

  val ser = (0 until numLanes) map { i =>
    hbwifIO(i) <> switcher.io.out(i+1)
  }
  // io.mem_narrow.get <> ser(0).io.serial // TODOHurricane - Howie says to wire in and out separately for SerialIO
  // TODOHurricane - wire up the HBWIF lanes
  // switcher.io.select(0) := ... // TODOHurricane - Need to hardcode all banks to route to channel 0, but it's unclear how to do this.
                                  // Eventually this should be configurable via SCR
}
