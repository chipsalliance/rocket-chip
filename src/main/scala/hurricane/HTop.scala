// See LICENSE for license details.

package hurricane 

import Chisel._
import cde.{Parameters, Field}
import util._
import testchipip._
import coreplex._
import diplomacy.LazyModule
import uncore.tilelink._
import uncore.agents._
import uncore.devices.{NTiles, AsyncDebugBusTo}
import uncore.converters.TileLinkWidthAdapter
import uncore.util.TileLinkEnqueuer
import junctions._
import hbwif._
import rocketchip._

case object BuildHTop extends Field[Parameters => HUpTop]

/* Hurricane Upstream Chisel Top */
class HUpTop(q: Parameters) extends BaseTop(q)
    with HurricaneExtraTopLevel
    with PeripheryBootROM
    with PeripheryDebug
    with PeripheryCoreplexLocalInterrupter
    with HurricaneIF
    with Hbwif
    with SCRResetVector {
  pDevices.add(AddrMapEntry("scrbus", new AddrMap(
    scrDevices.get, start = BigInt(0x60000000L), collapse=true)))

  topLevelSCRBuilder.addControl("coreplex_reset", UInt(1))
  for (i <- 0 until p(NTiles) - 1) {
    topLevelSCRBuilder.addControl(s"core_${i}_reset", UInt(1))
    topLevelSCRBuilder.addControl(s"core_${i}_rocc_reset", UInt(1))
  }
  topLevelSCRBuilder.addControl("pmu_reset", UInt(1))
  topLevelSCRBuilder.addControl("hbwif_reset", UInt(1))
  //                                                         hold      divisor
  topLevelSCRBuilder.addControl("slow_clock_divide", UInt("x00000005_00000014"))
  for (i <- 0 until p(NMemoryChannels)) {
    topLevelSCRBuilder.addControl(s"switcher_channel_$i", UInt(0))
  }
  override lazy val module = Module(new HUpTopModule(p, this, new HUpTopBundle(p)))
}

class HUpTopBundle(p: Parameters) extends BaseTopBundle(p)
    with PeripheryBootROMBundle
    with PeripheryDebugBundle
    with PeripheryCoreplexLocalInterrupterBundle
    with HurricaneIFBundle
    with HbwifBundle

class HUpTopModule[+L <: HUpTop, +B <: HUpTopBundle]
    (p: Parameters, l: L, b: => B) extends BaseTopModule(p, l, b)
    with HurricaneExtraTopLevelModule
    with PeripheryBootROMModule
    with PeripheryDebugModuleMC
    with PeripheryCoreplexLocalInterrupterModuleMC
    with HurricaneIFModule
    with HbwifModule
    with AsyncConnection
    with SCRResetVectorModule {
  val multiClockCoreplexIO = coreplexIO.asInstanceOf[MultiClockCoreplexBundle]

  system_clock := clock
  system_reset := ResetSync(reset, system_clock)

  coreplex.clock := clock
  coreplex.reset := ResetSync(scr.control("coreplex_reset")(0).toBool, coreplex.clock)

  multiClockCoreplexIO.tcrs.dropRight(1).zipWithIndex foreach { case (tcr, i) =>
    tcr.clock := clock
    tcr.reset := ResetSync(scr.control(s"core_${i}_reset")(0).toBool, tcr.clock)
    tcr.roccClock := clock
    tcr.roccReset := ResetSync(scr.control(s"core_${i}_rocc_reset")(0).toBool, tcr.roccClock)
  }
  multiClockCoreplexIO.tcrs.last.clock := system_clock
  multiClockCoreplexIO.tcrs.last.reset := scr.control(s"pmu_reset")(0).toBool

  // Hbwif connections
  hbwifFastClock := clock
  hbwifReset := scr.control("hbwif_reset")(0).toBool
}

/////

trait HurricaneExtraTopLevel extends LazyModule {
  implicit val p: Parameters
  val scrDevices = new ResourceManager[AddrMapEntry]
  val topLevelSCRBuilder = new SCRBuilder("SCRTop")

  scrDevices.add(AddrMapEntry("HSCRFile", MemSize(BigInt(p(HSCRFileSize)), MemAttr(AddrMapProt.RW))))
}

trait HurricaneExtraTopLevelModule extends HasPeripheryParameters {
  implicit val p: Parameters
  val hbwifFastClock: Clock = Wire(Clock())
  val outer: HurricaneExtraTopLevel
  val pBus: TileLinkRecursiveInterconnect
  val system_clock: Clock = Wire(Clock())
  val system_reset: Bool = Wire(Bool())

  val scrParams = p.alterPartial({ case TLId => "MMIOtoSCR" })

  val scrBus = Module(new TileLinkRecursiveInterconnect(
    2, p(GlobalAddrMap).subMap("io:pbus:scrbus"), c = system_clock, r = system_reset)(scrParams))

  //SCR file generation
  val scr = outer.topLevelSCRBuilder.generate(
      p(GlobalAddrMap)("io:pbus:scrbus:HSCRFile").start, c = system_clock, r = system_reset)(scrParams)
  scr.io.tl <> scrBus.port("HSCRFile")
  val pBusPort = TileLinkEnqueuer(pBus.port("scrbus"), 1, c = system_clock, r = system_reset)
  TileLinkWidthAdapter(scrBus.io.in(0), pBusPort, system_clock, system_reset)
  val lbscrTL = scrBus.io.in(1)
}

/////

trait HurricaneIF extends LazyModule {
  implicit val p: Parameters
  val pBusMasters: RangeManager

  pBusMasters.add("lbwif", 1)
}

trait HurricaneIFBundle extends HasPeripheryParameters {
  implicit val p: Parameters
  val serial = new SerialIO(p(NarrowWidth))
  val host_clock = Bool(OUTPUT)
}

trait HurricaneIFModule extends HasPeripheryParameters {
  implicit val p: Parameters
  val numLanes = p(HbwifKey).numLanes

  val outer: HurricaneIF
  val io: HurricaneIFBundle
  val coreplexIO: BaseCoreplexBundle
  val lbscrTL: ClientUncachedTileLinkIO
  val scr: SCRFile
  val system_clock: Clock
  val system_reset: Bool
  val hbwifIO = Wire(Vec(numLanes, new ClientUncachedTileLinkIO()(outerMMIOParams)))

  val lbwifWidth = p(NarrowWidth)
  val lbwifParams = p.alterPartial({ case TLId => "LBWIF" })
  val switcherParams = p.alterPartial({ case TLId => "Switcher" })

  val unmapper = Module(new ChannelAddressUnmapper(nMemChannels, c = system_clock, r = system_reset)(switcherParams))
  val switcher = Module(new ClientUncachedTileLinkIOSwitcher(
    nMemChannels, numLanes+2, c = system_clock, r = system_reset)(switcherParams))
  val lbwif = Module(
    new ClientUncachedTileLinkIOBidirectionalSerdes(lbwifWidth, system_clock, system_reset)(lbwifParams))

  unmapper.io.in <> coreplexIO.master.mem
  switcher.io.in <> unmapper.io.out

  def lbwifRouteSel(addr: UInt) =
    Mux(p(GlobalAddrMap).isInRegion("io:pbus:scrbus",addr), UInt(2), UInt(1))

  val lbwif_router = Module(new ClientUncachedTileLinkIORouter(
    2, lbwifRouteSel, c = system_clock, r = system_reset)(lbwifParams))
  val (r_start, r_end) = outer.pBusMasters.range("lbwif")

  lbwif.io.tl_manager <> switcher.io.out(0)
  lbwif_router.io.in <> lbwif.io.tl_client
  TileLinkWidthAdapter(lbscrTL, lbwif_router.io.out(1), system_clock, system_reset)
  coreplexIO.slave(r_start) <> lbwif_router.io.out(0)

  val slowio_module = Module(new SlowIO(p(SlowIOMaxDivide), c = system_clock, r = system_reset)(UInt(width=lbwifWidth)))

  lbwif.io.serial.in <> slowio_module.io.in_fast
  slowio_module.io.out_fast <> lbwif.io.serial.out
  slowio_module.io.in_slow <> io.serial.in
  io.serial.out <> slowio_module.io.out_slow
  io.host_clock := slowio_module.io.clk_slow

  slowio_module.io.divisor := scr.control("slow_clock_divide")(31,0)
  slowio_module.io.hold := scr.control("slow_clock_divide")(63, 32)

  val ser = (0 until numLanes) map { i =>
    hbwifIO(i) <> switcher.io.out(i+1)
  }

  for (i <- 0 until nMemChannels) {
    switcher.io.select(i) := scr.control(s"switcher_channel_$i")(log2Up(numLanes+2),0)
  }
}

trait AsyncConnection {
  val coreplexIO: BaseCoreplexBundle
  val coreplex: BaseCoreplexModule[BaseCoreplex, BaseCoreplexBundle]

  coreplexIO.master.mem.zip(coreplex.io.master.mem).map {
    case(out, in) => out <> AsyncUTileLinkFrom(coreplex.clock, coreplex.reset, in, sync = 2)
  }
  coreplexIO.master.mmio <> AsyncUTileLinkFrom(coreplex.clock, coreplex.reset, coreplex.io.master.mmio, sync = 2)
  coreplex.io.slave.zip(coreplexIO.slave).map {
    case(out, in) => out <> AsyncUTileLinkTo(coreplex.clock, coreplex.reset, in, sync = 2)
  }
  coreplexIO.interrupts.zip(coreplex.io.interrupts).map {
    case(out, in) => out <> LevelSyncFrom(coreplex.clock, in, sync = 2)
  }
  coreplex.io.debug <> AsyncDebugBusTo(coreplex.clock, coreplex.reset, coreplexIO.debug, sync = 2)
  coreplex.io.clint.zip(coreplexIO.clint).map {
    case(out, in) => {
      out.mtip := LevelSyncTo(coreplex.clock, in.mtip, sync = 2)
      out.msip := LevelSyncTo(coreplex.clock, in.msip, sync = 2)
    }
  }
  coreplex.io.resetVector := coreplexIO.resetVector
  coreplexIO.success := coreplex.io.success

  val multiClockIO = coreplexIO.asInstanceOf[MultiClockCoreplexBundle]
  val multiClock = coreplex.asInstanceOf[MultiClockCoreplexModule[MultiClockCoreplex, MultiClockCoreplexBundle]]

  multiClockIO.tcrs <> multiClock.io.tcrs
}

trait SCRResetVector extends LazyModule {
  implicit val p: Parameters
  val topLevelSCRBuilder: SCRBuilder

  topLevelSCRBuilder.addControl("reset_vector", UInt(0x1000)) // boot ROM
}

trait SCRResetVectorModule extends HasPeripheryParameters {
  implicit val p: Parameters
  val coreplexIO: BaseCoreplexBundle
  val scr: SCRFile

  coreplexIO.resetVector := scr.control("reset_vector")
}
