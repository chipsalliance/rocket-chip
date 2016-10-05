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
    with PeripheryBootROM
    with PeripheryDebug
    with PeripheryCoreplexLocalInterrupter
    with HurricaneExtraTopLevel
    with HurricaneIF
    with Hbwif {
  pDevices.add(AddrMapEntry("scrbus", new AddrMap(
    scrDevices.get, start = BigInt(0x60000000L), collapse=true)))

  topLevelSCRBuilder.addControl("coreplex_reset", UInt(1))
  for (i <- 0 until p(NTiles) - 1) {
    topLevelSCRBuilder.addControl(s"core_${i}_reset", UInt(1))
    topLevelSCRBuilder.addControl(s"core_${i}_rocc_reset", UInt(1))
  }
  topLevelSCRBuilder.addControl("pmu_reset", UInt(1))
  topLevelSCRBuilder.addControl("slow_clock_divide", UInt(0))
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
//TODOHurricane: add DRAM I/Os here

class HUpTopModule[+L <: HUpTop, +B <: HUpTopBundle]
    (p: Parameters, l: L, b: => B) extends BaseTopModule(p, l, b)
    with PeripheryBootROMModule
    with PeripheryDebugModule
    with PeripheryCoreplexLocalInterrupterModule
    with HurricaneExtraTopLevelModule
    with HurricaneIFModule
    with HbwifModule
    with AsyncConnection
    with HardwiredResetVector {
  val multiClockCoreplexIO = coreplexIO.asInstanceOf[MultiClockCoreplexBundle]

  coreplex.clock := clock
  coreplex.reset := ResetSync(scr.control("coreplex_reset")(0).toBool, coreplex.clock)

  multiClockCoreplexIO.tcrs.dropRight(1).zipWithIndex foreach { case (tcr, i) =>
    tcr.clock := clock
    tcr.reset := ResetSync(scr.control(s"core_${i}_reset")(0).toBool, tcr.clock)
    tcr.roccClock := clock
    tcr.roccReset := ResetSync(scr.control(s"core_${i}_rocc_reset")(0).toBool, tcr.roccClock)
  }
  multiClockCoreplexIO.tcrs.last.reset := scr.control(s"pmu_reset")(0).toBool

  // Hbwif connections
  hbwifFastClock := clock
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

  val scrParams = p.alterPartial({ case TLId => "MMIOtoSCR" })

  val scrBus = Module(new TileLinkRecursiveInterconnect(
    2, p(GlobalAddrMap).subMap("io:pbus:scrbus"))(scrParams))

  //SCR file generation
  val scr = outer.topLevelSCRBuilder.generate(scrParams)
  scr.io.tl <> scrBus.port("HSCRFile")
  val pBusPort = TileLinkEnqueuer(pBus.port("scrbus"), 1)
  TileLinkWidthAdapter(scrBus.io.in(0), pBusPort)
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
  val hbwifIO = Wire(Vec(numLanes, new ClientUncachedTileLinkIO()(outerMMIOParams)))

  val lbwifWidth = p(NarrowWidth)
  val lbwifParams = p.alterPartial({ case TLId => "LBWIF" })
  val switcherParams = p.alterPartial({ case TLId => "Switcher" })

  val unmapper = Module(new ChannelAddressUnmapper(nMemChannels)(switcherParams))
  val switcher = Module(new ClientUncachedTileLinkIOSwitcher(
    nMemChannels, numLanes+1)(switcherParams))
  val lbwif = Module(
    new ClientUncachedTileLinkIOBidirectionalSerdes(lbwifWidth)(lbwifParams))

  unmapper.io.in <> coreplexIO.master.mem
  switcher.io.in <> unmapper.io.out

  def lbwifRouteSel(addr: UInt) =
    UIntToOH(p(GlobalAddrMap).isInRegion("io:pbus:scrbus",addr))

  val lbwif_router = Module(new ClientUncachedTileLinkIORouter(
    2, lbwifRouteSel)(lbwifParams))
  val (r_start, r_end) = outer.pBusMasters.range("lbwif")

  lbwif.io.tl_manager <> switcher.io.out(0)
  lbwif_router.io.in <> lbwif.io.tl_client
  TileLinkWidthAdapter(lbscrTL, lbwif_router.io.out(1))
  coreplexIO.slave(r_start) <> lbwif_router.io.out(0)

  val slowio_module = Module(new SlowIO(p(SlowIOMaxDivide))(UInt(width=lbwifWidth)))

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
    switcher.io.select(i) := scr.control(s"switcher_channel_$i")(log2Up(nMemChannels),0)
  }
}

trait AsyncConnection {
  val coreplexIO: BaseCoreplexBundle
  val coreplex: BaseCoreplexModule[BaseCoreplex, BaseCoreplexBundle]

  coreplexIO.master.mem.zip(coreplex.io.master.mem).map {
    case(out, in) => out <> AsyncUTileLinkFrom(coreplex.clock, coreplex.reset, in)
  }
  coreplexIO.master.mmio <> AsyncUTileLinkFrom(coreplex.clock, coreplex.reset, coreplex.io.master.mmio)
  coreplex.io.slave.zip(coreplexIO.slave).map {
    case(out, in) => out <> AsyncUTileLinkTo(coreplex.clock, coreplex.reset, in)
  }
  coreplexIO.interrupts.zip(coreplex.io.interrupts).map {
    case(out, in) => out <> LevelSyncFrom(coreplex.clock, in)
  }
  coreplex.io.debug <> AsyncDebugBusTo(coreplex.clock, coreplex.reset, coreplexIO.debug)
  coreplex.io.clint.zip(coreplexIO.clint).map {
    case(out, in) => {
      out.mtip := LevelSyncTo(coreplex.clock, in.mtip)
      out.msip := LevelSyncTo(coreplex.clock, in.msip)
    }
  }
  coreplex.io.resetVector := coreplexIO.resetVector
  coreplexIO.success := coreplex.io.success
}
