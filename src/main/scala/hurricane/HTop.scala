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
import uncore.converters.{TileLinkWidthAdapter, TileLinkIOUnwrapper}
import uncore.util.{TileLinkEnqueuer, TileLinkDepths, UncachedTileLinkDepths}
import junctions._
import hbwif._
import rocketchip._
import rocket._

case object BuildHTop extends Field[Parameters => HUpTop]

trait HasSCRRouter {
  implicit val p: Parameters

  def scrRouteSel(addr: UInt) =
    Mux(p(GlobalAddrMap).isInRegion("io:pbus:scrbus",addr), UInt(2), UInt(1))

  def splitSCRPort(
      scrBusPort: ClientUncachedTileLinkIO,
      pBusPort: ClientUncachedTileLinkIO,
      masterPort: ClientUncachedTileLinkIO) {
    val router = Module(
      new ClientUncachedTileLinkIORouter(2, scrRouteSel)(masterPort.p))
    router.io.in <> masterPort
    TileLinkWidthAdapter(pBusPort, router.io.out(0))
    TileLinkWidthAdapter(scrBusPort, router.io.out(1))
  }
}

/* Hurricane Upstream Chisel Top */
class HUpTop(q: Parameters) extends BaseTop(q)
    with HurricaneExtraTopLevel
    with PeripheryBootROM
    with PeripheryDebug
    with PeripheryCoreplexLocalInterrupter
    with HurricaneIF
    with Hbwif
    with SCRResetVector
    with PowerManagementUnit {
  pDevices.add(AddrMapEntry("scrbus", new AddrMap(
    scrDevices.get, start = BigInt(0x60000000L), collapse=true)))

  topLevelSCRBuilder.addControl("coreplex_reset", UInt(1))
  for (i <- 0 until p(NTiles)) {
    topLevelSCRBuilder.addControl(s"core_${i}_reset", UInt(1))
    topLevelSCRBuilder.addControl(s"core_${i}_rocc_reset", UInt(1))
  }
  topLevelSCRBuilder.addControl("hbwif_reset", UInt(1))
  topLevelSCRBuilder.addControl("hbwif_reset_override", UInt(0))
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
    with SCRResetVectorModule
    with PowerManagementUnitModule {
  val multiClockCoreplexIO = coreplexIO.asInstanceOf[MultiClockCoreplexBundle]

  coreplex.clock := clock
  coreplex.reset := ResetSync(scr.control("coreplex_reset")(0).toBool, coreplex.clock)

  multiClockCoreplexIO.tcrs.zipWithIndex foreach { case (tcr, i) =>
    tcr.clock := clock
    tcr.reset := ResetSync(scr.control(s"core_${i}_reset")(0).toBool, tcr.clock)
    tcr.roccClock := clock
    tcr.roccReset := ResetSync(scr.control(s"core_${i}_rocc_reset")(0).toBool, tcr.roccClock)
  }

  // Hbwif connections
  hbwifFastClock := clock
  hbwifReset := scr.control("hbwif_reset")(0).toBool
  hbwifResetOverride := scr.control("hbwif_reset_override")(0).toBool
}

/////

trait HurricaneExtraTopLevel extends LazyModule {
  implicit val p: Parameters
  val scrDevices = new ResourceManager[AddrMapEntry]
  val scrBusMasters = new RangeManager
  val topLevelSCRBuilder = new SCRBuilder("SCRTop")

  scrDevices.add(AddrMapEntry("HSCRFile", MemSize(BigInt(p(HSCRFileSize)), MemAttr(AddrMapProt.RW))))
  scrBusMasters.add("pbus", 1)
}

trait HurricaneExtraTopLevelModule extends HasPeripheryParameters {
  implicit val p: Parameters
  val hbwifFastClock: Clock = Wire(Clock())
  val outer: HurricaneExtraTopLevel
  val pBus: TileLinkRecursiveInterconnect

  val scrParams = p.alterPartial({ case TLId => "MMIOtoSCR" })

  val scrBus = Module(new TileLinkRecursiveInterconnect(
    outer.scrBusMasters.sum, p(GlobalAddrMap).subMap("io:pbus:scrbus"))(scrParams))

  //SCR file generation
  val scr = outer.topLevelSCRBuilder.generate(
      p(GlobalAddrMap)("io:pbus:scrbus:HSCRFile").start)(scrParams)
  scr.io.tl <> scrBus.port("HSCRFile")
  val pBusPort = TileLinkEnqueuer(pBus.port("scrbus"), 1)
  val (pBusStart, _) = outer.scrBusMasters.range("pbus")
  TileLinkWidthAdapter(scrBus.io.in(pBusStart), pBusPort)
}

/////

trait HurricaneIF extends LazyModule {
  implicit val p: Parameters
  val pBusMasters: RangeManager
  val scrBusMasters: RangeManager

  pBusMasters.add("lbwif", 1)
  scrBusMasters.add("lbwif", 1)
}

trait HurricaneIFBundle extends HasPeripheryParameters {
  implicit val p: Parameters
  val serial = new SerialIO(p(NarrowWidth))
  val host_clock = Bool(OUTPUT)
}

trait HurricaneIFModule extends HasPeripheryParameters with HasSCRRouter {
  implicit val p: Parameters
  val numLanes = p(HbwifKey).numLanes

  val outer: HurricaneIF
  val io: HurricaneIFBundle
  val coreplexIO: BaseCoreplexBundle
  val scr: SCRFile
  val scrBus: TileLinkRecursiveInterconnect
  val hbwifIO = Wire(Vec(numLanes, new ClientUncachedTileLinkIO()(outerMMIOParams)))

  val lbwifWidth = p(NarrowWidth)
  val lbwifParams = p.alterPartial({ case TLId => "LBWIF" })
  val switcherParams = p.alterPartial({ case TLId => "Switcher" })

  val unmapper = Module(new ChannelAddressUnmapper(nMemChannels)(switcherParams))
  val switcher = Module(new ClientUncachedTileLinkIOSwitcher(
    nMemChannels, numLanes+2)(switcherParams))
  val lbwif = Module(
    new ClientUncachedTileLinkIOBidirectionalSerdes(lbwifWidth)(lbwifParams))

  unmapper.io.in <> coreplexIO.master.mem
  switcher.io.in <> unmapper.io.out

  val (core_start, _) = outer.pBusMasters.range("lbwif")
  val (scr_start, _) = outer.scrBusMasters.range("lbwif")

  splitSCRPort(
    scrBus.io.in(scr_start),
    coreplexIO.slave(core_start),
    lbwif.io.tl_client)

  lbwif.io.tl_manager <> switcher.io.out(0)

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

trait PowerManagementUnit extends LazyModule {
  implicit val p: Parameters
  val scrBusMasters: RangeManager
  val pBusMasters: RangeManager
  val pDevices: ResourceManager[AddrMapEntry]
  val topLevelSCRBuilder: SCRBuilder 
  val spadSize = 4096

  scrBusMasters.add("pmu", 1)
  pBusMasters.add("pmu", 2)
  pDevices.add(AddrMapEntry("pmu", MemSize(spadSize, MemAttr(AddrMapProt.RWX))))
  topLevelSCRBuilder.addControl("pmu_reset", UInt(1))
}

trait PowerManagementUnitModule extends HasSCRRouter {
  implicit val p: Parameters
  val scr: SCRFile
  val scrBus: TileLinkRecursiveInterconnect
  val pBus: TileLinkRecursiveInterconnect
  val coreplexIO: BaseCoreplexBundle
  val outer: PowerManagementUnit

  val pmu = Module(new RocketTile()(p.alterPartial({
    case DataScratchpadSize => outer.spadSize
    case TileId => 1
    case TLId => "L1toL2"
    case NUncachedTileLinkPorts => 1
    case FPUKey => None
    case BtbKey => BtbParameters(nEntries = 0)
    case DCacheKey => DCacheConfig(nSDQ = 2, nRPQ = 2, nMSHRs = 0)
    case MulDivKey => Some(MulDivConfig(mulUnroll = 1, mulEarlyOut = false, divEarlyOut = false))
    case UseCompressed => true
    case BuildRoCC => Nil
    case DataScratchpadAddrMapKey => (tileId: Int) => "io:pbus:pmu"
    case NSets => outer.spadSize / p(CacheBlockBytes)
    case NWays => 1
    case NTLBEntries => 4
  })))

  // Just some sanity checks
  require(pmu.io.slave.nonEmpty)
  require(pmu.io.cached.size == 1)
  require(pmu.io.uncached.size == 1)

  pmu.io.hartid := UInt(1)
  pmu.io.resetVector := UInt(p(GlobalAddrMap)("io:pbus:pmu").start)
  pmu.io.interrupts := pmu.io.interrupts.fromBits(UInt(0))
  pmu.io.slave.get <> pBus.port("pmu")
  pmu.reset := scr.control("pmu_reset")(0).toBool

  val (scrStart, scrEnd) = outer.scrBusMasters.range("pmu")
  val (coreStart, coreEnd) = outer.pBusMasters.range("pmu")

  val tlBuffering = TileLinkDepths(1,1,2,2,0)
  val utlBuffering = UncachedTileLinkDepths(1,2)
  val cached = TileLinkEnqueuer(pmu.io.cached.head, tlBuffering)
  val uncached = TileLinkEnqueuer(pmu.io.uncached.head, utlBuffering)
  val cachedUnwrapped = TileLinkIOUnwrapper(cached)

  splitSCRPort(
    scrBus.io.in(scrStart),
    coreplexIO.slave(coreStart),
    cachedUnwrapped)
  coreplexIO.slave(coreStart + 1) <> uncached
}
