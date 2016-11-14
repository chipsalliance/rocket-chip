// See LICENSE for license details.

package rocketchip

import Chisel._
import cde.{Parameters, Field, Dump}
import junctions._
import junctions.NastiConstants._
import diplomacy._
import uncore.tilelink._
import uncore.tilelink2._
import uncore.axi4._
import uncore.converters._
import uncore.devices._
import uncore.agents._
import uncore.util._
import util._
import rocket.XLen
import scala.math.max
import coreplex._

/** Options for memory bus interface */
object BusType {
  sealed trait EnumVal
  case object AXI extends EnumVal
  case object AHB extends EnumVal
  case object TL  extends EnumVal
  val busTypes = Seq(AXI, AHB, TL)
}

/** Memory channel controls */
case object TMemoryChannels extends Field[BusType.EnumVal]
/** External Bus controls */
case object NExtBusAXIChannels extends Field[Int]
/** Async configurations */
case object AsyncBusChannels extends Field[Boolean]
case object AsyncDebugBus extends Field[Boolean]
case object AsyncMemChannels extends Field[Boolean]
/** Specifies the size of external memory */
case object ExtMemSize extends Field[Long]
/** Specifies the number of external interrupts */
case object NExtTopInterrupts extends Field[Int]
/** Source of RTC. First bundle is TopIO.extra, Second bundle is periphery.io.extra  **/
case object RTCPeriod extends Field[Int]
/* Specifies the periphery bus configuration */
case class PeripheryBusConfig(arithAMO: Boolean, beatBytes: Int = 4)
case object PeripheryBusKey extends Field[PeripheryBusConfig]
/* Specifies the SOC-bus configuration */
case class SOCBusConfig(beatBytes: Int = 4)
case object SOCBusKey extends Field[SOCBusConfig]

/* Specifies the data and id width at the chip boundary */
case object EdgeDataBits extends Field[Int]
case object EdgeIDBits extends Field[Int]

object PeripheryUtils {
  def addQueueAXI(source: NastiIO) = {
    val sink = Wire(source)
    sink.ar  <> Queue(source.ar, 1)
    sink.aw  <> Queue(source.aw, 1)
    sink.w   <> Queue(source.w)
    source.r <> Queue(sink.r)
    source.b <> Queue(sink.b, 1)
    sink
  }
  def convertTLtoAXI(tl: ClientUncachedTileLinkIO) = {
    val bridge = Module(new NastiIOTileLinkIOConverter()(tl.p))
    bridge.io.tl <> tl
    addQueueAXI(bridge.io.nasti)
  }
  def convertTLtoAHB(tl: ClientUncachedTileLinkIO, atomics: Boolean) = {
    val bridge = Module(new AHBBridge(atomics)(tl.p))
    bridge.io.tl <> tl
    bridge.io.ahb
  }
}

/** Utility trait for quick access to some relevant parameters */
trait HasPeripheryParameters {
  implicit val p: Parameters
  lazy val tMemChannels = p(TMemoryChannels)
  lazy val nMemChannels = p(NMemoryChannels)
  lazy val nMemAXIChannels = if (tMemChannels == BusType.AXI) nMemChannels else 0
  lazy val nMemAHBChannels = if (tMemChannels == BusType.AHB) nMemChannels else 0
  lazy val nMemTLChannels  = if (tMemChannels == BusType.TL)  nMemChannels else 0
  lazy val edgeSlaveParams = p.alterPartial({ case TLId => "EdgetoSlave" })
  lazy val edgeMemParams = p.alterPartial({ case TLId => "MCtoEdge" })
  lazy val peripheryBusConfig = p(PeripheryBusKey)
  lazy val socBusConfig = p(SOCBusKey)
  lazy val cacheBlockBytes = p(CacheBlockBytes)
}

/////

trait PeripheryDebug {
  this: TopNetwork =>
}

trait PeripheryDebugBundle {
  this: TopNetworkBundle {
    val outer: PeripheryDebug
  } =>
  val debug_clk = (p(AsyncDebugBus) && !p(IncludeJtagDTM)).option(Clock(INPUT))
  val debug_rst = (p(AsyncDebugBus) && !p(IncludeJtagDTM)).option(Bool(INPUT))
  val debug = (!p(IncludeJtagDTM)).option(new DebugBusIO()(p).flip)
  val jtag = p(IncludeJtagDTM).option(new JTAGIO(true).flip)
}

trait PeripheryDebugModule {
  this: TopNetworkModule {
    val outer: PeripheryDebug
    val io: PeripheryDebugBundle
  } =>

  if (p(IncludeJtagDTM)) {
    // JtagDTMWithSync is a wrapper which
    // handles the synchronization as well.
    val dtm = Module (new JtagDTMWithSync()(p))
    dtm.io.jtag <> io.jtag.get
    coreplexDebug <> dtm.io.debug
  } else {
    coreplexDebug <>
      (if (p(AsyncDebugBus)) AsyncDebugBusFrom(io.debug_clk.get, io.debug_rst.get, io.debug.get)
      else io.debug.get)
  }
}

/////

trait PeripheryExtInterrupts {
  this: TopNetwork =>

  val extInterrupts = IntBlindInputNode(p(NExtTopInterrupts))
  val extInterruptXing = LazyModule(new IntXing)

  intBus.intnode := extInterruptXing.intnode
  extInterruptXing.intnode := extInterrupts
}

trait PeripheryExtInterruptsBundle {
  this: TopNetworkBundle {
    val outer: PeripheryExtInterrupts
  } =>
  val interrupts = outer.extInterrupts.bundleIn
}

trait PeripheryExtInterruptsModule {
  this: TopNetworkModule {
    val outer: PeripheryExtInterrupts
    val io: PeripheryExtInterruptsBundle
  } =>
}

/////

trait PeripheryMasterAXI4Mem {
  this: BaseTop[BaseCoreplex] with TopNetwork =>

  val base = 0x80000000L
  val size = p(ExtMemSize)
  val channels = coreplexMem.size
  Dump("MEM_BASE", base)

  val mem_axi4 = coreplexMem.zipWithIndex.map { case (node, i) =>
    val c_size = size/channels
    val c_base = base + c_size*i

    val axi4 = AXI4BlindOutputNode(AXI4SlavePortParameters(
      slaves = Seq(AXI4SlaveParameters(
        address       = List(AddressSet(c_base, c_size-1)),
        regionType    = RegionType.UNCACHED,   // cacheable
        executable    = true,
        supportsWrite = TransferSizes(1, 256), // The slave supports 1-256 byte transfers
        supportsRead  = TransferSizes(1, 256),
        interleavedId = Some(0))),             // slave does not interleave read responses
      beatBytes = 8)) // 64-bit AXI interface

    axi4 :=
      // AXI4Fragmenter(lite=false, maxInFlight = 20)( // beef device up to support awlen = 0xff
      TLToAXI4(idBits = 4)(                     // use idBits = 0 for AXI4-Lite
      TLWidthWidget(coreplex.l1tol2_beatBytes)( // convert width before attaching to the l1tol2
      node))

    axi4
  }
}

trait PeripheryMasterAXI4MemBundle {
  this: TopNetworkBundle {
    val outer: PeripheryMasterAXI4Mem
  } =>
  val mem_axi4 = outer.mem_axi4.map(_.bundleOut).toList.headOption // !!! remove headOption when Seq supported
}

trait PeripheryMasterAXI4MemModule {
  this: TopNetworkModule {
    val outer: PeripheryMasterAXI4Mem
    val io: PeripheryMasterAXI4MemBundle
  } =>
}

/////

// PeripheryMasterAXI4MMIO is an example, make your own cake pattern like this one.
trait PeripheryMasterAXI4MMIO {
  this: TopNetwork =>

  val mmio_axi4 = AXI4BlindOutputNode(AXI4SlavePortParameters(
    slaves = Seq(AXI4SlaveParameters(
      address       = List(AddressSet(0x60000000L, 0x1fffffffL)),
      executable    = true,                  // Can we run programs on this memory?
      supportsWrite = TransferSizes(1, 256), // The slave supports 1-256 byte transfers
      supportsRead  = TransferSizes(1, 256),
      interleavedId = Some(0))),             // slave does not interleave read responses
    beatBytes = 8)) // 64-bit AXI interface

  mmio_axi4 :=
    // AXI4Fragmenter(lite=false, maxInFlight = 20)( // beef device up to support awlen = 0xff
    TLToAXI4(idBits = 4)(                  // use idBits = 0 for AXI4-Lite
    TLWidthWidget(socBusConfig.beatBytes)( // convert width before attaching to socBus
    socBus.node))
}

trait PeripheryMasterAXI4MMIOBundle {
  this: TopNetworkBundle {
    val outer: PeripheryMasterAXI4MMIO
  } =>
  val mmio_axi = outer.mmio_axi4.bundleOut
}

trait PeripheryMasterAXI4MMIOModule {
  this: TopNetworkModule {
    val outer: PeripheryMasterAXI4MMIO
    val io: PeripheryMasterAXI4MMIOBundle
  } =>
  // nothing to do
}

/////

trait PeripherySlave {
  this: TopNetwork {
    val pBusMasters: RangeManager
  } =>

  if (p(NExtBusAXIChannels) > 0) pBusMasters.add("ext", 1) // NExtBusAXIChannels are arbitrated into one TL port
}

trait PeripherySlaveBundle {
  this: TopNetworkBundle {
    val outer: PeripherySlave
  } =>
  val bus_clk = p(AsyncBusChannels).option(Vec(p(NExtBusAXIChannels), Clock(INPUT)))
  val bus_rst = p(AsyncBusChannels).option(Vec(p(NExtBusAXIChannels), Bool (INPUT)))
  val bus_axi = Vec(p(NExtBusAXIChannels), new NastiIO).flip
}

trait PeripherySlaveModule {
  this: TopNetworkModule {
    val outer: PeripherySlave { val pBusMasters: RangeManager }
    val io: PeripherySlaveBundle
  } =>

  if (p(NExtBusAXIChannels) > 0) {
    val arb = Module(new NastiArbiter(p(NExtBusAXIChannels)))
    ((io.bus_axi zip arb.io.master) zipWithIndex) foreach { case ((bus, port), idx) =>
      port <> (
        if (!p(AsyncBusChannels)) bus
        else AsyncNastiFrom(io.bus_clk.get(idx), io.bus_rst.get(idx), bus)
      )
    }
    val conv = Module(new TileLinkIONastiIOConverter()(edgeSlaveParams))
    conv.io.nasti <> arb.io.slave

    val (r_start, r_end) = outer.pBusMasters.range("ext")
    require(r_end - r_start == 1, "RangeManager should return 1 slot")
    TileLinkWidthAdapter(coreplexSlave(r_start), conv.io.tl)
  }
}

/////

trait PeripheryBootROM {
  this: TopNetwork =>

  val bootrom_address = 0x1000
  val bootrom_size = 0x1000
  val bootrom = LazyModule(new TLROM(bootrom_address, bootrom_size, GenerateBootROM(p, bootrom_address), true, peripheryBusConfig.beatBytes))
  bootrom.node := TLFragmenter(peripheryBusConfig.beatBytes, cacheBlockBytes)(peripheryBus.node)
}

trait PeripheryBootROMBundle {
  this: TopNetworkBundle {
    val outer: PeripheryBootROM
  } =>
}

trait PeripheryBootROMModule {
  this: TopNetworkModule {
    val outer: PeripheryBootROM
    val io: PeripheryBootROMBundle
  } =>
}

/////

trait PeripheryTestRAM {
  this: TopNetwork =>

  val testram = LazyModule(new TLRAM(AddressSet(0x52000000, 0xfff), true, peripheryBusConfig.beatBytes))
  testram.node := TLFragmenter(peripheryBusConfig.beatBytes, cacheBlockBytes)(peripheryBus.node)
}

trait PeripheryTestRAMBundle {
  this: TopNetworkBundle {
    val outer: PeripheryTestRAM
  } =>
}

trait PeripheryTestRAMModule {
  this: TopNetworkModule {
    val outer: PeripheryTestRAM
    val io: PeripheryTestRAMBundle
  } =>
}

/////

trait PeripheryTestBusMaster {
  this: TopNetwork =>
  val fuzzer = LazyModule(new TLFuzzer(5000))
  peripheryBus.node := fuzzer.node
}

trait PeripheryTestBusMasterBundle {
  this: TopNetworkBundle {
    val outer: PeripheryTestBusMaster
  } =>
}

trait PeripheryTestBusMasterModule {
  this: TopNetworkModule {
    val outer: PeripheryTestBusMaster
    val io: PeripheryTestBusMasterBundle
  } =>
}

/////

trait HardwiredResetVector {
  this: TopNetworkModule {
    val outer: BaseTop[BaseCoreplex]
  } =>
  outer.coreplex.module.io.resetVector := UInt(0x1000) // boot ROM
}
