// See LICENSE.SiFive for license details.

package rocketchip

import Chisel._
import config._
import coreplex._
import diplomacy._
import tile.XLen
import uncore.tilelink2._
import uncore.axi4._
import uncore.converters._
import uncore.devices._
import uncore.util._
import util._
import scala.math.max

/** Specifies the size of external memory */
case class MasterConfig(base: Long, size: Long, beatBytes: Int, idBits: Int)
case object ExtMem extends Field[MasterConfig]
case object ExtBus extends Field[MasterConfig]
case class SlaveConfig(beatBytes: Int, idBits: Int, sourceBits: Int)
case object ExtIn extends Field[SlaveConfig]
/** Specifies the number of external interrupts */
case object NExtTopInterrupts extends Field[Int]
/** Source of RTC. First bundle is TopIO.extra, Second bundle is periphery.io.extra  **/
case object RTCPeriod extends Field[Int]
/* Specifies the periphery bus configuration */
case object PeripheryBusConfig extends Field[TLBusConfig]
case object PeripheryBusArithmetic extends Field[Boolean]
/* Specifies the SOC-bus configuration */
case object SOCBusConfig extends Field[TLBusConfig]
/* Specifies the location of the Zero device */
case class ZeroConfig(base: Long, size: Long, beatBytes: Int)
case object ZeroConfig extends Field[ZeroConfig]

/** Utility trait for quick access to some relevant parameters */
trait HasPeripheryParameters {
  implicit val p: Parameters
  def peripheryBusConfig = p(PeripheryBusConfig)
  def peripheryBusBytes = peripheryBusConfig.beatBytes
  def socBusConfig = p(SOCBusConfig)
  def socBusBytes = socBusConfig.beatBytes
  def cacheBlockBytes = p(CacheBlockBytes)
  def peripheryBusArithmetic = p(PeripheryBusArithmetic)
  def nMemoryChannels = p(coreplex.BankedL2Config).nMemoryChannels
}

/////

trait PeripheryExtInterrupts {
  this: HasTopLevelNetworks =>

  private val device = new Device with DeviceInterrupts {
    def describe(resources: ResourceBindings): Description = {
      Description("soc/offchip-interrupts", describeInterrupts(resources))
    }
  }

  val nExtInterrupts = p(NExtTopInterrupts)
  val extInterrupts = IntInternalInputNode(IntSourcePortSimple(num = nExtInterrupts, resources = device.int))

  if (nExtInterrupts > 0) {
    val extInterruptXing = LazyModule(new IntXing)
    intBus.intnode := extInterruptXing.intnode
    extInterruptXing.intnode := extInterrupts
  }
}

trait PeripheryExtInterruptsBundle {
  this: HasTopLevelNetworksBundle {
    val outer: PeripheryExtInterrupts
  } =>
  val interrupts = UInt(INPUT, width = outer.nExtInterrupts)
}

trait PeripheryExtInterruptsModule {
  this: HasTopLevelNetworksModule {
    val outer: PeripheryExtInterrupts
    val io: PeripheryExtInterruptsBundle
  } =>
  outer.extInterrupts.bundleIn.flatten.zipWithIndex.foreach { case(o, i) => o := io.interrupts(i) }
}

/////

trait PeripheryMasterAXI4Mem {
  this: HasTopLevelNetworks =>
  val module: PeripheryMasterAXI4MemModule

  private val config = p(ExtMem)
  private val channels = p(BankedL2Config).nMemoryChannels
  private val lineBytes = p(CacheBlockBytes)

  private val device = new MemoryDevice

  val mem_axi4 = AXI4BlindOutputNode(Seq.tabulate(channels) { channel =>
    val base = AddressSet(config.base, config.size-1)
    val filter = AddressSet(channel * lineBytes, ~((channels-1) * lineBytes))

    AXI4SlavePortParameters(
      slaves = Seq(AXI4SlaveParameters(
        address       = base.intersect(filter).toList,
        resources     = device.reg,
        regionType    = RegionType.UNCACHED,   // cacheable
        executable    = true,
        supportsWrite = TransferSizes(1, 256), // The slave supports 1-256 byte transfers
        supportsRead  = TransferSizes(1, 256),
        interleavedId = Some(0))),             // slave does not interleave read responses
      beatBytes = config.beatBytes)
  })

  private val converter = LazyModule(new TLToAXI4(config.idBits))
  private val buffer = LazyModule(new AXI4Buffer)

  mem foreach { case xbar =>
    converter.node := xbar.node
    buffer.node := converter.node
    mem_axi4 := buffer.node
  }
}

trait PeripheryMasterAXI4MemBundle {
  this: HasTopLevelNetworksBundle {
    val outer: PeripheryMasterAXI4Mem
  } =>
  val mem_axi4 = outer.mem_axi4.bundleOut
}

trait PeripheryMasterAXI4MemModule {
  this: HasTopLevelNetworksModule {
    val outer: PeripheryMasterAXI4Mem
    val io: PeripheryMasterAXI4MemBundle
  } =>
}

/////

trait PeripheryZero {
  this: HasTopLevelNetworks =>
  val module: PeripheryZeroModule

  private val config = p(ZeroConfig)
  private val address = AddressSet(config.base, config.size-1)
  private val lineBytes = p(CacheBlockBytes)

  val zeros = mem map { case xbar =>
    val zero = LazyModule(new TLZero(address, beatBytes = config.beatBytes))
    zero.node := TLFragmenter(config.beatBytes, lineBytes)(xbar.node)
    zero
  }
}

trait PeripheryZeroBundle {
  this: HasTopLevelNetworksBundle {
    val outer: PeripheryZero
  } =>
}

trait PeripheryZeroModule {
  this: HasTopLevelNetworksModule {
    val outer: PeripheryZero
    val io: PeripheryZeroBundle
  } =>
}

/////

// PeripheryMasterAXI4MMIO is an example, make your own cake pattern like this one.
trait PeripheryMasterAXI4MMIO {
  this: HasTopLevelNetworks =>

  private val config = p(ExtBus)
  private val device = new SimpleDevice("mmio", Nil)
  val mmio_axi4 = AXI4BlindOutputNode(Seq(AXI4SlavePortParameters(
    slaves = Seq(AXI4SlaveParameters(
      address       = List(AddressSet(BigInt(config.base), config.size-1)),
      resources     = device.reg,
      executable    = true,                  // Can we run programs on this memory?
      supportsWrite = TransferSizes(1, 256), // The slave supports 1-256 byte transfers
      supportsRead  = TransferSizes(1, 256),
      interleavedId = Some(0))),             // slave does not interleave read responses
    beatBytes = config.beatBytes)))

  mmio_axi4 :=
    AXI4Buffer()(
    // AXI4Fragmenter(lite=false, maxInFlight = 20)( // beef device up to support awlen = 0xff
    TLToAXI4(idBits = config.idBits)(      // use idBits = 0 for AXI4-Lite
    TLWidthWidget(socBusConfig.beatBytes)( // convert width before attaching to socBus
    socBus.node)))
}

trait PeripheryMasterAXI4MMIOBundle {
  this: HasTopLevelNetworksBundle {
    val outer: PeripheryMasterAXI4MMIO
  } =>
  val mmio_axi4 = outer.mmio_axi4.bundleOut
}

trait PeripheryMasterAXI4MMIOModule {
  this: HasTopLevelNetworksModule {
    val outer: PeripheryMasterAXI4MMIO
    val io: PeripheryMasterAXI4MMIOBundle
  } =>
  // nothing to do
}

/////

// PeripherySlaveAXI4 is an example, make your own cake pattern like this one.
trait PeripherySlaveAXI4 extends HasTopLevelNetworks {
  private val config = p(ExtIn)
  val l2FrontendAXI4Node = AXI4BlindInputNode(Seq(AXI4MasterPortParameters(
    masters = Seq(AXI4MasterParameters(
      id = IdRange(0, 1 << config.idBits))))))

  l2FrontendBus.node :=
    TLSourceShrinker(1 << config.sourceBits)(
    TLWidthWidget(config.beatBytes)(
    AXI4ToTL()(
    AXI4Fragmenter()(
    l2FrontendAXI4Node))))
}

trait PeripherySlaveAXI4Bundle extends HasTopLevelNetworksBundle {
  val outer: PeripherySlaveAXI4
  val l2_frontend_bus_axi4 = outer.l2FrontendAXI4Node.bundleIn
}

trait PeripherySlaveAXI4Module extends HasTopLevelNetworksModule {
  val outer: PeripherySlaveAXI4
  val io: PeripherySlaveAXI4Bundle
  // nothing to do
}

/////

// Add an external TL-UL slave
trait PeripheryMasterTLMMIO {
  this: HasTopLevelNetworks =>

  private val config = p(ExtBus)
  private val device = new SimpleDevice("mmio", Nil)
  val mmio_tl = TLBlindOutputNode(Seq(TLManagerPortParameters(
    managers = Seq(TLManagerParameters(
      address            = List(AddressSet(BigInt(config.base), config.size-1)),
      resources          = device.reg,
      executable         = true,
      supportsGet        = TransferSizes(1, cacheBlockBytes),
      supportsPutFull    = TransferSizes(1, cacheBlockBytes),
      supportsPutPartial = TransferSizes(1, cacheBlockBytes))),
    beatBytes = config.beatBytes)))

  mmio_tl :=
    TLBuffer()(
    TLSourceShrinker(config.idBits)(
    TLWidthWidget(socBusConfig.beatBytes)(
    socBus.node)))
}

trait PeripheryMasterTLMMIOBundle {
  this: HasTopLevelNetworksBundle {
    val outer: PeripheryMasterTLMMIO
  } =>
  val mmio_tl = outer.mmio_tl.bundleOut
}

trait PeripheryMasterTLMMIOModule {
  this: HasTopLevelNetworksModule {
    val outer: PeripheryMasterTLMMIO
    val io: PeripheryMasterTLMMIOBundle
  } =>
  // nothing to do
}

/////

// NOTE: this port is NOT allowed to issue Acquires
trait PeripherySlaveTL extends HasTopLevelNetworks {
  private val config = p(ExtIn)
  val l2FrontendTLNode = TLBlindInputNode(Seq(TLClientPortParameters(
    clients = Seq(TLClientParameters(
      sourceId = IdRange(0, 1 << config.idBits))))))

  l2FrontendBus.node :=
    TLSourceShrinker(1 << config.sourceBits)(
    TLWidthWidget(config.beatBytes)(
    l2FrontendTLNode))
}

trait PeripherySlaveTLBundle extends HasTopLevelNetworksBundle {
  val outer: PeripherySlaveTL
  val l2_frontend_bus_tl = outer.l2FrontendTLNode.bundleIn
}

trait PeripherySlaveTLModule extends HasTopLevelNetworksModule {
  val outer: PeripherySlaveTL
  val io: PeripherySlaveTLBundle
  // nothing to do
}

/////

trait PeripheryBootROM {
  this: HasTopLevelNetworks =>
  val coreplex: CoreplexRISCVPlatform

  private val bootrom_address = 0x1000
  private val bootrom_size = 0x1000
  private lazy val bootrom_contents = GenerateBootROM(p, bootrom_address, coreplex.dts)
  val bootrom = LazyModule(new TLROM(bootrom_address, bootrom_size, bootrom_contents, true, peripheryBusConfig.beatBytes))
  bootrom.node := TLFragmenter(peripheryBusConfig.beatBytes, cacheBlockBytes)(peripheryBus.node)
}

trait PeripheryBootROMBundle {
  this: HasTopLevelNetworksBundle {
    val outer: PeripheryBootROM
  } =>
}

trait PeripheryBootROMModule {
  this: HasTopLevelNetworksModule {
    val outer: PeripheryBootROM
    val io: PeripheryBootROMBundle
  } =>
}

/////

trait PeripheryTestRAM {
  this: HasTopLevelNetworks =>

  val testram = LazyModule(new TLRAM(AddressSet(0x52000000, 0xfff), true, peripheryBusConfig.beatBytes))
  testram.node := TLFragmenter(peripheryBusConfig.beatBytes, cacheBlockBytes)(peripheryBus.node)
}

trait PeripheryTestRAMBundle {
  this: HasTopLevelNetworksBundle {
    val outer: PeripheryTestRAM
  } =>
}

trait PeripheryTestRAMModule {
  this: HasTopLevelNetworksModule {
    val outer: PeripheryTestRAM
    val io: PeripheryTestRAMBundle
  } =>
}

/////

trait PeripheryTestBusMaster {
  this: HasTopLevelNetworks =>
  val fuzzer = LazyModule(new TLFuzzer(5000))
  peripheryBus.node := fuzzer.node
}

trait PeripheryTestBusMasterBundle {
  this: HasTopLevelNetworksBundle {
    val outer: PeripheryTestBusMaster
  } =>
}

trait PeripheryTestBusMasterModule {
  this: HasTopLevelNetworksModule {
    val outer: PeripheryTestBusMaster
    val io: PeripheryTestBusMasterBundle
  } =>
}
