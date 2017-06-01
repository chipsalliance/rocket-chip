// See LICENSE.SiFive for license details.

package rocketchip

import Chisel._
import config._
import coreplex._
import diplomacy._
import uncore.tilelink2._
import uncore.axi4._
import util._
import scala.math.{min,max}

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
/* Specifies the location of the Error device */
case class ErrorConfig(address: Seq[AddressSet])
case object ErrorConfig extends Field[ErrorConfig]

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
  def nExtInterrupts = p(NExtTopInterrupts)
}

/** HasSystemNetworks provides buses that will serve as attachment points,
  * for use in the following child traits that connect individual agents or external ports.
  */
trait HasSystemNetworks extends HasPeripheryParameters {
  val socBus = LazyModule(new TLXbar)          // Wide or unordered-access slave devices (TL-UH)
  val peripheryBus = LazyModule(new TLXbar)    // Narrow and ordered-access slave devices (TL-UL)
  val intBus = LazyModule(new IntXbar)         // Device and global external interrupts
  val fsb = LazyModule(new TLBuffer(BufferParams.none))          // Master devices talking to the frontside of the L2
  val bsb = LazyModule(new TLBuffer(BufferParams.none))          // Slave devices talking to the backside of the L2
  val mem = Seq.fill(nMemoryChannels) { LazyModule(new TLXbar) } // Ports out to DRAM

  // The peripheryBus hangs off of socBus;
  // here we convert TL-UH -> TL-UL
  peripheryBus.node :=
    TLBuffer()(
    TLWidthWidget(socBusConfig.beatBytes)(
    TLAtomicAutomata(arithmetic = peripheryBusArithmetic)(
    socBus.node)))
}

/** This trait adds externally driven interrupts to the system. 
  * However, it should not be used directly; instead one of the below
  * synchronization wiring child traits should be used.
  */
abstract trait HasPeripheryExtInterrupts extends HasSystemNetworks {
  private val device = new Device with DeviceInterrupts {
    def describe(resources: ResourceBindings): Description = {
      Description("soc/offchip-interrupts", describeInterrupts(resources))
    }
  }

  val extInterrupts = IntInternalInputNode(IntSourcePortSimple(num = nExtInterrupts, resources = device.int))
}

/** This trait should be used if the External Interrupts have NOT
  * already been synchronized to the Periphery (PLIC) Clock.
  */
trait HasPeripheryAsyncExtInterrupts extends HasPeripheryExtInterrupts {
  if (nExtInterrupts > 0) {
    val extInterruptXing = LazyModule(new IntXing)
    intBus.intnode := extInterruptXing.intnode
    extInterruptXing.intnode := extInterrupts
  }
}

/** This trait can be used if the External Interrupts have already been synchronized
  * to the Periphery (PLIC) Clock.
  */
trait HasPeripherySyncExtInterrupts extends HasPeripheryExtInterrupts {
  if (nExtInterrupts > 0) {
    intBus.intnode := extInterrupts
  }
}

/** Common io name and methods for propagating or tying off the port bundle */
trait HasPeripheryExtInterruptsBundle extends HasPeripheryParameters {
  val interrupts: UInt
  def tieOffInterrupts(dummy: Int = 1) {
    interrupts := UInt(0)
  }
}

/** This trait performs the translation from a UInt IO into Diplomatic Interrupts.
  * The wiring must be done in the concrete LazyModuleImp. 
  */
trait HasPeripheryExtInterruptsModuleImp extends LazyMultiIOModuleImp with HasPeripheryExtInterruptsBundle {
  val outer: HasPeripheryExtInterrupts
  val interrupts = IO(UInt(INPUT, width = outer.nExtInterrupts))

  outer.extInterrupts.bundleIn.flatten.zipWithIndex.foreach { case(o, i) => o := interrupts(i) }
}

///// The following traits add ports to the sytem, in some cases converting to different interconnect standards

/** Adds a port to the system intended to master an AXI4 DRAM controller. */
trait HasPeripheryMasterAXI4MemPort extends HasSystemNetworks {
  val module: HasPeripheryMasterAXI4MemPortModuleImp

  private val config = p(ExtMem)
  private val channels = p(BankedL2Config).nMemoryChannels
  private val blockBytes = p(CacheBlockBytes)

  private val device = new MemoryDevice

  val mem_axi4 = AXI4BlindOutputNode(Seq.tabulate(channels) { channel =>
    val base = AddressSet(config.base, config.size-1)
    val filter = AddressSet(channel * blockBytes, ~((channels-1) * blockBytes))

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

  private val converter = LazyModule(new TLToAXI4(config.beatBytes))
  private val trim = LazyModule(new AXI4IdIndexer(config.idBits))
  private val yank = LazyModule(new AXI4UserYanker)
  private val buffer = LazyModule(new AXI4Buffer)

  mem foreach { case xbar =>
    converter.node := xbar.node
    trim.node := converter.node
    yank.node := trim.node
    buffer.node := yank.node
    mem_axi4 := buffer.node
  }
}

/** Common io name and methods for propagating or tying off the port bundle */
trait HasPeripheryMasterAXI4MemPortBundle extends HasPeripheryParameters {
  val mem_axi4: HeterogeneousBag[AXI4Bundle]
  def connectSimAXIMem(dummy: Int = 1) = {
    if (nMemoryChannels > 0) Module(LazyModule(new SimAXIMem(nMemoryChannels)).module).io.axi4 <> mem_axi4
  }
}

/** Actually generates the corresponding IO in the concrete Module */
trait HasPeripheryMasterAXI4MemPortModuleImp extends LazyMultiIOModuleImp with HasPeripheryMasterAXI4MemPortBundle {
  val outer: HasPeripheryMasterAXI4MemPort
  val mem_axi4 = IO(outer.mem_axi4.bundleOut)
}

/** Adds a AXI4 port to the system intended to master an MMIO device bus */
trait HasPeripheryMasterAXI4MMIOPort extends HasSystemNetworks {
  private val config = p(ExtBus)
  private val device = new SimpleDevice("mmio", Nil)
  val mmio_axi4 = AXI4BlindOutputNode(Seq(AXI4SlavePortParameters(
    slaves = Seq(AXI4SlaveParameters(
      address       = List(AddressSet(BigInt(config.base), config.size-1)),
      resources     = device.reg,
      executable    = true,                  // Can we run programs on this memory?
      supportsWrite = TransferSizes(1, 256), // The slave supports 1-256 byte transfers
      supportsRead  = TransferSizes(1, 256))),
    beatBytes = config.beatBytes)))

  mmio_axi4 :=
    AXI4Buffer()(
    AXI4UserYanker()(
    AXI4Deinterleaver(cacheBlockBytes)(
    AXI4IdIndexer(config.idBits)(
    TLToAXI4(config.beatBytes)(
    TLWidthWidget(socBusConfig.beatBytes)( // convert width before attaching to socBus
    socBus.node))))))
}

/** Common io name and methods for propagating or tying off the port bundle */
trait HasPeripheryMasterAXI4MMIOPortBundle extends HasPeripheryParameters {
  val mmio_axi4: HeterogeneousBag[AXI4Bundle]
  def connectSimAXIMMIO(dummy: Int = 1) {
    Module(LazyModule(new SimAXIMem(1, 4096)).module).io.axi4 <> mmio_axi4
  }
}

/** Actually generates the corresponding IO in the concrete Module */
trait HasPeripheryMasterAXI4MMIOPortModuleImp extends LazyMultiIOModuleImp with HasPeripheryMasterAXI4MMIOPortBundle {
  val outer: HasPeripheryMasterAXI4MMIOPort
  val mmio_axi4 = IO(outer.mmio_axi4.bundleOut)
}

/** Adds an AXI4 port to the system intended to be a slave on an MMIO device bus */
trait HasPeripherySlaveAXI4Port extends HasSystemNetworks {
  private val config = p(ExtIn)
  val l2FrontendAXI4Node = AXI4BlindInputNode(Seq(AXI4MasterPortParameters(
    masters = Seq(AXI4MasterParameters(
      name = "AXI4 periphery",
      id   = IdRange(0, 1 << config.idBits))))))

  private val fifoBits = 1
  fsb.node :=
    TLWidthWidget(config.beatBytes)(
    AXI4ToTL()(
    AXI4UserYanker(Some(1 << (config.sourceBits - fifoBits - 1)))(
    AXI4Fragmenter()(
    AXI4IdIndexer(fifoBits)(
    l2FrontendAXI4Node)))))
}

/** Common io name and methods for propagating or tying off the port bundle */
trait HasPeripherySlaveAXI4PortBundle extends HasPeripheryParameters {
  val l2_frontend_bus_axi4: HeterogeneousBag[AXI4Bundle]
  def tieOffAXI4SlavePort(dummy: Int = 1) {
    l2_frontend_bus_axi4.foreach { l2_axi4 =>
      l2_axi4.ar.valid := Bool(false)
      l2_axi4.aw.valid := Bool(false)
      l2_axi4.w .valid := Bool(false)
      l2_axi4.r .ready := Bool(true)
      l2_axi4.b .ready := Bool(true)
    }
  }
}

/** Actually generates the corresponding IO in the concrete Module */
trait HasPeripherySlaveAXI4PortModuleImp extends LazyMultiIOModuleImp with HasPeripherySlaveAXI4PortBundle {
  val outer: HasPeripherySlaveAXI4Port
  val l2_frontend_bus_axi4 = IO(outer.l2FrontendAXI4Node.bundleIn)
}

/** Adds a TileLink port to the system intended to master an MMIO device bus */
trait HasPeripheryMasterTLMMIOPort extends HasSystemNetworks {
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
    TLSourceShrinker(1 << config.idBits)(
    TLWidthWidget(socBusConfig.beatBytes)(
    socBus.node)))
}

/** Common io name and methods for propagating or tying off the port bundle */
trait HasPeripheryMasterTLMMIOPortBundle extends HasPeripheryParameters {
  val mmio_tl: HeterogeneousBag[TLBundle]
  def tieOffTLMMIO(dummy: Int = 1) {
    mmio_tl.foreach { tl =>
      tl.a.ready := Bool(true)
      tl.b.valid := Bool(false)
      tl.c.ready := Bool(true)
      tl.d.valid := Bool(false)
      tl.e.ready := Bool(true)
    }
  }
}

/** Actually generates the corresponding IO in the concrete Module */
trait HasPeripheryMasterTLMMIOPortModuleImp extends LazyMultiIOModuleImp with HasPeripheryMasterTLMMIOPortBundle {
  val outer: HasPeripheryMasterTLMMIOPort
  val mmio_tl = IO(outer.mmio_tl.bundleOut)
}

/** Adds an AXI4 port to the system intended to be a slave on an MMIO device bus.
  * NOTE: this port is NOT allowed to issue Acquires.
  */
trait HasPeripherySlaveTLPort extends HasSystemNetworks {
  private val config = p(ExtIn)
  val l2FrontendTLNode = TLBlindInputNode(Seq(TLClientPortParameters(
    clients = Seq(TLClientParameters(
      name     = "TL periph",
      sourceId = IdRange(0, 1 << config.idBits))))))

  fsb.node :=
    TLSourceShrinker(1 << config.sourceBits)(
    TLWidthWidget(config.beatBytes)(
    l2FrontendTLNode))
}

/** Common io name and methods for propagating or tying off the port bundle */
trait HasPeripherySlaveTLPortBundle extends HasPeripheryParameters {
  val l2_frontend_bus_tl: HeterogeneousBag[TLBundle]
  def tieOffSlaveTLPort(dummy: Int = 1) {
    l2_frontend_bus_tl.foreach { tl =>
      tl.a.valid := Bool(false)
      tl.b.ready := Bool(true)
      tl.c.valid := Bool(false)
      tl.d.ready := Bool(true)
      tl.e.valid := Bool(false)
    }
  }
}

/** Actually generates the corresponding IO in the concrete Module */
trait HasPeripherySlaveTLPortModuleImp extends LazyMultiIOModuleImp with HasPeripherySlaveTLPortBundle {
  val outer: HasPeripherySlaveTLPort
  val l2_frontend_bus_tl = IO(outer.l2FrontendTLNode.bundleIn)
}

///// The following traits add specific devices to the periphery of the system.

/** Adds a /dev/null slave that generates */
trait HasPeripheryZeroSlave extends HasSystemNetworks {
  private val config = p(ZeroConfig)
  private val address = AddressSet(config.base, config.size-1)
  private val blockBytes = p(CacheBlockBytes)

  val zeros = mem map { case xbar =>
    val zero = LazyModule(new TLZero(address, beatBytes = config.beatBytes))
    zero.node := TLFragmenter(config.beatBytes, blockBytes)(xbar.node)
    zero
  }
}

/** Adds a /dev/null slave that generates TL2 error response messages. */
trait HasPeripheryErrorSlave extends HasSystemNetworks {
  private val config = p(ErrorConfig)
  private val maxXfer = min(config.address.map(_.alignment).max.toInt, 4096)
  val error = LazyModule(new TLError(config.address, peripheryBusConfig.beatBytes))
  error.node := TLFragmenter(peripheryBusConfig.beatBytes, maxXfer)(peripheryBus.node)
}


/** Adds a SRAM to the system for testing purposes. */
trait HasPeripheryTestRAMSlave extends HasSystemNetworks {
  val testram = LazyModule(new TLRAM(AddressSet(0x52000000, 0xfff), true, peripheryBusConfig.beatBytes))
  testram.node := TLFragmenter(peripheryBusConfig.beatBytes, cacheBlockBytes)(peripheryBus.node)
}

/** Adds a fuzzing master to the system for testing purposes. */
trait HasPeripheryTestFuzzMaster extends HasSystemNetworks {
  val fuzzer = LazyModule(new TLFuzzer(5000))
  peripheryBus.node := fuzzer.node
}
