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

/** External Bus controls */
case object NExtBusAXIChannels extends Field[Int]
/** Specifies the size of external memory */
case object ExtMemSize extends Field[Long]
case object ExtMemBase extends Field[Long]
case object ExtBusSize extends Field[Long]
case object ExtBusBase extends Field[Long]
/** Specifies the number of external interrupts */
case object NExtTopInterrupts extends Field[Int]
/** Source of RTC. First bundle is TopIO.extra, Second bundle is periphery.io.extra  **/
case object RTCPeriod extends Field[Int]
/* Specifies the periphery bus configuration */
case object PeripheryBusConfig extends Field[TLBusConfig]
case object PeripheryBusArithmetic extends Field[Boolean]
/* Specifies the SOC-bus configuration */
case object SOCBusConfig extends Field[TLBusConfig]

/* Specifies the data and id width at the chip boundary */
case object EdgeDataBits extends Field[Int]
case object EdgeIDBits extends Field[Int]

/** Utility trait for quick access to some relevant parameters */
trait HasPeripheryParameters {
  implicit val p: Parameters
  lazy val nMemChannels = p(NMemoryChannels)
  lazy val peripheryBusConfig = p(PeripheryBusConfig)
  lazy val socBusConfig = p(SOCBusConfig)
  lazy val cacheBlockBytes = p(CacheBlockBytes)
  lazy val peripheryBusArithmetic = p(PeripheryBusArithmetic)
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

  val base = p(ExtMemBase)
  val size = p(ExtMemSize)
  val channels = coreplexMem.size

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
      address       = List(AddressSet(BigInt(p(ExtBusBase)), p(ExtBusSize)-1)),
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
