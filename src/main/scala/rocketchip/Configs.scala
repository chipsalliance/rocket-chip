// See LICENSE for license details.

package rocketchip

import Chisel._
import junctions._
import rocket._
import rocket.Util._
import uncore.agents._
import uncore.tilelink._
import uncore.devices._
import uncore.converters._
import coreplex._
import scala.math.max
import scala.collection.mutable.{LinkedHashSet, ListBuffer}
import scala.collection.immutable.HashMap
import DefaultTestSuites._
import cde.{Parameters, Config, Dump, Knob, CDEMatchError}

class BasePlatformConfig extends Config (
  topDefinitions = { (pname,site,here) => 
    type PF = PartialFunction[Any,Any]
    def findBy(sname:Any):Any = here[PF](site[Any](sname))(pname)
    lazy val internalIOAddrMap: AddrMap = {
      val entries = collection.mutable.ArrayBuffer[AddrMapEntry]()
      entries += AddrMapEntry("debug", MemSize(4096, MemAttr(AddrMapProt.RWX)))
      entries += AddrMapEntry("bootrom", MemSize(4096, MemAttr(AddrMapProt.RX)))
      entries += AddrMapEntry("plic", MemRange(0x40000000, 0x4000000, MemAttr(AddrMapProt.RW)))
      entries += AddrMapEntry("prci", MemSize(0x4000000, MemAttr(AddrMapProt.RW)))
      if (site(DataScratchpadSize) > 0) { // TODO heterogeneous tiles
        require(site(NTiles) == 1) // TODO relax this
        require(site(NMemoryChannels) == 0) // TODO allow both scratchpad & DRAM
        entries += AddrMapEntry("dmem0", MemRange(0x80000000L, site[Int](DataScratchpadSize), MemAttr(AddrMapProt.RWX)))
      }
      new AddrMap(entries)
    }
    lazy val externalAddrMap = new AddrMap(
      site(ExtraDevices).addrMapEntries ++ site(ExtMMIOPorts),
      start = BigInt("50000000", 16),
      collapse = true)
    lazy val globalAddrMap = {
      val memBase = 0x80000000L
      val memSize = site(ExtMemSize)

      val intern = AddrMapEntry("int", internalIOAddrMap)
      val extern = AddrMapEntry("ext", externalAddrMap)
      val io = AddrMapEntry("io", AddrMap((intern +: site(ExportMMIOPort).option(extern).toSeq):_*))
      val mem = AddrMapEntry("mem", MemRange(memBase, memSize, MemAttr(AddrMapProt.RWX, true)))
      val addrMap = AddrMap((io +: (site(NMemoryChannels) > 0).option(mem).toSeq):_*)

      Dump("MEM_BASE", memBase)
      addrMap
    }
    def makeConfigString() = {
      val addrMap = globalAddrMap
      val plicAddr = addrMap("io:int:plic").start
      val prciAddr = addrMap("io:int:prci").start
      val plicInfo = site(PLICKey)
      val xLen = site(XLen)
      val res = new StringBuilder
      res append  "plic {\n"
      res append s"  priority 0x${plicAddr.toString(16)};\n"
      res append s"  pending 0x${(plicAddr + plicInfo.pendingBase).toString(16)};\n"
      res append s"  ndevs ${plicInfo.nDevices};\n"
      res append  "};\n"
      res append  "rtc {\n"
      res append s"  addr 0x${(prciAddr + PRCI.time).toString(16)};\n"
      res append  "};\n"
      if (addrMap contains "mem") {
        res append  "ram {\n"
        res append  "  0 {\n"
        res append s"    addr 0x${addrMap("mem").start.toString(16)};\n"
        res append s"    size 0x${addrMap("mem").size.toString(16)};\n"
        res append  "  };\n"
        res append  "};\n"
      }
      res append  "core {\n"
      for (i <- 0 until site(NTiles)) { // TODO heterogeneous tiles
        val isa = {
          val m = if (site(MulDivKey).nonEmpty) "m" else ""
          val a = if (site(UseAtomics)) "a" else ""
          val f = if (site(FPUKey).nonEmpty) "f" else ""
          val d = if (site(FPUKey).nonEmpty && site(XLen) > 32) "d" else ""
          val s = if (site(UseVM)) "s" else ""
          s"rv${site(XLen)}i$m$a$f$d$s"
        }
        res append s"  $i {\n"
        res append  "    0 {\n"
        res append s"      isa $isa;\n"
        res append s"      timecmp 0x${(prciAddr + PRCI.timecmp(i)).toString(16)};\n"
        res append s"      ipi 0x${(prciAddr + PRCI.msip(i)).toString(16)};\n"
        res append s"      plic {\n"
        res append s"        m {\n"
        res append s"         ie 0x${(plicAddr + plicInfo.enableAddr(i, 'M')).toString(16)};\n"
        res append s"         thresh 0x${(plicAddr + plicInfo.threshAddr(i, 'M')).toString(16)};\n"
        res append s"         claim 0x${(plicAddr + plicInfo.claimAddr(i, 'M')).toString(16)};\n"
        res append s"        };\n"
        if (site(UseVM)) {
          res append s"        s {\n"
          res append s"         ie 0x${(plicAddr + plicInfo.enableAddr(i, 'S')).toString(16)};\n"
          res append s"         thresh 0x${(plicAddr + plicInfo.threshAddr(i, 'S')).toString(16)};\n"
          res append s"         claim 0x${(plicAddr + plicInfo.claimAddr(i, 'S')).toString(16)};\n"
          res append s"        };\n"
        }
        res append  "      };\n"
        res append  "    };\n"
        res append  "  };\n"
      }
      res append  "};\n"
      res append (site(ExtraDevices).makeConfigString(addrMap))
      res append '\u0000'
      res.toString.getBytes
    }
    lazy val innerDataBits = 64
    lazy val innerDataBeats = (8 * site(CacheBlockBytes)) / innerDataBits
    pname match {
      //Memory Parameters
      case MIFTagBits => Dump("MIF_TAG_BITS", 5)
      case MIFDataBits => Dump("MIF_DATA_BITS", 64)
      case MIFAddrBits => Dump("MIF_ADDR_BITS",
                               site(PAddrBits) - site(CacheBlockOffsetBits))
      case MIFDataBeats => site(CacheBlockBytes) * 8 / site(MIFDataBits)
      case NastiKey => {
        Dump("MEM_STRB_BITS", site(MIFDataBits) / 8)
        NastiParameters(
          dataBits = Dump("MEM_DATA_BITS", site(MIFDataBits)),
          addrBits = Dump("MEM_ADDR_BITS", site(PAddrBits)),
          idBits = Dump("MEM_ID_BITS", site(MIFTagBits)))
      }
      case BuildCoreplex => (p: Parameters) => Module(new DefaultCoreplex(p))
      case NExtTopInterrupts => 2
      case NExtPeripheryInterrupts => site(ExtraDevices).nInterrupts
      // Note that PLIC asserts that this is > 0.
      case NExtInterrupts => site(NExtTopInterrupts) + site(NExtPeripheryInterrupts)
      case AsyncDebugBus => false
      case IncludeJtagDTM => false
      case AsyncMMIOChannels => false
      case ExtraDevices => new EmptyDeviceBlock
      case ExtraTopPorts => (p: Parameters) => new Bundle
      case ExtMMIOPorts => Nil
      case NExtMMIOAXIChannels => 0
      case NExtMMIOAHBChannels => 0
      case NExtMMIOTLChannels  => 0
      case ExportMMIOPort => !externalAddrMap.isEmpty
      case AsyncBusChannels => false
      case NExtBusAXIChannels => 0
      case NExternalClients => (if (site(NExtBusAXIChannels) > 0) 1 else 0) +
                                site(ExtraDevices).nClientPorts
      case ConnectExtraPorts =>
        (out: Bundle, in: Bundle, p: Parameters) => out <> in

      case HastiId => "Ext"
      case HastiKey("TL") =>
        HastiParameters(
          addrBits = site(PAddrBits),
          dataBits = site(TLKey(site(TLId))).dataBits / site(TLKey(site(TLId))).dataBeats)
      case HastiKey("Ext") =>
        HastiParameters(
          addrBits = site(PAddrBits),
          dataBits = site(XLen))
      case AsyncMemChannels => false
      case NMemoryChannels => Dump("N_MEM_CHANNELS", 1)
      case TMemoryChannels => BusType.AXI
      case ExtMemSize => Dump("MEM_SIZE", 0x10000000L)
      case ConfigString => makeConfigString()
      case GlobalAddrMap => globalAddrMap
      case RTCPeriod => 100 // gives 10 MHz RTC assuming 1 GHz uncore clock
      case RTCTick => (p: Parameters, t_io: Bundle, p_io:Bundle) => Counter(p(RTCPeriod)).inc() 
      case _ => throw new CDEMatchError
  }})

class BaseConfig extends Config(new BaseCoreplexConfig ++ new BasePlatformConfig)
class DefaultConfig extends Config(new WithBlockingL1 ++ new BaseConfig)

class DefaultL2Config extends Config(new WithL2Cache ++ new BaseConfig)
class DefaultBufferlessConfig extends Config(
  new WithBufferlessBroadcastHub ++ new BaseConfig)

class FPGAConfig extends Config (
  (pname,site,here) => pname match {
    case NAcquireTransactors => 4
    case ExportGroundTestStatus => true
    case _ => throw new CDEMatchError
  }
)

class DefaultFPGAConfig extends Config(new FPGAConfig ++ new BaseConfig)
class DefaultL2FPGAConfig extends Config(
  new WithL2Capacity(64) ++ new WithL2Cache ++ new DefaultFPGAConfig)

class PLRUL2Config extends Config(new WithPLRU ++ new DefaultL2Config)

class WithNMemoryChannels(n: Int) extends Config(
  (pname,site,here) => pname match {
    case NMemoryChannels => Dump("N_MEM_CHANNELS", n)
    case _ => throw new CDEMatchError
  }
)

class WithExtMemSize(n: Long) extends Config(
  (pname,site,here) => pname match {
    case ExtMemSize => Dump("MEM_SIZE", n)
    case _ => throw new CDEMatchError
  }
)
class WithAHB extends Config(
  (pname, site, here) => pname match {
    case TMemoryChannels     => BusType.AHB
    case NExtMMIOAHBChannels => 1
  })

class WithTL extends Config(
  (pname, site, here) => pname match {
    case TMemoryChannels     => BusType.TL
    case NExtMMIOTLChannels  => 1
  })

class WithScratchpads extends Config(new WithNMemoryChannels(0) ++ new WithDataScratchpad(16384))

class DefaultFPGASmallConfig extends Config(new WithSmallCores ++ new DefaultFPGAConfig)
class DefaultSmallConfig extends Config(new WithSmallCores ++ new BaseConfig)
class DefaultRV32Config extends Config(new WithRV32 ++ new DefaultConfig)

class DualBankConfig extends Config(
  new WithNBanksPerMemChannel(2) ++ new BaseConfig)
class DualBankL2Config extends Config(
  new WithNBanksPerMemChannel(2) ++ new WithL2Cache ++ new BaseConfig)

class DualChannelConfig extends Config(new WithNMemoryChannels(2) ++ new BaseConfig)
class DualChannelL2Config extends Config(
  new WithNMemoryChannels(2) ++ new WithL2Cache ++ new BaseConfig)

class DualChannelDualBankConfig extends Config(
  new WithNMemoryChannels(2) ++
  new WithNBanksPerMemChannel(2) ++ new BaseConfig)
class DualChannelDualBankL2Config extends Config(
  new WithNMemoryChannels(2) ++ new WithNBanksPerMemChannel(2) ++
  new WithL2Cache ++ new BaseConfig)

class RoccExampleConfig extends Config(new WithRoccExample ++ new BaseConfig)

class WithMIFDataBits(n: Int) extends Config(
  (pname, site, here) => pname match {
    case MIFDataBits => Dump("MIF_DATA_BITS", n)
  })

class MIF128BitConfig extends Config(
  new WithMIFDataBits(128) ++ new BaseConfig)
class MIF32BitConfig extends Config(
  new WithMIFDataBits(32) ++ new BaseConfig)

class SmallL2Config extends Config(
  new WithNMemoryChannels(2) ++ new WithNBanksPerMemChannel(4) ++
  new WithL2Capacity(256) ++ new DefaultL2Config)

class SingleChannelBenchmarkConfig extends Config(new WithL2Capacity(256) ++ new DefaultL2Config)
class DualChannelBenchmarkConfig extends Config(new WithNMemoryChannels(2) ++ new SingleChannelBenchmarkConfig)
class QuadChannelBenchmarkConfig extends Config(new WithNMemoryChannels(4) ++ new SingleChannelBenchmarkConfig)
class OctoChannelBenchmarkConfig extends Config(new WithNMemoryChannels(8) ++ new SingleChannelBenchmarkConfig)

class EightChannelConfig extends Config(new WithNMemoryChannels(8) ++ new BaseConfig)

class SplitL2MetadataTestConfig extends Config(new WithSplitL2Metadata ++ new DefaultL2Config)

class DualCoreConfig extends Config(
  new WithNCores(2) ++ new WithL2Cache ++ new BaseConfig)

class TinyConfig extends Config(
  new WithScratchpads ++
  new WithSmallCores ++ new WithRV32 ++
  new WithStatelessBridge ++ new BaseConfig)

class WithTestRAM extends Config(
  (pname, site, here) => pname match {
    case ExtraDevices => {
      class TestRAMDevice extends DeviceBlock {
        val ramSize = 0x1000
        def nClientPorts = 0
        def addrMapEntries = Seq(
          AddrMapEntry("testram", MemSize(ramSize, MemAttr(AddrMapProt.RW))))
        def builder(
          mmioPorts: HashMap[String, ClientUncachedTileLinkIO],
          clientPorts: Seq[ClientUncachedTileLinkIO],
          interrupts: Seq[Bool],
          extra: Bundle, p: Parameters) {
          val testram = Module(new TileLinkTestRAM(ramSize)(p))
          testram.io <> mmioPorts("testram")
        }
      }
      new TestRAMDevice
    }
  }
)

class WithAsyncDebug extends Config (
  (pname, site, here) => pname match {
     case AsyncDebugBus => true
  }
)


class WithJtagDTM extends Config (
  (pname, site, here) => pname match {
     case IncludeJtagDTM => true
  }
)
