// See LICENSE for license details.

package rocketchip

import Chisel._
import junctions._
import rocket._
import rocket.Util._
import uncore.agents._
import uncore.tilelink._
import uncore.tilelink2.{LazyModule}
import uncore.devices._
import uncore.converters._
import coreplex._
import scala.math.max
import scala.collection.mutable.{LinkedHashSet, ListBuffer}
import scala.collection.immutable.HashMap
import DefaultTestSuites._
import cde.{Parameters, Config, Dump, Knob, CDEMatchError}

class BasePlatformConfig extends Config(
  topDefinitions = {
    (pname,site,here) =>  {
      type PF = PartialFunction[Any,Any]
      def findBy(sname:Any):Any = here[PF](site[Any](sname))(pname)
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
        case BuildCoreplex =>
          (c: CoreplexConfig, p: Parameters) => uncore.tilelink2.LazyModule(new DefaultCoreplex(c)(p)).module
        case NExtTopInterrupts => 2
        case PeripheryBusKey => PeripheryBusConfig(arithAMO = true, beatBytes = 4)
        // Note that PLIC asserts that this is > 0.
        case AsyncDebugBus => false
        case IncludeJtagDTM => false
        case AsyncMMIOChannels => false
        case ExtMMIOPorts => Nil
        case NExtMMIOAXIChannels => 0
        case NExtMMIOAHBChannels => 0
        case NExtMMIOTLChannels  => 0
        case AsyncBusChannels => false
        case NExtBusAXIChannels => 0
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
        case RTCPeriod => 100 // gives 10 MHz RTC assuming 1 GHz uncore clock
        case BuildExampleTop =>
          (p: Parameters) => uncore.tilelink2.LazyModule(new ExampleTop(p))
        case SimMemLatency => 0
        case _ => throw new CDEMatchError
      }
    }
  })

class BaseConfig extends Config(new BaseCoreplexConfig ++ new BasePlatformConfig)
class DefaultConfig extends Config(new WithBlockingL1 ++ new BaseConfig)

class DefaultL2Config extends Config(new WithL2Cache ++ new BaseConfig)
class DefaultBufferlessConfig extends Config(
  new WithBufferlessBroadcastHub ++ new BaseConfig)

class FPGAConfig extends Config (
  (pname,site,here) => pname match {
    case NAcquireTransactors => 4
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
    case _ => throw new CDEMatchError
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

class WithAsyncDebug extends Config (
  (pname, site, here) => pname match {
    case AsyncDebugBus => true
    case _ => throw new CDEMatchError
  }
)

class WithJtagDTM extends Config (
  (pname, site, here) => pname match {
    case IncludeJtagDTM => true
    case _ => throw new CDEMatchError
  }
)

class WithMultiClock extends Config(
  (pname, site, here) => pname match {
    case BuildCoreplex => (c: CoreplexConfig, p: Parameters) =>
      LazyModule(new MultiClockCoreplex(c)(p)).module
    case BuildExampleTop => (p: Parameters) =>
      LazyModule(new ExampleMultiClockTop(p))
    case _ => throw new CDEMatchError
  }
)

class MultiClockConfig extends Config(
  new WithMultiClock ++ new BaseConfig)
