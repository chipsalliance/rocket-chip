// See LICENSE for license details.

package rocketchip

import Chisel._
import junctions._
import rocket._
import diplomacy._
import uncore.agents._
import uncore.tilelink._
import uncore.devices._
import uncore.converters._
import util._
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
      lazy val edgeDataBits = site(EdgeDataBits)
      lazy val edgeDataBeats = (8 * site(CacheBlockBytes)) / edgeDataBits
      pname match {
        //Memory Parameters
        case EdgeDataBits => 64
        case EdgeIDBits => 5
        case NastiKey => NastiParameters(
          dataBits = edgeDataBits,
          addrBits = site(PAddrBits),
          idBits = site(EdgeIDBits))
        case TLEmitMonitors => true
        case TLKey("EdgetoSlave") =>
          site(TLKey("L1toL2")).copy(dataBeats = edgeDataBeats)
        case TLKey("MCtoEdge") =>
          site(TLKey("L2toMC")).copy(dataBeats = edgeDataBeats)
        case TLKey("MMIOtoEdge") =>
          site(TLKey("L2toMMIO")).copy(dataBeats = edgeDataBeats)
        case NExtTopInterrupts => 2
        case SOCBusKey => SOCBusConfig(beatBytes = site(TLKey("L2toMMIO")).dataBitsPerBeat/8)
        case PeripheryBusKey => PeripheryBusConfig(arithAMO = true, beatBytes = 4)
        // Note that PLIC asserts that this is > 0.
        case AsyncDebugBus => false
        case IncludeJtagDTM => false
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
            dataBits = edgeDataBits)
        case AsyncMemChannels => false
        case NMemoryChannels => Dump("N_MEM_CHANNELS", 1)
        case TMemoryChannels => BusType.AXI
        case ExtMemSize => Dump("MEM_SIZE", 0x10000000L)
        case RTCPeriod => 100 // gives 10 MHz RTC assuming 1 GHz uncore clock
        case BuildExampleTop =>
          (p: Parameters) => LazyModule(new ExampleTop(new DefaultCoreplex()(_))(p))
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

class WithEdgeDataBits(dataBits: Int) extends Config(
  (pname, site, here) => pname match {
    case EdgeDataBits => dataBits
    case _ => throw new CDEMatchError
  })

class Edge128BitConfig extends Config(
  new WithEdgeDataBits(128) ++ new BaseConfig)
class Edge32BitConfig extends Config(
  new WithEdgeDataBits(32) ++ new BaseConfig)

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

class WithNoPeripheryArithAMO extends Config (
  (pname, site, here) => pname match {
    case PeripheryBusKey => PeripheryBusConfig(arithAMO = false, beatBytes = 4)
  }
)

class With64BitPeriphery extends Config (
  (pname, site, here) => pname match {
    case PeripheryBusKey => PeripheryBusConfig(arithAMO = true, beatBytes = 8)
  }
)

class WithTLMonitors extends Config (
  (pname, site, here) => pname match {
    case TLEmitMonitors => true
    case _ => throw new CDEMatchError
  }
)

class WithoutTLMonitors extends Config (
  (pname, site, here) => pname match {
    case TLEmitMonitors => false
    case _ => throw new CDEMatchError
  }
)
