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
import config._

class BasePlatformConfig extends Config(
  (pname,site,here) => pname match {
    //Memory Parameters
    case TLEmitMonitors => true
    case NExtTopInterrupts => 2
    case SOCBusConfig => site(L1toL2Config)
    case PeripheryBusConfig => TLBusConfig(beatBytes = 4)
    case PeripheryBusArithmetic => true
    // Note that PLIC asserts that this is > 0.
    case IncludeJtagDTM => false
    case ExtMem => MasterConfig(base=0x80000000L, size=0x10000000L, beatBytes=8, idBits=4)
    case ExtBus => MasterConfig(base=0x60000000L, size=0x20000000L, beatBytes=8, idBits=4)
    case ExtIn  => SlaveConfig(beatBytes=8, idBits=8, sourceBits=2)
    case RTCPeriod => 100 // gives 10 MHz RTC assuming 1 GHz uncore clock
    case _ => throw new CDEMatchError
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
  (pname,site,here,up) => pname match {
    case BankedL2Config => up(BankedL2Config).copy(nMemoryChannels = n)
    case _ => throw new CDEMatchError
  }
)

class WithExtMemSize(n: Long) extends Config(
  (pname,site,here,up) => pname match {
    case ExtMem => up(ExtMem).copy(size = n)
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
  (pname, site, here, up) => pname match {
    case ExtMem => up(ExtMem).copy(beatBytes = dataBits/8)
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

class DualCoreConfig extends Config(
  new WithNCores(2) ++ new WithL2Cache ++ new BaseConfig)

class TinyConfig extends Config(
  new WithScratchpads ++
  new WithSmallCores ++ new WithRV32 ++
  new WithStatelessBridge ++ new BaseConfig)

class WithJtagDTM extends Config (
  (pname, site, here) => pname match {
    case IncludeJtagDTM => true
    case _ => throw new CDEMatchError
  }
)

class WithNoPeripheryArithAMO extends Config (
  (pname, site, here) => pname match {
    case PeripheryBusArithmetic => false
  }
)

class With64BitPeriphery extends Config (
  (pname, site, here) => pname match {
    case PeripheryBusConfig => TLBusConfig(beatBytes = 8)
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

class WithNExtTopInterrupts(nExtInts: Int) extends Config(
  (pname, site, here) => pname match {
    case NExtTopInterrupts => nExtInts
    case _ => throw new CDEMatchError
  }
)

class WithNBreakpoints(hwbp: Int) extends Config (
  (pname,site,here) => pname match {
    case NBreakpoints => hwbp
    case _ => throw new CDEMatchError
  }
)
