// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package rocketchip

import Chisel._
import junctions._
import rocket._
import diplomacy._
import uncore.agents._
import uncore.tilelink2._
import uncore.devices._
import uncore.converters._
import util._
import coreplex._
import scala.math.max
import scala.collection.mutable.{LinkedHashSet, ListBuffer}
import scala.collection.immutable.HashMap
import DefaultTestSuites._
import config._

class BasePlatformConfig extends Config((site, here, up) => {
  // DTS descriptive parameters
  case DTSModel => "ucbbar,rocketchip-unknown"
  case DTSCompat => Nil
  case DTSTimebase => BigInt(0)
  // TileLink connection parameters
  case TLMonitorBuilder => (args: TLMonitorArgs) => Some(LazyModule(new TLMonitor(args)))
  case TLFuzzReadyValid => false
  case TLCombinationalCheck => false
  //Memory Parameters
  case NExtTopInterrupts => 2
  case SOCBusConfig => site(L1toL2Config)
  case PeripheryBusConfig => TLBusConfig(beatBytes = 4)
  case PeripheryBusArithmetic => true
  // Note that PLIC asserts that this is > 0.
  case IncludeJtagDTM => false
  case ZeroConfig => ZeroConfig(base=0xa000000L, size=0x2000000L, beatBytes=8)
  case ExtMem => MasterConfig(base=0x80000000L, size=0x10000000L, beatBytes=8, idBits=4)
  case ExtBus => MasterConfig(base=0x60000000L, size=0x20000000L, beatBytes=8, idBits=4)
  case ExtIn  => SlaveConfig(beatBytes=8, idBits=8, sourceBits=2)
  case RTCPeriod => 100 // gives 10 MHz RTC assuming 1 GHz uncore clock
})

/** Actual elaboratable target Configs */

class BaseConfig extends Config(new BaseCoreplexConfig ++ new BasePlatformConfig)
class DefaultConfig extends Config(new WithBlockingL1 ++ new WithNBigCores(1) ++ new BaseConfig)

class DefaultL2Config extends Config(new WithL2Cache ++ new WithNBigCores(1) ++ new BaseConfig)
class DefaultBufferlessConfig extends Config(
  new WithBufferlessBroadcastHub ++ new WithNBigCores(1) ++ new BaseConfig)

class FPGAConfig extends Config(Parameters.empty)
class DefaultFPGAConfig extends Config(new FPGAConfig ++ new BaseConfig)
class DefaultL2FPGAConfig extends Config(
  new WithL2Capacity(64) ++ new WithL2Cache ++ new DefaultFPGAConfig)

class DefaultSmallConfig extends Config(new WithNSmallCores(1) ++ new BaseConfig)
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

class RoccExampleConfig extends Config(new WithRoccExample ++ new DefaultConfig)

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
  new WithNBigCores(2) ++ new WithL2Cache ++ new BaseConfig)
class HeterogeneousDualCoreConfig extends Config(
  new WithNSmallCores(1) ++ new WithNBigCores(1) ++ new WithL2Cache ++ new BaseConfig)

class TinyConfig extends Config(
  new WithScratchpad ++
  new WithRV32 ++
  new WithStatelessBridge ++
  new WithNSmallCores(1) ++ new BaseConfig)

/* Composable partial function Configs to set individual parameters */

class WithEdgeDataBits(dataBits: Int) extends Config((site, here, up) => {
  case ExtMem => up(ExtMem, site).copy(beatBytes = dataBits/8)
  case ZeroConfig => up(ZeroConfig, site).copy(beatBytes = dataBits/8)
})

class WithJtagDTM extends Config ((site, here, up) => {
  case IncludeJtagDTM => true
})

class WithNoPeripheryArithAMO extends Config ((site, here, up) => {
  case PeripheryBusArithmetic => false
})

class With64BitPeriphery extends Config ((site, here, up) => {
  case PeripheryBusConfig => TLBusConfig(beatBytes = 8)
})

class WithoutTLMonitors extends Config ((site, here, up) => {
  case TLMonitorBuilder => (args: TLMonitorArgs) => None
})

class WithNExtTopInterrupts(nExtInts: Int) extends Config((site, here, up) => {
  case NExtTopInterrupts => nExtInts
})

class WithRTCPeriod(nCycles: Int) extends Config((site, here, up) => {
  case RTCPeriod => nCycles
})

class WithNMemoryChannels(n: Int) extends Config((site, here, up) => {
  case BankedL2Config => up(BankedL2Config, site).copy(nMemoryChannels = n)
})

class WithExtMemSize(n: Long) extends Config((site, here, up) => {
  case ExtMem => up(ExtMem, site).copy(size = n)
})

class WithDTS(model: String, compat: Seq[String]) extends Config((site, here, up) => {
  case DTSModel => model
  case DTSCompat => compat
})

class WithScratchpad extends Config(new WithNMemoryChannels(0) ++ new WithDataScratchpad(16384))

class DefaultFPGASmallConfig extends Config(new WithNSmallCores(1) ++ new DefaultFPGAConfig)
