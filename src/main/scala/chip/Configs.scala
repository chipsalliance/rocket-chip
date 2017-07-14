// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.chip

import Chisel._

import freechips.rocketchip.config._
import freechips.rocketchip.coreplex._
import freechips.rocketchip.devices._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile.XLen
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

import scala.math.max
import scala.collection.mutable.{LinkedHashSet, ListBuffer}
import scala.collection.immutable.HashMap

import DefaultTestSuites._

class BasePlatformConfig extends Config((site, here, up) => {
  // DTS descriptive parameters
  case DTSModel => "ucbbar,rocketchip-unknown"
  case DTSCompat => Nil
  case DTSTimebase => BigInt(1000000) // 1 MHz
  case RTCPeriod => 1000 // Implies coreplex clock is DTSTimebase * RTCPeriod = 1 GHz
  // TileLink connection parameters
  case TLMonitorBuilder => (args: TLMonitorArgs) => Some(LazyModule(new TLMonitor(args)))
  case TLCombinationalCheck => false
  //Memory Parameters
  case NExtTopInterrupts => 2
  case SOCBusConfig => site(L1toL2Config)
  case PeripheryBusConfig => TLBusConfig(beatBytes = 4)
  case PeripheryBusArithmetic => true
  // Default BootROMParams
  case PeripheryBootROMKey => BootROMParams()
  // Note that PLIC asserts that this is > 0.
  case IncludeJtagDTM => false
  case JtagDTMKey => new JtagDTMKeyDefault()
  case ZeroConfig => ZeroConfig(base=0xa000000L, size=0x2000000L, beatBytes=8)
  case ErrorConfig => ErrorConfig(Seq(AddressSet(0x3000, 0xfff)))
  case ExtMem => MasterConfig(base=0x80000000L, size=0x10000000L, beatBytes=8, idBits=4)
  case ExtBus => MasterConfig(base=0x60000000L, size=0x20000000L, beatBytes=8, idBits=4)
  case ExtIn  => SlaveConfig(beatBytes=8, idBits=8, sourceBits=4)
})

/** Actual elaboratable target Configs */

class BaseConfig extends Config(new BaseCoreplexConfig ++ new BasePlatformConfig)
class DefaultConfig extends Config(new WithNBigCores(1) ++ new BaseConfig)

class DefaultBufferlessConfig extends Config(
  new WithBufferlessBroadcastHub ++ new WithNBigCores(1) ++ new BaseConfig)

class FPGAConfig extends Config(Parameters.empty)
class DefaultFPGAConfig extends Config(new FPGAConfig ++ new BaseConfig)

class DefaultSmallConfig extends Config(new WithNSmallCores(1) ++ new BaseConfig)
class DefaultRV32Config extends Config(new WithRV32 ++ new DefaultConfig)

class DualBankConfig extends Config(
  new WithNBanksPerMemChannel(2) ++ new BaseConfig)

class DualChannelConfig extends Config(new WithNMemoryChannels(2) ++ new BaseConfig)

class DualChannelDualBankConfig extends Config(
  new WithNMemoryChannels(2) ++
  new WithNBanksPerMemChannel(2) ++ new BaseConfig)

class RoccExampleConfig extends Config(new WithRoccExample ++ new DefaultConfig)

class Edge128BitConfig extends Config(
  new WithEdgeDataBits(128) ++ new BaseConfig)
class Edge32BitConfig extends Config(
  new WithEdgeDataBits(32) ++ new BaseConfig)

class SingleChannelBenchmarkConfig extends Config(new DefaultConfig)
class DualChannelBenchmarkConfig extends Config(new WithNMemoryChannels(2) ++ new SingleChannelBenchmarkConfig)
class QuadChannelBenchmarkConfig extends Config(new WithNMemoryChannels(4) ++ new SingleChannelBenchmarkConfig)
class OctoChannelBenchmarkConfig extends Config(new WithNMemoryChannels(8) ++ new SingleChannelBenchmarkConfig)

class EightChannelConfig extends Config(new WithNMemoryChannels(8) ++ new BaseConfig)

class DualCoreConfig extends Config(
  new WithNBigCores(2) ++ new BaseConfig)
class HeterogeneousDualCoreConfig extends Config(
  new WithNSmallCores(1) ++ new WithNBigCores(1) ++ new BaseConfig)

class TinyConfig extends Config(
  new WithNMemoryChannels(0) ++
  new WithStatelessBridge ++
  new BaseConfig().alter((site, here, up) => {
    case XLen => 32
    case RocketTilesKey => Seq(
      RocketTileParams(
        core = RocketCoreParams(
          useVM = false,
          fpu = None,
          mulDiv = Some(MulDivParams(mulUnroll = 8))),
        btb = None,
        dcache = Some(DCacheParams(
          rowBits = site(L1toL2Config).beatBytes*8,
          nSets = 256, // 16Kb scratchpad
          nWays = 1,
          nTLBEntries = 4,
          nMSHRs = 0,
          blockBytes = site(CacheBlockBytes),
          scratch = Some(0x80000000L))),
        icache = Some(ICacheParams(
          rowBits = site(L1toL2Config).beatBytes*8,
          nSets = 64,
          nWays = 1,
          nTLBEntries = 4,
          blockBytes = site(CacheBlockBytes)))))}))

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

class WithTimebase(hertz: BigInt) extends Config((site, here, up) => {
  case DTSTimebase => hertz
})

class DefaultFPGASmallConfig extends Config(new WithNSmallCores(1) ++ new DefaultFPGAConfig)
