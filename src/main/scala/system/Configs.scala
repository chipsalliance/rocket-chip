// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.system

import Chisel._
import freechips.rocketchip.config.Config
import freechips.rocketchip.coreplex._
import freechips.rocketchip.devices.debug._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._

class BaseSystemConfig extends Config(new BaseCoreplexConfig().alter((site,here,up) => {
  // DTS descriptive parameters
  case DTSModel => "freechips,rocketchip-unknown"
  case DTSCompat => Nil
  // External port parameters
  case IncludeJtagDTM => false
  case JtagDTMKey => new JtagDTMKeyDefault()
  case NExtTopInterrupts => 2
  case ExtMem => MasterPortParams(
                      base = 0x80000000L,
                      size = 0x10000000L,
                      beatBytes = site(MemoryBusParams).beatBytes,
                      idBits = 4)
  case ExtBus => MasterPortParams(
                      base = 0x60000000L,
                      size = 0x20000000L,
                      beatBytes = site(MemoryBusParams).beatBytes,
                      idBits = 4)
  case ExtIn  => SlavePortParams(beatBytes = 8, idBits = 8, sourceBits = 4)
  // Additional device Parameters
  case ErrorParams => ErrorParams(Seq(AddressSet(0x3000, 0xfff)))
  case BootROMParams => None
  case MaskROMParams => Seq.empty
}))

class BaseConfig extends Config(new BaseSystemConfig().alter((site,here,up) => {
  case BootROMParams => Some(BootROMParams(contentFileName = "./bootrom/bootrom.img"))
}))
class MaskROMConfig extends Config(new BaseSystemConfig().alter((site,here,up) => {
  case MaskROMParams => List(MaskROMParams(address = 0x10000, boot = true))
}))

class DefaultConfig extends Config(new WithNBigCores(1) ++ new BaseConfig)
class DefaultMaskROMConfig extends Config(new WithNBigCores(1) ++ new MaskROMConfig)

class DefaultBufferlessConfig extends Config(
  new WithBufferlessBroadcastHub ++ new WithNBigCores(1) ++ new BaseConfig)

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
  new WithNTinyCores(1) ++
  new BaseConfig)

class DefaultFPGAConfig extends Config(new BaseConfig)

class DefaultFPGASmallConfig extends Config(new WithNSmallCores(1) ++ new DefaultFPGAConfig)
