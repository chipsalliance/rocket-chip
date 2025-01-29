// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.system

import org.chipsalliance.cde.config.Config
import freechips.rocketchip.subsystem._
import freechips.rocketchip.rocket.{WithNBigCores, WithNMedCores, WithNSmallCores, WithRV32, WithFP16, WithHypervisor, With1TinyCore, WithScratchpadsOnly, WithCloneRocketTiles, WithB}

class WithJtagDTMSystem extends freechips.rocketchip.subsystem.WithJtagDTM
class WithDebugSBASystem extends freechips.rocketchip.subsystem.WithDebugSBA
class WithDebugAPB extends freechips.rocketchip.subsystem.WithDebugAPB

class BaseConfig extends Config(
  new WithDefaultMemPort ++
  new WithDefaultMMIOPort ++
  new WithDefaultManagerPort ++
  new WithTimebase(BigInt(1000000)) ++ // 1 MHz
  new WithDTS("freechips,rocketchip-unknown", Nil) ++
  new WithNExtTopInterrupts(2) ++
  new BaseSubsystemConfig
)

class DefaultConfig extends Config(new WithNBigCores(1) ++ new WithCoherentBusTopology ++ new BaseConfig)

class DefaultBufferlessConfig extends Config(new WithBufferlessBroadcastHub ++ new DefaultConfig)
class DefaultSmallConfig extends Config(new WithNSmallCores(1) ++ new WithCoherentBusTopology ++ new BaseConfig)
class DefaultRV32Config extends Config(new WithRV32 ++ new DefaultConfig)
class DefaultFP16Config extends Config(new WithFP16 ++ new DefaultConfig)
class DefaultBConfig extends Config(new WithB ++ new DefaultConfig)
class DefaultRV32BConfig extends Config(new WithB ++ new DefaultRV32Config)

class HypervisorConfig extends Config(new WithHypervisor ++ new DefaultConfig)

class DualBankConfig extends Config(new WithNBanks(2) ++ new DefaultConfig)
class DualCoreConfig extends Config(new WithNBigCores(2) ++ new WithCoherentBusTopology ++ new BaseConfig)
class DualChannelConfig extends Config(new WithNMemoryChannels(2) ++ new DefaultConfig)
class EightChannelConfig extends Config(new WithNMemoryChannels(8) ++ new DefaultConfig)

class ClusterConfig extends Config(
  new WithNBigCores(2, InCluster(3)) ++
  new WithNBigCores(2, InCluster(1)) ++
  new WithNBigCores(2, InCluster(0)) ++
  new WithCluster(3, location=InCluster(2)) ++
  new WithCluster(2) ++
  new WithCluster(1) ++
  new WithCluster(0) ++
  new DefaultConfig
)

class DualChannelDualBankConfig extends Config(
  new WithNMemoryChannels(2) ++
  new WithNBanks(4) ++ new DefaultConfig
)

class RoccExampleConfig extends Config(new WithRoccExample ++ new DefaultConfig)

class HeterogeneousTileExampleConfig extends Config(
  new WithNBigCores(n = 1) ++
  new WithNMedCores(n = 1) ++
  new WithNSmallCores(n = 1) ++
  new WithCoherentBusTopology ++
  new BaseConfig
)

class Edge128BitConfig extends Config(
  new WithEdgeDataBits(128) ++ new DefaultConfig
)
class Edge32BitConfig extends Config(
  new WithEdgeDataBits(32) ++ new DefaultConfig
)

class SingleChannelBenchmarkConfig extends Config(new DefaultConfig)
class DualChannelBenchmarkConfig extends Config(new WithNMemoryChannels(2) ++ new SingleChannelBenchmarkConfig)
class QuadChannelBenchmarkConfig extends Config(new WithNMemoryChannels(4) ++ new SingleChannelBenchmarkConfig)
class OctoChannelBenchmarkConfig extends Config(new WithNMemoryChannels(8) ++ new SingleChannelBenchmarkConfig)

class TinyConfig extends Config(
  new WithNoMemPort ++
  new WithNMemoryChannels(0) ++
  new WithNBanks(0) ++
  new With1TinyCore ++
  new WithIncoherentBusTopology ++
  new BaseConfig
)

class MemPortOnlyConfig extends Config(
  new WithNoMMIOPort ++
  new WithNoManagerPort ++
  new DefaultConfig
)

class MMIOPortOnlyConfig extends Config(
  new WithNoManagerPort ++
  new WithNoMemPort ++
  new WithNMemoryChannels(0) ++
  new WithNBanks(0) ++
  new WithIncoherentTiles ++
  new WithScratchpadsOnly ++
  new WithIncoherentBusTopology ++
  new DefaultConfig
)

class BaseFPGAConfig extends Config(new BaseConfig ++ new WithCoherentBusTopology)
class DefaultFPGAConfig extends Config(new WithNSmallCores(1) ++ new BaseFPGAConfig)

class CloneTileConfig extends Config(new WithCloneRocketTiles(7) ++ new WithNBigCores(1) ++ new WithCoherentBusTopology ++ new BaseConfig)
