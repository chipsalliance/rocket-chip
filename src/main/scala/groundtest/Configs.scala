// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package groundtest

import Chisel._
import rocket._
import diplomacy._
import uncore.tilelink._
import uncore.coherence._
import uncore.agents._
import uncore.util._
import uncore.devices.NTiles
import junctions._
import config._
import coreplex._
import rocketchip._

/** Actual testing target Configs */

class GroundTestConfig extends Config(new BaseConfig)

class ComparatorConfig extends Config(
  new WithComparator ++ new GroundTestConfig)
class ComparatorL2Config extends Config(
  new WithAtomics ++ new WithPrefetches ++
  new WithL2Cache ++ new ComparatorConfig)
class ComparatorBufferlessConfig extends Config(
  new WithBufferlessBroadcastHub ++ new ComparatorConfig)
class ComparatorStatelessConfig extends Config(
  new WithStatelessBridge ++ new ComparatorConfig)

class MemtestConfig extends Config(new WithMemtest ++ new GroundTestConfig)
class MemtestL2Config extends Config(
  new WithL2Cache ++ new MemtestConfig)
class MemtestBufferlessConfig extends Config(
  new WithBufferlessBroadcastHub ++ new MemtestConfig)
class MemtestStatelessConfig extends Config(
  new WithNGenerators(0, 1) ++ new WithStatelessBridge ++ new MemtestConfig)
// Test ALL the things
class FancyMemtestConfig extends Config(
  new WithNGenerators(1, 2) ++ new WithNCores(2) ++ new WithMemtest ++
  new WithNMemoryChannels(2) ++ new WithNBanksPerMemChannel(4) ++
  new WithL2Cache ++ new GroundTestConfig)

class CacheFillTestConfig extends Config(
  new WithNL2Ways(4) ++ new WithL2Capacity(4) ++ new WithCacheFillTest ++ new WithL2Cache ++ new GroundTestConfig)

class BroadcastRegressionTestConfig extends Config(
  new WithBroadcastRegressionTest ++ new GroundTestConfig)
class BufferlessRegressionTestConfig extends Config(
  new WithBufferlessBroadcastHub ++ new BroadcastRegressionTestConfig)
class CacheRegressionTestConfig extends Config(
  new WithCacheRegressionTest ++ new WithL2Cache ++ new GroundTestConfig)

class TraceGenConfig extends Config(
  new WithNCores(2) ++ new WithTraceGen ++ new GroundTestConfig)
class TraceGenBufferlessConfig extends Config(
  new WithBufferlessBroadcastHub ++ new TraceGenConfig)
class TraceGenL2Config extends Config(
  new WithNL2Ways(1) ++ new WithL2Capacity(32 * 64 / 1024) ++
  new WithL2Cache ++ new TraceGenConfig)

class Edge128BitComparatorConfig extends Config(
  new WithEdgeDataBits(128) ++ new ComparatorConfig)
class Edge128BitMemtestConfig extends Config(
  new WithEdgeDataBits(128) ++ new MemtestConfig)

class Edge32BitComparatorConfig extends Config(
  new WithEdgeDataBits(32) ++ new ComparatorL2Config)
class Edge32BitMemtestConfig extends Config(
  new WithEdgeDataBits(32) ++ new MemtestConfig)

/* Composable Configs to set individual parameters */

class WithComparator extends Config((site, here, up) => {
  case GroundTestKey => Seq.fill(site(NTiles)) {
    GroundTestTileParams(uncached = 2)
  }
  case BuildGroundTest =>
    (p: Parameters) => Module(new ComparatorCore()(p))
  case ComparatorKey => ComparatorParameters(
    targets    = Seq(site(ExtMem).base, testRamAddr),
    width      = 8,
    operations = 1000,
    atomics    = false,
    prefetches = false)
})

class WithAtomics extends Config((site, here, up) => {
  case ComparatorKey => up(ComparatorKey, site).copy(atomics = true)
})

class WithPrefetches extends Config((site, here, up) => {
  case ComparatorKey => up(ComparatorKey, site).copy(prefetches = true)
})

class WithMemtest extends Config((site, here, up) => {
  case GroundTestKey => Seq.fill(site(NTiles)) {
    GroundTestTileParams(1, 1)
  }
  case GeneratorKey => TrafficGeneratorParameters(
    maxRequests = 128,
    startAddress = BigInt(site(ExtMem).base))
  case BuildGroundTest =>
    (p: Parameters) => Module(new GeneratorTest()(p))
})

class WithNGenerators(nUncached: Int, nCached: Int) extends Config((site, here, up) => {
  case GroundTestKey => Seq.fill(site(NTiles)) {
    GroundTestTileParams(nUncached, nCached)
  }
})

class WithCacheFillTest extends Config((site, here, up) => {
  case GroundTestKey => Seq.fill(site(NTiles)) {
    GroundTestTileParams(uncached = 1)
  }
  case BuildGroundTest =>
    (p: Parameters) => Module(new CacheFillTest()(p))
})

class WithBroadcastRegressionTest extends Config((site, here, up) => {
  case GroundTestKey => Seq.fill(site(NTiles)) {
    GroundTestTileParams(1, 1, maxXacts = 3)
  }
  case BuildGroundTest =>
    (p: Parameters) => Module(new RegressionTest()(p))
  case GroundTestRegressions =>
    (p: Parameters) => RegressionTests.broadcastRegressions(p)
})

class WithCacheRegressionTest extends Config((site, here, up) => {
  case GroundTestKey => Seq.fill(site(NTiles)) {
    GroundTestTileParams(1, 1, maxXacts = 5)
  }
  case BuildGroundTest =>
    (p: Parameters) => Module(new RegressionTest()(p))
  case GroundTestRegressions =>
    (p: Parameters) => RegressionTests.cacheRegressions(p)
})

class WithTraceGen extends Config((site, here, up) => {
  case GroundTestKey => Seq.fill(site(NTiles)) {
    GroundTestTileParams(dcache = Some(DCacheParams(nSets = 16, nWays = 1)))
  }
  case BuildGroundTest =>
    (p: Parameters) => Module(new GroundTestTraceGenerator()(p))
  case GeneratorKey => TrafficGeneratorParameters(
    maxRequests = 8192,
    startAddress = 0)
  case AddressBag => {
    val nSets = 2
    val nWays = 1
    val blockOffset = site(CacheBlockOffsetBits)
    val nBeats = site(CacheBlockBytes)/site(L1toL2Config).beatBytes
    List.tabulate(4 * nWays) { i =>
      Seq.tabulate(nBeats) { j => BigInt((j * 8) + ((i * nSets) << blockOffset)) }
    }.flatten
  }
})
