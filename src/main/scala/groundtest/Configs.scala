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
import tile.TileKey
import junctions._
import config._
import coreplex._
import rocketchip._

/** Actual testing target Configs */

class GroundTestConfig extends Config(new WithGroundTestTiles ++ new BaseConfig)

class ComparatorConfig extends Config(
  new WithComparator(1) ++ new GroundTestConfig)
class ComparatorL2Config extends Config(
  new WithAtomics ++ new WithPrefetches ++
  new WithL2Cache ++ new ComparatorConfig)
class ComparatorBufferlessConfig extends Config(
  new WithBufferlessBroadcastHub ++ new ComparatorConfig)
class ComparatorStatelessConfig extends Config(
  new WithStatelessBridge ++ new ComparatorConfig)

class MemtestConfig extends Config(new WithMemtest(1) ++ new GroundTestConfig)
class MemtestL2Config extends Config(
  new WithL2Cache ++ new MemtestConfig)
class MemtestBufferlessConfig extends Config(
  new WithBufferlessBroadcastHub ++ new MemtestConfig)
class MemtestStatelessConfig extends Config(
  new WithStatelessBridge ++ new MemtestConfig)
// Test ALL the things
class FancyMemtestConfig extends Config(
  new WithMemtest(2) ++
  new WithNMemoryChannels(2) ++ new WithNBanksPerMemChannel(4) ++
  new WithL2Cache ++ new GroundTestConfig)

class CacheFillTestConfig extends Config(
  new WithNL2Ways(4) ++ new WithL2Capacity(4) ++ 
  new WithCacheFillTest(1) ++ new WithL2Cache ++ new GroundTestConfig)

class BroadcastRegressionTestConfig extends Config(
  new WithBroadcastRegressionTest(1) ++ new GroundTestConfig)
class BufferlessRegressionTestConfig extends Config(
  new WithBufferlessBroadcastHub ++ new BroadcastRegressionTestConfig)
class CacheRegressionTestConfig extends Config(
  new WithCacheRegressionTest(1) ++ new WithL2Cache ++ new GroundTestConfig)

class TraceGenConfig extends Config(
  new WithTraceGen(2) ++ new GroundTestConfig)
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

class WithGroundTestTiles extends Config((site, here, up) => {
  case TileKey => site(GroundTestKey).head
  case NTiles => site(GroundTestKey).size
})

class WithComparator(n: Int) extends Config((site, here, up) => {
  case GroundTestKey => Seq.fill(n) {
    GroundTestTileParams(uncached = 2, dcache = None)
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

class WithMemtest(n: Int) extends Config((site, here, up) => {
  case GroundTestKey => Seq.fill(n) {
    GroundTestTileParams(1, 1)
  }
  case GeneratorKey => TrafficGeneratorParameters(
    maxRequests = 128,
    startAddress = BigInt(site(ExtMem).base))
  case BuildGroundTest =>
    (p: Parameters) => Module(new GeneratorTest()(p))
})

class WithCacheFillTest(n: Int) extends Config((site, here, up) => {
  case GroundTestKey => Seq.fill(n) {
    GroundTestTileParams(uncached = 1)
  }
  case BuildGroundTest =>
    (p: Parameters) => Module(new CacheFillTest()(p))
})

class WithBroadcastRegressionTest(n: Int) extends Config((site, here, up) => {
  case GroundTestKey => Seq.fill(n) {
    GroundTestTileParams(1, 1, maxXacts = 3)
  }
  case BuildGroundTest =>
    (p: Parameters) => Module(new RegressionTest()(p))
  case GroundTestRegressions =>
    (p: Parameters) => RegressionTests.broadcastRegressions(p)
})

class WithCacheRegressionTest(n: Int) extends Config((site, here, up) => {
  case GroundTestKey => Seq.fill(n) {
    GroundTestTileParams(1, 1, maxXacts = 5)
  }
  case BuildGroundTest =>
    (p: Parameters) => Module(new RegressionTest()(p))
  case GroundTestRegressions =>
    (p: Parameters) => RegressionTests.cacheRegressions(p)
})

class WithTraceGen(n: Int) extends Config((site, here, up) => {
  case GroundTestKey => Seq.fill(n) {
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
