package rocketchip

import Chisel._
import groundtest._
import rocket._
import uncore._
import junctions._
import scala.collection.mutable.LinkedHashSet
import cde.{Parameters, Config, Dump, Knob}
import scala.math.max
import ConfigUtils._

class WithGroundTest extends Config(
  (pname, site, here) => pname match {
    case TLKey("L1toL2") =>
      TileLinkParameters(
        coherencePolicy = new MESICoherence(site(L2DirectoryRepresentation)),
        nManagers = site(NBanksPerMemoryChannel)*site(NMemoryChannels) + 1,
        nCachingClients = site(NTiles),
        nCachelessClients = site(NTiles) + site(ExtraL1Clients),
        maxClientXacts = max(
          site(NMSHRs) + 1,
          if (site(BuildRoCC).isEmpty) 1 else site(RoccMaxTaggedMemXacts)),
        maxClientsPerPort = if (site(BuildRoCC).isEmpty) 1 else 2,
        maxManagerXacts = site(NAcquireTransactors) + 2,
        dataBits = site(CacheBlockBytes)*8)
    case BuildTiles => {
      TestGeneration.addSuite(new AssemblyGroundTestSuite)
      TestGeneration.addSuite(new BenchmarkGroundTestSuite)
      (0 until site(NTiles)).map { i =>
        (r: Bool, p: Parameters) =>
          Module(new GroundTestTile(i, r)
            (p.alterPartial({case TLId => "L1toL2"})))
      }
    }
    case GroundTestMaxXacts => 1
    case GroundTestCSRs => Nil
    case RoccNCSRs => site(GroundTestCSRs).size
    case UseFPU => false
  })

class WithMemtest extends Config(
  (pname, site, here) => pname match {
    case NGenerators => site(NTiles)
    case GenerateUncached => true
    case GenerateCached => true
    case MaxGenerateRequests => 128
    case GeneratorStartAddress => 0
    case BuildGroundTest =>
      (id: Int, p: Parameters) => Module(new GeneratorTest(id)(p))
  })

class WithCacheFillTest extends Config(
  (pname, site, here) => pname match {
    case BuildGroundTest =>
      (id: Int, p: Parameters) => Module(new CacheFillTest()(p))
  },
  knobValues = {
    case "L2_WAYS" => 4
    case "L2_CAPACITY_IN_KB" => 4
  })

class WithBroadcastRegressionTest extends Config(
  (pname, site, here) => pname match {
    case BuildGroundTest =>
      (id: Int, p: Parameters) => Module(new RegressionTest()(p))
    case GroundTestRegressions =>
      (p: Parameters) => RegressionTests.broadcastRegressions(p)
    case GroundTestMaxXacts => 3
  })

class WithCacheRegressionTest extends Config(
  (pname, site, here) => pname match {
    case BuildGroundTest =>
      (id: Int, p: Parameters) => Module(new RegressionTest()(p))
    case GroundTestRegressions =>
      (p: Parameters) => RegressionTests.cacheRegressions(p)
    case GroundTestMaxXacts => 3
  })

class WithDmaTest extends Config(
  (pname, site, here) => pname match {
    case BuildGroundTest =>
      (id: Int, p: Parameters) => Module(new DmaTest()(p))
    case DmaTestSet => DmaTestCases(
      (0x00001FF0, 0x00002FF4, 72),
      (0x00001FF4, 0x00002FF0, 72),
      (0x00001FF0, 0x00002FE0, 72),
      (0x00001FE0, 0x00002FF0, 72),
      (0x00884DA4, 0x008836C0, 40),
      (0x00800008, 0x00800008, 64))
    case DmaTestDataStart => 0x3012CC00
    case DmaTestDataStride => 8
  })

class WithDmaStreamTest extends Config(
  (pname, site, here) => pname match {
    case BuildGroundTest =>
      (id: Int, p: Parameters) => Module(new DmaStreamTest()(p))
    case DmaStreamTestSettings => DmaStreamTestConfig(
      source = 0x10, dest = 0x28, len = 0x18,
      size = site(StreamLoopbackWidth) / 8)
    case GroundTestCSRs =>
      Seq(DmaCtrlRegNumbers.CSR_BASE + DmaCtrlRegNumbers.OUTSTANDING)
  })

class WithNastiConverterTest extends Config(
  (pname, site, here) => pname match {
    case BuildGroundTest =>
      (id: Int, p: Parameters) => Module(new NastiConverterTest()(p))
  })

class WithUnitTest extends Config(
  (pname, site, here) => pname match {
    case BuildGroundTest =>
      (id: Int, p: Parameters) => Module(new UnitTestSuite()(p))
  })

class WithTraceGen extends Config(
  (pname, site, here) => pname match {
    case BuildGroundTest =>
      (id: Int, p: Parameters) => Module(new GroundTestTraceGenerator(id)(p))
    case NGenerators => site(NTiles)
    case MaxGenerateRequests => 128
    case AddressBag => List(0x8, 0x10, 0x108, 0x100008)
  })

class GroundTestConfig extends Config(new WithGroundTest ++ new DefaultConfig)
class MemtestConfig extends Config(new WithMemtest ++ new GroundTestConfig)
class MemtestL2Config extends Config(
  new WithMemtest ++ new WithL2Cache ++ new GroundTestConfig)
class CacheFillTestConfig extends Config(
  new WithCacheFillTest ++ new WithL2Cache ++ new GroundTestConfig)
class BroadcastRegressionTestConfig extends Config(
  new WithBroadcastRegressionTest ++ new GroundTestConfig)
class CacheRegressionTestConfig extends Config(
  new WithCacheRegressionTest ++ new WithL2Cache ++ new GroundTestConfig)
class DmaTestConfig extends Config(new WithDmaTest ++ new WithL2Cache ++ new GroundTestConfig)
class DmaStreamTestConfig extends Config(new WithDmaStreamTest ++ new WithStreamLoopback ++ new WithL2Cache ++ new GroundTestConfig)
class NastiConverterTestConfig extends Config(new WithNastiConverterTest ++ new GroundTestConfig)
class UnitTestConfig extends Config(new WithUnitTest ++ new GroundTestConfig)
class TraceGenConfig extends Config(new With2Cores ++ new WithL2Cache ++ new WithTraceGen ++ new GroundTestConfig)

class FancyMemtestConfig extends Config(
  new With2Cores ++ new With2MemoryChannels ++ new With2BanksPerMemChannel ++
  new WithMemtest ++ new WithL2Cache ++ new GroundTestConfig)
