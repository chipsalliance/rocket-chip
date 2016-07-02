package rocketchip

import Chisel._
import groundtest._
import rocket._
import uncore.tilelink._
import uncore.coherence._
import uncore.agents._
import uncore.devices.NTiles
import junctions._
import scala.collection.mutable.LinkedHashSet
import cde.{Parameters, Config, Dump, Knob, CDEMatchError}
import scala.math.max
import ConfigUtils._

class WithGroundTest extends Config(
  (pname, site, here) => pname match {
    case TLKey("L1toL2") =>
      TileLinkParameters(
        coherencePolicy = new MESICoherence(site(L2DirectoryRepresentation)),
        nManagers = site(NBanksPerMemoryChannel)*site(NMemoryChannels) + 1,
        nCachingClients = site(NCachedTileLinkPorts),
        nCachelessClients = site(NUncachedTileLinkPorts),
        maxClientXacts = max(
          site(NMSHRs) + 1,
          site(GroundTestMaxXacts)),
        maxClientsPerPort = 1,
        maxManagerXacts = site(NAcquireTransactors) + 2,
        dataBeats = 8,
        dataBits = site(CacheBlockBytes)*8)
    case BuildTiles => {
      val groundtest = if (site(XLen) == 64)
        DefaultTestSuites.groundtest64
      else
        DefaultTestSuites.groundtest32
      TestGeneration.addSuite(groundtest("p"))
      TestGeneration.addSuite(DefaultTestSuites.emptyBmarks)
      (0 until site(NTiles)).map { i =>
        (r: Bool, p: Parameters) =>
          Module(new GroundTestTile(i, r)(p.alterPartial({
            case TLId => "L1toL2"
            case NCachedTileLinkPorts =>
              if (p(GroundTestCachedClients) > 0) 1 else 0
            case NUncachedTileLinkPorts => p(GroundTestUncachedClients)
          })))
      }
    }
    case GroundTestCachedClients => 0
    case GroundTestUncachedClients => 0
    case GroundTestNPTW => 0
    case GroundTestMaxXacts => 1
    case GroundTestCSRs => Nil
    case TohostAddr => BigInt("80001000", 16)
    case RoccNCSRs => site(GroundTestCSRs).size
    case UseFPU => false
    case UseAtomics => false
    case _ => throw new CDEMatchError
  })

class WithComparator extends Config(
  (pname, site, here) => pname match {
    case GroundTestUncachedClients => site(ComparatorKey).targets.size
    case BuildGroundTest =>
      (id: Int, p: Parameters) => Module(new ComparatorCore()(p))
    case ComparatorKey => ComparatorParameters(
      targets    = Seq(0L, 0x100L).map(site(GlobalAddrMap)("mem").start.longValue + _),
      width      = 8,
      operations = 1000,
      atomics    = site(UseAtomics),
      prefetches = site("COMPARATOR_PREFETCHES"))
    case TohostAddr => BigInt("80001000", 16) // quit test by writing here
    case UseFPU => false
    case UseAtomics => false
    case "COMPARATOR_PREFETCHES" => false
    case _ => throw new CDEMatchError
  })

class WithAtomics extends Config(
  (pname, site, here) => pname match {
    case UseAtomics => true
  })

class WithPrefetches extends Config(
  (pname, site, here) => pname match {
    case "COMPARATOR_PREFETCHES" => true
  })

class WithMemtest extends Config(
  (pname, site, here) => pname match {
    case GroundTestCachedClients => 1
    case GroundTestUncachedClients => 1
    case GroundTestNPTW => 0
    case MaxGenerateRequests => 128
    case GeneratorStartAddress => site(TohostAddr) + BigInt(site(CacheBlockBytes))
    case BuildGroundTest =>
      (id: Int, p: Parameters) => Module(new GeneratorTest(id)(p))
    case _ => throw new CDEMatchError
  })

class WithCacheFillTest extends Config(
  (pname, site, here) => pname match {
    case GroundTestUncachedClients => 1
    case BuildGroundTest =>
      (id: Int, p: Parameters) => Module(new CacheFillTest()(p))
    case _ => throw new CDEMatchError
  },
  knobValues = {
    case "L2_WAYS" => 4
    case "L2_CAPACITY_IN_KB" => 4
    case _ => throw new CDEMatchError
  })

class WithBroadcastRegressionTest extends Config(
  (pname, site, here) => pname match {
    case GroundTestCachedClients => 1
    case GroundTestUncachedClients => 1
    case BuildGroundTest =>
      (id: Int, p: Parameters) => Module(new RegressionTest()(p))
    case GroundTestRegressions =>
      (p: Parameters) => RegressionTests.broadcastRegressions(p)
    case GroundTestMaxXacts => 3
    case _ => throw new CDEMatchError
  })

class WithCacheRegressionTest extends Config(
  (pname, site, here) => pname match {
    case GroundTestCachedClients => 1
    case GroundTestUncachedClients => 1
    case BuildGroundTest =>
      (id: Int, p: Parameters) => Module(new RegressionTest()(p))
    case GroundTestRegressions =>
      (p: Parameters) => RegressionTests.cacheRegressions(p)
    case GroundTestMaxXacts => 5
    case _ => throw new CDEMatchError
  })

class WithDmaTest extends Config(
  (pname, site, here) => pname match {
    case GroundTestNPTW => 1
    case GroundTestUncachedClients => 1
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
    case _ => throw new CDEMatchError
  })

class WithDmaStreamTest extends Config(
  (pname, site, here) => pname match {
    case GroundTestNPTW => 1
    case GroundTestUncachedClients => 1
    case BuildGroundTest =>
      (id: Int, p: Parameters) => Module(new DmaStreamTest()(p))
    case DmaStreamTestSettings => DmaStreamTestConfig(
      source = 0x10, dest = 0x28, len = 0x18,
      size = site(StreamLoopbackWidth) / 8)
    case GroundTestCSRs =>
      Seq(DmaCtrlRegNumbers.CSR_BASE + DmaCtrlRegNumbers.OUTSTANDING)
    case _ => throw new CDEMatchError
  })

class WithNastiConverterTest extends Config(
  (pname, site, here) => pname match {
    case GroundTestUncachedClients => 1
    case BuildGroundTest =>
      (id: Int, p: Parameters) => Module(new NastiConverterTest()(p))
    case _ => throw new CDEMatchError
  })

class WithUnitTest extends Config(
  (pname, site, here) => pname match {
    case BuildGroundTest =>
      (id: Int, p: Parameters) => Module(new UnitTestSuite()(p))
    case _ => throw new CDEMatchError
  })

class WithTraceGen extends Config(
  (pname, site, here) => pname match {
    case GroundTestCachedClients => 1
    case BuildGroundTest =>
      (id: Int, p: Parameters) => Module(new GroundTestTraceGenerator(id)(p))
    case MaxGenerateRequests => 128
    case AddressBag => List(0x8, 0x10, 0x108, 0x100008)
    case _ => throw new CDEMatchError
  })

class ComparatorConfig extends Config(new WithComparator ++ new GroundTestConfig)
class ComparatorL2Config extends Config(
  new WithAtomics ++ new WithPrefetches ++
  new WithL2Cache ++ new ComparatorConfig)
class GroundTestConfig extends Config(new WithGroundTest ++ new BaseConfig)
class MemtestConfig extends Config(new WithMemtest ++ new GroundTestConfig)
class MemtestL2Config extends Config(
  new WithMemtest ++ new WithL2Cache ++ new GroundTestConfig)
class CacheFillTestConfig extends Config(
  new WithCacheFillTest ++ new WithPLRU ++ new WithL2Cache ++ new GroundTestConfig)
class BroadcastRegressionTestConfig extends Config(
  new WithBroadcastRegressionTest ++ new GroundTestConfig)
class CacheRegressionTestConfig extends Config(
  new WithCacheRegressionTest ++ new WithL2Cache ++ new GroundTestConfig)
class DmaTestConfig extends Config(new WithDmaTest ++ new WithL2Cache ++ new GroundTestConfig)
class DmaStreamTestConfig extends Config(new WithDmaStreamTest ++ new WithStreamLoopback ++ new WithL2Cache ++ new GroundTestConfig)
class NastiConverterTestConfig extends Config(new WithNastiConverterTest ++ new GroundTestConfig)
class UnitTestConfig extends Config(new WithUnitTest ++ new GroundTestConfig)
class TraceGenConfig extends Config(new WithNCores(2) ++ new WithL2Cache ++ new WithTraceGen ++ new GroundTestConfig)

class WithNCachedGenerators(n: Int) extends Config(
  (pname, site, here) => pname match {
    case GroundTestCachedClients => n
    case _ => throw new CDEMatchError
  })

class WithNUncachedGenerators(n: Int) extends Config(
  (pname, site, here) => pname match {
    case GroundTestUncachedClients => n
    case _ => throw new CDEMatchError
  })

class FancyMemtestConfig extends Config(
  new WithNCachedGenerators(1) ++ new WithNUncachedGenerators(2) ++
  new WithNCores(2) ++
  new WithNMemoryChannels(2) ++ new WithNBanksPerMemChannel(4) ++
  new WithMemtest ++ new WithL2Cache ++ new GroundTestConfig)

class MIF128BitComparatorConfig extends Config(
  new WithMIFDataBits(128) ++ new ComparatorConfig)
class MIF128BitMemtestConfig extends Config(
  new WithMIFDataBits(128) ++ new MemtestConfig)

class MIF32BitComparatorConfig extends Config(
  new WithMIFDataBits(32) ++ new ComparatorConfig)
class MIF32BitMemtestConfig extends Config(
  new WithMIFDataBits(32) ++ new MemtestConfig)
