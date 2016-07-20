package rocketchip

import Chisel._
import groundtest._
import groundtest.unittests._
import groundtest.common._
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
        maxClientXacts = ((site(NMSHRs) + 1) +:
                           site(GroundTestKey).map(_.maxXacts))
                             .reduce(max(_, _)),
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
        val tileSettings = site(GroundTestKey)(i)
        (r: Bool, p: Parameters) => {
          Module(new GroundTestTile(r)(p.alterPartial({
            case TLId => "L1toL2"
            case GroundTestId => i
            case NCachedTileLinkPorts => if(tileSettings.cached > 0) 1 else 0
            case NUncachedTileLinkPorts => tileSettings.uncached
            case RoccNCSRs => tileSettings.csrs
          })))
        }
      }
    }
    case UseFPU => false
    case UseAtomics => false
    case _ => throw new CDEMatchError
  })

class WithComparator extends Config(
  (pname, site, here) => pname match {
    case GroundTestKey => Seq.fill(site(NTiles)) {
      GroundTestTileSettings(uncached = site(ComparatorKey).targets.size)
    }
    case BuildGroundTest =>
      (p: Parameters) => Module(new ComparatorCore()(p))
    case ComparatorKey => ComparatorParameters(
      targets    = Seq(0L, 0x100L).map(site(GlobalAddrMap)("mem").start.longValue + _),
      width      = 8,
      operations = 1000,
      atomics    = site(UseAtomics),
      prefetches = site("COMPARATOR_PREFETCHES"))
    case UseFPU => false
    case UseAtomics => false
    case "COMPARATOR_PREFETCHES" => false
    case _ => throw new CDEMatchError
  })

class WithAtomics extends Config(
  (pname, site, here) => pname match {
    case UseAtomics => true
    case _ => throw new CDEMatchError
  })

class WithPrefetches extends Config(
  (pname, site, here) => pname match {
    case "COMPARATOR_PREFETCHES" => true
    case _ => throw new CDEMatchError
  })

class WithMemtest extends Config(
  (pname, site, here) => pname match {
    case GroundTestKey => Seq.fill(site(NTiles)) {
      GroundTestTileSettings(1, 1)
    }
    case GeneratorKey => GeneratorParameters(
      maxRequests = 128,
      startAddress = site(GlobalAddrMap)("mem").start)
    case BuildGroundTest =>
      (p: Parameters) => Module(new GeneratorTest()(p))
    case _ => throw new CDEMatchError
  })

class WithNGenerators(nUncached: Int, nCached: Int) extends Config(
  (pname, site, here) => pname match {
    case GroundTestKey => Seq.fill(site(NTiles)) {
      GroundTestTileSettings(nUncached, nCached)
    }
    case _ => throw new CDEMatchError
  })

class WithCacheFillTest extends Config(
  (pname, site, here) => pname match {
    case GroundTestKey => Seq.fill(site(NTiles)) {
      GroundTestTileSettings(uncached = 1)
    }
    case BuildGroundTest =>
      (p: Parameters) => Module(new CacheFillTest()(p))
    case _ => throw new CDEMatchError
  },
  knobValues = {
    case "L2_WAYS" => 4
    case "L2_CAPACITY_IN_KB" => 4
    case _ => throw new CDEMatchError
  })

class WithBroadcastRegressionTest extends Config(
  (pname, site, here) => pname match {
    case GroundTestKey => Seq.fill(site(NTiles)) {
      GroundTestTileSettings(1, 1, maxXacts = 3)
    }
    case BuildGroundTest =>
      (p: Parameters) => Module(new RegressionTest()(p))
    case GroundTestRegressions =>
      (p: Parameters) => RegressionTests.broadcastRegressions(p)
    case _ => throw new CDEMatchError
  })

class WithCacheRegressionTest extends Config(
  (pname, site, here) => pname match {
    case GroundTestKey => Seq.fill(site(NTiles)) {
      GroundTestTileSettings(1, 1, maxXacts = 5)
    }
    case BuildGroundTest =>
      (p: Parameters) => Module(new RegressionTest()(p))
    case GroundTestRegressions =>
      (p: Parameters) => RegressionTests.cacheRegressions(p)
    case _ => throw new CDEMatchError
  })

class WithNastiConverterTest extends Config(
  (pname, site, here) => pname match {
    case GroundTestKey => Seq.fill(site(NTiles)) {
      GroundTestTileSettings(uncached = 1)
    }
    case GeneratorKey => GeneratorParameters(
      maxRequests = 128,
      startAddress = site(GlobalAddrMap)("mem").start)
    case BuildGroundTest =>
      (p: Parameters) => Module(new NastiConverterTest()(p))
    case _ => throw new CDEMatchError
  })

class WithUnitTest extends Config(
  (pname, site, here) => pname match {
    case GroundTestKey => Seq.fill(site(NTiles)) { GroundTestTileSettings() }
    case BuildGroundTest =>
      (p: Parameters) => Module(new UnitTestSuite()(p))
    case UnitTests => (testParams: Parameters) => {
      implicit val p = testParams
      Seq(
        Module(new MultiWidthFifoTest),
        Module(new SmiConverterTest),
        Module(new AtosConverterTest),
        Module(new NastiMemoryDemuxTest),
        Module(new ROMSlaveTest),
        Module(new TileLinkRAMTest),
        Module(new HastiTest))
    }
    case _ => throw new CDEMatchError
  })

class WithTraceGen extends Config(
  topDefinitions = (pname, site, here) => pname match {
    case GroundTestKey => Seq.fill(site(NTiles)) {
      GroundTestTileSettings(cached = 1)
    }
    case BuildGroundTest =>
      (p: Parameters) => Module(new GroundTestTraceGenerator()(p))
    case GeneratorKey => GeneratorParameters(
      maxRequests = 256,
      startAddress = 0)
    case AddressBag => {
      val nSets = 16
      val nWays = 1
      val blockOffset = site(CacheBlockOffsetBits)
      List.tabulate(2 * nWays) { i =>
        Seq.tabulate(2) { j => (i * nSets + j * 8) << blockOffset }
      }.flatten
    }
    case _ => throw new CDEMatchError
  },
  knobValues = {
    case "L1D_SETS" => 16
    case "L1D_WAYS" => 1
  })

class GroundTestConfig extends Config(new WithGroundTest ++ new BaseConfig)

class ComparatorConfig extends Config(new WithComparator ++ new GroundTestConfig)
class ComparatorL2Config extends Config(
  new WithAtomics ++ new WithPrefetches ++
  new WithL2Cache ++ new ComparatorConfig)
class ComparatorBufferlessConfig extends Config(
  new WithBufferlessBroadcastHub ++ new ComparatorConfig)

class MemtestConfig extends Config(new WithMemtest ++ new GroundTestConfig)
class MemtestL2Config extends Config(
  new WithMemtest ++ new WithL2Cache ++ new GroundTestConfig)
class MemtestBufferlessConfig extends Config(
  new WithMemtest ++ new WithBufferlessBroadcastHub ++ new GroundTestConfig)
// Test ALL the things
class FancyMemtestConfig extends Config(
  new WithNGenerators(1, 2) ++ new WithNCores(2) ++ new WithMemtest ++
  new WithNMemoryChannels(2) ++ new WithNBanksPerMemChannel(4) ++
  new WithSplitL2Metadata ++ new WithL2Cache ++ new GroundTestConfig)

class CacheFillTestConfig extends Config(
  new WithCacheFillTest ++ new WithPLRU ++ new WithL2Cache ++ new GroundTestConfig)

class BroadcastRegressionTestConfig extends Config(
  new WithBroadcastRegressionTest ++ new GroundTestConfig)
class BufferlessRegressionTestConfig extends Config(
  new WithBufferlessBroadcastHub ++ new BroadcastRegressionTestConfig)
class CacheRegressionTestConfig extends Config(
  new WithCacheRegressionTest ++ new WithL2Cache ++ new GroundTestConfig)

class NastiConverterTestConfig extends Config(new WithNastiConverterTest ++ new GroundTestConfig)
class FancyNastiConverterTestConfig extends Config(
  new WithNCores(2) ++ new WithNastiConverterTest ++
  new WithNMemoryChannels(2) ++ new WithNBanksPerMemChannel(4) ++
  new WithL2Cache ++ new GroundTestConfig)

class UnitTestConfig extends Config(new WithUnitTest ++ new GroundTestConfig)

class TraceGenConfig extends Config(
  new WithNCores(2) ++ new WithTraceGen ++ new GroundTestConfig)
class TraceGenBufferlessConfig extends Config(
  new WithBufferlessBroadcastHub ++ new TraceGenConfig)
class TraceGenL2Config extends Config(
  new WithL2Cache ++ new TraceGenConfig)

class MIF128BitComparatorConfig extends Config(
  new WithMIFDataBits(128) ++ new ComparatorConfig)
class MIF128BitMemtestConfig extends Config(
  new WithMIFDataBits(128) ++ new MemtestConfig)

class MIF32BitComparatorConfig extends Config(
  new WithMIFDataBits(32) ++ new ComparatorConfig)
class MIF32BitMemtestConfig extends Config(
  new WithMIFDataBits(32) ++ new MemtestConfig)

class WithPCIeMockupTest extends Config(
  (pname, site, here) => pname match {
    case NTiles => 2
    case GroundTestKey => Seq(
      GroundTestTileSettings(1, 1),
      GroundTestTileSettings(1))
    case GeneratorKey => GeneratorParameters(
      maxRequests = 128,
      startAddress = site(GlobalAddrMap)("mem").start)
    case BuildGroundTest =>
      (p: Parameters) => {
        val id = p(GroundTestId)
        if (id == 0) Module(new GeneratorTest()(p))
        else Module(new NastiConverterTest()(p))
      }
    case _ => throw new CDEMatchError
  })

class PCIeMockupTestConfig extends Config(
  new WithPCIeMockupTest ++ new GroundTestConfig)

class WithDirectGroundTest extends Config(
  (pname, site, here) => pname match {
    case TLKey("Outermost") => site(TLKey("L2toMC")).copy(
      maxClientXacts = site(GroundTestKey)(0).maxXacts,
      maxClientsPerPort = site(NBanksPerMemoryChannel),
      dataBeats = site(MIFDataBeats))
    case MIFTagBits => Dump("MIF_TAG_BITS", 2)
    case NBanksPerMemoryChannel => site(GroundTestKey)(0).uncached
    case _ => throw new CDEMatchError
  })

class WithDirectMemtest extends Config(
  (pname, site, here) => {
    val nGens = 8
    pname match {
      case GroundTestKey => Seq(GroundTestTileSettings(uncached = nGens))
      case GeneratorKey => GeneratorParameters(
        maxRequests = 1024,
        startAddress = 0)
      case BuildGroundTest =>
        (p: Parameters) => Module(new GeneratorTest()(p))
      case _ => throw new CDEMatchError
    }
  })

class WithDirectComparator extends Config(
  (pname, site, here) => pname match {
    case GroundTestKey => Seq.fill(site(NTiles)) {
      GroundTestTileSettings(uncached = site(ComparatorKey).targets.size)
    }
    case BuildGroundTest =>
      (p: Parameters) => Module(new ComparatorCore()(p))
    case ComparatorKey => ComparatorParameters(
      targets    = Seq(0L, 0x100L),
      width      = 8,
      operations = 1000,
      atomics    = site(UseAtomics),
      prefetches = site("COMPARATOR_PREFETCHES"))
    case UseFPU => false
    case UseAtomics => false
    case "COMPARATOR_PREFETCHES" => false
    case _ => throw new CDEMatchError
  })

class DirectGroundTestConfig extends Config(
  new WithDirectGroundTest ++ new GroundTestConfig)
class DirectMemtestConfig extends Config(
  new WithDirectMemtest ++ new DirectGroundTestConfig)
class DirectComparatorConfig extends Config(
  new WithDirectComparator ++ new DirectGroundTestConfig)

class DirectMemtestFPGAConfig extends Config(
  new FPGAConfig ++ new DirectMemtestConfig)
class DirectComparatorFPGAConfig extends Config(
  new FPGAConfig ++ new DirectComparatorConfig)
