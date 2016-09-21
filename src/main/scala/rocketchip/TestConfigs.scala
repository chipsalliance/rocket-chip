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
import scala.collection.immutable.HashMap
import cde.{Parameters, Config, Dump, Knob, CDEMatchError}
import scala.math.max
import coreplex._
import ConfigUtils._

class WithGroundTest extends Config(
  (pname, site, here) => pname match {
    case BuildCoreplex =>
      (p: Parameters, c: CoreplexConfig) => Module(new GroundTestCoreplex()(p, c))
    case TLKey("L1toL2") => {
      val useMEI = site(NTiles) <= 1 && site(NCachedTileLinkPorts) <= 1
      TileLinkParameters(
        coherencePolicy = (
          if (useMEI) new MEICoherence(site(L2DirectoryRepresentation))
          else new MESICoherence(site(L2DirectoryRepresentation))),
        nManagers = site(NBanksPerMemoryChannel)*site(NMemoryChannels) + 1,
        nCachingClients = site(NCachedTileLinkPorts),
        nCachelessClients = site(NCoreplexExtClients).get + site(NUncachedTileLinkPorts),
        maxClientXacts = ((site(DCacheKey).nMSHRs + 1) +:
                           site(GroundTestKey).map(_.maxXacts))
                             .reduce(max(_, _)),
        maxClientsPerPort = 1,
        maxManagerXacts = site(NAcquireTransactors) + 2,
        dataBeats = 8,
        dataBits = site(CacheBlockBytes)*8)
    }
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
          Module(new GroundTestTile(resetSignal = r)(p.alterPartial({
            case TLId => "L1toL2"
            case TileId => i
            case NCachedTileLinkPorts => if(tileSettings.cached > 0) 1 else 0
            case NUncachedTileLinkPorts => tileSettings.uncached
          })))
        }
      }
    }
    case BuildExampleTop =>
      (p: Parameters) => uncore.tilelink2.LazyModule(new ExampleTopWithTestRAM(p))
    case FPUKey => None
    case UseAtomics => false
    case UseCompressed => false
    case RegressionTestNames => LinkedHashSet("rv64ui-p-simple")
    case _ => throw new CDEMatchError
  })

class GroundTestConfig extends Config(new WithGroundTest ++ new BaseConfig)

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

class TraceGenConfig extends Config(
  new WithNCores(2) ++ new WithTraceGen ++ new GroundTestConfig)
class TraceGenBufferlessConfig extends Config(
  new WithBufferlessBroadcastHub ++ new TraceGenConfig)
class TraceGenL2Config extends Config(
  new WithNL2Ways(1) ++ new WithL2Capacity(32 * 64 / 1024) ++
  new WithL2Cache ++ new TraceGenConfig)

class MIF128BitComparatorConfig extends Config(
  new WithMIFDataBits(128) ++ new ComparatorConfig)
class MIF128BitMemtestConfig extends Config(
  new WithMIFDataBits(128) ++ new MemtestConfig)

class MIF32BitComparatorConfig extends Config(
  new WithMIFDataBits(32) ++ new ComparatorConfig)
class MIF32BitMemtestConfig extends Config(
  new WithMIFDataBits(32) ++ new MemtestConfig)

class PCIeMockupTestConfig extends Config(
  new WithPCIeMockupTest ++ new GroundTestConfig)
