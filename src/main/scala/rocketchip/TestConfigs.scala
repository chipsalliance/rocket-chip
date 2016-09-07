package rocketchip

import Chisel._
import groundtest._
import rocket._
import uncore.tilelink._
import uncore.coherence._
import uncore.agents._
import uncore.devices.NTiles
import uncore.unittests._
import junctions._
import junctions.unittests._
import scala.collection.mutable.LinkedHashSet
import scala.collection.immutable.HashMap
import cde.{Parameters, Config, Dump, Knob, CDEMatchError}
import scala.math.max
import coreplex._
import ConfigUtils._

class WithUnitTest extends Config(
  (pname, site, here) => pname match {
    case BuildCoreplex => {
      val groundtest = if (site(XLen) == 64)
        DefaultTestSuites.groundtest64
      else
        DefaultTestSuites.groundtest32
      TestGeneration.addSuite(groundtest("p"))
      TestGeneration.addSuite(DefaultTestSuites.emptyBmarks)
      (c: Clock, r: Bool, p: Parameters) => Module(new UnitTestCoreplex(p))
    }
    case UnitTests => (testParams: Parameters) =>
      JunctionsUnitTests(testParams) ++ UncoreUnitTests(testParams)
    case NMemoryChannels => Dump("N_MEM_CHANNELS", 0)
    case FPUKey => None
    case UseAtomics => false
    case UseCompressed => false
    case RegressionTestNames => LinkedHashSet("rv64ui-p-simple")
    case _ => throw new CDEMatchError
  })

class UnitTestConfig extends Config(new WithUnitTest ++ new BaseConfig)

class WithGroundTest extends Config(
  (pname, site, here) => pname match {
    case BuildCoreplex => (c: Clock, r: Bool, p: Parameters) => Module(new GroundTestCoreplex()(p))
    case TLKey("L1toL2") => {
      val useMEI = site(NTiles) <= 1 && site(NCachedTileLinkPorts) <= 1
      TileLinkParameters(
        coherencePolicy = (
          if (useMEI) new MEICoherence(site(L2DirectoryRepresentation))
          else new MESICoherence(site(L2DirectoryRepresentation))),
        nManagers = site(NBanksPerMemoryChannel)*site(NMemoryChannels) + 1,
        nCachingClients = site(NCachedTileLinkPorts),
        nCachelessClients = site(NExternalClients) + site(NUncachedTileLinkPorts),
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
    case FPUKey => None
    case UseAtomics => false
    case UseCompressed => false
    case RegressionTestNames => LinkedHashSet("rv64ui-p-simple")
    case _ => throw new CDEMatchError
  })

class GroundTestConfig extends Config(new WithGroundTest ++ new BaseConfig)

class ComparatorConfig extends Config(
  new WithTestRAM ++ new WithComparator ++ new GroundTestConfig)
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

class WithDirectGroundTest extends Config(
  (pname, site, here) => pname match {
    case ExportGroundTestStatus => true
    case BuildCoreplex => (c: Clock, r: Bool, p: Parameters) => Module(new DirectGroundTestCoreplex(p))
    case ExtraCoreplexPorts => (p: Parameters) =>
      if (p(ExportGroundTestStatus)) new GroundTestStatus else new Bundle
    case ExtraTopPorts => (p: Parameters) =>
      if (p(ExportGroundTestStatus)) new GroundTestStatus else new Bundle
    case TLKey("Outermost") => site(TLKey("L2toMC")).copy(
      maxClientXacts = site(GroundTestKey)(0).maxXacts,
      maxClientsPerPort = site(NBanksPerMemoryChannel),
      dataBeats = site(MIFDataBeats))
    case NBanksPerMemoryChannel => site(GroundTestKey)(0).uncached
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

class WithBusMasterTest extends Config(
  (pname, site, here) => pname match {
    case GroundTestKey => Seq.fill(site(NTiles)) {
      GroundTestTileSettings(uncached = 1)
    }
    case BuildGroundTest =>
      (p: Parameters) => Module(new BusMasterTest()(p))
    case ExtraDevices => {
      class BusMasterDevice extends DeviceBlock {
        def nClientPorts = 1
        def addrMapEntries = Seq(
          AddrMapEntry("busmaster", MemSize(4096, MemAttr(AddrMapProt.RW))))
        def builder(
          mmioPorts: HashMap[String, ClientUncachedTileLinkIO],
          clientPorts: Seq[ClientUncachedTileLinkIO],
          interrupts : Seq[Bool], 
          extra: Bundle, p: Parameters) {
          val busmaster = Module(new ExampleBusMaster()(p))
          busmaster.io.mmio <> mmioPorts("busmaster")
          clientPorts.head <> busmaster.io.mem
        }
      }
      new BusMasterDevice
    }
    case _ => throw new CDEMatchError
  })

class BusMasterTestConfig extends Config(new WithBusMasterTest ++ new GroundTestConfig)
