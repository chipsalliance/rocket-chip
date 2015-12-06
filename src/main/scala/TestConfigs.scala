package rocketchip

import Chisel._
import groundtest._
import rocket._
import uncore._
import cde.{Parameters, Config, Dump, Knob}
import scala.math.max

class WithGroundTest extends Config(
  (pname, site, here) => pname match {
    case TLKey("L1toL2") =>
      TileLinkParameters(
        coherencePolicy = new MESICoherence(site(L2DirectoryRepresentation)),
        nManagers = site(NBanksPerMemoryChannel)*site(NMemoryChannels),
        nCachingClients = site(NTiles),
        nCachelessClients = site(NTiles) + 1,
        maxClientXacts = max(site(NMSHRs) + site(NIOMSHRs),
                             site(GroundTestMaxXacts)),
        maxClientsPerPort = 1,
        maxManagerXacts = site(NAcquireTransactors) + 2,
        dataBits = site(CacheBlockBytes)*8)
    case BuildTiles => {
      (0 until site(NTiles)).map { i =>
        (r: Bool, p: Parameters) =>
          Module(new GroundTestTile(i, r)
            (p.alterPartial({case TLId => "L1toL2"})))
      }
    }
    case GroundTestMaxXacts => 1
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

class WithRegressionTest extends Config(
  (pname, site, here) => pname match {
    case BuildGroundTest =>
      (id: Int, p: Parameters) => Module(new RegressionTest()(p))
  })

class GroundTestConfig extends Config(new WithGroundTest ++ new DefaultConfig)
class MemtestConfig extends Config(new WithMemtest ++ new GroundTestConfig)
class MemtestL2Config extends Config(
  new WithMemtest ++ new WithL2Cache ++ new GroundTestConfig)
class CacheFillTestConfig extends Config(
  new WithCacheFillTest ++ new WithL2Cache ++ new GroundTestConfig)
class RegressionTestConfig extends Config(
  new WithRegressionTest ++ new GroundTestConfig)

class FancyMemtestConfig extends Config(
  new With2Cores ++ new With2MemoryChannels ++ new With2BanksPerMemChannel ++
  new WithMemtest ++ new WithL2Cache ++ new GroundTestConfig)
