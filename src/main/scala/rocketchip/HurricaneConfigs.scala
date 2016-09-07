package rocketchip

import Chisel._
import junctions._
import uncore.tilelink._
import uncore.coherence._
import uncore.agents._
import uncore.devices._
import uncore.converters._
import rocket._
import rocket.Util._
import groundtest._
import scala.math.max
import scala.collection.mutable.{LinkedHashSet, ListBuffer}
import coreplex._
import DefaultTestSuites._
import cde.{Parameters, Config, Dump, Knob, CDEMatchError, Field}
import hwacha._

case object NarrowIF extends Field[Bool]
case object NarrowWidth extends Field[Int]

class PMUConfig extends Config(
  topDefinitions = (pname,site,here) => pname match {
    case BuildTiles => {
        val env = if(site(UseVM)) List("p","v") else List("p")
        site(FPUKey) foreach { case cfg =>
          TestGeneration.addSuite(rv32udBenchmarks)
          TestGeneration.addSuites(env.map(rv64ufNoDiv))
          TestGeneration.addSuites(env.map(rv64udNoDiv))
          if (cfg.divSqrt) {
            TestGeneration.addSuites(env.map(rv64uf))
            TestGeneration.addSuites(env.map(rv64ud))
          }
        }
        if (site(UseAtomics)) TestGeneration.addSuites(env.map(if (site(XLen) == 64) rv64ua else rv32ua))
        if (site(UseCompressed)) TestGeneration.addSuites(env.map(if (site(XLen) == 64) rv64uc else rv32uc))
        val (rvi, rvu) =
          if (site(XLen) == 64) ((if (site(UseVM)) rv64i else rv64pi), rv64u)
          else ((if (site(UseVM)) rv32i else rv32pi), rv32u)
        TestGeneration.addSuites(rvi.map(_("p")))
        TestGeneration.addSuites((if(site(UseVM)) List("v") else List()).flatMap(env => rvu.map(_(env))))
        TestGeneration.addSuite(benchmarks)
        val tileList = List.tabulate(site(NTiles)-1){ i => (c: Clock, r: Bool, p: Parameters) =>
          Module(new RocketTile(clockSignal = c, resetSignal = r)(p.alterPartial({
            case TileId => i
            case TLId => "L1toL2"
            case NUncachedTileLinkPorts => 1 + site(RoccNMemChannels)
          })))
        }
        tileList :+ { (c: Clock, r: Bool, p: Parameters) =>
          Module(new RocketTile(clockSignal = c, resetSignal = r)(p.alterPartial({
            case TileId => site(NTiles)-1
            case TLId => "L1toL2"
            case NUncachedTileLinkPorts => 1
            case FPUKey => None
            case NTLBEntries => 4
            case BtbKey => BtbParameters(nEntries = 0)
            case DCacheKey => DCacheConfig(nSDQ = 2, nRPQ = 2, nMSHRs = 0)
            case MulDivKey => Some(MulDivConfig(mulUnroll = 1, mulEarlyOut = false, divEarlyOut = false))
            // [ben] This is a hack - we'd really prefer to set these per cache ID
            case NSets => 16
            case NWays => 1
            case BuildRoCC => Nil
          })))
        }
      }
    case _ => throw new CDEMatchError
  }
)

class NarrowIFConfig extends Config(
  topDefinitions = (pname,site,here) => pname match {
    case NarrowIF => true
    case NarrowWidth => Dump("NARROW_IF_WIDTH", 8)
    case NExtTopInterrupts => 1
    case _ => throw new CDEMatchError
  }
)

class NoJtagDTM extends Config (
  (pname, site, here) => pname match {
     case IncludeJtagDTM => false
  }
)

class DefaultNarrowConfig extends Config(new NarrowIFConfig ++ new DefaultConfig)

class HurricaneUpstreamConfig extends Config(new WithNCores(2) ++ new PMUConfig ++ new WithNLanes(2) ++ new WithNL2AcquireXacts(9) ++ new WithL2Capacity(512) ++ new WithNBanksPerMemChannel(1) ++ new WithNMemoryChannels(8) ++ new Process28nmConfig ++ new NarrowIFConfig ++ new WithJtagDTM ++ new HwachaConfig)

class HurricaneUpstreamTinyConfig extends Config(new WithoutConfPrec ++ new WithSmallPredRF ++ new WithNLanes(2)++ new WithNL2AcquireXacts(1) ++ new WithL2Capacity(64) ++ new HurricaneUpstreamConfig)

class HurricaneUpstreamConfigNoJtag extends Config(new NoJtagDTM ++ new HurricaneUpstreamConfig)
