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
        val (rvi, rvu) =
          if (site(XLen) == 64) ((if (site(UseVM)) rv64i else rv64pi), rv64u)
          else ((if (site(UseVM)) rv32i else rv32pi), rv32u)
        TestGeneration.addSuites(rvi.map(_("p")))
        TestGeneration.addSuites((if(site(UseVM)) List("v") else List()).flatMap(env => rvu.map(_(env))))
        TestGeneration.addSuite(if (site(UseVM)) benchmarks else emptyBmarks)
        val tileList = List.fill(site(NTiles)-1){ (c: Clock, r: Bool, p: Parameters) =>
          Module(new RocketTile(clockSignal = c, resetSignal = r)(p.alterPartial({
            case TLId => "L1toL2"
            case NUncachedTileLinkPorts => 1 + site(RoccNMemChannels)
          })))
        }
        tileList :+ { (c: Clock, r: Bool, p: Parameters) =>
          Module(new RocketTile(clockSignal = c, resetSignal = r)(p.alterPartial({
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
    case _ => throw new CDEMatchError
    }
  )

class DefaultNarrowConfig extends Config(new NarrowIFConfig ++ new DefaultConfig)

class HurricaneUpstreamConfig extends Config(new WithNCores(2) ++ new PMUConfig ++ new With2Lanes ++ new With9L2AcquireXacts ++ new WithL2Capacity(512) ++ new WithNBanksPerMemChannel(4) ++ new Process28nmConfig ++ new NarrowIFConfig ++ new HwachaConfig)

class HurricaneUpstreamTinyConfig extends Config(new WithoutConfPrec ++ new WithSmallPredRF ++ new With1Lane ++ new With2L2AcquireXacts ++ new WithL2Capacity(64) ++ new WithNBanksPerMemChannel(1) ++ new HurricaneUpstreamConfig)
