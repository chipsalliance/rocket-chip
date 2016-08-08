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
import DefaultTestSuites._
import cde.{Parameters, Config, Dump, Knob, CDEMatchError}
import hwacha._


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
            case UseFPU => false
            case MulUnroll => 1
            case DivEarlyOut => false  
            case NTLBEntries => 4
            case BtbKey => BtbParameters(nEntries = 0)
            case StoreDataQueueDepth => 2
            case ReplayQueueDepth => 2
            case NAcquireTransactors => 2
            // TODO - are these actually getting set to make the core tiny?
            case "L1I" => {
              case NSets => 64
              case NWays => 1
            }:PartialFunction[Any, Any]
            case "L1D" => {
              case NSets => 64
              case NWays => 1
            }:PartialFunction[Any, Any]
            case NMSHRs => 0
            case BuildRoCC => Nil
            // TODO - Anything else we should do to make the core smaller?
          })))
        }
      }
    case _ => throw new CDEMatchError
    }
  )

class HurricaneUpstreamConfig extends Config(new WithNCores(2) ++ new PMUConfig ++ new With2Lanes ++ new With9L2AcquireXacts ++ new WithL2Capacity(512) ++ new WithNBanksPerMemChannel(4) ++ new Process28nmConfig ++ new HwachaConfig)

class HurricaneUpstreamTinyConfig extends Config(new WithNCores(2) ++ new PMUConfig ++ new WithL2Capacity(64) ++ new Process28nmConfig ++ new HwachaConfig ++ new WithSmallCores)

