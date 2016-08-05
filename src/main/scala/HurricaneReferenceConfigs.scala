package rocketchip

import Chisel._
import cde._
import cde.Implicits._
import DefaultTestSuites._
import scala.collection.mutable.LinkedHashSet
import uncore._
import rocket._
import hwacha._

class PMUConfig extends Config(
  (pname, site, here) => pname match {
    case BuildTiles => {
        val (rvi, rvu) =
          if (site(XLen) == 64) (rv64i, rv64u)
          else (rv32i, rv32u)
        TestGeneration.addSuites(rvi.map(_("p")))
        TestGeneration.addSuites((if(site(UseVM)) List("v") else List()).flatMap(env => rvu.map(_(env))))
        TestGeneration.addSuite(bmarks)
        val tileList = List.fill(site(NTiles) - 1){ (c: Clock, r: Bool, p: Parameters) =>
          Module(new RocketTile(clockSignal = c, resetSignal = r)(p.alterPartial({case TLId => "L1toL2"})))
        }
        tileList :+ { (c: Clock, r: Bool, p: Parameters) =>
          Module(new RocketTile(clockSignal = c, resetSignal = r)(p.alterPartial({
            case TLId => "L1toL2"
            case UseFPU => false
            case rocket.FastMulDiv => false
            case NTLBEntries => 4
            case BtbKey => BtbParameters(nEntries = 0)
            case StoreDataQueueDepth => 2
            case ReplayQueueDepth => 2
            case NAcquireTransactors => 2
            case "L1I" => {
              case NSets => 64
              case NWays => 1
            }:PartialFunction[Any, Any]
            case "L1D" => {
              case NSets => 64
              case NWays => 1
            }:PartialFunction[Any, Any]
            case NMSHRs => 1
            case BuildRoCC => Nil
          }))
          )
        }
      }
    case _ => throw new CDEMatchError
    }
  )

class Hurricane2ReferenceConfig extends Config(new With2Lanes ++ new With9L2AcquireXacts ++ new WithL2Capacity512 ++ new With4BanksPerMemChannel ++ new Process28nmConfig ++ new HwachaVLSIConfig)

class Hurricane2TinyConfig extends Config(new With2Cores ++ new PMUConfig ++ new With5L2AcquireXacts ++ new WithL2Capacity64 ++ new Process28nmConfig ++ new HwachaVLSIConfig)

class Hurricane2TinyDefaultConfig extends Config(new With2Cores ++ new PMUConfig ++ new With5L2AcquireXacts ++ new WithL2Capacity64 ++ new DefaultVLSIConfig)
