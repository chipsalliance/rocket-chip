package hurricane

import Chisel._
import junctions._
import uncore.tilelink._
import uncore.tilelink2._
import uncore.coherence._
import uncore.agents._
import uncore.devices._
import uncore.converters._
import rocket._
import groundtest._
import scala.math.max
import scala.collection.mutable.{LinkedHashSet, ListBuffer}
import coreplex._
import rocketchip.DefaultTestSuites._
import cde.{Parameters, Config, Dump, Knob, CDEMatchError, Field}
import hwacha._
import hbwif._
import rocketchip._

case object NarrowWidth extends Field[Int]
case object HSCRFileSize extends Field[Int]
case object SlowIOMaxDivide extends Field[Int]

class PMUConfig extends Config(
  topDefinitions = (pname,site,here) => pname match {
    case BuildTiles => {
        val tileList = List.tabulate(site(NTiles)-1){ i => (r: Bool, p: Parameters) =>
          Module(new RocketTile(resetSignal = r)(p.alterPartial({
            case TileId => i
            case TLId => "L1toL2"
            case NUncachedTileLinkPorts => 1 + site(RoccNMemChannels)
          })))
        }
        tileList :+ { (r: Bool, p: Parameters) =>
          Module(new RocketTile(resetSignal = r)(p.alterPartial({
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

class NoJtagDTM extends Config (
  (pname, site, here) => pname match {
    case IncludeJtagDTM => false
    case _ => throw new CDEMatchError
  }
)

class WithHUpTop extends Config (
  (pname, site, here) => pname match {
    case NarrowWidth => Dump("NARROW_IF_WIDTH", 8)
    case SlowIOMaxDivide => 1024
    case HSCRFileSize => 1 << 12//rocket.HasCoreParameters.pgIdxBits
    case BuildCoreplex => (c: CoreplexConfig, p: Parameters) =>
      LazyModule(new MultiClockCoreplex(c)(p)).module
    case BuildHTop => (p: Parameters) =>
      LazyModule(new HUpTop(p))
    // Need to pick the key with the larger client_xact_id
    case TLKey("LBWIF") => {
      val memKey = site(TLKey("Switcher"))
      val mmioKey = site(TLKey("MMIOtoSCR")).copy(dataBeats = memKey.dataBeats)
      val memIdSize = memKey.maxClientXacts * memKey.maxClientsPerPort
      val mmioIdSize = mmioKey.maxClientXacts * mmioKey.maxClientsPerPort
      if (memIdSize > mmioIdSize) memKey else mmioKey
    }
    case TLKey("Switcher") =>
      site(TLKey("L2toMC")).copy(
        maxClientXacts = site(NAcquireTransactors) + 2,
        maxClientsPerPort = site(NBanksPerMemoryChannel) * site(NMemoryChannels))
    case TLKey("MMIOtoSCR") => {
      val scrDataBits = 64
      val scrDataBeats = (8 * site(CacheBlockBytes)) / scrDataBits
      site(TLKey("L2toMMIO")).copy(
        maxClientsPerPort = 2,
        dataBeats = scrDataBeats)
    }
    case _ => throw new CDEMatchError
  }
)

class WithTinyHbwif extends Config (
  (pname, site, here) => pname match {
    case HbwifKey => HbwifParameters(numLanes = site(NMemoryChannels))
    case _ => throw new CDEMatchError
  }
)

class DefaultHUpTopConfig extends Config(new WithTinyHbwif ++ new ExampleHbwifConfig ++ new WithHUpTop ++ new DefaultConfig)

class HurricaneUpstreamConfig extends Config (
  new WithNCores(2) ++
  new PMUConfig ++
  new WithNLanes(2) ++
  new WithNL2AcquireXacts(9) ++
  new WithL2Capacity(512) ++
  new WithNBanksPerMemChannel(1) ++
  new WithNMemoryChannels(8) ++
  new Process28nmConfig ++
  new ExampleHbwifConfig ++
  new WithHUpTop ++
  new WithJtagDTM ++
  new HwachaConfig
)

class HurricaneUpstreamTinyConfig extends Config (
  new WithoutConfPrec ++
  new WithSmallPredRF ++
  new WithNLanes(2) ++
  new WithNL2AcquireXacts(3) ++
  new WithL2Capacity(64) ++
  new HurricaneUpstreamConfig
)

class HurricaneUpstreamConfigNoJtag extends Config(new NoJtagDTM ++ new HurricaneUpstreamConfig)
