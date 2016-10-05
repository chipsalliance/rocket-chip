package hurricane

import Chisel._
import diplomacy.LazyModule
import junctions._
import uncore.tilelink._
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
import util._
import dma._

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
    case DecoupledRoCC => true
    case RoCCQueueDepth => 2
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

class WithEightLaneHbwif extends Config (
  (pname, site, here) => pname match {
    case HbwifKey => HbwifParameters(numLanes = 8)
    case _ => throw new CDEMatchError
  }
)

class WithHwachaAndDma extends Config (
  (pname, site, here) => pname match {
    case BuildRoCC => {
      import HwachaTestSuites._
      TestGeneration.addSuites(rv64uv.map(_("p")))
      TestGeneration.addSuites(rv64uv.map(_("vp")))
      // no excep or vm in v4 yet
      //TestGeneration.addSuites((if(site(UseVM)) List("pt","v") else List("pt")).flatMap(env => rv64uv.map(_(env))))
      TestGeneration.addSuite(rv64sv("p"))
      TestGeneration.addSuite(hwachaBmarks)
      TestGeneration.addVariable("SRC_EXTENSION", "$(base_dir)/hwacha/$(src_path)/*.scala")
      TestGeneration.addVariable("DISASM_EXTENSION", "--extension=hwacha")
      Seq(
        RoccParameters( // From hwacha/src/main/scala/configs.scala
        opcodes = OpcodeSet.custom0 | OpcodeSet.custom1,
        generator = (p: Parameters) => {
          val h = Module(new Hwacha()(p.alterPartial({
          case FetchWidth => 1
          case CoreInstBits => 64
          })))
          if(p(DecoupledRoCC)) {
            val decoupler = Module(new RoccBusyDecoupler(
            Seq(HwachaInstructions.VF, HwachaInstructions.VFT), 10)(p))
            AsyncQueueify(decoupler.clock, decoupler.reset,
              (h.io.elements - "utl").values,
              (decoupler.io.roccOut.elements - "utl").values,
              h.clock, h.reset,
              p(RoCCQueueDepth), 2)
            // UTL port is crossed in the coreplex for now
            decoupler.io.roccOut.utl <> h.io.utl

            val twoPhaseHwacha = Wire(Bool())
            twoPhaseHwacha := LevelSyncTo(h.clock, decoupler.io.twoPhase, 2)
            // Hwacha takes a cycle of decode to become busy
            // so we add another sync reg to account for this
            decoupler.io.delayTwoPhase := LevelSyncFrom(h.clock, twoPhaseHwacha, 2+1)
            decoupler
          } else h
          },
        nMemChannels = site(HwachaNLanes),
        nPTWPorts = 2 + site(HwachaNLanes), // icache + vru + vmus
        useFPU = true),
      RoccParameters( // From dma/src/main/scala/Configs.scala
        opcodes = OpcodeSet.custom3,
        generator = (p: Parameters) => Module(new CopyAccelerator()(p)),
        nMemChannels = (if (site(CopyAccelShareMemChannel)) 0 else 1),
        nPTWPorts = 1)
      )
    }
    case _ => throw new CDEMatchError
  }
)

class DefaultHUpTopConfig extends Config(new WithTinyHbwif ++ new ExampleHbwifConfig ++ new WithHUpTop ++ new DefaultConfig)

class WithHurricaneUpstreamSizingFullParams extends Config(
  (pname,site,here) => pname match {
    case NTiles => 2
    case NAcquireTransactors => 9
    case HwachaNLanes => 2
    case NMemoryChannels => Dump("N_MEM_CHANNELS", 8)
    case _ => throw new CDEMatchError
  },
  knobValues = {
    case "L2_CAPACITY_IN_KB" => 512
    case "NBANKS_PER_MEM_CHANNEL" => 1
    case _ => throw new CDEMatchError
  })

class WithHurricaneUpstreamSizingTinyParams extends Config(
  (pname,site,here) => pname match {
    case NTiles => 2
    case NAcquireTransactors => 3
    case HwachaNLanes => 2
    case HwachaConfPrec => false
    case HwachaNPredRFEntries => 128
    case NMemoryChannels => Dump("N_MEM_CHANNELS", 8)
    case _ => throw new CDEMatchError
  },
  knobValues = {
    case "L2_CAPACITY_IN_KB" => 64
    case "NBANKS_PER_MEM_CHANNEL" => 1
    case _ => throw new CDEMatchError
  })

class HurricaneUpstreamFeatureConfig extends Config (
  new PMUConfig ++
  new Process28nmConfig ++
  new ExampleHbwifConfig ++
  new WithHUpTop ++
  new WithJtagDTM ++
  new WithHwachaAndDma ++
  new WithDma ++
  new HwachaConfig
)
class HurricaneUpstreamConfig extends Config (
  new WithHurricaneUpstreamSizingFullParams ++
  new HurricaneUpstreamFeatureConfig
)

class HurricaneUpstreamTinyConfig extends Config (
  new WithHurricaneUpstreamSizingFullParams ++
  new HurricaneUpstreamFeatureConfig
)

class HurricaneUpstreamConfigNoJtag extends Config(new NoJtagDTM ++ new HurricaneUpstreamConfig)
