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
import testchipip._
import rocketchip._
import util._
import dma._

case object NarrowWidth extends Field[Int]
case object HSCRFileSize extends Field[Int]
case object SlowIOMaxDivide extends Field[Int]

class NoJtagDTM extends Config (
  (pname, site, here) => pname match {
    case IncludeJtagDTM => false
    case _ => throw new CDEMatchError
  }
)

class WithHUpTop extends Config (
  (pname, site, here) => pname match {
    case NarrowWidth => Dump("NARROW_IF_WIDTH", 8)
    case SerialInterfaceWidth => 32
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
        maxClientsPerPort = 3,
        dataBeats = scrDataBeats)
    }
    case TLKey("PMU") =>
      site(TLKey("DefaultL1toL2")).copy(
        maxClientsPerPort = site(TLKey("L2toMMIO")).maxClientsPerPort * 2)
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
            Module(new Hwacha()(p.alterPartial({
            case FetchWidth => 1
            case CoreInstBits => 64
            })))
            },
          nMemChannels = site(HwachaNLanes),
          nPTWPorts = 2 + site(HwachaNLanes), // icache + vru + vmus
          useFPU = true),
        RoccParameters( // From dma/src/main/scala/Configs.scala
          opcodes = OpcodeSet.custom3,
          generator = (p: Parameters) => Module(new CopyAccelerator()(p)),
          nMemChannels = site(NDmaTrackers),
          nPTWPorts = 1))
    }
    case RoccMaxTaggedMemXacts => max(
      max(site(HwachaNVLTEntries), site(HwachaNSMUEntries)),
      3 * site(NDmaTrackerMemXacts))
    case _ => throw new CDEMatchError
  }
)

class DefaultHUpTopConfig extends Config(new WithTinyHbwif ++ new ExampleHbwifConfig ++ new WithHUpTop ++ new DefaultConfig)

class DefaultHUpTopL2Config extends Config(new WithTinyHbwif ++ new ExampleHbwifConfig ++ new WithHUpTop ++ new DefaultL2Config)

class WithHurricaneUpstreamSizingFullParams extends Config(
  (pname,site,here) => pname match {
    case NTiles => 1
    case NAcquireTransactors => 9
    case HwachaNLanes => 2
    case NMemoryChannels => Dump("N_MEM_CHANNELS", 8)
    case "HwI" => {
      case NSets => 64
      case NWays => 1
      case RowBits => 2 * site(CoreInstBits)
      case NTLBEntries => 8
      case CacheIdBits => 0
      case SplitMetadata => false
    }:PartialFunction[Any, Any]
    case _ => throw new CDEMatchError
  },
  knobValues = {
    case "L2_CAPACITY_IN_KB" => 512
    case "NBANKS_PER_MEM_CHANNEL" => 1
    case _ => throw new CDEMatchError
  })

class WithHurricaneUpstreamSizingTinyParams extends Config(
  (pname,site,here) => pname match {
    case NTiles => 1
    case NAcquireTransactors => 3
    case HwachaNLanes => 2
    case HwachaConfPrec => false
    case HwachaNPredRFEntries => 128
    case NMemoryChannels => Dump("N_MEM_CHANNELS", 8)
    case "HwI" => {
      case NSets => 64
      case NWays => 1
      case RowBits => 2 * site(CoreInstBits)
      case NTLBEntries => 8
      case CacheIdBits => 0
      case SplitMetadata => false
    }:PartialFunction[Any, Any]
    case _ => throw new CDEMatchError
  },
  knobValues = {
    case "L2_CAPACITY_IN_KB" => 64
    case "NBANKS_PER_MEM_CHANNEL" => 1
    case _ => throw new CDEMatchError
  })

class HurricaneUpstreamFeatureConfig extends Config (
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
  new WithHurricaneUpstreamSizingTinyParams ++
  new HurricaneUpstreamFeatureConfig
)

class HurricaneUpstreamConfigNoJtag extends Config(new NoJtagDTM ++ new HurricaneUpstreamConfig)

class WithMultiClockGroundTest extends Config(
  (pname, site, here) => pname match {
    case BuildCoreplex => (c: CoreplexConfig, p: Parameters) =>
      LazyModule(new MultiClockGroundTestCoreplex(c)(p)).module
    case _ => throw new CDEMatchError
  })

class WithTestRAM extends Config(
  (pname, site, here) => pname match {
    case BuildHTop => (p: Parameters) =>
      LazyModule(new HUpTopWithTestRAM(p))
    case _ => throw new CDEMatchError
  })

class WithHbwifGroundTest extends Config(
  new WithMultiClockGroundTest ++ new WithGroundTest)

class WithHbwifMemtest extends Config(
  new WithAtomics ++ new WithMemtest ++ new WithHbwifGroundTest)

class MemtestHbwifConfig extends Config(
  new WithHbwifMemtest ++ new DefaultHUpTopConfig)

class MemtestHbwifL2Config extends Config(
  new WithHbwifMemtest ++ new DefaultHUpTopL2Config)

class ComparatorHbwifL2Config extends Config(
  new WithAtomics ++ new WithComparator ++ new WithTestRAM ++
  new WithBufferDepth(4) ++
  new WithHbwifGroundTest ++ new DefaultHUpTopL2Config)

class TraceGenHbwifConfig extends Config(
  new WithNCores(2) ++ new WithTraceGen ++
  new WithHbwifGroundTest ++ new DefaultHUpTopL2Config)
