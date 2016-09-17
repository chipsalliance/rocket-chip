// See LICENSE for license details.

package rocketchip

import Chisel._
import cde.{Parameters, Field}
import junctions._
import uncore.tilelink._
import uncore.tilelink2._
import uncore.devices._
import util.ParameterizedBundle
import rocket._
import rocket.Util._
import coreplex._

// the following parameters will be refactored properly with TL2
case object GlobalAddrMap extends Field[AddrMap]
case object ConfigString extends Field[String]
case object NCoreplexExtClients extends Field[Int]
/** Function for building Coreplex */
case object BuildCoreplex extends Field[(Parameters, CoreplexConfig) => Coreplex]

/** Base Top with no Periphery */
abstract class BaseTop(q: Parameters) extends LazyModule {
  // the following variables will be refactored properly with TL2
  val pInterrupts = new RangeManager
  val pBusMasters = new RangeManager
  val pDevices = new ResourceManager[AddrMapEntry]

  // Add a peripheral bus
  val peripheryBus = LazyModule(new TLXbar)
  lazy val peripheryManagers = peripheryBus.node.edgesIn(0).manager.managers

  lazy val c = CoreplexConfig(
    nTiles = q(NTiles),
    nExtInterrupts = pInterrupts.sum,
    nSlaves = pBusMasters.sum,
    nMemChannels = q(NMemoryChannels),
    hasSupervisor = q(UseVM),
    hasExtMMIOPort = true
  )

  lazy val genGlobalAddrMap = GenerateGlobalAddrMap(q, pDevices.get, peripheryManagers)
  private val qWithMap = q.alterPartial({case GlobalAddrMap => genGlobalAddrMap})

  lazy val genConfigString = GenerateConfigString(qWithMap, c, pDevices.get, peripheryManagers)
  implicit val p = qWithMap.alterPartial({
    case ConfigString => genConfigString
    case NCoreplexExtClients => pBusMasters.sum})

  val legacy = LazyModule(new TLLegacy()(p.alterPartial({ case TLId => "L2toMMIO" })))

  peripheryBus.node := TLBuffer(TLWidthWidget(TLHintHandler(legacy.node), legacy.tlDataBytes))
}

class BaseTopBundle(val p: Parameters, val c: Coreplex) extends ParameterizedBundle()(p) {
  val success = c.hasSuccessFlag.option(Bool(OUTPUT))
}

class BaseTopModule[+L <: BaseTop, +B <: BaseTopBundle](val p: Parameters, l: L, b: Coreplex => B) extends LazyModuleImp(l) {
  val outer: L = l

  val coreplex = p(BuildCoreplex)(p, outer.c)
  val io: B = b(coreplex)

  io.success zip coreplex.io.success map { case (x, y) => x := y }

  val mmioNetwork =
    Module(new TileLinkRecursiveInterconnect(1, p(GlobalAddrMap).subMap("io:ext"))(
      p.alterPartial({ case TLId => "L2toMMIO" })))
  mmioNetwork.io.in.head <> coreplex.io.master.mmio.get
  outer.legacy.module.io.legacy <> mmioNetwork.port("TL2")

  println("Generated Address Map")
  for (entry <- p(GlobalAddrMap).flatten) {
    val name = entry.name
    val start = entry.region.start
    val end = entry.region.start + entry.region.size - 1
    println(f"\t$name%s $start%x - $end%x")
  }

  println("Generated Configuration String")
  println(p(ConfigString))
  ConfigStringOutput.contents = Some(p(ConfigString))
}

/** Example Top with Periphery */
class ExampleTop(q: Parameters) extends BaseTop(q)
    with PeripheryBootROM with PeripheryDebug with PeripheryExtInterrupts with PeripheryCoreplexLocalInterrupter
    with PeripheryMasterMem with PeripheryMasterMMIO with PeripherySlave {
  override lazy val module = Module(new ExampleTopModule(p, this, new ExampleTopBundle(p, _)))
}

class ExampleTopBundle(p: Parameters, c: Coreplex) extends BaseTopBundle(p, c)
    with PeripheryBootROMBundle with PeripheryDebugBundle with PeripheryExtInterruptsBundle with PeripheryCoreplexLocalInterrupterBundle
    with PeripheryMasterMemBundle with PeripheryMasterMMIOBundle with PeripherySlaveBundle

class ExampleTopModule[+L <: ExampleTop, +B <: ExampleTopBundle](p: Parameters, l: L, b: Coreplex => B) extends BaseTopModule(p, l, b)
    with PeripheryBootROMModule with PeripheryDebugModule with PeripheryExtInterruptsModule with PeripheryCoreplexLocalInterrupterModule
    with PeripheryMasterMemModule with PeripheryMasterMMIOModule with PeripherySlaveModule

/** Example Top with TestRAM */
class ExampleTopWithTestRAM(q: Parameters) extends ExampleTop(q)
    with PeripheryTestRAM {
  override lazy val module = Module(new ExampleTopWithTestRAMModule(p, this, new ExampleTopWithTestRAMBundle(p, _)))
}

class ExampleTopWithTestRAMBundle(p: Parameters, c: Coreplex) extends ExampleTopBundle(p, c)
    with PeripheryTestRAMBundle

class ExampleTopWithTestRAMModule[+L <: ExampleTopWithTestRAM, +B <: ExampleTopWithTestRAMBundle](p: Parameters, l: L, b: Coreplex => B) extends ExampleTopModule(p, l, b)
    with PeripheryTestRAMModule
