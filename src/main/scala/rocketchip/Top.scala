// See LICENSE for license details.

package rocketchip

import Chisel._
import cde.{Parameters, Field}
import junctions._
import uncore.tilelink._
import uncore.tilelink2.{LazyModule, LazyModuleImp}
import uncore.devices._
import util.ParameterizedBundle
import rocket._
import rocket.Util._
import coreplex._

// the following parameters will be refactored properly with TL2
case object GlobalAddrMap extends Field[GlobalVariable[AddrMap]]
case object ConfigString extends Field[GlobalVariable[String]]
case object NCoreplexExtClients extends Field[GlobalVariable[Int]]
/** Function for building Coreplex */
case object BuildCoreplex extends Field[(Parameters, CoreplexConfig) => Coreplex]

/** Base Top with no Periphery */
abstract class BaseTop(val p: Parameters) extends LazyModule {
  // the following variables will be refactored properly with TL2
  val pInterrupts = new RangeManager
  val pBusMasters = new RangeManager
  val pDevices = new ResourceManager[AddrMapEntry]
}

class BaseTopBundle(val p: Parameters, val c: Coreplex) extends ParameterizedBundle()(p) {
  val success = c.hasSuccessFlag.option(Bool(OUTPUT))
}

class BaseTopModule[+L <: BaseTop, +B <: BaseTopBundle](val p: Parameters, l: L, b: Coreplex => B) extends LazyModuleImp(l) {
  val outer: L = l

  val c = CoreplexConfig(
    nTiles = p(NTiles),
    nExtInterrupts = outer.pInterrupts.sum,
    nSlaves = outer.pBusMasters.sum,
    nMemChannels = p(NMemoryChannels),
    hasSupervisor = p(UseVM),
    hasExtMMIOPort = !(outer.pDevices.get.isEmpty && p(ExtMMIOPorts).isEmpty)
  )

  def genGlobalAddrMap = GenerateGlobalAddrMap(p, outer.pDevices.get)
  def genConfigString = GenerateConfigString(p, c, outer.pDevices.get)

  p(NCoreplexExtClients).assign(outer.pBusMasters.sum)
  p(GlobalAddrMap).assign(genGlobalAddrMap)
  p(ConfigString).assign(genConfigString)

  println("Generated Address Map")
  for (entry <- p(GlobalAddrMap).get.flatten) {
    val name = entry.name
    val start = entry.region.start
    val end = entry.region.start + entry.region.size - 1
    println(f"\t$name%s $start%x - $end%x")
  }

  println("Generated Configuration String")
  println(p(ConfigString).get)

  val coreplex = p(BuildCoreplex)(p, c)
  val io: B = b(coreplex)

  io.success zip coreplex.io.success map { case (x, y) => x := y }

  val mmioNetwork = c.hasExtMMIOPort.option(
    Module(new TileLinkRecursiveInterconnect(1, p(GlobalAddrMap).get.subMap("io:ext"))(
      p.alterPartial({ case TLId => "L2toMMIO" }))))
  mmioNetwork.foreach { _.io.in.head <> coreplex.io.master.mmio.get }
}

/** Example Top with Periphery */
class ExampleTop(p: Parameters) extends BaseTop(p)
    with PeripheryDebug with PeripheryExtInterrupts with PeripheryAON
    with PeripheryMasterMem with PeripheryMasterMMIO with PeripherySlave {
  override lazy val module = Module(new ExampleTopModule(p, this, new ExampleTopBundle(p, _)))
}

class ExampleTopBundle(p: Parameters, c: Coreplex) extends BaseTopBundle(p, c)
    with PeripheryDebugBundle with PeripheryExtInterruptsBundle with PeripheryAONBundle
    with PeripheryMasterMemBundle with PeripheryMasterMMIOBundle with PeripherySlaveBundle

class ExampleTopModule[+L <: ExampleTop, +B <: ExampleTopBundle](p: Parameters, l: L, b: Coreplex => B) extends BaseTopModule(p, l, b)
    with PeripheryDebugModule with PeripheryExtInterruptsModule with PeripheryAONModule
    with PeripheryMasterMemModule with PeripheryMasterMMIOModule with PeripherySlaveModule

/** Example Top with TestRAM */
class ExampleTopWithTestRAM(p: Parameters) extends ExampleTop(p)
    with PeripheryTestRAM {
  override lazy val module = Module(new ExampleTopWithTestRAMModule(p, this, new ExampleTopWithTestRAMBundle(p, _)))
}

class ExampleTopWithTestRAMBundle(p: Parameters, c: Coreplex) extends ExampleTopBundle(p, c)
    with PeripheryTestRAMBundle

class ExampleTopWithTestRAMModule[+L <: ExampleTopWithTestRAM, +B <: ExampleTopWithTestRAMBundle](p: Parameters, l: L, b: Coreplex => B) extends ExampleTopModule(p, l, b)
    with PeripheryTestRAMModule
