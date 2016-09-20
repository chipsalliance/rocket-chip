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
  val success = Bool(OUTPUT)
}

abstract class BaseTopModule[+L <: BaseTop, +B <: BaseTopBundle](val p: Parameters, l: L, b: Coreplex => B) extends LazyModuleImp(l) {
  val outer: L = l

  val coreplex = p(BuildCoreplex)(p, outer.c)
  val io: B = b(coreplex)

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
    val prot = entry.region.attr.prot
    val protStr = (if ((prot & AddrMapProt.R) > 0) "R" else "") +
                  (if ((prot & AddrMapProt.W) > 0) "W" else "") +
                  (if ((prot & AddrMapProt.X) > 0) "X" else "")
    val cacheable = if (entry.region.attr.cacheable) " [C]" else ""
    println(f"\t$name%s $start%x - $end%x, $protStr$cacheable")
  }

  println("Generated Configuration String")
  println(p(ConfigString))
  ConfigStringOutput.contents = Some(p(ConfigString))

  io.success := coreplex.io.success
}
