// See LICENSE for license details.

package rocketchip

import Chisel._
import cde.{Parameters, Field}
import junctions._
import diplomacy._
import uncore.tilelink._
import uncore.tilelink2._
import uncore.devices._
import util._
import rocket._
import coreplex._

// the following parameters will be refactored properly with TL2
case object GlobalAddrMap extends Field[AddrMap]
case object ConfigString extends Field[String]
case object NCoreplexExtClients extends Field[Int]
case object NExtInterrupts extends Field[Int]
/** Enable or disable monitoring of Diplomatic buses */
case object TLEmitMonitors extends Field[Bool]

/** Base Top with no Periphery */
abstract class BaseTop[+C <: BaseCoreplex](buildCoreplex: Parameters => C)(implicit q: Parameters) extends LazyModule {
  // the following variables will be refactored properly with TL2
  val pInterrupts = new RangeManager
  val pBusMasters = new RangeManager

  TLImp.emitMonitors = q(TLEmitMonitors)

  // Add a SoC and peripheral bus
  val socBus = LazyModule(new TLXbar)
  val peripheryBus = LazyModule(new TLXbar)
  lazy val peripheryManagers = socBus.node.edgesIn(0).manager.managers

  // Fill in the TL1 legacy parameters
  val qWithSums = q.alterPartial {
    case NCoreplexExtClients => pBusMasters.sum
    case NExtInterrupts => pInterrupts.sum
  }
  val qWithMap = qWithSums.alterPartial {
     case GlobalAddrMap => GenerateGlobalAddrMap(qWithSums, peripheryManagers)
  }
  implicit val p = qWithMap.alterPartial {
    case ConfigString => GenerateConfigString(qWithMap, peripheryManagers)
  }

  val coreplex = LazyModule(buildCoreplex(p))

  peripheryBus.node :=
    TLBuffer()(
    TLAtomicAutomata(arithmetic = p(PeripheryBusKey).arithAMO)(
    TLWidthWidget(p(SOCBusKey).beatBytes)(
    socBus.node)))

  socBus.node := coreplex.mmio

  TopModule.contents = Some(this)
}

abstract class BaseTopBundle[+L <: BaseTop[BaseCoreplex]](val outer: L) extends Bundle {
  implicit val p = outer.p
  val success = Bool(OUTPUT)
}

abstract class BaseTopModule[+L <: BaseTop[BaseCoreplex], +B <: BaseTopBundle[L]](val outer: L, val io: B) extends LazyModuleImp(outer) {
  implicit val p = outer.p
  val coreplexIO = Wire(outer.coreplex.module.io)

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

  println("\nGenerated Interrupt Vector")
  outer.pInterrupts.print

  println("\nGenerated Configuration String")
  println(p(ConfigString))
  ConfigStringOutput.contents = Some(p(ConfigString))

  io.success := coreplexIO.success
}

trait DirectConnection {
  val coreplexIO: BaseCoreplexBundle[BaseCoreplex]
  val outer: BaseTop[BaseCoreplex]

  coreplexIO <> outer.coreplex.module.io
}
