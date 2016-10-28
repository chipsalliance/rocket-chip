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

  // Fill in the TL1 legacy parameters
  implicit val p = q.alterPartial {
    case NCoreplexExtClients => pBusMasters.sum
    case NExtInterrupts => pInterrupts.sum
    case GlobalAddrMap => legacyAddrMap
  }

  val coreplex : C = LazyModule(buildCoreplex(p))

  // Create the address map for legacy masters
  lazy val legacyAddrMap = GenerateGlobalAddrMap(q, coreplex.l1tol2.node.edgesIn(0).manager.managers)

  peripheryBus.node :=
    TLWidthWidget(p(SOCBusKey).beatBytes)(
    TLAtomicAutomata(arithmetic = p(PeripheryBusKey).arithAMO)(
    socBus.node))

  TopModule.contents = Some(this)
}

abstract class BaseTopBundle[+L <: BaseTop[BaseCoreplex]](val outer: L) extends Bundle {
  implicit val p = outer.p
  val success = Bool(OUTPUT)
}

abstract class BaseTopModule[+B <: BaseTopBundle[BaseTop[BaseCoreplex]]](val io: B) extends LazyModuleImp(io.outer) {
  val outer = io.outer.asInstanceOf[io.outer.type]
  implicit val p = outer.p

  val coreplexMem        : Vec[ClientUncachedTileLinkIO] = Wire(outer.coreplex.module.io.mem)
  val coreplexSlave      : Vec[ClientUncachedTileLinkIO] = Wire(outer.coreplex.module.io.slave)
  val coreplexDebug      : DebugBusIO                    = Wire(outer.coreplex.module.io.debug)
  val coreplexInterrupts : Vec[Bool]                     = Wire(outer.coreplex.module.io.interrupts)

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

  io.success := outer.coreplex.module.io.success
}

trait DirectConnection {
  val coreplex: BaseCoreplex
  val socBus: TLXbar

  socBus.node := coreplex.mmio
}

trait DirectConnectionModule {
  val outer: BaseTop[BaseCoreplex]

  val coreplexMem        : Vec[ClientUncachedTileLinkIO]
  val coreplexSlave      : Vec[ClientUncachedTileLinkIO]
  val coreplexDebug      : DebugBusIO
  val coreplexInterrupts : Vec[Bool]

  coreplexMem        <> outer.coreplex.module.io.mem
  coreplexInterrupts <> outer.coreplex.module.io.interrupts
  outer.coreplex.module.io.slave <> coreplexSlave
  outer.coreplex.module.io.debug <> coreplexDebug
}
