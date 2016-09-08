// See LICENSE for license details.

package rocketchip

import Chisel._
import cde.{Parameters}
import junctions._
import uncore.tilelink._
import uncore.tilelink2.{LazyModule, LazyModuleImp}
import rocket.Util._
import coreplex._


/** Base Top with no Periphery */

abstract class BaseTop(val p: Parameters) extends LazyModule

class BaseTopBundle(val p: Parameters, val c: Coreplex) extends ParameterizedBundle()(p) {
  val success = c.hasSuccessFlag.option(Bool(OUTPUT))
}

class BaseTopModule[L <: BaseTop, B <: BaseTopBundle](val p: Parameters, l: L, b: Coreplex => B) extends LazyModuleImp(l) {
  val coreplex = p(BuildCoreplex)(p)
  val outer: L = l
  val io: B = b(coreplex)

  io.success zip coreplex.io.success map { case (x, y) => x := y }
  coreplex.io.rtcTick := Counter(p(RTCPeriod)).inc()

  val mmioNetwork = p(ExportMMIOPort).option(
    Module(new TileLinkRecursiveInterconnect(1, p(GlobalAddrMap).subMap("io:ext"))(
      p.alterPartial({ case TLId => "L2toMMIO" }))))
  mmioNetwork.foreach { _.io.in.head <> coreplex.io.mmio.get }
}


/** Example Top with Periphery */

class ExampleTop(p: Parameters) extends BaseTop(p)
    with PeripheryDebug with PeripheryInterrupt
    with PeripheryMasterMem with PeripheryMasterMMIO with PeripherySlave {
  lazy val module = Module(new ExampleTopModule(p, this, new ExampleTopBundle(p, _)))
}

class ExampleTopBundle(p: Parameters, c: Coreplex) extends BaseTopBundle(p, c)
    with PeripheryDebugBundle with PeripheryInterruptBundle
    with PeripheryMasterMemBundle with PeripheryMasterMMIOBundle with PeripherySlaveBundle

class ExampleTopModule[L <: ExampleTop, B <: ExampleTopBundle](p: Parameters, l: L, b: Coreplex => B) extends BaseTopModule(p, l, b)
    with PeripheryDebugModule with PeripheryInterruptModule
    with PeripheryMasterMemModule with PeripheryMasterMMIOModule with PeripherySlaveModule
