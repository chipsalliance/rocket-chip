// See LICENSE.SiFive for license details.

package coreplex

import Chisel._
import config._
import junctions._
import diplomacy._
import tile._
import uncore.tilelink2._
import uncore.devices._
import util._

trait CoreplexRISCVPlatform extends CoreplexNetwork {
  val module: CoreplexRISCVPlatformModule

  val debug = LazyModule(new TLDebugModule())
  val plic  = LazyModule(new TLPLIC(hasSupervisor, maxPriorities = 7))
  val clint = LazyModule(new CoreplexLocalInterrupter)

  debug.node := TLFragmenter(cbus_beatBytes, cbus_lineBytes)(cbus.node)
  plic.node  := TLFragmenter(cbus_beatBytes, cbus_lineBytes)(cbus.node)
  clint.node := TLFragmenter(cbus_beatBytes, cbus_lineBytes)(cbus.node)

  plic.intnode := intBar.intnode

  lazy val configString = {
    val managers = l1tol2.node.edgesIn(0).manager.managers
    rocketchip.GenerateConfigString(p, clint, plic, managers)
  }

  lazy val dts = DTS(bindingTree)
}

trait CoreplexRISCVPlatformBundle extends CoreplexNetworkBundle {
  val outer: CoreplexRISCVPlatform

  val debug = new AsyncDebugBusIO().flip
  val rtcToggle = Bool(INPUT)
  val resetVector = UInt(INPUT, p(XLen))
}

trait CoreplexRISCVPlatformModule extends CoreplexNetworkModule {
  val outer: CoreplexRISCVPlatform
  val io: CoreplexRISCVPlatformBundle

  // Synchronize the debug bus into the coreplex
  outer.debug.module.io.db <> FromAsyncDebugBus(io.debug)

  // Synchronize the rtc into the coreplex
  val rtcSync = ShiftRegister(io.rtcToggle, 3)
  val rtcLast = Reg(init = Bool(false), next=rtcSync)
  outer.clint.module.io.rtcTick := Reg(init = Bool(false), next=(rtcSync & (~rtcLast)))

  println(s"\nGenerated Configuration String\n${outer.configString}")
  ElaborationArtefacts.add("cfg", outer.configString)

  println(outer.dts)
  ElaborationArtefacts.add("dts", outer.dts)
}
