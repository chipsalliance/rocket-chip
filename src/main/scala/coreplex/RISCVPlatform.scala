package coreplex

import Chisel._
import config._
import junctions._
import diplomacy._
import uncore.tilelink._
import uncore.tilelink2._
import uncore.coherence._
import uncore.agents._
import uncore.devices._
import uncore.util._
import uncore.converters._
import rocket._
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

  {
    val managers = outer.l1tol2.node.edgesIn(0).manager.managers

    // Allow something else to have override the config string
    if (!ConfigStringOutput.contents.isDefined) {
      ConfigStringOutput.contents = Some(rocketchip.GenerateConfigString(p, outer.clint, outer.plic, managers))
    }

    println(s"\nGenerated Configuration String\n${ConfigStringOutput.contents.get}")
  }
}
