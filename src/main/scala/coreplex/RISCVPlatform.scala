// See LICENSE.SiFive for license details.

package coreplex

import Chisel._
import config.Field
import diplomacy._
import tile._
import uncore.tilelink2._
import uncore.devices._
import util._

/** Number of tiles */
case object NTiles extends Field[Int]
case object PLICKey extends Field[PLICParams]
case object ClintKey extends Field[ClintParams]

trait CoreplexRISCVPlatform extends CoreplexNetwork {
  val module: CoreplexRISCVPlatformModule

  val debug = LazyModule(new TLDebugModule())
  debug.node := TLFragmenter(pbusBeatBytes, pbusBlockBytes)(pbus.node)

  val plic  = LazyModule(new TLPLIC(p(PLICKey)))
  plic.node  := TLFragmenter(pbusBeatBytes, pbusBlockBytes)(pbus.node)
  plic.intnode := int_xbar.intnode

  val clint = LazyModule(new CoreplexLocalInterrupter(nTiles, p(ClintKey)))
  clint.node := TLFragmenter(pbusBeatBytes, pbusBlockBytes)(pbus.node)

  lazy val dts = DTS(bindingTree)
  lazy val dtb = DTB(dts)
  lazy val json = JSON(bindingTree)
}

trait CoreplexRISCVPlatformBundle extends CoreplexNetworkBundle {
  val outer: CoreplexRISCVPlatform

  val debug = new ClockedDMIIO().flip
  val rtcToggle = Bool(INPUT)
  val resetVector = UInt(INPUT, p(ResetVectorBits))
  val ndreset = Bool(OUTPUT)
  val dmactive = Bool(OUTPUT)
}

trait CoreplexRISCVPlatformModule extends CoreplexNetworkModule {
  val outer: CoreplexRISCVPlatform
  val io: CoreplexRISCVPlatformBundle

  outer.debug.module.io.dmi  <> io.debug
  // TODO in inheriting traits: Set this to something meaningful, e.g. "component is in reset or powered down"
  val nDebugComponents = outer.debug.intnode.bundleOut.size
  outer.debug.module.io.ctrl.debugUnavail := Vec.fill(nDebugComponents){Bool(false)}
  io.dmactive := outer.debug.module.io.ctrl.dmactive
  io.ndreset := outer.debug.module.io.ctrl.ndreset

  // Synchronize the rtc into the coreplex
  val rtcSync = ShiftRegister(io.rtcToggle, 3)
  val rtcLast = Reg(init = Bool(false), next=rtcSync)
  outer.clint.module.io.rtcTick := Reg(init = Bool(false), next=(rtcSync & (~rtcLast)))

  println(outer.dts)
  ElaborationArtefacts.add("dts", outer.dts)
  ElaborationArtefacts.add("json", outer.json)
}
