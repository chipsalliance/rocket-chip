// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.debug

import Chisel._
import chisel3.core.{IntParam, Input, Output}
import chisel3.util.HasBlackBoxResource
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.amba.apb._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomaticobjectmodel.logicaltree.LogicalModuleTree
import freechips.rocketchip.diplomaticobjectmodel.model.OMComponent
import freechips.rocketchip.jtag._
import freechips.rocketchip.util._
import freechips.rocketchip.tilelink._

/** Options for possible debug interfaces */
case object ExportDebugDMI extends Field[Boolean](true)
case object ExportDebugJTAG extends Field[Boolean](false)
case object ExportDebugCJTAG extends Field[Boolean](false)
case object ExportDebugAPB extends Field[Boolean](false)
case object ExportDisableDebug extends Field[Boolean](false)

class ClockedAPBBundle(params: APBBundleParameters) extends APBBundle(params) with Clocked

class DebugIO(implicit val p: Parameters) extends ParameterizedBundle()(p) with CanHavePSDTestModeIO {
  val clockeddmi = p(ExportDebugDMI).option(new ClockedDMIIO().flip)
  val systemjtag = p(ExportDebugJTAG).option(new SystemJTAGIO)
  val apb = p(ExportDebugAPB).option(new ClockedAPBBundle(APBBundleParameters(addrBits=12, dataBits=32)).flip)
  //------------------------------
  val ndreset    = Bool(OUTPUT)
  val dmactive   = Bool(OUTPUT)
  val extTrigger = (p(DebugModuleParams).nExtTriggers > 0).option(new DebugExtTriggerIO())
  val disableDebug = p(ExportDisableDebug).option(Bool(INPUT))
}

/** Either adds a JTAG DTM to system, and exports a JTAG interface,
  * or exports the Debug Module Interface (DMI), or exports and hooks up APB,
  * based on a global parameter.
  */

trait HasPeripheryDebug { this: BaseSubsystem =>
  val debug = LazyModule(new TLDebugModule(cbus.beatBytes))

  LogicalModuleTree.add(logicalTreeNode, debug.logicalTreeNode)

  debug.node := cbus.coupleTo("debug"){ TLFragmenter(cbus) := _ }
  val debugCustomXbar = LazyModule( new DebugCustomXbar(outputRequiresInput = false))
  debug.dmInner.dmInner.customNode := debugCustomXbar.node

  val apbDebugNodeOpt = p(ExportDebugAPB).option(APBMasterNode(Seq(APBMasterPortParameters(Seq(APBMasterParameters("debugAPB"))))))

  (apbDebugNodeOpt zip debug.apbNodeOpt) foreach { case (master, slave) =>
    slave := master
  }

  debug.dmInner.dmInner.sb2tlOpt.foreach { sb2tl  =>
    fbus.fromPort(Some("debug_sb")){ FlipRendering { implicit p => TLWidthWidget(1) := sb2tl.node } }
  }
}

trait HasPeripheryDebugModuleImp extends LazyModuleImp {
  val outer: HasPeripheryDebug

  val debug = IO(new DebugIO)

  require(!(debug.clockeddmi.isDefined && debug.systemjtag.isDefined),
    "You cannot have both DMI and JTAG interface in HasPeripheryDebugModuleImp")

  require(!(debug.clockeddmi.isDefined && debug.apb.isDefined),
    "You cannot have both DMI and APB interface in HasPeripheryDebugModuleImp")

  require(!(debug.systemjtag.isDefined && debug.apb.isDefined),
    "You cannot have both APB and JTAG interface in HasPeripheryDebugModuleImp")

  debug.clockeddmi.foreach { dbg => outer.debug.module.io.dmi.get <> dbg }

  (debug.apb
    zip outer.apbDebugNodeOpt
    zip outer.debug.module.io.apb_clock
    zip outer.debug.module.io.apb_reset).foreach {
    case (((io, apb), c ), r) =>
      apb.out(0)._1 <> io
      c:= io.clock
      r:= io.reset
  }

  val dtm = debug.systemjtag.map { instantiateJtagDTM(_) }

  debug.ndreset  := outer.debug.module.io.ctrl.ndreset
  debug.dmactive := outer.debug.module.io.ctrl.dmactive
  debug.extTrigger.foreach { x => outer.debug.module.io.extTrigger.foreach {y => x <> y}}

  // TODO in inheriting traits: Set this to something meaningful, e.g. "component is in reset or powered down"
  outer.debug.module.io.ctrl.debugUnavail.foreach { _ := Bool(false) }

  def instantiateJtagDTM(sj: SystemJTAGIO): DebugTransportModuleJTAG = {

    val dtm = Module(new DebugTransportModuleJTAG(p(DebugModuleParams).nDMIAddrSize, p(JtagDTMKey)))
    dtm.io.jtag <> sj.jtag
    debug.disableDebug.foreach { x => dtm.io.jtag.TMS := sj.jtag.TMS | x }  // force TMS high when debug is disabled

    dtm.clock          := sj.jtag.TCK
    dtm.io.jtag_reset  := sj.reset
    dtm.io.jtag_mfr_id := sj.mfr_id
    dtm.reset          := dtm.io.fsmReset

    outer.debug.module.io.dmi.get.dmi <> dtm.io.dmi
    outer.debug.module.io.dmi.get.dmiClock := sj.jtag.TCK

    val psd = debug.psd.getOrElse(Wire(new PSDTestMode).fromBits(0.U))
    outer.debug.module.io.psd <> psd
    outer.debug.module.io.dmi.get.dmiReset := ResetCatchAndSync(sj.jtag.TCK, sj.reset, "dmiResetCatch", psd)
    dtm
  }
}

class SimDTM(implicit p: Parameters) extends BlackBox with HasBlackBoxResource {
  val io = new Bundle {
    val clk = Clock(INPUT)
    val reset = Bool(INPUT)
    val debug = new DMIIO
    val exit = UInt(OUTPUT, 32)
  }

  def connect(tbclk: Clock, tbreset: Bool, dutio: ClockedDMIIO, tbsuccess: Bool) = {
    io.clk := tbclk
    io.reset := tbreset
    dutio.dmi <> io.debug
    dutio.dmiClock := tbclk
    dutio.dmiReset := tbreset

    tbsuccess := io.exit === UInt(1)
    when (io.exit >= UInt(2)) {
      printf("*** FAILED *** (exit code = %d)\n", io.exit >> UInt(1))
      stop(1)
    }
  }

  setResource("/vsrc/SimDTM.v")
  setResource("/csrc/SimDTM.cc")
}

class SimJTAG(tickDelay: Int = 50) extends BlackBox(Map("TICK_DELAY" -> IntParam(tickDelay)))
  with HasBlackBoxResource {
  val io = new Bundle {
    val clock = Clock(INPUT)
    val reset = Bool(INPUT)
    val jtag = new JTAGIO(hasTRSTn = true)
    val enable = Bool(INPUT)
    val init_done = Bool(INPUT)
    val exit = UInt(OUTPUT, 32)
  }

  def connect(dutio: JTAGIO, tbclock: Clock, tbreset: Bool, init_done: Bool, tbsuccess: Bool) = {
    dutio <> io.jtag

    io.clock := tbclock
    io.reset := tbreset

    io.enable    := PlusArg("jtag_rbb_enable", 0, "Enable SimJTAG for JTAG Connections. Simulation will pause until connection is made.")
    io.init_done := init_done

    // Success is determined by the gdbserver
    // which is controlling this simulation.
    tbsuccess := io.exit === UInt(1)
    when (io.exit >= UInt(2)) {
      printf("*** FAILED *** (exit code = %d)\n", io.exit >> UInt(1))
      stop(1)
    }
  }

  setResource("/vsrc/SimJTAG.v")
  setResource("/csrc/SimJTAG.cc")
  setResource("/csrc/remote_bitbang.h")
  setResource("/csrc/remote_bitbang.cc")
}

object Debug {
  def connectDebug(
      debug: DebugIO,
      c: Clock,
      r: Bool,
      out: Bool,
      tckHalfPeriod: Int = 2,
      cmdDelay: Int = 2,
      psd: PSDTestMode = new PSDTestMode().fromBits(0.U))
      (implicit p: Parameters): Unit =  {
    debug.clockeddmi.foreach { d =>
      val dtm = Module(new SimDTM).connect(c, r, d, out)
    }
    debug.systemjtag.foreach { sj =>
      val jtag = Module(new SimJTAG(tickDelay=3)).connect(sj.jtag, c, r, ~r, out)
      sj.reset := r
      sj.mfr_id := p(JtagDTMKey).idcodeManufId.U(11.W)
    }
    debug.apb.foreach { apb =>
      require(false, "No support for connectDebug for an APB debug connection.")
    }
    debug.psd.foreach { _ <> psd }
    debug.disableDebug.foreach { x => x := Bool(false) }
  }

  def tieoffDebug(debug: DebugIO): Bool = {

    debug.systemjtag.foreach { sj =>
      sj.jtag.TCK := Bool(true).asClock
      sj.jtag.TMS := Bool(true)
      sj.jtag.TDI := Bool(true)
      sj.jtag.TRSTn.foreach { r => r := Bool(true) }
      sj.reset := Bool(true)
      sj.mfr_id := 0.U
    }

    debug.clockeddmi.foreach { d =>
      d.dmi.req.valid := Bool(false)
      d.dmi.resp.ready := Bool(true)
      d.dmiClock := Bool(false).asClock
      d.dmiReset := Bool(true)
    }

    debug.apb.foreach { apb =>
      apb.tieoff()
      apb.clock := Bool(false).asClock
      apb.reset := Bool(true)
    }

    debug.psd.foreach { _ <> new PSDTestMode().fromBits(0.U)}
    debug.disableDebug.foreach { x => x := Bool(false) }
    debug.ndreset
  }
}
