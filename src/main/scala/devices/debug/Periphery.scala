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

/** Protocols used for communicating with external debugging tools */
sealed trait DebugExportProtocol
case object DMI extends DebugExportProtocol
case object JTAG extends DebugExportProtocol
case object CJTAG extends DebugExportProtocol
case object APB extends DebugExportProtocol

/** Options for possible debug interfaces */
case class DebugAttachParams(
  protocols: Set[DebugExportProtocol] = Set(DMI),
  externalDisable: Boolean = false,
  masterWhere: BaseSubsystemBusAttachment = FBUS,
  slaveWhere: BaseSubsystemBusAttachment = CBUS
) {
  def dmi   = protocols.contains(DMI)
  def jtag  = protocols.contains(JTAG)
  def cjtag = protocols.contains(CJTAG)
  def apb   = protocols.contains(APB)
}

case object ExportDebug extends Field(DebugAttachParams())

class ClockedAPBBundle(params: APBBundleParameters) extends APBBundle(params) with Clocked

class DebugIO(implicit val p: Parameters) extends Bundle {
  val clockeddmi = p(ExportDebug).dmi.option(new ClockedDMIIO().flip)
  val systemjtag = p(ExportDebug).jtag.option(new SystemJTAGIO)
  val apb = p(ExportDebug).apb.option(new ClockedAPBBundle(APBBundleParameters(addrBits=12, dataBits=32)).flip)
  //------------------------------
  val ndreset    = Bool(OUTPUT)
  val dmactive   = Bool(OUTPUT)
  val extTrigger = (p(DebugModuleKey).get.nExtTriggers > 0).option(new DebugExtTriggerIO())
  val disableDebug = p(ExportDebug).externalDisable.option(Bool(INPUT))
}

class PSDIO(implicit val p: Parameters) extends Bundle with CanHavePSDTestModeIO {
}

/** Either adds a JTAG DTM to system, and exports a JTAG interface,
  * or exports the Debug Module Interface (DMI), or exports and hooks up APB,
  * based on a global parameter.
  */

trait HasPeripheryDebug { this: BaseSubsystem =>
  private val tlbus = attach(p(ExportDebug).slaveWhere)

  val debugCustomXbarOpt = p(DebugModuleKey).map(params => LazyModule( new DebugCustomXbar(outputRequiresInput = false)))
  val apbDebugNodeOpt = p(ExportDebug).apb.option(APBMasterNode(Seq(APBMasterPortParameters(Seq(APBMasterParameters("debugAPB"))))))
  val debugOpt = p(DebugModuleKey).map { params =>
    val debug = LazyModule(new TLDebugModule(tlbus.beatBytes))

    LogicalModuleTree.add(logicalTreeNode, debug.logicalTreeNode)

    debug.node := tlbus.coupleTo("debug"){ TLFragmenter(tlbus) := _ }
    debug.dmInner.dmInner.customNode := debugCustomXbarOpt.get.node

    (apbDebugNodeOpt zip debug.apbNodeOpt) foreach { case (master, slave) =>
      slave := master
    }

    debug.dmInner.dmInner.sb2tlOpt.foreach { sb2tl  =>
      attach(p(ExportDebug).masterWhere).asInstanceOf[CanAttachTLMasters].fromPort(Some("debug_sb")){
        FlipRendering { implicit p => TLWidthWidget(1) := sb2tl.node }
      }
    }
    debug
  }
}

trait HasPeripheryDebugModuleImp extends LazyModuleImp {
  val outer: HasPeripheryDebug

  val psd = IO(new PSDIO)

  val debug = outer.debugOpt.map { outerdebug =>
    val debug = IO(new DebugIO)
    require(!(debug.clockeddmi.isDefined && debug.systemjtag.isDefined),
      "You cannot have both DMI and JTAG interface in HasPeripheryDebugModuleImp")

    require(!(debug.clockeddmi.isDefined && debug.apb.isDefined),
      "You cannot have both DMI and APB interface in HasPeripheryDebugModuleImp")

    require(!(debug.systemjtag.isDefined && debug.apb.isDefined),
      "You cannot have both APB and JTAG interface in HasPeripheryDebugModuleImp")

    debug.clockeddmi.foreach { dbg => outerdebug.module.io.dmi.get <> dbg }

    (debug.apb
      zip outer.apbDebugNodeOpt
      zip outerdebug.module.io.apb_clock
      zip outerdebug.module.io.apb_reset).foreach {
      case (((io, apb), c ), r) =>
        apb.out(0)._1 <> io
        c:= io.clock
        r:= io.reset
    }

    debug.ndreset := outerdebug.module.io.ctrl.ndreset
    debug.dmactive := outerdebug.module.io.ctrl.dmactive
    debug.extTrigger.foreach { x => outerdebug.module.io.extTrigger.foreach {y => x <> y}}

    // TODO in inheriting traits: Set this to something meaningful, e.g. "component is in reset or powered down"
    outerdebug.module.io.ctrl.debugUnavail.foreach { _ := Bool(false) }

    outerdebug.module.io.psd <> psd.psd.getOrElse(Wire(new PSDTestMode).fromBits(0.U))

    debug
  }

  val dtm = debug.map(_.systemjtag.map { instantiateJtagDTM(_) })

  def instantiateJtagDTM(sj: SystemJTAGIO): DebugTransportModuleJTAG = {

    val dtm = Module(new DebugTransportModuleJTAG(p(DebugModuleKey).get.nDMIAddrSize, p(JtagDTMKey)))
    dtm.io.jtag <> sj.jtag

    debug.map(_.disableDebug.foreach { x => dtm.io.jtag.TMS := sj.jtag.TMS | x })  // force TMS high when debug is disabled
    val psdio = psd.psd.getOrElse(Wire(new PSDTestMode).fromBits(0.U))

    dtm.clock          := sj.jtag.TCK
    dtm.io.jtag_reset  := sj.reset
    dtm.io.jtag_mfr_id := sj.mfr_id
    dtm.io.jtag_part_number := sj.part_number
    dtm.io.jtag_version := sj.version
    dtm.reset          := dtm.io.fsmReset

    outer.debugOpt.map { outerdebug => 
      outerdebug.module.io.dmi.get.dmi <> dtm.io.dmi
      outerdebug.module.io.dmi.get.dmiClock := sj.jtag.TCK
      outerdebug.module.io.dmi.get.dmiReset := ResetCatchAndSync(sj.jtag.TCK, sj.reset, "dmiResetCatch", psdio)
    }
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
      debugOpt: Option[DebugIO],
      psdio: PSDIO,
      c: Clock,
      r: Bool,
      out: Bool,
      tckHalfPeriod: Int = 2,
      cmdDelay: Int = 2,
      psd: PSDTestMode = new PSDTestMode().fromBits(0.U))
      (implicit p: Parameters): Unit =  {
    debugOpt.map { debug =>
      debug.clockeddmi.foreach { d =>
        val dtm = Module(new SimDTM).connect(c, r, d, out)
      }
      debug.systemjtag.foreach { sj =>
        val jtag = Module(new SimJTAG(tickDelay=3)).connect(sj.jtag, c, r, ~r, out)
        sj.reset := r
        sj.mfr_id := p(JtagDTMKey).idcodeManufId.U(11.W)
        sj.part_number := p(JtagDTMKey).idcodePartNum.U(16.W)
        sj.version := p(JtagDTMKey).idcodeVersion.U(4.W)
      }
      debug.apb.foreach { apb =>
        require(false, "No support for connectDebug for an APB debug connection.")
      }
      psdio.psd.foreach { _ <> psd }
      debug.disableDebug.foreach { x => x := Bool(false) }
    }
  }

  def tieoffDebug(debugOpt: Option[DebugIO], psdio: Option[PSDIO] = None): Bool = {

    psdio.foreach(_.psd.foreach { _ <> new PSDTestMode().fromBits(0.U)})
    debugOpt.map { debug =>
      debug.systemjtag.foreach { sj =>
        sj.jtag.TCK := Bool(true).asClock
        sj.jtag.TMS := Bool(true)
        sj.jtag.TDI := Bool(true)
        sj.jtag.TRSTn.foreach { r => r := Bool(true) }
        sj.reset := Bool(true)
        sj.mfr_id := 0.U
        sj.part_number := 0.U
        sj.version := 0.U
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

      debug.disableDebug.foreach { x => x := Bool(false) }
      debug.ndreset
    }.getOrElse(false.B)
  }
}
