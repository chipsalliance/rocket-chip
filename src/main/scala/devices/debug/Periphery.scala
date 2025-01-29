// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.debug

import chisel3._
import chisel3.experimental.{noPrefix, IntParam}
import chisel3.util._

import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy.lazymodule._

import freechips.rocketchip.amba.apb.{APBBundle, APBBundleParameters, APBManagerNode, APBManagerParameters, APBManagerPortParameters}
import freechips.rocketchip.interrupts.{IntSyncXbar, NullIntSyncSource}
import freechips.rocketchip.jtag.JTAGIO
import freechips.rocketchip.prci.{ClockSinkNode, ClockSinkParameters}
import freechips.rocketchip.subsystem.{BaseSubsystem, CBUS, FBUS, ResetSynchronous, SubsystemResetSchemeKey, TLBusWrapperLocation}
import freechips.rocketchip.tilelink.{TLFragmenter, TLWidthWidget}
import freechips.rocketchip.util.{AsyncResetSynchronizerShiftReg, CanHavePSDTestModeIO, ClockGate, PSDTestMode, PlusArg, ResetSynchronizerShiftReg}

import freechips.rocketchip.util.BooleanToAugmentedBoolean

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
  clientWhere: TLBusWrapperLocation = FBUS,
  managerWhere: TLBusWrapperLocation = CBUS
) {
  def dmi   = protocols.contains(DMI)
  def jtag  = protocols.contains(JTAG)
  def cjtag = protocols.contains(CJTAG)
  def apb   = protocols.contains(APB)
}

case object ExportDebug extends Field(DebugAttachParams())

class ClockedAPBBundle(params: APBBundleParameters) extends APBBundle(params) {
  val clock = Clock()
  val reset = Reset()
}


class DebugIO(implicit val p: Parameters) extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Reset())
  val clockeddmi = p(ExportDebug).dmi.option(Flipped(new ClockedDMIIO()))
  val systemjtag = p(ExportDebug).jtag.option(new SystemJTAGIO)
  val apb = p(ExportDebug).apb.option(Flipped(new ClockedAPBBundle(APBBundleParameters(addrBits=12, dataBits=32))))
  //------------------------------
  val ndreset    = Output(Bool())
  val dmactive   = Output(Bool())
  val dmactiveAck = Input(Bool())
  val extTrigger = (p(DebugModuleKey).get.nExtTriggers > 0).option(new DebugExtTriggerIO())
  val disableDebug = p(ExportDebug).externalDisable.option(Input(Bool()))
}

class PSDIO(implicit val p: Parameters) extends Bundle with CanHavePSDTestModeIO {
}

class ResetCtrlIO(val nComponents: Int)(implicit val p: Parameters) extends Bundle {
  val hartResetReq = (p(DebugModuleKey).exists(x=>x.hasHartResets)).option(Output(Vec(nComponents, Bool())))
  val hartIsInReset = Input(Vec(nComponents, Bool()))
}

/** Either adds a JTAG DTM to system, and exports a JTAG interface,
  * or exports the Debug Module Interface (DMI), or exports and hooks up APB,
  * based on a global parameter.
  */

trait HasPeripheryDebug { this: BaseSubsystem =>
  private lazy val tlbus = locateTLBusWrapper(p(ExportDebug).managerWhere)

  lazy val debugCustomXbarOpt = p(DebugModuleKey).map(params => LazyModule( new DebugCustomXbar(outputRequiresInput = false)))
  lazy val apbDebugNodeOpt = p(ExportDebug).apb.option(APBManagerNode(Seq(APBManagerPortParameters(Seq(APBManagerParameters("debugAPB"))))))
  val debugTLDomainOpt = p(DebugModuleKey).map { _ =>
    val domain = ClockSinkNode(Seq(ClockSinkParameters()))
    domain := tlbus.fixedClockNode
    domain
  }
  lazy val debugOpt = p(DebugModuleKey).map { params =>
    val tlDM = LazyModule(new TLDebugModule(tlbus.beatBytes))

    tlDM.node := tlbus.coupleTo("debug"){ TLFragmenter(tlbus.beatBytes, tlbus.blockBytes, nameSuffix = Some("Debug")) := _ }
    tlDM.dmInner.dmInner.customNode := debugCustomXbarOpt.get.node

    // amba interfaces use Mananger -> Subordinate and tilelink uses Client -> Manager to indicate Initator -> Responder flow
    // in this case apbmanager is the Initiator and tlmanager the Responder.
    (apbDebugNodeOpt zip tlDM.apbNodeOpt) foreach { case (apbmanager, tlmanager) =>
      tlmanager := apbmanager
    }

    tlDM.dmInner.dmInner.sb2tlOpt.foreach { sb2tl  =>
      locateTLBusWrapper(p(ExportDebug).clientWhere).coupleFrom("debug_sb") {
        _ := TLWidthWidget(1) := sb2tl.node
      }
    }
    tlDM
  }

  val debugNode = debugOpt.map(_.intnode)

  val psd = InModuleBody {
    val psd = IO(new PSDIO)
    psd
  }

  val resetctrl = InModuleBody {
    debugOpt.map { debug =>
      debug.module.io.tl_reset := debugTLDomainOpt.get.in.head._1.reset
      debug.module.io.tl_clock := debugTLDomainOpt.get.in.head._1.clock
      val resetctrl = IO(new ResetCtrlIO(debug.dmOuter.dmOuter.intnode.edges.out.size))
      debug.module.io.hartIsInReset := resetctrl.hartIsInReset
      resetctrl.hartResetReq.foreach { rcio => debug.module.io.hartResetReq.foreach { rcdm => rcio := rcdm }}
      resetctrl
    }
  }

  // noPrefix is workaround https://github.com/freechipsproject/chisel3/issues/1603
  val debug = InModuleBody { noPrefix(debugOpt.map { debugmod =>
    val debug = IO(new DebugIO)

    require(!(debug.clockeddmi.isDefined && debug.systemjtag.isDefined),
      "You cannot have both DMI and JTAG interface in HasPeripheryDebug")

    require(!(debug.clockeddmi.isDefined && debug.apb.isDefined),
      "You cannot have both DMI and APB interface in HasPeripheryDebug")

    require(!(debug.systemjtag.isDefined && debug.apb.isDefined),
      "You cannot have both APB and JTAG interface in HasPeripheryDebug")

    debug.clockeddmi.foreach { dbg => debugmod.module.io.dmi.get <> dbg }

    (debug.apb
      zip apbDebugNodeOpt
      zip debugmod.module.io.apb_clock
      zip debugmod.module.io.apb_reset).foreach {
      case (((io, apb), c ), r) =>
        apb.out(0)._1 <> io
        c:= io.clock
        r:= io.reset
    }

    debugmod.module.io.debug_reset := debug.reset
    debugmod.module.io.debug_clock := debug.clock

    debug.ndreset := debugmod.module.io.ctrl.ndreset
    debug.dmactive := debugmod.module.io.ctrl.dmactive
    debugmod.module.io.ctrl.dmactiveAck := debug.dmactiveAck
    debug.extTrigger.foreach { x => debugmod.module.io.extTrigger.foreach {y => x <> y}}

    // TODO in inheriting traits: Set this to something meaningful, e.g. "component is in reset or powered down"
    debugmod.module.io.ctrl.debugUnavail.foreach { _ := false.B }

    debug
  })}

  val dtm = InModuleBody { debug.flatMap(_.systemjtag.map(instantiateJtagDTM(_))) }

  def instantiateJtagDTM(sj: SystemJTAGIO): DebugTransportModuleJTAG = {

    val dtm = Module(new DebugTransportModuleJTAG(p(DebugModuleKey).get.nDMIAddrSize, p(JtagDTMKey)))
    dtm.io.jtag <> sj.jtag

    debug.map(_.disableDebug.foreach { x => dtm.io.jtag.TMS := sj.jtag.TMS | x })  // force TMS high when debug is disabled

    dtm.io.jtag_clock  := sj.jtag.TCK
    dtm.io.jtag_reset  := sj.reset
    dtm.io.jtag_mfr_id := sj.mfr_id
    dtm.io.jtag_part_number := sj.part_number
    dtm.io.jtag_version := sj.version
    dtm.rf_reset := sj.reset

    debugOpt.map { outerdebug =>
      outerdebug.module.io.dmi.get.dmi <> dtm.io.dmi
      outerdebug.module.io.dmi.get.dmiClock := sj.jtag.TCK
      outerdebug.module.io.dmi.get.dmiReset := sj.reset
    }
    dtm
  }
}
/** BlackBox to export DMI interface */
class SimDTM(implicit p: Parameters) extends BlackBox with HasBlackBoxResource {
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val reset = Input(Bool())
    val debug = new DMIIO
    val exit = Output(UInt(32.W))
  })

  def connect(tbclk: Clock, tbreset: Bool, dutio: ClockedDMIIO, tbsuccess: Bool) = {
    io.clk := tbclk
    io.reset := tbreset
    dutio.dmi <> io.debug
    dutio.dmiClock := tbclk
    dutio.dmiReset := tbreset

    tbsuccess := io.exit === 1.U
    assert(io.exit < 2.U, "*** FAILED *** (exit code = %d)\n", io.exit >> 1.U)
  }

  addResource("/vsrc/SimDTM.v")
  addResource("/csrc/SimDTM.cc")
}
/** BlackBox to export JTAG interface */
class SimJTAG(tickDelay: Int = 50) extends BlackBox(Map("TICK_DELAY" -> IntParam(tickDelay)))
  with HasBlackBoxResource {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val jtag = new JTAGIO(hasTRSTn = true)
    val enable = Input(Bool())
    val init_done = Input(Bool())
    val exit = Output(UInt(32.W))
  })

  def connect(dutio: JTAGIO, tbclock: Clock, tbreset: Bool, init_done: Bool, tbsuccess: Bool) = {
    dutio.TCK := io.jtag.TCK
    dutio.TMS := io.jtag.TMS
    dutio.TDI := io.jtag.TDI
    io.jtag.TDO := dutio.TDO

    io.clock := tbclock
    io.reset := tbreset

    io.enable    := PlusArg("jtag_rbb_enable", 0, "Enable SimJTAG for JTAG Connections. Simulation will pause until connection is made.")
    io.init_done := init_done

    // Success is determined by the gdbserver
    // which is controlling this simulation.
    tbsuccess := io.exit === 1.U
    assert(io.exit < 2.U, "*** FAILED *** (exit code = %d)\n", io.exit >> 1.U)
  }

  addResource("/vsrc/SimJTAG.v")
  addResource("/csrc/SimJTAG.cc")
  addResource("/csrc/remote_bitbang.h")
  addResource("/csrc/remote_bitbang.cc")
}

object Debug {
  def connectDebug(
      debugOpt: Option[DebugIO],
      resetctrlOpt: Option[ResetCtrlIO],
      psdio: PSDIO,
      c: Clock,
      r: Bool,
      out: Bool,
      tckHalfPeriod: Int = 2,
      cmdDelay: Int = 2,
      psd: PSDTestMode = 0.U.asTypeOf(new PSDTestMode()))
      (implicit p: Parameters): Unit =  {
    connectDebugClockAndReset(debugOpt, c)
    resetctrlOpt.map { rcio => rcio.hartIsInReset.map { _ := r }}
    debugOpt.map { debug =>
      debug.clockeddmi.foreach { d =>
        val dtm = Module(new SimDTM).connect(c, r, d, out)
      }
      debug.systemjtag.foreach { sj =>
        val jtag = Module(new SimJTAG(tickDelay=3)).connect(sj.jtag, c, r, ~r, out)
        sj.reset := r.asAsyncReset
        sj.mfr_id := p(JtagDTMKey).idcodeManufId.U(11.W)
        sj.part_number := p(JtagDTMKey).idcodePartNum.U(16.W)
        sj.version := p(JtagDTMKey).idcodeVersion.U(4.W)
      }
      debug.apb.foreach { apb =>
        require(false, "No support for connectDebug for an APB debug connection.")
      }
      psdio.psd.foreach { _ <> psd }
      debug.disableDebug.foreach { x => x := false.B }
    }
  }

  def connectDebugClockAndReset(debugOpt: Option[DebugIO], c: Clock, sync: Boolean = true)(implicit p: Parameters): Unit = {
    debugOpt.foreach { debug =>
      val dmi_reset = debug.clockeddmi.map(_.dmiReset.asBool).getOrElse(false.B) |
        debug.systemjtag.map(_.reset.asBool).getOrElse(false.B) |
        debug.apb.map(_.reset.asBool).getOrElse(false.B)
      connectDebugClockHelper(debug, dmi_reset, c, sync)
    }
  }

  def connectDebugClockHelper(debug: DebugIO, dmi_reset: Reset, c: Clock, sync: Boolean = true)(implicit p: Parameters): Unit = {
    val debug_reset = Wire(Bool())
    withClockAndReset(c, dmi_reset) {
      val debug_reset_syncd = if(sync) ~AsyncResetSynchronizerShiftReg(in=true.B, sync=3, name=Some("debug_reset_sync")) else dmi_reset
      debug_reset := debug_reset_syncd
    }
    // Need to clock DM during debug_reset because of synchronous reset, so keep
    // the clock alive for one cycle after debug_reset asserts to action this behavior.
    // The unit should also be clocked when dmactive is high.
    withClockAndReset(c, debug_reset.asAsyncReset) {
      val dmactiveAck = if (sync) ResetSynchronizerShiftReg(in=debug.dmactive, sync=3, name=Some("dmactiveAck")) else debug.dmactive
      val clock_en = RegNext(next=dmactiveAck, init=true.B)
      val gated_clock =
        if (!p(DebugModuleKey).get.clockGate) c
        else ClockGate(c, clock_en, "debug_clock_gate")
      debug.clock := gated_clock
      debug.reset := (if (p(SubsystemResetSchemeKey)==ResetSynchronous) debug_reset else debug_reset.asAsyncReset)
      debug.dmactiveAck := dmactiveAck
    }
  }

  def tieoffDebug(debugOpt: Option[DebugIO], resetctrlOpt: Option[ResetCtrlIO] = None, psdio: Option[PSDIO] = None)(implicit p: Parameters): Bool = {

    psdio.foreach(_.psd.foreach { _ <> 0.U.asTypeOf(new PSDTestMode()) } )
    resetctrlOpt.map { rcio => rcio.hartIsInReset.map { _ := false.B }}
    debugOpt.map { debug =>
      debug.clock := true.B.asClock
      debug.reset := (if (p(SubsystemResetSchemeKey)==ResetSynchronous) true.B else true.B.asAsyncReset)

      debug.systemjtag.foreach { sj =>
        sj.jtag.TCK := true.B.asClock
        sj.jtag.TMS := true.B
        sj.jtag.TDI := true.B
        sj.jtag.TRSTn.foreach { r => r := true.B }
        sj.reset := true.B.asAsyncReset
        sj.mfr_id := 0.U
        sj.part_number := 0.U
        sj.version := 0.U
      }

      debug.clockeddmi.foreach { d =>
        d.dmi.req.valid := false.B
        d.dmi.req.bits.addr := 0.U
        d.dmi.req.bits.data := 0.U
        d.dmi.req.bits.op := 0.U
        d.dmi.resp.ready := true.B
        d.dmiClock := false.B.asClock
        d.dmiReset := true.B.asAsyncReset
      }

      debug.apb.foreach { apb =>
        apb.clock := false.B.asClock
        apb.reset := true.B.asAsyncReset
        apb.pready := false.B
        apb.pslverr := false.B
        apb.prdata := 0.U
        apb.pduser := 0.U.asTypeOf(chiselTypeOf(apb.pduser))
        apb.psel := false.B
        apb.penable := false.B
      }

      debug.extTrigger.foreach { t =>
        t.in.req := false.B
        t.out.ack := t.out.req
      }
      debug.disableDebug.foreach { x => x := false.B }
      debug.dmactiveAck := false.B
      debug.ndreset
    }.getOrElse(false.B)
  }
}
