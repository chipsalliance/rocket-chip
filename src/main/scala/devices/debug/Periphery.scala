// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.debug

import Chisel._
import chisel3.core.{IntParam, Input, Output}
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.jtag._
import freechips.rocketchip.util._

/** A knob selecting one of the two possible debug interfaces */
case object IncludeJtagDTM extends Field[Boolean](false)

/** A wrapper bundle containing one of the two possible debug interfaces */

class DebugIO(implicit val p: Parameters) extends ParameterizedBundle()(p) with CanHavePSDTestModeIO {
  val clockeddmi = (!p(IncludeJtagDTM)).option(new ClockedDMIIO().flip)
  val systemjtag = (p(IncludeJtagDTM)).option(new SystemJTAGIO)
  val ndreset    = Bool(OUTPUT)
  val dmactive   = Bool(OUTPUT)
}

/** Either adds a JTAG DTM to system, and exports a JTAG interface,
  * or exports the Debug Module Interface (DMI), based on a global parameter.
  */
trait HasPeripheryDebug { this: BaseSubsystem =>
  val debug = LazyModule(new TLDebugModule(pbus.beatBytes))
  pbus.toVariableWidthSlave(Some("debug")){ debug.node }

  debug.dmInner.dmInner.sb2tlOpt.foreach { sb2tl  =>
    fbus.fromPort(Some("debug_sb")){ sb2tl.node }
  }
}

trait HasPeripheryDebugBundle {
  implicit val p: Parameters

  val debug: DebugIO

  def connectDebug(c: Clock,
    r: Bool,
    out: Bool,
    tckHalfPeriod: Int = 2,
    cmdDelay: Int = 2,
    psd: PSDTestMode = new PSDTestMode().fromBits(0.U)): Unit =  {
    debug.clockeddmi.foreach { d =>
      val dtm = Module(new SimDTM).connect(c, r, d, out)
    }
    debug.systemjtag.foreach { sj =>
      val jtag = Module(new SimJTAG(tickDelay=3)).connect(sj.jtag, c, r, ~r, out)
      sj.reset := r
      sj.mfr_id := p(JtagDTMKey).idcodeManufId.U(11.W)
    }
    debug.psd.foreach { _ <> psd }
  }
}

trait HasPeripheryDebugModuleImp extends LazyModuleImp with HasPeripheryDebugBundle {
  val outer: HasPeripheryDebug

  val debug = IO(new DebugIO)

  debug.clockeddmi.foreach { dbg => outer.debug.module.io.dmi <> dbg }

  val dtm = debug.systemjtag.map { sj =>

    val dtm = Module(new DebugTransportModuleJTAG(p(DebugModuleParams).nDMIAddrSize, p(JtagDTMKey)))
    dtm.io.jtag <> sj.jtag

    dtm.clock          := sj.jtag.TCK
    dtm.io.jtag_reset  := sj.reset
    dtm.io.jtag_mfr_id := sj.mfr_id
    dtm.reset          := dtm.io.fsmReset

    outer.debug.module.io.dmi.dmi <> dtm.io.dmi
    outer.debug.module.io.dmi.dmiClock := sj.jtag.TCK

    val psd = debug.psd.getOrElse(Wire(new PSDTestMode).fromBits(0.U))
    outer.debug.module.io.psd <> psd
    outer.debug.module.io.dmi.dmiReset := ResetCatchAndSync(sj.jtag.TCK, sj.reset, "dmiResetCatch", psd)
    dtm
  }

  debug.ndreset  := outer.debug.module.io.ctrl.ndreset
  debug.dmactive := outer.debug.module.io.ctrl.dmactive

  // TODO in inheriting traits: Set this to something meaningful, e.g. "component is in reset or powered down"
  outer.debug.module.io.ctrl.debugUnavail.foreach { _ := Bool(false) }
}

class SimDTM(implicit p: Parameters) extends BlackBox {
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
}

class SimJTAG(tickDelay: Int = 50) extends BlackBox(Map("TICK_DELAY" -> IntParam(tickDelay))) {
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
}

