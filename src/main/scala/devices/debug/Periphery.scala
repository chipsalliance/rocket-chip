// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.debug

import Chisel._
import chisel3.core.{IntParam}
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.coreplex.HasPeripheryBus
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.jtag._
import freechips.rocketchip.util._

/** A knob selecting one of the two possible debug interfaces */
case object IncludeJtagDTM extends Field[Boolean]

/** A wrapper bundle containing one of the two possible debug interfaces */
class DebugIO(implicit p: Parameters) extends ParameterizedBundle()(p) {
  val clockeddmi = (!p(IncludeJtagDTM)).option(new ClockedDMIIO().flip)
  val systemjtag = (p(IncludeJtagDTM)).option(new SystemJTAGIO)
  val ndreset    = Bool(OUTPUT)
  val dmactive   = Bool(OUTPUT)
}

/** Either adds a JTAG DTM to system, and exports a JTAG interface,
  * or exports the Debug Module Interface (DMI), based on a global parameter.
  */
trait HasPeripheryDebug extends HasPeripheryBus {
  val module: HasPeripheryDebugModuleImp

  val debug = LazyModule(new TLDebugModule())

  debug.node := pbus.toVariableWidthSlaves
}

trait HasPeripheryDebugBundle {
  implicit val p: Parameters

  val debug: DebugIO

  def connectDebug(c: Clock, r: Bool, out: Bool, tckHalfPeriod: Int = 2, cmdDelay: Int = 2) {
    debug.clockeddmi.foreach { d =>
      val dtm = Module(new SimDTM).connect(c, r, d, out)
    }
    debug.systemjtag.foreach { sj =>
      val jtag = Module(new JTAGVPI(tckHalfPeriod = tckHalfPeriod, cmdDelay = cmdDelay)).connect(sj.jtag, sj.reset, r, out)
      sj.mfr_id := p(JtagDTMKey).idcodeManufId.U(11.W)
    }
  }
}

trait HasPeripheryDebugModuleImp extends LazyMultiIOModuleImp with HasPeripheryDebugBundle {
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
    outer.debug.module.io.dmi.dmiReset := ResetCatchAndSync(sj.jtag.TCK, sj.reset, "dmiResetCatch")
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

class JTAGVPI(tckHalfPeriod: Int = 2, cmdDelay: Int = 2)(implicit val p: Parameters)
    extends BlackBox ( Map ("TCK_HALF_PERIOD" -> IntParam(tckHalfPeriod),
      "CMD_DELAY" -> IntParam(cmdDelay))) {
  val io = new Bundle {
    val jtag = new JTAGIO(hasTRSTn = false)
    val enable = Bool(INPUT)
    val init_done = Bool(INPUT)
  }

  def connect(dutio: JTAGIO, jtag_reset: Bool, tbreset: Bool, tbsuccess: Bool) = {
    dutio <> io.jtag

    dutio.TRSTn.foreach{ _:= false.B}
    jtag_reset := tbreset

    io.enable    := ~tbreset
    io.init_done := ~tbreset

    // Success is determined by the gdbserver
    // which is controlling this simulation.
    tbsuccess := Bool(false)
  }
}
