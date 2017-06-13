// See LICENSE.SiFive for license details.

package rocketchip

import Chisel._
import config._
import coreplex._
import diplomacy._
import jtag.JTAGIO
import uncore.tilelink2._
import uncore.devices._
import util._

/** All the traits defined in this file assume that they are being mixed in
  * to a system that has a standard RISCV-based coreplex platform.
  */
trait HasCoreplexRISCVPlatform {
  implicit val p: Parameters
  val coreplex: CoreplexRISCVPlatform
}

/** A wrapper around JTAG providing a reset signal and manufacturer id. */
class SystemJTAGIO extends Bundle {
  val jtag = new JTAGIO(hasTRSTn = false).flip
  val reset = Bool(INPUT)
  val mfr_id = UInt(INPUT, 11)
}

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
trait HasPeripheryDebug extends HasSystemNetworks with HasCoreplexRISCVPlatform {
  val module: HasPeripheryDebugModuleImp
}

trait HasPeripheryDebugBundle extends HasPeripheryParameters {
  val debug: DebugIO
  def connectDebug(c: Clock, r: Bool, out: Bool) {
    debug.clockeddmi.foreach { d =>
      val dtm = Module(new SimDTM).connect(c, r, d, out)
    }
    debug.systemjtag.foreach { sj =>
      val jtag = Module(new JTAGVPI).connect(sj.jtag, sj.reset, r, out)
      sj.mfr_id := p(JtagDTMKey).idcodeManufId.U(11.W)
    }
  }
}

trait HasPeripheryDebugModuleImp extends LazyMultiIOModuleImp with HasPeripheryDebugBundle {
  val outer: HasPeripheryDebug

  val debug = IO(new DebugIO)

  debug.clockeddmi.foreach { dbg => outer.coreplex.module.io.debug <> dbg }

  val dtm = debug.systemjtag.map { sj => 
    val dtm = Module(new DebugTransportModuleJTAG(p(DMKey).nDMIAddrSize, p(JtagDTMKey)))
    dtm.io.jtag <> sj.jtag

    dtm.clock          := sj.jtag.TCK
    dtm.io.jtag_reset  := sj.reset
    dtm.io.jtag_mfr_id := sj.mfr_id
    dtm.reset          := dtm.io.fsmReset

    outer.coreplex.module.io.debug.dmi <> dtm.io.dmi
    outer.coreplex.module.io.debug.dmiClock := sj.jtag.TCK
    outer.coreplex.module.io.debug.dmiReset := ResetCatchAndSync(sj.jtag.TCK, sj.reset, "dmiResetCatch")
    dtm
  }

  debug.ndreset  := outer.coreplex.module.io.ndreset
  debug.dmactive := outer.coreplex.module.io.dmactive
}

/** Real-time clock is based on RTCPeriod relative to system clock.
  * Note: nothing about this is diplomatic, all the work is done in the ModuleImp
  */
trait HasPeripheryRTCCounter extends HasSystemNetworks with HasCoreplexRISCVPlatform {
  val module: HasPeripheryRTCCounterModuleImp
}

trait HasPeripheryRTCCounterModuleImp extends LazyMultiIOModuleImp {
  val outer: HasPeripheryRTCCounter
  val period = p(rocketchip.RTCPeriod)
  val rtcCounter = RegInit(UInt(0, width = log2Up(period)))
  val rtcWrap = rtcCounter === UInt(period-1)

  rtcCounter := Mux(rtcWrap, UInt(0), rtcCounter + UInt(1))
  outer.coreplex.module.io.rtcToggle := rtcCounter(log2Up(period)-1)
}

/** Adds a boot ROM that contains the DTB describing the system's coreplex. */
trait HasPeripheryBootROM extends HasSystemNetworks with HasCoreplexRISCVPlatform {
  val bootrom_address = 0x10000
  val bootrom_size    = 0x10000
  val bootrom_hang    = 0x10040
  private lazy val bootrom_contents = GenerateBootROM(coreplex.dtb)
  val bootrom = LazyModule(new TLROM(bootrom_address, bootrom_size, bootrom_contents, true, peripheryBusConfig.beatBytes))

  bootrom.node := TLFragmenter(peripheryBusConfig.beatBytes, cacheBlockBytes)(peripheryBus.node)
}

/** Coreplex will power-on running at 0x10040 (BootROM) */
trait HasPeripheryBootROMModuleImp extends LazyMultiIOModuleImp {
  val outer: HasPeripheryBootROM
  outer.coreplex.module.io.resetVector := UInt(outer.bootrom_hang)
}
