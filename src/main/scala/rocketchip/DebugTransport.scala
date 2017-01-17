// See LICENSE.SiFive for license details.

package rocketchip

import Chisel._
import uncore.devices._
import junctions._
import util._
import config._
import jtag._
import uncore.devices.{DbBusConsts, DebugBusReq, DebugBusResp}

case object IncludeJtagDTM extends Field[Boolean]

object dtmJTAGAddrs {
  def IDCODE       = 0x1
  def DTM_INFO     = 0x10
  def DEBUG_ACCESS = 0x11
}
  

class DebugAccessUpdate(addrBits: Int) extends Bundle {
  val op = UInt(width = DbBusConsts.dbOpSize)
  val data = UInt(width = DbBusConsts.dbDataSize)
  val addr = UInt(width = addrBits)
}

class DebugAccessCapture(addrBits: Int) extends Bundle {
  val resp = UInt(width = DbBusConsts.dbRespSize)
  val data = UInt(width = DbBusConsts.dbDataSize)
  val addr = UInt(width = addrBits)
}

class DTMInfo extends Bundle {
  val debugVersion = UInt(width = 4)
  val debugAddrBits = UInt(width = 4)
  val dbusStatus = UInt(width = 2)
  val dbusIdleCycles = UInt(width = 3)
  val reserved0 = UInt(width = 3)
  val dbusreset = Bool()
  val reserved1 = UInt(width = 15)
}

class DebugTransportModuleJTAG(
  idcodeVersion    : Int = 1,
  idcodePartNum   : Int = 0xE31, // Reference to Freedom Everywhere Coreplex
  idcodeManufId   : Int = 0x489, // Assigned by JEDEC
  debugAddrBits   : Int = 5,  // Spec allows 5-7
  debugVersion     : Int = 0,
  debugIdleCycles : Int = 5
) (implicit val p: Parameters) extends Module  {

  val io = new Bundle {
    val dbus = new DebugBusIO()(p)
    val jtag = new JtagIO().flip()
    val fsmReset = Bool(OUTPUT)
  }

  //--------------------------------------------------------
  // Reg and Wire Declarations

  val dtmInfo = Wire(new DTMInfo)

  val busyReg = Reg(init = Bool(false))
  val stickyBusyReg = Reg(init = Bool(false))
  val stickyNonzeroRespReg = Reg(init = Bool(false))

  val skipOpReg = Reg(init = Bool(false)) // Skip op because we're busy
  val downgradeOpReg = Reg(init = Bool(false)) // downgrade op because prev. failed.

  val busy = Wire(Bool())
  val nonzeroResp = Wire(Bool())

  val busyResp = Wire(new DebugAccessCapture(debugAddrBits))
  val nonbusyResp = Wire(new DebugAccessCapture(debugAddrBits))

  val dbusReqReg  = Reg(new DebugBusReq(debugAddrBits))
  val dbusReqValidReg = Reg(init = Bool(false));

  val dbusStatus = Wire(UInt(width = 2))
  
  //--------------------------------------------------------
  // DTM Info Chain

  dbusStatus := Cat(stickyNonzeroRespReg, stickyNonzeroRespReg | stickyBusyReg)

  dtmInfo := UInt(0)
  dtmInfo.debugVersion   := UInt(debugVersion)
  dtmInfo.debugAddrBits  := UInt(debugAddrBits)
  dtmInfo.dbusStatus     := dbusStatus
  dtmInfo.dbusIdleCycles := UInt(debugIdleCycles)

  val dtmInfoChain = Module (CaptureUpdateChain(gen = new DTMInfo()))
  dtmInfoChain.io.capture.bits := dtmInfo

  //--------------------------------------------------------
  // Debug Access Support

  // Busy Register. We become busy when we first try to send a request.
  // We stop being busy when we accept a response.

  when (io.dbus.req.valid) {
    busyReg <= Bool(true)
  }
  when (io.dbus.resp.fire()) {
    busyReg <= Bool(false)
  }
  

  // We are busy during a given CAPTURE
  // if we haven't received a valid response yet or if we
  // were busy last time without a reset.
  // busyReg will still be set when we check it,
  // so the logic for checking busy looks ahead.
  busy := (busyReg & !io.dbus.resp.valid) | stickyBusyReg;

  // Downgrade/Skip. We make the decision to downgrade or skip
  // during every CAPTURE_DR, and use the result in UPDATE_DR.
  // The sticky versions are reset by write to dbusReset in DTM_INFO.
  when (debugAccessChain.io.update.valid) {
    skipOpReg := Bool(false)
    downgradeOpReg := Bool(false)
  }
  when (debugAccessChain.io.capture.capture) {
    skipOpReg := busy
    downgradeOpReg := (!busy & nonzeroResp)
    stickyBusyReg := busy
    stickyNonzeroRespReg <= nonzeroResp
  }
  when (dtmInfoChain.io.update.valid) {
    when (dtmInfoChain.io.update.bits.dbusreset) {
      stickyNonzeroRespReg := Bool(false)
      stickyBusyReg := Bool(false)
    }
  }
  
 
  // Especially for the first request, we must consider dtmResp.valid,
  // so that we don't consider junk in the FIFO to be an error response.
  // The current specification says that any non-zero response is an error.
  nonzeroResp := stickyNonzeroRespReg | (io.dbus.resp.valid & (io.dbus.resp.bits.resp != UInt(0)))

  busyResp := UInt(0)

  nonbusyResp.addr := dbusReqReg.addr
  nonbusyResp.resp := io.dbus.resp.bits.resp
  nonbusyResp.data := io.dbus.resp.bits.addr

  //--------------------------------------------------------
  // Debug Access Chain

  val debugAccessChain = Module(new CaptureUpdateChain(genCapture = new DebugAccessCapture(debugAddrBits),
    genUpdate = new DebugAccessUpdate(debugAddrBits)))

  debugAccessChain.io.capture.bits := Mux(busy, busyResp, nonbusyResp)
  when (debugAccessChain.io.update.valid) {
    skipOpReg := Bool(false)
    downgradeOpReg := Bool(false)
  }
  when (debugAccessChain.io.capture.capture) {
      skipOpReg := busy
      downgradeOpReg := (!busy & nonzeroResp)
      stickyBusyReg := busy
      stickyNonzeroRespReg <= nonzeroResp
  }
  
  //--------------------------------------------------------
  // Drive Ready Valid Interface

  when (debugAccessChain.io.update.valid) {
    when (skipOpReg) {
      // Do Nothing
    }.otherwise {
      when (downgradeOpReg) {
        dbusReqReg = UInt(0) // NOP Encoding is 2'b00
      }.otherwise {
        dbusReqReg := debugAccessChain.io.update.bits
      }
      dbusReqValidReg := Bool(true)
    }
  }.otherwise {
    when (io.dbus.req.ready) {
      dbusReqValidReg := Bool(false)
    }
  }



  io.dtmResp.ready := debugAccessChain.io.capture.capture
  io.dbus.req.valid := dbusReqValidReg

  // This is a name-based, not type-based assignment. Do these still work?
  io.dbus.req.bits := dbusReqReg


  //--------------------------------------------------------
  // Actual JTAG TAP

  val tapIO = JtagTapGenerator(irLength = 5,
    instructions = Map(debugAccessChain -> dtmJTAGAddrs.DEBUG_ACCESS,
      dtmInfoChain -> dtmJTAGAddrs.DTM_INFO),
    idcode = Some((dtmJTAGAddrs.IDCODE, JtagIdcode(idcodeVersion, idcodePartNum, idcodeManufId))))


  //--------------------------------------------------------
  // Reset Generation (this is fed back to us by the instantiating module,
  // and is used to reset the debug registers).

  io.fsmReset = tapIO.status.reset

}



/*  JTAG-based Debug Transport Module
 *  and synchronization logic.
 *  
 *  This implements JTAG interface described
 *  in the RISC-V Debug Specification
 *
 * This Module is currently a
 *  wrapper around a JTAG implementation
 *  of the Debug Transport Module.
 *  This is black-boxed because
 *  Chisel doesn't currently support:
 *    - Negative Edge Clocking
 *    - Asynchronous Resets
 *   (The tristate requirements of JTAG are exported from the 
 *    Chisel domain with the DRV_TDO signal).
 *
 *  The 'TRST' input is used to asynchronously
 *  reset the JTAG TAP.
 *  This design requires that TRST be
 *  synchronized to TCK (for de-assert) outside
 *  of this module. 
 * 
 *  TRST is not a required input. If unused, it 
 *  should be tied low.
 */

class JtagDTMWithSync(implicit val p: Parameters) extends Module {

  // io.DebugBusIO <-> Sync <-> DebugBusIO <-> DTM

  val io = new Bundle {
    // This should be flip.
    val jtag = new JtagIO().flip()
    val debug = new AsyncDebugBusIO
  }

  val jtag_dtm = Module(new DebugTransportModuleJTAG(
    debugAddrBits  = p(DMKey).nDebugBusAddrSize,
    debugIdleCycles = 5)(p))

  jtag_dtm.io.jtag <> io.jtag

  val io_debug_bus = Wire (new DebugBusIO)
  io.debug <> ToAsyncDebugBus(io_debug_bus)

  jtag_dtm.io.dbus := io_debug_bus
}
