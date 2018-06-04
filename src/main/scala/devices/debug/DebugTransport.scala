// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.debug

import Chisel._

import freechips.rocketchip.config._
import freechips.rocketchip.jtag._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property._

case class JtagDTMConfig (
  idcodeVersion    : Int,      // chosen by manuf.
  idcodePartNum    : Int,      // Chosen by manuf.
  idcodeManufId    : Int,      // Assigned by JEDEC
  // Note: the actual manufId is passed in through a wire.
  // Do not forget to wire up io.jtag_mfr_id through your top-level to set the
  // mfr_id for this core.
  // If you wish to use this field in the config, you can obtain it along
  // the lines of p(JtagDTMKey).idcodeManufId.U(11.W).
  debugIdleCycles  : Int)

case object JtagDTMKey extends Field[JtagDTMConfig](new JtagDTMKeyDefault())

class JtagDTMKeyDefault extends JtagDTMConfig(
  idcodeVersion = 0,
  idcodePartNum = 0,
  idcodeManufId = 0,
  debugIdleCycles = 5) // Reasonable guess for synchronization.

object dtmJTAGAddrs {
  def IDCODE       = 0x1
  def DTM_INFO     = 0x10
  def DMI_ACCESS = 0x11
}

class DMIAccessUpdate(addrBits: Int) extends Bundle {
  val addr = UInt(width = addrBits)
  val data = UInt(width = DMIConsts.dmiDataSize)
  val op = UInt(width = DMIConsts.dmiOpSize)

  override def cloneType = new DMIAccessUpdate(addrBits).asInstanceOf[this.type]
}

class DMIAccessCapture(addrBits: Int) extends Bundle {
  val addr = UInt(width = addrBits)
  val data = UInt(width = DMIConsts.dmiDataSize)
  val resp = UInt(width = DMIConsts.dmiRespSize)

  override def cloneType = new DMIAccessCapture(addrBits).asInstanceOf[this.type]

}

class DTMInfo extends Bundle {
  val reserved1 = UInt(15.W)
  val dmireset = Bool()
  val reserved0 = UInt(1.W)
  val dmiIdleCycles = UInt(3.W)
  val dmiStatus = UInt(2.W)
  val debugAddrBits = UInt(6.W)
  val debugVersion = UInt(4.W)
}

/** A wrapper around JTAG providing a reset signal and manufacturer id. */
class SystemJTAGIO extends Bundle {
  val jtag = new JTAGIO(hasTRSTn = false).flip
  val reset = Bool(INPUT)
  val mfr_id = UInt(INPUT, 11)
}

class DebugTransportModuleJTAG(debugAddrBits: Int, c: JtagDTMConfig)
  (implicit val p: Parameters) extends Module  {

  val io = new Bundle {
    val dmi = new DMIIO()(p)
    val jtag = Flipped(new JTAGIO(hasTRSTn = false)) // TODO: re-use SystemJTAGIO here?
    val jtag_reset = Bool(INPUT)
    val jtag_mfr_id = UInt(INPUT, 11)
    val fsmReset = Bool(OUTPUT)
  }

  //--------------------------------------------------------
  // Reg and Wire Declarations

  val dtmInfo = Wire(new DTMInfo)

  val busyReg = RegInit(Bool(false))
  val stickyBusyReg = RegInit(Bool(false))
  val stickyNonzeroRespReg = RegInit(Bool(false))

  val skipOpReg = Reg(init = Bool(false)) // Skip op because we're busy
  val downgradeOpReg = Reg(init = Bool(false)) // downgrade op because prev. failed.

  val busy = Wire(Bool())
  val nonzeroResp = Wire(Bool())

  val busyResp    = Wire(new DMIAccessCapture(debugAddrBits))
  val nonbusyResp = Wire(new DMIAccessCapture(debugAddrBits))
  val dmiResp     = Wire(new DMIAccessCapture(debugAddrBits))
  val nopResp     = Wire(new DMIAccessCapture(debugAddrBits))


  val dmiReqReg  = Reg(new DMIReq(debugAddrBits))
  val dmiReqValidReg = Reg(init = Bool(false));

  val dmiStatus = Wire(UInt(width = 2))

  //--------------------------------------------------------
  // DTM Info Chain Declaration

  dmiStatus := Cat(stickyNonzeroRespReg, stickyNonzeroRespReg | stickyBusyReg)

  dtmInfo.debugVersion   := 1.U // This implements version 1 of the spec.
  dtmInfo.debugAddrBits  := UInt(debugAddrBits)
  dtmInfo.dmiStatus     := dmiStatus
  dtmInfo.dmiIdleCycles := UInt(c.debugIdleCycles)
  dtmInfo.reserved0      := 0.U
  dtmInfo.dmireset      := false.B // This is write-only
  dtmInfo.reserved1      := 0.U

  val dtmInfoChain = Module (CaptureUpdateChain(gen = new DTMInfo()))
  dtmInfoChain.io.capture.bits := dtmInfo

  //--------------------------------------------------------
  // Debug Access Chain Declaration

   val dmiAccessChain = Module(CaptureUpdateChain(genCapture = new DMIAccessCapture(debugAddrBits),
     genUpdate = new DMIAccessUpdate(debugAddrBits)))

  //--------------------------------------------------------
  // Debug Access Support

  // Busy Register. We become busy when we first try to send a request.
  // We stop being busy when we accept a response.

  when (io.dmi.req.valid) {
    busyReg := Bool(true)
  }
  when (io.dmi.resp.fire()) {
    busyReg := Bool(false)
  }

  // We are busy during a given CAPTURE
  // if we haven't received a valid response yet or if we
  // were busy last time without a reset.
  // busyReg will still be set when we check it,
  // so the logic for checking busy looks ahead.
  busy := (busyReg & !io.dmi.resp.valid) | stickyBusyReg;

  // Downgrade/Skip. We make the decision to downgrade or skip
  // during every CAPTURE_DR, and use the result in UPDATE_DR.
  // The sticky versions are reset by write to dmiReset in DTM_INFO.
  when (dmiAccessChain.io.update.valid) {
    skipOpReg := Bool(false)
    downgradeOpReg := Bool(false)
  }
  when (dmiAccessChain.io.capture.capture) {
    skipOpReg := busy
    downgradeOpReg := (!busy & nonzeroResp)
    stickyBusyReg := busy
    stickyNonzeroRespReg := nonzeroResp
  }
  when (dtmInfoChain.io.update.valid) {
    when (dtmInfoChain.io.update.bits.dmireset) {
      stickyNonzeroRespReg := Bool(false)
      stickyBusyReg := Bool(false)
    }
  }

  // Especially for the first request, we must consider dtmResp.valid,
  // so that we don't consider junk in the FIFO to be an error response.
  // The current specification says that any non-zero response is an error.
  // But there is actually no case in the current design where you SHOULD get an error,
  // as we haven't implemented Bus Masters or Serial Ports, which are the only cases errors
  // can occur.
  nonzeroResp := stickyNonzeroRespReg | (io.dmi.resp.valid & (io.dmi.resp.bits.resp =/= UInt(0)))
  assert(!nonzeroResp, "There is no reason to get a non zero response in the current system.");
  assert(!stickyNonzeroRespReg, "There is no reason to have a sticky non zero response in the current system.");

  busyResp.addr  := UInt(0)
  busyResp.resp  := Fill(DMIConsts.dmiRespSize, 1.U) // Generalizing busy to 'all-F'
  busyResp.data  := UInt(0)

  dmiResp.addr := dmiReqReg.addr
  dmiResp.resp := io.dmi.resp.bits.resp
  dmiResp.data := io.dmi.resp.bits.data

  nopResp.addr := UInt(0)
  nopResp.resp := UInt(0)
  nopResp.data := UInt(0)

  //--------------------------------------------------------
  // Debug Access Chain Implementation

  dmiAccessChain.io.capture.bits := Mux(busy, busyResp, Mux(io.dmi.resp.valid, dmiResp, nopResp))
  when (dmiAccessChain.io.update.valid) {
    skipOpReg := Bool(false)
    downgradeOpReg := Bool(false)
  }
  when (dmiAccessChain.io.capture.capture) {
    skipOpReg := busy
    downgradeOpReg := (!busy & nonzeroResp)
    stickyBusyReg := busy
    stickyNonzeroRespReg := nonzeroResp
  }

  //--------------------------------------------------------
  // Drive Ready Valid Interface

  val dmiReqValidCheck = Wire(init = Bool(false))
  assert(!(dmiReqValidCheck && io.dmi.req.fire()), "Conflicting updates for dmiReqValidReg, should not happen.");

  when (dmiAccessChain.io.update.valid) {
    when (skipOpReg) {
      // Do Nothing
    }.elsewhen (downgradeOpReg || (dmiAccessChain.io.update.bits.op === DMIConsts.dmi_OP_NONE)) {
      //Do Nothing
      dmiReqReg.addr := UInt(0)
      dmiReqReg.data := UInt(0)
      dmiReqReg.op   := UInt(0)
    }.otherwise {
      dmiReqReg := dmiAccessChain.io.update.bits
      dmiReqValidReg := Bool(true)
      dmiReqValidCheck := Bool(true)
    }
  }

  when (io.dmi.req.fire()) {
    dmiReqValidReg := Bool(false)
  }

  io.dmi.resp.ready := Mux(
    dmiReqReg.op === DMIConsts.dmi_OP_WRITE,
      // for write operations confirm resp immediately because we don't care about data
      io.dmi.resp.valid,
      // for read operations confirm resp when we capture the data
      dmiAccessChain.io.capture.capture)

  // incorrect operation - not enough time was spent in JTAG Idle state after DMI Write
  cover(dmiReqReg.op === DMIConsts.dmi_OP_WRITE & dmiAccessChain.io.capture.capture & busy, "Not enough Idle after DMI Write");
  // correct operation - enough time was spent in JTAG Idle state after DMI Write
  cover(dmiReqReg.op === DMIConsts.dmi_OP_WRITE & dmiAccessChain.io.capture.capture & !busy, "Enough Idle after DMI Write");

  // incorrect operation - not enough time was spent in JTAG Idle state after DMI Read
  cover(dmiReqReg.op === DMIConsts.dmi_OP_READ & dmiAccessChain.io.capture.capture & busy, "Not enough Idle after DMI Read");
  // correct operation - enough time was spent in JTAG Idle state after DMI Read
  cover(dmiReqReg.op === DMIConsts.dmi_OP_READ & dmiAccessChain.io.capture.capture & !busy, "Enough Idle after DMI Read");

  io.dmi.req.valid := dmiReqValidReg

  // This is a name-based, not type-based assignment. Do these still work?
  io.dmi.req.bits := dmiReqReg

  //--------------------------------------------------------
  // Actual JTAG TAP
  val idcode = Wire(init = new JTAGIdcodeBundle().fromBits(0.U))
  idcode.always1    := 1.U
  idcode.version    := c.idcodeVersion.U
  idcode.partNumber := c.idcodePartNum.U
  idcode.mfrId      := io.jtag_mfr_id

  val tapIO = JtagTapGenerator(irLength = 5,
    instructions = Map(
      dtmJTAGAddrs.DMI_ACCESS -> dmiAccessChain,
      dtmJTAGAddrs.DTM_INFO   -> dtmInfoChain),
    icode = Some(dtmJTAGAddrs.IDCODE)
  )

  tapIO.idcode.get := idcode
  tapIO.jtag <> io.jtag

  tapIO.control.jtag_reset := io.jtag_reset

  //--------------------------------------------------------
  // Reset Generation (this is fed back to us by the instantiating module,
  // and is used to reset the debug registers).

  io.fsmReset := tapIO.output.reset

}
