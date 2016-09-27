package rocketchip

import Chisel._
import uncore.devices.{DebugBusIO, AsyncDebugBusTo, AsyncDebugBusFrom, DebugBusReq, DebugBusResp, DMKey}
import junctions._
import cde.{Parameters, Field}

case object IncludeJtagDTM extends Field[Boolean]

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
 *  reset the Debug Transport Module and the
 *  DTM side of the synchronizer.
 *  This design requires that TRST be
 *  synchronized to TCK (for de-assert) outside
 *  of this module. Your top level code should ensure
 *  that TRST is asserted before the rocket-chip core
 *  comes out of reset.
 *  Note that TRST is an optional
 *  part of the JTAG protocol, but it is not
 *  optional for interfacing with this logic.
 * 
 */

class JtagDTMWithSync(depth: Int = 1, sync: Int = 3)(implicit val p: Parameters)
    extends Module {

  // io.DebugBusIO <-> Sync <-> DebugBusIO <-> UInt <-> DTM Black Box

  val io = new Bundle {

    val jtag = new JTAGIO(true).flip()
    val debug = new DebugBusIO()(p)

  }

  val req_width = io.debug.req.bits.getWidth
  val resp_width = io.debug.resp.bits.getWidth

  val jtag_dtm = Module (new DebugTransportModuleJtag(req_width, resp_width))

  jtag_dtm.io.jtag := io.jtag

  val dtm_req = Wire(new DecoupledIO(UInt(width = req_width)))
  val dtm_resp = Wire(new DecoupledIO(UInt(width = resp_width)))

  val io_debug_bus = Wire (new DebugBusIO)

  io.debug <> AsyncDebugBusFrom(io.jtag.TCK, io.jtag.TRST, io_debug_bus, depth, sync)

  // Translate from straight 'bits' interface of the blackboxes
  // into the Resp/Req data structures.
  io_debug_bus.req.valid  := dtm_req.valid
  io_debug_bus.req.bits   := new DebugBusReq(p(DMKey).nDebugBusAddrSize).fromBits(dtm_req.bits)
  dtm_req.ready := io_debug_bus.req.ready

  dtm_resp.valid := io_debug_bus.resp.valid
  dtm_resp.bits  := io_debug_bus.resp.bits.asUInt
  io_debug_bus.resp.ready  := dtm_resp.ready

  dtm_req := jtag_dtm.io.dtm_req
  jtag_dtm.io.dtm_resp := dtm_resp
}

class DebugTransportModuleJtag(reqSize : Int, respSize : Int)(implicit val p: Parameters)  extends BlackBox {

  val io = new Bundle {
    val jtag = new JTAGIO(true).flip()

    val dtm_req = new DecoupledIO(UInt(width = reqSize))

    val dtm_resp = new DecoupledIO(UInt(width = respSize)).flip()

  }

}

class AsyncMailbox extends BlackBox {

  // This Verilog module is parameterized, but until this is supported by Chisel,
  // this mailbox just has a fixed width of 64 bits, which is enough
  // for our specific purpose here.

  val io = new CrossingIO(UInt(width=64))
}
