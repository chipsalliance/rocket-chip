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
 *  wrapper around a number of black-boxed
 *  modules. The black-boxing is due to the fact that
 *  Chisel doesn't currently support:
 *    - Negative Edge Clocking
 *    - Asynchronous Resets
 *   (The tristate requirements of JTAG are exported from the 
 *    Chisel domain with the DRV_TDO signal).
 *  
 *  The AsyncDebugBus parameter here is overloaded.
 *  The DebugTransportModule JTAG definately needs a synchronizer,
 *  the parameter just currently selects whether the Chisel-generated
 *  crossing is used or the black-boxed crossing is used.
 *  Although Top is also capable of generating the 
 *  Chisel sychnronizers, it is done here for consistency
 *  of keeping the synchronizers in one place when
 *  instantiating the DTM.
 * 
 */

class JtagDTMWithSync(clockSignal: Clock = null, resetSignal: Bool = null)
    (implicit val p: Parameters) extends Module(Option(clockSignal), Option(resetSignal)) {

  // IO <-> [Chisel Sync?] <-> [DebugBusIO<->UInt] <-> [Black Box Sync?] <-> DTM Black Box

  val io = new Bundle {

    val jtag = new JtagIO(true).flip()
    val debug = new DebugBusIO()(p)

  }

  val req_width = io.debug.req.bits.getWidth
  val resp_width = io.debug.resp.bits.getWidth

  val jtag_dtm = Module (new DebugTransportModuleJtag(req_width, resp_width))

  jtag_dtm.io.jtag <> io.jtag

  val dtm_req = Wire(new DecoupledIO(UInt(width = req_width)))
  val dtm_resp = Wire(new DecoupledIO(UInt(width = resp_width)))

  val io_debug_bus = Wire (new DebugBusIO)

  // Optionally instantiate the Chisel synchronizers.
  // These go on this side of the DebugBusIO->UInt translation
  // because the Chisel synchronizers understand these data structures.
  if (p(AsyncDebugBus)){
    io.debug <> AsyncDebugBusFrom(io.jtag.TCK, io.jtag.TRST, io_debug_bus)
  } else {
    io.debug <> io_debug_bus
  }

  // Translate from straight 'bits' interface of the blackboxes
  // into the Resp/Req data structures.
  io_debug_bus.req.valid  := dtm_req.valid
  io_debug_bus.req.bits   := new DebugBusReq(p(DMKey).nDebugBusAddrSize).fromBits(dtm_req.bits)
  dtm_req.ready := io_debug_bus.req.ready

  dtm_resp.valid := io_debug_bus.resp.valid
  dtm_resp.bits  := io_debug_bus.resp.bits.asUInt
  io_debug_bus.resp.ready  := dtm_resp.ready

  // Optionally instantiate the black-box synchronizers
  // instead of the chisel ones.
  // These go on this side of the DebugBusIO->UInt translation
  // because they do not understand the DebugBusIO data structures.

  if (p(AsyncDebugBus)) {
    dtm_req <> jtag_dtm.io.dtm_req 
    jtag_dtm.io.dtm_resp <> dtm_resp
  } else {
    val req_sync  = Module (new AsyncMailbox())
    val resp_sync = Module (new AsyncMailbox())
    req_sync.io.enq             := jtag_dtm.io.dtm_req
    req_sync.io.enq_clock.get   := io.jtag.TCK
    req_sync.io.enq_reset.get   := io.jtag.TRST
    req_sync.io.deq_clock.get   := clock
    req_sync.io.deq_reset.get   := reset
    dtm_req                     := req_sync.io.deq

    jtag_dtm.io.dtm_resp        := resp_sync.io.deq
    resp_sync.io.deq_clock.get  := io.jtag.TCK
    resp_sync.io.deq_reset.get  := io.jtag.TRST
    resp_sync.io.enq_clock.get  := clock
    resp_sync.io.enq_reset.get  := reset
    resp_sync.io.enq            := dtm_resp
  }
}

class DebugTransportModuleJtag(reqSize : Int, respSize : Int)(implicit val p: Parameters)  extends BlackBox {

  val io = new Bundle {
    val jtag = new JtagIO(true).flip()

    val dtm_req = new DecoupledIO(UInt(width = reqSize))

    val dtm_resp = new DecoupledIO(UInt(width = respSize)).flip()

  }

}

class AsyncMailbox extends BlackBox {

  // This Verilog module is parameterized, but until this is supported by Chisel,
  // this mailbox just has a fixed width of 64 bits, which is enough
  // for our specific purpose here.

  val io = new Crossing(UInt(width=64), true, true)

}
