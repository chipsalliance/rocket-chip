// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.tilelink

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

/** Adds a /dev/null slave that generates TL error response messages */
class TLError(params: DevNullParams, buffer: Boolean = true, beatBytes: Int = 4)(implicit p: Parameters)
    extends DevNullDevice(params,
      minLatency = if (buffer) 1 else 0,
      beatBytes, new SimpleDevice("error-device", Seq("sifive,error0")))
{
  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    import TLMessages._
    import TLPermissions._

    val (in, edge) = node.in(0)
    val a = if (buffer) {Queue(in.a, 1)} else in.a

    val da = Wire(chiselTypeOf(in.d))
    val idle = RegInit(true.B)

    val a_last = edge.last(a)
    val (da_first, da_last, _) = edge.firstlast(da)

    assert (idle || da_first) // we only send Grant, never GrantData => simplified flow control below
    a.ready := (da.ready && da_last && idle) || !a_last
    da.valid := a.valid && a_last && idle

    da.bits.opcode  := TLMessages.adResponse(a.bits.opcode)
    da.bits.param   := 0.U // toT, but error grants must be handled transiently (ie: you don't keep permissions)
    da.bits.size    := a.bits.size
    da.bits.source  := a.bits.source
    da.bits.sink    := 0.U
    da.bits.denied  := true.B
    da.bits.data    := 0.U
    da.bits.corrupt := edge.hasData(da.bits)

    if (params.acquire) {
      val c = if (buffer) {Queue(in.c, 1)} else in.c
      val dc = Wire(chiselTypeOf(in.d))

      val c_last = edge.last(c)
      val dc_last = edge.last(dc)

      // Only allow one Grant in-flight at a time
      when (da.fire && da.bits.opcode === Grant) { idle := false.B }
      when (in.e.fire) { idle := true.B }

      c.ready := (dc.ready && dc_last) || !c_last
      dc.valid := c.valid && c_last

      // ReleaseAck is not allowed to report failure
      dc.bits.opcode  := ReleaseAck
      dc.bits.param   := VecInit(toB, toN, toN)(c.bits.param)
      dc.bits.size    := c.bits.size
      dc.bits.source  := c.bits.source
      dc.bits.sink    := 0.U
      dc.bits.denied  := false.B
      dc.bits.data    := 0.U
      dc.bits.corrupt := false.B

      // Combine response channels
      TLArbiter.lowest(edge, in.d, dc, da)
    } else {
      in.d <> da
    }

    // We never probe or issue B requests
    in.b.valid := false.B

    // Sink GrantAcks
    in.e.ready := true.B
  }
}
