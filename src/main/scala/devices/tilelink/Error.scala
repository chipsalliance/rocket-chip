// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.tilelink

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.coreplex.HasSystemBus
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import scala.math.min

case class ErrorParams(address: Seq[AddressSet], maxTransfer: Int = 4096)
case object ErrorParams extends Field[ErrorParams]

/** Adds a /dev/null slave that generates TL error response messages */
class TLError(params: ErrorParams, beatBytes: Int = 4)(implicit p: Parameters) extends LazyModule
{
  val address = params.address

  val device = new SimpleDevice("error-device", Seq("sifive,error0"))

  val xfer = TransferSizes(1, params.maxTransfer)
  val node = TLManagerNode(Seq(TLManagerPortParameters(
    Seq(TLManagerParameters(
      address            = address,
      resources          = device.reg("mem"),
      regionType         = RegionType.UNCACHED,
      supportsAcquireT   = xfer,
      supportsAcquireB   = xfer,
      supportsGet        = xfer,
      supportsPutPartial = xfer,
      supportsPutFull    = xfer,
      supportsArithmetic = xfer,
      supportsLogical    = xfer,
      supportsHint       = xfer,
      fifoId             = Some(0))), // requests are handled in order
    beatBytes  = beatBytes,
    endSinkId  = 1, // can receive GrantAck
    minLatency = 1))) // no bypass needed for this device

  lazy val module = new LazyModuleImp(this) {
    import TLMessages._
    import TLPermissions._

    val (in, edge) = node.in(0)
    val a = Queue(in.a, 1)
    val c = Queue(in.c, 1)
    val da = Wire(in.d)
    val dc = Wire(in.d)

    val a_last = edge.last(a)
    val c_last = edge.last(c)
    val da_last = edge.last(da)
    val dc_last = edge.last(dc)

    a.ready := (da.ready && da_last) || !a_last
    da.valid := a.valid && a_last

    val a_opcodes = Vec(AccessAck, AccessAck, AccessAckData, AccessAckData, AccessAckData, HintAck, Grant)
    da.bits.opcode  := a_opcodes(a.bits.opcode)
    da.bits.param   := UInt(0) // toT, but error grants must be handled transiently (ie: you don't keep permissions)
    da.bits.size    := a.bits.size
    da.bits.source  := a.bits.source
    da.bits.sink    := UInt(0)
    da.bits.data    := UInt(0)
    da.bits.error   := Bool(true)

    c.ready := (dc.ready && dc_last) || !c_last
    dc.valid := c.valid && c_last

    dc.bits.opcode := ReleaseAck
    dc.bits.param  := Vec(toB, toN, toN)(c.bits.param)
    dc.bits.size   := c.bits.size
    dc.bits.source := c.bits.source
    dc.bits.sink   := UInt(0)
    dc.bits.data   := UInt(0)
    dc.bits.error  := Bool(true)

    // Combine response channels
    TLArbiter.lowest(edge, in.d, dc, da)

    // We never probe or issue B requests; we are UNCACHED
    in.b.valid := Bool(false)

    // Sink GrantAcks
    in.e.ready := Bool(true)
  }
}

trait HasSystemErrorSlave extends HasSystemBus {
  private val params = p(ErrorParams)
  val error = LazyModule(new TLError(params, sbus.beatBytes))

  error.node := sbus.toSlave
}
