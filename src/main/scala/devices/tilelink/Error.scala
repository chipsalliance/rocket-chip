// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.tilelink

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.subsystem.BaseSubsystem
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import scala.math.min

case class ErrorParams(address: Seq[AddressSet], maxAtomic: Int, maxTransfer: Int, acquire: Boolean = false)
{
  require (1 <= maxAtomic && maxAtomic <= maxTransfer && maxTransfer <= 4096)
}

case object ErrorParams extends Field[ErrorParams]

abstract class DevNullDevice(params: ErrorParams, beatBytes: Int = 4)
                            (device: SimpleDevice)
                            (implicit p: Parameters) extends LazyModule {
  val xfer = TransferSizes(1, params.maxTransfer)
  val atom = TransferSizes(1, params.maxAtomic)
  val node = TLManagerNode(Seq(TLManagerPortParameters(
    Seq(TLManagerParameters(
      address            = params.address,
      resources          = device.reg("mem"),
      regionType         = if (params.acquire) RegionType.TRACKED else RegionType.UNCACHEABLE,
      executable         = true,
      supportsAcquireT   = if (params.acquire) xfer else TransferSizes.none,
      supportsAcquireB   = if (params.acquire) xfer else TransferSizes.none,
      supportsGet        = xfer,
      supportsPutPartial = xfer,
      supportsPutFull    = xfer,
      supportsArithmetic = atom,
      supportsLogical    = atom,
      supportsHint       = xfer,
      fifoId             = Some(0))), // requests are handled in order
    beatBytes  = beatBytes,
    endSinkId  = 1, // can receive GrantAck
    minLatency = 1))) // no bypass needed for this device
}

/** Adds a /dev/null slave that generates TL error response messages */
class TLError(params: ErrorParams, beatBytes: Int = 4)(implicit p: Parameters)
    extends DevNullDevice(params, beatBytes)(new SimpleDevice("error-device", Seq("sifive,error0")))
{
  lazy val module = new LazyModuleImp(this) {
    import TLMessages._
    import TLPermissions._

    val (in, edge) = node.in(0)
    val a = Queue(in.a, 1)
    val da = Wire(in.d)

    val a_last = edge.last(a)
    val da_last = edge.last(da)

    a.ready := (da.ready && da_last) || !a_last
    da.valid := a.valid && a_last

    val a_opcodes = Vec(AccessAck, AccessAck, AccessAckData, AccessAckData, AccessAckData, HintAck, Grant, Grant)
    da.bits.opcode  := a_opcodes(a.bits.opcode)
    da.bits.param   := UInt(0) // toT, but error grants must be handled transiently (ie: you don't keep permissions)
    da.bits.size    := a.bits.size
    da.bits.source  := a.bits.source
    da.bits.sink    := UInt(0)
    da.bits.data    := UInt(0)
    da.bits.error   := Bool(true)

    if (params.acquire) {
      val c = Queue(in.c, 1)
      val dc = Wire(in.d)

      val c_last = edge.last(c)
      val dc_last = edge.last(dc)

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
    } else {
      in.d <> da
    }

    // We never probe or issue B requests
    in.b.valid := Bool(false)

    // Sink GrantAcks
    in.e.ready := Bool(true)
  }
}

/** Adds a /dev/null slave that does not raise ready for any incoming traffic.
  * !!! WARNING: This device WILL cause your bus to deadlock for as long as you
  *              continue to send traffic to it !!!
  */
class DeadlockDevice(params: ErrorParams, beatBytes: Int = 4)(implicit p: Parameters)
    extends DevNullDevice(params, beatBytes)(new SimpleDevice("deadlock-device", Seq("sifive,deadlock0")))
{
  lazy val module = new LazyModuleImp(this) {
    val (in, _) = node.in(0)
    in.a.ready := Bool(false)
    in.b.valid := Bool(false)
    in.c.ready := Bool(false)
    in.d.valid := Bool(false)
    in.e.ready := Bool(false)
  }
}

trait HasSystemErrorSlave { this: BaseSubsystem =>
  private val params = p(ErrorParams)
  val error = LazyModule(new TLError(params, sbus.beatBytes))
  sbus.toSlave(Some("Error")){ error.node }
}
