// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.tilelink

import Chisel._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.coreplex.HasPeripheryBus
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import scala.math.min

case class ErrorParams(address: Seq[AddressSet])
case object ErrorParams extends Field[ErrorParams]

/** Adds a /dev/null slave that generates TL error response messages */
class TLError(params: ErrorParams, beatBytes: Int = 4)(implicit p: Parameters) extends LazyModule
{
  val address = params.address

  val device = new SimpleDevice("error-device", Seq("sifive,error0"))

  val node = TLManagerNode(Seq(TLManagerPortParameters(
    Seq(TLManagerParameters(
      address            = address,
      resources          = device.reg("mem"),
      supportsGet        = TransferSizes(1, beatBytes),
      supportsPutPartial = TransferSizes(1, beatBytes),
      supportsPutFull    = TransferSizes(1, beatBytes),
      supportsArithmetic = TransferSizes(1, beatBytes),
      supportsLogical    = TransferSizes(1, beatBytes),
      supportsHint       = TransferSizes(1, beatBytes),
      fifoId             = Some(0))), // requests are handled in order
    beatBytes  = beatBytes,
    minLatency = 1))) // no bypass needed for this device

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in = node.bundleIn
    }

    import TLMessages._
    val opcodes = Vec(AccessAck, AccessAck, AccessAckData, AccessAckData, AccessAckData, HintAck)

    val in = io.in(0)
    val a = Queue(in.a, 1)
    val d = in.d

    a.ready := d.ready
    d.valid := a.valid
    d.bits.opcode  := opcodes(a.bits.opcode)
    d.bits.param   := UInt(0)
    d.bits.size    := a.bits.size
    d.bits.source  := a.bits.source
    d.bits.sink    := UInt(0)
    d.bits.data    := UInt(0)
    d.bits.error   := a.bits.opcode =/= Hint // Hints may not error

    // Tie off unused channels
    in.b.valid := Bool(false)
    in.c.ready := Bool(true)
    in.e.ready := Bool(true)
  }
}

trait HasPeripheryErrorSlave extends HasPeripheryBus {
  private val params = p(ErrorParams)
  private val maxXfer = min(params.address.map(_.alignment).max.toInt, 4096)
  val error = LazyModule(new TLError(params, pbus.beatBytes))

  // Most slaves do not support a 4kB burst so this slave ends up with many more source bits than others;
  // we exclude the onerously large TLMonitor that results.
  error.node connectButDontMonitor pbus.toLargeBurstSlave(maxXfer)
}
