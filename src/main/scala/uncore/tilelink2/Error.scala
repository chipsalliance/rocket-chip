// See LICENSE.SiFive for license details.

package uncore.tilelink2

import Chisel._
import config._
import diplomacy._
import util._

class TLError(address: Seq[AddressSet], beatBytes: Int = 4)(implicit p: Parameters) extends LazyModule
{
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
    d.bits.addr_lo := a.bits.address
    d.bits.data    := UInt(0)
    d.bits.error   := a.bits.opcode =/= Hint // Hints may not error

    // Tie off unused channels
    in.b.valid := Bool(false)
    in.c.ready := Bool(true)
    in.e.ready := Bool(true)
  }
}
