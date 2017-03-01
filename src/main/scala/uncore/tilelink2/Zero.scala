// See LICENSE.SiFive for license details.

package uncore.tilelink2

import Chisel._
import config._
import diplomacy._

class TLZero(address: AddressSet, executable: Boolean = true, beatBytes: Int = 4)(implicit p: Parameters) extends LazyModule
{
  val device = new SimpleDevice("rom", Seq("ucbbar,cacheable-zero"))

  val node = TLManagerNode(Seq(TLManagerPortParameters(
    Seq(TLManagerParameters(
      address            = List(address),
      resources          = device.reg,
      regionType         = RegionType.UNCACHED,
      executable         = executable,
      supportsGet        = TransferSizes(1, beatBytes),
      supportsPutPartial = TransferSizes(1, beatBytes),
      supportsPutFull    = TransferSizes(1, beatBytes),
      fifoId             = Some(0))), // requests are handled in order
    beatBytes  = beatBytes,
    minLatency = 1))) // no bypass needed for this device

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val in = node.bundleIn
    }

    val in = io.in(0)
    val edge = node.edgesIn(0)

    val a = Queue(in.a, 2)
    val hasData = edge.hasData(a.bits)

    a.ready := in.d.ready
    in.d.valid := a.valid
    in.d.bits := edge.AccessAck(a.bits, UInt(0))
    in.d.bits.opcode := Mux(hasData, TLMessages.AccessAck, TLMessages.AccessAckData)

    // Tie off unused channels
    in.b.valid := Bool(false)
    in.c.ready := Bool(true)
    in.e.ready := Bool(true)
  }
}
