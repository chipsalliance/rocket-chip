// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.tilelink

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink.TLMessages

/** This /dev/null device accepts single beat gets/puts, as well as atomics.
  * Response data is always 0. Reequests to write data have no effect.
  */
class TLZero(address: AddressSet, beatBytes: Int = 4)(implicit p: Parameters)
  extends DevNullDevice(
    params = DevNullParams(
      address = List(address),
      maxAtomic = beatBytes,
      maxTransfer = beatBytes,
      region = RegionType.UNCACHED,
      executable = true,
      mayDenyGet = false,
      mayDenyPut = false),
    beatBytes = beatBytes,
    device = new SimpleDevice("rom", Seq("ucbbar,cacheable-zero0"))) {
  lazy val module = new LazyModuleImp(this) {
    val (in, edge) = node.in(0)

    val a = Queue(in.a, 2)

    a.ready := in.d.ready
    in.d.valid := a.valid
    in.d.bits := edge.AccessAck(a.bits)
    in.d.bits.opcode := TLMessages.adResponse(edge.opcode(a.bits))

    // Tie off unused channels
    in.b.valid := Bool(false)
    in.c.ready := Bool(true)
    in.e.ready := Bool(true)
  }
}
