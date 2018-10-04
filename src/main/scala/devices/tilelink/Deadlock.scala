// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.tilelink

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

/** Adds a /dev/null slave that does not raise ready for any incoming traffic.
  * !!! WARNING: This device WILL cause your bus to deadlock for as long as you
  *              continue to send traffic to it !!!
  */
class TLDeadlock(params: DevNullParams, beatBytes: Int = 4)(implicit p: Parameters)
    extends DevNullDevice(params, beatBytes, new SimpleDevice("deadlock-device", Seq("sifive,deadlock0")))
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
