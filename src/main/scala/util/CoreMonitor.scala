// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._

// this bundle is used to expose some internal core signals
// to verification monitors which sample instruction commits
class CoreMonitorBundle(val xLen: Int) extends Bundle {
  val hartid = UInt(width = xLen.W)
  val timer = UInt(width = 32.W)
  val valid = Bool()
  val pc = UInt(width = xLen.W)
  val wrdst = UInt(width = 5.W)
  val wrdata = UInt(width = xLen.W)
  val wren = Bool()
  val rd0src = UInt(width = 5.W)
  val rd0val = UInt(width = xLen.W)
  val rd1src = UInt(width = 5.W)
  val rd1val = UInt(width = xLen.W)
  val inst = UInt(width = 32.W)
}

// mark a module that has cores with CoreMonitorBundles
trait HasCoreMonitorBundles {
    def coreMonitorBundles: List[CoreMonitorBundle]
}
