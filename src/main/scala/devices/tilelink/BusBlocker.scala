// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.tilelink

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

/** BasicBusBlocker uses a single bit register to control whether
  * accesses of all types are allowed to proceed or bypassed to
  * a /dev/null device. It has a second bit register to report
  * whether any requests are pending on either path.
  */

case class BasicBusBlockerParams(
  controlAddress:   BigInt,
  controlBeatBytes: Int,
  deviceBeatBytes:  Int,
  deadlock: Boolean = false)

class BasicBusBlocker(params: BasicBusBlockerParams)(implicit p: Parameters)
    extends TLBusBypassBase(params.deviceBeatBytes, params.deadlock)
{
  val device = new SimpleDevice("basic-bus-blocker", Seq("sifive,basic-bus-blocker0"))

  val controlNode = TLRegisterNode(
    address   = Seq(AddressSet(params.controlAddress, 0xFFF)),
    device    = device,
    beatBytes = params.controlBeatBytes)

  lazy val module = new LazyModuleImp(this) {
    val allow = RegInit(true.B)
    val pending = RegNext(bar.module.io.pending)

    controlNode.regmap(
      0 -> Seq(RegField  (32, allow)),
      4 -> Seq(RegField.r(32, pending)))

    bar.module.io.bypass := !allow
  }
}
