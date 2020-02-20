// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.tilelink

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

/** This device extends a basic bus blocker by allowing it to gate the clocks of the device
  * whose tilelink port is being blocked. For now it is only possible to block
  * a single TL port and all the clocks simultaneously.
  */

class TLClockBlocker(params: BasicBusBlockerParams)(implicit p: Parameters)
    extends TLBusBypassBase(params.deviceBeatBytes, params.deadlock)
{
  val device = new SimpleDevice("clock-blocker", Seq("sifive,clock-blocker0"))

  val controlNode = TLRegisterNode(
    address   = Seq(AddressSet(params.controlAddress, 0xFFF)),
    device    = device,
    beatBytes = params.controlBeatBytes)

  val clockNode = ClockAdapterNode()

  lazy val module = new LazyModuleImp(this) {
    val allow = RegInit(true.B)
    val pending = RegNext(bar.module.io.pending)

    controlNode.regmap(
      0 -> Seq(RegField  (32, allow,
        RegFieldDesc("allow",
          "Used to enable/disable bus transactions", reset=Some(1)))),
      4 -> Seq(RegField.r(32, pending, RegFieldDesc("pending",
        "Indicates if bus transactions are in-flight", volatile=true)))
    )

    bar.module.io.bypass := !allow

    val (clock_in, _) = clockNode.in.unzip
    val (clock_out, _) = clockNode.out.unzip

    (clock_in zip clock_out) foreach { case (i, o) =>
      o.clock := ClockGate(i.clock, allow || pending)
      o.reset := i.reset
    }
  }
}
