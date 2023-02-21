// See LICENSE.SiFive for license details.

package freechips.rocketchip.examples

import chisel3._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.amba.ahb.HasAHBControlRegMap
import freechips.rocketchip.amba.axi4.HasAXI4ControlRegMap
import freechips.rocketchip.diplomacy.LazyModuleImp
import freechips.rocketchip.interrupts.HasInterruptSources
import freechips.rocketchip.tilelink.HasTLControlRegMap
import freechips.rocketchip.regmapper.{IORegisterRouter, RegisterRouterParams, RegField, RegFieldDesc}

case class ExampleDeviceParams(num: Int, address: BigInt)

class ExampleDeviceIOBundle(val params: ExampleDeviceParams) extends Bundle {
  val gpio = UInt(params.num.W)
}

/** Base class for the ExampleDevice containing its diplomatic+DTS metadata as well as
  * its concrete chisel implementation and register map.
  */
abstract class ExampleDevice(params: ExampleDeviceParams)(implicit p: Parameters)
  extends IORegisterRouter(
    RegisterRouterParams(
      name = "somedev",
      compat = Seq("ucbbar,random-interface"),
      base = params.address, 
      beatBytes = 4),
    new ExampleDeviceIOBundle(params))
  with HasInterruptSources
{
  def nInterrupts = 4

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {

    val state = RegInit(0.U(params.num.W))
    val pending = RegInit(0xf.U(nInterrupts.W))

    port.gpio := state
    interrupts := pending.asBools

    regmap(
      0 -> Seq(
        RegField(params.num, state,
          RegFieldDesc("state", "State: Example of a R/W Register with description.", reset = Some(0)))),
      4 -> Seq(
        RegField.w1ToClear(nInterrupts, pending, state,
          Some(RegFieldDesc("pending", "Pending: Example of a special (W1ToC) Register. " +
            "Writing a bit here causes it to be reset to 0. " +
            "The bits are set when the corresponding bit in 'state' is high.",
            reset=Some(0xF), volatile=true))))
    )
  }
}

/** Specialize the generic ExampleDevice to make it attachable to a TileLink interconnect. */
class TLExampleDevice(params: ExampleDeviceParams)(implicit p: Parameters)
  extends ExampleDevice(params) with HasTLControlRegMap

/** Specialize the generic ExampleDevice to make it attachable to an AXI4 interconnect. */
class AXI4ExampleDevice(params: ExampleDeviceParams)(implicit p: Parameters)
  extends ExampleDevice(params) with HasAXI4ControlRegMap

/** Specialize the generic ExampleDevice to make it attachable to an AHB interconnect. */
class AHBExampleDevice(params: ExampleDeviceParams)(implicit p: Parameters)
  extends ExampleDevice(params) with HasAHBControlRegMap
