// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.regmapper._

case class ExampleParams(num: Int, address: BigInt)

trait ExampleBundle
{
  val params: ExampleParams
  val gpio = UInt(width = params.num)
}

trait ExampleModule extends HasRegMap
{
  val params: ExampleParams
  val io: ExampleBundle
  val interrupts: Vec[Bool]

  val state = RegInit(UInt(0, width = params.num))
  val pending = RegInit(UInt(0xf, width = 4))

  io.gpio := state
  interrupts := pending.asBools

  regmap(
    0 -> Seq(
      RegField(params.num, state,
        RegFieldDesc("state", "State: Example of a R/W Register with description.", reset = Some(0)))),
    4 -> Seq(
      RegField.w1ToClear(4, pending, state,
        Some(RegFieldDesc("pending", "Pending: Example of a special (W1ToC) Register. " +
          "Writing a bit here causes it to be reset to 0. " +
          "The bits are set when the corresponding bit in 'state' is high.",
          reset=Some(0xF), volatile=true))))
  )
}

// Create a concrete TL2 version of the abstract Example slave
class TLExample(params: ExampleParams)(implicit p: Parameters)
  extends TLRegisterRouter(params.address, "somedev", Seq("ucbbar,random-interface"), 4)(
  new TLRegBundle(params, _)    with ExampleBundle)(
  new TLRegModule(params, _, _) with ExampleModule)
