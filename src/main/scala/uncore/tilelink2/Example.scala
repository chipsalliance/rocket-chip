// See LICENSE for license details.

package uncore.tilelink2

import Chisel._

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

  val state = RegInit(UInt(0))
  val pending = RegInit(UInt(0xf, width = 4))

  io.gpio := state
  interrupts := pending.toBools

  regmap(
    0 -> Seq(
      RegField(params.num, state)),
    4 -> Seq(
      RegField.w1ToClear(4, pending, state)))
}

// Create a concrete TL2 version of the abstract Example slave
class TLExample(p: ExampleParams) extends TLRegisterRouter(p.address, 4)(
  new TLRegBundle(p, _)    with ExampleBundle)(
  new TLRegModule(p, _, _) with ExampleModule)
