// See LICENSE for license details.

package uncore.tilelink2

import Chisel._

case class GPIOParams(num: Int, address: BigInt)

trait GPIOBundle
{
  val params: GPIOParams
  val gpio = UInt(width = params.num)
}

trait GPIOModule extends HasRegMap
{
  val params: GPIOParams
  val io: GPIOBundle
  val interrupts: Vec[Bool]

  val state = RegInit(UInt(0))
  val pending = RegInit(UInt(0xf, width = 4))

  io.gpio := state
  interrupts := pending.toBools

  regmap(
    0 -> Seq(
      RegField(params.num, state)),
    1 -> Seq(
      RegField.w1ToClear(4, pending, state)))
}

// Create a concrete TL2 version of the abstract GPIO slave
class TLGPIO(p: GPIOParams) extends TLRegisterRouter(p.address, 4)(
  new TLRegBundle(p, _)    with GPIOBundle)(
  new TLRegModule(p, _, _) with GPIOModule)
