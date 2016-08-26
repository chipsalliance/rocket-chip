// See LICENSE for license details.

package uncore.tilelink2

import Chisel._

case class GPIOParams(num: Int, address: Option[BigInt] = None)

trait GPIOBundle
{
  val params: GPIOParams
  val gpio = UInt(width = params.num)
}

trait GPIOModule extends HasRegMap
{
  val params: GPIOParams
  val io: GPIOBundle

  val state = RegInit(UInt(0))
  io.gpio := state

  regmap(0 -> Seq(RegField(params.num, state)))
}

// Create a concrete TL2 version of the abstract GPIO slave
class TLGPIO(p: GPIOParams) extends TLRegisterRouter(p.address)(
  new TLRegBundle(p, _)    with GPIOBundle)(
  new TLRegModule(p, _, _) with GPIOModule)
