// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._

class IdentityModule[T <: Data](gen: T) extends Module
{
  val io = IO(new Bundle {
    val in = Flipped(gen.cloneType)
    val out = gen.cloneType
  })

  io.out := io.in
}

object IdentityModule
{
  def apply[T <: Data](x: T): T = {
    val identity = Module(new IdentityModule(x))
    identity.io.in := x
    identity.io.out
  }
}
