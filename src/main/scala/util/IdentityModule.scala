// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._

@deprecated("moved to standalone rocketutils library", "rocketchip 2.0.0")
class IdentityModule[T <: Data](gen: T) extends Module
{
  val io = IO(new Bundle {
    val in = Flipped(gen.cloneType)
    val out = gen.cloneType
  })

  io.out := io.in
}

@deprecated("moved to standalone rocketutils library", "rocketchip 2.0.0")
object IdentityModule
{
  def apply[T <: Data](x: T): T = {
    val identity = Module(new IdentityModule(x))
    identity.io.in := x
    identity.io.out
  }
}
