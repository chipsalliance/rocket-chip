// See LICENSE.SiFive for license details.

package freechips.rocketchip.interrupts

import chisel3._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util.BlockDuringReset

/** BlockDuringReset ensures that no interrupt is raised while reset is raised. */
class IntBlockDuringReset()(implicit p: Parameters) extends LazyModule
{
  val intnode = IntAdapterNode()

  lazy val module = new LazyModuleImp(this) {
    intnode.in.zip(intnode.out).foreach { case ((in, _), (out, _)) =>
      in.zip(out).foreach { case (i, o) => o := BlockDuringReset(i) }
    }
  }
}

object IntBlockDuringReset {
  def apply()(implicit p: Parameters): IntNode = {
    val block_during_reset = LazyModule(new IntBlockDuringReset)
    block_during_reset.intnode
  }
}
