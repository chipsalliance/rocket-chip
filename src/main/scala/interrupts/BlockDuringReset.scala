// See LICENSE.SiFive for license details.

package freechips.rocketchip.interrupts

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.util.BlockDuringReset

/** BlockDuringReset ensures that no interrupt is raised while reset is raised. */
class IntBlockDuringReset(stretchResetCycles: Int = 0)(implicit p: Parameters) extends LazyModule
{
  val intnode = IntAdapterNode()
  override def shouldBeInlined = true
  lazy val module = new LazyModuleImp(this) {
    intnode.in.zip(intnode.out).foreach { case ((in, _), (out, _)) =>
      in.zip(out).foreach { case (i, o) => o := BlockDuringReset(i, stretchResetCycles) }
    }
  }
}

object IntBlockDuringReset {
  def apply(stretchResetCycles: Int = 0)(implicit p: Parameters): IntNode = {
    val block_during_reset = LazyModule(new IntBlockDuringReset(stretchResetCycles))
    block_during_reset.intnode
  }
}
