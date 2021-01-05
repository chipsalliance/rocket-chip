// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axis

import diplomacy._
import diplomacy.config._
import freechips.rocketchip.diplomacy.BufferParams
import freechips.rocketchip.util._

class AXISBuffer(val params: BufferParams)(implicit p: Parameters) extends LazyModule
{
  val node = AXISAdapterNode()
  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out :<>: params.irrevocable(in)
    }
  }
}

object AXISBuffer
{
  def apply(params: BufferParams = BufferParams.default)(implicit p: Parameters) = {
    val buffer = LazyModule(new AXISBuffer(params))
    buffer.node
  }
}
