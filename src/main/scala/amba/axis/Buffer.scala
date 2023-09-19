// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axis

import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._

class AXISBuffer(val params: BufferParams)(implicit p: Parameters) extends LazyModule
{
  val node = AXISAdapterNode()
  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out.waiveAs[chisel3.Data]() :<>= params.irrevocable(in).waiveAs[chisel3.Data]()
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
