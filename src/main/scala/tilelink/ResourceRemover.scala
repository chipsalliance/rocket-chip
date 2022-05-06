// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._


class TLResourceRemover(implicit p: Parameters) extends LazyModule
{
  val node = new TLAdapterNode(
    clientFn = { s => s },
    managerFn = { s => s.v2copy(slaves=s.slaves.map(_.copy(resources=Nil))) }
  )

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, _), (out, _)) =>
      out <> in
    }
  }
}

object TLResourceRemover
{
  def apply()(implicit p: Parameters): TLNode = {
    val resource_remover = LazyModule(new TLResourceRemover)
    resource_remover.node
  }
}
