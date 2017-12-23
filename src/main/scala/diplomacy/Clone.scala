// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomacy

import Chisel._
import chisel3.shim.CloneModule

final class CloneLazyModule private (val base: LazyModule)
{
  // Pay special attention to the .iParams and .oParams of the node, which
  // indicate the parameters a stand-in master must supply.
  def clone[DI, UI, EI, BI <: Data, DO, UO, EO, BO <: Data](node: NodeHandle[DI, UI, EI, BI, DO, UO, EO, BO])(implicit valName: ValName) =
    new MixedTestNode(node, this)

  protected[diplomacy] lazy val io = CloneModule(base.module)
}

object CloneLazyModule
{
  def apply(base: LazyModule) = new CloneLazyModule(base)
}
