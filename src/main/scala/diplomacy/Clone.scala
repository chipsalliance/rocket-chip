// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomacy

import Chisel._
import chisel3.shim.CloneModule
import chisel3.internal.sourceinfo.{SourceInfo}


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
  /** The old API **/
  def apply(base: LazyModule) = new CloneLazyModule(base)


  /** Constructs a [[LazyModule]], but replaces its [[LazyModuleImp]] with a cloned [[LazyModuleImp]]
    * from another source. The user of [[CloneLazyModule]] must be careful to guarantee that
    * bc and cloneProto have equivalent [[LazyModuleImp]]'s.
    *
    * @param bc         [[LazyModule]] instance to wrap, this instance will not evaluate its own [[LazyModuleImp]]
    * @param cloneProto [[LazyModule]] instance which will provide the [[LazyModuleImp]] implementation for bc
    */
  def apply[A <: LazyModule, B <: LazyModule](bc: A, cloneProto: B)(implicit valName: ValName, sourceInfo: SourceInfo): A = {
    require(LazyModule.scope.isDefined, s"CloneLazyModule ${bc.name} ${sourceLine(sourceInfo)} can only exist as the child of a parent LazyModule")
    LazyModule(bc)
    bc.cloneProto = Some(cloneProto)
    bc
  }
}
