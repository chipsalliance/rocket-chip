// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import chisel3.internal.sourceinfo.{SourceInfo, SourceLine, UnlocatableSourceInfo}

abstract class LazyModule
{
  protected[tilelink2] var bindings = List[() => Unit]()
  protected[tilelink2] var children = List[LazyModule]()
  protected[tilelink2] var nodes = List[RootNode]()
  protected[tilelink2] var info: SourceInfo = UnlocatableSourceInfo
  protected[tilelink2] val parent = LazyModule.stack.headOption

  LazyModule.stack = this :: LazyModule.stack
  parent.foreach(p => p.children = this :: p.children)

  // Use as: connect(source -> sink, source2 -> sink2, ...)
  def connect[PO, PI, EO, EI, B <: Data](edges: (BaseNode[PO, PI, EO, EI, B], BaseNode[PO, PI, EO, EI, B])*)(implicit sourceInfo: SourceInfo) = {
    edges.foreach { case (source, sink) => sink := source }
  }

  def name = getClass.getName.split('.').last
  def line = sourceLine(info)

  def module: LazyModuleImp

  protected[tilelink2] def instantiate() = {
    children.reverse.foreach { c => 
      // !!! fix chisel3 so we can pass the desired sourceInfo
      // implicit val sourceInfo = c.module.outer.info
      Module(c.module)
    }
    bindings.reverse.foreach { f => f () }
  }
}

object LazyModule
{
  protected[tilelink2] var stack = List[LazyModule]()

  def apply[T <: LazyModule](bc: T)(implicit sourceInfo: SourceInfo): T = {
    // Make sure the user put LazyModule around modules in the correct order
    // If this require fails, probably some grandchild was missing a LazyModule
    // ... or you applied LazyModule twice
    require (!stack.isEmpty, s"LazyModule() applied to ${bc.name} twice ${sourceLine(sourceInfo)}")
    require (stack.head eq bc, s"LazyModule() applied to ${bc.name} before ${stack.head.name} ${sourceLine(sourceInfo)}")
    stack = stack.tail
    bc.info = sourceInfo
    bc
  }
}

abstract class LazyModuleImp(outer: LazyModule) extends Module
{
  // .module had better not be accessed while LazyModules are still being built!
  require (LazyModule.stack.isEmpty, s"${outer.name}.module was constructed before LazyModule() was run on ${LazyModule.stack.head.name}")

  override def desiredName = outer.name
  outer.instantiate()
}
