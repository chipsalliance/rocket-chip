// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import scala.collection.mutable.ListBuffer
import chisel3.internal.sourceinfo.SourceInfo

abstract class LazyModule
{
  private val bindings = ListBuffer[() => Unit]()

  // Use as: connect(source -> sink, source2 -> sink2, ...)
  def connect(edges: (TLBaseNode, TLBaseNode)*)(implicit sourceInfo: SourceInfo) = {
    edges.foreach { case (source, sink) =>
      bindings += sink.edge(source)
    }
  }

  def module: LazyModuleImp

  protected[tilelink2] def instantiate() = {
    // Find all LazyModule members of self
    for (m <- getClass.getMethods) {
      if (m.getParameterTypes.isEmpty && 
          !java.lang.reflect.Modifier.isStatic(m.getModifiers) &&
          !(m.getName contains '$') &&
          classOf[LazyModule].isAssignableFrom(m.getReturnType)) {
        // ... and force their lazy module members to exist
        m.invoke(this).asInstanceOf[LazyModule].module
      }
    }
    bindings.foreach { f => f () }
  }
}

abstract class LazyModuleImp(outer: LazyModule) extends Module
{
  override def desiredName = outer.getClass.getName.split('.').last
  outer.instantiate()
}
