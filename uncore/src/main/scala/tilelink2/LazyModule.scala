// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import scala.collection.mutable.ListBuffer
import chisel3.internal.sourceinfo.SourceInfo

abstract class LazyModule
{
  private val bindings = ListBuffer[() => Unit]()
  private val extraChildren = ListBuffer[LazyModule]()

  // Use as: connect(source -> sink, source2 -> sink2, ...)
  def connect[PO, PI, EO, EI, B <: Bundle](edges: (BaseNode[PO, PI, EO, EI, B], BaseNode[PO, PI, EO, EI, B])*)(implicit sourceInfo: SourceInfo) = {
    edges.foreach { case (source, sink) =>
      bindings += (source edge sink)
    }
  }

  def module: LazyModuleImp

  protected[tilelink2] def instantiate() = {
    // Find all LazyModule members of self
    for (m <- getClass.getMethods) {
      if (m.getParameterTypes.isEmpty && 
          !java.lang.reflect.Modifier.isStatic(m.getModifiers) &&
          !(m.getName contains '$') &&
          !(m.getName == "lazyModule") &&
          classOf[LazyModule].isAssignableFrom(m.getReturnType)) {
        // ... and force their lazy module members to exist
        m.invoke(this).asInstanceOf[LazyModule].module
      }
    }
    extraChildren.foreach { _.module }
    bindings.foreach { f => f () }
  }

  implicit val lazyModule = this
  def addChild(x: LazyModule) = extraChildren += x
}

abstract class LazyModuleImp(outer: LazyModule) extends Module
{
  override def desiredName = outer.getClass.getName.split('.').last
  outer.instantiate()
}
