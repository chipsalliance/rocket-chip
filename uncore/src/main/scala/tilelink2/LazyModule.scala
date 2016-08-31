// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import scala.collection.mutable.ListBuffer
import chisel3.internal.sourceinfo.SourceInfo

abstract class LazyModule
{
  private val bindings = ListBuffer[() => Unit]()

  def tl(manager: TLBaseNode, client: TLBaseNode)(implicit sourceInfo: SourceInfo) = {
    bindings += manager.edge(client)
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
