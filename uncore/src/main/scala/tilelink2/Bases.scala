// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import scala.collection.mutable.ListBuffer
import chisel3.internal.sourceinfo.SourceInfo

abstract class TLFactory
{
  private val bindings = ListBuffer[(TLBaseNode, Int, TLBaseNode, Int, SourceInfo)]()

  def tl(manager: TLBaseNode, client: TLBaseNode)(implicit sourceInfo: SourceInfo) = {
    val (i, j) = manager.edge(client)
    bindings += ((manager, i, client, j, sourceInfo))
  }

  def module: TLModule

  protected[tilelink2] def instantiate() = {
    // Find all TLFactory members of self
    for (m <- getClass.getMethods) {
      if (m.getParameterTypes.isEmpty && 
          !java.lang.reflect.Modifier.isStatic(m.getModifiers) &&
          !(m.getName contains '$') &&
          classOf[TLFactory].isAssignableFrom(m.getReturnType)) {
        // ... and force their lazy module members to exist
        m.invoke(this).asInstanceOf[TLFactory].module
      }
    }
    bindings.foreach { case (x, i, y, j, s) =>
      x.bundleIn(i).<>(y.bundleOut(j))(s)
    }
  }
}

abstract class TLModule(factory: TLFactory) extends Module
{
  override def desiredName = factory.getClass.getName.split('.').last
  factory.instantiate()
}
