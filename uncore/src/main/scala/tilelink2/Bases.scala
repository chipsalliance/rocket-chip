// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import scala.collection.mutable.ListBuffer
import chisel3.internal.sourceinfo.SourceInfo

abstract class LazyModule
{
  private val bindings = ListBuffer[(TLBaseNode, Int, TLBaseNode, Int, SourceInfo)]()

  def tl(manager: TLBaseNode, client: TLBaseNode)(implicit sourceInfo: SourceInfo) = {
    val (i, j) = manager.edge(client)
    bindings += ((manager, i, client, j, sourceInfo))
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
    bindings.foreach { case (x, i, y, j, s) =>
      val in  = x.connectIn(i)
      val out = y.connectOut(j)
      TLMonitor.legalize(out, y.edgesOut(j), in, x.edgesIn(i), s)
      in.<>(out)(s)
      val mask = ~UInt(x.edgesIn(i).manager.beatBytes - 1)
      in .a.bits.address.:=(mask & out.a.bits.address)(s)
      out.b.bits.address.:=(mask & in .b.bits.address)(s)
      in .c.bits.address.:=(mask & out.c.bits.address)(s)
    }
  }
}

abstract class LazyModuleImp(outer: LazyModule) extends Module
{
  override def desiredName = outer.getClass.getName.split('.').last
  outer.instantiate()
}
