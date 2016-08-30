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
      TLMonitor.legalize(y.bundleOut(j), y.edgesOut(j), x.bundleIn(i), x.edgesIn(i), s)
      x.bundleIn(i).<>(y.bundleOut(j))(s)
      val mask = ~UInt(x.edgesIn(i).manager.beatBytes - 1)
      x.bundleIn (i).a.bits.address.:=(mask & y.bundleOut(j).a.bits.address)(s)
      y.bundleOut(j).b.bits.address.:=(mask & x.bundleIn (i).b.bits.address)(s)
      x.bundleIn (i).c.bits.address.:=(mask & y.bundleOut(j).c.bits.address)(s)
    }
  }
}

// Use this if you have only one node => makes factory adapters possible
abstract class TLSimpleFactory extends TLFactory
{
  def node: TLBaseNode
}

abstract class TLModule(factory: TLFactory) extends Module
{
  override def desiredName = factory.getClass.getName.split('.').last
  factory.instantiate()
}
