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

  def omitGraphML = nodes.isEmpty && children.isEmpty
  lazy val graphML: String = parent.map(_.graphML).getOrElse {
    val buf = new StringBuilder
    buf ++= "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    buf ++= "<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\" xmlns:y=\"http://www.yworks.com/xml/graphml\">\n"
    buf ++= "  <key for=\"node\" id=\"d1\" yfiles.type=\"nodegraphics\"/>\n"
    buf ++= "  <graph id=\"G\" edgedefault=\"directed\">\n"
    nodesGraphML(buf, "    ")
    edgesGraphML(buf, "    ")
    buf ++= "  </graph>\n"
    buf ++= "</graphml>\n"
    buf.toString
  }

  private val index = { LazyModule.index = LazyModule.index + 1; LazyModule.index }

  private def nodesGraphML(buf: StringBuilder, pad: String) {
    buf ++= s"""${pad}<node id=\"${index}\">\n"""
    buf ++= s"""${pad}  <data key=\"d1\"><y:ShapeNode><y:NodeLabel modelName=\"sides\" modelPosition=\"w\" rotationAngle=\"270.0\">${name}</y:NodeLabel></y:ShapeNode></data>\n"""
    buf ++= s"""${pad}  <graph id=\"${index}::\" edgedefault=\"directed\">\n"""
    nodes.filter(!_.omitGraphML).foreach { n =>
      buf ++= s"""${pad}    <node id=\"${index}::${n.index}\"/>\n"""
    }
    children.filter(!_.omitGraphML).foreach { _.nodesGraphML(buf, pad + "    ") }
    buf ++= s"""${pad}  </graph>\n"""
    buf ++= s"""${pad}</node>\n"""
  }
  private def edgesGraphML(buf: StringBuilder, pad: String) {
    nodes.filter(!_.omitGraphML) foreach { n => n.outputs.filter(!_.omitGraphML).foreach { o =>
      buf ++= pad
      buf ++= "<edge"
      buf ++= s""" source=\"${index}::${n.index}\""""
      buf ++= s""" target=\"${o.lazyModule.index}::${o.index}\"/>\n"""
    } }
    children.filter(!_.omitGraphML).foreach { c => c.edgesGraphML(buf, pad) }
  }
}

object LazyModule
{
  protected[tilelink2] var stack = List[LazyModule]()
  private var index = 0

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
