// See LICENSE.SiFive for license details.

package diplomacy

import Chisel._
import config._
import chisel3.experimental.{BaseModule, RawModule, MultiIOModule}
import chisel3.internal.sourceinfo.{SourceInfo, SourceLine, UnlocatableSourceInfo}

abstract class LazyModule()(implicit val p: Parameters)
{
  protected[diplomacy] var bindings = List[() => Unit]()
  protected[diplomacy] var children = List[LazyModule]()
  protected[diplomacy] var nodes = List[BaseNode]()
  protected[diplomacy] var info: SourceInfo = UnlocatableSourceInfo
  protected[diplomacy] val parent = LazyModule.stack.headOption

  LazyModule.stack = this :: LazyModule.stack
  parent.foreach(p => p.children = this :: p.children)

  private var suggestedName: Option[String] = None
  def suggestName(x: String) = suggestedName = Some(x)

  private lazy val childNames =
    getClass.getMethods.filter { m =>
      m.getParameterTypes.isEmpty &&
      !java.lang.reflect.Modifier.isStatic(m.getModifiers) &&
      m.getName != "children"
    }.flatMap { m =>
      if (classOf[LazyModule].isAssignableFrom(m.getReturnType)) {
        val obj = m.invoke(this)
        if (obj eq null) Seq() else Seq((m.getName, obj))
      } else if (classOf[Seq[LazyModule]].isAssignableFrom(m.getReturnType)) {
        val obj = m.invoke(this)
        if (obj eq null) Seq() else {
          val seq = try { obj.asInstanceOf[Seq[Object]] } catch { case _: Throwable => null }
          if (seq eq null) Seq() else {
            seq.zipWithIndex.map { case (l, i) => (m.getName + "_"  + i, l) }
          }
        }
      } else Seq()
    }
  private def findValName =
    parent.flatMap(_.childNames.find(_._2 eq this)).map(_._1)

  lazy val className = getClass.getName.split('.').last
  lazy val valName = suggestedName.orElse(findValName)
  lazy val outerName = if (nodes.size != 1) None else nodes(0).gco.flatMap(_.lazyModule.valName)

  def moduleName = className + valName.orElse(outerName).map("_" + _).getOrElse("")
  def instanceName = valName.getOrElse(outerName.map(_ + "_").getOrElse("") + className)
  def name = valName.getOrElse(className)
  def line = sourceLine(info)

  def module: LazyModuleImpLike

  protected[diplomacy] def instantiate() = {
    children.reverse.foreach { c =>
      // !!! fix chisel3 so we can pass the desired sourceInfo
      // implicit val sourceInfo = c.module.outer.info
      Module(c.module)
    }
    bindings.reverse.foreach { f => f () }
  }

  def omitGraphML: Boolean = !nodes.exists(!_.omitGraphML) && !children.exists(!_.omitGraphML)
  lazy val graphML: String = parent.map(_.graphML).getOrElse {
    val buf = new StringBuilder
    buf ++= "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    buf ++= "<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\" xmlns:y=\"http://www.yworks.com/xml/graphml\">\n"
    buf ++= "  <key for=\"node\" id=\"n\" yfiles.type=\"nodegraphics\"/>\n"
    buf ++= "  <key for=\"edge\" id=\"e\" yfiles.type=\"edgegraphics\"/>\n"
    buf ++= "  <key for=\"node\" id=\"d\" attr.name=\"NodeDebugString\" attr.type=\"string\"/>\n"
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
    buf ++= s"""${pad}  <data key=\"n\"><y:ShapeNode><y:NodeLabel modelName=\"sides\" modelPosition=\"w\" rotationAngle=\"270.0\">${module.instanceName}</y:NodeLabel></y:ShapeNode></data>\n"""
    buf ++= s"""${pad}  <graph id=\"${index}::\" edgedefault=\"directed\">\n"""
    nodes.filter(!_.omitGraphML).foreach { n =>
      buf ++= s"""${pad}    <node id=\"${index}::${n.index}\">\n"""
      buf ++= s"""${pad}      <data key=\"d\"><y:ShapeNode><y:Shape type="ellipse"/></y:ShapeNode>${n.nodedebugstring}</data>\n"""
      buf ++= s"""${pad}    </node>\n"""
    }
    children.filter(!_.omitGraphML).foreach { _.nodesGraphML(buf, pad + "    ") }
    buf ++= s"""${pad}  </graph>\n"""
    buf ++= s"""${pad}</node>\n"""
  }
  private def edgesGraphML(buf: StringBuilder, pad: String) {
    nodes.filter(!_.omitGraphML) foreach { n => n.outputs.filter(!_._1.omitGraphML).foreach { case (o, l) =>
      buf ++= pad
      buf ++= "<edge"
      if (o.reverse) {
        buf ++= s""" target=\"${index}::${n.index}\""""
        buf ++= s""" source=\"${o.lazyModule.index}::${o.index}\">"""
      } else {
        buf ++= s""" source=\"${index}::${n.index}\""""
        buf ++= s""" target=\"${o.lazyModule.index}::${o.index}\">"""
      }
      buf ++= s"""<data key=\"e\"><y:PolyLineEdge>"""
      if (o.reverse) {
        buf ++= s"""<y:Arrows source=\"standard\" target=\"none\"/>"""
      } else {
        buf ++= s"""<y:Arrows source=\"none\" target=\"standard\"/>"""
      }
      buf ++= s"""<y:LineStyle color=\"${o.colour}\" type=\"line\" width=\"1.0\"/>"""
      buf ++= s"""<y:EdgeLabel modelName=\"centered\" rotationAngle=\"270.0\">${l}</y:EdgeLabel>"""
      buf ++= s"""</y:PolyLineEdge></data></edge>\n"""
    } }
    children.filter(!_.omitGraphML).foreach { c => c.edgesGraphML(buf, pad) }
  }

  def nodeIterator(iterfunc: (LazyModule) => Unit): Unit = {
    iterfunc(this)
    children.foreach( _.nodeIterator(iterfunc) )
  }
}

object LazyModule
{
  protected[diplomacy] var stack = List[LazyModule]()
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

trait LazyModuleImpLike extends BaseModule
{
  val wrapper: LazyModule

  // .module had better not be accessed while LazyModules are still being built!
  require (LazyModule.stack.isEmpty, s"${wrapper.name}.module was constructed before LazyModule() was run on ${LazyModule.stack.head.name}")

  override def desiredName = wrapper.moduleName
  suggestName(wrapper.instanceName)

  wrapper.instantiate()

  implicit val p = wrapper.p
}

abstract class LazyModuleImp(val wrapper: LazyModule) extends Module with LazyModuleImpLike

abstract class LazyMultiIOModuleImp(val wrapper: LazyModule) extends MultiIOModule with LazyModuleImpLike

abstract class LazyRawModuleImp(val wrapper: LazyModule) extends RawModule with LazyModuleImpLike
