// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomacy

import Chisel.{defaultCompileOptions => _, _}
import freechips.rocketchip.util.CompileOptions.NotStrictInferReset
import chisel3.{RawModule, MultiIOModule, withClockAndReset, Reset}
import chisel3.experimental.{annotate, ChiselAnnotation}
import chisel3.internal.sourceinfo.{SourceInfo, SourceLine, UnlocatableSourceInfo}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.firrtl.ModuleNameAnnotation
import scala.collection.immutable.{SortedMap,ListMap}
import scala.util.matching._

abstract class LazyModule()(implicit val p: Parameters)
{
  protected[diplomacy] var children = List[LazyModule]()
  protected[diplomacy] var nodes = List[BaseNode]()
  protected[diplomacy] var info: SourceInfo = UnlocatableSourceInfo
  protected[diplomacy] val parent = LazyModule.scope

  // code snippets from 'InModuleBody' injection
  protected[diplomacy] var inModuleBody = List[() => Unit]()

  def parents: Seq[LazyModule] = parent match {
    case None => Nil
    case Some(x) => x +: x.parents
  }

  LazyModule.scope = Some(this)
  parent.foreach(p => p.children = this :: p.children)

  // suggestedName accumulates Some(names), taking the final one. Nones are ignored.
  private var suggestedNameVar: Option[String] = None
  def suggestName(x: String): this.type = suggestName(Some(x))
  def suggestName(x: Option[String]): this.type = {
    x.foreach { n => suggestedNameVar = Some(n) }
    this
  }

  private def findClassName(c: Class[_]): String = {
    val n = c.getName.split('.').last
    if (n.contains('$')) findClassName(c.getSuperclass) else n
  }

  lazy val className = findClassName(getClass)
  lazy val suggestedName = suggestedNameVar.getOrElse(className)
  lazy val desiredName = className // + hashcode?

  def name = suggestedName // className + suggestedName ++ hashcode ?
  def line = sourceLine(info)

  // Accessing these names can only be done after circuit elaboration!
  lazy val moduleName = module.name // The final Verilog Module name
  lazy val pathName = module.pathName
  lazy val instanceName = pathName.split('.').last // The final Verilog instance name

  def module: LazyModuleImpLike

  def omitGraphML: Boolean = !nodes.exists(!_.omitGraphML) && !children.exists(!_.omitGraphML)
  lazy val graphML: String = parent.map(_.graphML).getOrElse {
    val buf = new StringBuilder
    buf ++= "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    buf ++= "<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\" xmlns:y=\"http://www.yworks.com/xml/graphml\">\n"
    buf ++= "  <key for=\"node\" id=\"n\" yfiles.type=\"nodegraphics\"/>\n"
    buf ++= "  <key for=\"edge\" id=\"e\" yfiles.type=\"edgegraphics\"/>\n"
    buf ++= "  <key for=\"node\" id=\"d\" attr.name=\"Description\" attr.type=\"string\"/>\n"
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
    buf ++= s"""${pad}  <data key=\"n\"><y:ShapeNode><y:NodeLabel modelName=\"sides\" modelPosition=\"w\" rotationAngle=\"270.0\">${instanceName}</y:NodeLabel></y:ShapeNode></data>\n"""
    buf ++= s"""${pad}  <data key=\"d\">${moduleName} (${pathName})</data>\n"""
    buf ++= s"""${pad}  <graph id=\"${index}::\" edgedefault=\"directed\">\n"""
    nodes.filter(!_.omitGraphML).foreach { n =>
      buf ++= s"""${pad}    <node id=\"${index}::${n.index}\">\n"""
      buf ++= s"""${pad}      <data key=\"e\"><y:ShapeNode><y:Shape type="Ellipse"/></y:ShapeNode></data>\n"""
      buf ++= s"""${pad}      <data key=\"d\">${n.formatNode}, \n${n.nodedebugstring}</data>\n"""
      buf ++= s"""${pad}    </node>\n"""
    }
    children.filter(!_.omitGraphML).foreach { _.nodesGraphML(buf, pad + "    ") }
    buf ++= s"""${pad}  </graph>\n"""
    buf ++= s"""${pad}</node>\n"""
  }
  private def edgesGraphML(buf: StringBuilder, pad: String) {
    nodes.filter(!_.omitGraphML) foreach { n => n.outputs.filter(!_._1.omitGraphML).foreach { case (o, edge) =>
      val RenderedEdge(colour, label, flipped) = edge
      buf ++= pad
      buf ++= "<edge"
      if (flipped) {
        buf ++= s""" target=\"${index}::${n.index}\""""
        buf ++= s""" source=\"${o.lazyModule.index}::${o.index}\">"""
      } else {
        buf ++= s""" source=\"${index}::${n.index}\""""
        buf ++= s""" target=\"${o.lazyModule.index}::${o.index}\">"""
      }
      buf ++= s"""<data key=\"e\"><y:PolyLineEdge>"""
      if (flipped) {
        buf ++= s"""<y:Arrows source=\"standard\" target=\"none\"/>"""
      } else {
        buf ++= s"""<y:Arrows source=\"none\" target=\"standard\"/>"""
      }
      buf ++= s"""<y:LineStyle color=\"${colour}\" type=\"line\" width=\"1.0\"/>"""
      buf ++= s"""<y:EdgeLabel modelName=\"centered\" rotationAngle=\"270.0\">${label}</y:EdgeLabel>"""
      buf ++= s"""</y:PolyLineEdge></data></edge>\n"""
    } }
    children.filter(!_.omitGraphML).foreach { c => c.edgesGraphML(buf, pad) }
  }

  def childrenIterator(iterfunc: (LazyModule) => Unit): Unit = {
    iterfunc(this)
    children.foreach( _.childrenIterator(iterfunc) )
  }

  def nodeIterator(iterfunc: (BaseNode) => Unit): Unit = {
    nodes.foreach(iterfunc)
    childrenIterator(_.nodes.foreach(iterfunc))
  }

  def getChildren = children

  def getNodes = nodes
}

object LazyModule
{
  protected[diplomacy] var scope: Option[LazyModule] = None
  private var index = 0

  def apply[T <: LazyModule](bc: T)(implicit valName: ValName, sourceInfo: SourceInfo): T = {
    // Make sure the user put LazyModule around modules in the correct order
    // If this require fails, probably some grandchild was missing a LazyModule
    // ... or you applied LazyModule twice
    require (scope.isDefined, s"LazyModule() applied to ${bc.name} twice ${sourceLine(sourceInfo)}")
    require (scope.get eq bc, s"LazyModule() applied to ${bc.name} before ${scope.get.name} ${sourceLine(sourceInfo)}")
    scope = bc.parent
    bc.info = sourceInfo
    if (!bc.suggestedNameVar.isDefined) bc.suggestName(valName.name)
    bc
  }
}

sealed trait LazyModuleImpLike extends RawModule
{
  val wrapper: LazyModule
  val auto: AutoBundle
  protected[diplomacy] val dangles: Seq[Dangle]

  private val outer = this
  annotate(new ChiselAnnotation {
    def toFirrtl = new ModuleNameAnnotation(outer.desiredName, outer.toTarget)
  })
  // .module had better not be accessed while LazyModules are still being built!
  require (!LazyModule.scope.isDefined, s"${wrapper.name}.module was constructed before LazyModule() was run on ${LazyModule.scope.get.name}")

  override def desiredName = wrapper.desiredName
  suggestName(wrapper.suggestedName)

  implicit val p = wrapper.p

  protected[diplomacy] def instantiate() = {
    val childDangles = wrapper.children.reverse.flatMap { c =>
      implicit val sourceInfo = c.info
      val mod = Module(c.module)
      mod.finishInstantiate()
      mod.dangles
    }
    val nodeDangles = wrapper.nodes.reverse.flatMap(_.instantiate())
    val allDangles = nodeDangles ++ childDangles
    val pairing = SortedMap(allDangles.groupBy(_.source).toSeq:_*)
    val done = Set() ++ pairing.values.filter(_.size == 2).map { case Seq(a, b) =>
      require (a.flipped != b.flipped)
      if (a.flipped) { a.data <> b.data } else { b.data <> a.data }
      a.source
    }
    val forward = allDangles.filter(d => !done(d.source))
    val auto = IO(new AutoBundle(forward.map { d => (d.name, d.data, d.flipped) }:_*))
    val dangles = (forward zip auto.elements) map { case (d, (_, io)) =>
      if (d.flipped) { d.data <> io } else { io <> d.data }
      d.copy(data = io, name = wrapper.suggestedName + "_" + d.name)
    }
    wrapper.inModuleBody.reverse.foreach { _() }
    (auto, dangles)
  }

  protected[diplomacy] def finishInstantiate() {
    wrapper.nodes.reverse.foreach { _.finishInstantiate() }
  }
}

class LazyModuleImp(val wrapper: LazyModule) extends MultiIOModule with LazyModuleImpLike {
  val (auto, dangles) = instantiate()
}

class LazyRawModuleImp(val wrapper: LazyModule) extends RawModule with LazyModuleImpLike {
  // These wires are the default clock+reset for all LazyModule children
  // It is recommended to drive these even if you manually shove most of your children
  // Otherwise, anonymous children (Monitors for example) will not be clocked
  val childClock = Wire(Clock())
  val childReset = Wire(Reset())
  childClock := Bool(false).asClock
  childReset := chisel3.DontCare
  val (auto, dangles) = withClockAndReset(childClock, childReset) {
    instantiate()
  }
}

class SimpleLazyModule(implicit p: Parameters) extends LazyModule
{
  lazy val module = new LazyModuleImp(this)
}

trait LazyScope
{
  this: LazyModule =>
  override def toString: String = s"LazyScope named $name"
  def apply[T](body: => T) = {
    val saved = LazyModule.scope
    LazyModule.scope = Some(this)
    val out = body
    require (LazyModule.scope.isDefined, s"LazyScope ${name} tried to exit, but scope was empty!")
    require (LazyModule.scope.get eq this, s"LazyScope ${name} exited before LazyModule ${LazyModule.scope.get.name} was closed")
    LazyModule.scope = saved
    out
  }
}

object LazyScope
{
  def apply[T](body: => T)(implicit valName: ValName, p: Parameters): T = {
    val scope = LazyModule(new SimpleLazyModule with LazyScope)
    scope { body }
  }
  def apply[T](name: String)(body: => T)(implicit p: Parameters): T = {
    apply(body)(ValName(name), p)
  }
}

case class HalfEdge(serial: Int, index: Int) extends Ordered[HalfEdge] {
  import scala.math.Ordered.orderingToOrdered
  def compare(that: HalfEdge) = HalfEdge.unapply(this) compare HalfEdge.unapply(that)
}
case class Dangle(source: HalfEdge, sink: HalfEdge, flipped: Boolean, name: String, data: Data)

final class AutoBundle(elts: (String, Data, Boolean)*) extends Record {
  // We need to preserve the order of elts, despite grouping by name to disambiguate things
  val elements = ListMap() ++ elts.zipWithIndex.map(makeElements).groupBy(_._1).values.flatMap {
    case Seq((key, element, i)) => Seq(i -> (key -> element))
    case seq => seq.zipWithIndex.map { case ((key, element, i), j) => i -> (key + "_" + j -> element) }
  }.toList.sortBy(_._1).map(_._2)
  require (elements.size == elts.size)

  private def makeElements(tuple: ((String, Data, Boolean), Int)) = {
    val ((key, data, flip), i) = tuple
    // trim trailing _0_1_2 stuff so that when we append _# we don't create collisions
    val regex = new Regex("(_[0-9]+)*$")
    val element = if (flip) data.cloneType.flip else data.cloneType
    (regex.replaceAllIn(key, ""), element, i)
  }

  override def cloneType = (new AutoBundle(elts:_*)).asInstanceOf[this.type]
}

trait ModuleValue[T]
{
  def getWrappedValue: T
}

object InModuleBody
{
  def apply[T](body: => T): ModuleValue[T] = {
    require (LazyModule.scope.isDefined, s"InModuleBody invoked outside a LazyModule")
    val scope = LazyModule.scope.get
    val out = new ModuleValue[T] {
      var result: Option[T] = None
      def execute() { result = Some(body) }
      def getWrappedValue = {
        require (result.isDefined, s"InModuleBody contents were requested before module was evaluated!")
        result.get
      }
    }
    scope.inModuleBody = (out.execute _) +: scope.inModuleBody
    out
  }
}
