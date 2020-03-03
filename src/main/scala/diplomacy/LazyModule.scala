// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomacy

import Chisel._
import chisel3.{RawModule, MultiIOModule, withClockAndReset}
import chisel3.internal.sourceinfo.{SourceInfo, UnlocatableSourceInfo}
import freechips.rocketchip.config.Parameters
import scala.collection.immutable.{SortedMap, ListMap}
import scala.util.matching._

/** Introduction
  * This library contains these features:
  * - [[Parameters]] negotiation
  * - [[AutoBundle]] generation and connection
  *
  * To achieve these features, [[LazyModule]] consists sub-[[LazyModule]], [[BaseNode]] and [[LazyModuleImpLike]] module.
  * - [[BaseNode]] is for interaction of [[LazyModule]].
  * - [[LazyModuleImpLike]] module is for hardware module implementation.
  *
  * [[LazyModule]] leverage the magic of lazy-evaluation in Scala:
  * It split SoC generation into 2 phases.
  * - Phase 1:
  * -- Node binding
  * - Phase 2:
  * -- [[Parameters]] negotiation.
  * -- [[AutoBundle]] resolution.
  * -- [[LazyModuleImpLike]] module generation.
  *
  */
abstract class LazyModule()(implicit val p: Parameters)
{
  /** Contains sub-[[LazyModule]]s; can be accessed by [[getChildren]]. */
  protected[diplomacy] var children = List[LazyModule]()
  /** Contains the [[BaseNode]]s of this instance. */
  protected[diplomacy] var nodes = List[BaseNode]()
  /** Stores [[SourceInfo]] of this instance.
    *
    * The companion object factory method will set this to the correct value.
    */
  protected[diplomacy] var info: SourceInfo = UnlocatableSourceInfo
  /** Parent of this LazyModule. If this instance is at the top of the hierarchy, this will be [[None]]. */
  protected[diplomacy] val parent: Option[LazyModule] = LazyModule.scope

  /** Code snippets from [[InModuleBody]] injection. */
  protected[diplomacy] var inModuleBody = List[() => Unit]()

  /** Chain of ancestor LazyModules, starting with [[parent]] on left. */
  def parents: Seq[LazyModule] = parent match {
    case None => Nil
    case Some(x) => x +: x.parents
  }

  // [[LazyModule.scope]] stack push.
  LazyModule.scope = Some(this)
  // Prepend this instance to [[parent]]'s list of children.
  parent.foreach(p => p.children = this :: p.children)

  /** Accumulates Some(names), taking the final one. Nones are ignored. */
  private var suggestedNameVar: Option[String] = None

  /** Set instance name of [[LazyModuleImpLike]] module.
    * @param x suggested name to [[LazyModuleImpLike]] module.
    * */
  def suggestName(x: String): this.type = suggestName(Some(x))
  def suggestName(x: Option[String]): this.type = {
    x.foreach { n => suggestedNameVar = Some(n) }
    this
  }

  /** Helper of [[className]]. */
  private def findClassName(c: Class[_]): String = {
    val n = c.getName.split('.').last
    if (n.contains('$')) findClassName(c.getSuperclass) else n
  }

  /** Get the class name of this instance. */
  lazy val className: String = findClassName(getClass)
  /** set instance name. */
  lazy val suggestedName: String = suggestedNameVar.getOrElse(className)
  /** set module name. */
  lazy val desiredName: String = className // + hashcode?

  /** Return instance name. */
  def name = suggestedName // className + suggestedName ++ hashcode ?
  /** Return source line that defines this instance. */
  def line = sourceLine(info)

  // Accessing these names can only be done after circuit elaboration!
  /** Module name, used in GraphML. */
  lazy val moduleName = module.name
  /** Module hierarchy of this instance, used in GraphML. */
  lazy val pathName = module.pathName
  /** Instance name, used in GraphML. */
  lazy val instanceName = pathName.split('.').last

  /** chisel hardware implementation of this [[LazyModule]],
    * This module should be define by `lazy val` for lazy evaluation.
    * Generally, it is the beginning of phase 2.
    * */
  def module: LazyModuleImpLike

  /** generate the [[graphML]] representation for this instance. This is a representation of the Nodes, edges, Lazy Module hierarchy, and any other information that is added in by the implementations. It can be converted to a graphical representation of the [[LazyModule]] hierarchy with various third-party tools.*/
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

  /** give global [[LazyModule]] index to this instance. */
  private val index = { LazyModule.index = LazyModule.index + 1; LazyModule.index }

  /** generate [[BaseNode]] graphML string.
    * @param buf string buffer to write
    * @param pad pad as prefix for indent.
    * */
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

  /** generate [[Edges]] graphML string.
    * @param buf string buffer to write
    * @param pad pad as prefix for indent.
    * */
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

  /** execute function by all of this node's [[children]].
    * @param iterfunc function to run
    * */
  def nodeIterator(iterfunc: (LazyModule) => Unit): Unit = {
    iterfunc(this)
    children.foreach( _.nodeIterator(iterfunc) )
  }

  /** getter of val [[children]]. */
  def getChildren = children
}

object LazyModule
{
  /** Current [[LazyModule]] scope. The scope is a stack of [[LazyModule]]/[[LazyScope]].
    * Each call to [[LazyScope.apply]] or [[LazyModule.apply]] will push that item onto the current scope.
    * */
  protected[diplomacy] var scope: Option[LazyModule] = None
  /** global index of [[LazyModule]], notice there is no No.0 module. */
  private var index = 0

  /** any instance of [[LazyModule]] should be wrapper in to a [[LazyModule.apply]],
    * making use this singleton manage scope and index for elaboration.
    * @param bc [[LazyModule]] instance needed to be wrapped.
    * @param valName [[ValName]] used to naming this val,
    *                it can be automatically generated by [[ValName]] macro, or specify manually.
    * @param sourceInfo [[SourceInfo]] to generate this [[LazyModule]]
    * */
  def apply[T <: LazyModule](bc: T)(implicit valName: ValName, sourceInfo: SourceInfo): T = {
    /** Make sure the user puts [[LazyModule]] around modules in the correct order. */
    require (scope.isDefined, s"LazyModule() applied to ${bc.name} twice ${sourceLine(sourceInfo)}")
    require (scope.get eq bc, s"LazyModule() applied to ${bc.name} before ${scope.get.name} ${sourceLine(sourceInfo)}")
    /** [[LazyModule.scope]] stack pop. */
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

  /** [[wrapper.module]] had better not be accessed while LazyModules are still being built! */
  require (LazyModule.scope.isEmpty, s"${wrapper.name}.module was constructed before LazyModule() was run on ${LazyModule.scope.get.name}")

  /** set module name. */
  override def desiredName = wrapper.desiredName
  suggestName(wrapper.suggestedName)

  /** [[Parameters]] for chisel [[Module]]s. */
  implicit val p = wrapper.p

  /** [[instantiate]] will be called when a instance of [[LazyModuleImp]] is called. */
  protected[diplomacy] def instantiate() = {
    val childDangles = wrapper.children.reverse.flatMap { c =>
      implicit val sourceInfo = c.info
      /** calling [[c.module]] will push the real [[Module]] into [[chisel3.internal.Builder]],
        * notice this place is not calling [[instantiate]], it just push the [[c.module]] to Builder.
        * */
      val mod = Module(c.module)
      /** ask each child to finish instantiate. */
      mod.finishInstantiate()
      /** return [[Dangle]]s of each child. */
      mod.dangles
    }

    /** ask each node in the [[LazyModule]] to call [[BaseNode.instantiate]].
      * it will return a sequence of [[Dangle]] of these [[BaseNode]].
      * */
    val nodeDangles = wrapper.nodes.reverse.flatMap(_.instantiate())
    /** accumulate all the [[Dangle]]s from this node and any accumulated from its [[children]]*/
    val allDangles = nodeDangles ++ childDangles
    /** For [[allDangles]] which originate from the same [[Dangle.source]], group them together into [[pairing]]. */
    val pairing = SortedMap(allDangles.groupBy(_.source).toSeq:_*)
    /** For each set of [[Dangle]]s where there are sets of 2 with the same source, ensure that they can be connected in a source/sink pair, and then make the connection. For these sets, mark them as [[done]].*/
    val done = Set() ++ pairing.values.filter(_.size == 2).map { case Seq(a, b) =>
      require (a.flipped != b.flipped)
      /** @todo <> in chisel3 makes directionless connection. */
      if (a.flipped) { a.data <> b.data } else { b.data <> a.data }
      a.source
    }
    /** find all [[Dangle]]s which are still not connected. These will end up as [[AutoBundle]] [[IO]] ports on the module.*/
    val forward = allDangles.filter(d => !done(d.source))
    /** generate [[AutoBundle]] io from [[forward]]. */
    val auto = IO(new AutoBundle(forward.map { d => (d.name, d.data, d.flipped) }:_*))
    /** Pass the [[Dangle]]s which remained and were used to generate the [[AutoBundle]] I/O ports up to the [[parent]] [[LazyModule]]*/
    val dangles = (forward zip auto.elements) map { case (d, (_, io)) =>
      if (d.flipped) { d.data <> io } else { io <> d.data }
      d.copy(data = io, name = wrapper.suggestedName + "_" + d.name)
    }
    /** push all [[LazyModule.inModuleBody]] to [[chisel3.internal.Builder]]. */
    wrapper.inModuleBody.reverse.foreach { _() }
    /** return [[IO]] and [[Dangle]] of this [[LazyModuleImp]]. */
    (auto, dangles)
  }

  /** Ask each [[BaseNode]] in [[wrapper.nodes]] to call [[BaseNode.finishInstantiate]]
    * Note: There are 2 different `finishInstantiate` methods:
    *   [[LazyModuleImp.finishInstantiate]] and [[BaseNode.finishInstantiate]],
    *   the former is a wrapper to the latter.
    * */
  protected[diplomacy] def finishInstantiate() {
    wrapper.nodes.reverse.foreach { _.finishInstantiate() }
  }
}

class LazyModuleImp(val wrapper: LazyModule) extends MultiIOModule with LazyModuleImpLike {
  /** instantiate hardware of this `Module`. */
  val (auto, dangles) = instantiate()
}

class LazyRawModuleImp(val wrapper: LazyModule) extends RawModule with LazyModuleImpLike {
  /** These wires are the default clock+reset for all LazyModule children.
    * It is recommended to drive these even if you manually drive the [[clock]] and [[reset]] of all of the [[LazyRawModuleImp]] children.
    * Otherwise, anonymous children ([[Monitor]]s for example) will not have their [[clock]] and/or [[reset]] driven properly.
    * */
  val childClock = Wire(Clock())
  val childReset = Wire(Bool())
  childClock := Bool(false).asClock
  childReset := Bool(true)
  val (auto, dangles) = withClockAndReset(childClock, childReset) {
    instantiate()
  }
}

/** used for a [[LazyModule]] which does not need to define any [[LazyModuleImp]] implementation.
  * It can be used as wrapper that only instantiates and connects [[LazyModule]]s.
  * */
class SimpleLazyModule(implicit p: Parameters) extends LazyModule
{
  lazy val module = new LazyModuleImp(this)
}

trait LazyScope
{
  this: LazyModule =>
  override def toString: String = s"LazyScope named $name"
  /** preserve the previous value of the [[LazyModule.scope]], because when calling [[apply]] function,
    * [[LazyModule.scope]] will be altered.
    * */
  def apply[T](body: => T) = {
    val saved = LazyModule.scope
    LazyModule.scope = Some(this)
    /** [[LazyModule.scope]] stack push. */
    val saved = LazyModule.scope
    /** evaluate [[body]].
     * */
    val out = body
    /** when [[body]] is evaluated, try to escape from where. */
    require (LazyModule.scope.isDefined, s"LazyScope ${name} tried to exit, but scope was empty!")
    require (LazyModule.scope.get eq this, s"LazyScope ${name} exited before LazyModule ${LazyModule.scope.get.name} was closed")
    /** [[LazyModule.scope]] stack pop. */
    LazyModule.scope = saved
    out
  }
}

/** used to automatically create a level of module hierarchy (a [[SimpleLazyModule]]) within which [[LazyModule]]s can be instantiated and connected.
  * [[LazyModule]] can be created inside [[LazyScope]].
  * It will instantiate a [[SimpleLazyModule]] to manage append `body`.
  * and make `body` codes evaluated in this scope.
  * */
object LazyScope
{
  /** create a [[LazyScope]] with name in its val name.
    * @param body code need to be evaluated.
    * */
  def apply[T](body: => T)(implicit valName: ValName, p: Parameters): T = {
    val scope = LazyModule(new SimpleLazyModule with LazyScope)
    scope { body }
  }
  /** create a [[LazyScope]] with a self defined name.
    * @param name set valName of [[SimpleLazyModule]] created by this.
    * @param body code need to be evaluated.
    * */
  def apply[T](name: String)(body: => T)(implicit p: Parameters): T = {
    apply(body)(ValName(name), p)
  }
}

/** one side metadata of a [[Dangle]].
  *
  * @param serial serial of [[BaseNode]] contains this.
  * @param index index of resolved ports [[BaseNode]].
  * */
case class HalfEdge(serial: Int, index: Int) extends Ordered[HalfEdge] {
  import scala.math.Ordered.orderingToOrdered
  def compare(that: HalfEdge) = HalfEdge.unapply(this) compare HalfEdge.unapply(that)
}

/** [[Dangle]] is a handle to [[LazyModule]] and [[BaseNode]].
  * It will be returned by [[LazyModuleImpLike.instantiate]] and [[BaseNode.instantiate]],
  * It contains the IO information of a [[LazyModule]] and [[BaseNode]]
  * @param source source edge of this.
  * @param sink sink edge of this.
  * @param flipped flip or not in [[AutoBundle.makeElements]]
  * @param data actual data for connection.
  * */
case class Dangle(source: HalfEdge, sink: HalfEdge, flipped: Boolean, name: String, data: Data)

/** [[AutoBundle]] will construct the [[Bundle]] for [[LazyModule]] in [[LazyModuleImpLike.instantiate]],
  * @param elts is a sequence of data containin for each port  a tuple of (name, data, flipped), where
  *               name: IO name
  *               data: actual data for connection.
  *               flipped: flip or not in [[makeElements]]
  * */
final class AutoBundle(elts: (String, Data, Boolean)*) extends Record {
  /** We need to preserve the order of elts, despite grouping by name to disambiguate things. */
  val elements = ListMap() ++ elts.zipWithIndex.map(makeElements).groupBy(_._1).values.flatMap {
    /** if name is unique, it will return a Seq[index -> (name -> data)]. */
    case Seq((key, element, i)) => Seq(i -> (key -> element))
    /** if name is not unique, name will append with j, and return `Seq[index -> (s"${name}_${j}" -> data)]`. */
    case seq => seq.zipWithIndex.map { case ((key, element, i), j) => i -> (key + "_" + j -> element) }
  }.toList.sortBy(_._1).map(_._2)
  require (elements.size == elts.size)

  /** trim final "(_[0-9]+)*$" in the name,
    * flip data with flipped
    * */
  private def makeElements(tuple: ((String, Data, Boolean), Int)) = {
    val ((key, data, flip), i) = tuple
    /** trim trailing _0_1_2 stuff so that when we append _# we don't create collisions. */
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
  /** code snippet injection.
    * [[InModuleBody.apply(body)]] will inject body to current [[LazyModule.inModuleBody]],
    * it will be called by [[LazyModuleImpLike.instantiate]] to push these snippets to [[chisel3.internal.Builder]].
    * */
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

    /** Note: the [[out.execute]] function is not called here,
      * it is a `() => Unit` val which will be executed later
      * */
    scope.inModuleBody = (out.execute _) +: scope.inModuleBody
    out
  }
}
