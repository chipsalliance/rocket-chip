// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomacy

import Chisel.{defaultCompileOptions => _, _}
import chisel3.internal.sourceinfo.{SourceInfo, UnlocatableSourceInfo}
import chisel3.{MultiIOModule, RawModule, Reset, withClockAndReset}
import chisel3.experimental.{ChiselAnnotation, CloneModuleAsRecord}
import firrtl.passes.InlineAnnotation
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util.CompileOptions.NotStrictInferReset

import scala.collection.immutable.{ListMap, SortedMap}
import scala.util.matching._

/** While the [[freechips.rocketchip.diplomacy]] package allows fairly abstract parameter negotiation while constructing a DAG,
  * [[LazyModule]] builds on top of the DAG annotated with the negotiated parameters and leverage's Scala's lazy evaluation property to split Chisel module generation into two phases:
  *
  *   - Phase 1 (diplomatic) states parameters, hierarchy, and connections:
  *     - [[LazyModule]] and [[BaseNode]] instantiation.
  *     - [[BaseNode]] binding.
  *   - Phase 2 (lazy) generates [[chisel3]] Modules:
  *     - Parameters are negotiated across [[BaseNode]]s.
  *     - Concrete [[Bundle]]s are created along [[BaseNode]]s and connected
  *     - [[AutoBundle]] are automatically connected along [[Edges]], punching IO as necessary though module hierarchy
  *     - [[LazyModuleImpLike]] generates [[chisel3.Module]]s.
  */
abstract class LazyModule()(implicit val p: Parameters) {
  /** Contains sub-[[LazyModule]]s; can be accessed by [[getChildren]]. */
  protected[diplomacy] var children: List[LazyModule] = List[LazyModule]()
  /** Contains the [[BaseNode]]s instantiated within this instance. */
  protected[diplomacy] var nodes: List[BaseNode] = List[BaseNode]()
  /** Stores [[SourceInfo]] of this instance.
    *
    * The companion object factory method will set this to the correct value.
    */
  protected[diplomacy] var info: SourceInfo = UnlocatableSourceInfo
  /** Parent of this LazyModule. If this instance is at the top of the hierarchy, this will be [[None]]. */
  protected[diplomacy] val parent: Option[LazyModule] = LazyModule.scope
  /** If set, the LazyModule this LazyModule will be a clone of
    * Note that children of a cloned module will also have this set
    */
  var cloneProto: Option[LazyModule] = None

  /** Code snippets from [[InModuleBody]] injection. */
  protected[diplomacy] var inModuleBody: List[() => Unit] = List[() => Unit]()

  /** Sequence of ancestor LazyModules, starting with [[parent]]. */
  def parents: Seq[LazyModule] = parent match {
    case None => Nil
    case Some(x) => x +: x.parents
  }

  // Push this instance onto the [[LazyModule.scope]] stack.
  LazyModule.scope = Some(this)
  parent.foreach(p => p.children = this :: p.children)

  /** Accumulates Some(names), taking the final one. `None`s are ignored. */
  private var suggestedNameVar: Option[String] = None

  /** Suggests instance name for [[LazyModuleImpLike]] module. */
  def suggestName(x: String): this.type = suggestName(Some(x))

  def suggestName(x: Option[String]): this.type = {
    x.foreach { n => suggestedNameVar = Some(n) }
    this
  }

  /** Finds the name of the first non-anonymous Scala class while walking up the class hierarchy. */
  private def findClassName(c: Class[_]): String = {
    val n = c.getName.split('.').last
    if (n.contains('$')) findClassName(c.getSuperclass) else n
  }

  /** Scala class name of this instance. */
  lazy val className: String = findClassName(getClass)
  /** Suggested instance name. Defaults to [[className]].*/
  lazy val suggestedName: String = suggestedNameVar.getOrElse(className)
  /** Suggested module name. Defaults to [[className]].*/
  lazy val desiredName: String = className // + hashcode?

  /** Return instance name. */
  def name: String = suggestedName // className + suggestedName ++ hashcode ?
  /** Return source line that defines this instance. */
  def line: String = sourceLine(info)

  // Accessing these names can only be done after circuit elaboration!
  /** Module name in verilog, used in GraphML.
    * For cloned lazyModules, this is the name of the prototype
    */
  lazy val moduleName: String = cloneProto.map(_.module.name).getOrElse(module.name)
  /** Hierarchical path of this instance, used in GraphML.
    * For cloned modules, construct this manually (since this.module should not be evaluated)
    */
  lazy val pathName: String = if (cloneProto.isDefined) {
    s"${parent.get.pathName}.${cloneProto.get.instanceName}"
  } else {
    module.pathName
  }
  /** Instance name in verilog. Should only be accessed after circuit elaboration. */
  lazy val instanceName: String = pathName.split('.').last

  /** [[chisel3]] hardware implementation of this [[LazyModule]].
    *
    * Subclasses should define this function as `lazy val`s for lazy evaluation.
    * Generally, the evaluation of this marks the beginning of phase 2.
    */
  def module: LazyModuleImpLike

  /** Recursively traverse all child LazyModules and Nodes of this LazyModule
    * to construct the set of empty [[Dangle]]'s that are this module's top-level IO
    * This is effectively doing the same thing as [[LazyModuleImp.instantiate]], but
    * without constructing any [[Module]]'s
    */
  protected[diplomacy] def cloneDangles(): List[Dangle] = {
    children.foreach(c => require(c.cloneProto.isDefined, s"${c.info}, ${c.parent.get.info}"))
    val childDangles = children.reverse.flatMap { c => c.cloneDangles() }
    val nodeDangles = nodes.reverse.flatMap(n => n.cloneDangles())
    val allDangles = nodeDangles ++ childDangles
    val pairing = SortedMap(allDangles.groupBy(_.source).toSeq: _*)
    val done = Set() ++ pairing.values.filter(_.size == 2).map { case Seq(a, b) =>
      require(a.flipped != b.flipped)
      a.source
    }
    val forward = allDangles.filter(d => !done(d.source))
    val dangles = forward.map { d =>
      d.copy(name = suggestedName + "_" + d.name)
    }
    dangles
  }

  /** Whether to omit generating the GraphML for this [[LazyModule]].
    *
    * Recursively checks whether all [[BaseNode]]s and children [[LazyModule]]s should omit GraphML
    * generation.
    */
  def omitGraphML: Boolean = nodes.forall(_.omitGraphML) && children.forall(_.omitGraphML)

  /** Whether this [[LazyModule]]'s module should be marked for in-lining by FIRRTL.
    *
    *  The default heuristic is to inline any parents whose children have been inlined
    *  and whose nodes all produce identity circuits.
    */
  def shouldBeInlined: Boolean = nodes.forall(_.circuitIdentity) && children.forall(_.shouldBeInlined)

  /** GraphML representation for this instance.
    *
    * This is a representation of the Nodes, Edges, LazyModule hierarchy,
    * and any other information that is added in by implementations.
    * It can be converted to an image with various third-party tools.
    */
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

  /** A globally unique [[LazyModule]] index for this instance. */
  private val index = {
    LazyModule.index = LazyModule.index + 1
    LazyModule.index
  }

  /** Generate GraphML fragment for nodes.
    *
    * @param buf String buffer to write to.
    * @param pad Padding as prefix for indentation purposes.
    */
  private def nodesGraphML(buf: StringBuilder, pad: String): Unit = {
    buf ++= s"""$pad<node id=\"$index\">\n"""
    buf ++= s"""$pad  <data key=\"n\"><y:ShapeNode><y:NodeLabel modelName=\"sides\" modelPosition=\"w\" rotationAngle=\"270.0\">$instanceName</y:NodeLabel><y:BorderStyle type=\"${if (shouldBeInlined) "dotted" else "line"}\"/></y:ShapeNode></data>\n"""
    buf ++= s"""$pad  <data key=\"d\">$moduleName ($pathName)</data>\n"""
    buf ++= s"""$pad  <graph id=\"$index::\" edgedefault=\"directed\">\n"""
    nodes.filter(!_.omitGraphML).foreach { n =>
      buf ++= s"""$pad    <node id=\"$index::${n.index}\">\n"""
      buf ++= s"""$pad      <data key=\"n\"><y:ShapeNode><y:Shape type="ellipse"/><y:Fill color="#FFCC00" transparent=\"${n.circuitIdentity}\"/></y:ShapeNode></data>\n"""
      buf ++= s"""$pad      <data key=\"d\">${n.formatNode}, \n${n.nodedebugstring}</data>\n"""
      buf ++= s"""$pad    </node>\n"""
    }
    children.filter(!_.omitGraphML).foreach(_.nodesGraphML(buf, pad + "    "))
    buf ++= s"""$pad  </graph>\n"""
    buf ++= s"""$pad</node>\n"""
  }

  /** Generate GraphML fragment for edges.
    *
    * @param buf String buffer to write to.
    * @param pad Padding as prefix for indentation purposes.
    */
  private def edgesGraphML(buf: StringBuilder, pad: String): Unit = {
    nodes.filter(!_.omitGraphML) foreach { n =>
      n.outputs.filter(!_._1.omitGraphML).foreach { case (o, edge) =>
        val RenderedEdge(colour, label, flipped) = edge
        buf ++= pad
        buf ++= "<edge"
        if (flipped) {
          buf ++= s""" target=\"$index::${n.index}\""""
          buf ++= s""" source=\"${o.lazyModule.index}::${o.index}\">"""
        } else {
          buf ++= s""" source=\"$index::${n.index}\""""
          buf ++= s""" target=\"${o.lazyModule.index}::${o.index}\">"""
        }
        buf ++= s"""<data key=\"e\"><y:PolyLineEdge>"""
        if (flipped) {
          buf ++= s"""<y:Arrows source=\"standard\" target=\"none\"/>"""
        } else {
          buf ++= s"""<y:Arrows source=\"none\" target=\"standard\"/>"""
        }
        buf ++= s"""<y:LineStyle color=\"$colour\" type=\"line\" width=\"1.0\"/>"""
        buf ++= s"""<y:EdgeLabel modelName=\"centered\" rotationAngle=\"270.0\">$label</y:EdgeLabel>"""
        buf ++= s"""</y:PolyLineEdge></data></edge>\n"""
      }
    }
    children.filter(!_.omitGraphML).foreach { c => c.edgesGraphML(buf, pad) }
  }

  /** Call function on all of this [[LazyModule]]'s [[children]].
    *
    * @param iterfunc Function to call on each descendant.
    */
  def childrenIterator(iterfunc: LazyModule => Unit): Unit = {
    iterfunc(this)
    children.foreach(_.childrenIterator(iterfunc))
  }

  /** Call function on all of this [[LazyModule]]'s [[nodes]].
    *
    * @param iterfunc Function to call on each descendant.
    */
  def nodeIterator(iterfunc: BaseNode => Unit): Unit = {
    nodes.foreach(iterfunc)
    childrenIterator(_.nodes.foreach(iterfunc))
  }

  /** Accessor for [[children]]. */
  def getChildren: List[LazyModule] = children

  /** Accessor for [[nodes]]. */
  def getNodes: List[BaseNode] = nodes
}

object LazyModule {
  /** Current [[LazyModule]] scope. The scope is a stack of [[LazyModule]]/[[LazyScope]]s.
    *
    * Each call to [[LazyScope.apply]] or [[LazyModule.apply]] will push that item onto the current scope.
    */
  protected[diplomacy] var scope: Option[LazyModule] = None
  /** Global index of [[LazyModule]]. Note that there is no zeroth module. */
  private var index = 0

  /** Wraps a [[LazyModule]], handling bookkeeping of scopes.
    *
    * This method manages the scope and index of the [[LazyModule]]s. All [[LazyModule]]s must be
    * wrapped exactly once.
    *
    * @param bc         [[LazyModule]] instance to be wrapped.
    * @param valName    [[ValName]] used to name this instance,
    *                   it can be automatically generated by [[ValName]] macro, or specified manually.
    * @param sourceInfo [[SourceInfo]] information about where this [[LazyModule]] is being generated
    */
  def apply[T <: LazyModule](bc: T)(implicit valName: ValName, sourceInfo: SourceInfo): T = {
    // Make sure the user puts [[LazyModule]] around modules in the correct order.
    require(scope.isDefined, s"LazyModule() applied to ${bc.name} twice ${sourceLine(sourceInfo)}. Ensure that descendant LazyModules are instantiated with the LazyModule() wrapper and that you did not call LazyModule() twice.")
    require(scope.get eq bc, s"LazyModule() applied to ${bc.name} before ${scope.get.name} ${sourceLine(sourceInfo)}")
    // Pop from the [[LazyModule.scope]] stack.
    scope = bc.parent
    bc.info = sourceInfo
    if (bc.suggestedNameVar.isEmpty) bc.suggestName(valName.name)
    bc
  }
}

/** Trait describing the actual [[Module]] implementation wrapped by a [[LazyModule]].
  *
  * This is the actual Chisel module that is lazily-evaluated in the second phase of Diplomacy.
  */
sealed trait LazyModuleImpLike extends RawModule {
  /** [[LazyModule]] that contains this instance. */
  val wrapper: LazyModule
  /** IOs that will be automatically "punched" for this instance. */
  val auto: AutoBundle
  /** The metadata that describes the [[HalfEdge]]s which generated [[auto]]. */
  protected[diplomacy] val dangles: Seq[Dangle]

  // [[wrapper.module]] had better not be accessed while LazyModules are still being built!
  require(LazyModule.scope.isEmpty, s"${wrapper.name}.module was constructed before LazyModule() was run on ${LazyModule.scope.get.name}")

  /** Set module name. Defaults to the containing LazyModule's desiredName.*/
  override def desiredName: String = wrapper.desiredName

  suggestName(wrapper.suggestedName)

  /** [[Parameters]] for chisel [[Module]]s. */
  implicit val p: Parameters = wrapper.p

  /** instantiate this [[LazyModule]],
    * return [[AutoBundle]] and a unconnected [[Dangle]]s from this module and submodules. */
  protected[diplomacy] def instantiate(): (AutoBundle, List[Dangle]) = {
    // 1. It will recursively append [[wrapper.children]] into [[chisel3.internal.Builder]],
    // 2. return [[Dangle]]s from each module.
    val childDangles = wrapper.children.reverse.flatMap { c =>
      implicit val sourceInfo: SourceInfo = c.info
      c.cloneProto.map { cp =>
        // If the child is a clone, then recursively set cloneProto of its children as well
        def assignCloneProtos(bases: Seq[LazyModule], clones: Seq[LazyModule]): Unit = {
          require(bases.size == clones.size)
          (bases zip clones).map { case (l,r) =>
            l.cloneProto = Some(r)
            assignCloneProtos(l.children, r.children)
          }
        }
        assignCloneProtos(c.children, cp.children)
        // Clone the child module as a record, and get its [[AutoBundle]]
        val clone = CloneModuleAsRecord(cp.module).suggestName(c.suggestedName)
        val clonedAuto = clone("auto").asInstanceOf[AutoBundle]
        // Get the empty [[Dangle]]'s of the cloned child
        val rawDangles = c.cloneDangles()
        require(rawDangles.size == clonedAuto.elements.size)
        // Assign the [[AutoBundle]] fields of the cloned record to the empty [[Dangle]]'s
        val dangles = (rawDangles zip clonedAuto.elements).map { case (d, (_, io)) =>
          d.copy(dataOpt = Some(io))
        }
        dangles
      } .getOrElse {
        // For non-clones, instantiate the child module
        val mod = Module(c.module)
        mod.dangles
      }
    }

    // Ask each node in this [[LazyModule]] to call [[BaseNode.instantiate]].
    // This will result in a sequence of [[Dangle]] from these [[BaseNode]]s.
    val nodeDangles = wrapper.nodes.reverse.flatMap(_.instantiate())
    // Accumulate all the [[Dangle]]s from this node and any accumulated from its [[wrapper.children]]
    val allDangles = nodeDangles ++ childDangles
    // Group [[allDangles]] by their [[source]].
    val pairing = SortedMap(allDangles.groupBy(_.source).toSeq: _*)
    // For each [[source]] set of [[Dangle]]s of size 2, ensure that these
    // can be connected as a source-sink pair (have opposite flipped value).
    // Make the connection and mark them as [[done]].
    val done = Set() ++ pairing.values.filter(_.size == 2).map { case Seq(a, b) =>
      require(a.flipped != b.flipped)
      // @todo <> in chisel3 makes directionless connection.
      if (a.flipped) {
        a.data <> b.data
      } else {
        b.data <> a.data
      }
      a.source
    }
    // Find all [[Dangle]]s which are still not connected. These will end up as [[AutoBundle]] [[IO]] ports on the module.
    val forward = allDangles.filter(d => !done(d.source))
    // Generate [[AutoBundle]] IO from [[forward]].
    val auto = IO(new AutoBundle(forward.map { d => (d.name, d.data, d.flipped) }: _*))
    // Pass the [[Dangle]]s which remained and were used to generate the [[AutoBundle]] I/O ports up to the [[parent]] [[LazyModule]]
    val dangles = (forward zip auto.elements) map { case (d, (_, io)) =>
      if (d.flipped) {
        d.data <> io
      } else {
        io <> d.data
      }
      d.copy(dataOpt = Some(io), name = wrapper.suggestedName + "_" + d.name)
    }
    // Push all [[LazyModule.inModuleBody]] to [[chisel3.internal.Builder]].
    wrapper.inModuleBody.reverse.foreach {
      _ ()
    }

    if (wrapper.shouldBeInlined) {
      chisel3.experimental.annotate(new ChiselAnnotation {
        def toFirrtl = InlineAnnotation(toNamed)
      })
    }

    // Return [[IO]] and [[Dangle]] of this [[LazyModuleImp]].
    (auto, dangles)
  }
}

/** Actual description of a [[Module]] which can be instantiated by a call to [[LazyModule.module]].
  *
  * @param wrapper the [[LazyModule]] from which the `.module` call is being made.
  */
class LazyModuleImp(val wrapper: LazyModule) extends MultiIOModule with LazyModuleImpLike {
  /** Instantiate hardware of this `Module`. */
  val (auto, dangles) = instantiate()
}

/** Actual description of a [[RawModule]] which can be instantiated by a call to [[LazyModule.module]].
  *
  * @param wrapper the [[LazyModule]] from which the `.module` call is being made.
  */
class LazyRawModuleImp(val wrapper: LazyModule) extends RawModule with LazyModuleImpLike {
  // These wires are the default clock+reset for all LazyModule children.
  // It is recommended to drive these even if you manually drive the [[clock]] and [[reset]] of all of the
  // [[LazyRawModuleImp]] children.
  // Otherwise, anonymous children ([[Monitor]]s for example) will not have their [[clock]] and/or [[reset]] driven properly.
  /** drive clock explicitly. */
  val childClock: Clock = Wire(Clock())
  /** drive reset explicitly. */
  val childReset: Reset = Wire(Reset())
  // the default is that these are disabled
  childClock := Bool(false).asClock
  childReset := chisel3.DontCare
  val (auto, dangles) = withClockAndReset(childClock, childReset) {
    instantiate()
  }
}

/** Used for a [[LazyModule]] which does not need to define any [[LazyModuleImp]] implementation.
  *
  * It can be used as wrapper that only instantiates and connects [[LazyModule]]s.
  */
class SimpleLazyModule(implicit p: Parameters) extends LazyModule {
  lazy val module = new LazyModuleImp(this)
}

/** Allows dynamic creation of [[Module]] hierarchy and "shoving" logic into a [[LazyModule]]. */
trait LazyScope {
  this: LazyModule =>
  override def toString: String = s"LazyScope named $name"

  /** Evaluate `body` in the current [[LazyModule.scope]] */
  def apply[T](body: => T): T = {
    // Preserve the previous value of the [[LazyModule.scope]], because when calling [[apply]] function,
    // [[LazyModule.scope]] will be altered.
    val saved = LazyModule.scope
    // [[LazyModule.scope]] stack push.
    LazyModule.scope = Some(this)
    // Evaluate [[body]] in the current `scope`, saving the result to [[out]].
    val out = body
    // Check that the `scope` after evaluating `body` is the same as when we started.
    require(LazyModule.scope.isDefined, s"LazyScope $name tried to exit, but scope was empty!")
    require(LazyModule.scope.get eq this, s"LazyScope $name exited before LazyModule ${LazyModule.scope.get.name} was closed")
    // [[LazyModule.scope]] stack pop.
    LazyModule.scope = saved
    out
  }
}

/** Used to automatically create a level of module hierarchy (a [[SimpleLazyModule]]) within which [[LazyModule]]s can be instantiated and connected.
  *
  * It will instantiate a [[SimpleLazyModule]] to manage evaluation of `body` and evaluate `body` code snippets in this scope.
  */
object LazyScope {
  /** Create a [[LazyScope]] with an implicit instance name.
    *
    * @param body    code executed within the generated [[SimpleLazyModule]].
    * @param valName instance name of generated [[SimpleLazyModule]].
    * @param p       [[Parameters]] propagated to [[SimpleLazyModule]].
    */
  def apply[T](body: => T)(implicit valName: ValName, p: Parameters): T = {
    apply(valName.toString, "SimpleLazyModule", None)(body)(p)
  }

  /** Create a [[LazyScope]] with an explicitly defined instance name.
    *
    * @param name      instance name of generated [[SimpleLazyModule]].
    * @param body      code executed within the generated `SimpleLazyModule`
    * @param p         [[Parameters]] propagated to [[SimpleLazyModule]].
    */
  def apply[T](name: String)(body: => T)(implicit p: Parameters): T = {
    apply(name, "SimpleLazyModule", None)(body)(p)
  }

  /** Create a [[LazyScope]] with an explicit instance and class name, and control inlining.
    *
    * @param name instance name of generated [[SimpleLazyModule]].
    * @param desiredModuleName class name of generated [[SimpleLazyModule]].
    * @param overrideInlining tell FIRRTL that this [[SimpleLazyModule]]'s module should be inlined.
    * @param body code executed within the generated `SimpleLazyModule`
    * @param p [[Parameters]] propagated to [[SimpleLazyModule]].
    */
  def apply[T](
    name: String,
    desiredModuleName: String,
    overrideInlining: Option[Boolean] = None)
    (body: => T)
    (implicit p: Parameters): T =
  {
    val scope = LazyModule(new SimpleLazyModule with LazyScope {
      override lazy val desiredName = desiredModuleName
      override def shouldBeInlined = overrideInlining.getOrElse(super.shouldBeInlined)
    }).suggestName(name)
    scope {
      body
    }
  }

  /** Create a [[LazyScope]] to temporarily group children for some reason, but tell Firrtl to inline it.
    *
    * For example, we might want to control a set of children's clocks but then not keep the parent wrapper.
    *
    * @param body      code executed within the generated `SimpleLazyModule`
    * @param p         [[Parameters]] propagated to [[SimpleLazyModule]].
    */
  def inline[T](body: => T)(implicit p: Parameters): T = {
    apply("noname", "ShouldBeInlined", Some(false))(body)(p)
  }
}

/** One side metadata of a [[Dangle]].
  *
  * Describes one side of an edge going into or out of a [[BaseNode]].
  *
  * @param serial the global [[BaseNode.serial]] number of the [[BaseNode]] that this [[HalfEdge]] connects to.
  * @param index  the `index` in the [[BaseNode]]'s input or output port list that this [[HalfEdge]] belongs to.
  */
case class HalfEdge(serial: Int, index: Int) extends Ordered[HalfEdge] {

  import scala.math.Ordered.orderingToOrdered

  def compare(that: HalfEdge): Int = HalfEdge.unapply(this) compare HalfEdge.unapply(that)
}

/** [[Dangle]] captures the `IO` information of a [[LazyModule]] and which two [[BaseNode]]s the [[Edges]]/[[Bundle]] connects.
  *
  * [[Dangle]]s are generated by [[BaseNode.instantiate]]
  * using [[MixedNode.danglesOut]] and [[MixedNode.danglesIn]] ,
  * [[LazyModuleImp.instantiate]] connects those that go to internal or explicit IO connections
  * in a [[LazyModule]].
  *
  * @param source  the source [[HalfEdge]] of this [[Dangle]], which captures the source [[BaseNode]] and the port `index` within that [[BaseNode]].
  * @param sink    sink [[HalfEdge]] of this [[Dangle]], which captures the sink [[BaseNode]] and the port `index` within that [[BaseNode]].
  * @param flipped flip or not in [[AutoBundle.makeElements]]. If true this corresponds to `danglesOut`, if false it corresponds to `danglesIn`.
  * @param dataOpt actual [[Data]] for the hardware connection. Can be empty if this belongs to a cloned module
  */
case class Dangle(source: HalfEdge, sink: HalfEdge, flipped: Boolean, name: String, dataOpt: Option[Data]) {
  def data = dataOpt.get
}

/** [[AutoBundle]] will construct the [[Bundle]]s for a [[LazyModule]] in [[LazyModuleImpLike.instantiate]],
  *
  * @param elts is a sequence of data containing for each IO port  a tuple of (name, data, flipped), where
  *             name: IO name
  *             data: actual data for connection.
  *             flipped: flip or not in [[makeElements]]
  */
final class AutoBundle(elts: (String, Data, Boolean)*) extends Record {
  // We need to preserve the order of elts, despite grouping by name to disambiguate things.
  val elements: ListMap[String, Data] = ListMap() ++ elts.zipWithIndex.map(makeElements).groupBy(_._1).values.flatMap {
    // If name is unique, it will return a Seq[index -> (name -> data)].
    case Seq((key, element, i)) => Seq(i -> (key -> element))
    // If name is not unique, name will append with j, and return `Seq[index -> (s"${name}_${j}" -> data)]`.
    case seq => seq.zipWithIndex.map { case ((key, element, i), j) => i -> (key + "_" + j -> element) }
  }.toList.sortBy(_._1).map(_._2)
  require(elements.size == elts.size)

  // Trim final "(_[0-9]+)*$" in the name, flip data with flipped.
  private def makeElements(tuple: ((String, Data, Boolean), Int)) = {
    val ((key, data, flip), i) = tuple
    // Trim trailing _0_1_2 stuff so that when we append _# we don't create collisions.
    val regex = new Regex("(_[0-9]+)*$")
    val element = if (flip) data.cloneType.flip() else data.cloneType
    (regex.replaceAllIn(key, ""), element, i)
  }

  override def cloneType: this.type = new AutoBundle(elts: _*).asInstanceOf[this.type]
}

trait ModuleValue[T] {
  def getWrappedValue: T
}

/** Used to inject code snippets to be evaluated in [[LazyModuleImp.instantiate]] in the current [[LazyModule.scope]].
  *
  * It can be used to create additional hardware outside of the [[LazyModule.children]],
  * connections other than the internal [[BaseNode]] connections,
  * or additional IOs aside from the [[AutoBundle]]
  */
object InModuleBody {
  def apply[T](body: => T): ModuleValue[T] = {
    require(LazyModule.scope.isDefined, s"InModuleBody invoked outside a LazyModule")
    val scope = LazyModule.scope.get
    // a wrapper to [[body]], being able to extract result after `execute`.
    val out = new ModuleValue[T] {
      var result: Option[T] = None

      def execute(): Unit = {
        result = Some(body)
      }

      def getWrappedValue: T = {
        require(result.isDefined, s"InModuleBody contents were requested before module was evaluated!")
        result.get
      }
    }

    // Prepend [[out.execute]] to [[scope.inModuleBody]],
    // it is a val with type of `() => Unit`, which will be executed in [[LazyModuleImp.instantiate]].
    scope.inModuleBody = out.execute _ +: scope.inModuleBody
    out
  }
}
