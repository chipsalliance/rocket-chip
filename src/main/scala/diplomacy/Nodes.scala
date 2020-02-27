// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomacy

import Chisel._
import chisel3.experimental.IO
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.{Parameters,Field}
import freechips.rocketchip.util.HeterogeneousBag
import scala.collection.mutable.ListBuffer
import scala.util.matching._

/** Nodes system is the abstraction of the bus interconnect,
  * Since all blocks(CPUs, caches, peripheries) are connected to bus,
  * It has a very complex data flow illustrated graph below:
  *                                                  ┌────────────────────────────────────────────────────────────────────────────┐
  *                                                  ↓                                                                            │
  *                                       [[MixedNode.uoParams]]──────→[[MixedNode.mapParamsU]]───────────┐                       │
  *      [[InwardNode.accPI]]                                                     ↑                       │                       │
  *                  │                                                            │                       │                       │
  *                  ↓                                                            │                       ↓                       │
  *      [[InwardNode.iBindings]]──┐     [[MixedNode.iDirectPorts]]───→[[MixedNode.iPorts]]    [[MixedNode.uiParams]]             │
  *                  │             │                ↑                             │                       │                       │
  *                  │             │                │                             └────────┬──────────────┤                       │
  *                  │             │                │                                      │              ↓                       │
  *                  │             └────[[MixedNode.oPortMapping]]    [[MixedNode.oStar]]  │   [[MixedNode.edgesIn]]───┐          │
  *                  │                              ↑                            ↑         │              │            ↓          │
  *                  │                              │                            │         │              │ [[MixedNode.in]]      │
  *                  │                              │                            │         │              ↓            ↑          │
  *                  │                              │                            │         │   [[MixedNode.bundleIn]]──┘          │
  *                  ├─→ [[MixedNode.resolveStar]]──┼────────────────────────────┤         └────────────────────────────────────┐ │
  *                  │                              │                            │             [[MixedNode.bundleOut]]─┐        │ │
  *                  │                              │                            │                        ↑            ↓        │ │
  *                  │                              │                            │                        │ [[MixedNode.out]]   │ │
  *                  │                              ↓                            ↓                        │            ↑        │ │
  *                  │            ┌──────[[MixedNode.iPortMapping]]   [[MixedNode.iStar]]      [[MixedNode.edgesOut]]──┘        │ │
  *                  │            │                 │                                                     ↑                     │ │
  *                  │            │                 │                             ┌───────────────────────┤                     │ │
  *                  │            │                 ↓                             │                       │                     │ │
  *     [[OutwardNode.oBindings]]─┘      [[MixedNode.oDirectPorts]]───→[[MixedNode.oPorts]]    [[MixedNode.doParams]]           │ │
  *                  ↑                                                            │                       │                     │ │
  *                  │               ┌────────────────────────────────────────────┤                       │                     │ │
  *     [[OutwardNode.accPO]]        │                                            ↓                       │                     │ │
  *                                  │    [[MixedNode.diParams]]──────→[[MixedNode.mapParamsD]]───────────┘                     │ │
  *                                  │               ↑                                                                          │ │
  *                                  │               └──────────────────────────────────────────────────────────────────────────┘ │
  *                                  └────────────────────────────────────────────────────────────────────────────────────────────┘
  * It contains such phases:
  *   1. Node binding(non-lazy)
  *     `nodeB := nodeA` is a typical node binding in diplomacy.
  *     It means `nodeA` as master [[OutwardNode]], `nodeB` as slave [[InwardNode]] will be connected together.
  *     when executing `nodeB := nodeA`, `nodeB` will invoke [[InwardNode.bind]], which is implemented with [[MixedNode.bind]]:
  *       `nodeA` will call `nodeA.oPush(index, node, binding)`, in [[OutwardNode.oPush]]:
  *         `index` is current `nodeA` [[OutwardNode.accPO]] size, which represents the how many [[InwardNode]] has connected to `nodeA`
  *         `node` is [[NodeHandle]] of `NodeB`,
  *         `binding` is binding type, which is used for deciding port mapping behavior.
  *       similarly, `nodeB`, will call [[InwardNode.iPush]] after `nodeA` finish its binding.
  *   2. Port mapping(lazy)
  *     Port mapping converts [[OutwardNode.accPO]] and [[InwardNode.accPI]] to [[MixedNode.oPortMapping]] and [[MixedNode.iPortMapping]]
  *     it is implemented with [[MixedNode.resolveStar]], which solves the size of each connection.
  *   3. Parameter negotiation(lazy)
  *     After finishing port mapping,
  *     [[MixedNode.doParams]] and [[MixedNode.uiParams]] are result of parameter negotiation,
  *   4. Bundle building.(lazy)
  *     After finishing parameter propagation,
  *     [[MixedNode.edges]] can be generated by zipping ports and parameters together.
  *
  * Diplomacy has some implicit acronym described below:
  *   Parameters System:
  *   D[IO], U[IO], E[IO], B[IO] is parameters will be propagated.
  *   D: Downwards (master -> slave)
  *   U: Upwards (slave -> master)
  *   E: Edge contains necessary of nearby nodes connection information.
  *   B: Bundle is [[chisel3.Data]], which you can attach chisel data type to it.
  *
  * */

/** enable [[InwardNodeImp.monitor]],
  * which is used by [[freechips.rocketchip.tilelink.TLMonitorBase]] to generate Monitor Module.
  * */
case object MonitorsEnabled extends Field[Boolean](true)

/** flip edge illustrated in the GraphML. */
case object RenderFlipped extends Field[Boolean](false)

/** [[RenderedEdge]] can set the color and label of the generated DAG graph. */
case class RenderedEdge(
  colour:  String,
  label:   String  = "",
  flipped: Boolean = false)

/** [[InwardNodeImp]] is the Slave Interface implementation
  *
  * @tparam DI Downwards Parameters received on the inner side of the node
  *            It is usually a brunch of parameters describing the protocol parameters from a master. For an InwardNode, it is determined by the connected OutwardNode
  *            Since it can be connected to multiple masters, this parameter is always a Seq of master port parameters.
  * @tparam UI Upwards flowing Parameters generated by the inner side of the node
  *            It is usually a brunch of parameters describing the protocol parameters of a slave. For an InwardNode, it is determined itself
  * @tparam EI Edge Parameters describing a connection on the inner side of the node
  *            It is usually a brunch of transfers specified for a slave according to protocol
  * @tparam BI Bundle type used when connecting to the inner side of the node
  *            It is a hardware interface of this slave interface
  * */
trait InwardNodeImp[DI, UI, EI, BI <: Data]
{
  /** @param pd client port parameters
    * @param pu manager port parameters
    * @param p Parameter of module
    * @param sourceInfo [[SourceInfo]] of this edge.
    * @return inward edge of this node.
    * */
  def edgeI(pd: DI, pu: UI, p: Parameters, sourceInfo: SourceInfo): EI

  /**
    * @param ei inward Edge of this node.
    * @return inward Bundle of this node
    */
  def bundleI(ei: EI): BI

  /** function to generate monitor of this node input.
    * @param bundle input bundle of this node
    * @param edge edge of this node
    * */
  def monitor(bundle: BI, edge: EI) {}
  /** used to render this edge in graphML
    * @param e inward edge of this node.
    * @return [[RenderedEdge]] for graphML generation
    * */
  def render(e: EI): RenderedEdge

  /** In parameter negotiation, it will be used to alter name to track this node.
    * @param pu manager port parameters
    * @param node this node, required to be this.(@todo need refactor?)
    * @return altered version of this node.
    * */
  def mixI(pu: UI, node: InwardNode[DI, UI, BI]): UI = pu
}

/** [[OutwardNodeImp]] is the Master Interface implementation
 *
 * @tparam DO Downwards flowing Parameters generated on the outer side of the node
 *            It is usually a brunch of parameters describing the protocol parameters of a master. For an OutwardNode, it is determined itself
 * @tparam UO Upwards flowing Parameters received by the outer side of the node
 *            It is usually a brunch of parameters describing the protocol parameters from a slave. For an OutwardNode, it is determined by the connected InwardNode
 *            Since it can be connected to multiple slaves, this parameter is always a Seq of slave port parameters.
 * @tparam EO Edge Parameters describing a connection on the outer side of the node
 *            It is usually a brunch of transfers specified for a master according to protocol
 * @tparam BO Bundle type used when connecting to the outer side of the node
 *            It is a hardware interface of this master interface
 * */
trait OutwardNodeImp[DO, UO, EO, BO <: Data]
{
  /** @param pd client port parameters
    * @param pu manager port parameters
    * @param p Parameter of module
    * @param sourceInfo [[SourceInfo]] of this edge.
    * @return outward edge of this node.
    * */
  def edgeO(pd: DO, pu: UO, p: Parameters, sourceInfo: SourceInfo): EO
  /**
    * @param eo outward Edge of this node.
    * @return outward Bundle of this node
    */
  def bundleO(eo: EO): BO

  /** In parameter negotiation, it will be used to alter name to track this node.
    * @param pd client port parameters
    * @param node this node, required to be this.(@todo need refactor?)
    * @return altered version of this node.
    * */
  def mixO(pd: DO, node: OutwardNode[DO, UO, BO]): DO = pd
  /** Nerver used, @todo need refactor?
    * @param pd client port parameters
    * */
  def getI(pd: DO): Option[BaseNode] = None // most-inward common node
}

/** [[NodeImp]] contains Master and Slave Interface implementation,
  * it describes protocol properties of a kind of Node, which is always directly derived from interconnection protocol.
  *
  * @tparam D Downwards flowing Parameters of the node
  * @tparam U Upwards flowing Parameters of the node
  * @tparam EO Edge Parameters describing a connection on the outer side of the node
  * @tparam EI Edge Parameters describing a connection on the inner side of the node
  * @tparam B Bundle type used when connecting the node
  * */
abstract class NodeImp[D, U, EO, EI, B <: Data]
  extends Object with InwardNodeImp[D, U, EI, B] with OutwardNodeImp[D, U, EO, B]

/** [[SimpleNodeImp]] has a simple master and slave interface implementation, edge and bundle of which are same.
  * If only one direction of transaction is permitted, we can always use the [[SimpleNodeImp]]
  *
  * For example, [[freechips.rocketchip.interrupts.IntImp]] is an interrupt transfer protocol,
  * in which all signals shall flow from a master to a slave.
  *
  * @tparam D Downwards flowing Parameters of the node
  * @tparam U Upwards flowing Parameters of the node
  * @tparam E Edge Parameters describing a connection on the both side of the node
  * @tparam B Bundle type used when connecting the node
  **/
abstract class SimpleNodeImp[D, U, E, B <: Data]
  extends NodeImp[D, U, E, E, B]
{

  /** Define this edge implementation, will be applied to [[edgeO]] and [[edgeI]]
    * @param pd client port parameters
    * @param pu manager port parameters
    * @param p Parameter of module
    * @param sourceInfo [[SourceInfo]] of this edge.
    * @return outward edge of this node.
    * */
  def edge(pd: D, pu: U, p: Parameters, sourceInfo: SourceInfo): E
  def edgeO(pd: D, pu: U, p: Parameters, sourceInfo: SourceInfo) = edge(pd, pu, p, sourceInfo)
  def edgeI(pd: D, pu: U, p: Parameters, sourceInfo: SourceInfo) = edge(pd, pu, p, sourceInfo)

  /** Define Bundle generation method of this node. will be apply to [[bundleI]] and [[bundleO]]
    * @param e Edge of this node.
    * @return Bundle of this node
    */
  def bundle(e: E): B
  def bundleO(e: E) = bundle(e)
  def bundleI(e: E) = bundle(e)
}

/** [[BaseNode]] is the base abstraction class of diplomacy node system. */
abstract class BaseNode(implicit val valName: ValName)
{
  /** extract [[LazyModule]] scope from upside. */
  val scope = LazyModule.scope
  /** node [[index]] for a [[LazyModule]]*/
  val index = scope.map(_.nodes.size).getOrElse(0)
  /** @return the [[LazyModule]] which initiate this [[BaseNode]]*/
  def lazyModule = scope.get
  /** append this node to [[LazyModule.scope]]. */
  scope.foreach { lm => lm.nodes = this :: lm.nodes }

  /** access singleton [[BaseNode]] serial to set its own serial, and update [[BaseNode.serial]] */
  val serial = BaseNode.serial
  BaseNode.serial = BaseNode.serial + 1

  /** @return a sequence of [[Dangle]] of this node,
    * [[LazyModuleImpLike.instantiate]] will use which to generate IO.
    * */
  protected[diplomacy] def instantiate(): Seq[Dangle]

  /** A callback to finish the node generation,
   * This will be executed in [[LazyModuleImpLike.instantiate]].
   * */
  protected[diplomacy] def finishInstantiate(): Unit

  /** @return name of this node*/
  def name: String = scope.map(_.name).getOrElse("TOP") + "." + valName.name
  /** accessed by LazyModule in generating Graph, if true, graph generation will be omitted this node*/
  def omitGraphML: Boolean = outputs.isEmpty && inputs.isEmpty
  lazy val nodedebugstring: String = ""

  /** @return a sequence of [[LazyModule]] till Top*/
  def parents: Seq[LazyModule] = scope.map(lm => lm +: lm.parents).getOrElse(Nil)
  /** @return the description string for debug.*/
  def context: String =
    s"$name (A $description node with parent '" +
    parents.map(_.name).mkString("/") + "' at " +
    scope.map(_.line).getOrElse("<undef>") + ")"

  /** Determines the name to be used in elements of auto-punched bundles
    * by taking the name of the node as determined from valName,
    * converting camel case into snake case, and stripping "Node" or "NodeOpt" suffixes.
    */
  def wirePrefix = {
    val camelCase = "([a-z])([A-Z])".r
    val decamel = camelCase.replaceAllIn(valName.name, _ match { case camelCase(l, h) => l + "_" + h })
    val name = decamel.toLowerCase.stripSuffix("_opt").stripSuffix("node").stripSuffix("_")
    if (name.isEmpty) "" else name + "_"
  }

  /** @return Node description, which should defined by user*/
  def description: String
  def formatNode: String = ""

  /** @return metedata to print input graph. */
  def inputs:  Seq[(BaseNode, RenderedEdge)]

  /** @return metedata to print output graph. */
  def outputs: Seq[(BaseNode, RenderedEdge)]

  protected[diplomacy] def flexibleArityDirection: Boolean = false
  protected[diplomacy] val sinkCard: Int
  protected[diplomacy] val sourceCard: Int
  protected[diplomacy] val flexes: Seq[BaseNode]
  protected[diplomacy] val flexOffset: Int
}

/** singleton for [[BaseNode]], which is only used as the global serial number.*/
object BaseNode
{
  protected[diplomacy] var serial = 0
}

trait FormatEdge {
  def formatEdge: String
}

/** Define GraphML metadata. */
trait FormatNode[I <: FormatEdge, O <: FormatEdge] extends BaseNode {
  def edges: Edges[I,O]
  /** used in [[LazyModule.nodesGraphML]]. */
  override def formatNode = {
    edges.out.map(currEdge =>
      "On Output Edge:\n\n" + currEdge.formatEdge).mkString +
    "\n---------------------------------------------\n\n" +
    edges.in.map(currEdge =>
      "On Input Edge:\n\n" + currEdge.formatEdge).mkString
  }
}

trait NoHandle
case object NoHandleObject extends NoHandle

trait NodeHandle[DI, UI, EI, BI <: Data, DO, UO, EO, BO <: Data]
  extends InwardNodeHandle[DI, UI, EI, BI] with OutwardNodeHandle[DO, UO, EO, BO]
{
  /** [[BIND_ONCE]] this node as slave, [[BIND_ONCE]] that node as master.
    * @param h master node also with slave handle
    * @return a [[NodeHandle]] with that node as inwardNode, this node as outwardNode
    * */
  override def :=  [DX, UX, EX, BX <: Data, EY](h: NodeHandle[DX, UX, EX, BX, DI, UI, EY, BI])(implicit p: Parameters, sourceInfo: SourceInfo): NodeHandle[DX, UX, EX, BX, DO, UO, EO, BO] = { bind(h, BIND_ONCE);  NodeHandle(h, this) }
  /** [[BIND_STAR]] this node as slave, [[BIND_QUERY]] that node as master.
    * @param h master node also with slave handle
    * @return a [[NodeHandle]] with that node as inwardNode, this node as outwardNode
    * */
  override def :*= [DX, UX, EX, BX <: Data, EY](h: NodeHandle[DX, UX, EX, BX, DI, UI, EY, BI])(implicit p: Parameters, sourceInfo: SourceInfo): NodeHandle[DX, UX, EX, BX, DO, UO, EO, BO] = { bind(h, BIND_STAR);  NodeHandle(h, this) }
  /** [[BIND_QUERY]] this node as slave, [[BIND_STAR]] that node as master.
    * @param h master node also with slave handle
    * @return a [[NodeHandle]] with that node as inwardNode, this node as outwardNode
    * */
  override def :=* [DX, UX, EX, BX <: Data, EY](h: NodeHandle[DX, UX, EX, BX, DI, UI, EY, BI])(implicit p: Parameters, sourceInfo: SourceInfo): NodeHandle[DX, UX, EX, BX, DO, UO, EO, BO] = { bind(h, BIND_QUERY); NodeHandle(h, this) }
  /** [[BIND_FLEX]] this node as slave, [[BIND_FLEX]] that node as master.
    * @param h master node also with slave handle
    * @return a [[NodeHandle]] with that node as inwardNode, this node as outwardNode
    * */
  override def :*=*[DX, UX, EX, BX <: Data, EY](h: NodeHandle[DX, UX, EX, BX, DI, UI, EY, BI])(implicit p: Parameters, sourceInfo: SourceInfo): NodeHandle[DX, UX, EX, BX, DO, UO, EO, BO] = { bind(h, BIND_FLEX);  NodeHandle(h, this) }
  /** [[BIND_ONCE]] this node as slave, [[BIND_ONCE]] that node as master.
    * @param h master node also without slave handle
    * @return a [[OutwardNodeHandle]] with this node as outwardNode
    * */
  override def :=  [EY](h: OutwardNodeHandle[DI, UI, EY, BI])(implicit p: Parameters, sourceInfo: SourceInfo): OutwardNodeHandle[DO, UO, EO, BO] = { bind(h, BIND_ONCE);  this }
  /** [[BIND_STAR]] this node as slave, [[BIND_QUERY]] that node as master.
    * @param h master node also without slave handle
    * @return a [[OutwardNodeHandle]] with this node as outwardNode
    * */
  override def :*= [EY](h: OutwardNodeHandle[DI, UI, EY, BI])(implicit p: Parameters, sourceInfo: SourceInfo): OutwardNodeHandle[DO, UO, EO, BO] = { bind(h, BIND_STAR);  this }
  /** [[BIND_QUERY]] this node as slave, [[BIND_STAR]] that node as master.
    * @param h master node also without slave handle
    * @return a [[OutwardNodeHandle]] with this node as outwardNode
    * */
  override def :=* [EY](h: OutwardNodeHandle[DI, UI, EY, BI])(implicit p: Parameters, sourceInfo: SourceInfo): OutwardNodeHandle[DO, UO, EO, BO] = { bind(h, BIND_QUERY); this }
  /** [[BIND_FLEX]] this node as slave, [[BIND_FLEX]] that node as master.
    * @param h master node also without slave handle
    * @return a [[OutwardNodeHandle]] with this node as outwardNode
    * */
  override def :*=*[EY](h: OutwardNodeHandle[DI, UI, EY, BI])(implicit p: Parameters, sourceInfo: SourceInfo): OutwardNodeHandle[DO, UO, EO, BO] = { bind(h, BIND_FLEX);  this }
}

object NodeHandle
{
  /** generate a [[NodeHandlePair]]
    * @param i master node
    * @param o slave node
    * @return [[NodeHandle]] with inwardNode of i, outwardNode of o.
    * */
  def apply[DI, UI, EI, BI <: Data, DO, UO, EO, BO <: Data](i: InwardNodeHandle[DI, UI, EI, BI], o: OutwardNodeHandle[DO, UO, EO, BO]) = new NodeHandlePair(i, o)
}

/** [[NodeHandle]] with inward and outward handle*/
class NodeHandlePair[DI, UI, EI, BI <: Data, DO, UO, EO, BO <: Data]
  (inwardHandle: InwardNodeHandle[DI, UI, EI, BI], outwardHandle: OutwardNodeHandle[DO, UO, EO, BO])
  extends NodeHandle[DI, UI, EI, BI, DO, UO, EO, BO]
{

  val inward = inwardHandle.inward
  val outward = outwardHandle.outward
  /** @return [[NodeImp]] of [[inwardHandle]]. */
  def inner = inwardHandle.inner
  /** @return [[NodeImp]] of [[outwardHandle]]. */
  def outer = outwardHandle.outer
}

/** [[NodeHandle]] with inward handle*/
trait InwardNodeHandle[DI, UI, EI, BI <: Data] extends NoHandle
{
  /** @return [[InwardNode]] of inwardHandle. */
  def inward: InwardNode[DI, UI, BI]
  /** @return [[NodeImp]] of inwardHandle. */
  def inner: InwardNodeImp[DI, UI, EI, BI]

  /** bind this node to a outword node. */
  protected def bind[EY](h: OutwardNodeHandle[DI, UI, EY, BI], binding: NodeBinding)(implicit p: Parameters, sourceInfo: SourceInfo): Unit = inward.bind(h.outward, binding)

  /** [[BIND_ONCE]] this node as slave, [[BIND_ONCE]] that node as master.
    * @param h master node also with slave handle
    * @return a [[NodeHandle]] with that node as inwardNode, this node as outwardNode
    * */
  def :=  [DX, UX, EX, BX <: Data, EY](h: NodeHandle[DX, UX, EX, BX, DI, UI, EY, BI])(implicit p: Parameters, sourceInfo: SourceInfo): InwardNodeHandle[DX, UX, EX, BX] = { bind(h, BIND_ONCE);  h }
  /** [[BIND_STAR]] this node as slave, [[BIND_QUERY]] that node as master.
    * @param h master node also with slave handle
    * @return a [[NodeHandle]] with that node as inwardNode, this node as outwardNode
    * */
  def :*= [DX, UX, EX, BX <: Data, EY](h: NodeHandle[DX, UX, EX, BX, DI, UI, EY, BI])(implicit p: Parameters, sourceInfo: SourceInfo): InwardNodeHandle[DX, UX, EX, BX] = { bind(h, BIND_STAR);  h }
  /** [[BIND_QUERY]] this node as slave, [[BIND_STAR]] that node as master.
    * @param h master node also with slave handle
    * @return a [[NodeHandle]] with that node as inwardNode, this node as outwardNode
    * */
  def :=* [DX, UX, EX, BX <: Data, EY](h: NodeHandle[DX, UX, EX, BX, DI, UI, EY, BI])(implicit p: Parameters, sourceInfo: SourceInfo): InwardNodeHandle[DX, UX, EX, BX] = { bind(h, BIND_QUERY); h }
  /** [[BIND_FLEX]] this node as slave, [[BIND_FLEX]] that node as master.
    * @param h master node also with slave handle
    * @return a [[NodeHandle]] with that node as inwardNode, this node as outwardNode
    * */
  def :*=*[DX, UX, EX, BX <: Data, EY](h: NodeHandle[DX, UX, EX, BX, DI, UI, EY, BI])(implicit p: Parameters, sourceInfo: SourceInfo): InwardNodeHandle[DX, UX, EX, BX] = { bind(h, BIND_FLEX);  h }
  /** [[BIND_ONCE]] this node as slave, [[BIND_ONCE]] that node as master.
    * @param h master node also without slave handle.
    * @return [[NoHandle]] since both side cannot bind other node.
    * */
  def :=  [EY](h: OutwardNodeHandle[DI, UI, EY, BI])(implicit p: Parameters, sourceInfo: SourceInfo): NoHandle = { bind(h, BIND_ONCE);  NoHandleObject }
  /** [[BIND_STAR]] this node as slave, [[BIND_QUERY]] that node as master.
    * @param h master node also without slave handle.
    * @return [[NoHandle]] since both side cannot bind other node.
    * */
  def :*= [EY](h: OutwardNodeHandle[DI, UI, EY, BI])(implicit p: Parameters, sourceInfo: SourceInfo): NoHandle = { bind(h, BIND_STAR);  NoHandleObject }
  /** [[BIND_QUERY]] this node as slave, [[BIND_STAR]] that node as master.
    * @param h master node also without slave handle.
    * @return [[NoHandle]] since both side cannot bind other node.
    * */
  def :=* [EY](h: OutwardNodeHandle[DI, UI, EY, BI])(implicit p: Parameters, sourceInfo: SourceInfo): NoHandle = { bind(h, BIND_QUERY); NoHandleObject }
  /** [[BIND_FLEX]] this node as slave, [[BIND_FLEX]] that node as master.
    * @param h master node also without slave handle.
    * @return [[NoHandle]] since both side cannot bind other node.
    * */
  def :*=*[EY](h: OutwardNodeHandle[DI, UI, EY, BI])(implicit p: Parameters, sourceInfo: SourceInfo): NoHandle = { bind(h, BIND_FLEX);  NoHandleObject }
}

sealed trait NodeBinding
/** only bind one connection. */
case object BIND_ONCE  extends NodeBinding
/** it will bind N (N >= 0) connections,
  * query another node which N should be applied.
  * */
case object BIND_QUERY extends NodeBinding
/** it will bind N (N >= 0) connections,
  * greedy bind available nodes on this node.
  * */
case object BIND_STAR  extends NodeBinding
/** it will bind N (N >= 0) connections,
  * not sure to query which side.
  * */
case object BIND_FLEX  extends NodeBinding

trait InwardNode[DI, UI, BI <: Data] extends BaseNode
{
  /** when binding this node to another nodes,
    * [[accPI]] will append another node
    * */
  private val accPI = ListBuffer[(Int, OutwardNode[DI, UI, BI], NodeBinding, Parameters, SourceInfo)]()

  /** mark [[iBindings]] has been implemented.*/
  private var iRealized = false

  /** current size of [[accPI]]. [[InwardNode.bind]] will find it useful in indexing.*/
  protected[diplomacy] def iPushed = accPI.size

  /** call by [[InwardNode.bind]] to bind this node to that
    * @param index index of current.
    * @param node node to bind now.
    * @param binding Binding type.
    * */
  protected[diplomacy] def iPush(index: Int, node: OutwardNode[DI, UI, BI], binding: NodeBinding)(implicit p: Parameters, sourceInfo: SourceInfo) {
    val info = sourceLine(sourceInfo, " at ", "")
    require (!iRealized, s"${context} was incorrectly connected as a sink after its .module was used" + info)
    accPI += ((index, node, binding, p, sourceInfo))
  }

  /** It's the final result of [[accPI]].
    * after calling this, [[iPush]] will reject to add more nodes.
    * */
  protected[diplomacy] lazy val iBindings = { iRealized = true; accPI.result() }

  /** resolved STAR binding of inward nodes. */
  protected[diplomacy] val iStar: Int
  protected[diplomacy] val iPortMapping: Seq[(Int, Int)]
  protected[diplomacy] def iForward(x: Int): Option[(Int, InwardNode[DI, UI, BI])] = None
  protected[diplomacy] val diParams: Seq[DI] // from connected nodes
  protected[diplomacy] val uiParams: Seq[UI] // from this node

  protected[diplomacy] def bind(h: OutwardNode[DI, UI, BI], binding: NodeBinding)(implicit p: Parameters, sourceInfo: SourceInfo): Unit
}

trait OutwardNodeHandle[DO, UO, EO, BO <: Data] extends NoHandle
{
  def outward: OutwardNode[DO, UO, BO]
  def outer: OutwardNodeImp[DO, UO, EO, BO]
}

trait OutwardNode[DO, UO, BO <: Data] extends BaseNode
{
  /** when connecting this node to other nodes,
    * [[accPO]] will append other nodes's information.
    * */
  private val accPO = ListBuffer[(Int, InwardNode [DO, UO, BO], NodeBinding, Parameters, SourceInfo)]()
  private var oRealized = false

  /** current size of [[accPO]]. */
  protected[diplomacy] def oPushed = accPO.size

  /** call by [[InwardNode.bind]] to bind this node to it
    * @param index index of current.
    * @param node node to bind now.
    * @param binding Binding type.
    * */
  protected[diplomacy] def oPush(index: Int, node: InwardNode [DO, UO, BO], binding: NodeBinding)(implicit p: Parameters, sourceInfo: SourceInfo) {
    val info = sourceLine(sourceInfo, " at ", "")
    require (!oRealized, s"${context} was incorrectly connected as a source after its .module was used" + info)
    accPO += ((index, node, binding, p, sourceInfo))
  }

  /** It's the final result of [[accPO]].
    * after calling this, [[oPush]] will reject to add more nodes.
    * */
  protected[diplomacy] lazy val oBindings = { oRealized = true; accPO.result() }

  /** resolved STAR binding of outward nodes. */
  protected[diplomacy] val oStar: Int
  protected[diplomacy] val oPortMapping: Seq[(Int, Int)]
  protected[diplomacy] def oForward(x: Int): Option[(Int, OutwardNode[DO, UO, BO])] = None
  protected[diplomacy] val uoParams: Seq[UO] // from connected nodes
  protected[diplomacy] val doParams: Seq[DO] // from this node
}

abstract class CycleException(kind: String, loop: Seq[String]) extends Exception(s"Diplomatic ${kind} cycle detected involving ${loop}")
case class StarCycleException(loop: Seq[String] = Nil) extends CycleException("star", loop)
case class DownwardCycleException(loop: Seq[String] = Nil) extends CycleException("downward", loop)
case class UpwardCycleException(loop: Seq[String] = Nil) extends CycleException("upward", loop)

/** [[Edges]] is a collection of functions describing the functionality and connection for an interface,
  * which is often derived from interconnection protocol.
  * */
case class Edges[EI, EO](in: Seq[EI], out: Seq[EO])

/** The sealed Node in the package, all node are derived from it.
  * @param inner slave interface implementation
  * @param outer master interface implementation
  * @param valName val name of this node
  * @tparam DI Downwards Parameters received on the inner side of the node
  *            It is usually a brunch of parameters describing the protocol parameters from a master. For an InwardNode, it is determined by the connected OutwardNode
  *            Since it can be connected to multiple masters, this parameter is always a Seq of master port parameters.
  * @tparam UI Upwards flowing Parameters generated by the inner side of the node
  *            It is usually a brunch of parameters describing the protocol parameters of a slave. For an InwardNode, it is determined itself
  * @tparam EI Edge Parameters describing a connection on the inner side of the node
  *            It is usually a brunch of transfers specified for a slave according to protocol
  * @tparam BI Bundle type used when connecting to the inner side of the node
  *            It is a hardware interface of this slave interface
  * @tparam DO Downwards flowing Parameters generated on the outer side of the node
  *            It is usually a brunch of parameters describing the protocol parameters of a master. For an OutwardNode, it is determined itself
  * @tparam UO Upwards flowing Parameters received by the outer side of the node
  *            It is usually a brunch of parameters describing the protocol parameters from a slave. For an OutwardNode, it is determined by the connected InwardNode
  *            Since it can be connected to multiple slaves, this parameter is always a Seq of slave port parameters.
  * @tparam EO Edge Parameters describing a connection on the outer side of the node
  *            It is usually a brunch of transfers specified for a master according to protocol
  * @tparam BO Bundle type used when connecting to the outer side of the node
  *            It is a hardware interface of this master interface
  */
sealed abstract class MixedNode[DI, UI, EI, BI <: Data, DO, UO, EO, BO <: Data](
  val inner: InwardNodeImp [DI, UI, EI, BI],
  val outer: OutwardNodeImp[DO, UO, EO, BO])(
  implicit valName: ValName)
  extends BaseNode with NodeHandle[DI, UI, EI, BI, DO, UO, EO, BO] with InwardNode[DI, UI, BI] with OutwardNode[DO, UO, BO]
{
  /** generate a [[NodeHandle]] with inward and outward node are both this node. */
  val inward = this
  val outward = this

 /** converts [[OutwardNode.accPO]] and [[InwardNode.accPI]] to [[MixedNode.oPortMapping]] and [[MixedNode.iPortMapping]].
    * @param iKnown number of master nodes no need to query
    * @param oKnown number of slave nodes no need to query
    * @param iStar number of master nodes need to query
    * @param oStar number of slave nodes need to query
    * @return
    * */
  protected[diplomacy] def resolveStar(iKnown: Int, oKnown: Int, iStar: Int, oStar: Int): (Int, Int)
  /** function to generate [[doParams]] from [[oPorts]] and [[diParams]]
    * @todo since only one usage of this, maybe can refactor?
    * @param n size of [[oPorts]].
    * @param p [[diParams]] of this node.
    * @return [[doParams]]
    * */
  protected[diplomacy] def mapParamsD(n: Int, p: Seq[DI]): Seq[DO]
  /** function to generate [[uiParams]] from [[iPorts]] and [[uoParams]]
    * @todo since only one usage of this, maybe can refactor?
    * @param n size of [[iPorts]]
    * @param p [[uoParams]] of this node.
    * @return [[uiParams]]
    * */
  protected[diplomacy] def mapParamsU(n: Int, p: Seq[UO]): Seq[UI]
  /** number of query from down to top near this node. */
  protected[diplomacy] lazy val sinkCard   = oBindings.count(_._3 == BIND_QUERY) + iBindings.count(_._3 == BIND_STAR)
  /** number of query from top to down near this node. */
  protected[diplomacy] lazy val sourceCard = iBindings.count(_._3 == BIND_QUERY) + oBindings.count(_._3 == BIND_STAR)
  /** number of flex near this node. */
  protected[diplomacy] lazy val flexes     = oBindings.filter(_._3 == BIND_FLEX).map(_._2) ++
                                             iBindings.filter(_._3 == BIND_FLEX).map(_._2)
  /** positive = sink cardinality; define 0 to be sink (both should work)
    * @todo maybe use Boolean is better here since this index only used with:
    * */
  protected[diplomacy] lazy val flexOffset = {
    def DFS(v: BaseNode, visited: Map[Int, BaseNode]): Map[Int, BaseNode] = {
      if (visited.contains(v.serial) || !v.flexibleArityDirection) {
        visited
      } else {
        v.flexes.foldLeft(visited + (v.serial -> v))((sum, n) => DFS(n, sum))
      }
    }
    /** a partial flex set connect to this node, for example
      * {{{
      *   a :*=* b :*=* c
      *   d :*=* b
      *   e :*=* f
      * }}}
      * `flexSet` of `a`, `b`, `c`, `d` will be `Set(a, b, c, d)`
      * `flexSet` of `e`, `f` will be `Set(e,f)`
      * */
    val flexSet = DFS(this, Map()).values
    /** total size of query from down to top in this flex chain */
    val allSink   = flexSet.map(_.sinkCard).sum
    /** total size of query from top to down in this flex chain */
    val allSource = flexSet.map(_.sourceCard).sum
    /** require the flexSet containing this node query in one direction.
      * @todo flexSet.size seem not possible to be 1? */
    require (flexSet.size == 1 || allSink == 0 || allSource == 0,
      s"The nodes ${flexSet.map(_.name)} which are inter-connected by :*=* have ${allSink} :*= operators and ${allSource} :=* operators connected to them, making it impossible to determine cardinality inference direction.")
    allSink - allSource
  }

  protected[diplomacy] def edgeArityDirection(n: BaseNode): Int = {
    if (  flexibleArityDirection)   flexOffset else
    if (n.flexibleArityDirection) n.flexOffset else
    0
  }

  protected[diplomacy] def edgeAritySelect(n: BaseNode, l: => Int, r: => Int): Int = {
    val dir = edgeArityDirection(n)
    if (dir < 0) l else if (dir > 0) r else 1
  }

  private var starCycleGuard = false
  
  protected[diplomacy] lazy val (oPortMapping, iPortMapping, oStar, iStar) = {
    try {
      if (starCycleGuard) throw StarCycleException()
      starCycleGuard = true
      /** number of [[BIND_STAR]] in [[OutwardNode]]s connected to this node. */
      val oStars = oBindings.count { case (_,n,b,_,_) => b == BIND_STAR || (b == BIND_FLEX && edgeArityDirection(n) < 0) }
      /** number of [[BIND_STAR]] in [[InwardNode]]s connected to this node. */
      val iStars = iBindings.count { case (_,n,b,_,_) => b == BIND_STAR || (b == BIND_FLEX && edgeArityDirection(n) > 0) }
      /** number of known node in [[OutwardNode]]s connected to this node. */
      val oKnown = oBindings.map { case (_, n, b, _, _) => b match {
        case BIND_ONCE  => 1
        case BIND_FLEX  => edgeAritySelect(n, 0, n.iStar)
        case BIND_QUERY => n.iStar
        case BIND_STAR  => 0 }}.foldLeft(0)(_+_)
      /** number of known node in [[InwardNode]]s connected to this node.
        * @todo why oStar
        * */
      val iKnown = iBindings.map { case (_, n, b, _, _) => b match {
        case BIND_ONCE  => 1
        case BIND_FLEX  => edgeAritySelect(n, n.oStar, 0)
        case BIND_QUERY => n.oStar
        case BIND_STAR  => 0 }}.foldLeft(0)(_+_)
      val (iStar, oStar) = resolveStar(iKnown, oKnown, iStars, oStars)
      /** cumulative list of resolved outward nodes number. */
      val oSum = oBindings.map { case (_, n, b, _, _) => b match {
        case BIND_ONCE  => 1
        case BIND_FLEX  => edgeAritySelect(n, oStar, n.iStar)
        case BIND_QUERY => n.iStar
        case BIND_STAR  => oStar }}.scanLeft(0)(_+_)
      /** cumulative list of resolved inward nodes number. */
      val iSum = iBindings.map { case (_, n, b, _, _) => b match {
        case BIND_ONCE  => 1
        case BIND_FLEX  => edgeAritySelect(n, n.oStar, iStar)
        case BIND_QUERY => n.oStar
        case BIND_STAR  => iStar }}.scanLeft(0)(_+_)
      /** @todo [[oTotal]] and [[iTotal]] is not used. */
      val oTotal = oSum.lastOption.getOrElse(0)
      val iTotal = iSum.lastOption.getOrElse(0)
      (oSum.init zip oSum.tail, iSum.init zip iSum.tail, oStar, iStar)
    } catch {
      case c: StarCycleException => throw c.copy(loop = context +: c.loop)
    }
  }

  protected[diplomacy] lazy val oDirectPorts = oBindings.flatMap { case (i, n, _, p, s) =>
    val (start, end) = n.iPortMapping(i)
    (start until end) map { j => (j, n, p, s) }
  }
  protected[diplomacy] lazy val iDirectPorts = iBindings.flatMap { case (i, n, _, p, s) =>
    val (start, end) = n.oPortMapping(i)
    (start until end) map { j => (j, n, p, s) }
  }

  // Ephemeral nodes have in_degree = out_degree
  // Thus, there must exist an Eulerian path and the below algorithms terminate
  private def oTrace(tuple: (Int, InwardNode[DO, UO, BO], Parameters, SourceInfo)): (Int, InwardNode[DO, UO, BO], Parameters, SourceInfo) =
    tuple match { case (i, n, p, s) => n.iForward(i) match {
      case None => (i, n, p, s)
      case Some ((j, m)) => oTrace((j, m, p, s))
    } }
  private def iTrace(tuple: (Int, OutwardNode[DI, UI, BI], Parameters, SourceInfo)): (Int, OutwardNode[DI, UI, BI], Parameters, SourceInfo) =
    tuple match { case (i, n, p, s) => n.oForward(i) match {
      case None => (i, n, p, s)
      case Some ((j, m)) => iTrace((j, m, p, s))
    } }
  lazy val oPorts = oDirectPorts.map(oTrace)
  lazy val iPorts = iDirectPorts.map(iTrace)

  private var oParamsCycleGuard = false
  protected[diplomacy] lazy val diParams: Seq[DI] = iPorts.map { case (i, n, _, _) => n.doParams(i) }
  protected[diplomacy] lazy val doParams: Seq[DO] = {
    try {
      if (oParamsCycleGuard) throw DownwardCycleException()
      oParamsCycleGuard = true
      val o = mapParamsD(oPorts.size, diParams)
      require (o.size == oPorts.size, s"Diplomacy error: $context has ${o.size} != ${oPorts.size} down/up outer parameters")
      o.map(outer.mixO(_, this))
    } catch {
      case c: DownwardCycleException => throw c.copy(loop = context +: c.loop)
    }
  }

  private var iParamsCycleGuard = false
  protected[diplomacy] lazy val uoParams: Seq[UO] = oPorts.map { case (o, n, _, _) => n.uiParams(o) }
  protected[diplomacy] lazy val uiParams: Seq[UI] = {
    try {
      if (iParamsCycleGuard) throw UpwardCycleException()
      iParamsCycleGuard = true
      val i = mapParamsU(iPorts.size, uoParams)
      require (i.size == iPorts.size, s"Diplomacy error: $context has ${i.size} != ${iPorts.size} up/down inner parameters")
      i.map(inner.mixI(_, this))
    } catch {
      case c: UpwardCycleException => throw c.copy(loop = context +: c.loop)
    }
  }

  protected[diplomacy] lazy val edgesOut = (oPorts zip doParams).map { case ((i, n, p, s), o) => outer.edgeO(o, n.uiParams(i), p, s) }
  protected[diplomacy] lazy val edgesIn  = (iPorts zip uiParams).map { case ((o, n, p, s), i) => inner.edgeI(n.doParams(o), i, p, s) }

  // If you need access to the edges of a foreign Node, use this method (in/out create bundles)
  lazy val edges = Edges(edgesIn, edgesOut)

  // These need to be chisel3.Wire because Chisel.Wire assigns Reset to a default value of Bool,
  // and FIRRTL will not allow a Reset assigned to Bool to later be assigned to AsyncReset.
  // If the diplomatic Bundle contains Resets this will hamstring them into synchronous resets.
  // The jury is still out on whether the lack of ability to override the reset type
  // is a  Chisel/firrtl bug or whether this should be supported,
  // but as of today it does not work to do so.
  protected[diplomacy] lazy val bundleOut: Seq[BO] = edgesOut.map(e => chisel3.Wire(outer.bundleO(e)))
  protected[diplomacy] lazy val bundleIn:  Seq[BI] = edgesIn .map(e => chisel3.Wire(inner.bundleI(e)))

  protected[diplomacy] def danglesOut: Seq[Dangle] = oPorts.zipWithIndex.map { case ((j, n, _, _), i) =>
    Dangle(
      source = HalfEdge(serial, i),
      sink   = HalfEdge(n.serial, j),
      flipped= false,
      name   = wirePrefix + "out",
      data   = bundleOut(i))
  }
  protected[diplomacy] def danglesIn: Seq[Dangle] = iPorts.zipWithIndex.map { case ((j, n, _, _), i) =>
    Dangle(
      source = HalfEdge(n.serial, j),
      sink   = HalfEdge(serial, i),
      flipped= true,
      name   = wirePrefix + "in",
      data   = bundleIn(i))
  }

  private var bundlesSafeNow = false
  /** Accessors to the result of negotiation to be used in [[LazyModuleImp]]. */
  def out: Seq[(BO, EO)] = {
    require(bundlesSafeNow, s"${name}.out should only be called from the context of its module implementation")
    bundleOut zip edgesOut
  }
  def in: Seq[(BI, EI)] = {
    require(bundlesSafeNow, s"${name}.in should only be called from the context of its module implementation")
    bundleIn zip edgesIn
  }

  // Used by LazyModules.module.instantiate
  protected val identity = false
  protected[diplomacy] def instantiate() = {
    bundlesSafeNow = true
    if (!identity) {
      (iPorts zip in) foreach {
        case ((_, _, p, _), (b, e)) => if (p(MonitorsEnabled)) inner.monitor(b, e)
    } }
    danglesOut ++ danglesIn
  }

  protected[diplomacy] def finishInstantiate() = {
    bundlesSafeNow = false
  }

  // connects the outward part of a node with the inward part of this node
  protected[diplomacy] def bind(h: OutwardNode[DI, UI, BI], binding: NodeBinding)(implicit p: Parameters, sourceInfo: SourceInfo) {
    val x = this // x := y
    val y = h
    val info = sourceLine(sourceInfo, " at ", "")
    val i = x.iPushed
    val o = y.oPushed
    y.oPush(i, x, binding match {
      case BIND_ONCE  => BIND_ONCE
      case BIND_FLEX  => BIND_FLEX
      case BIND_STAR  => BIND_QUERY
      case BIND_QUERY => BIND_STAR })
    x.iPush(o, y, binding)
  }

  // meta-data for printing the node graph
  def inputs = (iPorts zip edgesIn) map { case ((_, n, p, _), e) =>
    val re = inner.render(e)
    (n, re.copy(flipped = re.flipped != p(RenderFlipped)))
  }
  def outputs = oPorts map { case (i, n, _, _) => (n, n.inputs(i)._2) }
}

/** If designer wanna do the funky jobs, just extend the Node.
  * It's the external wrapper of the [[CustomNode]]
  */
abstract class MixedCustomNode[DI, UI, EI, BI <: Data, DO, UO, EO, BO <: Data](
  inner: InwardNodeImp [DI, UI, EI, BI],
  outer: OutwardNodeImp[DO, UO, EO, BO])(
  implicit valName: ValName)
  extends MixedNode(inner, outer)
{
  override def description = "custom"
  def resolveStar(iKnown: Int, oKnown: Int, iStars: Int, oStars: Int): (Int, Int)
  def mapParamsD(n: Int, p: Seq[DI]): Seq[DO]
  def mapParamsU(n: Int, p: Seq[UO]): Seq[UI]
}

/** [[MixedCustomNode]] has a same [[NodeImp]] of inward and outward. */
abstract class CustomNode[D, U, EO, EI, B <: Data](imp: NodeImp[D, U, EO, EI, B])(
  implicit valName: ValName)
  extends MixedCustomNode(imp, imp)

/* A JunctionNode creates multiple parallel arbiters.
 * For example,
 *   val jbar = LazyModule(new JBar)
 *   slave1.node := jbar.node
 *   slave2.node := jbar.node
 *   extras.node :=* jbar.node
 *   jbar.node :*= masters1.node
 *   jbar.node :*= masters2.node
 * In the above example, only the first two connections have their multiplicity specified.
 * All the other connections include a '*' on the JBar's side, so the JBar decides the multiplicity.
 * Thus, in this example, we get 2x crossbars with 2 masters like this:
 *    {slave1, extras.1} <= jbar.1 <= {masters1.1, masters2.1}
 *    {slave2, extras.2} <= jbar.2 <= {masters1.2, masters2,2}
 * Here is another example:
 *   val jbar = LazyModule(new JBar)
 *   jbar.node :=* masters.node
 *   slaves1.node :=* jbar.node
 *   slaves2.node :=* jbar.node
 * In the above example, the first connection takes multiplicity (*) from the right (masters).
 * Supposing masters.node had 3 edges, this would result in these three arbiters:
 *   {slaves1.1, slaves2.1} <= jbar.1 <= { masters.1 }
 *   {slaves1.2, slaves2.2} <= jbar.2 <= { masters.2 }
 *   {slaves1.3, slaves2.3} <= jbar.3 <= { masters.3 }
 */
class MixedJunctionNode[DI, UI, EI, BI <: Data, DO, UO, EO, BO <: Data](
  inner: InwardNodeImp [DI, UI, EI, BI],
  outer: OutwardNodeImp[DO, UO, EO, BO])(
  dFn: Seq[DI] => Seq[DO],
  uFn: Seq[UO] => Seq[UI])(
  implicit valName: ValName)
  extends MixedNode(inner, outer)
{
  protected[diplomacy] var multiplicity = 0

  def uRatio = iPorts.size / multiplicity
  def dRatio = oPorts.size / multiplicity

  override def description = "junction"
  protected[diplomacy] def resolveStar(iKnown: Int, oKnown: Int, iStars: Int, oStars: Int): (Int, Int) = {
    require (iKnown == 0 || oKnown == 0, s"$context appears left of a :=* or a := AND right of a :*= or :=. Only one side may drive multiplicity.")
    multiplicity = iKnown max oKnown
   (multiplicity, multiplicity)
  }
  protected[diplomacy] def mapParamsD(n: Int, p: Seq[DI]): Seq[DO] =
    p.grouped(multiplicity).toList.transpose.map(dFn).transpose.flatten
  protected[diplomacy] def mapParamsU(n: Int, p: Seq[UO]): Seq[UI] =
    p.grouped(multiplicity).toList.transpose.map(uFn).transpose.flatten

  def inoutGrouped: Seq[(Seq[(BI, EI)], Seq[(BO, EO)])] = {
    val iGroups = in .grouped(multiplicity).toList.transpose
    val oGroups = out.grouped(multiplicity).toList.transpose
    iGroups zip oGroups
  }
}

/**
  * Junction nodes are a bit esoteric.
  * You need them when you need, for example, a 2:1 arbiter, but several times.
  * Suppose you had N banks of L2 and wanted to connect those to two different driver crossbars.
  * In that case you can do this:
  * {{{
  *   l2banks.node :*= jbar.node
  *   jbar.node :*= xbar1.node
  *   jbar.node :*= xbar2.node
  * }}}
  * If the L2 has 4 banks, now there are 4 egress ports on both xbar1 and xbar2 and they are arbitrated by the jbar.
  * @todo not understand.
  * */
class JunctionNode[D, U, EO, EI, B <: Data](imp: NodeImp[D, U, EO, EI, B])(
  dFn: Seq[D] => Seq[D],
  uFn: Seq[U] => Seq[U])(
  implicit valName: ValName)
    extends MixedJunctionNode[D, U, EI, B, D, U, EO, B](imp, imp)(dFn, uFn)

class MixedAdapterNode[DI, UI, EI, BI <: Data, DO, UO, EO, BO <: Data](
  inner: InwardNodeImp [DI, UI, EI, BI],
  outer: OutwardNodeImp[DO, UO, EO, BO])(
  dFn: DI => DO,
  uFn: UO => UI)(
  implicit valName: ValName)
  extends MixedNode(inner, outer)
{
  override def description = "adapter"
  protected[diplomacy] override def flexibleArityDirection = true
  protected[diplomacy] def resolveStar(iKnown: Int, oKnown: Int, iStars: Int, oStars: Int): (Int, Int) = {
    require (oStars + iStars <= 1, s"$context appears left of a :*= $iStars times and right of a :=* $oStars times; at most once is allowed")
    if (oStars > 0) {
      require (iKnown >= oKnown, s"$context has $oKnown outputs and $iKnown inputs; cannot assign ${iKnown-oKnown} edges to resolve :=*")
      (0, iKnown - oKnown)
    } else if (iStars > 0) {
      require (oKnown >= iKnown, s"$context has $oKnown outputs and $iKnown inputs; cannot assign ${oKnown-iKnown} edges to resolve :*=")
      (oKnown - iKnown, 0)
    } else {
      require (oKnown == iKnown, s"$context has $oKnown outputs and $iKnown inputs; these do not match")
      (0, 0)
    }
  }
  protected[diplomacy] def mapParamsD(n: Int, p: Seq[DI]): Seq[DO] = {
    require(n == p.size, s"$context has ${p.size} inputs and ${n} outputs; they must match")
    p.map(dFn)
  }
  protected[diplomacy] def mapParamsU(n: Int, p: Seq[UO]): Seq[UI] = {
    require(n == p.size, s"$context has ${n} inputs and ${p.size} outputs; they must match")
    p.map(uFn)
  }
}
/** The [[MixedAdapterNode.inner]] and [[MixedAdapterNode.outer]] of [[AdapterNode]] are same,
  * but it has to provide the [[uFn]] and [[dFn]] to alter the [[Parameters]] transform protocol
  * When the [[U]] and [[D]] parameters are different, an adapter node is used.
  */
class AdapterNode[D, U, EO, EI, B <: Data](imp: NodeImp[D, U, EO, EI, B])(
  dFn: D => D,
  uFn: U => U)(
  implicit valName: ValName)
    extends MixedAdapterNode[D, U, EI, B, D, U, EO, B](imp, imp)(dFn, uFn)

/** The [[AdapterNode.inner]] and [[AdapterNode.outer]] of [[IdentityNode]] are same,
  * and directly pass the between input and output.
  * it automatically connect their inputs to outputs.
  */
class IdentityNode[D, U, EO, EI, B <: Data](imp: NodeImp[D, U, EO, EI, B])()(implicit valName: ValName)
  extends AdapterNode(imp)({ s => s }, { s => s })
{
  override def description = "identity"
  protected override val identity = true
  override protected[diplomacy] def instantiate() = {
    val dangles = super.instantiate()
    (out zip in) foreach { case ((o, _), (i, _)) => o <> i }
    dangles
  } 
}

/** EphemeralNodes disappear from the final node graph. */
class EphemeralNode[D, U, EO, EI, B <: Data](imp: NodeImp[D, U, EO, EI, B])()(implicit valName: ValName)
  extends AdapterNode(imp)({ s => s }, { s => s })
{
  override def description = "ephemeral"
  override def omitGraphML = true
  override def oForward(x: Int) = Some(iDirectPorts(x) match { case (i, n, _, _) => (i, n) })
  override def iForward(x: Int) = Some(oDirectPorts(x) match { case (i, n, _, _) => (i, n) })
  override protected[diplomacy] def instantiate() = Nil
}

/** [[MixedNexusNode]] is used to handle the case which not sure the number of nodes connect to.
  * [[NodeImp]] is different between [[inner]] and [[outer]],
  *
  * @param dFn merge [[Parameters]] from multi master sources connected to this node into a single parameter
  * @param uFn merge [[Parameters]] from multi slave sources connected to this node into a single parameter
  * @param inputRequiresOutput @todo
  * @param outputRequiresInput @todo
  */
class MixedNexusNode[DI, UI, EI, BI <: Data, DO, UO, EO, BO <: Data](
  inner: InwardNodeImp [DI, UI, EI, BI],
  outer: OutwardNodeImp[DO, UO, EO, BO])(
  dFn: Seq[DI] => DO,
  uFn: Seq[UO] => UI,
  // no inputs and no outputs is always allowed
  inputRequiresOutput: Boolean = true,
  outputRequiresInput: Boolean = true)(
  implicit valName: ValName)
  extends MixedNode(inner, outer)
{
  override def description = "nexus"
  protected[diplomacy] def resolveStar(iKnown: Int, oKnown: Int, iStars: Int, oStars: Int): (Int, Int) = {
    // a nexus treats :=* as a weak pointer
    require (!outputRequiresInput || oKnown == 0 || iStars + iKnown != 0, s"$context has $oKnown required outputs and no possible inputs")
    require (!inputRequiresOutput || iKnown == 0 || oStars + oKnown != 0, s"$context has $iKnown required inputs and no possible outputs")
    if (iKnown == 0 && oKnown == 0) (0, 0) else (1, 1)
  }
  protected[diplomacy] def mapParamsD(n: Int, p: Seq[DI]): Seq[DO] = { if (n > 0) { val a = dFn(p); Seq.fill(n)(a) } else Nil }
  protected[diplomacy] def mapParamsU(n: Int, p: Seq[UO]): Seq[UI] = { if (n > 0) { val a = uFn(p); Seq.fill(n)(a) } else Nil }
}

/** [[NexusNode]] is a [[MixedNexusNode]], which `inner` and `outer` has same [[NodeImp]]. */
class NexusNode[D, U, EO, EI, B <: Data](imp: NodeImp[D, U, EO, EI, B])(
  dFn: Seq[D] => D,
  uFn: Seq[U] => U,
  inputRequiresOutput: Boolean = true,
  outputRequiresInput: Boolean = true)(
  implicit valName: ValName)
    extends MixedNexusNode[D, U, EI, B, D, U, EO, B](imp, imp)(dFn, uFn, inputRequiresOutput, outputRequiresInput)

/** [[SourceNode]] will ignore the [[MixedNode.iPorts]].
  * [[SourceNode]] is to model a master node in the graph which only has outwards arcs but no inwards arcs.
  * For example, a processor would be a [[SourceNode]].
  * cannot appear left of a `:=`， `:*=`, `:*=*`
  * There are no Mixed SourceNodes.
  * */
class SourceNode[D, U, EO, EI, B <: Data](imp: NodeImp[D, U, EO, EI, B])(po: Seq[D])(implicit valName: ValName)
  extends MixedNode(imp, imp)
{
  override def description = "source"
  protected[diplomacy] def resolveStar(iKnown: Int, oKnown: Int, iStars: Int, oStars: Int): (Int, Int) = {
    require (oStars <= 1, s"$context appears right of a :=* ${oStars} times; at most once is allowed")
    require (iStars == 0, s"$context cannot appear left of a :*=")
    require (iKnown == 0, s"$context cannot appear left of a :=")
    require (po.size == oKnown || oStars == 1, s"$context has only ${oKnown} outputs connected out of ${po.size}")
    require (po.size >= oKnown, s"$context has ${oKnown} outputs out of ${po.size}; cannot assign ${po.size - oKnown} edges to resolve :=*")
    (0, po.size - oKnown)
  }
  protected[diplomacy] def mapParamsD(n: Int, p: Seq[D]): Seq[D] = po
  protected[diplomacy] def mapParamsU(n: Int, p: Seq[U]): Seq[U] = Seq()

  def makeIOs()(implicit valName: ValName): HeterogeneousBag[B] = {
    val bundles = this.out.map(_._1)
    val ios = IO(Flipped(new HeterogeneousBag(bundles.map(_.cloneType))))
    ios.suggestName(valName.name)
    bundles.zip(ios).foreach { case (bundle, io) => bundle <> io }
    ios
  }
}

/** [[SinkNode]] will ignore the [[MixedNode.oPorts]].
  * [[SinkNode]] is a slave node in the graph which has only inwards arcs but no outwards arcs,
  * For example, a RAM block (sink) in regarding to the LLC.
  * cannot appear right of a `:=`， `:*=`, `:*=*`
  * */
class SinkNode[D, U, EO, EI, B <: Data](imp: NodeImp[D, U, EO, EI, B])(pi: Seq[U])(implicit valName: ValName)
  extends MixedNode(imp, imp)
{
  override def description = "sink"
  protected[diplomacy] def resolveStar(iKnown: Int, oKnown: Int, iStars: Int, oStars: Int): (Int, Int) = {
    require (iStars <= 1, s"$context appears left of a :*= ${iStars} times; at most once is allowed")
    require (oStars == 0, s"$context cannot appear right of a :=*")
    require (oKnown == 0, s"$context cannot appear right of a :=")
    require (pi.size == iKnown || iStars == 1, s"$context has only ${iKnown} inputs connected out of ${pi.size}")
    require (pi.size >= iKnown, s"$context has ${iKnown} inputs out of ${pi.size}; cannot assign ${pi.size - iKnown} edges to resolve :*=")
    (pi.size - iKnown, 0)
  }
  protected[diplomacy] def mapParamsD(n: Int, p: Seq[D]): Seq[D] = Seq()
  protected[diplomacy] def mapParamsU(n: Int, p: Seq[U]): Seq[U] = pi

  def makeIOs()(implicit valName: ValName): HeterogeneousBag[B] = {
    val bundles = this.in.map(_._1)
    val ios = IO(new HeterogeneousBag(bundles.map(_.cloneType)))
    ios.suggestName(valName.name)
    bundles.zip(ios).foreach { case (bundle, io) => io <> bundle }
    ios
  }
}

class MixedTestNode[DI, UI, EI, BI <: Data, DO, UO, EO, BO <: Data] protected[diplomacy](
  node: NodeHandle [DI, UI, EI, BI, DO, UO, EO, BO], clone: CloneLazyModule)(
  implicit valName: ValName)
  extends MixedNode(node.inner, node.outer)
{
  // The devices connected to this test node must recreate these parameters:
  def iParams: Seq[DI] = node.inward .diParams
  def oParams: Seq[UO] = node.outward.uoParams

  override def description = "test"
  protected[diplomacy] def resolveStar(iKnown: Int, oKnown: Int, iStars: Int, oStars: Int): (Int, Int) = {
    require (oStars <= 1, s"$context appears right of a :=* $oStars times; at most once is allowed")
    require (iStars <= 1, s"$context appears left of a :*= $iStars times; at most once is allowed")
    require (node.inward .uiParams.size == iKnown || iStars == 1, s"$context has only $iKnown inputs connected out of ${node.inward.uiParams.size}")
    require (node.outward.doParams.size == oKnown || oStars == 1, s"$context has only $oKnown outputs connected out of ${node.outward.doParams.size}")
    (node.inward.uiParams.size - iKnown, node.outward.doParams.size - oKnown)
  }

  protected[diplomacy] def mapParamsU(n: Int, p: Seq[UO]): Seq[UI] = node.inward .uiParams
  protected[diplomacy] def mapParamsD(n: Int, p: Seq[DI]): Seq[DO] = node.outward.doParams

  override protected[diplomacy] def instantiate() = {
    val dangles = super.instantiate()
    val orig_module = clone.base.module
    val clone_auto = clone.io("auto").asInstanceOf[AutoBundle]

    danglesOut.zipWithIndex.foreach { case (d, i) =>
      val orig = orig_module.dangles.find(_.source == HalfEdge(node.outward.serial, i))
      require (orig.isDefined, s"Cloned node ${node.outward.name} must be connected externally out ${orig_module.name}")
      val io_name = orig_module.auto.elements.find(_._2 eq orig.get.data).get._1
      d.data <> clone_auto.elements(io_name)
    }
    danglesIn.zipWithIndex.foreach { case (d, i) =>
      val orig = orig_module.dangles.find(_.sink == HalfEdge(node.inward.serial, i))
      require (orig.isDefined, s"Cloned node ${node.inward.name} must be connected externally in ${orig_module.name}")
      val io_name = orig_module.auto.elements.find(_._2 eq orig.get.data).get._1
      clone_auto.elements(io_name) <> d.data
    }

    dangles
  }
}
