// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util.{AsyncQueueParams,RationalDirection}
import scala.collection.mutable.ListBuffer

case object TLMonitorBuilder extends Field[TLMonitorArgs => TLMonitorBase](args => new TLMonitor(args))

object TLImp extends NodeImp[TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLBundle]
{
  def edgeO(pd: TLClientPortParameters, pu: TLManagerPortParameters, p: Parameters, sourceInfo: SourceInfo) = new TLEdgeOut(pd, pu, p, sourceInfo)
  def edgeI(pd: TLClientPortParameters, pu: TLManagerPortParameters, p: Parameters, sourceInfo: SourceInfo) = new TLEdgeIn (pd, pu, p, sourceInfo)

  def bundleO(eo: TLEdgeOut) = TLBundle(eo.bundle)
  def bundleI(ei: TLEdgeIn)  = TLBundle(ei.bundle)

  def render(ei: TLEdgeIn) = RenderedEdge(colour = "#000000" /* black */, label = (ei.manager.beatBytes * 8).toString)

  override def monitor(bundle: TLBundle, edge: TLEdgeIn) {
    val monitor = Module(edge.params(TLMonitorBuilder)(TLMonitorArgs(edge)))
    monitor.io.in := TLBundleSnoop(bundle, bundle)
  }

  override def mixO(pd: TLClientPortParameters, node: OutwardNode[TLClientPortParameters, TLManagerPortParameters, TLBundle]): TLClientPortParameters  =
    pd.copy(clients  = pd.clients.map  { c => c.copy (nodePath = node +: c.nodePath) })
  override def mixI(pu: TLManagerPortParameters, node: InwardNode[TLClientPortParameters, TLManagerPortParameters, TLBundle]): TLManagerPortParameters =
    pu.copy(managers = pu.managers.map { m => m.copy (nodePath = node +: m.nodePath) })
}

trait TLFormatNode extends BaseNode
{
  def edges: Edges[List[TLEdgeIn], List[TLEdgeOut]]
  override def formatNode() = edges.in.map(currEdge =>
  currEdge.manager.managers.map(currManager => 
  "Input Edges (edges.in.map.manager.managers.map):\nManager Address" + currManager.address +
  "\nsupportsAcquireT = " + currManager.supportsAcquireT +
  "\nsupportsAcquireB = " + currManager.supportsAcquireB +
  "\nsupportsArithmetic = " + currManager.supportsArithmetic +
  "\nsupportsLogical = " + currManager.supportsLogical +
  "\nsupportsGet = " + currManager.supportsGet +
  "\nsupportsPutFull = " + currManager.supportsPutFull +
  "\nsupportsPutPartial = " + currManager.supportsPutPartial +
  "\nsupportsHint = " + currManager.supportsHint + "\n").mkString).mkString + edges.out.map(currEdge =>
  currEdge.client.clients.map(currClient => "Output Edges (edges.out.map.client.clients.map):\nClient Name:" + currClient.name +
  (if (currClient.knownToEmit == None) "\nEmits parameters are UNKNOWN\n" else
  "\nemitsAcquireT = " + currClient.knownToEmit.get.emitsAcquireT +
  "\nemitsAcquireB = " + currClient.knownToEmit.get.emitsAcquireB +
  "\nemitsArithmetic = " + currClient.knownToEmit.get.emitsArithmetic +
  "\nemitsLogical = " + currClient.knownToEmit.get.emitsLogical +
  "\nemitsGet = " + currClient.knownToEmit.get.emitsGet +
  "\nemitsPutFull = " + currClient.knownToEmit.get.emitsPutFull +
  "\nemitsPutPartial = " + currClient.knownToEmit.get.emitsPutPartial +
  "\nemitsHint = " + currClient.knownToEmit.get.emitsHint + "\n")).mkString).mkString
}

case class TLClientNode(portParams: Seq[TLClientPortParameters])(implicit valName: ValName) extends SourceNode(TLImp)(portParams) with TLFormatNode
//{
//  override def formatNode() = "Output Edge (edges.out.head.client): \nanyEmitAcquireT = " + edges.out.head.client.anyEmitAcquireT +
//  "\nanyEmitAcquireB = " + edges.out.head.client.anyEmitAcquireB +
//  "\nanyEmitArithmetic = " + edges.out.head.client.anyEmitArithmetic +
//  "\nanyEmitLogical = " + edges.out.head.client.anyEmitLogical
//}
case class TLManagerNode(portParams: Seq[TLManagerPortParameters])(implicit valName: ValName) extends SinkNode(TLImp)(portParams) with TLFormatNode
case class TLAdapterNode(
  clientFn:  TLClientPortParameters  => TLClientPortParameters  = { s => s },
  managerFn: TLManagerPortParameters => TLManagerPortParameters = { s => s })(
  implicit valName: ValName)
  extends AdapterNode(TLImp)(clientFn, managerFn) with TLFormatNode
case class TLIdentityNode()(implicit valName: ValName) extends IdentityNode(TLImp)() with TLFormatNode

case class TLEphemeralNode()(implicit valName: ValName) extends EphemeralNode(TLImp)() with TLFormatNode

object TLNameNode {
  def apply(name: ValName) = TLIdentityNode()(name)
  def apply(name: Option[String]): TLIdentityNode = apply(ValName(name.getOrElse("with_no_name")))
  def apply(name: String): TLIdentityNode = apply(Some(name))
}

case class TLNexusNode(
  clientFn:        Seq[TLClientPortParameters]  => TLClientPortParameters,
  managerFn:       Seq[TLManagerPortParameters] => TLManagerPortParameters)(
  implicit valName: ValName)
  extends NexusNode(TLImp)(clientFn, managerFn) with TLFormatNode

abstract class TLCustomNode(implicit valName: ValName)
  extends CustomNode(TLImp) with TLFormatNode

// Asynchronous crossings

object TLAsyncImp extends SimpleNodeImp[TLAsyncClientPortParameters, TLAsyncManagerPortParameters, TLAsyncEdgeParameters, TLAsyncBundle]
{
  def edge(pd: TLAsyncClientPortParameters, pu: TLAsyncManagerPortParameters, p: Parameters, sourceInfo: SourceInfo) = TLAsyncEdgeParameters(pd, pu, p, sourceInfo)
  def bundle(e: TLAsyncEdgeParameters) = new TLAsyncBundle(e.bundle)
  def render(e: TLAsyncEdgeParameters) = RenderedEdge(colour = "#ff0000" /* red */, label = e.manager.async.depth.toString)

  override def mixO(pd: TLAsyncClientPortParameters, node: OutwardNode[TLAsyncClientPortParameters, TLAsyncManagerPortParameters, TLAsyncBundle]): TLAsyncClientPortParameters  =
   pd.copy(base = pd.base.copy(clients  = pd.base.clients.map  { c => c.copy (nodePath = node +: c.nodePath) }))
  override def mixI(pu: TLAsyncManagerPortParameters, node: InwardNode[TLAsyncClientPortParameters, TLAsyncManagerPortParameters, TLAsyncBundle]): TLAsyncManagerPortParameters =
   pu.copy(base = pu.base.copy(managers = pu.base.managers.map { m => m.copy (nodePath = node +: m.nodePath) }))
}

case class TLAsyncAdapterNode(
  clientFn:  TLAsyncClientPortParameters  => TLAsyncClientPortParameters  = { s => s },
  managerFn: TLAsyncManagerPortParameters => TLAsyncManagerPortParameters = { s => s })(
  implicit valName: ValName)
  extends AdapterNode(TLAsyncImp)(clientFn, managerFn)

case class TLAsyncIdentityNode()(implicit valName: ValName) extends IdentityNode(TLAsyncImp)()

object TLAsyncNameNode {
  def apply(name: ValName) = TLAsyncIdentityNode()(name)
  def apply(name: Option[String]): TLAsyncIdentityNode = apply(ValName(name.getOrElse("with_no_name")))
  def apply(name: String): TLAsyncIdentityNode = apply(Some(name))
}

case class TLAsyncSourceNode(sync: Option[Int])(implicit valName: ValName)
  extends MixedAdapterNode(TLImp, TLAsyncImp)(
    dFn = { p => TLAsyncClientPortParameters(p) },
    uFn = { p => p.base.copy(minLatency = p.base.minLatency + sync.getOrElse(p.async.sync)) }) // discard cycles in other clock domain

case class TLAsyncSinkNode(async: AsyncQueueParams)(implicit valName: ValName)
  extends MixedAdapterNode(TLAsyncImp, TLImp)(
    dFn = { p => p.base.copy(minLatency = p.base.minLatency + async.sync) },
    uFn = { p => TLAsyncManagerPortParameters(async, p) })

// Rationally related crossings

object TLRationalImp extends SimpleNodeImp[TLRationalClientPortParameters, TLRationalManagerPortParameters, TLRationalEdgeParameters, TLRationalBundle]
{
  def edge(pd: TLRationalClientPortParameters, pu: TLRationalManagerPortParameters, p: Parameters, sourceInfo: SourceInfo) = TLRationalEdgeParameters(pd, pu, p, sourceInfo)
  def bundle(e: TLRationalEdgeParameters) = new TLRationalBundle(e.bundle)
  def render(e: TLRationalEdgeParameters) = RenderedEdge(colour = "#00ff00" /* green */)

  override def mixO(pd: TLRationalClientPortParameters, node: OutwardNode[TLRationalClientPortParameters, TLRationalManagerPortParameters, TLRationalBundle]): TLRationalClientPortParameters  =
   pd.copy(base = pd.base.copy(clients  = pd.base.clients.map  { c => c.copy (nodePath = node +: c.nodePath) }))
  override def mixI(pu: TLRationalManagerPortParameters, node: InwardNode[TLRationalClientPortParameters, TLRationalManagerPortParameters, TLRationalBundle]): TLRationalManagerPortParameters =
   pu.copy(base = pu.base.copy(managers = pu.base.managers.map { m => m.copy (nodePath = node +: m.nodePath) }))
}

case class TLRationalAdapterNode(
  clientFn:  TLRationalClientPortParameters  => TLRationalClientPortParameters  = { s => s },
  managerFn: TLRationalManagerPortParameters => TLRationalManagerPortParameters = { s => s })(
  implicit valName: ValName)
  extends AdapterNode(TLRationalImp)(clientFn, managerFn)

case class TLRationalIdentityNode()(implicit valName: ValName) extends IdentityNode(TLRationalImp)()

object TLRationalNameNode {
  def apply(name: ValName) = TLRationalIdentityNode()(name)
  def apply(name: Option[String]): TLRationalIdentityNode = apply(ValName(name.getOrElse("with_no_name")))
  def apply(name: String): TLRationalIdentityNode = apply(Some(name))
}

case class TLRationalSourceNode()(implicit valName: ValName)
  extends MixedAdapterNode(TLImp, TLRationalImp)(
    dFn = { p => TLRationalClientPortParameters(p) },
    uFn = { p => p.base.copy(minLatency = 1) }) // discard cycles from other clock domain

case class TLRationalSinkNode(direction: RationalDirection)(implicit valName: ValName)
  extends MixedAdapterNode(TLRationalImp, TLImp)(
    dFn = { p => p.base.copy(minLatency = 1) },
    uFn = { p => TLRationalManagerPortParameters(direction, p) })
