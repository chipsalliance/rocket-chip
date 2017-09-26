// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util.RationalDirection
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
  override def getO(pu: TLManagerPortParameters): Option[BaseNode] = {
    val head = pu.managers.map(_.nodePath.headOption)
    if (head.exists(!_.isDefined) || head.map(_.get).distinct.size != 1) {
      None
    } else {
      val subproblem = pu.copy(managers = pu.managers.map(m => m.copy(nodePath = m.nodePath.tail)))
      getO(subproblem) match {
        case Some(x) => Some(x)
        case None => Some(head(0).get)
      }
    }
  }
}

case class TLClientNode(portParams: Seq[TLClientPortParameters])(implicit valName: ValName) extends SourceNode(TLImp)(portParams)
case class TLManagerNode(portParams: Seq[TLManagerPortParameters])(implicit valName: ValName) extends SinkNode(TLImp)(portParams)

case class TLAdapterNode(
  clientFn:  TLClientPortParameters  => TLClientPortParameters  = { s => s },
  managerFn: TLManagerPortParameters => TLManagerPortParameters = { s => s },
  num:       Range.Inclusive = 0 to 999)(
  implicit valName: ValName)
  extends AdapterNode(TLImp)(clientFn, managerFn, num)

case class TLIdentityNode()(implicit valName: ValName) extends IdentityNode(TLImp)()

case class TLNexusNode(
  clientFn:        Seq[TLClientPortParameters]  => TLClientPortParameters,
  managerFn:       Seq[TLManagerPortParameters] => TLManagerPortParameters,
  numClientPorts:  Range.Inclusive = 1 to 999,
  numManagerPorts: Range.Inclusive = 1 to 999)(
  implicit valName: ValName)
  extends NexusNode(TLImp)(clientFn, managerFn, numClientPorts, numManagerPorts)

abstract class TLCustomNode(
  numClientPorts:  Range.Inclusive,
  numManagerPorts: Range.Inclusive)(
  implicit valName: ValName)
  extends CustomNode(TLImp)(numClientPorts, numManagerPorts)

// Asynchronous crossings

object TLAsyncImp extends SimpleNodeImp[TLAsyncClientPortParameters, TLAsyncManagerPortParameters, TLAsyncEdgeParameters, TLAsyncBundle]
{
  def edge(pd: TLAsyncClientPortParameters, pu: TLAsyncManagerPortParameters, p: Parameters, sourceInfo: SourceInfo) = TLAsyncEdgeParameters(pd, pu, p, sourceInfo)
  def bundle(e: TLAsyncEdgeParameters) = new TLAsyncBundle(e.bundle)
  def render(e: TLAsyncEdgeParameters) = RenderedEdge(colour = "#ff0000" /* red */, label = e.manager.depth.toString)

  override def mixO(pd: TLAsyncClientPortParameters, node: OutwardNode[TLAsyncClientPortParameters, TLAsyncManagerPortParameters, TLAsyncBundle]): TLAsyncClientPortParameters  =
   pd.copy(base = pd.base.copy(clients  = pd.base.clients.map  { c => c.copy (nodePath = node +: c.nodePath) }))
  override def mixI(pu: TLAsyncManagerPortParameters, node: InwardNode[TLAsyncClientPortParameters, TLAsyncManagerPortParameters, TLAsyncBundle]): TLAsyncManagerPortParameters =
   pu.copy(base = pu.base.copy(managers = pu.base.managers.map { m => m.copy (nodePath = node +: m.nodePath) }))
}

case class TLAsyncAdapterNode(
  clientFn:  TLAsyncClientPortParameters  => TLAsyncClientPortParameters  = { s => s },
  managerFn: TLAsyncManagerPortParameters => TLAsyncManagerPortParameters = { s => s },
  num:       Range.Inclusive = 0 to 999)(
  implicit valName: ValName)
  extends AdapterNode(TLAsyncImp)(clientFn, managerFn, num)

case class TLAsyncIdentityNode()(implicit valName: ValName) extends IdentityNode(TLAsyncImp)()

case class TLAsyncSourceNode(sync: Int)(implicit valName: ValName)
  extends MixedAdapterNode(TLImp, TLAsyncImp)(
    dFn = { p => TLAsyncClientPortParameters(p) },
    uFn = { p => p.base.copy(minLatency = sync+1) }) // discard cycles in other clock domain

case class TLAsyncSinkNode(depth: Int, sync: Int)(implicit valName: ValName)
  extends MixedAdapterNode(TLAsyncImp, TLImp)(
    dFn = { p => p.base.copy(minLatency = sync+1) },
    uFn = { p => TLAsyncManagerPortParameters(depth, p) })

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
  managerFn: TLRationalManagerPortParameters => TLRationalManagerPortParameters = { s => s },
  num:       Range.Inclusive = 0 to 999)(
  implicit valName: ValName)
  extends AdapterNode(TLRationalImp)(clientFn, managerFn, num)

case class TLRationalIdentityNode()(implicit valName: ValName) extends IdentityNode(TLRationalImp)()

case class TLRationalSourceNode()(implicit valName: ValName)
  extends MixedAdapterNode(TLImp, TLRationalImp)(
    dFn = { p => TLRationalClientPortParameters(p) },
    uFn = { p => p.base.copy(minLatency = 1) }) // discard cycles from other clock domain

case class TLRationalSinkNode(direction: RationalDirection)(implicit valName: ValName)
  extends MixedAdapterNode(TLRationalImp, TLImp)(
    dFn = { p => p.base.copy(minLatency = 1) },
    uFn = { p => TLRationalManagerPortParameters(direction, p) })
