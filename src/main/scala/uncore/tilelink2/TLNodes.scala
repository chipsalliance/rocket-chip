// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import scala.collection.mutable.ListBuffer
import chisel3.internal.sourceinfo.SourceInfo

object TLImp extends NodeImp[TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLBundle]
{
  def edgeO(po: TLClientPortParameters, pi: TLManagerPortParameters): TLEdgeOut = new TLEdgeOut(po, pi)
  def edgeI(po: TLClientPortParameters, pi: TLManagerPortParameters): TLEdgeIn  = new TLEdgeIn(po, pi)
  def bundleO(eo: Seq[TLEdgeOut]): Vec[TLBundle] = {
    require (!eo.isEmpty)
    Vec(eo.size, TLBundle(eo.map(_.bundle).reduce(_.union(_))))
  }
  def bundleI(ei: Seq[TLEdgeIn]): Vec[TLBundle] = {
    require (!ei.isEmpty)
    Vec(ei.size, TLBundle(ei.map(_.bundle).reduce(_.union(_)))).flip
  }

  def connect(bo: => TLBundle, eo: => TLEdgeOut, bi: => TLBundle, ei: => TLEdgeIn)(implicit sourceInfo: SourceInfo): (Option[LazyModule], () => Unit) = {
    val monitor = LazyModule(new TLMonitor(() => new TLBundleSnoop(bo.params), () => eo, sourceInfo))
    (Some(monitor), () => {
      require (eo.asInstanceOf[TLEdgeParameters] == ei.asInstanceOf[TLEdgeParameters])
      bi <> bo
      monitor.module.io.in := TLBundleSnoop(bo)
    })
  }

  override def mixO(po: TLClientPortParameters,  node: TLBaseNode): TLClientPortParameters  =
   po.copy(clients  = po.clients.map  { c => c.copy (nodePath = node +: c.nodePath) })
  override def mixI(pi: TLManagerPortParameters, node: TLBaseNode): TLManagerPortParameters =
   pi.copy(managers = pi.managers.map { m => m.copy (nodePath = node +: m.nodePath) })
}

case class TLIdentityNode() extends IdentityNode(TLImp)
case class TLOutputNode() extends OutputNode(TLImp)
case class TLInputNode() extends InputNode(TLImp)

case class TLClientNode(params: TLClientParameters, numPorts: Range.Inclusive = 1 to 1)
  extends SourceNode(TLImp)(TLClientPortParameters(Seq(params)), numPorts)

case class TLManagerNode(beatBytes: Int, params: TLManagerParameters, numPorts: Range.Inclusive = 1 to 1, minLatency: Int = 0)
  extends SinkNode(TLImp)(TLManagerPortParameters(Seq(params), beatBytes, minLatency), numPorts)

case class TLAdapterNode(
  clientFn:        Seq[TLClientPortParameters]  => TLClientPortParameters,
  managerFn:       Seq[TLManagerPortParameters] => TLManagerPortParameters,
  numClientPorts:  Range.Inclusive = 1 to 1,
  numManagerPorts: Range.Inclusive = 1 to 1)
  extends InteriorNode(TLImp)(clientFn, managerFn, numClientPorts, numManagerPorts)
