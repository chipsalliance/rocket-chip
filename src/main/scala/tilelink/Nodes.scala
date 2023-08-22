// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import chisel3._
import chisel3.experimental.SourceInfo
import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util.{AsyncQueueParams,RationalDirection}

case object TLMonitorBuilder extends Field[TLMonitorArgs => TLMonitorBase](args => new TLMonitor(args))

object TLImp extends NodeImp[TLMasterPortParameters, TLSlavePortParameters, TLEdgeOut, TLEdgeIn, TLBundle]
{
  def edgeO(pd: TLMasterPortParameters, pu: TLSlavePortParameters, p: Parameters, sourceInfo: SourceInfo) = new TLEdgeOut(pd, pu, p, sourceInfo)
  def edgeI(pd: TLMasterPortParameters, pu: TLSlavePortParameters, p: Parameters, sourceInfo: SourceInfo) = new TLEdgeIn (pd, pu, p, sourceInfo)

  def bundleO(eo: TLEdgeOut) = TLBundle(eo.bundle)
  def bundleI(ei: TLEdgeIn)  = TLBundle(ei.bundle)

  def render(ei: TLEdgeIn) = RenderedEdge(colour = "#000000" /* black */, label = (ei.manager.beatBytes * 8).toString)

  override def monitor(bundle: TLBundle, edge: TLEdgeIn): Unit = {
    val monitor = Module(edge.params(TLMonitorBuilder)(TLMonitorArgs(edge)))
    monitor.io.in := bundle
  }

  override def mixO(pd: TLMasterPortParameters, node: OutwardNode[TLMasterPortParameters, TLSlavePortParameters, TLBundle]): TLMasterPortParameters  =
    pd.v1copy(clients  = pd.clients.map  { c => c.v1copy (nodePath = node +: c.nodePath) })
  override def mixI(pu: TLSlavePortParameters, node: InwardNode[TLMasterPortParameters, TLSlavePortParameters, TLBundle]): TLSlavePortParameters =
    pu.v1copy(managers = pu.managers.map { m => m.v1copy (nodePath = node +: m.nodePath) })
}


trait TLFormatNode extends FormatNode[TLEdgeIn, TLEdgeOut]

case class TLClientNode(portParams: Seq[TLMasterPortParameters])(implicit valName: ValName) extends SourceNode(TLImp)(portParams) with TLFormatNode
case class TLManagerNode(portParams: Seq[TLSlavePortParameters])(implicit valName: ValName) extends SinkNode(TLImp)(portParams) with TLFormatNode

case class TLAdapterNode(
  clientFn:  TLMasterPortParameters => TLMasterPortParameters = { s => s },
  managerFn: TLSlavePortParameters  => TLSlavePortParameters  = { s => s })(
  implicit valName: ValName)
  extends AdapterNode(TLImp)(clientFn, managerFn) with TLFormatNode

case class TLJunctionNode(
  clientFn:     Seq[TLMasterPortParameters] => Seq[TLMasterPortParameters],
  managerFn:    Seq[TLSlavePortParameters]  => Seq[TLSlavePortParameters])(
  implicit valName: ValName)
  extends JunctionNode(TLImp)(clientFn, managerFn) with TLFormatNode

case class TLIdentityNode()(implicit valName: ValName) extends IdentityNode(TLImp)() with TLFormatNode

object TLNameNode {
  def apply(name: ValName) = TLIdentityNode()(name)
  def apply(name: Option[String]): TLIdentityNode = apply(ValName(name.getOrElse("with_no_name")))
  def apply(name: String): TLIdentityNode = apply(Some(name))
}

case class TLEphemeralNode()(implicit valName: ValName) extends EphemeralNode(TLImp)()

object TLTempNode {
  def apply(): TLEphemeralNode = TLEphemeralNode()(ValName("temp"))
}

case class TLNexusNode(
  clientFn:        Seq[TLMasterPortParameters] => TLMasterPortParameters,
  managerFn:       Seq[TLSlavePortParameters]  => TLSlavePortParameters)(
  implicit valName: ValName)
  extends NexusNode(TLImp)(clientFn, managerFn) with TLFormatNode

abstract class TLCustomNode(implicit valName: ValName)
  extends CustomNode(TLImp) with TLFormatNode

// Asynchronous crossings

trait TLAsyncFormatNode extends FormatNode[TLAsyncEdgeParameters, TLAsyncEdgeParameters]

object TLAsyncImp extends SimpleNodeImp[TLAsyncClientPortParameters, TLAsyncManagerPortParameters, TLAsyncEdgeParameters, TLAsyncBundle]
{
  def edge(pd: TLAsyncClientPortParameters, pu: TLAsyncManagerPortParameters, p: Parameters, sourceInfo: SourceInfo) = TLAsyncEdgeParameters(pd, pu, p, sourceInfo)
  def bundle(e: TLAsyncEdgeParameters) = new TLAsyncBundle(e.bundle)
  def render(e: TLAsyncEdgeParameters) = RenderedEdge(colour = "#ff0000" /* red */, label = e.manager.async.depth.toString)

  override def mixO(pd: TLAsyncClientPortParameters, node: OutwardNode[TLAsyncClientPortParameters, TLAsyncManagerPortParameters, TLAsyncBundle]): TLAsyncClientPortParameters  =
   pd.copy(base = pd.base.v1copy(clients  = pd.base.clients.map  { c => c.v1copy (nodePath = node +: c.nodePath) }))
  override def mixI(pu: TLAsyncManagerPortParameters, node: InwardNode[TLAsyncClientPortParameters, TLAsyncManagerPortParameters, TLAsyncBundle]): TLAsyncManagerPortParameters =
   pu.copy(base = pu.base.v1copy(managers = pu.base.managers.map { m => m.v1copy (nodePath = node +: m.nodePath) }))
}

case class TLAsyncAdapterNode(
  clientFn:  TLAsyncClientPortParameters  => TLAsyncClientPortParameters  = { s => s },
  managerFn: TLAsyncManagerPortParameters => TLAsyncManagerPortParameters = { s => s })(
  implicit valName: ValName)
  extends AdapterNode(TLAsyncImp)(clientFn, managerFn) with TLAsyncFormatNode

case class TLAsyncIdentityNode()(implicit valName: ValName) extends IdentityNode(TLAsyncImp)() with TLAsyncFormatNode

object TLAsyncNameNode {
  def apply(name: ValName) = TLAsyncIdentityNode()(name)
  def apply(name: Option[String]): TLAsyncIdentityNode = apply(ValName(name.getOrElse("with_no_name")))
  def apply(name: String): TLAsyncIdentityNode = apply(Some(name))
}

case class TLAsyncSourceNode(sync: Option[Int])(implicit valName: ValName)
  extends MixedAdapterNode(TLImp, TLAsyncImp)(
    dFn = { p => TLAsyncClientPortParameters(p) },
    uFn = { p => p.base.v1copy(minLatency = p.base.minLatency + sync.getOrElse(p.async.sync)) }) with FormatNode[TLEdgeIn, TLAsyncEdgeParameters] // discard cycles in other clock domain
case class TLAsyncSinkNode(async: AsyncQueueParams)(implicit valName: ValName)
  extends MixedAdapterNode(TLAsyncImp, TLImp)(
    dFn = { p => p.base.v1copy(minLatency = p.base.minLatency + async.sync) },
    uFn = { p => TLAsyncManagerPortParameters(async, p) }) with FormatNode[TLAsyncEdgeParameters, TLEdgeOut]

// Rationally related crossings

trait TLRationalFormatNode extends FormatNode[TLRationalEdgeParameters, TLRationalEdgeParameters]

object TLRationalImp extends SimpleNodeImp[TLRationalClientPortParameters, TLRationalManagerPortParameters, TLRationalEdgeParameters, TLRationalBundle]
{
  def edge(pd: TLRationalClientPortParameters, pu: TLRationalManagerPortParameters, p: Parameters, sourceInfo: SourceInfo) = TLRationalEdgeParameters(pd, pu, p, sourceInfo)
  def bundle(e: TLRationalEdgeParameters) = new TLRationalBundle(e.bundle)
  def render(e: TLRationalEdgeParameters) = RenderedEdge(colour = "#00ff00" /* green */)

  override def mixO(pd: TLRationalClientPortParameters, node: OutwardNode[TLRationalClientPortParameters, TLRationalManagerPortParameters, TLRationalBundle]): TLRationalClientPortParameters  =
   pd.copy(base = pd.base.v1copy(clients  = pd.base.clients.map  { c => c.v1copy (nodePath = node +: c.nodePath) }))
  override def mixI(pu: TLRationalManagerPortParameters, node: InwardNode[TLRationalClientPortParameters, TLRationalManagerPortParameters, TLRationalBundle]): TLRationalManagerPortParameters =
   pu.copy(base = pu.base.v1copy(managers = pu.base.managers.map { m => m.v1copy (nodePath = node +: m.nodePath) }))
}

case class TLRationalAdapterNode(
  clientFn:  TLRationalClientPortParameters  => TLRationalClientPortParameters  = { s => s },
  managerFn: TLRationalManagerPortParameters => TLRationalManagerPortParameters = { s => s })(
  implicit valName: ValName)
  extends AdapterNode(TLRationalImp)(clientFn, managerFn) with TLRationalFormatNode

case class TLRationalIdentityNode()(implicit valName: ValName) extends IdentityNode(TLRationalImp)() with TLRationalFormatNode

object TLRationalNameNode {
  def apply(name: ValName) = TLRationalIdentityNode()(name)
  def apply(name: Option[String]): TLRationalIdentityNode = apply(ValName(name.getOrElse("with_no_name")))
  def apply(name: String): TLRationalIdentityNode = apply(Some(name))
}

case class TLRationalSourceNode()(implicit valName: ValName)
  extends MixedAdapterNode(TLImp, TLRationalImp)(
    dFn = { p => TLRationalClientPortParameters(p) },
    uFn = { p => p.base.v1copy(minLatency = 1) }) with FormatNode[TLEdgeIn, TLRationalEdgeParameters] // discard cycles from other clock domain
case class TLRationalSinkNode(direction: RationalDirection)(implicit valName: ValName)
  extends MixedAdapterNode(TLRationalImp, TLImp)(
    dFn = { p => p.base.v1copy(minLatency = 1) },
    uFn = { p => TLRationalManagerPortParameters(direction, p) }) with FormatNode[TLRationalEdgeParameters, TLEdgeOut]

// Credited version of TileLink channels

trait TLCreditedFormatNode extends FormatNode[TLCreditedEdgeParameters, TLCreditedEdgeParameters]

object TLCreditedImp extends SimpleNodeImp[TLCreditedClientPortParameters, TLCreditedManagerPortParameters, TLCreditedEdgeParameters, TLCreditedBundle]
{
  def edge(pd: TLCreditedClientPortParameters, pu: TLCreditedManagerPortParameters, p: Parameters, sourceInfo: SourceInfo) = TLCreditedEdgeParameters(pd, pu, p, sourceInfo)
  def bundle(e: TLCreditedEdgeParameters) = new TLCreditedBundle(e.bundle)
  def render(e: TLCreditedEdgeParameters) = RenderedEdge(colour = "#ffff00" /* yellow */, e.delay.toString)

  override def mixO(pd: TLCreditedClientPortParameters, node: OutwardNode[TLCreditedClientPortParameters, TLCreditedManagerPortParameters, TLCreditedBundle]): TLCreditedClientPortParameters  =
   pd.copy(base = pd.base.v1copy(clients  = pd.base.clients.map  { c => c.v1copy (nodePath = node +: c.nodePath) }))
  override def mixI(pu: TLCreditedManagerPortParameters, node: InwardNode[TLCreditedClientPortParameters, TLCreditedManagerPortParameters, TLCreditedBundle]): TLCreditedManagerPortParameters =
   pu.copy(base = pu.base.v1copy(managers = pu.base.managers.map { m => m.v1copy (nodePath = node +: m.nodePath) }))
}

case class TLCreditedAdapterNode(
  clientFn:  TLCreditedClientPortParameters  => TLCreditedClientPortParameters  = { s => s },
  managerFn: TLCreditedManagerPortParameters => TLCreditedManagerPortParameters = { s => s })(
  implicit valName: ValName)
  extends AdapterNode(TLCreditedImp)(clientFn, managerFn) with TLCreditedFormatNode

case class TLCreditedIdentityNode()(implicit valName: ValName) extends IdentityNode(TLCreditedImp)() with TLCreditedFormatNode

object TLCreditedNameNode {
  def apply(name: ValName) = TLCreditedIdentityNode()(name)
  def apply(name: Option[String]): TLCreditedIdentityNode = apply(ValName(name.getOrElse("with_no_name")))
  def apply(name: String): TLCreditedIdentityNode = apply(Some(name))
}

case class TLCreditedSourceNode(delay: TLCreditedDelay)(implicit valName: ValName)
  extends MixedAdapterNode(TLImp, TLCreditedImp)(
    dFn = { p => TLCreditedClientPortParameters(delay, p) },
    uFn = { p => p.base.v1copy(minLatency = 1) }) with FormatNode[TLEdgeIn, TLCreditedEdgeParameters] // discard cycles from other clock domain

case class TLCreditedSinkNode(delay: TLCreditedDelay)(implicit valName: ValName)
  extends MixedAdapterNode(TLCreditedImp, TLImp)(
    dFn = { p => p.base.v1copy(minLatency = 1) },
    uFn = { p => TLCreditedManagerPortParameters(delay, p) }) with FormatNode[TLCreditedEdgeParameters, TLEdgeOut]
