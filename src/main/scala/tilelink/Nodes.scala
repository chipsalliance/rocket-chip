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

trait HasFormatNode[A, B] extends BaseNode {
  def edges: Edges[List[A], List[B]]
}

object FormatNodeDefinition {
  def formatNode[A, B](edges: Edges[List[A], List[B]])
                  (getClients: B => Seq[TLClientParameters])
                  (getManagerPort: A => TLManagerPortParameters): String =
    edges.out.map(currEdge =>
      getClients(currEdge).map(currClient => 
        s"""Output Edges (edges.out.map.client.clients.map):
          |Client Name = ${currClient.name}
          |visibility = ${currClient.visibility}
          |""".stripMargin + 
          (if (currClient.knownToEmit == None) "\nEmits parameters are UNKNOWN\n" else {
            s"""emitsAcquireT = ${currClient.knownToEmit.get.emitsAcquireT}
              |emitsAcquireB = ${currClient.knownToEmit.get.emitsAcquireB}
              |emitsArithmetic = ${currClient.knownToEmit.get.emitsArithmetic}
              |emitsLogical = ${currClient.knownToEmit.get.emitsLogical}
              |emitsGet = ${currClient.knownToEmit.get.emitsGet}
              |emitsPutFull = ${currClient.knownToEmit.get.emitsPutFull}
              |emitsPutPartial = ${currClient.knownToEmit.get.emitsPutPartial}
              |emitsHint = ${currClient.knownToEmit.get.emitsHint}
              |""".stripMargin
          })).mkString).mkString +
    edges.in.map(currEdge =>
          "Input Edge Manager Beatbytes (edges.in.map.manager.beatBytes): = " + getManagerPort(currEdge).beatBytes + "\n" +
          getManagerPort(currEdge).managers.map(currManager =>
            s"""Input Edges (edges.in.map.manager.managers.map):
              |Manager Name = ${currManager.name}
              |Manager Address = ${currManager.address}
              |supportsAcquireT = ${currManager.supportsAcquireT}
              |supportsAcquireB = ${currManager.supportsAcquireB}
              |supportsArithmetic = ${currManager.supportsArithmetic}
              |supportsLogical = ${currManager.supportsLogical}
              |supportsGet = ${currManager.supportsGet}
              |supportsPutFull = ${currManager.supportsPutFull}
              |supportsPutPartial = ${currManager.supportsPutPartial}
              |supportsHint = ${currManager.supportsHint}
              |""".stripMargin
            ).mkString).mkString
}

trait TLFormatNode extends HasFormatNode[TLEdgeIn, TLEdgeOut] {
  override def formatNode = FormatNodeDefinition.formatNode(edges)(_.client.clients)(_.manager)
}

case class TLClientNode(portParams: Seq[TLClientPortParameters])(implicit valName: ValName) extends SourceNode(TLImp)(portParams) with TLFormatNode
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

trait TLAsyncFormatNode extends HasFormatNode[TLAsyncEdgeParameters, TLAsyncEdgeParameters] {
  override def formatNode = FormatNodeDefinition.formatNode(edges)(_.client.base.clients)(_.manager.base)
}

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
    uFn = { p => p.base.copy(minLatency = p.base.minLatency + sync.getOrElse(p.async.sync)) }) { // discard cycles in other clock domain
      override def formatNode = FormatNodeDefinition.formatNode(edges)(_.client.base.clients)(_.manager)
    }

case class TLAsyncSinkNode(async: AsyncQueueParams)(implicit valName: ValName)
  extends MixedAdapterNode(TLAsyncImp, TLImp)(
    dFn = { p => p.base.copy(minLatency = p.base.minLatency + async.sync) },
    uFn = { p => TLAsyncManagerPortParameters(async, p) }) {
      override def formatNode = FormatNodeDefinition.formatNode(edges)(_.client.clients)(_.manager.base)
    }

// Rationally related crossings

trait TLRationalFormatNode extends HasFormatNode[TLRationalEdgeParameters, TLRationalEdgeParameters] {
  override def formatNode = FormatNodeDefinition.formatNode(edges)(_.client.base.clients)(_.manager.base)
}

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
    uFn = { p => p.base.copy(minLatency = 1) }) { // discard cycles from other clock domain
      override def formatNode = FormatNodeDefinition.formatNode(edges)(_.client.base.clients)(_.manager)
    }

case class TLRationalSinkNode(direction: RationalDirection)(implicit valName: ValName)
  extends MixedAdapterNode(TLRationalImp, TLImp)(
    dFn = { p => p.base.copy(minLatency = 1) },
    uFn = { p => TLRationalManagerPortParameters(direction, p) }) {
      override def formatNode = FormatNodeDefinition.formatNode(edges)(_.client.clients)(_.manager.base)
    }
