// See LICENSE.SiFive for license details.

package uncore.tilelink2

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import config._
import diplomacy._
import scala.collection.mutable.ListBuffer
import util.RationalDirection

case object TLMonitorBuilder extends Field[TLMonitorArgs => Option[TLMonitorBase]]
case object TLCombinationalCheck extends Field[Boolean]

object TLImp extends NodeImp[TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLBundle]
{
  def edgeO(pd: TLClientPortParameters, pu: TLManagerPortParameters): TLEdgeOut = new TLEdgeOut(pd, pu)
  def edgeI(pd: TLClientPortParameters, pu: TLManagerPortParameters): TLEdgeIn  = new TLEdgeIn(pd, pu)

  def bundleO(eo: TLEdgeOut): TLBundle = TLBundle(eo.bundle)
  def bundleI(ei: TLEdgeIn):  TLBundle = TLBundle(ei.bundle)

  def colour = "#000000" // black
  override def labelI(ei: TLEdgeIn)  = (ei.manager.beatBytes * 8).toString
  override def labelO(eo: TLEdgeOut) = (eo.manager.beatBytes * 8).toString

  override def connect(edges: () => Seq[TLEdgeIn], bundles: () => Seq[(TLBundle, TLBundle)])(implicit p: Parameters, sourceInfo: SourceInfo): (Option[TLMonitorBase], () => Unit) = {
    val monitor = p(TLMonitorBuilder)(TLMonitorArgs(edges, sourceInfo, p))
    (monitor, () => {
      val eval = bundles ()
      monitor.foreach { m => (eval zip m.module.io.in) foreach { case ((i,o), m) => m := TLBundleSnoop(o,i) } }
      eval.foreach { case (bi, bo) =>
        bi <> bo
        if (p(TLCombinationalCheck)) {
          // It is forbidden for valid to depend on ready in TL2
          // If someone did that, then this will create a detectable combinational loop
          bo.a.ready := bi.a.ready && bo.a.valid
          bi.b.ready := bo.b.ready && bi.b.valid
          bo.c.ready := bi.c.ready && bo.c.valid
          bi.d.ready := bo.d.ready && bi.d.valid
          bo.e.ready := bi.e.ready && bo.e.valid
        }
      }
    })
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

// Nodes implemented inside modules
case class TLIdentityNode() extends IdentityNode(TLImp)
case class TLClientNode(portParams: Seq[TLClientPortParameters]) extends SourceNode(TLImp)(portParams)
case class TLManagerNode(portParams: Seq[TLManagerPortParameters]) extends SinkNode(TLImp)(portParams)

object TLClientNode
{
  def apply(params: TLClientParameters) =
    new TLClientNode(Seq(TLClientPortParameters(Seq(params))))
}

object TLManagerNode
{
  def apply(beatBytes: Int, params: TLManagerParameters) =
    new TLManagerNode(Seq(TLManagerPortParameters(Seq(params), beatBytes, minLatency = 0)))
}

case class TLAdapterNode(
  clientFn:  TLClientPortParameters  => TLClientPortParameters,
  managerFn: TLManagerPortParameters => TLManagerPortParameters,
  num:       Range.Inclusive = 0 to 999)
  extends AdapterNode(TLImp)(clientFn, managerFn, num)

case class TLNexusNode(
  clientFn:        Seq[TLClientPortParameters]  => TLClientPortParameters,
  managerFn:       Seq[TLManagerPortParameters] => TLManagerPortParameters,
  numClientPorts:  Range.Inclusive = 1 to 999,
  numManagerPorts: Range.Inclusive = 1 to 999)
  extends NexusNode(TLImp)(clientFn, managerFn, numClientPorts, numManagerPorts)

// Nodes passed from an inner module
case class TLOutputNode() extends OutputNode(TLImp)
case class TLInputNode() extends InputNode(TLImp)

// Nodes used for external ports
case class TLBlindOutputNode(portParams: Seq[TLManagerPortParameters]) extends BlindOutputNode(TLImp)(portParams)
case class TLBlindInputNode(portParams: Seq[TLClientPortParameters]) extends BlindInputNode(TLImp)(portParams)

case class TLInternalOutputNode(portParams: Seq[TLManagerPortParameters]) extends InternalOutputNode(TLImp)(portParams)
case class TLInternalInputNode(portParams: Seq[TLClientPortParameters]) extends InternalInputNode(TLImp)(portParams)

/** Synthesizeable unit tests */
import unittest._

class TLInputNodeTest(txns: Int = 5000, timeout: Int = 500000)(implicit p: Parameters) extends UnitTest(timeout) {
  class Acceptor extends LazyModule {
    val node = TLInputNode()
    val tlram = LazyModule(new TLRAM(AddressSet(0x54321000, 0xfff)))
    tlram.node := node

    lazy val module = new LazyModuleImp(this) {
      val io = new Bundle {
        val in = node.bundleIn
      }
    }
  }

  val fuzzer = LazyModule(new TLFuzzer(txns))
  LazyModule(new Acceptor).node := TLFragmenter(4, 64)(fuzzer.node)

  io.finished := Module(fuzzer.module).io.finished
}

object TLAsyncImp extends NodeImp[TLAsyncClientPortParameters, TLAsyncManagerPortParameters, TLAsyncEdgeParameters, TLAsyncEdgeParameters, TLAsyncBundle]
{
  def edgeO(pd: TLAsyncClientPortParameters, pu: TLAsyncManagerPortParameters): TLAsyncEdgeParameters = TLAsyncEdgeParameters(pd, pu)
  def edgeI(pd: TLAsyncClientPortParameters, pu: TLAsyncManagerPortParameters): TLAsyncEdgeParameters = TLAsyncEdgeParameters(pd, pu)

  def bundleO(eo: TLAsyncEdgeParameters): TLAsyncBundle = new TLAsyncBundle(eo.bundle)
  def bundleI(ei: TLAsyncEdgeParameters): TLAsyncBundle = new TLAsyncBundle(ei.bundle)

  def colour = "#ff0000" // red
  override def labelI(ei: TLAsyncEdgeParameters) = ei.manager.depth.toString
  override def labelO(eo: TLAsyncEdgeParameters) = eo.manager.depth.toString

  override def mixO(pd: TLAsyncClientPortParameters, node: OutwardNode[TLAsyncClientPortParameters, TLAsyncManagerPortParameters, TLAsyncBundle]): TLAsyncClientPortParameters  =
   pd.copy(base = pd.base.copy(clients  = pd.base.clients.map  { c => c.copy (nodePath = node +: c.nodePath) }))
  override def mixI(pu: TLAsyncManagerPortParameters, node: InwardNode[TLAsyncClientPortParameters, TLAsyncManagerPortParameters, TLAsyncBundle]): TLAsyncManagerPortParameters =
   pu.copy(base = pu.base.copy(managers = pu.base.managers.map { m => m.copy (nodePath = node +: m.nodePath) }))
}

case class TLAsyncIdentityNode() extends IdentityNode(TLAsyncImp)
case class TLAsyncOutputNode() extends OutputNode(TLAsyncImp)
case class TLAsyncInputNode() extends InputNode(TLAsyncImp)

case class TLAsyncSourceNode(sync: Int)
  extends MixedAdapterNode(TLImp, TLAsyncImp)(
    dFn = { p => TLAsyncClientPortParameters(p) },
    uFn = { p => p.base.copy(minLatency = sync+1) }) // discard cycles in other clock domain

case class TLAsyncSinkNode(depth: Int, sync: Int)
  extends MixedAdapterNode(TLAsyncImp, TLImp)(
    dFn = { p => p.base.copy(minLatency = sync+1) },
    uFn = { p => TLAsyncManagerPortParameters(depth, p) })

object TLRationalImp extends NodeImp[TLRationalClientPortParameters, TLRationalManagerPortParameters, TLRationalEdgeParameters, TLRationalEdgeParameters, TLRationalBundle]
{
  def edgeO(pd: TLRationalClientPortParameters, pu: TLRationalManagerPortParameters): TLRationalEdgeParameters = TLRationalEdgeParameters(pd, pu)
  def edgeI(pd: TLRationalClientPortParameters, pu: TLRationalManagerPortParameters): TLRationalEdgeParameters = TLRationalEdgeParameters(pd, pu)

  def bundleO(eo: TLRationalEdgeParameters): TLRationalBundle = new TLRationalBundle(eo.bundle)
  def bundleI(ei: TLRationalEdgeParameters): TLRationalBundle = new TLRationalBundle(ei.bundle)

  def colour = "#00ff00" // green

  override def mixO(pd: TLRationalClientPortParameters, node: OutwardNode[TLRationalClientPortParameters, TLRationalManagerPortParameters, TLRationalBundle]): TLRationalClientPortParameters  =
   pd.copy(base = pd.base.copy(clients  = pd.base.clients.map  { c => c.copy (nodePath = node +: c.nodePath) }))
  override def mixI(pu: TLRationalManagerPortParameters, node: InwardNode[TLRationalClientPortParameters, TLRationalManagerPortParameters, TLRationalBundle]): TLRationalManagerPortParameters =
   pu.copy(base = pu.base.copy(managers = pu.base.managers.map { m => m.copy (nodePath = node +: m.nodePath) }))
}

case class TLRationalIdentityNode() extends IdentityNode(TLRationalImp)
case class TLRationalOutputNode() extends OutputNode(TLRationalImp)
case class TLRationalInputNode() extends InputNode(TLRationalImp)

case class TLRationalSourceNode()
  extends MixedAdapterNode(TLImp, TLRationalImp)(
    dFn = { p => TLRationalClientPortParameters(p) },
    uFn = { p => p.base.copy(minLatency = 1) }) // discard cycles from other clock domain

case class TLRationalSinkNode(direction: RationalDirection)
  extends MixedAdapterNode(TLRationalImp, TLImp)(
    dFn = { p => p.base.copy(minLatency = 1) },
    uFn = { p => TLRationalManagerPortParameters(direction, p) })
