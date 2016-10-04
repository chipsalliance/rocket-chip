// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import diplomacy._
import scala.collection.mutable.ListBuffer

object TLImp extends NodeImp[TLClientPortParameters, TLManagerPortParameters, TLEdgeOut, TLEdgeIn, TLBundle]
{
  def edgeO(pd: TLClientPortParameters, pu: TLManagerPortParameters): TLEdgeOut = new TLEdgeOut(pd, pu)
  def edgeI(pd: TLClientPortParameters, pu: TLManagerPortParameters): TLEdgeIn  = new TLEdgeIn(pd, pu)
  def bundleO(eo: Seq[TLEdgeOut]): Vec[TLBundle] = {
    require (!eo.isEmpty)
    Vec(eo.size, TLBundle(eo.map(_.bundle).reduce(_.union(_))))
  }
  def bundleI(ei: Seq[TLEdgeIn]): Vec[TLBundle] = {
    require (!ei.isEmpty)
    Vec(ei.size, TLBundle(ei.map(_.bundle).reduce(_.union(_)))).flip
  }

  def colour = "#000000" // black
  def connect(bo: => TLBundle, bi: => TLBundle, ei: => TLEdgeIn)(implicit sourceInfo: SourceInfo): (Option[LazyModule], () => Unit) = {
    val monitor = LazyModule(new TLMonitor(() => new TLBundleSnoop(bo.params), () => ei, sourceInfo))
    (Some(monitor), () => {
      bi <> bo
      monitor.module.io.in := TLBundleSnoop(bo)
    })
  }

  override def mixO(pd: TLClientPortParameters, node: OutwardNode[TLClientPortParameters, TLManagerPortParameters, TLBundle]): TLClientPortParameters  =
   pd.copy(clients  = pd.clients.map  { c => c.copy (nodePath = node +: c.nodePath) })
  override def mixI(pu: TLManagerPortParameters, node: InwardNode[TLClientPortParameters, TLManagerPortParameters, TLBundle]): TLManagerPortParameters =
   pu.copy(managers = pu.managers.map { m => m.copy (nodePath = node +: m.nodePath) })
}

case class TLIdentityNode() extends IdentityNode(TLImp)
case class TLOutputNode() extends OutputNode(TLImp)
case class TLInputNode() extends InputNode(TLImp)

case class TLClientNode(portParams: TLClientPortParameters, numPorts: Range.Inclusive = 1 to 1)
  extends SourceNode(TLImp)(portParams, numPorts)
case class TLManagerNode(portParams: TLManagerPortParameters, numPorts: Range.Inclusive = 1 to 1)
  extends SinkNode(TLImp)(portParams, numPorts)

object TLClientNode
{
  def apply(params: TLClientParameters) =
    new TLClientNode(TLClientPortParameters(Seq(params)), 1 to 1)
}

object TLManagerNode
{
  def apply(beatBytes: Int, params: TLManagerParameters) =
    new TLManagerNode(TLManagerPortParameters(Seq(params), beatBytes, 0), 1 to 1)
}

case class TLAdapterNode(
  clientFn:        Seq[TLClientPortParameters]  => TLClientPortParameters,
  managerFn:       Seq[TLManagerPortParameters] => TLManagerPortParameters,
  numClientPorts:  Range.Inclusive = 1 to 1,
  numManagerPorts: Range.Inclusive = 1 to 1)
  extends InteriorNode(TLImp)(clientFn, managerFn, numClientPorts, numManagerPorts)

/** Synthesizeable unit tests */
import unittest._

class TLInputNodeTest extends UnitTest(500000) {
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

  val fuzzer = LazyModule(new TLFuzzer(5000))
  LazyModule(new Acceptor).node := TLFragmenter(4, 64)(fuzzer.node)

  io.finished := Module(fuzzer.module).io.finished
}

object TLAsyncImp extends NodeImp[TLAsyncClientPortParameters, TLAsyncManagerPortParameters, TLAsyncEdgeParameters, TLAsyncEdgeParameters, TLAsyncBundle]
{
  def edgeO(pd: TLAsyncClientPortParameters, pu: TLAsyncManagerPortParameters): TLAsyncEdgeParameters = TLAsyncEdgeParameters(pd, pu)
  def edgeI(pd: TLAsyncClientPortParameters, pu: TLAsyncManagerPortParameters): TLAsyncEdgeParameters = TLAsyncEdgeParameters(pd, pu)
  def bundleO(eo: Seq[TLAsyncEdgeParameters]): Vec[TLAsyncBundle] = {
    require (eo.size == 1)
    Vec(eo.size, new TLAsyncBundle(eo(0).bundle))
  }
  def bundleI(ei: Seq[TLAsyncEdgeParameters]): Vec[TLAsyncBundle] = {
    require (ei.size == 1)
    Vec(ei.size, new TLAsyncBundle(ei(0).bundle)).flip
  }

  def colour = "#ff0000" // red
  def connect(bo: => TLAsyncBundle, bi: => TLAsyncBundle, ei: => TLAsyncEdgeParameters)(implicit sourceInfo: SourceInfo): (Option[LazyModule], () => Unit) = {
    (None, () => { bi <> bo })
  }

  override def mixO(pd: TLAsyncClientPortParameters, node: OutwardNode[TLAsyncClientPortParameters, TLAsyncManagerPortParameters, TLAsyncBundle]): TLAsyncClientPortParameters  =
   pd.copy(base = pd.base.copy(clients  = pd.base.clients.map  { c => c.copy (nodePath = node +: c.nodePath) }))
  override def mixI(pu: TLAsyncManagerPortParameters, node: InwardNode[TLAsyncClientPortParameters, TLAsyncManagerPortParameters, TLAsyncBundle]): TLAsyncManagerPortParameters =
   pu.copy(base = pu.base.copy(managers = pu.base.managers.map { m => m.copy (nodePath = node +: m.nodePath) }))
}

case class TLAsyncIdentityNode() extends IdentityNode(TLAsyncImp)
case class TLAsyncOutputNode() extends OutputNode(TLAsyncImp)
case class TLAsyncInputNode() extends InputNode(TLAsyncImp)

case class TLAsyncSourceNode() extends MixedNode(TLImp, TLAsyncImp)(
  dFn = { case (1, s) => s.map(TLAsyncClientPortParameters(_)) },
  uFn = { case (1, s) => s.map(_.base) },
  numPO = 1 to 1,
  numPI = 1 to 1)

case class TLAsyncSinkNode(depth: Int) extends MixedNode(TLAsyncImp, TLImp)(
  dFn = { case (1, s) => s.map(_.base) },
  uFn = { case (1, s) => s.map(TLAsyncManagerPortParameters(depth, _)) },
  numPO = 1 to 1,
  numPI = 1 to 1)
