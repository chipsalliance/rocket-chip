// See LICENSE for license details.

package uncore.tilelink2

import Chisel._
import scala.collection.mutable.ListBuffer
import chisel3.internal.sourceinfo.SourceInfo

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
