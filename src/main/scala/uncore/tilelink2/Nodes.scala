// See LICENSE.SiFive for license details.

package uncore.tilelink2

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import config._
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
    Vec(ei.size, TLBundle(ei.map(_.bundle).reduce(_.union(_))))
  }

  var emitMonitors = true
  var stressTestDecoupled = false
  var combinationalCheck = false

  def colour = "#000000" // black
  override def labelI(ei: TLEdgeIn)  = (ei.manager.beatBytes * 8).toString
  override def labelO(eo: TLEdgeOut) = (eo.manager.beatBytes * 8).toString

  def connect(bo: => TLBundle, bi: => TLBundle, ei: => TLEdgeIn)(implicit p: Parameters, sourceInfo: SourceInfo): (Option[LazyModule], () => Unit) = {
    val monitor = if (emitMonitors) {
      Some(LazyModule(new TLMonitor(() => new TLBundleSnoop(bo.params), () => ei, sourceInfo)))
    } else {
      None
    }
    (monitor, () => {
      bi <> bo
      monitor.foreach { _.module.io.in := TLBundleSnoop(bo) }
      if (combinationalCheck) {
        // It is forbidden for valid to depend on ready in TL2
        // If someone did that, then this will create a detectable combinational loop
        bo.a.ready := bi.a.ready && bo.a.valid
        bi.b.ready := bo.b.ready && bi.b.valid
        bo.c.ready := bi.c.ready && bo.c.valid
        bi.d.ready := bo.d.ready && bi.d.valid
        bo.e.ready := bi.e.ready && bo.e.valid
      }
      if (stressTestDecoupled) {
        // Randomly stall the transfers
        val allow = LFSRNoiseMaker(5)
        bi.a.valid := bo.a.valid && allow(0)
        bo.a.ready := bi.a.ready && allow(0)
        bo.b.valid := bi.b.valid && allow(1)
        bi.b.ready := bo.b.ready && allow(1)
        bi.c.valid := bo.c.valid && allow(2)
        bo.c.ready := bi.c.ready && allow(2)
        bo.d.valid := bi.d.valid && allow(3)
        bi.d.ready := bo.d.ready && allow(3)
        bi.e.valid := bo.e.valid && allow(4)
        bo.e.ready := bi.e.ready && allow(4)
        // Inject garbage whenever not valid
        val bits_a = bo.a.bits.fromBits(LFSRNoiseMaker(bo.a.bits.asUInt.getWidth))
        val bits_b = bi.b.bits.fromBits(LFSRNoiseMaker(bi.b.bits.asUInt.getWidth))
        val bits_c = bo.c.bits.fromBits(LFSRNoiseMaker(bo.c.bits.asUInt.getWidth))
        val bits_d = bi.d.bits.fromBits(LFSRNoiseMaker(bi.d.bits.asUInt.getWidth))
        val bits_e = bo.e.bits.fromBits(LFSRNoiseMaker(bo.e.bits.asUInt.getWidth))
        when (!bi.a.valid) { bi.a.bits := bits_a }
        when (!bo.b.valid) { bo.b.bits := bits_b }
        when (!bi.c.valid) { bi.c.bits := bits_c }
        when (!bo.d.valid) { bo.d.bits := bits_d }
        when (!bi.e.valid) { bi.e.bits := bits_e }
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
    new TLManagerNode(TLManagerPortParameters(Seq(params), beatBytes, minLatency = 0), 1 to 1)
}

case class TLAdapterNode(
  clientFn:        Seq[TLClientPortParameters]  => TLClientPortParameters,
  managerFn:       Seq[TLManagerPortParameters] => TLManagerPortParameters,
  numClientPorts:  Range.Inclusive = 1 to 1,
  numManagerPorts: Range.Inclusive = 1 to 1)
  extends InteriorNode(TLImp)(clientFn, managerFn, numClientPorts, numManagerPorts)

// Nodes passed from an inner module
case class TLOutputNode() extends OutputNode(TLImp)
case class TLInputNode() extends InputNode(TLImp)

// Nodes used for external ports
case class TLBlindOutputNode(portParams: TLManagerPortParameters) extends BlindOutputNode(TLImp)(portParams)
case class TLBlindInputNode(portParams: TLClientPortParameters) extends BlindInputNode(TLImp)(portParams)

case class TLInternalOutputNode(portParams: TLManagerPortParameters) extends InternalOutputNode(TLImp)(portParams)
case class TLInternalInputNode(portParams: TLClientPortParameters) extends InternalInputNode(TLImp)(portParams)

/** Synthesizeable unit tests */
import unittest._

class TLInputNodeTest()(implicit p: Parameters) extends UnitTest(500000) {
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
    Vec(ei.size, new TLAsyncBundle(ei(0).bundle))
  }

  def colour = "#ff0000" // red
  override def labelI(ei: TLAsyncEdgeParameters) = ei.manager.depth.toString
  override def labelO(eo: TLAsyncEdgeParameters) = eo.manager.depth.toString

  def connect(bo: => TLAsyncBundle, bi: => TLAsyncBundle, ei: => TLAsyncEdgeParameters)(implicit p: Parameters, sourceInfo: SourceInfo): (Option[LazyModule], () => Unit) = {
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
