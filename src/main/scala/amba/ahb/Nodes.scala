// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.ahb

import chisel3._
import chisel3.experimental.SourceInfo

import org.chipsalliance.cde.config.{Parameters, Field}

import org.chipsalliance.diplomacy.ValName
import org.chipsalliance.diplomacy.nodes.{SimpleNodeImp, RenderedEdge, OutwardNode, InwardNode, SourceNode, SinkNode, IdentityNode, AdapterNode, MixedNexusNode, NexusNode}

case object AHBSubordinateMonitorBuilder extends Field[AHBSubordinateMonitorArgs => AHBSubordinateMonitorBase]

// From Arbiter to Subordinate
object AHBImpSubordinate extends SimpleNodeImp[AHBManagerPortParameters, AHBSubordinatePortParameters, AHBEdgeParameters, AHBSubordinateBundle]
{
  def edge(pd: AHBManagerPortParameters, pu: AHBSubordinatePortParameters, p: Parameters, sourceInfo: SourceInfo) = AHBEdgeParameters(pd, pu, p, sourceInfo)
  def bundle(e: AHBEdgeParameters) = AHBSubordinateBundle(e.bundle)
  def render(e: AHBEdgeParameters) = RenderedEdge(colour = "#00ccff" /* bluish */, label = (e.subordinate.beatBytes * 8).toString)

  override def monitor(bundle: AHBSubordinateBundle, edge: AHBEdgeParameters): Unit = {
    edge.params.lift(AHBSubordinateMonitorBuilder).foreach { builder =>
      val monitor = Module(builder(AHBSubordinateMonitorArgs(edge)))
      monitor.io.in := bundle
    }
  }

  override def mixO(pd: AHBManagerPortParameters, node: OutwardNode[AHBManagerPortParameters, AHBSubordinatePortParameters, AHBSubordinateBundle]): AHBManagerPortParameters  =
   pd.copy(managers = pd.managers.map  { c => c.copy (nodePath = node +: c.nodePath) })
  override def mixI(pu: AHBSubordinatePortParameters, node: InwardNode[AHBManagerPortParameters, AHBSubordinatePortParameters, AHBSubordinateBundle]): AHBSubordinatePortParameters =
   pu.copy(subordinates  = pu.subordinates.map { m => m.copy (nodePath = node +: m.nodePath) })
}

case object AHBManagerMonitorBuilder extends Field[AHBManagerMonitorArgs => AHBManagerMonitorBase]

// From Manager to Arbiter
object AHBImpManager extends SimpleNodeImp[AHBManagerPortParameters, AHBSubordinatePortParameters, AHBEdgeParameters, AHBManagerBundle]
{
  def edge(pd: AHBManagerPortParameters, pu: AHBSubordinatePortParameters, p: Parameters, sourceInfo: SourceInfo) = AHBEdgeParameters(pd, pu, p, sourceInfo)
  def bundle(e: AHBEdgeParameters) = AHBManagerBundle(e.bundle)
  def render(e: AHBEdgeParameters) = RenderedEdge(colour = "#00ccff" /* bluish */, label = (e.subordinate.beatBytes * 8).toString)

  override def monitor(bundle: AHBManagerBundle, edge: AHBEdgeParameters): Unit = {
    edge.params.lift(AHBManagerMonitorBuilder).foreach { builder =>
      val monitor = Module(builder(AHBManagerMonitorArgs(edge)))
      monitor.io.in := bundle
    }
  }

  override def mixO(pd: AHBManagerPortParameters, node: OutwardNode[AHBManagerPortParameters, AHBSubordinatePortParameters, AHBManagerBundle]): AHBManagerPortParameters  =
   pd.copy(managers = pd.managers.map  { c => c.copy (nodePath = node +: c.nodePath) })
  override def mixI(pu: AHBSubordinatePortParameters, node: InwardNode[AHBManagerPortParameters, AHBSubordinatePortParameters, AHBManagerBundle]): AHBSubordinatePortParameters =
   pu.copy(subordinates  = pu.subordinates.map { m => m.copy (nodePath = node +: m.nodePath) })
}

// Nodes implemented inside modules
case class AHBManagerSourceNode(portParams: Seq[AHBManagerPortParameters])(implicit valName: ValName) extends SourceNode(AHBImpManager)(portParams)
case class AHBSubordinateSourceNode(portParams: Seq[AHBManagerPortParameters])(implicit valName: ValName) extends SourceNode(AHBImpSubordinate)(portParams)
case class AHBManagerSinkNode(portParams: Seq[AHBSubordinatePortParameters])(implicit valName: ValName) extends SinkNode(AHBImpManager)(portParams)
case class AHBSubordinateSinkNode(portParams: Seq[AHBSubordinatePortParameters])(implicit valName: ValName) extends SinkNode(AHBImpSubordinate)(portParams)
case class AHBManagerIdentityNode()(implicit valName: ValName) extends IdentityNode(AHBImpManager)()
case class AHBSubordinateIdentityNode()(implicit valName: ValName) extends IdentityNode(AHBImpSubordinate)()

case class AHBManagerAdapterNode(
  managerFn:       AHBManagerPortParameters => AHBManagerPortParameters,
  subordinateFn:        AHBSubordinatePortParameters  => AHBSubordinatePortParameters)(
  implicit valName: ValName)
  extends AdapterNode(AHBImpManager)(managerFn, subordinateFn)

case class AHBSubordinateAdapterNode(
  managerFn:       AHBManagerPortParameters => AHBManagerPortParameters,
  subordinateFn:        AHBSubordinatePortParameters  => AHBSubordinatePortParameters)(
  implicit valName: ValName)
  extends AdapterNode(AHBImpManager)(managerFn, subordinateFn)

// From Manager to Arbiter to Subordinate
case class AHBArbiterNode(
  managerFn:       Seq[AHBManagerPortParameters] => AHBManagerPortParameters,
  subordinateFn:        Seq[AHBSubordinatePortParameters]  => AHBSubordinatePortParameters)(
  implicit valName: ValName)
  extends MixedNexusNode(AHBImpManager, AHBImpSubordinate)(managerFn, subordinateFn)

// Combine multiple Subordinates into one logical Subordinate (suitable to attach to an Arbiter)
case class AHBFanoutNode(
  managerFn:       Seq[AHBManagerPortParameters] => AHBManagerPortParameters,
  subordinateFn:        Seq[AHBSubordinatePortParameters]  => AHBSubordinatePortParameters)(
  implicit valName: ValName)
  extends NexusNode(AHBImpSubordinate)(managerFn, subordinateFn)
