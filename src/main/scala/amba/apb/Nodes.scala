// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.apb

import chisel3._
import chisel3.experimental.SourceInfo

import org.chipsalliance.cde.config.{Parameters, Field}

import org.chipsalliance.diplomacy.ValName
import org.chipsalliance.diplomacy.nodes.{SimpleNodeImp,RenderedEdge, InwardNode, OutwardNode, SourceNode, SinkNode, NexusNode, IdentityNode}

case object APBMonitorBuilder extends Field[APBMonitorArgs => APBMonitorBase]

object APBImp extends SimpleNodeImp[APBManagerPortParameters, APBSubordinatePortParameters, APBEdgeParameters, APBBundle]
{
  def edge(pd: APBManagerPortParameters, pu: APBSubordinatePortParameters, p: Parameters, sourceInfo: SourceInfo) = APBEdgeParameters(pd, pu, p, sourceInfo)
  def bundle(e: APBEdgeParameters) = APBBundle(e.bundle)
  def render(e: APBEdgeParameters) = RenderedEdge(colour = "#00ccff" /* bluish */, (e.subordinate.beatBytes * 8).toString)

  override def monitor(bundle: APBBundle, edge: APBEdgeParameters): Unit = {
    edge.params.lift(APBMonitorBuilder).foreach { builder =>
      val monitor = Module(builder(APBMonitorArgs(edge)))
      monitor.io.in := bundle
    }
  }

  override def mixO(pd: APBManagerPortParameters, node: OutwardNode[APBManagerPortParameters, APBSubordinatePortParameters, APBBundle]): APBManagerPortParameters  =
   pd.copy(managers = pd.managers.map  { c => c.copy (nodePath = node +: c.nodePath) })
  override def mixI(pu: APBSubordinatePortParameters, node: InwardNode[APBManagerPortParameters, APBSubordinatePortParameters, APBBundle]): APBSubordinatePortParameters =
   pu.copy(subordinates  = pu.subordinates.map { m => m.copy (nodePath = node +: m.nodePath) })
}

case class APBManagerNode(portParams: Seq[APBManagerPortParameters])(implicit valName: ValName) extends SourceNode(APBImp)(portParams)
case class APBSubordinateNode(portParams: Seq[APBSubordinatePortParameters])(implicit valName: ValName) extends SinkNode(APBImp)(portParams)
case class APBNexusNode(
  managerFn:       Seq[APBManagerPortParameters] => APBManagerPortParameters,
  subordinateFn:        Seq[APBSubordinatePortParameters]  => APBSubordinatePortParameters)(
  implicit valName: ValName)
  extends NexusNode(APBImp)(managerFn, subordinateFn)

case class APBIdentityNode()(implicit valName: ValName) extends IdentityNode(APBImp)()
