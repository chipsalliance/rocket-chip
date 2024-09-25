// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axis

import chisel3.experimental.SourceInfo

import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.ValName
import org.chipsalliance.diplomacy.nodes.{SimpleNodeImp, SourceNode, SinkNode, NexusNode, AdapterNode, OutwardNode, IdentityNode, RenderedEdge, InwardNode}

object AXISImp extends SimpleNodeImp[AXISManagerPortParameters, AXISSubordinatePortParameters, AXISEdgeParameters, AXISBundle]
{
  def edge(pd: AXISManagerPortParameters, pu: AXISSubordinatePortParameters, p: Parameters, sourceInfo: SourceInfo) = AXISEdgeParameters.v1(pd, pu, p, sourceInfo)
  def bundle(e: AXISEdgeParameters) = AXISBundle(e.bundle)
  def render(e: AXISEdgeParameters) = RenderedEdge(colour = "#00ccff" /* bluish */, (e.beatBytes * 8).toString)

  override def mixO(pd: AXISManagerPortParameters, node: OutwardNode[AXISManagerPortParameters, AXISSubordinatePortParameters, AXISBundle]): AXISManagerPortParameters  =
    pd.v1copy(managers = pd.managers.map  { c => c.v1copy (nodePath = node +: c.nodePath) })
  override def mixI(pu: AXISSubordinatePortParameters, node: InwardNode[AXISManagerPortParameters, AXISSubordinatePortParameters, AXISBundle]): AXISSubordinatePortParameters =
    pu.v1copy(subordinates  = pu.subordinates.map { m => m.v1copy (nodePath = node +: m.nodePath) })
}

case class AXISManagerNode(portParams: Seq[AXISManagerPortParameters])(implicit valName: ValName) extends SourceNode(AXISImp)(portParams)
case class AXISSubordinateNode(portParams: Seq[AXISSubordinatePortParameters])(implicit valName: ValName) extends SinkNode(AXISImp)(portParams)
case class AXISNexusNode(
  managerFn:       Seq[AXISManagerPortParameters] => AXISManagerPortParameters,
  subordinateFn:        Seq[AXISSubordinatePortParameters]  => AXISSubordinatePortParameters)(
  implicit valName: ValName)
  extends NexusNode(AXISImp)(managerFn, subordinateFn)

case class AXISAdapterNode(
  managerFn:  AXISManagerPortParameters => AXISManagerPortParameters = { m => m },
  subordinateFn:   AXISSubordinatePortParameters  => AXISSubordinatePortParameters  = { s => s })(
  implicit valName: ValName)
  extends AdapterNode(AXISImp)(managerFn, subordinateFn)
case class AXISIdentityNode()(implicit valName: ValName) extends IdentityNode(AXISImp)()
