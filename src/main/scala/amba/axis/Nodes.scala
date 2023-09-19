// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axis

import chisel3.experimental.SourceInfo
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._

object AXISImp extends SimpleNodeImp[AXISMasterPortParameters, AXISSlavePortParameters, AXISEdgeParameters, AXISBundle]
{
  def edge(pd: AXISMasterPortParameters, pu: AXISSlavePortParameters, p: Parameters, sourceInfo: SourceInfo) = AXISEdgeParameters.v1(pd, pu, p, sourceInfo)
  def bundle(e: AXISEdgeParameters) = AXISBundle(e.bundle)
  def render(e: AXISEdgeParameters) = RenderedEdge(colour = "#00ccff" /* bluish */, (e.beatBytes * 8).toString)

  override def mixO(pd: AXISMasterPortParameters, node: OutwardNode[AXISMasterPortParameters, AXISSlavePortParameters, AXISBundle]): AXISMasterPortParameters  =
    pd.v1copy(masters = pd.masters.map  { c => c.v1copy (nodePath = node +: c.nodePath) })
  override def mixI(pu: AXISSlavePortParameters, node: InwardNode[AXISMasterPortParameters, AXISSlavePortParameters, AXISBundle]): AXISSlavePortParameters =
    pu.v1copy(slaves  = pu.slaves.map { m => m.v1copy (nodePath = node +: m.nodePath) })
}

case class AXISMasterNode(portParams: Seq[AXISMasterPortParameters])(implicit valName: ValName) extends SourceNode(AXISImp)(portParams)
case class AXISSlaveNode(portParams: Seq[AXISSlavePortParameters])(implicit valName: ValName) extends SinkNode(AXISImp)(portParams)
case class AXISNexusNode(
  masterFn:       Seq[AXISMasterPortParameters] => AXISMasterPortParameters,
  slaveFn:        Seq[AXISSlavePortParameters]  => AXISSlavePortParameters)(
  implicit valName: ValName)
  extends NexusNode(AXISImp)(masterFn, slaveFn)

case class AXISAdapterNode(
  masterFn:  AXISMasterPortParameters => AXISMasterPortParameters = { m => m },
  slaveFn:   AXISSlavePortParameters  => AXISSlavePortParameters  = { s => s })(
  implicit valName: ValName)
  extends AdapterNode(AXISImp)(masterFn, slaveFn)
case class AXISIdentityNode()(implicit valName: ValName) extends IdentityNode(AXISImp)()
