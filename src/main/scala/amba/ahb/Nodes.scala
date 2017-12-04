// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.ahb

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

object AHBImp extends SimpleNodeImp[AHBMasterPortParameters, AHBSlavePortParameters, AHBEdgeParameters, AHBBundle]
{
  def edge(pd: AHBMasterPortParameters, pu: AHBSlavePortParameters, p: Parameters, sourceInfo: SourceInfo) = AHBEdgeParameters(pd, pu, p, sourceInfo)
  def bundle(e: AHBEdgeParameters) = AHBBundle(e.bundle)
  def render(e: AHBEdgeParameters) = RenderedEdge(colour = "#00ccff" /* bluish */, label = (e.slave.beatBytes * 8).toString)

  override def mixO(pd: AHBMasterPortParameters, node: OutwardNode[AHBMasterPortParameters, AHBSlavePortParameters, AHBBundle]): AHBMasterPortParameters  =
   pd.copy(masters = pd.masters.map  { c => c.copy (nodePath = node +: c.nodePath) })
  override def mixI(pu: AHBSlavePortParameters, node: InwardNode[AHBMasterPortParameters, AHBSlavePortParameters, AHBBundle]): AHBSlavePortParameters =
   pu.copy(slaves  = pu.slaves.map { m => m.copy (nodePath = node +: m.nodePath) })
}

// Nodes implemented inside modules
case class AHBMasterNode(portParams: Seq[AHBMasterPortParameters])(implicit valName: ValName) extends SourceNode(AHBImp)(portParams)
case class AHBSlaveNode(portParams: Seq[AHBSlavePortParameters])(implicit valName: ValName) extends SinkNode(AHBImp)(portParams)
case class AHBNexusNode(
  masterFn:       Seq[AHBMasterPortParameters] => AHBMasterPortParameters,
  slaveFn:        Seq[AHBSlavePortParameters]  => AHBSlavePortParameters)(
  implicit valName: ValName)
  extends NexusNode(AHBImp)(masterFn, slaveFn)

case class AHBIdentityNode()(implicit valName: ValName) extends IdentityNode(AHBImp)()
