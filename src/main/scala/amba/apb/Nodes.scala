// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.apb

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

object APBImp extends SimpleNodeImp[APBMasterPortParameters, APBSlavePortParameters, APBEdgeParameters, APBBundle]
{
  def edge(pd: APBMasterPortParameters, pu: APBSlavePortParameters, p: Parameters, sourceInfo: SourceInfo) = APBEdgeParameters(pd, pu, p, sourceInfo)
  def bundle(e: APBEdgeParameters) = APBBundle(e.bundle)
  def render(e: APBEdgeParameters) = RenderedEdge(colour = "#00ccff" /* bluish */, (e.slave.beatBytes * 8).toString)

  override def mixO(pd: APBMasterPortParameters, node: OutwardNode[APBMasterPortParameters, APBSlavePortParameters, APBBundle]): APBMasterPortParameters  =
   pd.copy(masters = pd.masters.map  { c => c.copy (nodePath = node +: c.nodePath) })
  override def mixI(pu: APBSlavePortParameters, node: InwardNode[APBMasterPortParameters, APBSlavePortParameters, APBBundle]): APBSlavePortParameters =
   pu.copy(slaves  = pu.slaves.map { m => m.copy (nodePath = node +: m.nodePath) })
}

case class APBMasterNode(portParams: Seq[APBMasterPortParameters])(implicit valName: ValName) extends SourceNode(APBImp)(portParams)
case class APBSlaveNode(portParams: Seq[APBSlavePortParameters])(implicit valName: ValName) extends SinkNode(APBImp)(portParams)
case class APBNexusNode(
  masterFn:       Seq[APBMasterPortParameters] => APBMasterPortParameters,
  slaveFn:        Seq[APBSlavePortParameters]  => APBSlavePortParameters)(
  implicit valName: ValName)
  extends NexusNode(APBImp)(masterFn, slaveFn)

case class APBIdentityNode()(implicit valName: ValName) extends IdentityNode(APBImp)()
