// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.axi4

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

object AXI4Imp extends SimpleNodeImp[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4EdgeParameters, AXI4Bundle]
{
  def edge(pd: AXI4MasterPortParameters, pu: AXI4SlavePortParameters, p: Parameters, sourceInfo: SourceInfo) = AXI4EdgeParameters(pd, pu, p, sourceInfo)
  def bundle(e: AXI4EdgeParameters) = AXI4Bundle(e.bundle)
  def render(e: AXI4EdgeParameters) = RenderedEdge(colour = "#00ccff" /* bluish */, label  = (e.slave.beatBytes * 8).toString)

  override def mixO(pd: AXI4MasterPortParameters, node: OutwardNode[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4Bundle]): AXI4MasterPortParameters  =
   pd.copy(masters = pd.masters.map  { c => c.copy (nodePath = node +: c.nodePath) })
  override def mixI(pu: AXI4SlavePortParameters, node: InwardNode[AXI4MasterPortParameters, AXI4SlavePortParameters, AXI4Bundle]): AXI4SlavePortParameters =
   pu.copy(slaves  = pu.slaves.map { m => m.copy (nodePath = node +: m.nodePath) })
}

case class AXI4MasterNode(portParams: Seq[AXI4MasterPortParameters])(implicit valName: ValName) extends SourceNode(AXI4Imp)(portParams)
case class AXI4SlaveNode(portParams: Seq[AXI4SlavePortParameters])(implicit valName: ValName) extends SinkNode(AXI4Imp)(portParams)
case class AXI4AdapterNode(
  masterFn:  AXI4MasterPortParameters => AXI4MasterPortParameters,
  slaveFn:   AXI4SlavePortParameters  => AXI4SlavePortParameters,
  numPorts:  Range.Inclusive = 0 to 999)(
  implicit valName: ValName)
  extends AdapterNode(AXI4Imp)(masterFn, slaveFn, numPorts)
case class AXI4IdentityNode()(implicit valName: ValName) extends IdentityNode(AXI4Imp)()
