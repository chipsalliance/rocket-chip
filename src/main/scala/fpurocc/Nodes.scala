// See LICENSE.SiFive for license details.

package freechips.rocketchip.NAMESPACE

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

object NAMESPACEImp extends SimpleNodeImp[NAMESPACEMasterPortParameters, NAMESPACESlavePortParameters, NAMESPACEEdgeParameters, NAMESPACEBundle]
{
  def edge(pd: NAMESPACEMasterPortParameters, pu: NAMESPACESlavePortParameters, p: Parameters, sourceInfo: SourceInfo) = NAMESPACEEdgeParameters(pd, pu, p, sourceInfo)
  def bundle(e: NAMESPACEEdgeParameters) = NAMESPACEBundle(e.bundle)
  def render(e: NAMESPACEEdgeParameters) = RenderedEdge(colour = "#00ccff" /* bluish */, label = (e.slave.beatBytes * 8).toString)

  override def mixO(pd: NAMESPACEMasterPortParameters, node: OutwardNode[NAMESPACEMasterPortParameters, NAMESPACESlavePortParameters, NAMESPACEBundle]): NAMESPACEMasterPortParameters  =
   pd.copy(masters = pd.masters.map  { c => c.copy (nodePath = node +: c.nodePath) })
  override def mixI(pu: NAMESPACESlavePortParameters, node: InwardNode[NAMESPACEMasterPortParameters, NAMESPACESlavePortParameters, NAMESPACEBundle]): NAMESPACESlavePortParameters =
   pu.copy(slaves  = pu.slaves.map { m => m.copy (nodePath = node +: m.nodePath) })
}

// Nodes implemented inside modules
case class NAMESPACEMasterNode(portParams: Seq[NAMESPACEMasterPortParameters])(implicit valName: ValName) extends SourceNode(NAMESPACEImp)(portParams)
case class NAMESPACESlaveNode(portParams: Seq[NAMESPACESlavePortParameters])(implicit valName: ValName) extends SinkNode(NAMESPACEImp)(portParams)
case class NAMESPACENexusNode(
  masterFn:       Seq[NAMESPACEMasterPortParameters] => NAMESPACEMasterPortParameters,
  slaveFn:        Seq[NAMESPACESlavePortParameters]  => NAMESPACESlavePortParameters)(
  implicit valName: ValName)
  extends NexusNode(NAMESPACEImp)(masterFn, slaveFn)

case class NAMESPACEIdentityNode()(implicit valName: ValName) extends IdentityNode(NAMESPACEImp)()
