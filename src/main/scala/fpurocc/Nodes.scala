// See LICENSE.SiFive for license details.

package freechips.rocketchip.NAMESPACE

import Chisel._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

object NAMESPACEImp extends SimpleNodeImp[NAMESPACESourcePortParameters, NAMESPACESinkPortParameters, NAMESPACEEdgeParameters, NAMESPACEBundle]
{
  def edge(pd: NAMESPACESourcePortParameters, pu: NAMESPACESinkPortParameters, p: Parameters, sourceInfo: SourceInfo) = NAMESPACEEdgeParameters(pd, pu, p, sourceInfo)
  def bundle(e: NAMESPACEEdgeParameters) = NAMESPACEBundle(e.bundle)
  def render(e: NAMESPACEEdgeParameters) = RenderedEdge(colour = "#00ccff" /* bluish */, label = (e.slave.beatBytes * 8).toString)

  override def mixO(pd: NAMESPACESourcePortParameters, node: OutwardNode[NAMESPACESourcePortParameters, NAMESPACESinkPortParameters, NAMESPACEBundle]): NAMESPACESourcePortParameters  =
   pd.copy(masters = pd.masters.map  { c => c.copy (nodePath = node +: c.nodePath) })
  override def mixI(pu: NAMESPACESinkPortParameters, node: InwardNode[NAMESPACESourcePortParameters, NAMESPACESinkPortParameters, NAMESPACEBundle]): NAMESPACESinkPortParameters =
   pu.copy(slaves  = pu.slaves.map { m => m.copy (nodePath = node +: m.nodePath) })
}

// Nodes implemented inside modules
case class NAMESPACESourceNode(portParams: Seq[NAMESPACESourcePortParameters])(implicit valName: ValName) extends SourceNode(NAMESPACEImp)(portParams)
case class NAMESPACESinkNode(portParams: Seq[NAMESPACESinkPortParameters])(implicit valName: ValName) extends SinkNode(NAMESPACEImp)(portParams)
case class NAMESPACENexusNode(
  sourceFn:       Seq[NAMESPACESourcePortParameters] => NAMESPACESourcePortParameters,
  sinkFn:        Seq[NAMESPACESinkPortParameters]  => NAMESPACESinkPortParameters)(
  implicit valName: ValName)
  extends NexusNode(NAMESPACEImp)(masterFn, slaveFn)

case class NAMESPACEIdentityNode()(implicit valName: ValName) extends IdentityNode(NAMESPACEImp)()
