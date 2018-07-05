// See LICENSE.SiFive for license details.

package freechips.rocketchip.NAMESPACE

import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

object NAMESPACEImp extends SimpleNodeImp[NAMESPACENullParameters, NAMESPACESinkParameters, NAMESPACESinkParameters, NAMESPACEBundle]
{
  def edge(pd: NAMESPACENullParameters, pu: NAMESPACESinkParameters, p: Parameters, sourceInfo: SourceInfo) = NAMESPACESinkParameters(pd, pu, p, sourceInfo)
  def bundle(e: NAMESPACESinkParameters) = NAMESPACEBundle(e.bundle)
  def render(e: NAMESPACESinkParameters) = RenderedEdge(colour = "#00ccff" /* bluish */, label = (e.sink.beatBytes * 8).toString)

  //override def mixO(pd: NAMESPACESourcePortParameters, node: OutwardNode[NAMESPACESourcePortParameters, NAMESPACESinkPortParameters, NAMESPACEBundle]): NAMESPACESourcePortParameters  =
  // pd.copy(sources = pd.sources.map  { c => c.copy (nodePath = node +: c.nodePath) })
  //override def mixI(pu: NAMESPACESinkPortParameters, node: InwardNode[NAMESPACESourcePortParameters, NAMESPACESinkPortParameters, NAMESPACEBundle]): NAMESPACESinkPortParameters =
  // pu.copy(sinks  = pu.sinks.map { m => m.copy (nodePath = node +: m.nodePath) })
}

// Nodes implemented inside modules
case class NAMESPACESourceNode(])(implicit valName: ValName) extends SourceNode(NAMESPACEImp)(Seq(NAMESPACENullParameters))
case class NAMESPACESinkNode(portParams: Seq[NAMESPACESinkParameters])(implicit valName: ValName) extends SinkNode(NAMESPACEImp)(portParams)
case class NAMESPACENexusNode(
  sinkFn:        Seq[NAMESPACESinkParameters]  => NAMESPACESinkParameters)(
  implicit valName: ValName)
  extends NexusNode(NAMESPACEImp)((Seq[NAMESPACENullParameters]) => NAMESPACENullParameters(), sinkFn)

//case class NAMESPACEIdentityNode()(implicit valName: ValName) extends IdentityNode(NAMESPACEImp)()
