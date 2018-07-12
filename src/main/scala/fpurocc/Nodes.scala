// See LICENSE.SiFive for license details.

package freechips.rocketchip.NAMESPACE

import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

object NAMESPACEImp extends SimpleNodeImp[NAMESPACENullParameters, NAMESPACESinkParameters, NAMESPACESinkParameters, NAMESPACEBundle]
{
  def edge(pd: NAMESPACENullParameters, pu: NAMESPACESinkParameters, p: Parameters, sourceInfo: SourceInfo) = pu
  def bundle(e: NAMESPACESinkParameters) = NAMESPACEBundle(e)
  def render(e: NAMESPACESinkParameters) = RenderedEdge(colour = "#00ccff" /* bluish */, label = (e.fLen).toString)

  //override def mixO(pd: NAMESPACESourcePortParameters, node: OutwardNode[NAMESPACESourcePortParameters, NAMESPACESinkPortParameters, NAMESPACEBundle]): NAMESPACESourcePortParameters  =
  // pd.copy(sources = pd.sources.map  { c => c.copy (nodePath = node +: c.nodePath) })
  //override def mixI(pu: NAMESPACESinkPortParameters, node: InwardNode[NAMESPACESourcePortParameters, NAMESPACESinkPortParameters, NAMESPACEBundle]): NAMESPACESinkPortParameters =
  // pu.copy(sinks  = pu.sinks.map { m => m.copy (nodePath = node +: m.nodePath) })
}

// Nodes implemented inside modules
case class NAMESPACESourceNode()(implicit valName: ValName) extends SourceNode(NAMESPACEImp)(Seq(NAMESPACENullParameters()))
case class NAMESPACESinkNode(portParams: NAMESPACESinkParameters)(implicit valName: ValName) extends SinkNode(NAMESPACEImp)(Seq(portParams))
case class NAMESPACENexusNode(
  sinkFn:        Seq[NAMESPACESinkParameters]  => NAMESPACESinkParameters)(
  implicit valName: ValName)
  extends NexusNode(NAMESPACEImp)(
	{ _: Seq[NAMESPACENullParameters] => NAMESPACENullParameters()},
	sinkFn
  )

//case class NAMESPACEIdentityNode()(implicit valName: ValName) extends IdentityNode(NAMESPACEImp)()
