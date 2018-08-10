// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket.hellacache

import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._

object HellaCacheImp extends SimpleNodeImp[HellaCacheNullParameters, HellaCacheNullParameters, HellaCacheEdgeParameters, HellaCacheDiplomaticBundle]
{
	def edge(pd: HellaCacheNullParameters, pu: HellaCacheNullParameters, p: Parameters, sourceInfo: SourceInfo) = HellaCacheEdgeParameters(pd, pu, p, sourceInfo)
	//def bundle(e: HellaCacheEdgeParameters) = HellaCacheDiplomaticBundle(e.bundle)
	def bundle(implicit p: Parameters) = HellaCacheDiplomaticBundle
	def render(e: HellaCacheEdgeParameters) = RenderedEdge(colour = "#00ccff" /* bluish */, label = (e.sink.beatBytes * 8).toString)
	
}

// Nodes implemented inside modules
case class HellaCacheSourceNode(portParams: Seq[HellaCacheNullParameters])(implicit valName: ValName) extends SourceNode(HellaCacheImp)(portParams)
case class HellaCacheSinkNode(portParams: Seq[HellaCacheNullParameters])(implicit valName: ValName) extends SinkNode(HellaCacheImp)(portParams)
case class HellaCacheNexusNode()(
	//sourceFn:       Seq[HellaCacheNullParameters] => HellaCacheNullParameters,
	//sinkFn:        Seq[HellaCacheNullParameters]  => HellaCacheNullParameters)(
	implicit valName: ValName)
	extends NexusNode(HellaCacheImp)(
		{_: Seq[HellaCacheNullParameters] => HellaCacheNullParameters()},
		{_: Seq[HellaCacheNullParameters] => HellaCacheNullParameters()})
case class HellaCacheAdapterNode()(
	//sourceFn: HellaCacheSourceParameters => HellaCacheSourceParameters = { s => s },
	//sinkFn:   HellaCacheSinkParameters   => HellaCacheSinkParameters   = { s => s })(
	valName: ValName)
	extends AdapterNode(HellaCacheImp)(
		{_: Seq[HellaCacheNullParameters] => HellaCacheNullParameters()},
		{_: Seq[HellaCacheNullParameters] => HellaCacheNullParameters()})
case class HellaCacheIdentityNode()(implicit valName: ValName) extends IdentityNode(HellaCacheImp)()



