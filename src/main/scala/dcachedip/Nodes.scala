// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket.hellacache

import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util.MaskGen

//package
package object hellacache {
  type HellaCacheOutwardNode = OutwardNodeHandle[HellaCacheNullParameters, HellaCacheNullParameters, HellaCacheNullParameters, HellaCacheDiplomaticBundle]
  type HellaCacheInwardNode = InwardNodeHandle[HellaCacheNullParameters, HellaCacheNullParameters, HellaCacheNullParameters, HellaCacheDiplomaticBundle]
  type HellaCacheNode = SimpleNodeHandle[HellaCacheNullParameters, HellaCacheNullParameters, HellaCacheNullParameters, HellaCacheDiplomaticBundle]
}

//Bundles
// Signal directions are from the master's point-of-view
class HellaCacheDiplomaticBundle(unused: HellaCacheNullParameters)(implicit p: Parameters) extends HellaCacheIO()(p) 

object HellaCacheDiplomaticBundle {
	def apply(e: HellaCacheNullParameters)(implicit p: Parameters) = new HellaCacheDiplomaticBundle(e)
}

//Parameters
case class HellaCacheNullParameters()(implicit val p: Parameters) 
	                                  


//Nodes
object HellaCacheImp extends SimpleNodeImp[HellaCacheNullParameters, HellaCacheNullParameters, HellaCacheNullParameters, HellaCacheDiplomaticBundle] {
	def edge(pd: HellaCacheNullParameters, pu: HellaCacheNullParameters, p: Parameters, sourceInfo: SourceInfo) = HellaCacheNullParameters()(p)
	//def bundle(e: HellaCacheEdgeParameters) = HellaCacheDiplomaticBundle(e.bundle)
	def bundle(e: HellaCacheNullParameters) = HellaCacheDiplomaticBundle(e)(e.p)
	def render(e: HellaCacheNullParameters) = RenderedEdge(colour = "#00ccff" /* bluish */, label = "HellaCache")
}

// Nodes implemented inside modules
case class HellaCacheSourceNode()(implicit valName: ValName, p: Parameters) extends SourceNode(HellaCacheImp)(List(HellaCacheNullParameters()))
case class HellaCacheSinkNode()(implicit valName: ValName, p: Parameters) extends SinkNode(HellaCacheImp)(List(HellaCacheNullParameters()))
case class HellaCacheNexusNode()(implicit valName: ValName, p: Parameters)
	extends NexusNode(HellaCacheImp)(
		{_: Seq[HellaCacheNullParameters] => HellaCacheNullParameters()},
		{_: Seq[HellaCacheNullParameters] => HellaCacheNullParameters()})

//Xbar
class HellaCacheFanin()(implicit p: Parameters) extends LazyModule()(p) {
	val node = HellaCacheNexusNode()

	lazy val module = new LazyModuleImp(this) {
		val numPorts = node.in.size
		val (in, _) = node.in.unzip
		val out = node.out.head._1 
		
		val dcacheArb = new HellaCacheArbiter(numPorts)

		in.zip(dcacheArb.io.requestor).foreach { case (in, req) => in <> req }
		dcacheArb.io.mem <> out
	}
}

