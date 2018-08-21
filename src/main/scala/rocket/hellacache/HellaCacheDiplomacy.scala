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
  type HellaCacheOutwardNode = OutwardNodeHandle[HellaCacheDummyParameters, HellaCacheDummyParameters, HellaCacheDummyParameters, HellaCacheDiplomaticBundle]
  type HellaCacheInwardNode = InwardNodeHandle[HellaCacheDummyParameters, HellaCacheDummyParameters, HellaCacheDummyParameters, HellaCacheDiplomaticBundle]
  type HellaCacheNode = SimpleNodeHandle[HellaCacheDummyParameters, HellaCacheDummyParameters, HellaCacheDummyParameters, HellaCacheDiplomaticBundle]
}

//Bundles
// Signal directions are from the master's point-of-view
class HellaCacheDiplomaticBundle(val unused: HellaCacheDummyParameters)(implicit p: Parameters) extends HellaCacheIO()(p) 

object HellaCacheDiplomaticBundle {
	def apply(e: HellaCacheDummyParameters)(implicit p: Parameters) = new HellaCacheDiplomaticBundle(e)
}

//Parameters
case class HellaCacheDummyParameters()(implicit val p: Parameters) 
	                                  


//Nodes
object HellaCacheImp extends SimpleNodeImp[HellaCacheDummyParameters, HellaCacheDummyParameters, HellaCacheDummyParameters, HellaCacheDiplomaticBundle] {
	def edge(pd: HellaCacheDummyParameters, pu: HellaCacheDummyParameters, p: Parameters, sourceInfo: SourceInfo) = HellaCacheDummyParameters()(p)
	//def bundle(e: HellaCacheEdgeParameters) = HellaCacheDiplomaticBundle(e.bundle)
	def bundle(e: HellaCacheDummyParameters) = HellaCacheDiplomaticBundle(e)(e.p)
	def render(e: HellaCacheDummyParameters) = RenderedEdge(colour = "#00ccff" /* bluish */, label = "")
}

// Nodes implemented inside modules
case class HellaCacheSourceNode()(implicit valName: ValName, p: Parameters) extends SourceNode(HellaCacheImp)(List(HellaCacheDummyParameters()))
case class HellaCacheSinkNode()(implicit valName: ValName, p: Parameters) extends SinkNode(HellaCacheImp)(List(HellaCacheDummyParameters()))
case class HellaCacheNexusNode()(implicit valName: ValName, p: Parameters)
	extends NexusNode(HellaCacheImp)(
		{_: Seq[HellaCacheDummyParameters] => HellaCacheDummyParameters()},
		{_: Seq[HellaCacheDummyParameters] => HellaCacheDummyParameters()})

//Xbar
class HellaCacheFanin()(implicit p: Parameters) extends LazyModule()(p) {
	val node = HellaCacheNexusNode()

	lazy val module = new LazyModuleImp(this) {
		val numPorts = node.in.size
		val (in, _) = node.in.unzip
		val out = node.out.head._1 
		
		val dcacheArb = Module(new HellaCacheArbiter(numPorts))

		in.zip(dcacheArb.io.requestor).foreach { case (in, req) => req <> in }
		out <> dcacheArb.io.mem
	}
}

