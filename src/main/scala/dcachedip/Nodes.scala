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
class HellaCacheDiplomaticBundle()(implicit val p: Parameters) extends HellaCacheIO(p) 

object HellaCacheDiplomaticBundle {
	def apply()(implicit p: Parameters) = new HellaCacheDiplomaticBundle
}

//Parameters
case class HellaCacheNullParameters() //dummy parameters,
                                      //everything is from core parameters in this case

//Nodes
object HellaCacheImp extends SimpleNodeImp[HellaCacheNullParameters, HellaCacheNullParameters, HellaCacheEdgeParameters, HellaCacheDiplomaticBundle] {
	def edge(pd: HellaCacheNullParameters, pu: HellaCacheNullParameters, p: Parameters, sourceInfo: SourceInfo) = HellaCacheNullParameters
	//def bundle(e: HellaCacheEdgeParameters) = HellaCacheDiplomaticBundle(e.bundle)
	def bundle(implicit p: Parameters) = HellaCacheDiplomaticBundle
	def render(e: HellaCacheEdgeParameters) = RenderedEdge(colour = "#00ccff" /* bluish */, label = s("HellaCache")
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

//Xbar
class HellaCacheFanin()(implicit val p: Parameters) extends LazyModule {
	val node = HellaCacheNexusNode()

	lazy val module = new LazyModuleImp(this) {
		val numPorts = node.in.size
		val (in, _) = node.in.unzip
		val out = node.out.head._1 
		
		val dcacheArb = new HellaCacheArbiter(numPorts)

		in <> dcacheArb.io.requestor
		dcacheArb.io.mem <> out
	}
}

//ToTL Adapter Node
case class HellaCacheToTLNode()(implicit valName: ValName) extends MixedAdapterNode(HellaCacheImp, TLImp)(
	dFn = { case HellaCacheSourcePortParameters(masters) =>
		TLClientPortParameters(clients = masters.map { m =>
			TLClientParameters(name = m.name, nodePath = m.nodePath)
		})
	},
	uFn = { mp => HellaCacheSinkPortParameters(
		slaves = mp.managers.map { m =>
			def adjust(x: TransferSizes) = {
				if (x.contains(mp.beatBytes)) {
					TransferSizes(x.min, m.minAlignment.min(mp.beatBytes * HellaCacheParameters.maxTransfer).toInt)
				} else { // larger than beatBytes requires beatBytes if misaligned
					x.intersect(TransferSizes(1, mp.beatBytes))
				}
			}
		}
	)
)

class HellaCacheToTL()(implicit p: Parameters) extends LazyModule {
	val node = HellaCacheToTLNode()
	lazy val module = new LazyModuleImp(this) {
		(node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
		
		}
	}
}

object HellaCacheToTL {
	def apply()(implicit p: Parameters) = {
		val hc2tl = LazyModule(new HellaCacheToTL)
		hc2tl.node
	}
}
