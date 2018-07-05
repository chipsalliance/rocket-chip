// See LICENSE.SiFive for license details.

package freechips.rocketchip.NAMESPACE

import chisel3._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.util.InOrderArbiter
import freechips.rocketchip.tile.{FPInput, FPResult}
import scala.math.{min,max}

class NAMESPACEFanout()(implicit p: Parameters) extends LazyModule {
	val node = NAMESPACENexusNode(
		sourceFn = { case Seq(m) => m },
		sinkFn  = { seq => seq(0).copy(sinks = seq.flatMap(_.sinks)) }<)
	
	lazy val module = new LazyModuleImp(this) {
		if (node.edges.in.size >= 1) {
		require (node.edges.in.size > 0, "NAMESPACEFanout requires at least one source")
		require (node.edges.out.size > 0, "NAMESPACEFanout requires at least one sink")
	
		val fpArb = Module(InOrderArbiter(new FPInput()(p), new FPResult()(p), node.edge.in.size) 
	
		}
	}
}
