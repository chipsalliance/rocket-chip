// See LICENSE.SiFive for license details.

package freechips.rocketchip.NAMESPACE

import chisel3._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.util.InOrderArbiter
import freechips.rocketchip.tile.{FPInput, FPResult}
import scala.math.{min,max}

class NAMESPACEFanin()(implicit p: Parameters) extends LazyModule {
	val node = NAMESPACENexusNode(
	{case Seq(NAMESPACENullParameters) => NAMESPACENullParameters },
		sinkFn  = { seq => seq(0).copy(sinks = seq.flatMap(_.sinks)) })
	
	lazy val module = new LazyModuleImp(this) {
		require (node.edges.in.size >= 0, "NAMESPACEFanout requires at least one source")
		if (node.edges.in.size >= 1) {
			require (node.edges.out.size == 1, "NAMESPACEFanout requires at least one sink")
	
			val fpArb = Module(InOrderArbiter(new FPInput()(p), new FPResult()(p), node.edge.in.size))
			val (out, _) = node.out(0)
			val (inputs, _)= node.in.unzip
			fpArb.io.in_req <> inputs.map(_.fpu_req)
			(inputs.map(_.fpu_resp) zip fpArb.io.in_resp) foreach {
				case (out, in) => out <> in
			}
			fpArb.io.out_resp <> out.fpu_resp
			out.fpu_req <> fbArb.io.out_req
			
		}
		else {
			node.out(0)._1.fpu_req.valid := Bool(false)
			node.out(0)._1.fpu_resp.ready := Bool(false)
		}
	}
	
}
