// See LICENSE.SiFive for license details.

package freechips.rocketchip.NAMESPACE

import chisel3._
import chisel3.experimental.dontTouch
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.util.InOrderArbiter
import freechips.rocketchip.tile.{FPInput, FPResult}
import scala.math.{min,max}

class NAMESPACEFanin()(implicit p: Parameters) extends LazyModule {
	val node = NAMESPACENexusNode(
		sinkFn  = { seq => seq(0) }
	)
	//val dummy = node.edges.in.size
	lazy val module = new LazyModuleImp(this) {
		require (node.edges.in.size >= 0, "NAMESPACEFanout requires at least one source")
		if (node.edges.in.size >= 1) {
			require (node.edges.out.size == 1, "NAMESPACEFanout requires at least one sink")
			val fLen = node.edges.out.head.fLen
			val fpArb = Module(new InOrderArbiter(new FPInput(fLen), new FPResult(fLen), node.edges.in.size))
			val (out, _) = node.out(0)
			val (inputs, _)= node.in.unzip
			fpArb.io.in_req <> inputs.map(_.cp_req)
			(inputs.map(_.cp_resp) zip fpArb.io.in_resp) foreach {
				case (out, in) => out <> in
			}
	
			//dontTouch(out)
			//dontTouch(inputs.head)
			fpArb.io.out_resp <> out.cp_resp

			out.cp_req <> fpArb.io.out_req

		}
		else {
			node.out(0)._1.cp_req.valid := false.B
			node.out(0)._1.cp_resp.ready := false.B
		}
	}
	
}
