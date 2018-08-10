// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket.hellacache

import chisel3._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.rocket._
import scala.math.{min,max}
/*
class HellaCacheFanin() extends LazyModule {
	val node = HellaCacheNexusNode()

	lazy val module = new LazyModuleImp(this) {
		val numPorts = node.in.size
		val (in, _) = node.in.unzip
		val out = node.out.head._1 
		
		if (numPorts == 1) {
			out <> in.head
		} else {
			val s1_id = Reg(UInt())
			val s2_id = Reg(next=s1_id)
		
			out.req.valid := in.map(_.req.valid).reduce(_||_)
			in(0).req.ready := out.req.ready
			for (i <- 1 until numPorts)
				in(i).req.ready := in(i-1).req.ready && !in(i-1).req.valid
			
			for (i <- numPorts-1 to 0 by -1) {
				val in_req = in(i).req
				def connect_s0() = {
					out.req.bits.cmd  := in_req.bits.cmd
					out.req.bits.typ  := in_req.bits.typ
					out.req.bits.addr := in_req.bits.addr
					out.req.bits.phys := in_req.bits.phys
					out.req.bits.tag  := Cat(in_req.bits.tag, UInt(i, log2Up(n)))
					s1_id := UInt(i)
				}
				def connect_s1() = {
					out.s1_kill := in(i).s1_kill
					out.s1_data := in(i).s1_data
				}
				def connect_s2() = {
					out.s2_kill := in(i).s2_kill
				}
				
				if (i == numPorts-1) {
					connect_s0()
					connect_s1()
					connect_s2()
				} else {
					when (in_req.valid) { connect_s0() }
					when (s1_id === UInt(i)) { connect_s1() }
					when (s2_id === UInt(i)) { connect_s2() }
				}
			}
			
			for (i <- 0 until numPorts) {
				val in_resp = in(i).resp
				val tag_hit = out.resp.bits.tag(log2Up(n)-1,0) === UInt(i)
				resp.valid := out.resp.valid && tag_hit
				in(i).s2_xcpt := out.s2_xcpt
				in(i).ordered := out.ordered
				in(i).perf := out.perf
				in(i).s2_nack := out.s2_nack && s2_id === UInt(i)
				in(i).s2_nack_cause_raw := out.s2_nack_cause_raw
				resp.bits := out.resp.bits
				resp.bits.tag := out.resp.bits.tag >> log2Up(numPorts)
				
				in(i).replay_next := out.replay_next
			}
		}
	}
}
*/

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
