package freechips.rocketchip.tile

import chisel3._

import freechips.rocketchip.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import freechips.rocketchip.NAMESPACE._

class Fbist(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
	override lazy val module = new FbistModuleImp(this)
	override val usesFPU = true
	val passed = Output(Bool())
}


class FbistModuleImp(outer: Fbist)(implicit p: Parameters) extends LazyRoCCModuleImp(outer) 
	with HasCoreParameters {
		val in_counter = RegInit(0.U(4.W))
		val out_counter = RegInit(0.U(4.W))
		val timer = RegInit(0.U(1.W))
		val next_in_counter = in_counter
		val next_out_counter = out_counter

		val count_compare = Bool()
		val req = outer.NAMESPACENode.get.out.head._1.cp_req
		val resp = outer.NAMESPACENode.get.out.head._1.cp_resp
		//val lsfr = RegInit(0.U(fLen.W))
		//val next_lsfr = (lsfr << 1) ^ lsfr
		//val result = RegInit(0.U(fLen.W))
		//val next_result
		//result:= Mux(resp.valid, resp.data, result)
		//lsfr := next_lsfr
		//val rand_operand = RegInit(0.U(4.W))
		//val next_rand_operand = (rand_operand << 1) ^ rand_operand
		//rand_operand := next_rand_operand

		req.bits.ldst := false.B
		req.bits.swap12 := false.B
		req.bits.swap23 := false.B
		req.bits.fromint := false.B
		req.bits.toint := false.B
		req.bits.singleIn := false.B
		req.bits.singleOut := false.B
		req.bits.fastpipe := false.B
		req.bits.fma := false.B
		req.bits.div := false.B
		req.bits.sqrt := false.B
		req.bits.wflags := false.B

		req.bits.wen := false.B
		req.bits.ren1 := false.B
		req.bits.ren2 := false.B
		req.bits.ren3 := false.B

		req.bits.rm := 0.U
		req.bits.fmaCmd := 0.U
		req.bits.typ := 0.U
		req.bits.in1 := 0.U
		req.bits.in2 := 0.U
		req.bits.in3 := 0.U

		req.valid := false.B
		resp.ready := false.B
		when (timer === 0.U) {
			req.valid := true.B
			when (req.ready === true.B) {
				timer := 1.U
				out_counter := next_out_counter + 1.U
			}
		} .otherwise {
			resp.ready := true.B
			when (resp.valid === true.B) {
				timer := 0.U
				in_counter := next_in_counter + 1.U
			}
		}
		count_compare := in_counter === out_counter

		when ((in_counter >= 3.U) && count_compare) {
			outer.passed := true.B
		}
}

//nothing can extend an object
//nothing can extend a case class
//classes can extend one other class and any number of traits
//traits can extend any other traits
