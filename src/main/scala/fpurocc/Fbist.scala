package freechips.rocketchip.tile

import chisel3._

import freechips.rocketchip.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

class Fbist(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes) {
	override lazy val module = new FbistModuleImp(this)
	override val usesFPU = true
}


class FbistModuleImp(outer: Fbist)(implicit p: Parameters) extends LazyRoCCModuleImp(outer) 
	with HasCoreParameters {
		//val regfile = Mem(outer.n, UInt(width = UInt(fLen)))
		val lsfr = RegInit(0.U(fLen.W))
		val next_lsfr = (lsfr << 1) ^ lsfr
		val result = RegInit(0.U(fLen.W))
		/*
		result:= Mux(node.edges.in.head._1.cp_resp.valid, io.cp_resp.data, result)
		lsfr := next_lsfr
		
		val rand_operand = RegInit(0.U(4.W))
		val next_rand_operand = (rand_operand << 1) ^ rand_operand
		rand_operand := next_rand_operand

		switch (rand_operand) {
			is(0.U) {
				io.cp_req.wen := somehting
			.default {
				io.cp_req.wen := 34
				other signal := somehting


		io.cp_req.ldst := false.B
		io.cp_req.swap12 := false.B
		io.cp_req.swap23 := false.B
		io.cp_req.fromint := false.B
		io.cp_req.toint := false.B
		io.cp_req.fastpipe := false.B
		io.cp_req.fma := false.B
		io.cp_req.div := false.B
		io.cp_req.wflags := false.B

*/


		

}

//nothing can extend an object
//nothing can extend a case class
//classes can extend one other class and any number of traits
//traits can extend any other traits
