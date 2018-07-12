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
		val regfile = Mem(outer.n, UInt(width = fLen))
		val lsfr = RegInit(0.U(fLen.W))
		val next_lsfr = (lsfr << 1) ^ lsfr
		lsfr := next_lsfr
		io.cp_req.ldst := false
		io.cp_req.wflags := false
		io.cp_req.fromint := false
		io.cp_req.toint := false
		io.cp_req.fastpipe := false
		

}


