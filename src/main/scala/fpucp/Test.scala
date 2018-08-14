package freechips.rocketchip.tile.fpucp

import chisel3._
import chisel3.internal.sourceinfo.{SourceLine, SourceInfo}
//import chisel3.experimental.dontTouch
import freechips.rocketchip.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import freechips.rocketchip.unittest._

//OpcodeSet isn't necessary here, but just passing in one to make sure it works
class Fbist(opcodes: OpcodeSet = OpcodeSet.custom0)(implicit p: Parameters) extends LazyRoCC(opcodes, usesFPU = true) {
	override lazy val module = new FbistModuleImp(this)
	//override val usesFPU = true
}


class FbistModuleImp(outer: Fbist)(implicit p: Parameters) extends LazyRoCCModuleImp(outer) 
	with HasCoreParameters {
		val finished = IO(Output(Bool()))
		
		val in_counter = RegInit(0.U(4.W))
		val out_counter = RegInit(0.U(4.W))
		val timer = RegInit(0.U(13.W))
		val next_in_counter = in_counter
		val next_out_counter = out_counter

		val count_compare = Wire(Bool())
		val req = outer.FPUCPNode.get.out.head._1.cp_req
		val resp = outer.FPUCPNode.get.out.head._1.cp_resp
		val saved_result = RegNext(resp.bits.data, 123.U)
		//val saved_result = RegNext(resp.data, 123.U)
		//println(s"resp elements are ${resp.elements}")
		//println(s"resp is $resp")
		//println(s"resp data is ${resp.data}")
		//System.exit(0)

		//dontTouch(req)
		//dontTouch(resp)
		//dontTouch(timer)
		//dontTouch(in_counter)
		//dontTouch(out_counter)
		//dontTouch(finished)
		req.bits.ldst := false.B
		req.bits.swap12 := false.B
		req.bits.swap23 := false.B
		req.bits.fromint := false.B
		req.bits.toint := false.B
		req.bits.singleIn := false.B
		req.bits.singleOut := false.B
		req.bits.fastpipe := false.B
		req.bits.fma := true.B
		req.bits.div := false.B
		req.bits.sqrt := false.B
		req.bits.wflags := false.B

		req.bits.wen := false.B
		req.bits.ren1 := true.B
		req.bits.ren2 := true.B
		req.bits.ren3 := true.B

		req.bits.rm := 0.U
		req.bits.fmaCmd := 0.U
		req.bits.typ := 0.U
		req.bits.in1 := 0.U
		req.bits.in2 := 0.U
		req.bits.in3 := 0.U

		req.valid := false.B
		finished := false.B
		val next_timer = timer + 1.U
		timer := next_timer
		when (timer === 0.U) {
			req.valid := true.B
			when (req.ready === true.B) {
				out_counter := next_out_counter + 1.U
				req.bits.in1 := saved_result
				req.bits.in2 := 456.U
				req.bits.in3 := 789.U
			}
		}
		resp.ready := true.B
		when (resp.valid === true.B) {
			in_counter := next_in_counter + 1.U
		}
		
		count_compare := (in_counter === out_counter)

		when ((in_counter >= 4.U) && count_compare) {
			finished := true.B
		}
		//println(s"finished is $finished")
}

//nothing can extend an object
//nothing can extend a case class
//classes can extend one other class and any number of traits
//traits can extend any other traits

class FPUCPGateway()(implicit p: Parameters, sourceInfo: SourceInfo) extends LazyModule {

	//ONLY ADDING THESE THINGS TO GET A TLEdgeOut which is needed because of implicit p
	val manager1_addr = List(AddressSet(0x10000, 0x0F0FF), AddressSet(0x30000, 0x0F0FF))
	val manager2_addr = List(AddressSet(0x20000, 0x0FFFF), AddressSet(0x60000, 0x00F0F))
	val manager3_addr = List(AddressSet(0x50000, 0x0F0FF), AddressSet(0x70000, 0x00F0F))
	// TODO make different transfer sizes
	//    currently make the same to help w
	val full_feature_manager1 = TLManagerParameters(
		address=manager1_addr,
		regionType = RegionType.CACHED,
		executable = true,
		nodePath = Seq(),
		supportsAcquireT = TransferSizes(1, 4),
		supportsAcquireB = TransferSizes(1, 4),
		supportsArithmetic = TransferSizes.none,  // TransferSizes(2, 8),
		supportsLogical = TransferSizes.none,  // TransferSizes(1, 8),
		supportsGet = TransferSizes(1, 4),
		supportsPutFull = TransferSizes(1, 4),
		supportsPutPartial = TransferSizes(1, 4),
		supportsHint = TransferSizes(1, 4),
		fifoId = None)
	val full_feature_manager2 = TLManagerParameters(
		address=manager2_addr,
		regionType = RegionType.CACHED,
		executable = false,
		nodePath = Seq(),
		supportsAcquireT = TransferSizes(1, 4),
		supportsAcquireB = TransferSizes(1, 4),
		supportsArithmetic = TransferSizes.none, // TransferSizes(1, 8),
		supportsLogical = TransferSizes.none, // TransferSizes(1, 4),
		supportsGet = TransferSizes(1, 4),
		supportsPutFull = TransferSizes(1, 4),
		supportsPutPartial = TransferSizes(1, 4),
		supportsHint = TransferSizes(1, 4),
		// TODO Current check does not support different fifoId for different managers
		fifoId = None)
	val full_feature_manager3 = TLManagerParameters(
		address=manager3_addr,
		regionType = RegionType.CACHED,
		executable = false,
		nodePath = Seq(),
		supportsAcquireT = TransferSizes(1, 4),
		supportsAcquireB = TransferSizes(1, 4),
		supportsArithmetic = TransferSizes.none, // TransferSizes(2, 4),
		supportsLogical = TransferSizes.none,  // TransferSizes(1, 2),
		supportsGet = TransferSizes(1, 4),
		supportsPutFull = TransferSizes(1, 4),
		supportsPutPartial = TransferSizes(1, 4),
		supportsHint = TransferSizes(1, 4),
		fifoId = None)
	
	val manager_parameters1 = TLManagerPortParameters(
		managers = Seq(full_feature_manager1, full_feature_manager2),
		beatBytes = 1,
		endSinkId=3)
	
	val client1_params = TLClientParameters(
		// Might need to increase this to allow more than two operations in flight at the same time
		name = "client1",
		sourceId = IdRange(0, 1),
		nodePath = Seq(),
		supportsProbe = TransferSizes(1, 4),
		supportsArithmetic = TransferSizes(1, 4),
		supportsLogical = TransferSizes(1, 4),
		supportsGet = TransferSizes(1, 4),
		supportsPutFull = TransferSizes(1, 4),
		supportsPutPartial = TransferSizes(1, 4),
		supportsHint = TransferSizes(1, 4))
	val client2_params = TLClientParameters(
		name = "client2",
		sourceId = IdRange(3, 5),
		nodePath = Seq(),
		supportsProbe = TransferSizes(1, 4),
		supportsArithmetic = TransferSizes(1, 4),
		supportsLogical = TransferSizes(1, 4),
		supportsGet = TransferSizes(1, 4),
		supportsPutFull = TransferSizes(1, 4),
		supportsPutPartial = TransferSizes(1, 4),
		supportsHint = TransferSizes(1, 4))
	
	
	val client_parameters1 = TLClientPortParameters(clients = Seq(client1_params, client2_params))

	val sharedMemoryTLEdge = new TLEdgeOut(client = client_parameters1, manager = manager_parameters1, params = p, sourceInfo = sourceInfo)

	//REAL CODE STARTS HERE
	val rocc = List(LazyModule(new Fbist()(p.alter((site, here, up) => {case TileKey => site(RocketTilesKey).head 
		case SharedMemoryTLEdge => sharedMemoryTLEdge}))),
		LazyModule(new Fbist()(p.alter((site, here, up) => {case TileKey => site(RocketTilesKey).head 
		case SharedMemoryTLEdge => sharedMemoryTLEdge}))),
		LazyModule(new Fbist()(p.alter((site, here, up) => {case TileKey => site(RocketTilesKey).head 
		case SharedMemoryTLEdge => sharedMemoryTLEdge})))
	)
	val xbar = LazyModule(new FPUCPFanin)
	val fpu = LazyModule(new LazyFPU(new FPUParams)(p.alter((site, here, up) => {case TileKey => site(RocketTilesKey).head 
		case SharedMemoryTLEdge => sharedMemoryTLEdge}))
	)

	rocc.map(_.FPUCPNode).foreach { _.foreach { dip_node => xbar.node := dip_node}}
	fpu.node := xbar.node
	lazy val module = new LazyModuleImp(this) with UnitTestModule {
		fpu.module.io.valid := false.B //turning off inputs from the core
		io.finished := rocc.map(_.module.finished).reduce(_&_)
	}

}

class FPUCPTest(timeout: Int = 50000)(implicit p: Parameters) extends UnitTest(timeout) {
	val dut = Module(LazyModule(new FPUCPGateway()).module)
	io.finished := dut.io.finished
	dut.io.start := DontCare
}
