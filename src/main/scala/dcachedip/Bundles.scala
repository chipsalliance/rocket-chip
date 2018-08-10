// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket.hellacache

import chisel3._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._

// Signal directions are from the master's point-of-view
/*
abstract class HellaCacheDiplomaticParameterSource (params: HellaCacheBundleParameters) extends Bundle

class HellaCacheDiplomaticReq(params: HellaCacheBundleParameters) {
	val addr = UInt(width = params.addrBits)
	val tag  = UInt(width = params.dcacheReqBits)
	val cmd  = UInt(width = M_SZ)
	val typ  = UInt(width = MT_SZ)
	val data = UInt(width = params.dataBits)
	val phys = Bool()
}

class HellaCacheDiplomaticResp(params: HellaCacheBundleParameters) {
	val replay = Bool()
	val has_data = Bool()
	val data_word_bypass = UInt(width = params.dataBits)
	val data_raw = UInt(width = params.dataBits)
	val store_data = UInt(width = params.dataBits)
}

class HellaCacheDiplomaticWriteData (params: HellaCacheBundleParameters) {
	val data = UInt(width = params.dataBits)
	val mask = UInt(width = (params.dataBits / 8))
}


// interface between D$ and processor/DTLB
class HellaCacheDiplomaticBundle(params: HellaCacheBundleParameters) extends Bundle {

	val req = Decoupled(new HellaCacheDiplomaticReq(params))
	val s1_kill = Output(Bool()) // kill previous cycle's req
	val s1_data = Output(new HellaCacheDiplomaticWriteData(params)) // data for previous cycle's req
	val s2_nack = Input(Bool()) // req from two cycles ago is rejected
	val s2_nack_cause_raw = Input(Bool()) // reason for nack is store-load RAW hazard (performance hint)
	val s2_kill = Output(Bool()) // kill req from two cycles ago
	
	val resp = Flipped(Valid(new HellaCacheDiplomaticResp(params)))
	val replay_next = Input(Bool())
	val s2_xcpt = Input(new HellaCacheExceptions)
	val ordered = Input(Bool())
	val perf = Input(new HellaCachePerfEvents())

}

object HellaCacheDiplomaticBundle {
	def apply(params: HellaCacheBundleParameters) = new HellaCacheDiplomaticBundle(params)
}
*/

class HellaCacheDiplomaticBundle()(implicit val p: Parameters) extends HellaCacheIO(p) 
//{
//	val mem = new HellaCacheIO()
//}

object HellaCacheDiplomaticBundle {
	def apply(implicit p: Parameters) = new HellaCacheDiplomaticBundle
}
