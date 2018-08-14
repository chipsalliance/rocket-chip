// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile.fpucp

import chisel3._
import chisel3.util._
import freechips.rocketchip.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.{FPConstants, FPInput, FPResult}


// Signal directions are from the master's point-of-view
class FPUCPBundle(val params: FPUCPSinkParameters) extends Bundle
{
	val cp_req = Decoupled(new FPInput(params.fLen))
	val cp_resp = Flipped(Decoupled(new FPResult(params.fLen)))
}

object FPUCPBundle
{
	def apply(params: FPUCPSinkParameters) = new FPUCPBundle(params)
}
