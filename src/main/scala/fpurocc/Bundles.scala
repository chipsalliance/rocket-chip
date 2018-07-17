// See LICENSE.SiFive for license details.

package freechips.rocketchip.NAMESPACE

import chisel3._
import chisel3.util._
import freechips.rocketchip.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.{FPConstants, FPInput, FPResult}

//abstract class NAMESPACEBundleBase(params: NAMESPACESinkParameters) extends GenericParameterizedBundle(params)

// Signal directions are from the master's point-of-view
class NAMESPACEBundle(val params: NAMESPACESinkParameters) extends Bundle
{
	val cp_req = Decoupled(new FPInput(params.fLen)).flip
	val cp_resp = Decoupled(new FPResult(params.fLen))
}

object NAMESPACEBundle
{
	def apply(params: NAMESPACESinkParameters) = new NAMESPACEBundle(params)
}
