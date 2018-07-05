// See LICENSE.SiFive for license details.

package freechips.rocketchip.NAMESPACE

import chisel3._
import freechips.rocketchip.util.GenericParameterizedBundle
import freechips.rocketchip.tile.{FPConstants, FPInput, FPResult}

//abstract class NAMESPACEBundleBase(params: NAMESPACESinkParameters) extends GenericParameterizedBundle(params)

// Signal directions are from the master's point-of-view
class NAMESPACEBundle(params: NAMESPACESinkParameters) extends GenericParameterizedBundle
{
	val fpu_req = new FPInput()(p.alter{})
	val fpu_resp = new FPResult()(p.alter{})
}

object NAMESPACEBundle
{
	def apply(params: NAMESPACESinkParameters) = new NAMESPACEBundle(params)
}
