// See LICENSE.SiFive for license details.

package freechips.rocketchip.amba.ahb

import Chisel._
import freechips.rocketchip.util.GenericParameterizedBundle
import freechips.rocketchip.tile.{FPInput, FPResult}

abstract class NAMESPACEBundleBase(params: NAMESPACEBundleParameters) extends GenericParameterizedBundle(params)

// Signal directions are from the master's point-of-view
class NAMESPACEBundle(params: NAMESPACEBundleParameters) extends NAMESPACEBundleBase(params)
{
	val fpu_req = Decoupled(new FPInput()).flip
	val fpu_resp = Decoupled(new FPResult())
}

object NAMESPACEBundle
{
	def apply(params: NAMESPACEBundleParameters) = new NAMESPACEBundle(params)
}
