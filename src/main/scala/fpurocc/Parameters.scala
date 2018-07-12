// See LICENSE.SiFive for license details.

package freechips.rocketchip.NAMESPACE

import chisel3._
import chisel3.internal.sourceinfo.SourceInfo
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile
import scala.math.max


case class NAMESPACESinkParameters(
	fLen: Int,
	divSqrt: Boolean
)


case class NAMESPACENullParameters()


