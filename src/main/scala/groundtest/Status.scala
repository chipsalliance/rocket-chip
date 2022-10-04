// See LICENSE.SiFive for license details.

package freechips.rocketchip.groundtest

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.ValidMux

class GroundTestStatus extends Bundle {
  val timeout = Valid(UInt(4.W))
  val error = Valid(UInt(4.W))
}

object DebugCombiner {
  def apply(debugs: Seq[GroundTestStatus]): GroundTestStatus = {
    val out = Wire(new GroundTestStatus)
    out.timeout  := ValidMux(debugs.map(_.timeout))
    out.error    := ValidMux(debugs.map(_.error))
    out
  }
}
