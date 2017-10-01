// See LICENSE.SiFive for license details.

package freechips.rocketchip.groundtest

import Chisel._
import freechips.rocketchip.util.ValidMux

class GroundTestStatus extends Bundle {
  val finished = Bool(OUTPUT)
  val timeout = Valid(UInt(width = 4))
  val error = Valid(UInt(width = 4))
}

object DebugCombiner {
  def apply(debugs: Seq[GroundTestStatus]): GroundTestStatus = {
    val out = Wire(new GroundTestStatus)
    out.finished := debugs.map(_.finished).reduce(_ && _)
    out.timeout  := ValidMux(debugs.map(_.timeout))
    out.error    := ValidMux(debugs.map(_.error))
    out
  }
}

