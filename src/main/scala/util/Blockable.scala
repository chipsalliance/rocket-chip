// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._

import org.chipsalliance.rocketutils.Blockable

import freechips.rocketchip.tile.TraceBundle
import freechips.rocketchip.rocket.TracedInstruction

object BlockableTrace {
  implicit object BlockableTraceBundle extends Blockable[TraceBundle] {
    def blockWhile(enable_blocking: Bool, data: TraceBundle) = {
      val blocked = WireInit(data)
      blocked.insns := implicitly[Blockable[Vec[TracedInstruction]]].blockWhile(enable_blocking, data.insns)
      blocked
    }
  }
}
