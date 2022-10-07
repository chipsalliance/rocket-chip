// See LICENSE.SiFive for license details.

package freechips.rocketchip.interrupts

import chisel3._
import freechips.rocketchip.util._

class SyncInterrupts(params: IntEdge) extends GenericParameterizedBundle(params)
{
  val sync = Vec(params.source.num, Bool())
}
