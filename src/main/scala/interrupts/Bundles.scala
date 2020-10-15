// See LICENSE.SiFive for license details.

package freechips.rocketchip.interrupts

import chisel3._

class SyncInterrupts(val params: IntEdge) extends Bundle
{
  val sync = Vec(params.num, Bool())
}
