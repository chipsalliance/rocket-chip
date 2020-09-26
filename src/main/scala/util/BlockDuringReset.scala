// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util.{Counter, RegEnable}

/** Blocks transactions until the cycle after reset. */
object BlockDuringReset
{
  private def outOfReset(stretchCycles: Int): Bool = stretchCycles match {
    case 0 => RegNext(true.B, false.B)
    case i => RegEnable(true.B, false.B, Counter(true.B, i)._2)
  }

  def apply[T <: Data : Blockable](data: T, stretchCycles: Int = 0): T = {
    implicitly[Blockable[T]].blockWhile(!outOfReset(stretchCycles), data)
  }
}
