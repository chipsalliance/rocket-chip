// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util.DecoupledIO

/** Blocks transactions until the cycle after reset. */
object BlockDuringReset
{
  def apply[T <: Data](enq: DecoupledIO[T]): DecoupledIO[T] = {
    val out_of_reset = RegNext(true.B, false.B)
    val res = Wire(enq.cloneType)
    res.valid := enq.valid
    enq.ready := res.ready
    res.bits := enq.bits
    when (!out_of_reset) {
      res.valid := false.B
      enq.ready := false.B
    }
    res
  }

  def apply(enq: Bool): Bool = {
    val out_of_reset = RegNext(true.B, false.B)
    enq && out_of_reset
  }
}
