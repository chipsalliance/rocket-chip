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

  def apply(valid: Bool): Bool = {
    val out_of_reset = RegNext(true.B, false.B)
    valid && out_of_reset
  }

  def apply[T <: DataCanBeValid](data: T): T = {
    val out_of_reset = RegNext(true.B, false.B)
    val res = Wire(data.cloneType)
    res := data
    res.valid := data.valid && out_of_reset
    res
  }

  def apply[T <: DataCanBeValid](data: Vec[T]): Vec[T] = {
    val out_of_reset = RegNext(true.B, false.B)
    val res = Wire(data.cloneType)
    res.zip(data)foreach { case (r, d) =>
      r := d
      r.valid := d.valid && out_of_reset
    }
    res
  }
}
