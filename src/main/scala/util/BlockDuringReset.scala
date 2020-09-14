// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util.{Counter, DecoupledIO}

/** Blocks transactions until the cycle after reset. */
object BlockDuringReset
{
  private def outOfReset(stretchCycles: Int): Bool = stretchCycles match {
    case 0 => RegNext(true.B, false.B)
    case i => Counter(true.B, i)._2
  }

  def apply[T <: Data](enq: DecoupledIO[T], stretchCycles: Int = 0): DecoupledIO[T] = {
    val out_of_reset = outOfReset(stretchCycles)
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

  def apply(valid: Bool, stretchCycles: Int): Bool = {
    val out_of_reset = outOfReset(stretchCycles)
    valid && out_of_reset
  }

  def apply[T <: DataCanBeValid](data: T, stretchCycles: Int): T = {
    val out_of_reset = outOfReset(stretchCycles)
    val res = Wire(data.cloneType)
    res := data
    res.valid := data.valid && out_of_reset
    res
  }

  def apply[T <: DataCanBeValid](data: Vec[T], stretchCycles: Int): Vec[T] = {
    val out_of_reset = outOfReset(stretchCycles)
    val res = Wire(data.cloneType)
    res.zip(data)foreach { case (r, d) =>
      r := d
      r.valid := d.valid && out_of_reset
    }
    res
  }
}
