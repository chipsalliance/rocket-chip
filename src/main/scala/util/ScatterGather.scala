// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util._

object Gather {
  // Compress all the valid data to the lowest indices
  def apply[T <: Data](data: Seq[ValidIO[T]], prefixSum: PrefixSum = DensePrefixSum): (UInt, Vec[T]) = {
    val popBits = log2Ceil(data.size+1)
    val holes = data.map(x => WireInit(UInt(popBits.W), (!x.valid).asUInt))
    val shift = prefixSum(holes)(_ + _)
    def helper(offset: Int, x: Vector[T]): Vector[T] = {
      if (offset >= x.size) {
        x
      } else {
        val bit = log2Ceil(offset)
        helper(offset << 1, Vector.tabulate(x.size) { i =>
          if (i+offset >= x.size) {
            x(i)
          } else {
            Mux(shift(i+offset-1)(bit), x(i+offset), x(i))
          }
        })
      }
    }
    val set = data.size.U - shift.last
    val bits = helper(1, data.map(_.bits).toVector)
    (set, VecInit(bits))
  }
}

object Scatter {
  def apply[T <: Data](data: Seq[ValidIO[T]], prefixSum: PrefixSum = DensePrefixSum): (UInt, Vec[T]) = {
    val popBits = log2Ceil(data.size+1)
    val holes = data.map(x => WireInit(UInt(popBits.W), (!x.valid).asUInt))
    val shift = prefixSum(holes)(_ + _)
    def helper(offset: Int, x: Vector[T]): Vector[T] = {
      if (offset <= 0) {
        x
      } else {
        val bit = log2Ceil(offset)
        helper(offset >> 1, Vector.tabulate(x.size) { i =>
          if (i < offset) {
            x(i)
          } else {
            Mux(shift(i-1)(bit), x(i-offset), x(i))
          }
        })
      }
    }
    val set = data.size.U - shift.last
    val offset = if (data.size <= 1) 0 else 1 << log2Floor(data.size-1)
    val bits = helper(offset, data.map(_.bits).toVector)
    (set, VecInit(bits))
  }
}

import freechips.rocketchip.unittest._

class GatherTest(size: Int, timeout: Int = 500000) extends UnitTest(timeout) {
  val bits = log2Ceil(size+1)
  val mask = RegInit(0.U(size.W))
  io.finished := mask.andR
  mask := mask + !io.finished

  // Put 0, 1, 2, 3 ... at all of the lanes with mask=1
  val sum = RipplePrefixSum(0.U(bits.W) +: mask.toBools.map { x => WireInit(UInt(bits.W), x) })(_+_)
  val input = Wire(Vec(size, Valid(UInt(bits.W))))
  for (i <- 0 until size) {
    input(i).valid := mask(i)
    input(i).bits := Mux(mask(i), sum(i), sum.last)
  }

  val (total, output) = Gather(input)

  assert (total === PopCount(mask))
  for (i <- 0 until size) assert (i.U >= total || output(i) === i.U)
}

class ScatterTest(size: Int, timeout: Int = 500000) extends UnitTest(timeout) {
  val bits = log2Ceil(size+1)
  val mask = RegInit(0.U(size.W))
  io.finished := mask.andR
  mask := mask + !io.finished

  val input = Wire(Vec(size, Valid(UInt(bits.W))))
  for (i <- 0 until size) {
    input(i).valid := mask(i)
    input(i).bits := i.U
  }

  val (total, output) = Scatter(input)

  val sum = RipplePrefixSum(0.U(bits.W) +: mask.toBools.map { x => WireInit(UInt(bits.W), x) })(_+_)
  assert (total === PopCount(mask))
  for (i <- 0 until size) assert (!mask(i) || output(i) === sum(i))
}
