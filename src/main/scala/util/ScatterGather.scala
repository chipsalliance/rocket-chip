// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util._

object Gather {
  // Compress all the valid data to the lowest indices
  def apply[T <: Data](data: Seq[ValidIO[T]], prefixSum: PrefixSum = DensePrefixSum): Vec[T] = {
    val popBits = log2Ceil(data.size)
    val holes = data.map(x => WireInit(UInt(popBits.W), (!x.valid).asUInt))
    apply(data.map(_.bits), prefixSum(holes)(_ + _))
  }
  def apply[T <: Data](data: Seq[T], prefixHoleSum: Seq[UInt]): Vec[T] = {
    def helper(offset: Int, x: Vector[T]): Vector[T] = {
      if (offset >= x.size) {
        x
      } else {
        val bit = log2Ceil(offset)
        helper(offset << 1, Vector.tabulate(x.size) { i =>
          if (i+offset >= x.size) {
            x(i)
          } else {
            Mux(prefixHoleSum(i+offset-1)(bit), x(i+offset), x(i))
          }
        })
      }
    }
    VecInit(helper(1, data.toVector))
  }
}

object Scatter {
  def apply[T <: Data](data: Seq[ValidIO[T]], prefixSum: PrefixSum = DensePrefixSum): Vec[T] = {
    val popBits = log2Ceil(data.size)
    val holes = data.map(x => WireInit(UInt(popBits.W), (!x.valid).asUInt))
    apply(data.map(_.bits), prefixSum(holes)(_ + _))
  }
  def apply[T <: Data](data: Seq[T], prefixHoleSum: Seq[UInt]): Vec[T] = {
    def helper(offset: Int, x: Vector[T]): Vector[T] = {
      if (offset <= 0) {
        x
      } else {
        val bit = log2Ceil(offset)
        helper(offset >> 1, Vector.tabulate(x.size) { i =>
          if (i < offset) {
            x(i)
          } else {
            Mux(prefixHoleSum(i-1)(bit), x(i-offset), x(i))
          }
        })
      }
    }
    val offset = if (data.size <= 1) 0 else 1 << log2Floor(data.size-1)
    VecInit(helper(offset, data.toVector))
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

  val output = Gather(input)
  val total = PopCount(mask)
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

  val output = Scatter(input)
  val sum = RipplePrefixSum(0.U(bits.W) +: mask.toBools.map { x => WireInit(UInt(bits.W), x) })(_+_)
  for (i <- 0 until size) assert (!mask(i) || output(i) === sum(i))
}
