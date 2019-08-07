// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util._

object Gather {
  // Compress all the valid data to the lowest indices
  def apply[T <: Data](data: Seq[ValidIO[T]]): Vec[T] = apply(data, DensePrefixSum)
  def apply[T <: Data](data: Seq[ValidIO[T]], prefixSum: PrefixSum): Vec[T] = {
    val popBits = log2Ceil(data.size)
    val holes = data.map(x => WireInit(UInt(popBits.W), (!x.valid).asUInt))
    apply(data.map(_.bits), prefixSum(holes)(_ + _))
  }
  def apply[T <: Data](data: Seq[T], holeSum: Seq[UInt], layerOp: (Int, Seq[T], Seq[UInt]) => (Seq[T], Seq[UInt]) = idLayer[T] _): Vec[T] = {
    def helper(layer: Int, offset: Int, holeSum0: Vector[UInt], data0: Vector[T]): Vector[T] = {
      val (a, b) = layerOp(layer, data0, holeSum0)
      val data = a.toVector
      val holeSum = b.toVector
      if (offset >= data.size) {
        data
      } else {
        val bit = log2Ceil(offset)
        helper(layer + 1, offset << 1, holeSum, Vector.tabulate(data.size) { i =>
          if (i+offset >= data.size) {
            data(i)
          } else {
            Mux(holeSum(i+offset-1)(bit), data(i+offset), data(i))
          }
        })
      }
    }
    VecInit(helper(0, 1, holeSum.toVector, data.toVector))
  }
  def layers(size: Int) = if (size == 0) 1 else 1+log2Ceil(size)
  def idLayer[T](layer: Int, data: Seq[T], holeSum: Seq[UInt]) = (data, holeSum)
}

object Scatter {
  def apply[T <: Data](data: Seq[ValidIO[T]]): Vec[T] = apply(data, DensePrefixSum)
  def apply[T <: Data](data: Seq[ValidIO[T]], prefixSum: PrefixSum): Vec[T] = {
    val popBits = log2Ceil(data.size)
    val holes = data.map(x => WireInit(UInt(popBits.W), (!x.valid).asUInt))
    apply(data.map(_.bits), prefixSum(holes)(_ + _))
  }
  def apply[T <: Data](data: Seq[T], holeSum: Seq[UInt], layerOp: (Int, Seq[T], Seq[UInt]) => (Seq[T], Seq[UInt]) = idLayer[T] _): Vec[T] = {
    def helper(layer: Int, offset: Int, holeSum0: Vector[UInt], data0: Vector[T]): Vector[T] = {
      val (a, b) = layerOp(layer, data0, holeSum0)
      val data = a.toVector
      val holeSum = b.toVector
      if (offset <= 0) {
        data
      } else {
        val bit = log2Ceil(offset)
        helper(layer + 1, offset >> 1, holeSum, Vector.tabulate(data.size) { i =>
          if (i < offset) {
            data(i)
          } else {
            Mux(holeSum(i-1)(bit), data(i-offset), data(i))
          }
        })
      }
    }
    val offset = if (data.size <= 1) 0 else 1 << log2Floor(data.size-1)
    VecInit(helper(0, offset, holeSum.toVector, data.toVector))
  }
  def layers(size: Int) = if (size == 0) 1 else 1+log2Ceil(size)
  def idLayer[T](layer: Int, data: Seq[T], holeSum: Seq[UInt]) = (data, holeSum)
}
