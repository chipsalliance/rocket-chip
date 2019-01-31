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

import freechips.rocketchip.unittest._

class GatherTest(size: Int, timeout: Int = 500000) extends UnitTest(timeout) {
  val bits = log2Ceil(size+1)
  val mask = RegInit(0.U(size.W))
  io.finished := mask.andR
  mask := mask + !io.finished

  // Put 0, 1, 2, 3 ... at all of the lanes with mask=1
  val sum = RipplePrefixSum(0.U(bits.W) +: mask.asBools.map { x => WireInit(UInt(bits.W), x) })(_+_)
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
  val sum = RipplePrefixSum(0.U(bits.W) +: mask.asBools.map { x => WireInit(UInt(bits.W), x) })(_+_)
  for (i <- 0 until size) assert (!mask(i) || output(i) === sum(i))
}
