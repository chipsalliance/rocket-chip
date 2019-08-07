// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3.util.{log2Ceil, log2Floor}

trait PrefixSum {
  // out[0] = summands[0]
  // out[1] = summands[0] + summands[1]
  // out[2] = summands[0] + summands[1] + summands[2]
  // ...
  // where + is your associative operator (reflexivity not required)
  // layerOp is called on each level of the circuit
  def apply[T](summands: Seq[T])(associativeOp: (T, T) => T, layerOp: (Int, Vector[T]) => Vector[T] = idLayer[T] _): Vector[T]
  def layers(size: Int): Int
  def idLayer[T](x: Int, y: Vector[T]) = y
}

// N-1 area, N-1 depth
object RipplePrefixSum extends PrefixSum {
  def layers(size: Int) = if (size == 0) 1 else size
  def apply[T](summands: Seq[T])(associativeOp: (T, T) => T, layerOp: (Int, Vector[T]) => Vector[T]): Vector[T] = {
    def helper(layer: Int, offset: Int, x: Vector[T]): Vector[T] = {
      if (offset >= x.size) {
        x
      } else {
        helper(layer+1, offset+1, layerOp(layer, Vector.tabulate(x.size) { i =>
          if (i != offset) {
            x(i)
          } else {
            associativeOp(x(i-1), x(i))
          }
        }))
      }
    }
    helper(1, 1, layerOp(0, summands.toVector))
  }
}

// O(NlogN) area, logN depth
object DensePrefixSum extends PrefixSum {
  def layers(size: Int) = if (size == 0) 1 else 1+log2Ceil(size)
  def apply[T](summands: Seq[T])(associativeOp: (T, T) => T, layerOp: (Int, Vector[T]) => Vector[T]): Vector[T] = {
    def helper(layer: Int, offset: Int, x: Vector[T]): Vector[T] = {
      if (offset >= x.size) {
        x
      } else {
        helper(layer+1, offset << 1, layerOp(layer, Vector.tabulate(x.size) { i =>
          if (i < offset) {
            x(i)
          } else {
            associativeOp(x(i-offset), x(i))
          }
        }))
      }
    }
    helper(1, 1, layerOp(0, summands.toVector))
  }
}

// 2N area, 2logN depth
object SparsePrefixSum extends PrefixSum {
  def layers(size: Int) = if (size <= 1) 1 else 2*log2Floor(size) +
                            (if (2*size >= (3 << log2Floor(size))) 1 else 0)
  def apply[T](summands: Seq[T])(associativeOp: (T, T) => T, layerOp: (Int, Vector[T]) => Vector[T]): Vector[T] = {
    def contract(layer: Int, offset: Int, x: Vector[T]): Vector[T] = {
      val double = offset << 1
      val offset1 = offset - 1
      if (offset <= 0) {
        x
      } else if (double+offset1 >= x.size) {
        contract(layer, offset >> 1, x)
      } else {
        contract(layer+1, offset >> 1, layerOp(layer, Vector.tabulate(x.size) { i =>
          if (i % double == offset1 && i >= offset) {
            associativeOp(x(i-offset), x(i))
          } else {
            x(i)
          }
        }))
      }
    }
    def expand(layer: Int, offset: Int, x: Vector[T]): Vector[T] = {
      val double = offset << 1
      val double1 = double - 1
      if (double1 >= x.size) {
        contract(layer, offset >> 1, x)
      } else {
        expand(layer+1, double, layerOp(layer, Vector.tabulate(x.size) { i =>
          if (i % double == double1) {
            associativeOp(x(i-offset), x(i))
          } else {
            x(i)
          }
        }))
      }
    }
    expand(1, 1, layerOp(0, summands.toVector))
  }
}

object TestPrefixSums {
  def testSize(size: Int) {
    val input = Seq.tabulate(size) { i => Seq(i) }
    var last: Int = 0
    var value: Vector[Seq[Int]] = Vector.empty
    def layers(layer: Int, x: Vector[Seq[Int]]) = {
      require (layer == last, "Discontiguous layers")
      require (x != value, "Useless layer detected")
      last += 1
      value = x
      x
    }

    last = 0
    value = Vector.fill(1) { Seq(9) }
    val ripple = RipplePrefixSum(input)(_ ++ _, layers)
    val rippleL = RipplePrefixSum.layers(input.size)
    require (last == rippleL, s"Wrong layers for RipplePrefixSum; ${last} != ${rippleL}")

    last = 0
    value = Vector.fill(1) { Seq(9) }
    val dense = DensePrefixSum (input)(_ ++ _, layers)
    val denseL = DensePrefixSum.layers(input.size)
    require (last == denseL, s"Wrong layers for DensePrefixSum; ${last} != ${denseL}")
    require (ripple == dense, s"DensePrefixSum bug: ${ripple} != ${dense}")

    last = 0
    value = Vector.fill(1) { Seq(9) }
    val sparse = SparsePrefixSum(input)(_ ++ _, layers)
    val sparseL = SparsePrefixSum.layers(input.size)
    require (last == sparseL, s"Wrong layers for SparsePrefixSum; ${last} != ${sparseL}")
    require (ripple == sparse, s"SparsePrefixSum bug: ${ripple} != ${sparse}")

    println(s"PrefixSums correct for size ${size}")
  }

  def test { Seq.tabulate(519){i=>i}.foreach(testSize) }
}
