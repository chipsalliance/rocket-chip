// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util._

/** Given a list of (frequency, value) pairs, return a random value
  * according to the frequency distribution.  The sum of the
  * frequencies in the distribution must be a power of two.
  */
object Frequency {
  def apply(dist : List[(Int, Bits)]) : Bits = {
    // Distribution must be non-empty
    require(dist.length > 0)

    // Require that the frequencies sum to a power of two
    val (freqs, vals) = dist.unzip
    val total = freqs.sum
    require(isPow2(total))

    // First item in the distribution
    val (firstFreq, firstVal) = dist.head

    // Result wire
    val result = Wire(Bits(firstVal.getWidth.W))
    result := 0.U

    // Random value
    val randVal = LCG(log2Up(total))

    // Pick return value
    var count = firstFreq
    var select = when (randVal < firstFreq.U) { result := firstVal }
    for (p <- dist.drop(1)) {
      count = count + p._1
      select = select.elsewhen(randVal < count.U) { result := p._2 }
    }

    return result
  }
}

