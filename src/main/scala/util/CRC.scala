// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util._

object CRC
{
  // A divisor is interpretted using an explicit+1 CRC notation where bit 0 is required to be one.
  // For example, a divisor of 0x1d5 is interpretted to be x^8 + x^7 + x^6 + x^4 + x^2 + 1
  // Let n be the highest term in the divisor; n=8 in 0x1d5.
  // Then, this function calculates c mod d, returning an n-bit UInt.
  // coefficient.width must be <= width
  def apply(divisor: BigInt, coefficient: UInt, width: Integer): UInt = {
    require (divisor > 0 && divisor.testBit(0))
    require (width > 0)
    assert (coefficient >> width === 0.U)
    val n = log2Floor(divisor)
    val m = width
    if (m <= n) return coefficient

    // Initialize the reduction matrix
    val array = Array.tabulate(m) { BigInt(1) << _ }
    // Reduce the matrix of terms larger than n
    for {
      i <- (n until m).reverse
      j <- 0 to n
      if divisor.testBit(j)
    } array(i-(n-j)) ^= array(i)
    // Construct the circuit
    Cat(Seq.tabulate(n) { i => (array(i).U & coefficient).xorR } .reverse)
  }

  // Find more great CRC polynomials (using implicit+1 notation) here: https://users.ece.cmu.edu/~koopman/crc/
  val CRC_16F_4_2 = BigInt(0x1a2eb) // HD=4 for <32751 bits and HD=6 for <93 bits, 0xd175 in implicit+1 notation
}
