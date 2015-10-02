/// See LICENSE for license details.
package junctions
import Chisel._

object bigIntPow2 {
  def apply(in: BigInt): Boolean = in > 0 && ((in & (in-1)) == 0)
}
