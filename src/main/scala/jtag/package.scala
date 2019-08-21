// See LICENSE.jtag for license details

package freechips.rocketchip

import scala.language.implicitConversions

package object jtag {
  /** An implicit conversion to allow the instruction map for the TAP generator to be specified as
    * an Int instead of BigInt. Scala doesn't seem to want to automatically apply the built-in Int
    * to BigInt conversion when used as a Map key.
    *
    * This is limited to value types of Chain to limit application scope.
    */
  implicit def instructionIntKeyToBigInt[V <: Chain](x: (Int, V)) = (BigInt(x._1), x._2)
}
