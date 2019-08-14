// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import Chisel._
import scala.reflect.ClassTag

/* MuxLiteral creates a lookup table from a key to a list of values.
 * Unlike MuxLookup, the table keys must be exclusive literals.
 */
object MuxLiteral
{
  def apply[T <: Data:ClassTag](index: UInt, default: T, first: (UInt, T), rest: (UInt, T)*): T = 
    apply(index, default, first :: rest.toList)
  def apply[T <: Data:ClassTag](index: UInt, default: T, cases: Seq[(UInt, T)]): T =
    MuxTable(index, default, cases.map { case (k, v) => (k.litValue, v) })
}

object MuxSeq
{
  def apply[T <: Data:ClassTag](index: UInt, default: T, first: T, rest: T*): T =
    apply(index, default, first :: rest.toList)
  def apply[T <: Data:ClassTag](index: UInt, default: T, cases: Seq[T]): T =
    MuxTable(index, default, cases.zipWithIndex.map { case (v, i) => (BigInt(i), v) })
}

object MuxTable
{
  def apply[T <: Data:ClassTag](index: UInt, default: T, first: (BigInt, T), rest: (BigInt, T)*): T =
    apply(index, default, first :: rest.toList)
  def apply[T <: Data:ClassTag](index: UInt, default: T, cases: Seq[(BigInt, T)]): T = {
    /* All keys must be >= 0 and distinct */
    cases.foreach { case (k, _) => require (k >= 0) }
    require (cases.map(_._1).distinct.size == cases.size)

    /* Filter out any cases identical to the default */
    val simple = cases.filter { case (k, v) => !default.isLit || !v.isLit || v.litValue != default.litValue }

    val maxKey = (BigInt(0) +: simple.map(_._1)).max
    val endIndex = BigInt(1) << log2Ceil(maxKey+1)

    if (simple.isEmpty) {
      default
    } else if (endIndex <= 2*simple.size) {
      /* The dense encoding case uses a Vec */
      val table = Array.fill(endIndex.toInt) { default }
      simple.foreach { case (k, v) => table(k.toInt) = v }
      Mux(index >= UInt(endIndex), default, Vec(table)(index))
    } else {
      /* The sparse encoding case uses switch */
      val out = Wire(init = default)
      simple.foldLeft(new chisel3.util.SwitchContext(index, None, Set.empty)) { case (acc, (k, v)) =>
        acc.is (UInt(k)) { out := v }
      }
      out
    }
  }
}
