// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import Chisel._
import chisel3.core.Record
import scala.collection.immutable.ListMap

final case class HeterogeneousBag[T <: Data](elts: Seq[T]) extends Record with collection.IndexedSeq[T] {
  def apply(x: Int) = elts(x)
  def length = elts.length

  val elements = ListMap(elts.zipWithIndex.map { case (n,i) => (i.toString, n) }:_*)
  override def cloneType: this.type = (new HeterogeneousBag(elts.map(_.chiselCloneType))).asInstanceOf[this.type]

  // IndexedSeq has its own hashCode/equals that we must not use
  override def hashCode: Int = super[Record].hashCode
  override def equals(that: Any): Boolean = super[Record].equals(that)
}

object HeterogeneousBag
{
  def fromNode[D <: Data, E](elts: Seq[(D, E)]) = new HeterogeneousBag(elts.map(_._1.cloneType))
}
