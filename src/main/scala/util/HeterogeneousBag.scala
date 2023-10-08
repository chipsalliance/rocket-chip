// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.Record
import chisel3.reflect.DataMirror.internal.chiselTypeClone
import scala.collection.immutable.ListMap

final case class HeterogeneousBag[T <: Data](elts: Seq[T]) extends Record with collection.IndexedSeq[T] {
  def apply(x: Int) = elements(x.toString).asInstanceOf[T]
  def length = elts.length

  override def className: String = super.className
  val elements = ListMap(elts.zipWithIndex.map { case (n,i) => (i.toString, chiselTypeClone(n)) }:_*)
  // IndexedSeq has its own hashCode/equals that we must not use
  override def hashCode: Int = super[Record].hashCode
  override def equals(that: Any): Boolean = super[Record].equals(that)
}

object HeterogeneousBag
{
  def fromNode[D <: Data, E](elts: Seq[(D, E)]) = new HeterogeneousBag(elts.map(_._1.cloneType))
}
