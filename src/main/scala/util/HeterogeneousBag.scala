// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import Chisel._
import chisel3.Record
import scala.collection.immutable.ListMap

final case class HeterogeneousBag[T <: Data](elts: Seq[T], eltNames: Option[Seq[String]] = None)
    extends Record with collection.IndexedSeq[T] {

  eltNames.foreach{ n =>
    require(elts.size == n.size,
    s"If eltNames are specified, they must be specified for all elts " +
      s" (eltNames.size = ${n.size} but elts.size = ${elts.size})")
    require(n.distinct.size == n.size, "If eltNames are specified they must all be specified & unique")
  }

  private def nameFromIndex(i: Int): String = eltNames.map(_(i)).getOrElse(i.toString)

  private val nameToElts: Map[String, T] = ListMap(elts.zipWithIndex.map { case (elt, i) =>
    val name = nameFromIndex(i)
    name -> elt
  }:_*)

  def apply(x: Int) = elts(x)

  def apply(s: String) = nameToElts(s)

  def length = elts.length

  val elements = ListMap(elts.zipWithIndex.map { case (elt, i) =>
    (nameFromIndex(i), elt)
  }:_*)

  override def cloneType: this.type = (new HeterogeneousBag(elts.map(_.chiselCloneType), eltNames)).asInstanceOf[this.type]

  // IndexedSeq has its own hashCode/equals that we must not use
  override def hashCode: Int = super[Record].hashCode
  override def equals(that: Any): Boolean = super[Record].equals(that)
}

object HeterogeneousBag
{
  def fromNode[D <: Data, E](elts: Seq[(D, E)]) = new HeterogeneousBag(elts.map(_._1.cloneType))
}
