// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import Chisel._
import chisel3.Record
import scala.collection.immutable.ListMap

final case class RecordMap[T <: Data](eltMap: ListMap[String, T])
    extends Record with collection.Seq[T] {

  // Required by Seq
  def apply(x: Int) = eltMap.values.toSeq(x)
  def length = eltMap.size
  def iterator = eltMap.values.iterator

  def apply(x: String) = eltMap.get(x)

  // Required by Record
  val elements = eltMap

  override def cloneType: this.type = (new RecordMap(eltMap.map{case (k, v) => k -> v.chiselCloneType})).asInstanceOf[this.type]

  // Map has its own hashCode/equals that we must not use
  override def hashCode: Int = super[Record].hashCode
  override def equals(that: Any): Boolean = super[Record].equals(that)
}
