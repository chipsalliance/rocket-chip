// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import Chisel._
import chisel3.Record
import scala.collection.immutable.ListMap

final case class RecordListMap[T <: Data](eltMap: ListMap[String, T])
    extends Record with collection.Map[String, T] {

  // Required by collection.Map[String, T]
  def get(key: String) = eltMap.get(key)
  def -(key: String) = eltMap - key
  def +[V1 >: T](kv: (String, V1)) = eltMap + kv
  def iterator = eltMap.iterator

  // Required by Record
  val elements = eltMap

  override def cloneType: this.type = (new RecordListMap(eltMap.map{case (k, v) => k -> v.chiselCloneType})).asInstanceOf[this.type]

  // Map has its own hashCode/equals that we must not use
  override def hashCode: Int = super[Record].hashCode
  override def equals(that: Any): Boolean = super[Record].equals(that)
}
