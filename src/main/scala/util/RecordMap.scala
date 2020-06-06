// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import Chisel._
import chisel3.Record
import scala.collection.immutable.ListMap
import chisel3.internal.requireIsChiselType
import chisel3.experimental.DataMirror.internal.chiselTypeClone

final class RecordMap[T <: Data](eltMap: ListMap[String, T])
    extends Record {
  
  eltMap.foreach { case (name, elt) => requireIsChiselType(elt, name) }

  val elements = ListMap() ++ eltMap.mapValues(chiselTypeClone) // mapValues return value is lazy

  override def cloneType: this.type = (new RecordMap(eltMap)).asInstanceOf[this.type]

  def apply(x: Int) = eltMap.values.toSeq(x)
  def apply(x: String) = eltMap.get(x)

}

object RecordMap {

  def apply[T <: Data](eltMap: ListMap[String, T]) = new RecordMap(eltMap)

  def apply[T <: Data](elements: (String, T)*) {
    new RecordMap[T](ListMap(elements:_*))
  }
}
