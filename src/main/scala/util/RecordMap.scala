// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import scala.collection.immutable.ListMap
import chisel3.internal.requireIsChiselType
import chisel3.experimental.DataMirror.internal.chiselTypeClone

final class RecordMap[T <: Data](val eltMap: ListMap[String, T])
    extends Record {
  
  eltMap.foreach { case (name, elt) => requireIsChiselType(elt, name) }


  def apply(x: Int) = eltMap.values.toSeq(x)
  def apply(x: String) = eltMap.get(x)
  def size = eltMap.size
  def data = eltMap.values

  // This is needed for Record, and doesn't give the actual elements
  val elements: ListMap[String, T] = {
    val prev: Map[String, T] = eltMap.mapValues{ v => // mapValues return value is lazy
      val foo: T = chiselTypeClone(v)  
      foo
    }
    ListMap[String, T]() ++ prev
  }
  override def cloneType: this.type = (new RecordMap(eltMap)).asInstanceOf[this.type]

}

object RecordMap {

  def apply[T <: Data](eltMap: ListMap[String, T]) = new RecordMap(eltMap)

  def apply[T <: Data](elements: (String, T)*) {
    new RecordMap[T](ListMap[String, T](elements:_*))
  }
}
