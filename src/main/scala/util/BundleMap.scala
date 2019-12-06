// See LICENSE.SiFive for license details.
package freechips.rocketchip.util

import chisel3._
import chisel3.util._
import scala.collection.immutable.ListMap

case class BundleField(key: BundleKeyBase, data: Data)
{
  def getDefault = {
    val out = Wire(chiselTypeOf(data))
    key.setDefault(out)
    out
  }
}

sealed trait BundleKeyBase {
  def name: String
  def setDefault(x: Data): Unit
}

sealed class BundleKey[T <: Data](val name: String, setDefaultArg: T => Unit) extends BundleKeyBase {
  def apply(data: T) = BundleField(this, data)
  def setDefault(x: Data): Unit = setDefaultArg(x.asInstanceOf[T])
}

object BundleKey {
  def setDefaultZero(x: Data): Unit = {
    x := 0.U.asTypeOf(x)
  }
}

class    DataKey[T <: Data](name: String, setDefaultArg: T => Unit = BundleKey.setDefaultZero(_)) extends BundleKey[T](name, setDefaultArg)
class ControlKey[T <: Data](name: String, setDefaultArg: T => Unit = BundleKey.setDefaultZero(_)) extends BundleKey[T](name, setDefaultArg)

class BundleMap(val fields: Seq[BundleField]) extends Record {
  require(fields.map(_.key.name).distinct.size == fields.size)
  override def cloneType: this.type = (new BundleMap(fields)).asInstanceOf[this.type]
  val elements = ListMap(fields.map { bf => bf.key.name -> chisel3.experimental.DataMirror.internal.chiselTypeClone(bf.data) } :_*)
  def apply[T <: Data](key: BundleKey[T]) = elements(key.name).asInstanceOf[T]
}

