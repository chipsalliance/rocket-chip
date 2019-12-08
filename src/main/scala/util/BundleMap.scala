// See LICENSE.SiFive for license details.
package freechips.rocketchip.util

import chisel3._
import chisel3.util._
import chisel3.experimental.DataMirror
import scala.collection.immutable.ListMap

case class BundleField(key: BundleKeyBase, data: Data)
{
  def getDefault = {
    val out = Wire(chiselTypeOf(data))
    key.setDefault(out)
    out
  }
  def isOutput = DataMirror.specifiedDirectionOf(data) match {
    case SpecifiedDirection.Unspecified => true
    case SpecifiedDirection.Output      => true
    case SpecifiedDirection.Input       => false
    case SpecifiedDirection.Flip        => false
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

  def apply[T <: Data](key: BundleKey[T]): T         = elements(key.name).asInstanceOf[T]
  def lift [T <: Data](key: BundleKey[T]): Option[T] = elements.lift(key.name).map(_.asInstanceOf[T])

  def apply(key: BundleKeyBase): Data         = elements(key.name)
  def lift (key: BundleKeyBase): Option[Data] = elements.lift(key.name)

  // For every key where this.output or that.input
  def assignL(that: BundleMap): Seq[BundleKeyBase] = {
    val a = this.fields.filter( _.isOutput).map(_.key)
    val b = that.fields.filter(!_.isOutput).map(_.key)
    (a ++ b).toList.distinct
  }

  // For every key where this.input or that.output
  def assignR(that: BundleMap): Seq[BundleKeyBase] = {
    val a = this.fields.filter(!_.isOutput).map(_.key)
    val b = that.fields.filter( _.isOutput).map(_.key)
    (a ++ b).toList.distinct
  }
}

// Implement :<= :=> and :<>
object FixChisel3 {
  private def descendL(x: Data, y: Data): Unit = {
    DataMirror.specifiedDirectionOf(x) match {
      case SpecifiedDirection.Unspecified => assignL(x, y)
      case SpecifiedDirection.Output      => x := y
      case SpecifiedDirection.Input       => ()
      case SpecifiedDirection.Flip        => assignR(y, x)
    }
  }

  private def descendR(x: Data, y: Data): Unit = {
    DataMirror.specifiedDirectionOf(y) match {
      case SpecifiedDirection.Unspecified => assignR(x, y)
      case SpecifiedDirection.Output      => ()
      case SpecifiedDirection.Input       => y := x
      case SpecifiedDirection.Flip        => assignL(y, x)
    }
  }

  def assignL(x: Data, y: Data): Unit = {
    (x, y) match {
      case (vx: Vec[_], vy: Vec[_]) => {
        require (vx.size == vy.size)
        (vx zip vy) foreach { case (ex, ey) => descendL(ex, ey) }
      }
      case (rx: Record, ry: Record) => {
        require (rx.elements.size == ry.elements.size)
        val keys = (rx.elements.keys ++ ry.elements.keys).toList.distinct
        require (keys.size == rx.elements.size)
        keys.foreach { key => descendL(rx.elements(key), ry.elements(key)) }
      }
      case (vx: Vec[_], DontCare) => vx.foreach { case ex => descendL(ex, DontCare) }
      case (rx: Record, DontCare) => rx.elements.foreach { case (_, dx) => descendL(dx, DontCare) }
      case _ => x := y // interpret Unspecified leaves as Output
    }
  }

  def assignR(x: Data, y: Data) = {
    (x, y) match {
      case (vx: Vec[_], vy: Vec[_]) => {
        require (vx.size == vy.size)
        (vx zip vy) foreach { case (ex, ey) => descendR(ex, ey) }
      }
      case (rx: Record, ry: Record) => {
        require (rx.elements.size == ry.elements.size)
        val keys = (rx.elements.keys ++ ry.elements.keys).toList.distinct
        require (keys.size == rx.elements.size)
        keys.foreach { key => descendR(rx.elements(key), ry.elements(key)) }
      }
      case (DontCare, vy: Vec[_]) => vy.foreach { case ey => descendR(DontCare, ey) }
      case (DontCare, ry: Record) => ry.elements.foreach { case (_, dy) => descendR(DontCare, dy) }
      case _ =>  // x :=> y is a no-op for Unspecified
    }
  }
}
