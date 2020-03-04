// See LICENSE.SiFive for license details.
package freechips.rocketchip.util

import chisel3._
import chisel3.util._
import chisel3.experimental.DataMirror
import scala.collection.immutable.ListMap

/* BundleMaps include IOs for every BundleField they are constructed with.
 * A given BundleField in a BundleMap is accessed by a BundleKey.
 *
 * So, for example:
 *   val myBundleMap = BundleMap(Seq(MyBundleDataField(width = 8), ...))
 *   myBundleMap(MyBundleData) := 7.U
 *
 * case object MyBundleData extends DataKey[UInt]("data") // "data" is the name of the IO used in BundleMaps
 * case class MyBundleDataField(width: Int) extends BundleField(MyBundleData) {
 *   def data = Output(UInt(width.W))
 *   def default(x: UInt) { x := 0.U }
 * }
 * OR:
 * case class MyBundleDataField(width: Int) extends SimpleBundleField(MyBundleData)(Output(UInt(width.W)), 0.U)
 */

trait BundleFieldBase {
  def key: BundleKeyBase
  def data: Data // the field's chisel type with a direction
  def setDataDefault(x: Data): Unit

  // Overload this if there is a way to unify differently parameterized cases of a field
  // (For example, by selecting the widest width)
  def unify(that: BundleFieldBase): BundleFieldBase = {
    require (this == that, s"Attempted to unify two BundleMaps with conflicting fields: ${this} and ${that}")
    this
  }
}

/* Always extends BundleField with a case class.
 * This will ensure that there is an appropriate equals() operator to detect name conflicts.
 */
abstract class BundleField[T <: Data](val key: BundleKey[T]) extends BundleFieldBase
{
  def data: T
  def default(x: T): Unit
  def setDataDefault(x: Data): Unit = default(x.asInstanceOf[T])
}

abstract class SimpleBundleField[T <: Data](key: BundleKey[T])(typeT: => T, defaultT: => T) extends BundleField[T](key)
{
  def data = typeT
  def default(x: T): Unit = { x := defaultT }
}

object BundleField {
  /* Consider an arbiter that receives two request streams A and B and combines them to C.
   * The output stream C should have the union of all keys from A and B.
   * When a key from A and B have the same name:
   *  - it is an error if they are not equal.
   *  - the union contains only one copy.
   */
  def union(fields: Seq[BundleFieldBase]): Seq[BundleFieldBase] =
    fields.groupBy(_.key.name).map(_._2.reduce(_ unify _)).toList
  def isOutput(x: Data) = DataMirror.specifiedDirectionOf(x) match {
    case SpecifiedDirection.Unspecified => true
    case SpecifiedDirection.Output      => true
    case SpecifiedDirection.Input       => false
    case SpecifiedDirection.Flip        => false
  }
}

sealed trait BundleKeyBase {
  def name: String
}

sealed class BundleKey[T <: Data](val name: String) extends BundleKeyBase

/* Custom bundle fields have two broad categories:
 *  - data fields (which are per-beat/byte and should be widened by bus-width adapters)
 *  - control fields (which are per-burst and are unaffected by width adapters)
 */
abstract        class DataKey   [T <: Data](name: String) extends BundleKey[T](name)
abstract sealed class ControlKey[T <: Data](name: String) extends BundleKey[T](name)
/* Control signals can be further categorized in a request-response protocol:
 *  - request fields flow from master to slave
 *  - response fields flow from slave to master
 *  - echo fields flow from master to slave to master; a master must receive the same value in the response as he sent in the request
 */
abstract class RequestKey [T <: Data](name: String) extends ControlKey[T](name)
abstract class ResponseKey[T <: Data](name: String) extends ControlKey[T](name)
abstract class EchoKey    [T <: Data](name: String) extends ControlKey[T](name)

// If you extend this class, you must either redefine cloneType or have a fields constructor
class BundleMap(val fields: Seq[BundleFieldBase]) extends Record with CustomBulkAssignable {
  // All fields must have distinct key.names
  require(fields.map(_.key.name).distinct.size == fields.size)

  val elements = ListMap(fields.map { bf => bf.key.name -> chisel3.experimental.DataMirror.internal.chiselTypeClone(bf.data) } :_*)
  override def cloneType: this.type = {
    try {
      this.getClass.getConstructors.head.newInstance(fields).asInstanceOf[this.type]
    } catch {
      case e: java.lang.IllegalArgumentException =>
        throw new Exception("Unable to use BundleMap.cloneType on " +
                       this.getClass + ", probably because " + this.getClass +
                       " does not have a constructor accepting BundleFields.  Consider overriding " +
                       "cloneType() on " + this.getClass, e)
    }
  }

  def apply[T <: Data](key: BundleKey[T]): T         = elements(key.name).asInstanceOf[T]
  def lift [T <: Data](key: BundleKey[T]): Option[T] = elements.lift(key.name).map(_.asInstanceOf[T])

  def apply(key: BundleKeyBase): Data         = elements(key.name)
  def lift (key: BundleKeyBase): Option[Data] = elements.lift(key.name)

  // Assign all outputs of this from either:
  //   outputs of that (if they exist)
  //   or the default value for the BundleField
  def assignL(that: CustomBulkAssignable): Unit = {
    def other = that.asInstanceOf[BundleMap]
    fields.foreach { field =>
      val value = apply(field.key)
      other.lift(field.key) match {
        case Some(x) => value :<= x
        case None    => if (BundleField.isOutput(value)) field.setDataDefault(value)
      }
    }
  }

  // Assign all inputs of that from either:
  //   inputs of this (if they exist)
  //   or the default value for the BundleField
  def assignR(that: CustomBulkAssignable): Unit = {
    def other = that.asInstanceOf[BundleMap]
    other.fields.foreach { field =>
      val value = other(field.key)
      lift(field.key) match {
        case Some(x) => x :=> value
        case None    => if (!BundleField.isOutput(value)) field.setDataDefault(value)
      }
    }
  }
}

trait CustomBulkAssignable {
  def assignL(that: CustomBulkAssignable): Unit
  def assignR(that: CustomBulkAssignable): Unit
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
      case (cx: CustomBulkAssignable, cy: CustomBulkAssignable) => cx.assignL(cy)
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
      case (cx: CustomBulkAssignable, cy: CustomBulkAssignable) => cx.assignR(cy)
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
