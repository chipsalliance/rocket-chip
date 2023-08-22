// See LICENSE.SiFive for license details.
package freechips.rocketchip.util

import chisel3._
import chisel3.reflect.DataMirror

import scala.collection.immutable.SeqMap
import scala.collection.immutable.HashMap

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

sealed trait BundleFieldBase {
  def key: BundleKeyBase
  def data: Data // the field's chisel type with a direction
  def setDataDefault(x: Data): Unit

  // Overload this if there is a way to unify differently parameterized cases of a field
  // (For example, by selecting the widest width)
  def unify(that: BundleFieldBase): BundleFieldBase = {
    require (this == that, s"Attempted to unify two BundleMaps with conflicting fields: ${this} and ${that}")
    that
  }
}

abstract class BundleField[T <: Data](val key: BundleKey[T], typeT: => T, val default: T => Unit) extends BundleFieldBase {
  def data: T = typeT
  def defaultFlip(x: T): Unit = {}
  def setDataDefault(x: Data): Unit = default(x.asInstanceOf[T])
  def setDataDefaultFlip(x: Data): Unit = defaultFlip(x.asInstanceOf[T])
}

abstract class SimpleBundleField[T <: Data](key: BundleKey[T])(typeT: => T, defaultT: => T) extends BundleField(key, typeT, { x: T => x := defaultT })

object BundleField {
  /* Consider an arbiter that receives two request streams A and B and combines them to C.
   * The output stream C should have the union of all keys from A and B.
   * When a key from A and B have the same name:
   *  - it is an error if they are not equal.
   *  - the union contains only one copy.
   */
  def union(fields: Seq[BundleFieldBase]): Seq[BundleFieldBase] =
    fields.groupBy(_.key.name).map(_._2.reduce(_ unify _)).toList
  /* There is no point in carrying an extra field if the other end does not use it.
   */
  def accept(fields: Seq[BundleFieldBase], keys: Seq[BundleKeyBase]): Seq[BundleFieldBase] = {
    def hk = HashMap(keys.map(k => (k.name, k)):_*)
    fields.filter(f => hk.lift(f.key.name) == Some(f.key))
  }
}

sealed trait BundleKeyBase {
  def name: String

  def isControl: Boolean = this match {
    case _: IsControlKey => true
    case _               => false
  }

  def isData: Boolean = this match {
    case _: IsDataKey => true
    case _            => false
  }
}

/* Custom bundle fields have two broad categories:
 *  - data fields (which are per-beat/byte and should be widened by bus-width adapters)
 *  - control fields (which are per-burst and are unaffected by width adapters)
 */
sealed trait IsDataKey    extends BundleKeyBase
sealed trait IsControlKey extends BundleKeyBase

sealed class BundleKey[T <: Data](val name: String) extends BundleKeyBase
abstract class ControlKey[T <: Data](name: String) extends BundleKey[T](name) with IsControlKey
abstract class DataKey   [T <: Data](name: String) extends BundleKey[T](name) with IsDataKey

/* Signals can be further categorized in a request-response protocol:
 *  - request fields flow from master to slave
 *  - response fields flow from slave to master
 *  - echo fields flow from master to slave to master; a master must receive the same value in the response as he sent in the request
 * Generally, this categorization belongs in different BundleMaps
 */

class BundleMap(val fields: Seq[BundleFieldBase]) extends Record {
  // All fields must have distinct key.names
  require(fields.map(_.key.name).distinct.size == fields.size)

  val elements: SeqMap[String, Data] = SeqMap(fields.map { bf => bf.key.name -> chisel3.reflect.DataMirror.internal.chiselTypeClone(bf.data) } :_*)

  // A BundleMap is best viewed as a map from BundleKey to Data
  def keydata: Seq[(BundleKeyBase, Data)] = (fields zip elements) map { case (field, (_, data)) => (field.key, data) }

  def apply[T <: Data](key: BundleKey[T]): T = elements(key.name).asInstanceOf[T]
  def lift [T <: Data](key: BundleKey[T]): Option[T] = elements.lift(key.name).map(_.asInstanceOf[T])

  def apply(key: BundleKeyBase): Data         = elements(key.name)
  def lift (key: BundleKeyBase): Option[Data] = elements.lift(key.name)

  // Create a new BundleMap with only the selected Keys retained
  def subset(fn: BundleKeyBase => Boolean): BundleMap = {
    val out = Wire(BundleMap(fields.filter(x => fn(x.key))))
    out :<= this.waiveAll
    out
  }
}

object BundleMap {
  def apply(fields: Seq[BundleFieldBase] = Nil) = new BundleMap(fields)
  /** Sets the default values of all bundle map elements that are aligned w.r.t. d */
  def setAlignedDefaults[T <: Data](c: Connectable[T]): Connectable[T] = {
    DataMirror.collectAlignedDeep(c.base) { case member: BundleMap =>
      member.fields.foreach { f =>
        f.setDataDefault(member.elements(f.key.name))
      }
    }
    c
  }
  /** Sets the default values of all bundle map elements that are flipped w.r.t. d */
  def setFlippedDefaults[T <: Data](c: Connectable[T]): Connectable[T] = {
    DataMirror.collectFlippedDeep(c.base) { case member: BundleMap =>
      member.fields.foreach { f =>
        f.setDataDefault(member.elements(f.key.name))
      }
    }
    c
  }
  /** Waives all bundle map elements */
  def waive[T <: Data](c: Connectable[T]): Connectable[T] = {
    val bundleFields = DataMirror
      .collectMembers(c.base) { case member: BundleMap =>
        member.getElements
      }
      .flatten
    Connectable(c.base, bundleFields.toSet)
  }
  /** Waives all bundle map elements and sets the default values of all bundle map elements that are aligned w.r.t. d */
  def waiveAndSetAlignedDefaults[T <: Data](c: Connectable[T]): Connectable[T] = {
    setAlignedDefaults(c)
    waive(c)
  }
  /** Waives all bundle map elements and sets the default values of all bundle map elements that are flipped w.r.t. d */
  def waiveAndSetFlippedDefaults[T <: Data](c: Connectable[T]): Connectable[T] = {
    setFlippedDefaults(c)
    waive(c)
  }
}
