// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import scala.language.dynamics
import scala.collection.mutable.Map

class Location[T](val name: String) extends Dynamic {
  def selectDynamic[A](portname: String): Location[A] = new Location[A](s"${name}_${portname}")
  def applyDynamic[A](portname: String)(args: A*): (Location[A], A) = {
    require(args.size == 1, "Location: can't support multiple things at one port yet")
    (new Location[A](s"${name}_${portname}"), args.head)
  }
  override def toString = s"Location($name)"
}

object Location {
  def apply[T](name: String): Location[T] = new Location[T](name)
}

class LocationMap[T] private (val internalMap: Map[String, T]) extends Map[Location[_], T] {
  def addOne(kv: (Location[_], T)) = { (internalMap += (kv._1.name -> kv._2)); this }
  def subtractOne(key: Location[_]) = { (internalMap -= key.name); this }
  def get(key: Location[_]) = internalMap.get(key.name)
  def iterator = internalMap.iterator.map(kv => (new Location(kv._1), kv._2))
  // TODO override def default to provide specific exception on missing location?
  // TODO figure out how to be more clever about applying sub-type casting
  //      for other the other Map trait methods
  def required[L <: T](key: Location[_]): L = internalMap(key.name).asInstanceOf[L]
  def optional[L <: T](key: Location[_]): Option[L] = internalMap.lift(key.name).map(_.asInstanceOf[L])
}

object LocationMap {
  def apply[T](lm: Map[String, T]): LocationMap[T] = new LocationMap(lm)
  def empty[T]: LocationMap[T] = new LocationMap(Map.empty[String, T])
}
