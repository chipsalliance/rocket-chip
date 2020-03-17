// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import scala.language.dynamics

class Location[T](val name: String)

class LocationMap[T] extends Dynamic {
  private val locationMap = scala.collection.mutable.Map.empty[String, T]

  def lift(location: Location[T]): Option[T] = lift(location.name)
  def lift(name: String): Option[T] = locationMap.lift(name)

  def select(location: Location[T]): T = selectDynamic(location.name)
  def selectDynamic(name: String): T = {
    locationMap.getOrElse(name, throw new Exception(s"could not find location named $name"))
  }

  def update(location: Location[T])(value: T): Unit = updateDynamic(location.name)(value)
  def updateDynamic(name: String)(value: T): Unit = {
    locationMap += name -> value
  }
}
