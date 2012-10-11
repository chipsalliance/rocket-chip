package rocket

import Chisel._
import Node._
import scala.math._

object Util
{
  implicit def intToUFix(x: Int): UFix = UFix(x)
  implicit def intToBoolean(x: Int): Boolean = if (x != 0) true else false
  implicit def booleanToInt(x: Boolean): Int = if (x) 1 else 0
}
