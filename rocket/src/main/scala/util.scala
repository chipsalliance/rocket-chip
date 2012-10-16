package rocket

import Chisel._

object Util
{
  implicit def intToUFix(x: Int): UFix = UFix(x)
  implicit def intToBoolean(x: Int): Boolean = if (x != 0) true else false
  implicit def booleanToInt(x: Boolean): Int = if (x) 1 else 0
}
