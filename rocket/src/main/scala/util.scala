package rocket

import Chisel._

object Util
{
  implicit def intToUFix(x: Int): UFix = UFix(x)
  implicit def intToBoolean(x: Int): Boolean = if (x != 0) true else false
  implicit def booleanToInt(x: Boolean): Int = if (x) 1 else 0
  implicit def booleanToBool(x: Boolean): Bits = Bool(x)

  implicit def wcToUFix(c: WideCounter): UFix = c.value
}

object AVec
{
  def apply[T <: Data](elts: Seq[T]): Vec[T] = Vec(elts) { elts.head.clone }
  def apply[T <: Data](elts: Vec[T]): Vec[T] = apply(elts.toSeq)
  def apply[T <: Data](elt0: T, elts: T*): Vec[T] = apply(elt0 :: elts.toList)
}

// a counter that clock gates most of its MSBs using the LSB carry-out
case class WideCounter(width: Int, inc: Bool = Bool(true))
{
  private val isWide = width >= 4
  private val smallWidth = if (isWide) log2Up(width) else width
  private val small = Reg(resetVal = UFix(0, smallWidth))
  private val nextSmall = small + UFix(1, smallWidth+1)
  when (inc) { small := nextSmall(smallWidth-1,0) }

  private val large = if (isWide) {
    val r = Reg(resetVal = UFix(0, width - smallWidth))
    when (inc && nextSmall(smallWidth)) { r := r + UFix(1) }
    r
  } else null

  val value = Cat(large, small)

  def := (x: UFix) = {
    val w = x.getWidth
    small := x(w.min(smallWidth)-1,0)
    if (isWide) large := (if (w < smallWidth) UFix(0) else x(w.min(width)-1,smallWidth))
  }
}
