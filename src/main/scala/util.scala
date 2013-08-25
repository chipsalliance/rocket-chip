package rocket

import Chisel._
import scala.math._

object Util
{
  implicit def intToUInt(x: Int): UInt = UInt(x)
  implicit def intToBoolean(x: Int): Boolean = if (x != 0) true else false
  implicit def booleanToInt(x: Boolean): Int = if (x) 1 else 0
  implicit def booleanToBool(x: Boolean): Bits = Bool(x)

  implicit def wcToUInt(c: WideCounter): UInt = c.value
}

object AVec
{
  def apply[T <: Data](elts: Seq[T]): Vec[T] = Vec(elts)
  def apply[T <: Data](elts: Vec[T]): Vec[T] = apply(elts.toSeq)
  def apply[T <: Data](elt0: T, elts: T*): Vec[T] = apply(elt0 :: elts.toList)

  def tabulate[T <: Data](n: Int)(f: Int => T): Vec[T] =
    apply((0 until n).map(i => f(i)))
  def tabulate[T <: Data](n1: Int, n2: Int)(f: (Int, Int) => T): Vec[Vec[T]] =
    tabulate(n1)(i1 => tabulate(n2)(f(i1, _)))
}

object Str
{
  def apply(s: String): Bits = {
    var i = BigInt(0)
    require(s.forall(validChar _))
    for (c <- s)
      i = (i << 8) | c
    Lit(i, s.length*8){Bits()}
  }
  def apply(x: Char): Bits = {
    require(validChar(x))
    Lit(x, 8){Bits()}
  }
  def apply(x: UInt): Bits = apply(x, 10)
  def apply(x: UInt, radix: Int): Bits = {
    val rad = UInt(radix)
    val w = x.getWidth
    require(w > 0)

    var q = x
    var s = digit(q % rad)
    for (i <- 1 until ceil(log(2)/log(radix)*w).toInt) {
      q = q / rad
      s = Cat(Mux(Bool(radix == 10) && q === UInt(0), Str(' '), digit(q % rad)), s)
    }
    s
  }
  def apply(x: SInt): Bits = apply(x, 10)
  def apply(x: SInt, radix: Int): Bits = {
    val neg = x < SInt(0)
    val abs = x.abs
    if (radix != 10) {
      Cat(Mux(neg, Str('-'), Str(' ')), Str(abs, radix))
    } else {
      val rad = UInt(radix)
      val w = abs.getWidth
      require(w > 0)

      var q = abs
      var s = digit(q % rad)
      var needSign = neg
      for (i <- 1 until ceil(log(2)/log(radix)*w).toInt) {
        q = q / rad
        val placeSpace = q === UInt(0)
        val space = Mux(needSign, Str('-'), Str(' '))
        needSign = needSign && !placeSpace
        s = Cat(Mux(placeSpace, space, digit(q % rad)), s)
      }
      Cat(Mux(needSign, Str('-'), Str(' ')), s)
    }
  }

  private def digit(d: UInt): Bits = Mux(d < UInt(10), Str('0')+d, Str(('a'-10).toChar)+d)(7,0)
  private def validChar(x: Char) = x == (x & 0xFF)
}

object Split
{
  // is there a better way to do do this?
  def apply(x: Bits, n0: Int) = {
    val w = checkWidth(x, n0)
    (x(w-1,n0), x(n0-1,0))
  }
  def apply(x: Bits, n1: Int, n0: Int) = {
    val w = checkWidth(x, n1, n0)
    (x(w-1,n1), x(n1-1,n0), x(n0-1,0))
  }
  def apply(x: Bits, n2: Int, n1: Int, n0: Int) = {
    val w = checkWidth(x, n2, n1, n0)
    (x(w-1,n2), x(n2-1,n1), x(n1-1,n0), x(n0-1,0))
  }

  private def checkWidth(x: Bits, n: Int*) = {
    val w = x.getWidth
    def decreasing(x: Seq[Int]): Boolean =
      if (x.tail.isEmpty) true
      else x.head > x.tail.head && decreasing(x.tail)
    require(decreasing(w :: n.toList))
    w
  }
}

// a counter that clock gates most of its MSBs using the LSB carry-out
case class WideCounter(width: Int, inc: Bool = Bool(true))
{
  private val isWide = width >= 4
  private val smallWidth = if (isWide) log2Up(width) else width
  private val small = Reg(init=UInt(0, smallWidth))
  private val nextSmall = small + UInt(1, smallWidth+1)
  when (inc) { small := nextSmall(smallWidth-1,0) }

  private val large = if (isWide) {
    val r = Reg(init=UInt(0, width - smallWidth))
    when (inc && nextSmall(smallWidth)) { r := r + UInt(1) }
    r
  } else null

  val value = Cat(large, small)

  def := (x: UInt) = {
    val w = x.getWidth
    small := x(w.min(smallWidth)-1,0)
    if (isWide) large := (if (w < smallWidth) UInt(0) else x(w.min(width)-1,smallWidth))
  }
}

object Random
{
  def apply(mod: Int, inc: Bool = Bool(true)): UInt = {
    if (isPow2(mod)) {
      require(mod <= 65536)
      LFSR16(inc)(log2Up(mod)-1,0).toUInt
    } else {
      val max = 1 << log2Up(mod*8)
      val rand_pow2 = apply(max, inc)

      var res = UInt(mod-1)
      for (i <- mod-1 to 1 by -1)
        res = Mux(rand_pow2 < UInt(i*max/mod), UInt(i-1), res)
      res
    }
  }
}
