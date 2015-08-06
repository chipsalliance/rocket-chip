// See LICENSE for license details.

package rocket

import Chisel._
import uncore._
import scala.math._

object Util {
  implicit def intToUInt(x: Int): UInt = UInt(x)
  implicit def booleanToBool(x: Boolean): Bits = Bool(x)
  implicit def intSeqToUIntSeq(x: Seq[Int]): Seq[UInt] = x.map(UInt(_))
  implicit def seqToVec[T <: Data](x: Seq[T]): Vec[T] = Vec(x)
  implicit def wcToUInt(c: WideCounter): UInt = c.value
  implicit def sextToConv(x: UInt) = new AnyRef {
    def sextTo(n: Int): UInt = Cat(Fill(n - x.getWidth, x(x.getWidth-1)), x)
  }

  implicit def intToUnsigned(x: Int): Unsigned = new Unsigned(x)
  implicit def booleanToIntConv(x: Boolean) = new AnyRef {
    def toInt: Int = if (x) 1 else 0
  }
}

import Util._

object Str
{
  def apply(s: String): UInt = {
    var i = BigInt(0)
    require(s.forall(validChar _))
    for (c <- s)
      i = (i << 8) | c
    UInt(i, s.length*8)
  }
  def apply(x: Char): UInt = {
    require(validChar(x))
    UInt(x.toInt, 8)
  }
  def apply(x: UInt): UInt = apply(x, 10)
  def apply(x: UInt, radix: Int): UInt = {
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
  def apply(x: SInt): UInt = apply(x, 10)
  def apply(x: SInt, radix: Int): UInt = {
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

  private def digit(d: UInt): UInt = Mux(d < UInt(10), Str('0')+d, Str(('a'-10).toChar)+d)(7,0)
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
      else x.head >= x.tail.head && decreasing(x.tail)
    require(decreasing(w :: n.toList))
    w
  }
}

// a counter that clock gates most of its MSBs using the LSB carry-out
case class WideCounter(width: Int, inc: UInt = UInt(1))
{
  require(inc.getWidth > 0)
  private val isWide = width > 2*inc.getWidth
  private val smallWidth = if (isWide) inc.getWidth max log2Up(width) else width
  private val small = Reg(init=UInt(0, smallWidth))
  private val doInc = inc.orR
  private val nextSmall =
    if (inc.getWidth == 1) small + UInt(1, smallWidth+1)
    else Cat(UInt(0,1), small) + inc
  when (doInc) { small := nextSmall(smallWidth-1,0) }

  private val large = if (isWide) {
    val r = Reg(init=UInt(0, width - smallWidth))
    when (doInc && nextSmall(smallWidth)) { r := r + UInt(1) }
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
  def apply(mod: Int, random: UInt): UInt = {
    if (isPow2(mod)) random(log2Up(mod)-1,0)
    else PriorityEncoder(partition(apply(1 << log2Up(mod*8), random), mod))
  }
  def apply(mod: Int): UInt = apply(mod, randomizer)
  def oneHot(mod: Int, random: UInt): UInt = {
    if (isPow2(mod)) UIntToOH(random(log2Up(mod)-1,0))
    else PriorityEncoderOH(partition(apply(1 << log2Up(mod*8), random), mod)).toBits
  }
  def oneHot(mod: Int): UInt = oneHot(mod, randomizer)

  private def randomizer = LFSR16()
  private def round(x: Double): Int =
    if (x.toInt.toDouble == x) x.toInt else (x.toInt + 1) & -2
  private def partition(value: UInt, slices: Int) =
    Vec.tabulate(slices)(i => value < round((i << value.getWidth).toDouble / slices))
}
