// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import Chisel._
import freechips.rocketchip.config.Parameters
import scala.math._

class ParameterizedBundle(implicit p: Parameters) extends Bundle

trait Clocked extends Bundle {
  val clock = Clock()
  val reset = Bool()  
}

object DecoupledHelper {
  def apply(rvs: Bool*) = new DecoupledHelper(rvs)
}

class DecoupledHelper(val rvs: Seq[Bool]) {
  def fire(exclude: Bool, includes: Bool*) = {
    require(rvs.contains(exclude), "Excluded Bool not present in DecoupledHelper! Note that DecoupledHelper uses referential equality for exclusion! If you don't want to exclude anything, use fire()!")
    (rvs.filter(_ ne exclude) ++ includes).reduce(_ && _)
  }
  def fire() = {
    rvs.reduce(_ && _)
  }
}

object MuxT {
  def apply[T <: Data, U <: Data](cond: Bool, con: (T, U), alt: (T, U)): (T, U) =
    (Mux(cond, con._1, alt._1), Mux(cond, con._2, alt._2))

  def apply[T <: Data, U <: Data, W <: Data](cond: Bool, con: (T, U, W), alt: (T, U, W)): (T, U, W) =
    (Mux(cond, con._1, alt._1), Mux(cond, con._2, alt._2), Mux(cond, con._3, alt._3))

  def apply[T <: Data, U <: Data, W <: Data, X <: Data](cond: Bool, con: (T, U, W, X), alt: (T, U, W, X)): (T, U, W, X) =
    (Mux(cond, con._1, alt._1), Mux(cond, con._2, alt._2), Mux(cond, con._3, alt._3), Mux(cond, con._4, alt._4))
}

/** Creates a cascade of n MuxTs to search for a key value. */
object MuxTLookup {
  def apply[S <: UInt, T <: Data, U <: Data](key: S, default: (T, U), mapping: Seq[(S, (T, U))]): (T, U) = {
    var res = default
    for ((k, v) <- mapping.reverse)
      res = MuxT(k === key, v, res)
    res
  }

  def apply[S <: UInt, T <: Data, U <: Data, W <: Data](key: S, default: (T, U, W), mapping: Seq[(S, (T, U, W))]): (T, U, W) = {
    var res = default
    for ((k, v) <- mapping.reverse)
      res = MuxT(k === key, v, res)
    res
  }
}

object ValidMux {
  def apply[T <: Data](v1: ValidIO[T], v2: ValidIO[T]*): ValidIO[T] = {
    apply(v1 +: v2.toSeq)
  }
  def apply[T <: Data](valids: Seq[ValidIO[T]]): ValidIO[T] = {
    val out = Wire(Valid(valids.head.bits.cloneType))
    out.valid := valids.map(_.valid).reduce(_ || _)
    out.bits := MuxCase(valids.head.bits,
      valids.map(v => (v.valid -> v.bits)))
    out
  }
}

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
    val abs = x.abs.asUInt
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
  def apply(x: UInt, n0: Int) = {
    val w = x.getWidth
    (x.extract(w-1,n0), x.extract(n0-1,0))
  }
  def apply(x: UInt, n1: Int, n0: Int) = {
    val w = x.getWidth
    (x.extract(w-1,n1), x.extract(n1-1,n0), x.extract(n0-1,0))
  }
  def apply(x: UInt, n2: Int, n1: Int, n0: Int) = {
    val w = x.getWidth
    (x.extract(w-1,n2), x.extract(n2-1,n1), x.extract(n1-1,n0), x.extract(n0-1,0))
  }
}

object Random
{
  def apply(mod: Int, random: UInt): UInt = {
    if (isPow2(mod)) random.extract(log2Ceil(mod)-1,0)
    else PriorityEncoder(partition(apply(1 << log2Up(mod*8), random), mod))
  }
  def apply(mod: Int): UInt = apply(mod, randomizer)
  def oneHot(mod: Int, random: UInt): UInt = {
    if (isPow2(mod)) UIntToOH(random(log2Up(mod)-1,0))
    else PriorityEncoderOH(partition(apply(1 << log2Up(mod*8), random), mod)).asUInt
  }
  def oneHot(mod: Int): UInt = oneHot(mod, randomizer)

  private def randomizer = LFSR16()
  private def partition(value: UInt, slices: Int) =
    Seq.tabulate(slices)(i => value < UInt(((i + 1) << value.getWidth) / slices))
}

object Majority {
  def apply(in: Set[Bool]): Bool = {
    val n = (in.size >> 1) + 1
    val clauses = in.subsets(n).map(_.reduce(_ && _))
    clauses.reduce(_ || _)
  }

  def apply(in: Seq[Bool]): Bool = apply(in.toSet)

  def apply(in: UInt): Bool = apply(in.asBools.toSet)
}

object PopCountAtLeast {
  private def two(x: UInt): (Bool, Bool) = x.getWidth match {
    case 1 => (x.asBool, Bool(false))
    case n =>
      val half = x.getWidth / 2
      val (leftOne, leftTwo) = two(x(half - 1, 0))
      val (rightOne, rightTwo) = two(x(x.getWidth - 1, half))
      (leftOne || rightOne, leftTwo || rightTwo || (leftOne && rightOne))
  }
  def apply(x: UInt, n: Int): Bool = n match {
    case 0 => Bool(true)
    case 1 => x.orR
    case 2 => two(x)._2
    case 3 => PopCount(x) >= UInt(n)
  }
}

// This gets used everywhere, so make the smallest circuit possible ...
// Given an address and size, create a mask of beatBytes size
// eg: (0x3, 0, 4) => 0001, (0x3, 1, 4) => 0011, (0x3, 2, 4) => 1111
// groupBy applies an interleaved OR reduction; groupBy=2 take 0010 => 01
object MaskGen {
  def apply(addr_lo: UInt, lgSize: UInt, beatBytes: Int, groupBy: Int = 1): UInt = {
    require (groupBy >= 1 && beatBytes >= groupBy)
    require (isPow2(beatBytes) && isPow2(groupBy))
    val lgBytes = log2Ceil(beatBytes)
    val sizeOH = UIntToOH(lgSize | 0.U(log2Up(beatBytes).W), log2Up(beatBytes)) | UInt(groupBy*2 - 1)

    def helper(i: Int): Seq[(Bool, Bool)] = {
      if (i == 0) {
        Seq((lgSize >= UInt(lgBytes), Bool(true)))
      } else {
        val sub = helper(i-1)
        val size = sizeOH(lgBytes - i)
        val bit = addr_lo(lgBytes - i)
        val nbit = !bit
        Seq.tabulate (1 << i) { j =>
          val (sub_acc, sub_eq) = sub(j/2)
          val eq = sub_eq && (if (j % 2 == 1) bit else nbit)
          val acc = sub_acc || (size && eq)
          (acc, eq)
        }
      }
    }

    if (groupBy == beatBytes) UInt(1) else
      Cat(helper(lgBytes-log2Ceil(groupBy)).map(_._1).reverse)
  }
}
