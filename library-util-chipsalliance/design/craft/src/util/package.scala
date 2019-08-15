// See LICENSE.SiFive for license details.

package freechips.rocketchip

import Chisel._
import scala.math.min
import scala.collection.{immutable, mutable}

package object util {
  implicit class UnzippableOption[S, T](val x: Option[(S, T)]) {
    def unzip = (x.map(_._1), x.map(_._2))
  }

  implicit class UIntIsOneOf(val x: UInt) extends AnyVal {
    def isOneOf(s: Seq[UInt]): Bool = s.map(x === _).reduce(_||_)
  
    def isOneOf(u1: UInt, u2: UInt*): Bool = isOneOf(u1 +: u2.toSeq)
  }

  implicit class SeqToAugmentedSeq[T <: Data](val x: Seq[T]) extends AnyVal {
    def apply(idx: UInt): T = {
      if (x.size <= 1) {
        x.head
      } else if (!isPow2(x.size)) {
        // For non-power-of-2 seqs, reflect elements to simplify decoder
        (x ++ x.takeRight(x.size & -x.size)).toSeq(idx)
      } else {
        // Ignore MSBs of idx
        val truncIdx =
          if (idx.isWidthKnown && idx.getWidth <= log2Ceil(x.size)) idx
          else (idx | UInt(0, log2Ceil(x.size)))(log2Ceil(x.size)-1, 0)
        (x.head /: x.zipWithIndex.tail) { case (prev, (cur, i)) => Mux(truncIdx === i.U, cur, prev) }
      }
    }

    def asUInt(): UInt = Cat(x.map(_.asUInt).reverse)

    def rotate(n: Int): Seq[T] = x.drop(n) ++ x.take(n)

    def rotate(n: UInt): Seq[T] = {
      require(isPow2(x.size))
      val amt = n.padTo(log2Ceil(x.size))
      (x /: (0 until log2Ceil(x.size)))((r, i) => (r.rotate(1 << i) zip r).map { case (s, a) => Mux(amt(i), s, a) })
    }

    def rotateRight(n: Int): Seq[T] = x.takeRight(n) ++ x.dropRight(n)

    def rotateRight(n: UInt): Seq[T] = {
      require(isPow2(x.size))
      val amt = n.padTo(log2Ceil(x.size))
      (x /: (0 until log2Ceil(x.size)))((r, i) => (r.rotateRight(1 << i) zip r).map { case (s, a) => Mux(amt(i), s, a) })
    }
  }

  // allow bitwise ops on Seq[Bool] just like UInt
  implicit class SeqBoolBitwiseOps(val x: Seq[Bool]) extends AnyVal {
    def & (y: Seq[Bool]): Seq[Bool] = (x zip y).map { case (a, b) => a && b }
    def | (y: Seq[Bool]): Seq[Bool] = padZip(x, y).map { case (a, b) => a || b }
    def ^ (y: Seq[Bool]): Seq[Bool] = padZip(x, y).map { case (a, b) => a ^ b }
    def << (n: Int): Seq[Bool] = Seq.fill(n)(false.B) ++ x
    def >> (n: Int): Seq[Bool] = x drop n
    def unary_~(): Seq[Bool] = x.map(!_)
    def andR: Bool = if (x.isEmpty) true.B else x.reduce(_&&_)
    def orR: Bool = if (x.isEmpty) false.B else x.reduce(_||_)
    def xorR: Bool = if (x.isEmpty) false.B else x.reduce(_^_)

    private def padZip(y: Seq[Bool], z: Seq[Bool]): Seq[(Bool, Bool)] = y.padTo(z.size, false.B) zip z.padTo(y.size, false.B)
  }

  implicit class DataToAugmentedData[T <: Data](val x: T) extends AnyVal {
    def holdUnless(enable: Bool): T = Mux(enable, x, RegEnable(x, enable))
  }

  implicit class SeqMemToAugmentedSeqMem[T <: Data](val x: SeqMem[T]) extends AnyVal {
    def readAndHold(addr: UInt, enable: Bool): T = x.read(addr, enable) holdUnless RegNext(enable)
  }

  implicit class StringToAugmentedString(val x: String) extends AnyVal {
    /** converts from camel case to to underscores, also removing all spaces */
    def underscore: String = x.tail.foldLeft(x.headOption.map(_.toLower + "") getOrElse "") {
      case (acc, c) if c.isUpper => acc + "_" + c.toLower
      case (acc, c) if c == ' ' => acc
      case (acc, c) => acc + c
    }

    /** converts spaces or underscores to hyphens, also lowering case */
    def kebab: String = x.toLowerCase map {
      case ' ' => '-'
      case '_' => '-'
      case c => c
    }

    def named(name: Option[String]): String = {
      x + name.map("_named_" + _ ).getOrElse("_with_no_name")
    }

    def named(name: String): String = named(Some(name))
  }

  implicit def uintToBitPat(x: UInt): BitPat = BitPat(x)
  implicit def wcToUInt(c: WideCounter): UInt = c.value

  implicit class UIntToAugmentedUInt(val x: UInt) extends AnyVal {
    def sextTo(n: Int): UInt = {
      require(x.getWidth <= n)
      if (x.getWidth == n) x
      else Cat(Fill(n - x.getWidth, x(x.getWidth-1)), x)
    }

    def padTo(n: Int): UInt = {
      require(x.getWidth <= n)
      if (x.getWidth == n) x
      else Cat(UInt(0, n - x.getWidth), x)
    }

    // Like UInt.apply(hi, lo), but returns 0.U for zero-width extracts
    def extract(hi: Int, lo: Int): UInt = {
      require(hi >= lo-1)
      if (hi == lo-1) UInt(0)
      else x(hi, lo)
    }

    // Like Some(UInt.apply(hi, lo)), but returns None for zero-width extracts
    def extractOption(hi: Int, lo: Int): Option[UInt] = {
      require(hi >= lo-1)
      if (hi == lo-1) None
      else Some(x(hi, lo))
    }

    // like x & ~y, but first truncate or zero-extend y to x's width
    def andNot(y: UInt): UInt = x & ~(y | (x & 0.U))

    def rotateRight(n: Int): UInt = if (n == 0) x else Cat(x(n-1, 0), x >> n)

    def rotateRight(n: UInt): UInt = {
      val amt = n.padTo(log2Ceil(x.getWidth))
      (x /: (0 until log2Ceil(x.getWidth)))((r, i) => Mux(amt(i), r.rotateRight(1 << i), r))
    }

    def rotateLeft(n: Int): UInt = if (n == 0) x else Cat(x(x.getWidth-1-n,0), x(x.getWidth-1,x.getWidth-n))

    def rotateLeft(n: UInt): UInt = {
      val amt = n.padTo(log2Ceil(x.getWidth))
      (x /: (0 until log2Ceil(x.getWidth)))((r, i) => Mux(amt(i), r.rotateLeft(1 << i), r))
    }

    // compute (this + y) % n, given (this < n) and (y < n)
    def addWrap(y: UInt, n: Int): UInt = {
      val z = x +& y
      if (isPow2(n)) z(n.log2-1, 0) else Mux(z >= n.U, z - n.U, z)(log2Ceil(n)-1, 0)
    }

    // compute (this - y) % n, given (this < n) and (y < n)
    def subWrap(y: UInt, n: Int): UInt = {
      val z = x -& y
      if (isPow2(n)) z(n.log2-1, 0) else Mux(z(z.getWidth-1), z + n.U, z)(log2Ceil(n)-1, 0)
    }

    def grouped(width: Int): Seq[UInt] =
      (0 until x.getWidth by width).map(base => x(base + width - 1, base))

    def inRange(base: UInt, bounds: UInt) = x >= base && x < bounds

    def ## (y: Option[UInt]): UInt = y.map(x ## _).getOrElse(x)
  }

  implicit class OptionUIntToAugmentedOptionUInt(val x: Option[UInt]) extends AnyVal {
    def ## (y: UInt): UInt = x.map(_ ## y).getOrElse(y)
    def ## (y: Option[UInt]): Option[UInt] = x.map(_ ## y)
  }

  implicit class BooleanToAugmentedBoolean(val x: Boolean) extends AnyVal {
    def toInt: Int = if (x) 1 else 0

    // this one's snagged from scalaz
    def option[T](z: => T): Option[T] = if (x) Some(z) else None
  }

  implicit class IntToAugmentedInt(val x: Int) extends AnyVal {
    // exact log2
    def log2: Int = {
      require(isPow2(x))
      log2Ceil(x)
    }
  }

  def OH1ToOH(x: UInt): UInt = (x << 1 | UInt(1)) & ~Cat(UInt(0, width=1), x)
  def OH1ToUInt(x: UInt): UInt = OHToUInt(OH1ToOH(x))
  def UIntToOH1(x: UInt, width: Int): UInt = ~(SInt(-1, width=width).asUInt << x)(width-1, 0)

  def trailingZeros(x: Int): Option[Int] = if (x > 0) Some(log2Ceil(x & -x)) else None

  // Fill 1s from low bits to high bits
  def leftOR(x: UInt): UInt = leftOR(x, x.getWidth, x.getWidth)
  def leftOR(x: UInt, width: Integer, cap: Integer = 999999): UInt = {
    val stop = min(width, cap)
    def helper(s: Int, x: UInt): UInt =
      if (s >= stop) x else helper(s+s, x | (x << s)(width-1,0))
    helper(1, x)(width-1, 0)
  }

  // Fill 1s form high bits to low bits
  def rightOR(x: UInt): UInt = rightOR(x, x.getWidth, x.getWidth)
  def rightOR(x: UInt, width: Integer, cap: Integer = 999999): UInt = {
    val stop = min(width, cap)
    def helper(s: Int, x: UInt): UInt =
      if (s >= stop) x else helper(s+s, x | (x >> s))
    helper(1, x)(width-1, 0)
  }

  def OptimizationBarrier[T <: Data](in: T): T = {
    val foo = Module(new Module {
      val io = IO(new Bundle {
        val x = Input(in)
        val y = Output(in)
      })
      io.y := io.x
    })
    foo.io.x := in
    foo.io.y
  }

  /** Similar to Seq.groupBy except this returns a Seq instead of a Map
    * Useful for deterministic code generation
    */
  def groupByIntoSeq[A, K](xs: Seq[A])(f: A => K): immutable.Seq[(K, immutable.Seq[A])] = {
    val map = mutable.LinkedHashMap.empty[K, mutable.ListBuffer[A]]
    for (x <- xs) {
      val key = f(x)
      val l = map.getOrElseUpdate(key, mutable.ListBuffer.empty[A])
      l += x
    }
    map.view.map({ case (k, vs) => k -> vs.toList }).toList
  }
}
