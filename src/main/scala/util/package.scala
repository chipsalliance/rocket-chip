// See LICENSE.SiFive for license details.

package freechips.rocketchip

import Chisel._
import scala.math.min

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
      if (x.size == 1) {
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
  }

  implicit class DataToAugmentedData[T <: Data](val x: T) extends AnyVal {
    def holdUnless(enable: Bool): T = Mux(enable, x, RegEnable(x, enable))
  }

  implicit class SeqMemToAugmentedSeqMem[T <: Data](val x: SeqMem[T]) extends AnyVal {
    def readAndHold(addr: UInt, enable: Bool): T = x.read(addr, enable) holdUnless RegNext(enable)
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

    def extract(hi: Int, lo: Int): UInt = {
      if (hi == lo-1) UInt(0)
      else x(hi, lo)
    }

    def grouped(width: Int): Seq[UInt] =
      (0 until x.getWidth by width).map(base => x(base + width - 1, base))

    def inRange(base: UInt, bounds: UInt) = x >= base && x < bounds
  }

  implicit class BooleanToAugmentedBoolean(val x: Boolean) extends AnyVal {
    def toInt: Int = if (x) 1 else 0

    // this one's snagged from scalaz
    def option[T](z: => T): Option[T] = if (x) Some(z) else None
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
}
