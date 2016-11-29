// See LICENSE.SiFive for license details.

import Chisel._

package object util {
  implicit class UIntIsOneOf(val x: UInt) extends AnyVal {
    def isOneOf(s: Seq[UInt]): Bool = s.map(x === _).reduce(_||_)
  
    def isOneOf(u1: UInt, u2: UInt*): Bool = isOneOf(u1 +: u2.toSeq)
  }

  implicit class SeqToAugmentedSeq[T <: Data](val x: Seq[T]) extends AnyVal {
    def apply(idx: UInt): T = {
      if (x.size == 1) {
        x.head
      } else {
        val half = 1 << (log2Ceil(x.size) - 1)
        val newIdx = idx & UInt(half - 1)
        Mux(idx >= UInt(half), x.drop(half)(newIdx), x.take(half)(newIdx))
      }
    }

    def asUInt(): UInt = Cat(x.map(_.asUInt).reverse)
  }

  implicit def uintToBitPat(x: UInt): BitPat = BitPat(x)
  implicit def wcToUInt(c: WideCounter): UInt = c.value

  implicit class UIntToAugmentedUInt(val x: UInt) extends AnyVal {
    def sextTo(n: Int): UInt =
      if (x.getWidth == n) x
      else Cat(Fill(n - x.getWidth, x(x.getWidth-1)), x)

    def extract(hi: Int, lo: Int): UInt = {
      if (hi == lo-1) UInt(0)
      else x(hi, lo)
    }
  }

  implicit class BooleanToAugmentedBoolean(val x: Boolean) extends AnyVal {
    def toInt: Int = if (x) 1 else 0

    // this one's snagged from scalaz
    def option[T](z: => T): Option[T] = if (x) Some(z) else None
  }

  object PopCountAtLeast {
    private def two(x: UInt): (Bool, Bool) = x.getWidth match {
      case 1 => (x.toBool, Bool(false))
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
}
