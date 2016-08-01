package uncore

import Chisel._

package object Util {
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
}
