// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.rocket

import Chisel._
import freechips.rocketchip.config.Parameters

class StoreGen(typ: UInt, addr: UInt, dat: UInt, maxSize: Int) {
  val size = typ(log2Up(log2Up(maxSize)+1)-1,0)
  def misaligned =
    (addr & ((UInt(1) << size) - UInt(1))(log2Up(maxSize)-1,0)).orR

  def mask = {
    var res = UInt(1)
    for (i <- 0 until log2Up(maxSize)) {
      val upper = Mux(addr(i), res, UInt(0)) | Mux(size >= UInt(i+1), UInt((BigInt(1) << (1 << i))-1), UInt(0))
      val lower = Mux(addr(i), UInt(0), res)
      res = Cat(upper, lower)
    }
    res
  }

  protected def genData(i: Int): UInt =
    if (i >= log2Up(maxSize)) dat
    else Mux(size === UInt(i), Fill(1 << (log2Up(maxSize)-i), dat((8 << i)-1,0)), genData(i+1))

  def data = genData(0)
  def wordData = genData(2)
}

class LoadGen(typ: UInt, signed: Bool, addr: UInt, dat: UInt, zero: Bool, maxSize: Int) {
  private val size = new StoreGen(typ, addr, dat, maxSize).size

  private def genData(logMinSize: Int): UInt = {
    var res = dat
    for (i <- log2Up(maxSize)-1 to logMinSize by -1) {
      val pos = 8 << i
      val shifted = Mux(addr(i), res(2*pos-1,pos), res(pos-1,0))
      val doZero = Bool(i == 0) && zero
      val zeroed = Mux(doZero, UInt(0), shifted)
      res = Cat(Mux(size === UInt(i) || doZero, Fill(8*maxSize-pos, signed && zeroed(pos-1)), res(8*maxSize-1,pos)), zeroed)
    }
    res
  }

  def wordData = genData(2)
  def data = genData(0)
}

class AMOALU(operandBits: Int)(implicit p: Parameters) extends Module {
  val minXLen = 32
  val widths = (0 to log2Ceil(operandBits / minXLen)).map(minXLen << _)

  val io = new Bundle {
    val mask = UInt(INPUT, operandBits/8)
    val cmd = Bits(INPUT, M_SZ)
    val lhs = Bits(INPUT, operandBits)
    val rhs = Bits(INPUT, operandBits)
    val out = Bits(OUTPUT, operandBits)
    val out_unmasked = Bits(OUTPUT, operandBits)
  }

  val max = io.cmd === M_XA_MAX || io.cmd === M_XA_MAXU
  val min = io.cmd === M_XA_MIN || io.cmd === M_XA_MINU
  val add = io.cmd === M_XA_ADD
  val logic_and = io.cmd === M_XA_OR || io.cmd === M_XA_AND
  val logic_xor = io.cmd === M_XA_XOR || io.cmd === M_XA_OR

  val adder_out = {
    // partition the carry chain to support sub-xLen addition
    val mask = ~(UInt(0, operandBits) +: widths.init.map(w => !io.mask(w/8-1) << (w-1))).reduce(_|_)
    (io.lhs & mask) + (io.rhs & mask)
  }

  val less = {
    // break up the comparator so the lower parts will be CSE'd
    def isLessUnsigned(x: UInt, y: UInt, n: Int): Bool = {
      if (n == minXLen) x(n-1, 0) < y(n-1, 0)
      else x(n-1, n/2) < y(n-1, n/2) || x(n-1, n/2) === y(n-1, n/2) && isLessUnsigned(x, y, n/2)
    }

    def isLess(x: UInt, y: UInt, n: Int): Bool = {
      val signed = {
        val mask = M_XA_MIN ^ M_XA_MINU
        (io.cmd & mask) === (M_XA_MIN & mask)
      }
      Mux(x(n-1) === y(n-1), isLessUnsigned(x, y, n), Mux(signed, x(n-1), y(n-1)))
    }

    PriorityMux(widths.reverse.map(w => (io.mask(w/8/2), isLess(io.lhs, io.rhs, w))))
  }

  val minmax = Mux(Mux(less, min, max), io.lhs, io.rhs)
  val logic =
    Mux(logic_and, io.lhs & io.rhs, 0.U) |
    Mux(logic_xor, io.lhs ^ io.rhs, 0.U)
  val out =
    Mux(add,                    adder_out,
    Mux(logic_and || logic_xor, logic,
                                minmax))

  val wmask = FillInterleaved(8, io.mask)
  io.out := wmask & out | ~wmask & io.lhs
  io.out_unmasked := out
}
