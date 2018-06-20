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
  require(operandBits == 32 || operandBits == 64)
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

  val adder_out =
    if (operandBits == 32) io.lhs + io.rhs
    else {
      val mask = ~UInt(0,64) ^ (!io.mask(3) << 31)
      (io.lhs & mask) + (io.rhs & mask)
    }

  val less = {
    val sgned = {
      val mask = M_XA_MIN ^ M_XA_MINU
      (io.cmd & mask) === (M_XA_MIN & mask)
    }

    if (operandBits == 32) {
      Mux(io.lhs(31) === io.rhs(31), io.lhs < io.rhs, Mux(sgned, io.lhs(31), io.rhs(31)))
    } else {
      val cmp_lhs = Mux(!io.mask(4), io.lhs(31), io.lhs(63))
      val cmp_rhs = Mux(!io.mask(4), io.rhs(31), io.rhs(63))
      val lt_lo = io.lhs(31,0) < io.rhs(31,0)
      val lt_hi = io.lhs(63,32) < io.rhs(63,32)
      val eq_hi = io.lhs(63,32) === io.rhs(63,32)
      val lt =
        Mux(io.mask(4) && io.mask(3), lt_hi || eq_hi && lt_lo,
        Mux(io.mask(4), lt_hi, lt_lo))
      Mux(cmp_lhs === cmp_rhs, lt, Mux(sgned, cmp_lhs, cmp_rhs))
    }
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
