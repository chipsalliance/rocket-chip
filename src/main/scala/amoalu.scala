// See LICENSE for license details.

package uncore
import Chisel._
import cde.{Parameters, Field}

abstract class StoreGen(typ: UInt, addr: UInt, dat: UInt) {
  val byte = typ === MT_B || typ === MT_BU
  val half = typ === MT_H || typ === MT_HU
  val word = typ === MT_W || typ === MT_WU
  
  def mask: UInt
  def data: UInt
  def wordData: UInt
}

class StoreGen64(typ: UInt, addr: UInt, dat: UInt) extends StoreGen(typ, addr, dat) {
  def mask =
    Mux(byte, Bits(  1) <<     addr(2,0),
    Mux(half, Bits(  3) << Cat(addr(2,1), Bits(0,1)),
    Mux(word, Bits( 15) << Cat(addr(2),   Bits(0,2)),
              Bits(255))))

  def data =
    Mux(byte, Fill(8, dat( 7,0)),
    Mux(half, Fill(4, dat(15,0)),
                      wordData))
  def wordData =
    Mux(word, Fill(2, dat(31,0)),
                      dat)
}

class StoreGenAligned64(typ: UInt, addr: UInt, dat: UInt) extends StoreGen64(typ, addr, dat) {
  override def data = dat
  override def wordData = dat
}

class StoreGen32(typ: UInt, addr: UInt, dat: UInt) extends StoreGen(typ, addr, dat){
  override val word = typ === MT_W

  def mask =
    Mux(byte, Bits(  1) <<     addr(2,0),
    Mux(half, Bits(  3) << Cat(addr(2,1), Bits(0,1)),
              Bits( 15)))

  def data =
    Mux(byte, Fill(4, dat( 7,0)),
    Mux(half, Fill(2, dat(15,0)),
                      wordData))

  def wordData = dat

  def size =
    Mux(byte, UInt("b000"),
    Mux(half, UInt("b001"),
              UInt("b010")))
}

class LoadGen64(typ: UInt, addr: UInt, dat: UInt, zero: Bool) {
  val t = new StoreGen64(typ, addr, dat)
  val sign = typ === MT_B || typ === MT_H || typ === MT_W || typ === MT_D

  val wordShift = Mux(addr(2), dat(63,32), dat(31,0))
  val word = Cat(Mux(t.word, Fill(32, sign && wordShift(31)), dat(63,32)), wordShift)
  val halfShift = Mux(addr(1), word(31,16), word(15,0))
  val half = Cat(Mux(t.half, Fill(48, sign && halfShift(15)), word(63,16)), halfShift)
  val byteShift = Mux(zero, UInt(0), Mux(addr(0), half(15,8), half(7,0)))
  val byte = Cat(Mux(zero || t.byte, Fill(56, sign && byteShift(7)), half(63,8)), byteShift)
}

class LoadGen32(typ: UInt, addr: UInt, dat: UInt) {
  val t = new StoreGen32(typ, addr, dat)
  val sign = typ === MT_B || typ === MT_H || typ === MT_W

  val word = dat
  val halfShift = Mux(addr(1), word(31,16), word(15,0))
  val half = Cat(Mux(t.half, Fill(16, sign && halfShift(15)), word(31,16)), halfShift)
  val byteShift = Mux(addr(0), half(15,8), half(7,0))
  val byte = Cat(Mux(t.byte, Fill(24, sign && byteShift(7)), half(31,8)), byteShift)
}

class AMOALU(rhsIsAligned: Boolean = false)(implicit p: Parameters) extends CacheModule()(p) {
  val operandBits = p(AmoAluOperandBits)
  require(operandBits == 64)
  val io = new Bundle {
    val addr = Bits(INPUT, blockOffBits)
    val cmd = Bits(INPUT, M_SZ)
    val typ = Bits(INPUT, MT_SZ)
    val lhs = Bits(INPUT, operandBits)
    val rhs = Bits(INPUT, operandBits)
    val out = Bits(OUTPUT, operandBits)
  }

  val storegen = if(rhsIsAligned) new StoreGenAligned64(io.typ, io.addr, io.rhs)
                   else new StoreGen64(io.typ, io.addr, io.rhs)
  val rhs = storegen.wordData
  
  val sgned = io.cmd === M_XA_MIN || io.cmd === M_XA_MAX
  val max = io.cmd === M_XA_MAX || io.cmd === M_XA_MAXU
  val min = io.cmd === M_XA_MIN || io.cmd === M_XA_MINU
  val word = io.typ === MT_W || io.typ === MT_WU || // Logic minimization:
               io.typ === MT_B || io.typ === MT_BU

  val mask = ~UInt(0,64) ^ (io.addr(2) << 31)
  val adder_out = (io.lhs & mask).toUInt + (rhs & mask)

  val cmp_lhs  = Mux(word && !io.addr(2), io.lhs(31), io.lhs(63))
  val cmp_rhs  = Mux(word && !io.addr(2), rhs(31), rhs(63))
  val lt_lo = io.lhs(31,0) < rhs(31,0)
  val lt_hi = io.lhs(63,32) < rhs(63,32)
  val eq_hi = io.lhs(63,32) === rhs(63,32)
  val lt = Mux(word, Mux(io.addr(2), lt_hi, lt_lo), lt_hi || eq_hi && lt_lo)
  val less = Mux(cmp_lhs === cmp_rhs, lt, Mux(sgned, cmp_lhs, cmp_rhs))

  val out = Mux(io.cmd === M_XA_ADD, adder_out,
            Mux(io.cmd === M_XA_AND, io.lhs & rhs,
            Mux(io.cmd === M_XA_OR,  io.lhs | rhs,
            Mux(io.cmd === M_XA_XOR, io.lhs ^ rhs,
            Mux(Mux(less, min, max), io.lhs,
            storegen.data)))))

  val wmask = FillInterleaved(8, storegen.mask)
  io.out := wmask & out | ~wmask & io.lhs
}
