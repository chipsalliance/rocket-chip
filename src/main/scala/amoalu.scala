// See LICENSE for license details.

package uncore
import Chisel._
import cde.{Parameters, Field}

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

class StoreGenAligned(typ: UInt, addr: UInt, dat: UInt, maxSize: Int) extends StoreGen(typ, addr, dat, maxSize) {
  override def genData(i: Int) = dat
}

class LoadGen(typ: UInt, addr: UInt, dat: UInt, zero: Bool, maxSize: Int) {
  private val t = new StoreGen(typ, addr, dat, maxSize)
  private val signed = typ.toSInt >= SInt(0)

  private def genData(logMinSize: Int): UInt = {
    var res = dat
    for (i <- log2Up(maxSize)-1 to logMinSize by -1) {
      val pos = 8 << i
      val shifted = Mux(addr(i), res(2*pos-1,pos), res(pos-1,0))
      val doZero = Bool(i == 0) && zero
      val zeroed = Mux(doZero, UInt(0), shifted)
      res = Cat(Mux(t.size === UInt(i) || doZero, Fill(8*maxSize-pos, signed && zeroed(pos-1)), res(8*maxSize-1,pos)), zeroed)
    }
    res
  }

  def wordData = genData(2)
  def data = genData(0)
}

class AMOALU(rhsIsAligned: Boolean = false)(implicit p: Parameters) extends CacheModule()(p) {
  val operandBits = p(AmoAluOperandBits)
  require(operandBits == 32 || operandBits == 64)
  val io = new Bundle {
    val addr = Bits(INPUT, blockOffBits)
    val cmd = Bits(INPUT, M_SZ)
    val typ = Bits(INPUT, MT_SZ)
    val lhs = Bits(INPUT, operandBits)
    val rhs = Bits(INPUT, operandBits)
    val out = Bits(OUTPUT, operandBits)
  }

  val storegen =
    if(rhsIsAligned) new StoreGenAligned(io.typ, io.addr, io.rhs, operandBits/8)
    else new StoreGen(io.typ, io.addr, io.rhs, operandBits/8)
  val rhs = storegen.wordData
  
  val sgned = io.cmd === M_XA_MIN || io.cmd === M_XA_MAX
  val max = io.cmd === M_XA_MAX || io.cmd === M_XA_MAXU
  val min = io.cmd === M_XA_MIN || io.cmd === M_XA_MINU
  val word = io.typ === MT_W || io.typ === MT_WU || // Logic minimization:
               io.typ === MT_B || io.typ === MT_BU

  val adder_out =
    if (operandBits == 32) io.lhs + rhs
    else {
      val mask = ~UInt(0,64) ^ (io.addr(2) << 31)
      (io.lhs & mask).toUInt + (rhs & mask)
    }

  val less =
    if (operandBits == 32) Mux(io.lhs(31) === rhs(31), io.lhs < rhs, Mux(sgned, io.lhs(31), io.rhs(31)))
    else {
      val cmp_lhs = Mux(word && !io.addr(2), io.lhs(31), io.lhs(63))
      val cmp_rhs = Mux(word && !io.addr(2), rhs(31), rhs(63))
      val lt_lo = io.lhs(31,0) < rhs(31,0)
      val lt_hi = io.lhs(63,32) < rhs(63,32)
      val eq_hi = io.lhs(63,32) === rhs(63,32)
      val lt = Mux(word, Mux(io.addr(2), lt_hi, lt_lo), lt_hi || eq_hi && lt_lo)
      Mux(cmp_lhs === cmp_rhs, lt, Mux(sgned, cmp_lhs, cmp_rhs))
    }

  val out = Mux(io.cmd === M_XA_ADD, adder_out,
            Mux(io.cmd === M_XA_AND, io.lhs & rhs,
            Mux(io.cmd === M_XA_OR,  io.lhs | rhs,
            Mux(io.cmd === M_XA_XOR, io.lhs ^ rhs,
            Mux(Mux(less, min, max), io.lhs,
            storegen.data)))))

  val wmask = FillInterleaved(8, storegen.mask)
  io.out := wmask & out | ~wmask & io.lhs
}
