package rocket

import Chisel._
import Constants._
import uncore._
import Util._

abstract class Decoding
{
  def uncorrected: Bits
  def corrected: Bits
  def correctable: Bool
  def uncorrectable: Bool
  def error = correctable || uncorrectable
}

abstract class Encoding
{
  def width(w0: Int): Int
  def encode(x: Bits): Bits
  def decode(x: Bits): Decoding
}

class Parity extends Encoding
{
  def width(w0: Int) = w0+1
  def encode(x: Bits) = Cat(x.xorR, x)
  def decode(y: Bits) = new Decoding {
    def uncorrected = y(y.getWidth-2,0)
    def corrected = uncorrected
    def correctable = y.xorR
    def uncorrectable = Bool(false)
  }
}

class SEC extends Encoding
{
  def width(k: Int) = {
    val m = log2Up(k) + 1 - !isPow2(k)
    k + m + ((1 << m) < m+k+1)
  }
  def encode(x: Bits) = {
    val k = x.getWidth
    require(k > 0)
    val n = width(k)

    val y = for (i <- 1 to n) yield {
      if (isPow2(i)) {
        val r = for (j <- 1 to n; if j != i && (j & i))
          yield x(mapping(j))
        r reduce (_^_)
      } else
        x(mapping(i))
    }
    Vec(y){Bool()}.toBits
  }
  def decode(y: Bits) = new Decoding {
    val n = y.getWidth
    require(n > 0 && !isPow2(n))

    val p2 = for (i <- 0 until log2Up(n)) yield 1 << i
    val syndrome = p2 map { i =>
      val r = for (j <- 1 to n; if j & i)
        yield y(j-1)
      r reduce (_^_)
    }
    val s = Vec(syndrome){Bool()}.toBits

    private def swizzle(z: Bits) = Vec((1 to n).filter(i => !isPow2(i)).map(i => z(i-1))){Bool()}.toBits
    def uncorrected = swizzle(y)
    def corrected = swizzle(((y << 1) ^ UFixToOH(s)) >> 1)
    def correctable = s.orR
    def uncorrectable = Bool(false)
  }
  private def mapping(i: Int) = i-1-log2Up(i)
}

class SECDED extends Encoding
{
  def width(k: Int) = new SEC().width(k)+1
  def encode(x: Bits) = new Parity().encode(new SEC().encode(x))
  def decode(x: Bits) = new Decoding {
    val sec = new SEC().decode(x(x.getWidth-2,0))
    val par = new Parity().decode(x)
    def uncorrected = sec.uncorrected
    def corrected = sec.corrected
    def correctable = par.correctable
    def uncorrectable = !par.correctable && sec.correctable
  }
}

class SECDEDTest extends Component
{
  def inject(x: Bits, n: UFix) = {
    val r = LFSR16()
    val r1 = UFixToOH(r(log2Up(x.getWidth)-1,0))(x.getWidth-1,0)
    val r2 = UFixToOH(r(log2Up(x.getWidth)*2-1,log2Up(x.getWidth)))(x.getWidth-1,0)
    x ^ Mux(n < UFix(1), UFix(0), r1) ^ Mux(n < UFix(2), UFix(0), r2)
  }

  val code = new SECDED
  val k = 4
  val n = code.width(k)

  val io = new Bundle {
    val original = Bits(OUTPUT, k)
    val encoded = Bits(OUTPUT, n)
    val injected = Bits(OUTPUT, n)
    val uncorrected = Bits(OUTPUT, k)
    val corrected = Bits(OUTPUT, k)
    val correctable = Bool(OUTPUT)
    val uncorrectable = Bool(OUTPUT)
  }

  val c = Counter(Bool(true), 1 << k)
  val numErrors = Counter(c._2, 3)._1
  val e = code.encode(c._1)
  val i = inject(e, numErrors)
  val d = code.decode(i)

  io.original := c._1
  io.encoded := e
  io.injected := i
  io.uncorrected := d.uncorrected
  io.corrected := d.corrected
  io.correctable := d.correctable
  io.uncorrectable := d.uncorrectable
}
