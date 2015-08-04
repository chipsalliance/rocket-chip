// See LICENSE for license details.

package uncore

import Chisel._

abstract class Decoding
{
  def uncorrected: UInt
  def corrected: UInt
  def correctable: Bool
  def uncorrectable: Bool
  def error = correctable || uncorrectable
}

abstract class Code
{
  def width(w0: Int): Int
  def encode(x: UInt): UInt
  def decode(x: UInt): Decoding
}

class IdentityCode extends Code
{
  def width(w0: Int) = w0
  def encode(x: UInt) = x
  def decode(y: UInt) = new Decoding {
    def uncorrected = y
    def corrected = y
    def correctable = Bool(false)
    def uncorrectable = Bool(false)
  }
}

class ParityCode extends Code
{
  def width(w0: Int) = w0+1
  def encode(x: UInt) = Cat(x.xorR, x)
  def decode(y: UInt) = new Decoding {
    def uncorrected = y(y.getWidth-2,0)
    def corrected = uncorrected
    def correctable = Bool(false)
    def uncorrectable = y.xorR
  }
}

class SECCode extends Code
{
  def width(k: Int) = {
    val m = new Unsigned(k).log2 + 1
    k + m + (if((1 << m) < m+k+1) 1 else 0)
  }
  def encode(x: UInt) = {
    val k = x.getWidth
    require(k > 0)
    val n = width(k)

    val y = for (i <- 1 to n) yield {
      if (isPow2(i)) {
        val r = for (j <- 1 to n; if j != i && (j & i) != 0)
          yield x(mapping(j))
        r reduce (_^_)
      } else
        x(mapping(i))
    }
    Vec(y).toBits
  }
  def decode(y: UInt) = new Decoding {
    val n = y.getWidth
    require(n > 0 && !isPow2(n))

    val p2 = for (i <- 0 until log2Up(n)) yield 1 << i
    val syndrome = p2 map { i =>
      val r = for (j <- 1 to n; if (j & i) != 0)
        yield y(j-1)
      r reduce (_^_)
    }
    val s = Vec(syndrome).toBits

    private def swizzle(z: UInt) = Vec((1 to n).filter(i => !isPow2(i)).map(i => z(i-1))).toBits
    def uncorrected = swizzle(y)
    def corrected = swizzle(((y.toUInt << 1) ^ UIntToOH(s)) >> 1)
    def correctable = s.orR
    def uncorrectable = Bool(false)
  }
  private def mapping(i: Int) = i-1-log2Up(i)
}

class SECDEDCode extends Code
{
  private val sec = new SECCode
  private val par = new ParityCode

  def width(k: Int) = sec.width(k)+1
  def encode(x: UInt) = par.encode(sec.encode(x))
  def decode(x: UInt) = new Decoding {
    val secdec = sec.decode(x(x.getWidth-2,0))
    val pardec = par.decode(x)

    def uncorrected = secdec.uncorrected
    def corrected = secdec.corrected
    def correctable = pardec.uncorrectable
    def uncorrectable = !pardec.uncorrectable && secdec.correctable
  }
}

object ErrGen
{
  // generate a 1-bit error with approximate probability 2^-f
  def apply(width: Int, f: Int): UInt = {
    require(width > 0 && f >= 0 && log2Up(width) + f <= 16)
    UIntToOH(LFSR16()(log2Up(width)+f-1,0))(width-1,0)
  }
  def apply(x: UInt, f: Int): UInt = x ^ apply(x.getWidth, f)
}

class SECDEDTest extends Module
{
  val code = new SECDEDCode
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
  val i = e ^ Mux(numErrors < UInt(1), UInt(0), ErrGen(n, 1)) ^ Mux(numErrors < UInt(2), UInt(0), ErrGen(n, 1))
  val d = code.decode(i)

  io.original := c._1
  io.encoded := e
  io.injected := i
  io.uncorrected := d.uncorrected
  io.corrected := d.corrected
  io.correctable := d.correctable
  io.uncorrectable := d.uncorrectable
}
