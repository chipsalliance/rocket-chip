// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.util

import chisel3._
import chisel3.util._

// Produces 0-width value when counting to 1
class ZCounter(val n: Int) {
  val value = RegInit(0.U(log2Ceil(n).W))
  def inc(): Bool = {
    if (n == 1) true.B
    else {
      val wrap = value === (n-1).U
      value := Mux(!isPow2(n).B && wrap, 0.U, value + 1.U)
      wrap
    }
  }
}

object ZCounter {
  def apply(n: Int) = new ZCounter(n)
  def apply(cond: Bool, n: Int): (UInt, Bool) = {
    val c = new ZCounter(n)
    var wrap: Bool = null
    when (cond) { wrap = c.inc() }
    (c.value, cond && wrap)
  }
}

object TwoWayCounter {
  def apply(up: Bool, down: Bool, max: Int): UInt = {
    val cnt = RegInit(0.U(log2Up(max + 1).W))
    when (up && !down) { cnt := cnt + 1.U }
    when (down && !up) { cnt := cnt - 1.U }
    cnt
  }
}

// a counter that clock gates most of its MSBs using the LSB carry-out
case class WideCounter(width: Int, inc: UInt = 1.U, reset: Boolean = true, inhibit: Bool = false.B) {
  private val isWide = width > (2 * inc.getWidth)
  private val smallWidth = if (isWide) inc.getWidth max log2Up(width) else width
  private val small = if (reset) RegInit(0.U(smallWidth.W)) else Reg(UInt(smallWidth.W))
  private val nextSmall = small +& inc
  when (!inhibit) { small := nextSmall }

  private val large = if (isWide) {
    val r = if (reset) RegInit(0.U((width - smallWidth).W)) else Reg(UInt((width - smallWidth).W))
    when (nextSmall(smallWidth) && !inhibit) { r := r + 1.U }
    r
  } else null

  val value = if (isWide) Cat(large, small) else small
  lazy val carryOut = {
    val lo = (small ^ nextSmall) >> 1
    if (!isWide)
      lo
    else {
      val hi = Mux(nextSmall(smallWidth), large ^ (large +& 1.U), 0.U) >> 1
      Cat(hi, lo)
    }
  }

  def := (x: UInt) = {
    small := x
    if (isWide) large := x >> smallWidth
  }
}
