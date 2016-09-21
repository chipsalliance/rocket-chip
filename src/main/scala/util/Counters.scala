package util

import Chisel._
import cde.Parameters

// Produces 0-width value when counting to 1
class ZCounter(val n: Int) {
  val value = Reg(init=UInt(0, log2Ceil(n)))
  def inc(): Bool = {
    if (n == 1) Bool(true)
    else {
      val wrap = value === UInt(n-1)
      value := Mux(Bool(!isPow2(n)) && wrap, UInt(0), value + UInt(1))
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
    val cnt = Reg(init = UInt(0, log2Up(max+1)))
    when (up && !down) { cnt := cnt + UInt(1) }
    when (down && !up) { cnt := cnt - UInt(1) }
    cnt
  }
}
