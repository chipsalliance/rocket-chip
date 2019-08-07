package freechips.rocketchip.util

import Chisel._

object LFSR64
{
  def apply(increment: Bool = Bool(true)): UInt =
  {
    val wide = 64
    val lfsr = Reg(UInt(width = wide)) // random initial value based on simulation seed
    val xor = lfsr(0) ^ lfsr(1) ^ lfsr(3) ^ lfsr(4)
    when (increment) {
      lfsr := Mux(lfsr === UInt(0), UInt(1), Cat(xor, lfsr(wide-1,1)))
    }
    lfsr
  }
}
