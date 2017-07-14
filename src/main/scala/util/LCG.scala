// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.util

import Chisel._

/** A 16-bit psuedo-random generator based on a linear conguential
  * generator (LCG).  The state is stored in an unitialised register.
  * When using the C++ backend, it is straigtforward to arrange a
  * random initial value for each uninitialised register, effectively
  * seeding each LCG16 instance with a different seed.
  */
class LCG16 extends Module { 
  val io = new Bundle { 
    val out = UInt(OUTPUT, 16) 
    val inc = Bool(INPUT)
  } 
  val state = Reg(UInt(width = 32))
  when (io.inc) {
    state := state * UInt(1103515245, 32) + UInt(12345, 32)
  }
  io.out := state(30, 15)
} 
 
/** An n-bit psuedo-random generator made from many instances of a
  * 16-bit LCG.  Parameter 'width' must be larger than 0.
  */
class LCG(val w: Int) extends Module {
  val io = new Bundle { 
    val out = UInt(OUTPUT, w) 
    val inc = Bool(INPUT)
  } 
  require(w > 0)
  val numLCG16s : Int = (w+15)/16
  val outs = Seq.fill(numLCG16s) { LCG16(io.inc) }
  io.out := Cat(outs)
}

object LCG16 {
  def apply(inc: Bool = Bool(true)): UInt = {
    val lcg = Module(new LCG16)
    lcg.io.inc := inc
    lcg.io.out
  }
}

object LCG {
  def apply(w: Int, inc: Bool = Bool(true)): UInt = {
    val lcg = Module(new LCG(w))
    lcg.io.inc := inc
    lcg.io.out
  }
}

