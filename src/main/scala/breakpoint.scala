// See LICENSE for license details.

package rocket

import Chisel._
import Util._
import cde.Parameters

class BPControl extends Bundle {
  val matchcond = UInt(width = 2)
  val m = Bool()
  val h = Bool()
  val s = Bool()
  val u = Bool()
  val r = Bool()
  val w = Bool()
  val x = Bool()
}

class BreakpointUnit(implicit p: Parameters) extends CoreModule()(p) {
  val io = new Bundle {
    val bpcontrol = Vec(p(NBreakpoints), new BPControl).asInput
    val bpaddress = Vec(p(NBreakpoints), UInt(width = vaddrBits)).asInput
    val pc = UInt(INPUT, vaddrBits)
    val ea = UInt(INPUT, vaddrBits)
    val xcpt_if = Bool(OUTPUT)
    val xcpt_ld = Bool(OUTPUT)
    val xcpt_st = Bool(OUTPUT)
  }

  io.xcpt_if := false
  io.xcpt_ld := false
  io.xcpt_st := false

  for (((bpc, bpa), i) <- io.bpcontrol zip io.bpaddress zipWithIndex) {
    var mask: UInt = bpc.matchcond(1)
    for (i <- 1 until log2Ceil(16))
      mask = Cat(mask(i-1) && bpa(i-1), mask)

    def matches(x: UInt) = (~x | mask) === (~bpa | mask)
    when (matches(io.pc) && bpc.x) { io.xcpt_if := true }
    when (matches(io.ea) && bpc.r) { io.xcpt_ld := true }
    when (matches(io.ea) && bpc.w) { io.xcpt_st := true }
  }
}
