// See LICENSE for license details.

package rocket

import Chisel._
import Util._
import cde.Parameters

class TDRSelect(implicit p: Parameters) extends CoreBundle()(p) {
  val tdrmode = Bool()
  val reserved = UInt(width = xLen - 1 - log2Up(nTDR))
  val tdrindex = UInt(width = log2Up(nTDR))

  def nTDR = p(NBreakpoints)
}

class BPControl(implicit p: Parameters) extends CoreBundle()(p) {
  val tdrtype = UInt(width = 4)
  val bpamaskmax = UInt(width = 5)
  val reserved = UInt(width = xLen-28)
  val bpaction = UInt(width = 8)
  val bpmatch = UInt(width = 4)
  val m = Bool()
  val h = Bool()
  val s = Bool()
  val u = Bool()
  val r = Bool()
  val w = Bool()
  val x = Bool()

  def tdrType = 1
  def bpaMaskMax = 4
}

class BreakpointUnit(implicit p: Parameters) extends CoreModule()(p) {
  val io = new Bundle {
    val status = new MStatus().asInput
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
    var mask: UInt = bpc.bpmatch(1)
    for (i <- 1 until bpc.bpaMaskMax)
      mask = Cat(mask(i-1) && bpa(i-1), mask)

    def matches(x: UInt) = (~x | mask) === (~bpa | mask)
    when (Cat(bpc.m, bpc.h, bpc.s, bpc.u)(io.status.prv)) {
      when (matches(io.pc) && bpc.x) { io.xcpt_if := true }
      when (matches(io.ea) && bpc.r) { io.xcpt_ld := true }
      when (matches(io.ea) && bpc.w) { io.xcpt_st := true }
    }
  }
}
