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
  def enabled(mstatus: MStatus) = Cat(m, h, s, u)(mstatus.prv)
}

class BP(implicit p: Parameters) extends CoreBundle()(p) {
  val control = new BPControl
  val address = UInt(width = vaddrBits)

  def mask(dummy: Int = 0) = {
    var mask: UInt = control.bpmatch(1)
    for (i <- 1 until control.bpaMaskMax)
      mask = Cat(mask(i-1) && address(i-1), mask)
    mask
  }

  def pow2AddressMatch(x: UInt) =
    (~x | mask()) === (~address | mask())
}

class BreakpointUnit(implicit p: Parameters) extends CoreModule()(p) {
  val io = new Bundle {
    val status = new MStatus().asInput
    val bp = Vec(p(NBreakpoints), new BP).asInput
    val pc = UInt(INPUT, vaddrBits)
    val ea = UInt(INPUT, vaddrBits)
    val xcpt_if = Bool(OUTPUT)
    val xcpt_ld = Bool(OUTPUT)
    val xcpt_st = Bool(OUTPUT)
  }

  io.xcpt_if := false
  io.xcpt_ld := false
  io.xcpt_st := false

  for (bp <- io.bp) {
    when (bp.control.enabled(io.status)) {
      when (bp.pow2AddressMatch(io.pc) && bp.control.x) { io.xcpt_if := true }
      when (bp.pow2AddressMatch(io.ea) && bp.control.r) { io.xcpt_ld := true }
      when (bp.pow2AddressMatch(io.ea) && bp.control.w) { io.xcpt_st := true }
    }
  }

  if (!io.bp.isEmpty) for ((bpl, bph) <- io.bp zip io.bp.tail) {
    def matches(x: UInt) = !(x < bpl.address) && x < bph.address
    when (bph.control.enabled(io.status) && bph.control.bpmatch === 1) {
      when (matches(io.pc) && bph.control.x) { io.xcpt_if := true }
      when (matches(io.ea) && bph.control.r) { io.xcpt_ld := true }
      when (matches(io.ea) && bph.control.w) { io.xcpt_st := true }
    }
  }
}
