// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket

import Chisel._
import Chisel.ImplicitConversions._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.{CoreModule, CoreBundle}
import freechips.rocketchip.util._

class BPControl(implicit p: Parameters) extends CoreBundle()(p) {
  val ttype = UInt(width = 4)
  val dmode = Bool()
  val maskmax = UInt(width = 6)
  val reserved = UInt(width = xLen - (if (coreParams.useBPWatch) 26 else 24))
  val action = UInt(width = (if (coreParams.useBPWatch) 3 else 1))
  val chain = Bool()
  val zero = UInt(width = 2)
  val tmatch = UInt(width = 2)
  val m = Bool()
  val h = Bool()
  val s = Bool()
  val u = Bool()
  val x = Bool()
  val w = Bool()
  val r = Bool()

  def tType = 2
  def maskMax = 4
  def enabled(mstatus: MStatus) = !mstatus.debug && Cat(m, h, s, u)(mstatus.prv)
}

class BP(implicit p: Parameters) extends CoreBundle()(p) {
  val control = new BPControl
  val address = UInt(width = vaddrBits)

  def mask(dummy: Int = 0) =
    (0 until control.maskMax-1).scanLeft(control.tmatch(0))((m, i) => m && address(i)).asUInt

  def pow2AddressMatch(x: UInt) =
    (~x | mask()) === (~address | mask())

  def rangeAddressMatch(x: UInt) =
    (x >= address) ^ control.tmatch(0)

  def addressMatch(x: UInt) =
    Mux(control.tmatch(1), rangeAddressMatch(x), pow2AddressMatch(x))
}

class BPWatch extends Bundle() {
  val valid = Bool()
  val action = UInt(width = 3)
}

class BreakpointUnit(n: Int)(implicit p: Parameters) extends CoreModule()(p) {
  val io = new Bundle {
    val status = new MStatus().asInput
    val bp = Vec(n, new BP).asInput
    val pc = UInt(INPUT, vaddrBits)
    val ea = UInt(INPUT, vaddrBits)
    val xcpt_if = Bool(OUTPUT)
    val xcpt_ld = Bool(OUTPUT)
    val xcpt_st = Bool(OUTPUT)
    val debug_if = Bool(OUTPUT)
    val debug_ld = Bool(OUTPUT)
    val debug_st = Bool(OUTPUT)
    val bpwatch = Vec(n, new BPWatch).asOutput
  }

  io.xcpt_if := false
  io.xcpt_ld := false
  io.xcpt_st := false
  io.debug_if := false
  io.debug_ld := false
  io.debug_st := false

  (io.bpwatch zip io.bp).foldLeft((Bool(true), Bool(true), Bool(true))) { case ((ri, wi, xi), (bpw, bp)) =>
    val en = bp.control.enabled(io.status)
    val r = en && bp.control.r && bp.addressMatch(io.ea)
    val w = en && bp.control.w && bp.addressMatch(io.ea)
    val x = en && bp.control.x && bp.addressMatch(io.pc)
    val end = !bp.control.chain
    val action = bp.control.action

    bpw.action := action
    bpw.valid := false.B

    when (end && r && ri) { io.xcpt_ld := (action === 0.U); io.debug_ld := (action === 1.U); bpw.valid := true.B }
    when (end && w && wi) { io.xcpt_st := (action === 0.U); io.debug_st := (action === 1.U); bpw.valid := true.B }
    when (end && x && xi) { io.xcpt_if := (action === 0.U); io.debug_if := (action === 1.U); bpw.valid := true.B }

    (end || r, end || w, end || x)
  }
}
