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
  val reserved = UInt(width = xLen-24)
  val action = Bool()
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
  }

  io.xcpt_if := false
  io.xcpt_ld := false
  io.xcpt_st := false
  io.debug_if := false
  io.debug_ld := false
  io.debug_st := false

  io.bp.foldLeft((Bool(true), Bool(true), Bool(true))) { case ((ri, wi, xi), bp) =>
    val en = bp.control.enabled(io.status)
    val r = en && bp.control.r && bp.addressMatch(io.ea)
    val w = en && bp.control.w && bp.addressMatch(io.ea)
    val x = en && bp.control.x && bp.addressMatch(io.pc)
    val end = !bp.control.chain

    when (end && r && ri) { io.xcpt_ld := !bp.control.action; io.debug_ld := bp.control.action }
    when (end && w && wi) { io.xcpt_st := !bp.control.action; io.debug_st := bp.control.action }
    when (end && x && xi) { io.xcpt_if := !bp.control.action; io.debug_if := bp.control.action }

    (end || r, end || w, end || x)
  }
}
