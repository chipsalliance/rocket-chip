// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket

import chisel3._
import chisel3.util.{Cat}
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tile.{CoreBundle, HasCoreParameters}
import freechips.rocketchip.util._

class BPControl(implicit p: Parameters) extends CoreBundle()(p) {
  val ttype = UInt(4.W)
  val dmode = Bool()
  val maskmax = UInt(6.W)
  val reserved = UInt((xLen - (if (coreParams.useBPWatch) 26 else 24)).W)
  val action = UInt((if (coreParams.useBPWatch) 3 else 1).W)
  val chain = Bool()
  val zero = UInt(2.W)
  val tmatch = UInt(2.W)
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

class TExtra(implicit p: Parameters) extends CoreBundle()(p) {
  def mvalueBits: Int = if (xLen == 32) coreParams.mcontextWidth min  6 else coreParams.mcontextWidth min 13
  def svalueBits: Int = if (xLen == 32) coreParams.scontextWidth min 16 else coreParams.scontextWidth min 34
  def mselectPos: Int = if (xLen == 32) 25 else 50
  def mvaluePos : Int = mselectPos + 1
  def sselectPos: Int = 0
  def svaluePos : Int = 2

  val mvalue  = UInt(mvalueBits.W)
  val mselect = Bool()
  val pad2    = UInt((mselectPos - svalueBits - 2).W)
  val svalue  = UInt(svalueBits.W)
  val pad1    = UInt(1.W)
  val sselect = Bool()
}

class BP(implicit p: Parameters) extends CoreBundle()(p) {
  val control = new BPControl
  val address = UInt(vaddrBits.W)
  val textra  = new TExtra

  def contextMatch(mcontext: UInt, scontext: UInt) =
    (if (coreParams.mcontextWidth > 0) (!textra.mselect || (mcontext(textra.mvalueBits-1,0) === textra.mvalue)) else true.B) &&
    (if (coreParams.scontextWidth > 0) (!textra.sselect || (scontext(textra.svalueBits-1,0) === textra.svalue)) else true.B)

  def mask(dummy: Int = 0) =
    (0 until control.maskMax-1).scanLeft(control.tmatch(0))((m, i) => m && address(i)).asUInt

  def pow2AddressMatch(x: UInt) =
    (~x | mask()) === (~address | mask())

  def rangeAddressMatch(x: UInt) =
    (x >= address) ^ control.tmatch(0)

  def addressMatch(x: UInt) =
    Mux(control.tmatch(1), rangeAddressMatch(x), pow2AddressMatch(x))
}

class BPWatch (val n: Int) extends Bundle() {
  val valid = Vec(n, Bool())
  val rvalid = Vec(n, Bool())
  val wvalid = Vec(n, Bool())
  val ivalid = Vec(n, Bool())
  val action = UInt(3.W)
}

class BreakpointUnit(n: Int)(implicit val p: Parameters) extends Module with HasCoreParameters {
  val io = IO(new Bundle {
    val status = Input(new MStatus())
    val bp = Input(Vec(n, new BP))
    val pc = Input(UInt(vaddrBits.W))
    val ea = Input(UInt(vaddrBits.W))
    val mcontext = Input(UInt(coreParams.mcontextWidth.W))
    val scontext = Input(UInt(coreParams.scontextWidth.W))
    val xcpt_if  = Output(Bool())
    val xcpt_ld  = Output(Bool())
    val xcpt_st  = Output(Bool())
    val debug_if = Output(Bool())
    val debug_ld = Output(Bool())
    val debug_st = Output(Bool())
    val bpwatch  = Output(Vec(n, new BPWatch(1)))
  })

  io.xcpt_if := false.B
  io.xcpt_ld := false.B
  io.xcpt_st := false.B
  io.debug_if := false.B
  io.debug_ld := false.B
  io.debug_st := false.B

  (io.bpwatch zip io.bp).foldLeft((true.B, true.B, true.B)) { case ((ri, wi, xi), (bpw, bp)) =>
    val en = bp.control.enabled(io.status)
    val cx = bp.contextMatch(io.mcontext, io.scontext)
    val r = en && bp.control.r && bp.addressMatch(io.ea) && cx
    val w = en && bp.control.w && bp.addressMatch(io.ea) && cx
    val x = en && bp.control.x && bp.addressMatch(io.pc) && cx
    val end = !bp.control.chain
    val action = bp.control.action

    bpw.action := action
    bpw.valid(0) := false.B
    bpw.rvalid(0) := false.B
    bpw.wvalid(0) := false.B
    bpw.ivalid(0) := false.B

    when (end && r && ri) { io.xcpt_ld := (action === 0.U); io.debug_ld := (action === 1.U); bpw.valid(0) := true.B; bpw.rvalid(0) := true.B }
    when (end && w && wi) { io.xcpt_st := (action === 0.U); io.debug_st := (action === 1.U); bpw.valid(0) := true.B; bpw.wvalid(0) := true.B }
    when (end && x && xi) { io.xcpt_if := (action === 0.U); io.debug_if := (action === 1.U); bpw.valid(0) := true.B; bpw.ivalid(0) := true.B }

    (end || r, end || w, end || x)
  }
}
