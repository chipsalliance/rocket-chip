// See LICENSE.SiFive for license details.

package rocket

import Chisel._
import Chisel.ImplicitConversions._
import config._
import tile._
import util._

class PMPConfig extends Bundle {
  val p = UInt(width = 2)
  val a = UInt(width = 2)
  val m = Bool()
  val x = Bool()
  val w = Bool()
  val r = Bool()
}

class PMP(implicit p: Parameters) extends CoreBundle()(p) {
  val cfg = new PMPConfig
  val addr = UInt(width = paddrBits - lgAlign)

  def enabled(prv: UInt) = cfg.p.orR && (cfg.m || prv <= PRV.S)
  def locked = cfg.p.andR
  def addrLocked(next: PMP) = locked || next.locked && next.cfg.a(1)

  private def lgAlign = 2
  private def mask = (0 until paddrBits - lgAlign).scanLeft(cfg.a(0))((m, i) => m && addr(i)).asUInt

  def pow2AddressMatch(x: UInt, lgSize: UInt, lgMaxSize: Int) = {
    val m = mask
    def checkOne(a: UInt) = (((a >> lgAlign) ^ addr) & ~m) === 0
    var res = checkOne(x)
    for (i <- (1 << lgAlign) until (1 << lgMaxSize) by (1 << lgAlign))
      res = res || (lgSize > log2Ceil(i) && checkOne(x | i))
    res
  }

  def hit(prv: UInt, x: UInt, lgSize: UInt, lgMaxSize: Int) = {
    enabled(prv) && pow2AddressMatch(x, lgSize, lgMaxSize)
  }
}

class PMPChecker(lgMaxSize: Int)(implicit p: Parameters) extends CoreModule()(p)
    with HasRocketCoreParameters {
  val io = new Bundle {
    val prv = UInt(INPUT, PRV.SZ)
    val pmp = Vec(nPMPs, new PMP).asInput
    val addr = UInt(INPUT, paddrBits)
    val size = UInt(INPUT, log2Ceil(lgMaxSize + 1))
    val r = Bool(OUTPUT)
    val w = Bool(OUTPUT)
    val x = Bool(OUTPUT)
  }

  def hits = io.pmp.map(_.hit(io.prv, io.addr, io.size, lgMaxSize))
  val default = io.prv > PRV.S
  val (r, w, x, _) = ((default, default, default, 0.U) /: (io.pmp zip hits).reverse) { case ((r, w, x, pri), (pmp, hit)) =>
    MuxT(hit && pmp.cfg.p >= pri, (pmp.cfg.r, pmp.cfg.w, pmp.cfg.x, pmp.cfg.p), (r, w, x, pri))
  }

  io.r := r
  io.w := w
  io.x := x
}
