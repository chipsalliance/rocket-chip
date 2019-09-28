// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket

import Chisel._
import Chisel.ImplicitConversions._
import chisel3.core.Printable
import freechips.rocketchip.config._
import freechips.rocketchip.tile._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property._

class PMAConfig extends Bundle {
  val res = UInt(width = 3)
  val a = UInt(width = 2)
  val c = Bool()
  val w = Bool()
  val r = Bool()
}


object PMA {
  def lgAlign = 6
  def apply(reg: PMA): PMA = {
    val pma = Wire(new PMA()(reg.p))
    pma := reg
    pma.mask := pma.computeMask
    pma
  }
}

class PMA(implicit p: Parameters) extends PMReg {
  val cfg = new PMAConfig
  val addr = UInt(width = paddrBits - PMA.lgAlign)
  val lgAlign = PMA.lgAlign

  def reset() {
    cfg.a := 0
    cfg.c := 1
  }
  def readAddr = if (pmaGranularity.log2 == PMA.lgAlign) addr else {
    val mask = ((BigInt(1) << (pmaGranularity.log2 - PMA.lgAlign)) - 1).U
    Mux(napot, addr | (mask >> 1), ~(~addr | mask))
  }
  override def napot = cfg.a(1)
  override def torNotNAPOT = cfg.a(0)
  override def tor = !napot && torNotNAPOT
  override def getGranularity = pmaGranularity
  override def getLgAlign = PMA.lgAlign

  def computeMask = {
    val base = Cat(addr, cfg.a(0)) | ((pmaGranularity - 1) >> lgAlign)
    Cat(base & ~(base + 1), UInt((1 << lgAlign) - 1))
  }
  override def comparand = ~(~(addr << PMA.lgAlign) | (pmaGranularity - 1))
  def cfgCached = cfg.c
  def addrCached(next: PMA) = cfgCached || next.cfgCached && next.tor
}


class PMAChecker(lgMaxSize: Int)(implicit p: Parameters) extends CoreModule()(p)
    with HasCoreParameters {
  val io = new Bundle {
    val prv = UInt(INPUT, PRV.SZ)
    val pma = Vec(nPMAs, new PMA).asInput
    val addr = UInt(INPUT, paddrBits)
    val size = UInt(INPUT, log2Ceil(lgMaxSize + 1))
    val r = Bool(OUTPUT)
    val w = Bool(OUTPUT)
    val c = Bool(OUTPUT)
  }

  val default = true.B
  val pma0 = Wire(init = 0.U.asTypeOf(new PMA))
  pma0.cfg.r := default
  pma0.cfg.w := default
  pma0.cfg.c := default

  val res = (pma0 /: (io.pma zip (pma0 +: io.pma)).reverse) { case (prev, (pma, prevPMA)) =>
    val hit = pma.hit(io.addr, io.size, lgMaxSize, prevPMA)
    val aligned = pma.aligned(io.addr, io.size, lgMaxSize, prevPMA)

    for ((name, idx) <- Seq("no", "TOR", "NA4", "NAPOT").zipWithIndex)
      cover(pma.cfg.a === idx, s"The cfg access is set to ${name} access ", "Cover PMA access mode setting")

    cover(pma.cfg.c === 0x0, s"The cfg cacheable is set to low", "Cover PMA uncacheable setting")

    // Not including Write and no Read permission as the combination is reserved
    for ((name, idx) <- Seq("no", "RO", "", "RW", "X", "RX", "", "RWX").zipWithIndex; if name.nonEmpty)
      cover((Cat(pma.cfg.w, pma.cfg.r) === idx), s"The permission is set to ${name} access ", "Cover PMA access permission setting")

    for ((name, idx) <- Seq("", "TOR", "NA4", "NAPOT").zipWithIndex; if name.nonEmpty) {
      cover(hit && aligned && pma.cfg.a === idx, s"The access matches ${name} mode ", "Cover PMA access")
    }

    val cur = Wire(init = pma)
    cur.cfg.r := aligned && pma.cfg.r
    cur.cfg.w := aligned && pma.cfg.w
    cur.cfg.c := aligned && pma.cfg.c
    Mux(hit, cur, prev)
  }

  io.r := res.cfg.r
  io.w := res.cfg.w
  io.c := res.cfg.c
}
