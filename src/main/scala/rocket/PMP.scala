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

object PMP {
  def lgAlign = 2

  def apply(reg: PMPReg): PMP = {
    val pmp = Wire(new PMP()(reg.p))
    pmp := reg
    pmp.mask := pmp.computeMask
    pmp
  }
}

class PMPReg(implicit p: Parameters) extends CoreBundle()(p) {
  val cfg = new PMPConfig
  val addr = UInt(width = paddrBits - PMP.lgAlign)

  def locked = cfg.p(1)
  def addrLocked(next: PMPReg) = locked || next.locked && next.cfg.a(1)
}

class PMP(implicit p: Parameters) extends PMPReg {
  val mask = UInt(width = paddrBits)

  import PMP._
  def computeMask = Cat((0 until paddrBits - lgAlign).scanLeft(cfg.a(0))((m, i) => m && addr(i)).asUInt, UInt((BigInt(1) << lgAlign) - 1, lgAlign))
  private def comparand = addr << lgAlign

  private def pow2Match(x: UInt, lgSize: UInt, lgMaxSize: Int) = {
    def eval(a: UInt, b: UInt, m: UInt) = ((a ^ b) & ~m) === 0
    if (lgMaxSize <= lgAlign) {
      eval(x, comparand, mask)
    } else {
      // break up the circuit; the MSB part will be CSE'd
      val lsbMask = mask | ~(((BigInt(1) << lgMaxSize) - 1).U << lgSize)
      val msbMatch = eval(x >> lgMaxSize, comparand >> lgMaxSize, mask >> lgMaxSize)
      val lsbMatch = eval(x(lgMaxSize-1, 0), comparand(lgMaxSize-1, 0), lsbMask(lgMaxSize-1, 0))
      msbMatch && lsbMatch
    }
  }

  private def boundMatch(x: UInt, lsbMask: UInt, lgMaxSize: Int) = {
    if (lgMaxSize <= lgAlign) {
      x < comparand
    } else {
      // break up the circuit; the MSB part will be CSE'd
      val msbsLess = (x >> lgMaxSize) < (comparand >> lgMaxSize)
      val msbsEqual = ((x >> lgMaxSize) ^ (comparand >> lgMaxSize)) === 0
      val lsbsLess =  (x(lgMaxSize-1, 0) | lsbMask) < comparand(lgMaxSize-1, 0)
      msbsLess || (msbsEqual && lsbsLess)
    }
  }

  private def lowerBoundMatch(x: UInt, lgSize: UInt, lgMaxSize: Int) =
    !boundMatch(x, ~(((BigInt(1) << lgMaxSize) - 1).U << lgSize)(lgMaxSize-1, 0), lgMaxSize)

  private def upperBoundMatch(x: UInt, lgMaxSize: Int) =
    boundMatch(x, 0.U, lgMaxSize)

  private def rangeMatch(x: UInt, lgSize: UInt, lgMaxSize: Int, prev: PMP) =
    prev.lowerBoundMatch(x, lgSize, lgMaxSize) && upperBoundMatch(x, lgMaxSize)

  private def pow2Homogeneous(x: UInt, pgLevel: UInt) = {
    val maskHomogeneous = pgLevelMap { idxBits => mask(idxBits - 1) } (pgLevel)
    maskHomogeneous || (pgLevelMap { idxBits => ((x ^ comparand) >> idxBits) =/= 0 } (pgLevel))
  }

  private def pgLevelMap[T](f: Int => T) = (0 until pgLevels).map { i =>
    f(pgIdxBits + (pgLevels - 1 - i) * pgLevelBits)
  }

  private def rangeHomogeneous(x: UInt, pgLevel: UInt, prev: PMP) = {
    val beginsAfterLower = !(x < prev.comparand)
    val beginsAfterUpper = !(x < comparand)

    val pgMask = pgLevelMap { idxBits => ((BigInt(1) << paddrBits) - (BigInt(1) << idxBits)).U } (pgLevel)
    val endsBeforeLower = (x & pgMask) < (prev.comparand & pgMask)
    val endsBeforeUpper = (x & pgMask) < (comparand & pgMask)

    endsBeforeLower || beginsAfterUpper || (beginsAfterLower && endsBeforeUpper)
  }

  // returns whether this PMP completely contains, or contains none of, a page
  def homogeneous(x: UInt, pgLevel: UInt, prev: PMP): Bool =
    !cfg.p(0) || Mux(cfg.a(1), rangeHomogeneous(x, pgLevel, prev), pow2Homogeneous(x, pgLevel))

  // returns whether this matching PMP fully contains the access
  def aligned(x: UInt, lgSize: UInt, lgMaxSize: Int, prev: PMP): Bool = if (lgMaxSize <= lgAlign) true.B else {
    val lsbMask = ~(((BigInt(1) << lgMaxSize) - 1).U << lgSize)(lgMaxSize-1, 0)
    val straddlesLowerBound = ((x >> lgMaxSize) ^ (prev.comparand >> lgMaxSize)) === 0 && (prev.comparand(lgMaxSize-1, 0) & ~x(lgMaxSize-1, 0)) =/= 0
    val straddlesUpperBound = ((x >> lgMaxSize) ^ (comparand >> lgMaxSize)) === 0 && (comparand(lgMaxSize-1, 0) & (x(lgMaxSize-1, 0) | lsbMask)) =/= 0
    val rangeAligned = !(straddlesLowerBound || straddlesUpperBound)
    val pow2Aligned = (lsbMask & ~mask(lgMaxSize-1, 0)) === 0
    Mux(cfg.a(1), rangeAligned, pow2Aligned)
  }

  // returns whether this PMP matches at least one byte of the access
  def hit(x: UInt, lgSize: UInt, lgMaxSize: Int, prev: PMP): Bool =
    cfg.p(0) && Mux(cfg.a(1), rangeMatch(x, lgSize, lgMaxSize, prev), pow2Match(x, lgSize, lgMaxSize))
}

class PMPHomogeneityChecker(pmps: Seq[PMP])(implicit p: Parameters) {
  def apply(addr: UInt, pgLevel: UInt): Bool = {
    ((true.B, 0.U.asTypeOf(new PMP)) /: pmps) { case ((h, prev), pmp) =>
      (h && pmp.homogeneous(addr, pgLevel, prev), pmp)
    }._1
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

  val default = io.prv > PRV.S
  val pmp0 = Wire(init = 0.U.asTypeOf(new PMP))
  pmp0.cfg.r := default
  pmp0.cfg.w := default
  pmp0.cfg.x := default

  val res = (pmp0 /: (io.pmp zip (pmp0 +: io.pmp)).reverse) { case (prev, (pmp, prevPMP)) =>
    val hit = pmp.hit(io.addr, io.size, lgMaxSize, prevPMP)
    val ignore = default && !pmp.cfg.m
    val aligned = pmp.aligned(io.addr, io.size, lgMaxSize, prevPMP)
    val cur = Wire(init = pmp)
    cur.cfg.r := (aligned && pmp.cfg.r) || ignore
    cur.cfg.w := (aligned && pmp.cfg.w) || ignore
    cur.cfg.x := (aligned && pmp.cfg.x) || ignore
    Mux(hit, cur, prev)
  }

  io.r := res.cfg.r
  io.w := res.cfg.w
  io.x := res.cfg.x
}
