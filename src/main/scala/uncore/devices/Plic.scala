// See LICENSE for license details.

package uncore.devices

import Chisel._
import Chisel.ImplicitConversions._

import junctions._
import regmapper._
import uncore.tilelink2._
import cde.Parameters

class GatewayPLICIO extends Bundle {
  val valid = Bool(OUTPUT)
  val ready = Bool(INPUT)
  val complete = Bool(INPUT)
}

class LevelGateway extends Module {
  val io = new Bundle {
    val interrupt = Bool(INPUT)
    val plic = new GatewayPLICIO
  }

  val inFlight = Reg(init=Bool(false))
  when (io.interrupt && io.plic.ready) { inFlight := true }
  when (io.plic.complete) { inFlight := false }
  io.plic.valid := io.interrupt && !inFlight
}

case class PLICConfig(nHartsIn: Int, supervisor: Boolean, nDevices: Int, nPriorities: Int, address: BigInt = 0xC000000) {
  def contextsPerHart = if (supervisor) 2 else 1
  def nHarts = contextsPerHart * nHartsIn
  def context(i: Int, mode: Char) = mode match {
    case 'M' => i * contextsPerHart
    case 'S' => require(supervisor); i * contextsPerHart + 1
  }
  def claimAddr(i: Int, mode: Char) = hartBase + hartOffset(context(i, mode)) + claimOffset
  def threshAddr(i: Int, mode: Char) = hartBase + hartOffset(context(i, mode))
  def enableAddr(i: Int, mode: Char) = enableBase + enableOffset(context(i, mode))
  def size = hartBase + hartOffset(maxHarts)

  def maxDevices = 1023
  def maxHarts = 15872
  def priorityBase = 0x0
  def pendingBase = 0x1000
  def enableBase = 0x2000
  def hartBase = 0x200000
  require(hartBase >= enableBase + enableOffset(maxHarts))

  def enableOffset(i: Int) = i * ((maxDevices+7)/8)
  def hartOffset(i: Int) = i * 0x1000
  def claimOffset = 4
  def priorityBytes = 4

  require(nDevices <= maxDevices)
  require(nHarts > 0 && nHarts <= maxHarts)
  require(nPriorities >= 0 && nPriorities <= nDevices)
}

trait HasPLICParamters {
  val params: (PLICConfig, Parameters)
  val cfg = params._1
  implicit val p = params._2
}

trait PLICBundle extends Bundle with HasPLICParamters {
  val devices = Vec(cfg.nDevices, new GatewayPLICIO).flip
  val harts = Vec(cfg.nHarts, Bool()).asOutput
}

trait PLICModule extends Module with HasRegMap with HasPLICParamters {
  val io: PLICBundle

  val priority =
    if (cfg.nPriorities > 0) Reg(Vec(cfg.nDevices+1, UInt(width=log2Up(cfg.nPriorities+1))))
    else Wire(init=Vec.fill(cfg.nDevices+1)(UInt(1)))
  val threshold =
    if (cfg.nPriorities > 0) Reg(Vec(cfg.nHarts, UInt(width = log2Up(cfg.nPriorities+1))))
    else Wire(init=Vec.fill(cfg.nHarts)(UInt(0)))
  val pending = Reg(init=Vec.fill(cfg.nDevices+1){Bool(false)})
  val enables = Reg(Vec(cfg.nHarts, Vec(cfg.nDevices+1, Bool())))

  for ((p, g) <- pending.tail zip io.devices) {
    g.ready := !p
    g.complete := false
    when (g.valid) { p := true }
  }

  def findMax(x: Seq[UInt]): (UInt, UInt) = {
    if (x.length > 1) {
      val half = 1 << (log2Ceil(x.length) - 1)
      val lMax = findMax(x take half)
      val rMax = findMax(x drop half)
      val useLeft = lMax._1 >= rMax._1
      (Mux(useLeft, lMax._1, rMax._1), Mux(useLeft, lMax._2, UInt(half) | rMax._2))
    } else (x.head, UInt(0))
  }

  val maxDevs = Wire(Vec(cfg.nHarts, UInt(width = log2Up(pending.size))))
  for (hart <- 0 until cfg.nHarts) {
    val effectivePriority =
      for (((p, en), pri) <- (pending zip enables(hart) zip priority).tail)
        yield Cat(p && en, pri)
    val (maxPri, maxDev) = findMax((UInt(1) << priority(0).getWidth) +: effectivePriority)

    maxDevs(hart) := Reg(next = maxDev)
    io.harts(hart) := Reg(next = maxPri) > Cat(UInt(1), threshold(hart))
  }

  def priorityRegField(x: UInt) = if (cfg.nPriorities > 0) RegField(32, x) else RegField.r(32, x)
  val piorityRegFields = Seq(cfg.priorityBase -> priority.map(p => priorityRegField(p)))
  val pendingRegFields = Seq(cfg.pendingBase  -> pending .map(b => RegField.r(1, b)))

  val enableRegFields = enables.zipWithIndex.map { case (e, i) =>
    cfg.enableBase + cfg.enableOffset(i) -> e.map(b => RegField(1, b))
  }

  val hartRegFields = Seq.tabulate(cfg.nHarts) { i =>
    cfg.hartBase + cfg.hartOffset(i) -> Seq(
      priorityRegField(threshold(i)),
      RegField(32,
        RegReadFn { valid =>
          when (valid) {
            pending(maxDevs(i)) := Bool(false)
            maxDevs(i) := UInt(0) // flush pipeline
          }
          (Bool(true), maxDevs(i))
        },
        RegWriteFn { (valid, data) =>
          when (valid && enables(i)(data)) {
            io.devices(data - UInt(1)).complete := Bool(true)
          }
          Bool(true)
        }
      )
    )
  }

  regmap((piorityRegFields ++ pendingRegFields ++ enableRegFields ++ hartRegFields):_*)

  priority(0) := 0
  pending(0) := false
  for (e <- enables)
    e(0) := false
}

/** Platform-Level Interrupt Controller */
class TLPLIC(c: PLICConfig)(implicit val p: Parameters)
  extends TLRegisterRouter(c.address, size = c.size, beatBytes = p(rocket.XLen)/8, undefZero = false)(
  new TLRegBundle((c, p), _)    with PLICBundle)(
  new TLRegModule((c, p), _, _) with PLICModule)
