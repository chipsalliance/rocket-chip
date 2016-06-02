// See LICENSE for license details.

package uncore

import Chisel._
import Chisel.ImplicitConversions._

import junctions._
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

case class PLICConfig(nHartsIn: Int, supervisor: Boolean, nDevices: Int, nPriorities: Int) {
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
  def maxHarts = 992
  def pendingBase = 0x800
  def enableBase = 0x1000
  def enableOffset(i: Int) = i * ((maxDevices+7)/8)
  def hartBase = enableBase + enableOffset(maxHarts)
  def hartOffset(i: Int) = i * 0x1000
  def claimOffset = 2

  require(nDevices > 0 && nDevices <= maxDevices)
  require(nHarts > 0 && nHarts <= maxHarts)
  require(nPriorities >= 0 && nPriorities <= nDevices)
}

/** Platform-Level Interrupt Controller */
class PLIC(val cfg: PLICConfig)(implicit val p: Parameters) extends Module
    with HasTileLinkParameters
    with HasAddrMapParameters {
  val io = new Bundle {
    val devices = Vec(cfg.nDevices, new GatewayPLICIO).flip
    val harts = Vec(cfg.nHarts, Bool()).asOutput
    val tl = new ClientUncachedTileLinkIO().flip
  }

  val priority =
    if (cfg.nPriorities > 0) Reg(Vec(cfg.nDevices+1, UInt(width=log2Up(cfg.nPriorities+1))))
    else Wire(init=Vec.fill(cfg.nDevices+1)(UInt(1)))
  val pending = Reg(init=Vec.fill(cfg.nDevices+1){Bool(false)})
  val enables = Reg(Vec(cfg.nHarts, UInt(width = cfg.nDevices+1)))
  val threshold = Reg(Vec(cfg.nHarts, UInt(width = log2Up(cfg.nPriorities+1))))

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
      (Mux(useLeft, lMax._1, rMax._1), Mux(useLeft, lMax._2, UInt(half) + rMax._2))
    } else (x.head, UInt(0))
  }

  val maxDevs = Wire(Vec(cfg.nHarts, UInt(width = log2Up(pending.size))))
  for (hart <- 0 until cfg.nHarts) {
    val effectivePriority =
      for (((p, en), pri) <- (pending zip enables(hart).toBools zip priority).tail)
        yield Cat(p && en, pri)
    val (_, maxDev) = findMax(Cat(UInt(1), threshold(hart)) +: effectivePriority)

    maxDevs(hart) := Reg(next = maxDev)
    io.harts(hart) := maxDevs(hart) =/= 0
  }

  val acq = Queue(io.tl.acquire, 1)
  val read = acq.fire() && acq.bits.isBuiltInType(Acquire.getType)
  val write = acq.fire() && acq.bits.isBuiltInType(Acquire.putType)
  assert(!acq.fire() || read || write, "unsupported PLIC operation")
  val addr = acq.bits.full_addr()(log2Up(cfg.size)-1,0)

  val claimant =
    if (cfg.nHarts == 1) UInt(0)
    else (addr - cfg.hartBase)(log2Up(cfg.hartOffset(cfg.nHarts))-1,log2Up(cfg.hartOffset(1)))
  val hart = Wire(init = claimant)
  val myMaxDev = maxDevs(claimant) + UInt(0) // XXX FIRRTL bug w/o the + UInt(0)
  val myEnables = enables(hart) >> 1 << 1
  val rdata = Wire(init = Cat(myMaxDev, UInt(0, 16-threshold(0).getWidth), threshold(claimant)))
  val masked_wdata = (acq.bits.data & acq.bits.full_wmask()) | (rdata & ~acq.bits.full_wmask())

  when (addr >= cfg.hartBase) {
    when (read && addr(log2Ceil(cfg.claimOffset))) {
      pending(myMaxDev) := false
    }.elsewhen (write && acq.bits.wmask()(cfg.claimOffset)) {
      val dev = (acq.bits.data >> (8 * cfg.claimOffset))(log2Up(pending.size)-1,0)
      when (myEnables(dev)) { io.devices(dev-1).complete := true }
    }.elsewhen (write) {
      val thresh = acq.bits.data(log2Up(pending.size)-1,0)
      threshold(claimant) := thresh
    }
  }.elsewhen (addr >= cfg.enableBase) {
    rdata := myEnables
    if (cfg.nHarts > 1)
      hart := (addr - cfg.enableBase)(log2Up(cfg.enableOffset(cfg.nHarts))-1,log2Up(cfg.enableOffset(1)))
    require(enables.size <= tlDataBits) // TODO this can be relaxed
    when (write) { enables(hart) := masked_wdata >> 1 << 1 }
  }.elsewhen (addr >= cfg.pendingBase) {
    for (i <- 0 until pending.size by tlDataBytes) {
      val cond =
        if (tlDataBytes >= pending.size) Bool(true)
        else addr(log2Up(pending.size)-1,log2Up(tlDataBytes)) === i/tlDataBytes
      when (cond) {
        rdata := Cat(pending.slice(i, i + tlDataBytes).map(p => Cat(UInt(0, 7), p)).reverse)
        for (j <- 0 until (tlDataBytes min (pending.size - i))) {
          when (write) { pending(i+j) := masked_wdata(j * 8) }
        }
      }
    }
  }.otherwise {
    val regAddrBits = log2Up(log2Up(cfg.maxDevices+1)) - 3
    val regsPerBeat = tlDataBytes >> regAddrBits
    for (i <- 0 until priority.size by regsPerBeat) {
      val cond =
        if (regsPerBeat >= priority.size) Bool(true)
        else addr(log2Up(priority.size)+regAddrBits-1,log2Up(tlDataBytes)) === i/regsPerBeat
      when (cond) {
        rdata := Cat(priority.slice(i, i + regsPerBeat).map(p => Cat(UInt(0, 16-p.getWidth), p)).reverse)
        for (j <- 0 until (regsPerBeat min (priority.size - i))) {
          if (cfg.nPriorities > 0) when (write) { priority(i+j) := masked_wdata >> (j * (8 << regAddrBits)) }
        }
      }
    }
  }

  priority(0) := 0
  pending(0) := false

  io.tl.grant.valid := acq.valid
  acq.ready := io.tl.grant.ready
  io.tl.grant.bits := Grant(
    is_builtin_type = Bool(true),
    g_type = acq.bits.getBuiltInGrantType(),
    client_xact_id = acq.bits.client_xact_id,
    manager_xact_id = UInt(0),
    addr_beat = UInt(0),
    data = rdata)
}
