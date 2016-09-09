// See LICENSE for license details.

package uncore.devices

import Chisel._
import Chisel.ImplicitConversions._

import junctions._
import uncore.tilelink._
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
  def maxHarts = 15872
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
      (Mux(useLeft, lMax._1, rMax._1), Mux(useLeft, lMax._2, UInt(half) + rMax._2))
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

  val acq = Queue(io.tl.acquire, 1)
  val read = acq.fire() && acq.bits.isBuiltInType(Acquire.getType)
  val write = acq.fire() && acq.bits.isBuiltInType(Acquire.putType)
  assert(!acq.fire() || read || write, "unsupported PLIC operation")
  val addr = acq.bits.full_addr()(log2Up(cfg.size)-1,0)

  val claimant =
    if (cfg.nHarts == 1) UInt(0)
    else (addr - cfg.hartBase)(log2Up(cfg.hartOffset(cfg.nHarts))-1,log2Up(cfg.hartOffset(1)))
  val hart = Wire(init = claimant)
  val myMaxDev = maxDevs(claimant)
  val myEnables = enables(hart)
  val rdata = Wire(init = UInt(0, tlDataBits))
  val masked_wdata = (acq.bits.data & acq.bits.full_wmask()) | (rdata & ~acq.bits.full_wmask())

  if (cfg.nDevices > 0) when (addr >= cfg.hartBase) {
    val word =
      if (tlDataBytes > cfg.claimOffset) UInt(0)
      else addr(log2Up(cfg.claimOffset),log2Up(tlDataBytes))
    rdata := Cat(myMaxDev, UInt(0, 8*cfg.priorityBytes-threshold(0).getWidth), threshold(claimant)) >> (word * tlDataBits)

    when (read && addr(log2Ceil(cfg.claimOffset))) {
      pending(myMaxDev) := false
    }
    when (write) {
      when (if (tlDataBytes > cfg.claimOffset) acq.bits.wmask()(cfg.claimOffset) else addr(log2Ceil(cfg.claimOffset))) {
        val dev = (acq.bits.data >> ((8 * cfg.claimOffset) % tlDataBits))(log2Up(pending.size)-1,0)
        when (myEnables(dev)) { io.devices(dev-1).complete := true }
      }.otherwise {
        if (cfg.nPriorities > 0) threshold(claimant) := acq.bits.data
      }
    }
  }.elsewhen (addr >= cfg.enableBase) {
    val enableHart =
      if (cfg.nHarts > 1) (addr - cfg.enableBase)(log2Up(cfg.enableOffset(cfg.nHarts))-1,log2Up(cfg.enableOffset(1)))
      else UInt(0)
    hart := enableHart
    val word =
      if (tlDataBits >= cfg.nHarts) UInt(0)
      else addr(log2Up((cfg.nHarts+7)/8)-1,log2Up(tlDataBytes))
    for (i <- 0 until cfg.nHarts by tlDataBits) {
      when (word === i/tlDataBits) {
        rdata := Cat(myEnables.slice(i, i + tlDataBits).reverse)
        for (j <- 0 until (tlDataBits min (myEnables.size - i))) {
          when (write) { enables(enableHart)(i+j) := masked_wdata(j) }
        }
      }
    }
  }.elsewhen (addr >= cfg.pendingBase) {
    val word =
      if (tlDataBytes >= pending.size) UInt(0)
      else addr(log2Up(pending.size)-1,log2Up(tlDataBytes))
    rdata := pending.asUInt >> (word * tlDataBits)
  }.otherwise {
    val regsPerBeat = tlDataBytes >> log2Up(cfg.priorityBytes)
    val word =
      if (regsPerBeat >= priority.size) UInt(0)
      else addr(log2Up(priority.size*cfg.priorityBytes)-1,log2Up(tlDataBytes))
    for (i <- 0 until priority.size by regsPerBeat) {
      when (word === i/regsPerBeat) {
        rdata := Cat(priority.slice(i, i + regsPerBeat).map(p => Cat(UInt(0, 8*cfg.priorityBytes-p.getWidth), p)).reverse)
        for (j <- 0 until (regsPerBeat min (priority.size - i))) {
          if (cfg.nPriorities > 0) when (write) { priority(i+j) := masked_wdata >> (j * 8 * cfg.priorityBytes) }
        }
      }
    }
  }

  priority(0) := 0
  pending(0) := false
  for (e <- enables)
    e(0) := false

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
