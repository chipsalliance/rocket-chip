package uncore

import Chisel._
import junctions._
import cde.{Parameters, Field}

case object RTCPeriod extends Field[Int]

class RTC(nHarts: Int)(implicit val p: Parameters) extends Module
    with HasTileLinkParameters
    with HasAddrMapParameters {
  val io = new Bundle {
    val tl = new ClientUncachedTileLinkIO().flip
    val irqs = Vec(nHarts, Bool()).asOutput
  }

  val w = 64
  val regs = Reg(Vec(nHarts+1, UInt(width = w)))
  require(w == tlDataBits) // TODO relax this constraint for narrower TL

  val acq = Queue(io.tl.acquire, 1)

  val full_addr = acq.bits.full_addr()
  val byte_addr = full_addr(log2Up(w/8)-1,0)
  val size = w/8*(nHarts+1)
  val addr = full_addr(log2Up(size)-1,log2Up(w/8))
  val rdata = regs(addr)
  val wdata = acq.bits.data
  val read = acq.bits.a_type === Acquire.getType
  val write = acq.bits.a_type === Acquire.putType
  val wmask = acq.bits.full_wmask()
  assert(!acq.valid || read || write, "unsupported RTC operation")

  io.tl.grant.valid := acq.valid
  acq.ready := io.tl.grant.ready
  io.tl.grant.bits := Grant(
    is_builtin_type = Bool(true),
    g_type = acq.bits.getBuiltInGrantType(),
    client_xact_id = acq.bits.client_xact_id,
    manager_xact_id = UInt(0),
    addr_beat = UInt(0),
    data = rdata)

  for ((irq, cmp) <- io.irqs zip regs.tail)
    irq := (regs(0) >= cmp)

  when (Counter(p(RTCPeriod)).inc()) { regs(0) := regs(0) + UInt(1) }
  when (acq.valid && write) { regs(addr) := wdata & wmask | rdata & ~wmask }
  when (reset) { regs(0) := UInt(0) }
}
