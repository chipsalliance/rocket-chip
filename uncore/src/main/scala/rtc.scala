package uncore

import Chisel._
import junctions._
import cde.{Parameters, Field}

case object RTCPeriod extends Field[Int]

class RTC(csr_MTIME: Int)(implicit p: Parameters) extends HtifModule
    with HasTileLinkParameters
    with HasAddrMapParameters {
  val io = new ClientUncachedTileLinkIO

  val addrTable = Vec.tabulate(nCores) { i =>
    UInt(addrMap(s"conf:csr$i").start + csr_MTIME * csrDataBytes, p(PAddrBits))
  }

  val rtc = Reg(init=UInt(0, csrDataBits))
  val rtc_tick = Counter(p(RTCPeriod)).inc()

  val sending = Reg(init = Bool(false))
  val send_acked = Reg(init = Vec.fill(nCores)(Bool(true)))
  val coreId = Wire(UInt(width = log2Up(nCores)))

  when (rtc_tick) {
    rtc := rtc + UInt(1)
    send_acked := Vec.fill(nCores)(Bool(false))
    sending := Bool(true)
  }

  if (nCores > 1) {
    val (send_cnt, send_done) = Counter(io.acquire.fire(), nCores)

    when (send_done) { sending := Bool(false) }

    coreId := send_cnt
  } else {
    when (io.acquire.fire()) { sending := Bool(false) }

    coreId := UInt(0)
  }

  when (io.grant.fire()) { send_acked(io.grant.bits.client_xact_id) := Bool(true) }

  val addr_full = addrTable(coreId)
  val addr_block = addr_full(p(PAddrBits) - 1, p(CacheBlockOffsetBits))
  val addr_beat = addr_full(p(CacheBlockOffsetBits) - 1, tlByteAddrBits)
  val addr_byte = addr_full(tlByteAddrBits - 1, 0)
  val wmask = Fill(csrDataBytes, UInt(1, 1)) << addr_byte

  io.acquire.valid := sending
  io.acquire.bits := Put(
    client_xact_id = coreId,
    addr_block = addr_block,
    addr_beat = addr_beat,
    wmask = wmask,
    data = Fill(tlDataBytes / csrDataBytes, rtc))
  io.grant.ready := Bool(true)

  require(tlClientXactIdBits >= log2Up(nCores))

  assert(!rtc_tick || send_acked.reduce(_ && _),
    "Not all clocks were updated for rtc tick")
}
