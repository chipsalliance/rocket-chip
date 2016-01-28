package uncore

import Chisel._
import junctions._
import cde.{Parameters, Field}

case object RTCPeriod extends Field[Int]

class RTC(csr_MTIME: Int)(implicit p: Parameters) extends HtifModule
    with HasAddrMapParameters {
  val io = new NastiIO

  val addrTable = Vec.tabulate(nCores) { i =>
    UInt(addrMap(s"conf:csr$i").start + csr_MTIME * csrDataBytes)
  }

  val rtc = Reg(init=UInt(0, scrDataBits))
  val rtc_tick = Counter(p(RTCPeriod)).inc()

  val sending_addr = Reg(init = Bool(false))
  val sending_data = Reg(init = Bool(false))
  val send_acked = Reg(init = Vec.fill(nCores)(Bool(true)))
  val coreId = Wire(UInt(width = log2Up(nCores)))

  when (rtc_tick) {
    rtc := rtc + UInt(1)
    send_acked := Vec.fill(nCores)(Bool(false))
    sending_addr := Bool(true)
    sending_data := Bool(true)
  }

  if (nCores > 1) {
    val (addr_send_cnt, addr_send_done) = Counter(io.aw.fire(), nCores)
    val (_, data_send_done) = Counter(io.w.fire(), nCores)

    when (addr_send_done) { sending_addr := Bool(false) }
    when (data_send_done) { sending_data := Bool(false) }

    coreId := addr_send_cnt
  } else {
    when (io.aw.fire()) { sending_addr := Bool(false) }
    when (io.w.fire()) { sending_addr := Bool(false) }

    coreId := UInt(0)
  }

  when (io.b.fire()) { send_acked(io.b.bits.id) := Bool(true) }

  io.aw.valid := sending_addr
  io.aw.bits := NastiWriteAddressChannel(
    id = coreId,
    addr = addrTable(coreId),
    size = UInt(log2Up(csrDataBytes)))

  io.w.valid := sending_data
  io.w.bits := NastiWriteDataChannel(data = rtc)

  io.b.ready := Bool(true)
  io.ar.valid := Bool(false)
  io.r.ready := Bool(false)

  assert(!rtc_tick || send_acked.reduce(_ && _),
    "Not all clocks were updated for rtc tick")

  assert(!io.b.valid || io.b.bits.resp === UInt(0),
    "RTC received NASTI error response")
}
