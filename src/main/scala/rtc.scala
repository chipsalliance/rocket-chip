package uncore

import Chisel._
import junctions.{NASTIIO, NASTIAddrHashMap, SMIIO}

class RTC(pcr_MTIME: Int) extends Module {
  val io = new NASTIIO

  private val nCores = params(HTIFNCores)
  private val addrMap = params(NASTIAddrHashMap)

  val addrTable = Vec.tabulate(nCores) { i =>
    UInt(addrMap(s"conf:csr$i").start + pcr_MTIME * 8)
  }

  val rtc = Reg(init=UInt(0,64))
  val rtc_tick = Counter(params(RTCPeriod)).inc()

  val sending_addr = Reg(init = Bool(false))
  val sending_data = Reg(init = Bool(false))
  val send_acked = Reg(init = Vec(nCores, Bool(true)))

  when (rtc_tick) {
    rtc := rtc + UInt(1)
    send_acked := Vec(nCores, Bool(false))
    sending_addr := Bool(true)
    sending_data := Bool(true)
  }

  if (nCores > 1) {
    val (core, addr_send_done) = Counter(io.aw.fire(), nCores)
    val (_, data_send_done) = Counter(io.w.fire(), nCores)

    when (addr_send_done) { sending_addr := Bool(false) }
    when (data_send_done) { sending_data := Bool(false) }

    io.aw.bits.id := core
    io.aw.bits.addr := addrTable(core)
  } else {
    when (io.aw.fire()) { sending_addr := Bool(false) }
    when (io.w.fire()) { sending_addr := Bool(false) }

    io.aw.bits.id := UInt(0)
    io.aw.bits.addr := addrTable(0)
  }

  when (io.b.fire()) { send_acked(io.b.bits.id) := Bool(true) }

  io.aw.valid := sending_addr
  io.aw.bits.size := UInt(3) // 8 bytes
  io.aw.bits.len := UInt(0)
  io.aw.bits.burst := Bits("b01")
  io.aw.bits.lock := Bool(false)
  io.aw.bits.cache := UInt("b0000")
  io.aw.bits.prot := UInt("b000")
  io.aw.bits.qos := UInt("b0000")
  io.aw.bits.region := UInt("b0000")
  io.aw.bits.user := UInt(0)

  io.w.valid := sending_data
  io.w.bits.data := rtc
  io.w.bits.strb := Bits(0x00FF)
  io.w.bits.user := UInt(0)
  io.w.bits.last := Bool(true)

  io.b.ready := Bool(true)
  io.ar.valid := Bool(false)
  io.r.ready := Bool(false)

  assert(!rtc_tick || send_acked.toBits.andR,
    s"Not all clocks were updated for rtc tick")
}
