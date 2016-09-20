// See LICENSE for license details.

package rocketchip

import Chisel._
import cde.{Parameters, Field}
import rocket.Util._
import util.LatencyPipe
import junctions._
import junctions.NastiConstants._

case object BuildExampleTop extends Field[Parameters => ExampleTop]
case object SimMemLatency extends Field[Int]

class TestHarness(q: Parameters) extends Module {
  val io = new Bundle {
    val success = Bool(OUTPUT)
  }
  val dut = q(BuildExampleTop)(q).module
  implicit val p = dut.p

  // This test harness isn't especially flexible yet
  require(dut.io.mem_clk.isEmpty)
  require(dut.io.mem_rst.isEmpty)
  require(dut.io.mem_ahb.isEmpty)
  require(dut.io.mem_tl.isEmpty)
  require(dut.io.bus_clk.isEmpty)
  require(dut.io.bus_rst.isEmpty)
  require(dut.io.mmio_clk.isEmpty)
  require(dut.io.mmio_rst.isEmpty)
  require(dut.io.mmio_ahb.isEmpty)
  require(dut.io.mmio_tl.isEmpty)

  for (int <- dut.io.interrupts)
    int := false

  if (dut.io.mem_axi.nonEmpty) {
    val memSize = p(GlobalAddrMap)("mem").size
    require(memSize % dut.io.mem_axi.size == 0)
    for (axi <- dut.io.mem_axi) {
      val mem = Module(new SimAXIMem(memSize / dut.io.mem_axi.size))
      mem.io.axi.ar <> axi.ar
      mem.io.axi.aw <> axi.aw
      mem.io.axi.w  <> axi.w
      axi.r <> LatencyPipe(mem.io.axi.r, p(SimMemLatency))
      axi.b <> LatencyPipe(mem.io.axi.b, p(SimMemLatency))
    }
  }

  if (!p(IncludeJtagDTM)) {
    // Todo: enable the usage of different clocks
    // to test the synchronizer more aggressively.
    val dtm_clock = clock
    val dtm_reset = reset
    if (dut.io.debug_clk.isDefined) dut.io.debug_clk.get := dtm_clock
    if (dut.io.debug_rst.isDefined) dut.io.debug_rst.get := dtm_reset
    val dtm = Module(new SimDTM).connect(dtm_clock, dtm_reset, dut.io.debug.get,
      dut.io.success, io.success)
  } else {
    val jtag = Module(new JTAGVPI).connect(dut.io.jtag.get, reset, io.success)
  }

  for (bus_axi <- dut.io.bus_axi) {
    bus_axi.ar.valid := Bool(false)
    bus_axi.aw.valid := Bool(false)
    bus_axi.w.valid  := Bool(false)
    bus_axi.r.ready  := Bool(false)
    bus_axi.b.ready  := Bool(false)
  }

  for (mmio_axi <- dut.io.mmio_axi) {
    val slave = Module(new NastiErrorSlave)
    slave.io <> mmio_axi
  }

}

class SimAXIMem(size: BigInt)(implicit p: Parameters) extends NastiModule()(p) {
  val io = new Bundle {
    val axi = new NastiIO().flip
  }

  val rValid = Reg(init = Bool(false))
  val ar = RegEnable(io.axi.ar.bits, io.axi.ar.fire())
  io.axi.ar.ready := !rValid
  when (io.axi.ar.fire()) { rValid := true }
  when (io.axi.r.fire()) {
    assert(ar.burst === NastiConstants.BURST_INCR)
    ar.addr := ar.addr + (UInt(1) << ar.size)
    ar.len := ar.len - 1
    when (ar.len === UInt(0)) { rValid := false }
  }

  val w = io.axi.w.bits
  require((size * 8) % nastiXDataBits == 0)
  val depth = (size * 8) / nastiXDataBits
  val mem = Mem(depth.toInt, w.data)

  val wValid = Reg(init = Bool(false))
  val bValid = Reg(init = Bool(false))
  val aw = RegEnable(io.axi.aw.bits, io.axi.aw.fire())
  io.axi.aw.ready := !wValid && !bValid
  io.axi.w.ready := wValid
  when (io.axi.b.fire()) { bValid := false }
  when (io.axi.aw.fire()) { wValid := true }
  when (io.axi.w.fire()) {
    assert(aw.burst === NastiConstants.BURST_INCR)
    aw.addr := aw.addr + (UInt(1) << aw.size)
    aw.len := aw.len - 1
    when (aw.len === UInt(0)) {
      wValid := false
      bValid := true
    }

    def row = mem((aw.addr >> log2Ceil(nastiXDataBits/8))(log2Ceil(depth)-1, 0))
    val mask = FillInterleaved(8, w.strb)
    val newData = mask & w.data | ~mask & row
    row := newData
  }

  io.axi.b.valid := bValid
  io.axi.b.bits.id := aw.id
  io.axi.b.bits.resp := RESP_OKAY

  io.axi.r.valid := rValid
  io.axi.r.bits.id := ar.id
  io.axi.r.bits.data := mem((ar.addr >> log2Ceil(nastiXDataBits/8))(log2Ceil(depth)-1, 0))
  io.axi.r.bits.resp := RESP_OKAY
  io.axi.r.bits.last := ar.len === UInt(0)
}

class SimDTM(implicit p: Parameters) extends BlackBox {
  val io = new Bundle {
    val clk = Clock(INPUT)
    val reset = Bool(INPUT)
    val debug = new uncore.devices.DebugBusIO
    val exit = UInt(OUTPUT, 32)
  }

  def connect(tbclk: Clock, tbreset: Bool, dutio: uncore.devices.DebugBusIO,
      dutsuccess: Bool, tbsuccess: Bool) = {
    io.clk := tbclk
    io.reset := tbreset
    dutio <> io.debug

    tbsuccess := dutsuccess || io.exit === 1
    when (io.exit >= 2) {
      printf("*** FAILED *** (exit code = %d)\n", io.exit >> 1)
      stop(1)
    }
  }
}

class JTAGVPI(implicit val p: Parameters) extends BlackBox {
  val io = new Bundle {
    val jtag = new JTAGIO(false)
    val enable = Bool(INPUT)
    val init_done = Bool(INPUT)
  }

  def connect(dutio: JTAGIO, tbreset: Bool, tbsuccess: Bool) = {
    dutio <> io.jtag

    // To be proper,
    // TRST should really be synchronized
    // with TCK. But this is a fairly
    // accurate representation of how
    // HW may drive this signal.
    // Neither OpenOCD nor JtagVPI drive TRST.

    dutio.TRST := tbreset
    io.enable := ~tbreset
    io.init_done := ~tbreset

    // Success is determined by the gdbserver
    // which is controlling this simulation.
    tbsuccess := Bool(false)
  }
}
