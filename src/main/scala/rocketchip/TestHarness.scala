// See LICENSE.SiFive for license details.

package rocketchip

import Chisel._
import config._
import junctions._
import diplomacy._
import coreplex._
import uncore.axi4._
import jtag.JTAGIO

class TestHarness()(implicit p: Parameters) extends Module {
  val io = new Bundle {
    val success = Bool(OUTPUT)
  }
  val dut = Module(LazyModule(new ExampleRocketTop).module)

  for (int <- dut.io.interrupts(0))
    int := Bool(false)

  if (dut.io.mem_axi4.nonEmpty) {
    val memSize = p(ExtMem).size
    require(memSize % dut.io.mem_axi4.size == 0)
    for (axi4 <- dut.io.mem_axi4) {
      Module(LazyModule(new SimAXIMem(memSize / dut.io.mem_axi4.size)).module).io.axi4 <> axi4
    }
  }

  if (!p(IncludeJtagDTM)) {
    val dtm = Module(new SimDTM).connect(clock, reset, dut.io.debug.get, io.success)
  } else {
    val jtag = Module(new JTAGVPI).connect(dut.io.jtag.get, reset, io.success)
  }

  val mmio_sim = Module(LazyModule(new SimAXIMem(4096)).module)
  mmio_sim.io.axi4 <> dut.io.mmio_axi4

  val l2_axi4 = dut.io.l2_axi4(0)
  l2_axi4.ar.valid := Bool(false)
  l2_axi4.aw.valid := Bool(false)
  l2_axi4.w .valid := Bool(false)
  l2_axi4.r .ready := Bool(true)
  l2_axi4.b .ready := Bool(true)
}

class SimAXIMem(size: BigInt)(implicit p: Parameters) extends LazyModule {
  val config = p(ExtMem)

  val node = AXI4BlindInputNode(AXI4MasterPortParameters(Seq(AXI4MasterParameters(IdRange(0, 1 << config.idBits)))))
  val sram = LazyModule(new AXI4RAM(AddressSet(0, size-1), beatBytes = config.beatBytes))
  sram.node := AXI4Buffer()(AXI4Fragmenter(maxInFlight = 4)(node))

  lazy val module = new LazyModuleImp(this) {
    val io = new Bundle {
      val axi4 = node.bundleIn
    }
  }
}

class SimDTM(implicit p: Parameters) extends BlackBox {
  val io = new Bundle {
    val clk = Clock(INPUT)
    val reset = Bool(INPUT)
    val debug = new uncore.devices.DebugBusIO
    val exit = UInt(OUTPUT, 32)
  }

  def connect(tbclk: Clock, tbreset: Bool, dutio: uncore.devices.DebugBusIO, tbsuccess: Bool) = {
    io.clk := tbclk
    io.reset := tbreset
    dutio <> io.debug

    tbsuccess := io.exit === UInt(1)
    when (io.exit >= UInt(2)) {
      printf("*** FAILED *** (exit code = %d)\n", io.exit >> UInt(1))
      stop(1)
    }
  }
}

class JTAGVPI(implicit val p: Parameters) extends BlackBox {
  val io = new Bundle {
    val jtag = new JTAGIO()
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

    dutio.TRSTn  := ~tbreset
    io.enable    := ~tbreset
    io.init_done := ~tbreset

    // Success is determined by the gdbserver
    // which is controlling this simulation.
    tbsuccess := Bool(false)
  }
}
